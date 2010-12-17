;;;; dd2lisp.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the CLEVE root directory for more info.
;;;;
;;;; Bugs:
;;;; - 1e+35 becomes 1.0d0

;;; Packages

(asdf:oos 'asdf:load-op :arnesi)
(asdf:oos 'asdf:load-op :split-sequence)

(defpackage :cleve-data-dump
  (:use :cl :split-sequence)
  (:import-from :arnesi :parse-float)
  (:import-from :cl-user :quit))

(in-package :cleve-data-dump)


;;; Parameters

(defparameter *tables* nil)
(defparameter *types* nil)


;;; Macros

(defmacro with-lines-from-file ((var filename &key (external-format :default))
                                &body body)
  (let ((stream (gensym)))
    `(with-open-file (,stream ,filename :external-format ,external-format)
      (loop for ,var = (read-line ,stream nil nil)
            while ,var
            do ,@body))))


;;; Functions

(defun append1 (lst obj)
  (append lst (list obj)))


(defun lispify-string (string)
  (loop with prev-char = nil
        with result = nil
        for c across string
        do (cond ;; "X" => "-x"
                 ((and (upper-case-p c)
                       (> (length result) 1)  ; don't put a #\- at the front
                       (not (upper-case-p prev-char)))  ; keep abbreviations
                  (push #\- result)
                  (push c result))
                 ;; " " => "-"
                 ((equal c #\Space) (push #\- result))
                 ;; "_" => "-"
                 ((equal c #\_)
                  (when (> (length result) 1)  ; don't put a #\- at the front
                    (push #\- result)))
                 ;; Otherwise just collect the char.
                 (t (push c result)))
           (setf prev-char c)
        finally (return (string-downcase (coerce (nreverse result) 'string)))))


(defun make-keyword (string)
  (intern (string-upcase (lispify-string string)) :keyword))


(defun parse-column-name (line)
  (let ((column-name (third (split-sequence #\space line))))
    (make-keyword (subseq column-name 1 (- (length column-name) 1)))))


(defun starts-with (sequence subsequence)
  (let ((sublen (length subsequence)))
    (when (and (> sublen 0)
               (<= sublen (length sequence)))
      (equal (subseq sequence 0 sublen) subsequence))))


(defun parse-column-type (line)
  (let ((type-name (fourth (split-sequence #\space line))))
    (cond ((starts-with type-name "double") :double-float)
          ((or (starts-with type-name "bigint")
               (starts-with type-name "int")
               (starts-with type-name "smallint")
               (starts-with type-name "tinyint")) :integer)
          ((or (starts-with type-name "char")
               ;; XXX: Not correct but suffices for now.
               (starts-with type-name "datetime")
               (starts-with type-name "varchar")) :string)
          (t (error "Unknown SQL type: ~S" type-name)))))


;; XXX: very similar to parse-primary-key, need to be merged
(defun parse-insert-into-table-name (line)
  (loop with inside-backquote = nil
        for c across line
        when (equal c #\`) do (setf inside-backquote (not inside-backquote))
        when (and (not (equal c #\`))
                  inside-backquote)
          collect c into result
        finally (return (make-keyword (coerce result 'string)))))


(defun parse-value (value type)
  (let ((string (coerce (nreverse value) 'string)))
    (cond ((equal string "NULL") nil)
          ((equal type :double-float) (parse-float string :type 'double-float))
          ((equal type :integer) (parse-integer string))
          ((equal type :string) (subseq string 1 (- (length string) 1)))
          (t (error "Unknown type \"~S\" for value: ~S" type value)))))


(defun parse-insert-into (line)
  (loop with table = (parse-insert-into-table-name line)
        with columns = (second (assoc table *types* :test #'equal))
        with inside-string = nil
        with prev-char = nil
        with value = nil
        with result = nil
        with i = 0
        for c across (subseq line (+ (position #\( line) 1))
        for column = (elt columns i)
        do (cond ;; end of values
                 ((and (equal c #\)) (not inside-string))
                  (push (first column) result)
                  (push (parse-value value (second column)) result)
                  (loop-finish))
                 ;; end of value
                 ((and (equal c #\comma) (not inside-string))
                  (push (first column) result)
                  (push (parse-value value (second column)) result)
                  (setf value nil)
                  (incf i))
                 ;; string delimiters
                 ((and (equal c #\') (not (equal prev-char #\\)))
                  (setf inside-string (not inside-string))
                  (push c value)
                  (setf prev-char c))
                 ;; everything else
                 (t (push c value)
                    (setf prev-char c)))
        finally (return (list table (nreverse result)))))


;; XXX: very similar to parse-insert-into-table-name, need to be merged
(defun parse-primary-key (line)
  (loop with inside-backquote = nil
        for c across line
        ;; order of first two 'when's is important!
        when (and (equal c #\`) inside-backquote)
          do (loop-finish)
        when (and (equal c #\`) (not inside-backquote))
          do (setf inside-backquote t)
        when (and (not (equal c #\`)) inside-backquote)
          collect c into result
        finally (return (make-keyword (coerce result 'string)))))


(defun parse-table-name (line)
  (let ((table-name (third (split-sequence #\space line))))
    (make-keyword (subseq table-name 1 (- (length table-name) 1)))))


(defun primary-key (table)
  (second (assoc :primary-key (second (assoc table *types*)))))


(defun print-hash (hash)
  (maphash #'(lambda (key value)
               (format t "~S => ~S~%" key value))
           hash))


;;; Modify Macros

(define-modify-macro appendf (&rest lists) append
  "Modify-macro for APPEND. Appends LISTS to the place designated by the first
  argument.")


(define-modify-macro appendf1 (object) append1)


;;; Main Program

;; test.sql is similar to inc101-mysql5-v1.sql but with the
;; mapCelestialStatistics, mapDenormalize and ramAssemblyLines tables removed,
;; both for size (SBCL would exhaust the stack with its defaults) and speed.
(defun parse-data-dump (&optional (file "tmp/test.sql"))
  (format t "Parsing file ~S...~%" file)
  (setf *tables* nil)
  (setf *types* nil)
  (let ((state :default)
        (table nil))
    (with-lines-from-file (line file)
      (cond ;; CREATE TABLE
            ((and (equal state :default) (starts-with line "CREATE TABLE `"))
             (setf state :create-table)
             (let ((table-name (parse-table-name line)))
               (setf table table-name)
               (setf (getf *tables* table-name) (make-hash-table))
               (pushnew (list table-name nil) *types* :test #'equal)
               (format t "Creating table ~S...~%" table-name)))
            ;; inside CREATE TABLE (typedef)
            ((and (equal state :create-table) (starts-with line "  `"))
             (let ((column (parse-column-name line))
                   (type (parse-column-type line)))
               (appendf1 (second (assoc table *types* :test #'equal))
                         (list column type))))
            ;; inside CREATE TABLE (PRIMARY KEY)
            ((and (equal state :create-table) (starts-with line "  PRIMARY K"))
             (let ((primary-key (parse-primary-key line)))
               (appendf1 (second (assoc table *types* :test #'equal))
                         (list :primary-key primary-key))))
            ;; end of CREATE TABLE
            ((and (equal state :create-table) (starts-with line ")"))
             (setf table nil
                   state :default))
            ;; INSERT INTO
            ((and (equal state :default) (starts-with line "INSERT INTO `"))
             (let* ((parsed-line (parse-insert-into line))
                    (key (first parsed-line))
                    (plist (second parsed-line)))
               (setf (gethash (getf plist (primary-key key))
                              (getf *tables* key))
                     plist)))))))
