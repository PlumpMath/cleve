;;;; data-dump.lisp
;;;;
;;;; Do (clsql-sys:enable-sql-reader-syntax) on the Slime REPL otherwise
;;;; you'll get warnings when redefining a form.

(in-package :cleve-data-dump)

(clsql:file-enable-sql-reader-syntax)
(clsql:connect '("data-dump/db/rub10-sqlite3-v1.db") :database-type :sqlite3)

;; For debugging
;(clsql:start-sql-recording)


;;; Functions

(defun attribute-by-id (id)
  (multiple-value-bind (rows column-names)
      (clsql:select [*] :from [dgmAttributeTypes] :where [= [attributeID] id])
    (values (car rows) column-names)))


(defun category-by-id (id)
  (multiple-value-bind (rows column-names)
      (clsql:select [*] :from [invCategories] :where [= [categoryID] id])
    (values (car rows) column-names)))


(defun effect-by-id (id)
  (multiple-value-bind (rows column-names)
      (clsql:select [*] :from [dgmEffects] :where [= [effectID] id])
    (values (car rows) column-names)))


(defun group-by-id (id)
  (multiple-value-bind (rows column-names)
      (clsql:select [*] :from [invGroups] :where [= [groupID] id])
    (values (car rows) column-names)))


(defun region-by-id (id)
  (multiple-value-bind (rows column-names)
      (clsql:select [*] :from [mapRegions] :where [= [regionID] id])
    (values (car rows) column-names)))


(defun solar-system-by-id (id)
  (multiple-value-bind (rows column-names)
      (clsql:select [*] :from [mapSolarSystems] :where [= [solarSystemID] id])
    (values (car rows) column-names)))


(defun solar-system-from-jumps-by-id (id)
  (multiple-value-bind (rows column-names)
      (clsql:select [*] :from [mapSolarSystemJumps]
                        :where [= [fromSolarSystemID] id])
    (values rows column-names)))


(defun solar-system-to-jumps-by-id (id)
  (multiple-value-bind (rows column-names)
      (clsql:select [*] :from [mapSolarSystemJumps]
                        :where [= [toSolarSystemID] id])
    (values rows column-names)))


(defun station-by-id (id)
  (multiple-value-bind (rows column-names)
      (clsql:select [*] :from [staStations] :where [= [stationID] id])
    (values (car rows) column-names)))


;; Checkout the following link for subselects:
;; http://www.ravenbrook.com/doc/2002/09/13/common-sql/#section-4.1
;; and for iteration:
;; http://www.ravenbrook.com/doc/2002/09/13/common-sql/#section-4.3
(defun type-attributes-by-id (id)
  (let ((attrs (clsql:select [*] :from [dgmTypeAttributes]
                                 :where [= [typeID] id])))
    (loop for attr in attrs
          for value-int = (elt attr 2)
          for value-float = (elt attr 3)
          collect (cons (attribute-by-id (elt attr 1))
                        (if value-int
                            value-int
                            value-float)))))


(defun type-effects-by-id (id)
  (let ((effects (clsql:select [*] :from [dgmTypeEffects]
                                   :where [= [typeID] id])))
    (loop for effect in effects
          for is-default = (elt effect 2)
          collect (cons (effect-by-id (elt effect 1)) is-default))))


(defun type-by-id (id &rest columns)
  "Returns one[1] result for type ID from the table invTypes or NIL as the
  first value.  Returns the column names as second value.
  [1] since we're querying on primary key"
  (multiple-value-bind (rows column-names)
      (clsql:select [*] :from [invTypes] :where [= [typeID] id])
    (if columns
        (loop for column in columns
              for position = (position column column-names :test #'string=)
              when position
                collect (elt (car rows) position) into row
              and
                collect (elt column-names position) into names
              finally (return (if (< (length row) 2)
                                  (values (car row) (car names))
                                  (values row names))))
        (values (car rows) column-names))))


(defun type-by-name (name)
  "Returns multiple rows from the table invTypes or NIL as the first value.
  Returns the column names as second value.
  It is possible to use wildcards in NAME, eg.: \"tempest%\"."
  (multiple-value-bind (rows column-names)
      (clsql:select [*] :from [invTypes] :where [like [typeName] name])
    (values rows column-names)))
