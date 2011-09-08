;;;; data-dump.lisp
;;;;
;;;; Do (clsql-sys:enable-sql-reader-syntax) on the Slime REPL otherwise
;;;; you'll get warnings when redefining a form.

(in-package :cleve-data-dump)

(clsql:file-enable-sql-reader-syntax)
(clsql:connect '("data-dump/db/inca10-sqlite3-v1.db") :database-type :sqlite3)

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
          collect (list :attribute (attribute-by-id (elt attr 1))
                        :value (if value-int
                                   value-int
                                   value-float)))))


(defun type-by-id (id)
  (multiple-value-bind (rows column-names)
      (clsql:select [*] :from [invTypes] :where [= [typeID] id])
    (values (car rows) column-names)))
