;;;; data-dump.lisp
;;;;
;;;; Do (clsql-sys:enable-sql-reader-syntax) on the Slime REPL otherwise
;;;; you'll get warnings when redefining a form.

(in-package :cleve-data-dump)

(clsql:file-enable-sql-reader-syntax)
(clsql:connect '("data-dump/db/inca10-sqlite3-v1.db") :database-type :sqlite3)

;; For debugging
(clsql:start-sql-recording)


;;; Functions

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
