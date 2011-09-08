;;;; data-dump.asd

(in-package :cl-user)

(asdf:defsystem :cleve-data-dump
  :components ((:module data-dump
                :components ((:module src
                              :serial t
                              :components ((:file "package")
                                           (:file "data-dump"))))))
  :depends-on (:clsql-sqlite3))
