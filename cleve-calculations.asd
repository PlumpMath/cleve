;;;; calculations.asd

(in-package :cl-user)

(asdf:defsystem :cleve-calculations
  :components ((:module calculations
                :components ((:module src
                              :serial t
                              :components ((:file "package")
                                           (:file "calculations"))))))
  :depends-on (:cleve-data-dump :clsql-sqlite3))
