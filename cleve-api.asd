;;;; api.asd

(in-package :cl-user)

(asdf:defsystem :cleve-api
  :components ((:module api
                :components ((:module src
                              :serial t
                              :components ((:file "package")
                                           (:file "api"))))))
  :depends-on (:cxml :drakma :parse-number :split-sequence))
