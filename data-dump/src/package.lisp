;;;; package.lisp

(in-package :cl-user)

(defpackage :cleve-data-dump
  (:use :cl)
  (:export :connect-to-eve-data-dump
           :attribute-by-id :category-by-id :effect-by-id :group-by-id
           :region-by-id :solar-system-by-id :solar-system-from-jumps-by-id
           :solar-system-to-jumps-by-id :station-by-id :type-attributes-by-id
           :type-effects-by-id :type-by-id :type-by-name))
