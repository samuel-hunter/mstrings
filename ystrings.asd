;;; ystrings.asd - system definition
;;;
;;; Copyright (c) 2022 Samuel Hunter
;;; BSD 3-Clause

(defsystem #:ystrings
  :description "Reader macro that eliminates leading whitespace"
  :version "0.0.0"
  :license "BSD 3-Clause"
  :author "Samuel Hunter"

  :depends-on (#:cl-syntax)
  :serial t
  :components ((:file "ystrings")))
