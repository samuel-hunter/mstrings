;;; ystrings.asd - system definition
;;;
;;; Copyright (c) 2022 Samuel Hunter
;;; All rights reserved

(defsystem #:ystrings
  :version "0.0.0"
  :license "Proprietary" ;; For now, until it's ready for the public.
  :author "Samuel Hunter"

  :depends-on (#:cl-syntax)
  :serial t
  :components ((:file "ystrings")))
