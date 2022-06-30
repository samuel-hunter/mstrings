;;; mstrings.asd - system definitions
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD 3-Clause License. See LICENSE for details.

(defsystem #:mstrings
  :description "Reader macro for friendlier multiline strings"
  :long-description "Mstrings defines a reader macro for multiline strings with various quality-of-life features, including whitespace elimination and line folding."
  :version "0.0.0"
  :license "BSD 3-Clause"
  :author "Samuel Hunter"

  :homepage "https://sr.ht/~shunter/mstrings/"
  :source-control (:git "https://git.sr.ht/~shunter/mstrings/")
  :bug-tracker "https://todo.sr.ht/~shunter/mstrings/"
  :mailto "\~\s\h\u\n\t\e\r\/\p\u\b\l\i\c\-\i\n\b\o\x\@\l\i\s\t\s\.\s\r\.\h\t"

  :depends-on (#:cl-syntax)
  :components ((:file "mstrings"))
  :in-order-to ((test-op (test-op :mstrings/test))))

(defsystem #:mstrings/test
  :description "Mstrings library test suite"
  :version "0.0.0"
  :license "BSD 3-Clause"
  :author "Samuel Hunter"

  :depends-on (#:mstrings
               #:parachute)
  :components ((:file "test"))
  :perform (test-op (op c)
             (uiop:symbol-call :parachute :test :xyz.shunter.mstrings.test)))
