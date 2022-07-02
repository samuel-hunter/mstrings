;;; test.lisp - test suite
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD 3-Clause License. See LICENSE for details.

(defpackage #:xyz.shunter.mstrings.test
  (:use #:cl #:xyz.shunter.mstrings)
  (:local-nicknames (#:t #:parachute))
  (:documentation "Mstrings library test suite"))

(in-package #:xyz.shunter.mstrings.test)
(named-readtables:in-readtable full-mstring-syntax)



(t:define-test singleline-mstrings
  ;; A single-line M-string effectively reads as a normal string.
  (t:is string= "Jabberwocky!"
        #M"Jabberwocky!")

  (t:is string= ""
        #M"")

  (t:is string= "\H\e\l\l\o"
        #M"\H\e\l\l\o"))

(t:define-test literal-mode-mstrings
  (t:is string= "Hello
World!"
        #M"Hello
           World!")

  (t:is string= "HelloWorld!"
        #M"Hello\
           World!")

  (t:is string= "Hello

World!"
        #M"Hello

           World!")

  (t:is string= "The
smallest
scorpions
sting
the
worst"
        #M"The
                smallest
        scorpions
                sting
                        the
           worst")

  (t:is string= "Overhang
"
        #M"Overhang
           ")

  (t:is string= "
Jabberwocky!"
        #M"
        Jabberwocky!")

  (t:is string= "

Jabberwocky!"
        #M"

        Jabberwocky!"))

(t:define-test folding-mode-mstrings
  (t:is string= "Hello World!"
        #M>"Hello
            World!")

  (t:is string= "HelloWorld!"
        #M>"Hello\
            World!")

  (t:is string= "Hello
World!"
        #M>"Hello

           World!")

  (t:is string= "The smallest scorpions sting the worst"
        #M>"The
                smallest
        scorpions
                sting
                        the
            worst")

  (t:is string= "Jabberwocky!"
        #M>"
        Jabberwocky!")

  (t:is string= "
Jabberwocky!"
        #M>"

        Jabberwocky!"))

(t:define-test mstring-macro-shorthands
  (t:is string= "Literal-block
Mode"
        #"Literal-block
          Mode")

  (t:skip-on (ccl-1.2) "#> macro claimed by Clozure CL 1.2 and later"
    (t:is string= "Folding-block Mode"
          #-CCL-1.2
          #>"Folding-block Mode"
          #+CCL-1.2
          nil)))
