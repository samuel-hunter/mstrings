;;; test.lisp - test suite
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD 3-Clause License. See LICENSE for details.

(defpackage #:xyz.shunter.mstrings.test
  (:use #:cl #:xyz.shunter.mstrings)
  (:local-nicknames (#:t #:parachute))
  (:documentation "Mstrings library test suite"))

(in-package #:xyz.shunter.mstrings.test)
(syntax:use-syntax '#:xyz.shunter.mstrings)



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
scorpion
stings
the
most"
        #M"The
                smallest
        scorpion
                stings
                        the
           most")

  (t:is string= "Overhang
"
        #M"Overhang
           "))

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

  (t:is string= "The smallest scorpion stings the most"
        #M>"The
                smallest
        scorpion
                stings
                        the
            most"))
