;;; test.lisp - test suite
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD 3-Clause License. See LICENSE for details.

(defpackage #:xyz.shunter.ystrings.test
  (:use #:cl #:xyz.shunter.ystrings)
  (:local-nicknames (#:t #:parachute))
  (:documentation "Ystrings library test suite"))

(in-package #:xyz.shunter.ystrings.test)
(syntax:use-syntax '#:xyz.shunter.ystrings)



(t:define-test singleline-ystrings
  ;; A single-line Y-string effectively reads as a normal string.
  (t:is string= "Jabberwocky!"
        #Y"Jabberwocky!")

  (t:is string= ""
        #Y"")

  (t:is string= "\H\e\l\l\o"
        #Y"\H\e\l\l\o"))

(t:define-test literal-mode-ystrings
  (t:is string= "Hello
World!"
        #Y"Hello
           World!")

  (t:is string= "HelloWorld!"
        #Y"Hello\
           World!")

  (t:is string= "Hello

World!"
        #Y"Hello

           World!")

  (t:is string= "The
smallest
scorpion
stings
the
most"
        #Y"The
                smallest
        scorpion
                stings
                        the
           most")

  (t:is string= "Overhang
"
        #Y"Overhang
           "))

(t:define-test folding-mode-ystrings
  (t:is string= "Hello World!"
        #Y>"Hello
            World!")

  (t:is string= "HelloWorld!"
        #Y>"Hello\
            World!")

  (t:is string= "Hello
World!"
        #Y>"Hello

           World!")

  (t:is string= "The smallest scorpion stings the most"
        #Y>"The
                smallest
        scorpion
                stings
                        the
            most"))
