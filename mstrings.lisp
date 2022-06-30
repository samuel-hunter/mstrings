;;; mstrings.lisp - reader macro implementation
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD 3-Clause License. See LICENSE for details.

(defpackage #:xyz.shunter.mstrings
  (:nicknames #:mstrings)
  (:use #:cl)
  (:export #:mstring-reader
           #:shorthand-mstrings)
  (:documentation "Reader macro for friendlier multiline strings"))

(in-package #:xyz.shunter.mstrings)



;; Literal-block mode (#M"..."):
;; - Newlines and trailing spaces turn into a single newline
;; - Sequence of n newlines and any # of spaces are folded into n newlines
;; - Non-space or escaped character marks beginning of next line

;; Folding-block mode (#M>"..."):
;; - Newlines and trailing spaces turn into a single space
;; - Sequence of n+1 newlines and any # of spaces are folded into n newlines
;; - non-space or escaped character marks beginning of next line


;; Convenience functions with default behavior
(declaim (inline read-char! peek-char!))

(defun read-char! (stream)
  (read-char stream t nil t))

(defun peek-char! (peek-type stream)
  (peek-char peek-type stream t nil t))

(defun whitespacep (c)
  (and (position c #.(coerce '(#\Space #\Tab #\Page #\Newline) 'string))
       t))


(defun read-line-until-delim (stream out)
  ;; Read from STREAM until it reaches a Newline or string delimiter #\" and
  ;; write to OUT. Return whether it terminated from an unescaped Newline.
  (do ((c (read-char! stream) (read-char! stream)))
      ((char= c #\Newline) t)
    (cond
      ((char= c #\")
       (unread-char #\" stream)
       (return))
      ((not (char= c #\\))
       (write-char c out))
      ((char= (setf c (read-char! stream)) #\Newline)
       (return))
      (t (write-char c out)))))

(defun skip-empty-lines (stream out)
  ;; Skip to the first non-space character, and transform the number of "empty
  ;; lines" (lines with no text or only whitespace) into #\Newline to OUT.
  ;;
  ;; Return whether any newlines were printed
  (do ((c (read-char! stream) (read-char! stream))
       newlines-printed?)
      ((not (whitespacep c))
       (unread-char c stream)
       newlines-printed?)
    (when (char= c #\Newline)
      (write-char #\Newline out)
      (setf newlines-printed? t))))

(defun read-literal-mstring (stream)
  (with-output-to-string (out)
    (do () (nil)
      (when (read-line-until-delim stream out)
        (write-char #\Newline out))
      (when (char= (peek-char! nil stream) #\")
        (read-char! stream)
        (return))
      (skip-empty-lines stream out))))

(defun read-folding-mstring (stream)
  (with-output-to-string (out)
    (do (print-space?) (nil)
      (setf print-space? (read-line-until-delim stream out))
      (when (char= (peek-char! nil stream) #\")
        (read-char! stream)
        (return))
      (when (and (not (skip-empty-lines stream out))
                 print-space?)
        (write-char #\Space out)))))

(defun mstring-reader (stream subchar arg)
  "Multiline string reader function to be installed as a dispatching macro character.
It ignores and warns on any provided ARG, and provides a few quality-of-life features depending on the value of SUBCHAR:

- If SUBCHAR is #\\\", it is always literal-block mode (`#M\"...\"`) and reads the rest of the string.
- If SUBCHAR is #\\>, it is always folding-block mode (`#M>\"...\"`).
- If SUBCHAR is anything else, it reverts to its default behavior: it assumes literal-block mode unless there is a greater-than-sign preceding the string, in which case it switches to folding-block mode."
  (when arg
    (warn "Numerical argument ~S was ignored in M-string." arg))

  (cond
    ((char= subchar #\")
     (read-literal-mstring stream))
    ((char= subchar #\>)
     (assert (char= #\" (read-char! stream)))
     (read-folding-mstring stream))

    ((char= (peek-char! nil stream) #\")
     (read-char! stream)
     (read-literal-mstring stream))
    ((char= (peek-char! nil stream) #\>)
     (read-char! stream)
     (assert (char= (read-char! stream) #\"))
     (read-folding-mstring stream))
    ;; Room for more block modes here if need be
    (t (error "Unknown string style '~C'" (peek-char! nil stream)))))

(syntax:define-package-syntax #:xyz.shunter.mstrings
  (:merge :standard)
  (:dispatch-macro-char #\# #\M #'mstring-reader))

(syntax:defsyntax shorthand-mstrings
  (:merge :standard)
  ;; #\" is also claimed by the string-escape library
  (:dispatch-macro-char #\# #\" #'mstring-reader)
  ;; #> is claimed by Clozure CL 1.2 and later :(
  #-CCL-1.2
  (:dispatch-macro-char #\# #\> #'mstring-reader))
