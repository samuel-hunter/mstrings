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



;; Convenience functions
(declaim (inline read-char! peek-char! whitespacep))

(defun read-char! (stream)
  (read-char stream t nil t))

(defun peek-char! (peek-type stream)
  (peek-char peek-type stream t nil t))

(defun whitespacep (c)
  (position c #.(coerce '(#\Space #\Tab #\Page #\Newline) 'string)))

(defmacro do@ (varlist endlist &body body)
  "Anaphoric DO. An @ in the then-form replays the init-form."
  `(do ,(mapcar (lambda (var-form)
                  (if (listp var-form)
                      (destructuring-bind (var &optional init-form step-form)
                                          var-form
                        (if (eq step-form '@)
                            `(,var ,init-form ,init-form)
                            var-form))
                      var-form))
               varlist)
       ,endlist ,@body))

;; Literal-block mode (#M"..."):
;; - Newlines and trailing spaces turn into a single newline
;; - Sequence of n newlines and any # of spaces are folded into n newlines
;; - Non-space or escaped character marks beginning of next line

;; Folding-block mode (#M>"..."):
;; - Newlines and trailing spaces turn into a single space
;; - Sequence of n+1 newlines and any # of spaces are folded into n newlines
;; - non-space or escaped character marks beginning of next line

(defun skip-spaces (stream)
  "Skip until the first Newline or non-whitespace character and return it, unconsumed."
  (do@ ((c (read-char! stream) @))
       ((or (char= c #\Newline)
            (not (whitespacep c)))
        (unread-char c stream)
        c)))

(defun read-line-until-delim (stream out)
  "Read mstring contets from STREAM until a Newline or string delimiter \".
Read from STREAM until it consumes a Newline or reaches the string delimiter \", and write to OUT.
Escaped quotes and whitespaces are treated as a non-whitespace character.

Returns two values:
- Whether there should be a linebreak at the block mode's discretion;
- Whether it has written any contents to OUT."
  (skip-spaces stream)
  (do@ ((c (read-char! stream) @)
        written?)
       ((char= c #\")
        (unread-char #\" stream)
        (values nil written?))
    (cond
      ;; Unescaped Newline
      ((char= c #\Newline)
       (return (values t written?)))
      ;; Unescaped character
      ((not (char= c #\\))
       (write-char c out)
       (setf written? t))
      ;; Escaped Newline
      ((char= (setf c (read-char! stream)) #\Newline)
       (return (values nil written?)))
      ;; Escaped character
      (t (write-char c out)
       (setf written? t)))))

(defun skip-empty-lines (stream out)
  "Skip all lines that contain only unescaped whitespaces, and write them out as a singular Newline.
Return whether any newlines were written."
  (do@ ((c (skip-spaces stream) @)
        (newlines-written? nil t))
      ((not (char= c #\Newline))
       newlines-written?)
    (write-char (read-char! stream) out)
    (setf newlines-written? t)))

(defun read-literal-mstring (stream)
  (with-output-to-string (out)
    (loop
      (when (read-line-until-delim stream out)
        (write-char #\Newline out))
      (when (char= (peek-char! nil stream) #\")
        (read-char! stream)
        (return)))))

(defun read-folding-mstring (stream)
  (with-output-to-string (out)
    (do (print-space?) (nil)
      (multiple-value-bind (linebreak? written?)
                           (read-line-until-delim stream out)
        (when (char= (peek-char! nil stream) #\")
          (read-char! stream)
          (return))
        (when (and (not (skip-empty-lines stream out))
                   linebreak? written?)
          (write-char #\Space out))))))

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
