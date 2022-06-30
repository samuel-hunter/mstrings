;;; ystrings.lisp - reader macro implementation
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD 3-Clause License. See LICENSE for details.

(defpackage #:xyz.shunter.ystrings
  (:nicknames #:ystrings)
  (:use #:cl)
  (:export #:use-ystrings)
  (:documentation "Reader macro for friendlier multiline strings"))

(in-package #:xyz.shunter.ystrings)



;; Literal mode (default):
;; - Newlines and trailing spaces turn into a single newline
;; - Sequence of n newlines and any # of spaces are folded into n newlines
;; - Non-space or escaped character marks beginning of next line

;; Folding mode:
;; - Newlines and trailing spaces turn into a single space
;; - Sequence of n+1 newlines and any # of spaces are folded into n newlines
;; - non-space or escaped character marks beginning of next line

(defun read-line-until-delim (stream out)
  ;; Read from STREAM until it reaches a Newline or string delimiter #\" and
  ;; write to OUT. Return whether it terminated from an unescaped Newline.
  (do ((c (read-char stream t nil t)
          (read-char stream t nil t)))
      ((char= c #\Newline) t)
    (cond
      ((char= c #\")
       (unread-char #\" stream)
       (return))
      ((not (char= c #\\))
       (write-char c out))
      ((char= (setf c (read-char stream t nil t)) #\Newline)
       (return))
      (t (write-char c out)))))

(declaim (ftype (function (character) boolean) whitespacep)
         (inline whitespacep))
(defun whitespacep (c)
  (and (position c #.(coerce '(#\Space #\Tab #\Page #\Newline) 'string))
       t))

(defun skip-empty-lines (stream out)
  ;; Skip to the first non-space character, and transform the number of "empty
  ;; lines" (lines with no text or only whitespace) into #\Newline to OUT.
  ;;
  ;; Return whether any newlines were printed
  (do ((c (read-char stream t nil t)
          (read-char stream t nil t))
       newlines-printed?)
      ((not (whitespacep c))
       (unread-char c stream)
       newlines-printed?)
    (when (char= c #\Newline)
      (write-char #\Newline out)
      (setf newlines-printed? t))))

(defun read-literal-ystring (stream)
  (with-output-to-string (out)
    (do () (nil)
      (when (read-line-until-delim stream out)
        (write-char #\Newline out))
      (when (char= (peek-char nil stream t nil t) #\")
        (read-char stream t nil t)
        (return))
      (skip-empty-lines stream out))))

(defun read-folding-ystring (stream)
  (with-output-to-string (out)
    (do (print-space?) (nil)
      (setf print-space? (read-line-until-delim stream out))
      (when (char= (peek-char nil stream t nil t) #\")
        (read-char stream t nil t)
        (return))
      (when (and (not (skip-empty-lines stream out))
                 print-space?)
        (write-char #\Space out)))))

(defun read-ystring-reader (stream subchar numarg)
  (declare (ignore subchar))
  (when numarg
    (warn "Ystring argument ~D was ignored." numarg))

  (let ((arg (read-char stream t nil t))
        (reading-strategy #'read-literal-ystring))
    (cond
      ((char= arg #\>)
       (setf reading-strategy #'read-folding-ystring))
      ;; Room for more string styles here, if need be
      ((not (char= arg #\"))
       (error "Unknown string style '~C'" arg)))
    (unless (char= arg #\")
      (assert (char= #\" (read-char stream t nil t))))

    (funcall reading-strategy stream)))

(syntax:define-package-syntax #:xyz.shunter.ystrings
  (:merge :standard)
  (:dispatch-macro-char #\# #\Y #'read-ystring-reader))

(defun use-ystrings (&optional (subchar #\Y) (readtable *readtable*))
  (set-dispatch-macro-character
    #\# subchar #'read-ystring-reader readtable))
