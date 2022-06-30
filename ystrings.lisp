;;; ystrings.lisp - reader macro implementation

(defpackage #:xyz.shunter.ystrings
  (:nicknames #:ystrings)
  (:use #:cl))

(in-package #:xyz.shunter.ystrings)



;; Literal mode (default):
;; - Newlines and trailing spaces turn into a single newline
;; - Sequence of n newlines and any # of spaces are folded into n newlines
;; - Non-space or escaped character marks beginning of next line

;; Folded mode:
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

(defun whitespacep (c)
  ;; Whether the character is a whitespace
  (position c #(#\Space #\Tab #\Page #\Newline)))

(defun skip-empty-lines (stream out)
  ;; Skip to the first non-space character, and transform the number of "empty
  ;; lines" (lines with no text or only whitespace) into #\Newline to OUT.
  (do ((c (read-char stream t nil t)
          (read-char stream t nil t))
       (count 0))
      ((not (whitespacep c))
       (unread-char c stream)
       count)
    (when (char= c #\Newline)
      (write-char #\Newline out))))

(defun read-ystring (stream newline-style)
  (with-output-to-string (out)
    (loop
      (when (char= (peek-char nil stream t nil t) #\")
        (read-char stream t nil t)
        (return))
      (when (read-line-until-delim stream out)
        (write-char newline-style out))
      (when (char= (peek-char nil stream t nil t) #\")
        (read-char stream t nil t)
        (return))
      (skip-empty-lines stream out))))

(defun read-ystring-reader (stream subchar numarg)
  (declare (ignore subchar))
  (when numarg
    (warn "Ystring argument ~D was ignored." numarg))

  (let ((arg (read-char stream t nil t))
        (newline-style #\Newline))
    (cond
      ((char= arg #\>)
       (setf newline-style #\Space))
      ;; Room for more string styles here, if need be
      ((not (char= arg #\"))
       (error "Unknown string style '~C'" arg)))
    (unless (char= arg #\")
      (assert (char= #\" (read-char stream t nil t))))

    (read-ystring stream newline-style)))

(syntax:define-package-syntax #:xyz.shunter.ystrings
  (:merge :standard)
  (:dispatch-macro-char #\# #\Y 'read-ystring-reader))
