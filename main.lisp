(in-package #:lispy-format)

(declaim (inline %string))
(defun %string (string-or-character)
  (etypecase string-or-character
    (string string-or-character)
    (character (string string-or-character))))

(declaim (inline %write-string))
(defun %write-string (string-or-character stream)
  (etypecase string-or-character
    (string (write-string string-or-character stream))
    (character (write-char string-or-character stream))))

(defmacro with-output-to-string-or-stream
    ((var &optional (string-or-stream var))
     &body body)
  (let ((shared (gensym (string '#:shared))))
    `(flet ((,shared (,var)
              ,@body))
       (let ((,var ,string-or-stream))
         (if ,var
             (,shared (if (eq ,var 't)
                          *standard-output*
                          ,var))
             (with-output-to-string (,var)
               (,shared ,var)))))))

(defun printing-char-p (character)
  (and (graphic-char-p character)
       (not (char= character #\Space))))

(defmacro with-~format-stream ((var &optional (stream var)) &body body)
  `(with-output-to-string-or-stream (,var ,stream)
     ,@body))

;;; ~c
;; Assume simple-character. Test assumption with:
#+nil
(let (non-simple-chars)
  (dotimes (code char-code-limit (values (length non-simple-chars)
                                         (nreverse non-simple-chars)))
    (let* ((char (code-char code))
           (write-char (with-output-to-string (string)
                         (write-char char string)))
           (non-simple (format nil "~C" char)))
      (when (string/= write-char non-simple)
        (format t "~2%~A: ~A~%~A~%~A"
                code char write-char non-simple)
        (push char non-simple-chars)))))
;; Assume ":@" is the same as just ":". Test assumption with:
#+nil
(let (modifiers-chars)
  (dotimes (code char-code-limit (values (length modifiers-chars)
                                         (nreverse modifiers-chars)))
    (let* ((char (code-char code))
           (pretty (format nil "~:C" char))
           (modifiers (format nil "~:@C" char)))
      (when (string/= pretty modifiers)
        (format t "~2%~A: ~A~%~A~%~A"
                code char pretty modifiers)
        (push char modifiers-chars)))))
;; http://www.lispworks.com/documentation/HyperSpec/Body/22_caa.htm
(defun ~c (character &key stream
           pretty  ; :
           escape) ; @
  (check-type character character)
  (with-~format-stream (stream)
    (cond ((and pretty escape)
           (error "~S can't have both :pretty and :escape be true. ~
                   (undefined consequences)" '~c))
          (pretty
           (if (printing-char-p character)
               (write-char character stream)
               (let ((name (char-name character)))
                 (if name
                     (write-string name stream)
                     ;; Could also punt to FORMAT.
                     (error "Don't know how to output ~
                             character with no name ~S." character)))))
          (escape
           (write character :escape t :stream stream))
          (t (write-char character stream)))
    character))

;; (No direct FORMAT equivalent)
(defun ~repeat (string-or-char n
                &key stream (between "") (before "") (after ""))
  (with-~format-stream (stream)
    (when (plusp n)
      (%write-string before stream)
      (if (= n 1)
          (%write-string string-or-char stream)
          (let* ((string (%string string-or-char))
                 (between-then-string (concatenate 'string
                                                   (%string between)
                                                   string)))
            (write-string string stream)
            (dotimes (i (1- n))
              (write-string between-then-string stream))))
      (%write-string after stream)
      n)))

;; http://www.lispworks.com/documentation/HyperSpec/Body/22_cab.htm
(defun ~% (&optional (n 1) stream)
  (~repeat #\Newline n :stream stream))

;; http://www.lispworks.com/documentation/HyperSpec/Body/22_cac.htm
(defun ~& (&optional (n 1) stream)
  (if stream
      (let ((freshp (when (plusp n)
                      (fresh-line stream))))
        (~% (1- n) stream)
        (let ((total (if freshp
                         n
                         (1- n))))
          (when (plusp total) total)))
      (~% n stream)))

;; (Maybe this feature could be dropped?)
;; http://www.lispworks.com/documentation/HyperSpec/Body/22_cad.htm
(defun ~page (&optional (n 1) stream)
  (~repeat #\Page n :stream stream))

;; http://www.lispworks.com/documentation/HyperSpec/Body/22_cae.htm
;; (defun ~~ (&optional (n 1) stream) ...) intentionally omitted.

