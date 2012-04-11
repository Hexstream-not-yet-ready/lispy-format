(in-package #:lispy-format)

(defun cheat (stream control &rest args)
  (apply #'format stream control args))

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

(defun printing-char-p (character)
  (and (graphic-char-p character)
       (not (char= character #\Space))))

(defmacro with-~format-stream ((var &key (stream var)
                                    (maybe-output-to-string t))
                               &body body)
  (if maybe-output-to-string
      (let ((shared (gensym (string '#:shared))))
        `(flet ((,shared (,var)
                  ,@body))
           (let ((,var ,stream))
             (if ,var
                 (,shared (if (eq ,var 't)
                              *standard-output*
                              ,var))
                 (with-output-to-string (,var)
                   (,shared ,var))))))
      `(let ((,var ,stream))
         ,@body)))

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
;; ~C http://www.lispworks.com/documentation/HyperSpec/Body/22_caa.htm
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

;; No direct FORMAT equivalent.
(defun ~repeat (string-or-char n
                &key stream (separator "") (before "") (after ""))
  (with-~format-stream (stream)
    (when (plusp n)
      (%write-string before stream)
      (if (= n 1)
          (%write-string string-or-char stream)
          (let* ((string (%string string-or-char))
                 (separator-then-string (concatenate 'string
                                                     (%string separator)
                                                     string)))
            (write-string string stream)
            (dotimes (i (1- n))
              (write-string separator-then-string stream))))
      (%write-string after stream)
      n)))

;; ~% http://www.lispworks.com/documentation/HyperSpec/Body/22_cab.htm
(defun ~% (&optional (n 1) stream)
  (~repeat #\Newline n :stream stream))

;; ~& http://www.lispworks.com/documentation/HyperSpec/Body/22_cac.htm
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
;; ~| http://www.lispworks.com/documentation/HyperSpec/Body/22_cad.htm
(defun ~page (&optional (n 1) stream)
  (~repeat #\Page n :stream stream))

;; ~~ http://www.lispworks.com/documentation/HyperSpec/Body/22_cae.htm
;; (defun ~~ (&optional (n 1) stream) ...) intentionally omitted.


;;; http://www.lispworks.com/documentation/HyperSpec/Body/22_cba.htm
;;; ~R without prefix arguments
;; ~R
(defun ~cardinal (integer &optional stream)
  (with-~format-stream (stream)
    (cheat stream (formatter "~R") integer)))

;; ~:R
(defun ~ordinal (integer &optional stream)
  (with-~format-stream (stream)
    (cheat stream (formatter "~:R") integer)))

;; ~@R
(defun ~roman (positive-integer &optional stream)
  (with-~format-stream (stream)
    (cheat stream (formatter "~@R") positive-integer)))

;; ~:@R
(defun ~old-roman (positive-integer &optional stream)
  (with-~format-stream (stream)
    (cheat stream (formatter "~:@R") positive-integer)))


;; ~R http://www.lispworks.com/documentation/HyperSpec/Body/22_cba.htm
(defun ~radix (integer radix &key stream width pad separator group)
  (declare (ignore integer radix stream width pad separator group)))

;; ~D http://www.lispworks.com/documentation/HyperSpec/Body/22_cbb.htm
(defun ~d (integer &rest keys &key stream width pad separator group)
  (declare (ignore stream width pad separator group))
  (apply #'~radix integer 10 keys))

;; ~B http://www.lispworks.com/documentation/HyperSpec/Body/22_cbc.htm
(defun ~binary (integer &rest keys &key stream width pad separator group)
  (declare (ignore stream width pad separator group))
  (apply #'~radix integer 2 keys))

;; ~O http://www.lispworks.com/documentation/HyperSpec/Body/22_cbd.htm
(defun ~octal (integer &rest keys &key stream width pad separator group)
  (declare (ignore stream width pad separator group))
  (apply #'~radix integer 8 keys))

;; ~X http://www.lispworks.com/documentation/HyperSpec/Body/22_cbe.htm
(defun ~hex (integer &rest keys &key stream width pad separator group)
  (declare (ignore stream width pad separator group))
  (apply #'~radix integer 16 keys))


;; ~F http://www.lispworks.com/documentation/HyperSpec/Body/22_cca.htm
(defun ~ffloat (float &key stream width decimals (scale 0) overflow pad sign)
  (declare (ignore float stream width decimals scale overflow pad sign)))

;; ~E http://www.lispworks.com/documentation/HyperSpec/Body/22_ccb.htm
(defun ~efloat (float &key stream width decimals ewidth (scale 1) pad epad sign)
  (declare (ignore float stream width decimals ewidth scale pad epad sign)))

;; ~G http://www.lispworks.com/documentation/HyperSpec/Body/22_ccc.htm
(defun ~float (float &key stream width decimals ewidth scale overflow pad epad sign)
  (declare (ignore float stream width decimals ewidth scale overflow pad epad sign)))

;; ~$ http://www.lispworks.com/documentation/HyperSpec/Body/22_ccd.htm
(defun ~money (amount &key stream width decimals integrals sign)
  (declare (ignore amount stream width decimals integrals sign)))

;; ~A http://www.lispworks.com/documentation/HyperSpec/Body/22_cda.htm
(defun ~a (object
           &key stream width pad (align :left)
           ((:+ colinc) 1) ((:nil %nil) :symbol))
  (declare (ignore object stream width pad align colinc %nil)))

;; ~S http://www.lispworks.com/documentation/HyperSpec/Body/22_cdb.htm
(defun ~s (object
           &key stream width pad (align :left)
           ((:+ colinc) 1) ((:nil %nil) :symbol))
  (declare (ignore object stream width pad align colinc %nil)))

;; ~W http://www.lispworks.com/documentation/HyperSpec/Body/22_cdc.htm
(defun ~w (object &key stream pretty fully)
  (declare (ignore object stream pretty fully)))

;; ~_ http://www.lispworks.com/documentation/HyperSpec/Body/22_cea.htm
(defun ~_ (&optional (kind :linear) stream)
  (with-~format-stream (stream)
    (pprint-newline kind stream)))

;; ~<~:> http://www.lispworks.com/documentation/HyperSpec/Body/22_ceb.htm
(defmacro ~block () ; TODO, seems complicated.
  )

;; ~I http://www.lispworks.com/documentation/HyperSpec/Body/22_cec.htm
(defun ~i (&optional (n 0) (relative-to :block) stream)
  (with-~format-stream (stream :maybe-output-to-string nil)
    (pprint-indent relative-to n stream)))

;; ~/ http://www.lispworks.com/documentation/HyperSpec/Body/22_ced.htm
;; Intentionally omitted!

;;; ~T http://www.lispworks.com/documentation/HyperSpec/Body/22_cfa.htm
;; ~T
(defun ~tab (absolute-column column-increment
             &optional (stream *standard-output*))
  (with-~format-stream (stream :maybe-output-to-string nil)
    (pprint-tab :line absolute-column column-increment stream)))

;; ~@T
(defun ~rtab (relative-column column-increment
              &optional (stream *standard-output*))
  (with-~format-stream (stream :maybe-output-to-string nil)
    (pprint-tab :line-relative relative-column column-increment stream)))

;; ~:T
(defun ~stab (absolute-column column-increment
              &optional (stream *standard-output*))
  ;; TODO: "but measuring horizontal positions relative to"
  ;;       "the start of the dynamically enclosing section"
  (with-~format-stream (stream :maybe-output-to-string nil)
    (pprint-tab :section absolute-column column-increment stream)))

;; ~:@T
(defun ~srtab (relative-column column-increment
               &optional (stream *standard-output*))
  ;; TODO: Same as for ~stab
  (with-~format-stream (stream :maybe-output-to-string nil)
    (pprint-tab :section-relative relative-column column-increment stream)))

;; ~<~> http://www.lispworks.com/documentation/HyperSpec/Body/22_cfb.htm
(defmacro ~justify () ; TODO, seems complicated.
  )

;; ~* http://www.lispworks.com/documentation/HyperSpec/Body/22_cga.htm
;; Intentionally omitted.

;; ~[ http://www.lispworks.com/documentation/HyperSpec/Body/22_cgb.htm
;; TODO: IF, WHEN, UNLESS, COND, CASE, etc.

;; ~] http://www.lispworks.com/documentation/HyperSpec/Body/22_cgc.htm
;; Intentionally omitted...

;; ~{ http://www.lispworks.com/documentation/HyperSpec/Body/22_cgd.htm
;; TODO.

;; ~} http://www.lispworks.com/documentation/HyperSpec/Body/22_cge.htm
;; Intentionally omitted...

;; ~? http://www.lispworks.com/documentation/HyperSpec/Body/22_cgf.htm
;; TODO.

;;; ~( http://www.lispworks.com/documentation/HyperSpec/Body/22_cha.htm
;; ~(
(defun ~downcase (string &optional stream)
  (with-~format-stream (stream)
    (write-string (string-downcase string) stream)))

;; ~:(
(defun ~capitalize (string &optional stream)
  (with-~format-stream (stream)
    (write-string (string-capitalize string) stream)))

;; ~@(
(defun ~capitalize1 (string &optional stream)
  (check-type string string)
  (with-~format-stream (stream)
    (cheat stream "~@(~A~)" string)))

;; ~:@(
(defun ~upcase (string &optional stream)
  (with-~format-stream (stream)
    (write-string (string-upcase string) stream)))

;; ~) http://www.lispworks.com/documentation/HyperSpec/Body/22_chb.htm
;; Intentionally omitted...

;; ~P http://www.lispworks.com/documentation/HyperSpec/Body/22_chc.htm
;; "back up" (:) feature intentionally not supported in function form.
(defun ~p (quantity &optional (kind :s) stream)
  (with-~format-stream (stream)
    (ecase kind
      (:s (unless (eql quantity 1)
            (write-char #\s stream)))
      (:ies (write-string (if (eql quantity 1)
                              "y"
                              "ies")
                          stream)))))

;; ~; http://www.lispworks.com/documentation/HyperSpec/Body/22_cia.htm
;; Intentionally omitted...

;; ~^ http://www.lispworks.com/documentation/HyperSpec/Body/22_cib.htm
;; TODO.

;; ~\n http://www.lispworks.com/documentation/HyperSpec/Body/22_cic.htm
;; Intentionally omitted.
