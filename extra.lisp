(in-package #:lispy-format)

;;;; Niceties that go beyond the original FORMAT.
;;;; Generalizations of constrained FORMAT features and such.

;; No direct FORMAT equivalent.
;; Present in some form in: ~%, ~&, ~|, ~~.
(defun ~repeat (string-or-char n
                &key stream (separator "") (before "") (after ""))
  (with-~format-stream (stream stream)
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

;; No direct FORMAT equivalent.
;; Present in some form in: ~D, ~R.
(defun ~group (string group-size
               &key stream (separator " ") align pad)
  (check-type string string)
  (with-~format-stream (stream stream)
    (let* ((separator (%string separator))
           (separator-length (length separator)))
      (cond
        ((and group-size
              (> (length string) group-size)
              separator
              (not (and (stringp separator)
                        (zerop separator-length))))
         (unless (plusp group-size)
           (error "group-size must be a positive integer."))
         (%output-~groups string
                          stream
                          group-size
                          separator
                          align
                          pad))
        (t (write-string string stream))))))

(defun %switch-left-right (align)
  (case align
    (:left :right)
    (:right :left)
    (t align)))

(defun %compute-~group-sizes (string group-size align
                              &aux (string-length (length string)))
  (multiple-value-bind (full-groups-count leftovers)
      (floor string-length group-size)
    (multiple-value-call
        (lambda (left right)
          (if (and (plusp full-groups-count)
                   (zerop left))
              (values group-size (1- full-groups-count) right)
              (values left full-groups-count right)))
      (multiple-value-bind (align bias) (%destructure-align align)
        (cond
          ((not (eq align :center))
           (when (and (not align) (plusp leftovers))
             (error "Don't know how to align groups with leftovers ~D ~
                     since :align nil (or unsupplied)~2%string:~%~S"
                    leftovers string))
           (when (zerop full-groups-count)
             (setf align (%switch-left-right align)))
           (multiple-value-bind (big small)
               (values (if (plusp full-groups-count)
                           (prog1 group-size
                             (decf full-groups-count))
                           (shiftf leftovers 0))
                       leftovers)
             (ecase align
               ((nil :left) (values big small))
               (:right (values small big)))))
          (t
           (when (zerop full-groups-count)
             (setf bias (%switch-left-right bias)))
           (multiple-value-bind (half rem) (floor leftovers 2)
             (if (zerop rem)
                 (values half half)
                 (ecase bias
                   (:left (values (1+ half) half))
                   (:right (values half (1+ half)))
                   (:error
                    (error "~A/~A yields ~A leftovers, ~
                            and :align is ~S (center with no bias), ~
                            and ~D is not divisible by 2.~@
                            May want to specify :align '~S or '~S."
                           string-length
                           group-size
                           leftovers
                           '=
                           leftovers
                           '<=
                           '>=)))))))))))

(defun %output-~groups (string stream group-size separator align pad
                        &aux (string-length (length string)))
  (multiple-value-bind (left-pad-char
                        right-pad-char
                        first-group-size
                        middle-groups-count
                        last-group-size)
      (multiple-value-call #'values
        (%destructure-pad pad)
        (%compute-~group-sizes string group-size align))
    (when (plusp first-group-size)
      (when left-pad-char
        (dotimes (i (- group-size first-group-size))
          (write-char left-pad-char stream)))
      (write-string string stream :start 0 :end first-group-size))
    (do* ((i 0 (1+ i))
          (start first-group-size end)
          (end (+ start group-size) (+ start group-size)))
         ((>= i middle-groups-count))
      (write-string separator stream)
      (write-string string stream :start start :end end))
    (when (plusp last-group-size)
      (when (plusp first-group-size)
        (write-string separator stream))
      (write-string string stream
                    :start (- string-length last-group-size))
      (when right-pad-char
        (dotimes (i (- group-size last-group-size))
          (write-char right-pad-char stream))))))

#+nil
(defun ~column (string min-width align
                &key stream pad ((:+ colinc) 1) (min-pad 0))
  (let (())
    (with-~format-stream (stream stream)
      )))

#+nil
(defun ~overflow (string max-width &key stream)
  (if (<= (length string) max-width)
      (write-string string stream)
      ))
