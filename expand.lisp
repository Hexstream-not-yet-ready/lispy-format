(in-package #:lispy-format)

(defvar *~formats* (make-hash-table :test 'eq))

(defclass %format ()
  ((%name :initarg :name
          :reader %format-name
          :type symbol)
   (%opargs :initarg :opargs
            :reader %format-opargs
            :type list)
   (%args :initarg :args
          :reader %format-args
          :type list)
   (%expander :initarg :expander
              :reader %format-expander)))

(defmethod print-object ((format %format) stream)
  (print-unreadable-object (format stream :type t)
    (format stream "~A ~S ~S"
            (%format-name format)
            (%format-opargs format)
            (%format-args format))))

(defun find-~format (name &key (errorp t))
  (check-type name symbol)
  (or (gethash name *~formats*)
      (when errorp
        (error "There is no ~S operator named ~S."
               '~format name))))

(defun (setf find-~format) (new name &key (errorp t))
  (declare (ignore errorp))
  (check-type name symbol)
  (setf (gethash name *~formats*) new))

(defun formatexpand-finalize (form ~
                              &aux (stream `(,~ stream)) optimized)
  (with-open-stream (concatenated-constants (make-string-output-stream))
    (labels ((cut ()
               (let ((concatenated (get-output-stream-string
                                    concatenated-constants)))
                 (unless (zerop (length concatenated))
                   (push `(write-string ,concatenated ,stream)
                         optimized))))
             (optimize (form)
               (etypecase form
                 ((cons (eql progn))
                  (mapc #'optimize (rest form)))
                 (string
                  (write-string form concatenated-constants))
                 (t (cut) (push form optimized)))))
      (optimize form)
      (cut))
    (setf optimized (nreverse optimized))
    (if (rest optimized)
        `(progn ,@optimized)
        (first optimized))))

(defun formatexpand (form ~)
  (formatexpand-finalize (formatexpand-partially form ~) ~))

(defun formatexpand-forms (forms ~)
  (let ((result (formatexpand `(progn ,@forms) ~)))
    (if (typep result '(cons (eql progn)))
        (rest result)
        (list result))))

(defun formatexpand-partially (form ~)
  (flet ((expand (operator opargs args)
           (let ((format (find-~format operator :errorp nil)))
             (if format
                 (funcall (%format-expander format)
                          operator ~ opargs args)
                 form))))
    (typecase form
      ((cons symbol)
       (expand (first form) nil (rest form)))
      ((cons (cons symbol))
       (destructuring-bind ((operator &rest opargs) &rest args) form
         (expand operator opargs args)))
      (string form)
      ((or character number) (write-to-string form :escape nil))
      (t form))))

(defun formatexpand-forms-partially (forms ~)
  (mapcar (lambda (form)
            (formatexpand-partially form ~))
          forms))

(defmacro %with-~ ((~ stream) &body body)
  `(macrolet ((,~ (&rest args)
                (if (and (= (length args) 1)
                         (eq (first args) 'stream))
                    ',stream
                    (error "Unrecognized: ~S."
                           (cons ',~ args)))))
     ,@body))

(defmacro with-~format-stream ((var stream &key (maybe-output-to-string t))
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

(defmacro ~format ((~ &optional stream) &body body)
  (let* ((stream-var (gensym (string '#:stream))))
    `(with-~format-stream (,stream-var ,stream)
       (%with-~ (,~ ,stream-var)
         ,@(formatexpand-forms body ~)))))

(defmacro define-~format (name
                          (~ &rest opargs) (&rest args)
                          &body body)
  (let ((e-operator (gensym (string '#:operator)))
        (e-opargs (gensym (string '#:opargs)))
        (e-args (gensym (string '#:args))))
    `(setf (find-~format ',name)
           (make-instance
            '%format
            :name ',name
            :opargs ',opargs
            :args ',args
            :expander (lambda (,e-operator ,~ ,e-opargs ,e-args)
                        (declare (ignorable ,e-operator))
                        (destructuring-bind ,opargs ,e-opargs
                          (destructuring-bind ,args ,e-args
                            ,@body)))))))
