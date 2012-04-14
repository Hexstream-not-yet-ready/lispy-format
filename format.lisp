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

(defun formatexpand-finalize (form stream &aux optimized)
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

(defun formatexpand (form ~ stream)
  (formatexpand-finalize (formatexpand-partially form ~ stream) stream))

(defun formatexpand-forms (forms ~ stream)
  (let ((result (formatexpand `(progn ,@forms) ~ stream)))
    (if (typep result '(cons (eql progn)))
        (rest result)
        (list result))))

(defun formatexpand-partially (form ~ stream)
  (flet ((expand (operator opargs args)
           (let ((format (find-~format operator :errorp nil)))
             (if format
                 (funcall (%format-expander format)
                          operator ~ stream opargs args)
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

(defun formatexpand-forms-partially (forms ~ stream)
  (mapcar (lambda (form)
            (formatexpand-partially form ~ stream))
          forms))

(defmacro ~format ((~ &optional stream) &body body)
  (let* ((stream-var (gensym (string '#:stream))))
    `(with-~format-stream (,stream-var
                           ,@(when stream (list :stream stream)))
       ,@(formatexpand-forms body ~ stream-var))))

(defmacro define-~format (name
                          (~ stream &rest opargs) (&rest args)
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
            :expander (lambda (,e-operator ,~ ,stream ,e-opargs ,e-args)
                        (declare (ignorable ,e-operator))
                        (destructuring-bind ,opargs ,e-opargs
                          (destructuring-bind ,args ,e-args
                            ,@body)))))))

(define-~format progn (~ stream) (&rest forms)
  `(progn ,@(formatexpand-forms-partially forms ~ stream)))

(define-~format if (~ stream) (test then &optional (else nil elsep))
  `(if ,test
       ,(formatexpand-partially then ~ stream)
       ,@(when elsep (list (formatexpand-partially else ~ stream)))))

(define-~format when (~ stream) (test &rest body)
  `(when ,test
     ,@(formatexpand-forms-partially body ~ stream)))

(define-~format unless (~ stream) (test &rest body)
  `(unless ,test
     ,@(formatexpand-forms-partially body ~ stream)))

(define-~format cond (~ stream) (&rest clauses)
  `(cond ,@(mapcar
            (lambda (clause)
              (cons (first clause)
                    (formatexpand-forms-partially (rest clause) ~ stream)))
                   clauses)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-caselike (operator ~ stream keyform cases)
    `(,operator
      ,keyform
      ,@(mapcar
         (lambda (case)
           (cons (first case)
                 (formatexpand-forms-partially (rest case) ~ stream)))
         cases)))

  (defun expand-letlike (operator ~ stream bindings body)
    `(,operator
      ,(mapcar
        (lambda (binding)
          (if (consp binding)
              (cons (first binding)
                    (formatexpand-forms-partially (rest binding) ~ stream))
              binding))
        bindings)
      ,@body)))

(define-~format case (~ stream) (keyform &body cases)
  (expand-caselike 'case ~ stream keyform cases))

(define-~format ccase (~ stream) (keyform &body cases)
  (expand-caselike 'ccase ~ stream keyform cases))

(define-~format ecase (~ stream) (keyform &body cases)
  (expand-caselike 'ecase ~ stream keyform cases))

(define-~format typecase (~ stream) (keyform &body cases)
  (expand-caselike 'typecase ~ stream keyform cases))

(define-~format ctypecase (~ stream) (keyform &body cases)
  (expand-caselike 'ctypecase ~ stream keyform cases))

(define-~format etypecase (~ stream) (keyform &body cases)
  (expand-caselike 'etypecase ~ stream keyform cases))

(define-~format let (~ stream) (bindings &body body)
  (expand-letlike 'let ~ stream bindings body))

(define-~format let* (~ stream) (bindings &body body)
  (expand-letlike 'let* ~ stream bindings body))


#+nil
(defmacro ~error ((~) &body body)
  `(error "~A" (~format (,~) ,@body)))
