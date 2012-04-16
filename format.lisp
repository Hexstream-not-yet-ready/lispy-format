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

(define-~format progn (~) (&rest forms)
  `(progn ,@(formatexpand-forms-partially forms ~)))

(define-~format if (~) (test then &optional (else nil elsep))
  `(if ,test
       ,(formatexpand then ~)
       ,@(when elsep (list (formatexpand else ~)))))

(define-~format when (~) (test &rest body)
  `(when ,test
     ,@(formatexpand-forms body ~)))

(define-~format unless (~) (test &rest body)
  `(unless ,test
     ,@(formatexpand-forms body ~)))

(define-~format cond (~) (&rest clauses)
  `(cond ,@(mapcar
            (lambda (clause)
              (cons (first clause)
                    (formatexpand-forms (rest clause) ~)))
                   clauses)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-caselike (operator ~ keyform cases)
    `(,operator
      ,keyform
      ,@(mapcar
         (lambda (case)
           (cons (first case)
                 (formatexpand-forms (rest case) ~)))
         cases)))

  (defun expand-letlike (operator ~ bindings body)
    `(,operator
      ,(mapcar
        (lambda (binding)
          (if (consp binding)
              (cons (first binding)
                    (formatexpand-forms (rest binding) ~))
              binding))
        bindings)
      ,@body)))

(define-~format case (~) (keyform &body cases)
  (expand-caselike 'case ~ keyform cases))

(define-~format ccase (~) (keyform &body cases)
  (expand-caselike 'ccase ~ keyform cases))

(define-~format ecase (~) (keyform &body cases)
  (expand-caselike 'ecase ~ keyform cases))

(define-~format typecase (~) (keyform &body cases)
  (expand-caselike 'typecase ~ keyform cases))

(define-~format ctypecase (~) (keyform &body cases)
  (expand-caselike 'ctypecase ~ keyform cases))

(define-~format etypecase (~) (keyform &body cases)
  (expand-caselike 'etypecase ~ keyform cases))

(define-~format let (~) (bindings &body body)
  (expand-letlike 'let ~ bindings body))

(define-~format let* (~) (bindings &body body)
  (expand-letlike 'let* ~ bindings body))


#+nil
(defmacro ~error ((~) &body body)
  `(error "~A" (~format (,~) ,@body)))
