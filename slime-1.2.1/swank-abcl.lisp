;;;; -*- Mode: lisp; indent-tabs-mode: nil; outline-regexp: ";;;;;*"; -*-
;;;
;;; swank-abcl.lisp --- Armedbear CL specific code for SLIME. 
;;;
;;; Adapted from swank-acl.lisp, Andras Simon, 2004
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed. 
;;;  

(in-package :swank-backend)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :collect) ;just so that it doesn't spoil the flying letters
  (require :pprint))

(defun sys::break (&optional (format-control "BREAK called") 
                   &rest format-arguments)
  (let ((*saved-backtrace* (sys::backtrace-as-list)))
    (with-simple-restart (continue "Return from BREAK.")
      (invoke-debugger
       (sys::%make-condition 'simple-condition
                             (list :format-control format-control
                                   :format-arguments format-arguments))))
    nil))


(defimplementation make-fn-streams (input-fn output-fn)
  (let* ((output (ext:make-slime-output-stream output-fn))
         (input  (ext:make-slime-input-stream input-fn output)))
    (values input output)))

;;; swank-mop

;;dummies:

(defclass standard-slot-definition ()())

(defun class-finalized-p (class) t)

(defun slot-definition-documentation (slot) #+nil (documentation slot 't))
(defun slot-definition-type (slot) t)
(defun class-prototype (class))
(defun generic-function-declarations (gf))
(defun specializer-direct-methods (spec) nil)

(import-to-swank-mop
 '( ;; classes
   cl:standard-generic-function
   standard-slot-definition ;;dummy
   cl:method
   cl:standard-class
   ;; standard-class readers
   sys::class-default-initargs
   sys::class-direct-default-initargs
   sys::class-direct-slots
   sys::class-direct-subclasses
   sys::class-direct-superclasses
   sys::eql-specializer
   class-finalized-p ;;dummy
   cl:class-name
   sys::class-precedence-list
   class-prototype ;;dummy
   sys::class-slots
   specializer-direct-methods ;;dummy
   ;; eql-specializer accessors
   sys::eql-specializer-object
   ;; generic function readers
   sys::generic-function-argument-precedence-order
   generic-function-declarations ;;dummy
   sys::generic-function-lambda-list
   sys::generic-function-methods
   sys::generic-function-method-class
   sys::generic-function-method-combination
   sys::generic-function-name
   ;; method readers
   sys::method-generic-function
   sys::method-function
   sys::method-lambda-list
   sys::method-specializers
   sys::method-qualifiers
   ;; slot readers
   sys::slot-definition-allocation
   slot-definition-documentation ;;dummy
   sys::slot-definition-initargs
   sys::slot-definition-initform
   sys::slot-definition-initfunction
   sys::slot-definition-name
   slot-definition-type ;;dummy
   sys::slot-definition-readers
   sys::slot-definition-writers))

;;;; TCP Server


(defimplementation preferred-communication-style ()
  :spawn)



(defimplementation create-socket (host port)
  (ext:make-server-socket port))


(defimplementation local-port (socket)
  (java:jcall (java:jmethod "java.net.ServerSocket" "getLocalPort") socket))


(defimplementation close-socket (socket)
  (ext:server-socket-close socket))

(defimplementation accept-connection (socket 
                                      &key (external-format :iso-latin-1-unix))
  (assert (eq external-format :iso-latin-1-unix))
  (ext:get-socket-stream (ext:socket-accept socket)))

;;;; Unix signals

(defimplementation call-without-interrupts (fn)
  (funcall fn))

;;there are too many to count
(defimplementation getpid ()
  0)

(defimplementation lisp-implementation-type-name ()
  "armedbear")

(defimplementation set-default-directory (directory)
  (let ((dir (sys::probe-directory directory)))
    (when dir (setf *default-pathname-defaults* dir))
    (namestring dir)))


;;;; Misc


(defimplementation arglist ((symbol t))
  (multiple-value-bind (arglist present)
      (sys::arglist symbol)
    (if present arglist :not-available)))


(defimplementation function-name (function)
  (nth-value 2 (function-lambda-expression function)))

(defimplementation macroexpand-all (form)
  (macroexpand form))

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (flet ((doc (kind &optional (sym symbol))
             (or (documentation sym kind) :not-documented))
           (maybe-push (property value)
             (when value
               (setf result (list* property value result)))))
      (maybe-push
       :variable (when (boundp symbol)
                   (doc 'variable)))
      (maybe-push
       :function (if (fboundp symbol)
                     (doc 'function)))
      (maybe-push
       :class (if (find-class symbol nil)
                  (doc 'class)))
      result)))


(defimplementation describe-definition (symbol namespace)
  (ecase namespace
    (:variable 
     (describe symbol))
    ((:function :generic-function)
     (describe (symbol-function symbol)))
    (:class
     (describe (find-class symbol)))))

(defimplementation describe-definition (symbol namespace)
  (ecase namespace
    (:variable 
     (describe symbol))
    ((:function :generic-function)
     (describe (symbol-function symbol)))
    (:class
     (describe (find-class symbol)))))


;;;; Debugger

(defvar *sldb-topframe*)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let ((*sldb-topframe* (car (ext:backtrace-as-list)) #+nil (excl::int-newest-frame)))
    (funcall debugger-loop-fn)))

(defun nth-frame (index)
  (nth index (ext:backtrace-as-list)))

(defimplementation compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum)))
    (subseq (ext:backtrace-as-list) start end)))

(defimplementation print-frame (frame stream)
  (write-string (string-trim '(#\space #\newline)
                             (prin1-to-string frame))
                stream))

(defimplementation frame-locals (index)
  `((list :name "??" :id 0 :value "??")))


(defimplementation frame-catch-tags (index)
  (declare (ignore index))
  nil)

#+nil
(defimplementation disassemble-frame (index)
  (disassemble (debugger:frame-function (nth-frame index))))

(defimplementation frame-source-location-for-emacs (index)
  (list :error (format nil "Cannot find source for frame: ~A"
                       (nth-frame index))))

#+nil
(defimplementation eval-in-frame (form frame-number)
  (debugger:eval-form-in-context 
   form
   (debugger:environment-of-frame (nth-frame frame-number))))

#+nil
(defimplementation return-from-frame (frame-number form)
  (let ((frame (nth-frame frame-number)))
    (multiple-value-call #'debugger:frame-return 
      frame (debugger:eval-form-in-context 
             form 
             (debugger:environment-of-frame frame)))))
                         
;;; XXX doesn't work for frames with arguments 
#+nil
(defimplementation restart-frame (frame-number)
  (let ((frame (nth-frame frame-number)))
    (debugger:frame-retry frame (debugger:frame-function frame))))
                          
;;;; Compiler hooks

(defvar *buffer-name* nil)
(defvar *buffer-start-position*)
(defvar *buffer-string*)
(defvar *compile-filename*)

(defun handle-compiler-warning (condition)
  #+nil
  (let ((loc (getf (slot-value condition 'excl::plist) :loc)))
    (signal (make-condition
             'compiler-condition
             :original-condition condition
             :severity :warning
             :message (format nil "~A" condition)
             :location (cond (*buffer-name*
                              (make-location 
                               (list :buffer *buffer-name*)
                               (list :position *buffer-start-position*)))
                             (loc
                              (destructuring-bind (file . pos) loc
                                (make-location
                                 (list :file (namestring (truename file)))
                                 (list :position (1+ pos)))))
                             (t  
                              (make-location
                               (list :file *compile-filename*)
                               (list :position 1))))))))

(defimplementation swank-compile-file (*compile-filename* load-p)
  (handler-bind ((warning #'handle-compiler-warning))
                (let ((*buffer-name* nil))
                  (multiple-value-bind (fn warn fail) 
                      (compile-file *compile-filename*)
                    (when load-p (unless fail (load fn)))))))

(defimplementation swank-compile-string (string &key buffer position directory)
  (declare (ignore directory))
  (handler-bind ((warning #'handle-compiler-warning))
    (let ((*buffer-name* buffer)
          (*buffer-start-position* position)
          (*buffer-string* string))
      (funcall (compile nil (read-from-string
                             (format nil "(~S () ~A)" 'lambda string)))))))

#|
;;;; Definition Finding

(defun find-fspec-location (fspec type)
  (let ((file (excl::fspec-pathname fspec type)))
    (etypecase file
      (pathname
       (let ((start (scm:find-definition-in-file fspec type file)))
         (make-location (list :file (namestring (truename file)))
                        (if start
                            (list :position (1+ start))
                            (list :function-name (string fspec))))))
      ((member :top-level)
       (list :error (format nil "Defined at toplevel: ~A" fspec)))
      (null 
       (list :error (format nil "Unkown source location for ~A" fspec))))))

(defun fspec-definition-locations (fspec)
  (let ((defs (excl::find-multiple-definitions fspec)))
    (loop for (fspec type) in defs 
          collect (list fspec (find-fspec-location fspec type)))))

(defimplementation find-definitions (symbol)
  (fspec-definition-locations symbol))

|#

(defun source-location (symbol)
  (when (pathnamep (ext:source-pathname symbol))
    `(((,symbol)
       (:location 
        (:file ,(namestring (ext:source-pathname symbol)))
        (:position ,(or (ext:source-file-position symbol) 0) t)
        (:snippet nil))))))


(defimplementation find-definitions (symbol)
  (source-location symbol))

#| 
Uncomment this if you have patched xref.lisp, as in 
http://article.gmane.org/gmane.lisp.slime.devel/2425
Also, make sure that xref.lisp is loaded by modifying the armedbear
part of *sysdep-pathnames* in swank.loader.lisp. 

;;;; XREF
(setq pxref:*handle-package-forms* '(cl:in-package))

(defmacro defxref (name function)
  `(defimplementation ,name (name)
    (xref-results (,function name))))

(defxref who-calls      pxref:list-callers)
(defxref who-references pxref:list-readers)
(defxref who-binds      pxref:list-setters)
(defxref who-sets       pxref:list-setters)
(defxref list-callers   pxref:list-callers)
(defxref list-callees   pxref:list-callees)

(defun xref-results (symbols)
  (let ((xrefs '()))
    (dolist (symbol symbols)
      (push (list symbol (cadar (source-location symbol))) xrefs))
    xrefs))
|#

;;;; Inspecting

(defclass abcl-inspector (inspector)
  ())

(defimplementation make-default-inspector ()
  (make-instance 'abcl-inspector))

(defmethod inspect-for-emacs ((slot sys::slot-definition) (inspector abcl-inspector))
  (declare (ignore inspector))
  (values "A slot." 
          `("Name: " (:value ,(sys::slot-definition-name slot))
            (:newline)
            "Documentation:" (:newline)
            ,@(when (slot-definition-documentation slot)
                `((:value ,(slot-definition-documentation slot)) (:newline)))
            "Initialization:" (:newline)
            "  Args: " (:value ,(sys::slot-definition-initargs slot)) (:newline)
            "  Form: "  ,(if (sys::slot-definition-initfunction slot)
                             `(:value ,(sys::slot-definition-initform slot))
                             "#<unspecified>") (:newline)
            "  Function: " (:value ,(sys::slot-definition-initfunction slot))
            (:newline))))

(defmethod inspect-for-emacs ((f function) (inspector abcl-inspector))
  (declare (ignore inspector))
  (values "A function."
          `(,@(when (function-name f)
                    `("Name: " 
                      ,(princ-to-string (function-name f)) (:newline)))
            ,@(multiple-value-bind (args present) 
                                   (sys::arglist f)
                                   (when present `("Argument list: " ,(princ-to-string args) (:newline))))
            (:newline)
            #+nil,@(when (documentation f t)
                         `("Documentation:" (:newline) ,(documentation f t) (:newline)))
            ,@(when (function-lambda-expression f)
                    `("Lambda expression:" 
                      (:newline) ,(princ-to-string (function-lambda-expression f)) (:newline))))))

#|

(defimplementation inspect-for-emacs ((o t) (inspector abcl-inspector))
  (let* ((class (class-of o))
         (slots (sys::class-slots class)))
    (values (format nil "~A~%   is a ~A" o class)
            (mapcar (lambda (slot)
                      (let ((name (sys::slot-definition-name slot)))
                        (cons (princ-to-string name)
                              (slot-value o name))))
                    slots))))
|#

;;;; Multithreading

(defimplementation startup-multiprocessing ()
  #+nil(mp:start-scheduler))

(defimplementation spawn (fn &key name)
  (ext:make-thread (lambda () (funcall fn)) :name name))

(defvar *thread-props-lock* (ext:make-thread-lock))

(defvar *thread-props* (make-hash-table) ; should be a weak table
  "A hashtable mapping threads to a plist.")

(defvar *thread-id-counter* 0)

(defimplementation thread-id (thread)
  (ext:with-thread-lock (*thread-props-lock*)
    (or (getf (gethash thread *thread-props*) 'id)
        (setf (getf (gethash thread *thread-props*) 'id)
              (incf *thread-id-counter*)))))

(defimplementation find-thread (id)
  (find id (all-threads) 
        :key (lambda (thread)
                (getf (gethash thread *thread-props*) 'id))))

(defimplementation thread-name (thread)
  (ext:thread-name thread))

(defimplementation thread-status (thread)
  (format nil "Thread is ~:[dead~;alive~]" (ext:thread-alive-p thread)))

(defimplementation make-lock (&key name)
  (ext:make-thread-lock))

(defimplementation call-with-lock-held (lock function)
  (ext:with-thread-lock (lock) (funcall function)))

(defimplementation current-thread ()
  (ext:current-thread))

(defimplementation all-threads ()
  (copy-list (ext:mapcar-threads #'identity)))

(defimplementation interrupt-thread (thread fn)
  (ext:interrupt-thread thread fn))

(defimplementation kill-thread (thread)
  (ext:destroy-thread thread))

(defun mailbox (thread)
  "Return THREAD's mailbox."
  (ext:with-thread-lock (*thread-props-lock*)
    (or (getf (gethash thread *thread-props*) 'mailbox)
        (setf (getf (gethash thread *thread-props*) 'mailbox)
              (ext:make-mailbox)))))

(defimplementation send (thread object)
  (ext:mailbox-send (mailbox thread) object))

(defimplementation receive ()
  (ext:mailbox-read (mailbox (ext:current-thread))))

(defimplementation quit-lisp ()
  (ext:exit))

