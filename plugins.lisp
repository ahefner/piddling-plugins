;;;; Framework for reconfigurable application extensions (aka "plugins").

;;;; Copyright (c) 2012 Andy Hefner <ahefner@gmail.com>

;;; The application is modelled as an instance of a CLOS class, with
;;; extensions implemented as mixins. By modifying the application
;;; instance to reflect the active plugins, generic functions and the
;;; standard CLOS method dispatch can be used to extend application
;;; behavior.

;;; As extensions are enabled/disabled, the application object is
;;; modified (via CHANGE-CLASS) at runtime to include the currently
;;; enabled mixins. New subclasses of the application are defined at
;;; runtime to reflect the chosen combination of extensions.

;;; Think carefully before sharing base classes between different
;;; extensions under such a scheme. Think *very* carefully before
;;; defining plugin methods on non-leaf classes.

(defpackage #:piddling-plugins
  (:use :common-lisp)
  (:export
   #:plugin-enabled #:plugin-disabled
   #:enabled-plugins
   #:reconfigure
   #:enable-plugin
   #:disable-plugin
   #:*application*
   #:defun-extensible))

(in-package :piddling-plugins)

;;;; ----------------------------------------
;;;; Dynamic class machinery
;;;; ----------------------------------------

;;; Internals:

(defun derived-name (base mixins)
  (if mixins
      (let* ((*package* (symbol-package base))
             (name (intern (format nil "~W/~{~W~^/~}" base mixins) (symbol-package base))))
        (setf (get name 'application-class-name) base
              (get name 'plugin-mixins) mixins)
        name)
      base))

(defun ensure-configuration-class (base mixins)
  (or (find-class (derived-name base mixins) nil)
      ;; You could also use (make-instance 'standard-class) or
      ;; ensure-class-using-class..
      (eval `(defclass ,(derived-name base mixins) (,base ,@mixins) ()))))

(defun conf-base (derived-name) (or (get derived-name 'application-class-name) derived-name))
(defun conf-plugins (derived-name) (get derived-name 'plugin-mixins))
(defun conf-add-plugin (derived-name plugin-name) (remove-duplicates (append (conf-plugins derived-name) (list plugin-name))))
(defun conf-remove-plugin (derived-name plugin-name) (remove plugin-name (conf-plugins derived-name)))

;;; Public interface:

(defgeneric plugin-enabled (application plugin-name &key &allow-other-keys)
  (:documentation "Notification that a plugin has just been
  enabled. Plugin initialization methods must EQL-specialize on the
  PLUGIN-NAME argument. ")
  (:method (application plugin-name &rest initargs)
    (declare (ignore plugin-name initargs))))

(defgeneric plugin-disabled (application plugin-name)
  (:documentation "Notification that a plugin is about to be
  disabled. Plugin shutdown methods must EQL-specialize on the
  PLUGIN-NAME argument. ")
  (:method (application plugin-name)
    (declare (ignore application plugin-name))))

(defun enabled-plugins (application)
  "List of plugins currently enabled by an application."
  (conf-plugins (type-of application)))

(defgeneric reconfigure (application plugins &rest initargs)
  (:documentation "Reconfigure an application to enable the specified
  plugins. Currently enabled plugins outside this set are disabled.")
  (:method (application plugins &rest initargs)
    ;; Validate selected plugins
    (map nil #'find-class plugins)
    (let* ((name (type-of application))
           (current-plugins (conf-plugins name))
           (enabled-plugins (set-difference plugins current-plugins))
           (disabled-plugins (set-difference current-plugins plugins)))
      ;; Notify plugins about to be disabled.
      (dolist (plugin disabled-plugins)
        (plugin-disabled application plugin))
      ;; Reconfigure the application by calling CHANGE-CLASS.
      (apply #'change-class
             application
             (ensure-configuration-class (conf-base name) plugins)
             initargs)
      ;; Notify new plugins they've been enabled.
      (dolist (plugin enabled-plugins)
        (apply #'plugin-enabled application plugin initargs))
      ;; Return the application object.
      application)))

(defun enable-plugin (application plugin-name &rest initargs)
  "Enable a plugin within an application."
  (apply #'reconfigure application (conf-add-plugin (type-of application) plugin-name) initargs))

(defun disable-plugin (application plugin-name)
  "Disable a plugin within an application."
  (reconfigure application (conf-remove-plugin (type-of application) plugin-name)))

;;;; ----------------------------------------
;;;; Sugar for extensible functions:
;;;; ----------------------------------------

;;; Extensible functions wrap a defun-style definition in a generic
;;; function (prefixed with EXTENDING-) with an additional first
;;; argument, binding the application instance to *application*.
;;; Extensions use this argument to specialize on their mixin. The
;;; body is defined in an unspecialized primary method. A wrapper is
;;; defined using DEFUN which calls the generic function with the
;;; value of *application* as the first argument. Therefore, you must
;;; bind *application* to the application object before calling any
;;; such functions.

(defvar *application*)

;; Massage arglist so we can put it in a method, wrapped by a regular
;; function, without completely obscuring the arglist. This way at
;; least the required args will still be visible in SLIME.

;; (If I had better taste, I'd rip this out and replace with the
;;  trivial equivalent using only APPLY.)

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; This works around a bug present up to SBCL 1.0.55.
  (defun simplified-keyword-name (spec)
    (let ((namespec (first spec)))
      (list
       (etypecase namespec
         (symbol namespec)
         (list
          (if (and (keywordp (first namespec))
                   (string= (first namespec)
                            (second namespec)))
              (first namespec)
              namespec))))))

  (defun wrapper-arguments (arglist)
    (multiple-value-bind (required optional rest keys aok aux)
        (alexandria:parse-ordinary-lambda-list arglist)
      (let ((rest-arg (and (or optional rest keys aok) (gensym))))
        (values
         (if rest-arg
             `(,@required &rest ,rest-arg)
             required)
         required
         rest-arg
         `(,@required
           ,@(and optional (list* '&optional (mapcar #'first optional)))
           ,@(and rest (list '&rest rest))
           ,@(and keys (list* '&key (mapcar #'simplified-keyword-name keys)))
         ,@(and aok (list '&allow-other-keys))
         ,@(and aux (list* '&aux aux))))))))

(defmacro defun-extensible (name arglist &body body)
  (let ((gf-name (intern (format nil "EXTENDING-~A" (symbol-name name)) (symbol-package name))))
    (multiple-value-bind (wrapper-args required-args rest-arg gf-args) (wrapper-arguments arglist)
      `(progn
         (defgeneric ,gf-name (*application* ,@gf-args))
         (defmethod ,gf-name (*application* ,@arglist) ,@body)
         (defun ,name ,wrapper-args (apply #',gf-name *application* ,@required-args ,rest-arg))))))

;;; Test forms

#+(or)
(defun-extensible test-1 ()
  (print 'test-1))

#+(or)
(defun-extensible test-2 (&key (woot 1))
  (print woot))

#+(or)
(defun-extensible test-3 (a b &rest args &key
                            (test t test-p)
                            ((test-2-key test-2-var) 't2 test-2-p)
                             &allow-other-keys)
  (print (list a b test test-p test-2-var test-2-p :args args)))

;;;; ---------------
;;;; References
;;;; ---------------

;;;  Strandh R., Hamer J., Baumann G. "Using Stealth Mixins to Achieve Modularity" (2007)
;;;   Implementation in gsharp - http://common-lisp.net/cgi-bin/gitweb.cgi?p=projects/gsharp/gsharp.git;a=blob;f=utilities.lisp
;;;  Modes in the ESA framework (formerly part of Climacs):
;;;    http://git.boinkor.net/gitweb/mcclim.git/blob/HEAD:/ESA/utils.lisp#l446
;;;  Throwaway classes (comp.lang.lisp): http://groups.google.com/group/comp.lang.lisp/browse_thread/thread/78ef4a5fcd6a1661?pli=1
;;;  AMOP 2.4: http://www.foldr.org/~michaelw/lisp/amop-programmatic-class.lisp
;;;  ContextL - http://common-lisp.net/project/closer/contextl.html
;;;  Dynamic Classes - http://common-lisp.net/project/dynamic-classes/
;;;  ...

