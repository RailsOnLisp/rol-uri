;;
;;  RoL-uri  -  URL and URI utilities
;;
;;  Copyright 2012-2015 Thomas de Grivel <thomas@lowh.net>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :RoL-uri)

;;  RFC 6570 - URI Template

(defvar *op*)

(define-constant +op-level2+ "+#"
  :test 'string=)

(define-constant +op-level3+ "./;?&"
  :test 'string=)

(define-constant +op-reserve+ "=,!@|"
  :test 'string=)

(define-constant +operator+
    (concatenate 'string +op-level2+ +op-level3+ +op-reserve+)
  :test 'string=)

(defun op-prefix (op)
  (case-char op
    ("#./;?&" op)))

(defvar *separator* #\,)

(defun op-separator (op)
  (case-char op
    ("?&" #\&)
    (t #\,)))

(defun explode-separator (op)
  (case-char op
    ("./;&" op)
    ("?" #\&)
    (t #\,)))

(defclass uri-template ()
  ((parts :type list
	  :initarg :parts
	  :accessor uri-template-parts)
   (expand-function :type function
		    :reader uri-template-expand-function)))

(defun uri-template-p (thing)
  "
URI-TEMPLATE-P returns T if THING is a URI Template or a string with
at least one URI Template operator. Otherwise NIL is returned.
"
  (typecase thing
    (string (find #\{ thing))
    (uri-template t)))

;;  URI vars

(defparameter *uri-vars-package*
  (let ((pkg (find-package :RoL-uri.vars)))
    (when pkg
      (do-symbols (sym pkg)
	(unexport sym pkg)
	(unintern sym pkg)))
    (defpackage :RoL-uri.vars
      (:nicknames :L>uri.vars :lowh.triangle.uri.vars)))
  "All uri variable names are interned in this package.")

(defun uri-var (name)
  "URI-VAR interns NAME in the current *URI-VARS-PACKAGE*."
  (let ((sym (intern (string name) *uri-vars-package*)))
    (export sym *uri-vars-package*)
    sym))

(defun uri-var-p (thing)
  (and (symbolp thing)
       (eq *uri-vars-package* (symbol-package thing))))

(deftype uri-var ()
  "The type of all uri variables."
  `(and symbol
	(satisfies uri-var-p)))

(defmacro uri-let (bindings &body body)
  "Create dynamic bindings for URI variables and lexical bindings to
access them in BODY."
  (let ((bindings (mapcar (lambda (b) (if (consp b) b (list b nil)))
			  bindings)))
    `(let ,(mapcar (lambda (b) `(,(uri-var (car b)) ,(cadr b)))
		   bindings)
       (declare (special ,@(mapcar (lambda (b) (uri-var (car b)))
				   bindings)))
       (symbol-macrolet ,(mapcar (lambda (b)
				   `(,(car b) ,(uri-var (car b))))
				 bindings)
	 ,@body))))

;;  Read

(defun read-uri-template-from-string (string)
  "Read a URI Template string as per RFC 6570 and return an instance of
URI-TEMPLATE."
  (labels ((read/var (s)
	     (cl-ppcre:register-groups-bind (name prefix explode)
		 (#.(str "(" #1="(?:[0-9A-Za-z_]|%[0-9][0-9])"
			 "(?:[.]?" #1# ")*)(?::(.*)|([*]))?$")
		    s)
	       (let ((sym (uri-var (string-upcase name))))
		 (cond (prefix  `(prefix ,(parse-integer prefix) ,sym))
		       (explode `(explode ,sym))
		       (t       sym)))))
	   (read/part (s)
	     (cond ((char= #\{ (char s 0))
		    (let ((start 1)
			  (end (1- (length s))))
		      (assert (char= #\} (char s end)))
		      (let ((op (find (char s 1) +operator+)))
			(when op (incf start))
			(let ((vars (mapcar #'read/var
					    (cl-ppcre:split "," s
							    :start start
							    :end end))))
			  `(,op ,@vars)))))
		   (t s))))
    (let (parts chars)
      (flet ((push-part ()
	       (when chars
		 (push (read/part (coerce (nreverse chars) 'string))
		       parts)
		 (setf chars nil))))
	(dotimes (i (length string))
	  (let ((c (char string i)))
	    (cond
	      ((char= #\{ c) (push-part) (push c chars))
	      ((char= #\} c) (push c chars) (push-part))
	      (t (push c chars)))))
	(push-part))
      (make-instance 'uri-template :parts (nreverse parts)))))

;;  Cache

(defvar *uri-templates* (make-hash-table :test 'equal :weakness :value))

(defun uri-template (thing)
  (typecase thing
    (string (or (gethash thing *uri-templates*)
		(setf (gethash thing *uri-templates*)
		      (read-uri-template-from-string thing))))
    (uri-template thing)))

(defmethod make-load-form ((uri-template uri-template) &optional env)
  (declare (ignore env))
  `(uri-template ,(uri-template-string uri-template)))

#+nil
(define-compiler-macro uri-template (&whole whole string)
  (if (stringp string)
      (uri-template string)
      whole))

#+test
(uri-template-parts (uri-template "/assets/{+path:4}"))

;;  Eval

(defun do-uri-template (template &key (parts #'list) (string #'identity)
				      (op #'list) explode prefix
				      (var #'identity))
  (labels ((do-var (x)
	     (if (symbolp x)
		 (funcall var x)
		 (destructuring-bind (a b &optional c) x
		   (ecase a
		     ((explode) (let ((*separator* (explode-separator *op*)))
				  (funcall explode (funcall var b))))
		     ((prefix) (funcall prefix b (funcall var c)))))))
	   (do-part (part)
	     (typecase part
	       (string (funcall string part))
	       (cons (destructuring-bind (op-char &rest vars) part
		       (let ((*op* op-char)
			     (*separator* (op-separator op-char)))
			 (apply op op-char (remove-if
					    #'null
					    (mapcar #'do-var vars)))))))))
    (when (stringp template)
      (setq template (uri-template template)))
    (apply parts (mapcar #'do-part (uri-template-parts template)))))

;;  Print

(defun uri-template-string (template)
  (do-uri-template template
    :parts #'str
    :op (lambda (op &rest vars) (format nil "{~@[~C~]~{~A~^,~}}" op vars))
    :explode (lambda (var) (format nil "~A*" var))
    :prefix (lambda (n var) (format nil "~A:~D" var n))
    :var #'string-downcase))

#+test
(uri-template-string (uri-template "/assets/{+path*,dir*}"))

(defmethod print-object ((o uri-template) stream)
  (prin1 `(uri-template ,(uri-template-string o)) stream))

;;  Expand

(defmacro with-uri-vars (vars &body body)
  (let ((unbound-vars (gensym "UNBOUND-VARS-"))
	(nils (gensym "NILS")))
    `(locally (declare (special ,@vars)) ;; (sb-ext:muffle-conditions warning)
       (let (,unbound-vars ,nils)
	 (dolist (v ',vars)
	   (when (not (boundp v))
	     (push v ,unbound-vars)
	     (push nil ,nils)))
	 (progv unbound-vars nils
	   ,@body)))))

(defun prefix (n s)
  (when s
    (if (<= (length s) n)
	s
	(subseq s 0 n))))

(defgeneric expand-value (thing)
  (:documentation "Returns the expansion of THING as a string."))

(defmethod expand-value ((x null))
  "")

(defmethod expand-value ((x string))
  (%-encode x nil (if *op* #'uri-char-p #'unreserved-char-p)))

(defmethod expand-value ((x symbol))
  (expand-value (string-downcase (symbol-name x))))

(defmethod expand-value ((x t))
  (expand-value (the string (str x))))

(defun expand-alist (alist separator assoc stream)
  (labels ((eat (l)
	     (unless (endp l)
	       (destructuring-bind (key . value) (first l)
		 (let ((expanded (expand-value value)))
		   (unless (emptyp expanded)
		     (write-char separator stream)
		     (write-string key stream)
		     (write-char assoc stream)
		     (write-string expanded stream))))
	       (eat (rest l))))
	   (eat-first (l)
	     (unless (endp l)
	       (destructuring-bind (key . value) (first l)
		 (let ((expanded (expand-value value)))
		   (cond ((emptyp expanded) (eat-first (rest l)))
			 (:otherwise (write-string key stream)
				     (write-char assoc stream)
				     (write-string expanded stream)
				     (eat (rest l)))))))))
    (eat-first alist)))

(defun expand-list (list separator stream)
  (labels ((eat (l)
	     (unless (endp l)
	       (let ((expanded (expand-value (first l))))
		 (unless (emptyp expanded)
		   (write-char separator stream)
		   (write-string expanded stream)))
	       (eat (rest l))))
	   (eat-first (l)
	     (unless (endp l)
	       (let ((expanded (expand-value (first l))))
		 (cond ((emptyp expanded) (eat-first (rest l)))
		       (:otherwise (write-string expanded stream)
				   (eat (rest l))))))))
    (eat-first list)))

(defmethod expand-value ((x cons))
  (with-output-to-string (o)
    (if (consp (car x))
	(expand-alist x *separator* #\= o)
	(expand-list x *separator* o))))

(defun uri-template-expand-code (template &optional (stream-var 'stream))
  (declare (type symbol stream-var))
  (let (vars)
    (values
     (do-uri-template template
       :string (lambda (x) `(write-string ,x ,stream-var))
       :op (lambda (op &rest v) `(let ((*op* ,op)
				       (*separator* ,*separator*))
				   ,@(when-let ((pre (op-prefix op)))
				       `((write-char ,pre ,stream-var)))
				   (expand-list (list ,@v)
						,*separator* stream)))
       :explode (lambda (v) `(let ((*separator* ,*separator*)) ,v))
       :prefix (lambda (n v) `(prefix ,n ,v))
       :var (lambda (v) (pushnew v vars) `(expand-value ,v)))
     vars)))

#+nil
(uri-template-expand-code "/assets{/plop*}")

(defun compile-uri-template-expand-function (template)
  (format *debug-io* "~&; compiling ~S~%" template)
  (multiple-value-bind (code vars)
      (uri-template-expand-code template 'stream)
    (compile nil `(lambda (stream &key ,@vars)
		    (declare (type stream stream)
			     (special ,@vars))
		    ,@code))))

(defmethod slot-unbound (class
			 (template uri-template)
			 (slot (eql 'expand-function)))
    (setf (slot-value template 'expand-function)
	  (compile-uri-template-expand-function template)))

(defgeneric expand-uri (output uri &rest vars &key &allow-other-keys))

(defmethod expand-uri ((output null) uri &rest vars &key &allow-other-keys)
  (with-output-to-string (stream)
    (apply #'expand-uri stream uri vars)))

(defmethod expand-uri (output (uri string) &rest vars)
  (if (uri-template-p uri)
      (apply #'expand-uri output (uri-template uri) vars)
      uri))

(defmethod expand-uri ((output stream) (uri uri-template) &rest vars)
  (apply (uri-template-expand-function uri) output vars))

#+test
(expand-uri nil "/assets/{path}" :path "Mathématiques")

#+test
(expand-uri nil "/assets/{path}" :path "abc%/")

#+test
(time
 (let ((template (uri-template "/assets/{+path,pat,path}")))
   (dotimes (i 100000)
     (expand-uri nil template :path i))))

#+test
(time
 (let ((template (uri-template "/assets/{+path}")))
   (dotimes (i 8)
     (uri-template-expand template :path i))))

#+test
(time
 (dotimes (i 1000)
   (let ((template (uri-template (format nil "/assets/{+path}/~D" (mod i 10)))))
     (uri-template-expand template :path i))))

;;  Destructuring regex

(defun var-regex (op)
  (let ((char-regex
	 (case-char op
	   (nil '(:alternation
		  (:property not-reserved-char-p)
		  (:char-class #\@)
		  (:sequence #\% :digit-class :digit-class)))
	   ("+#./;?&" `(:alternation
			(:property not-reserved-char-p)
			(:sequence #\% :digit-class :digit-class)
			(:char-class ,@(coerce +reserved+ 'list)))))))
    `(:sequence
      #1=(:greedy-repetition 1 nil ,char-regex)
      (:non-greedy-repetition 0 nil (:sequence ,(op-separator op) #1#)))))

(defun var-name (var)
  (if (consp var)
      (string (second var))
      (string var)))

(defun explode (op)
  (lambda (var)
    (cl-ppcre:split `(:char-class ,(explode-separator op)) var)))

(defun var-binding (op var)
  (if (consp var)
      (case (car var)
	((explode) `(,(explode op) ,(second var)))
	(:otherwise var))
      var))

(defun uri-template-regex (template)
  (let (bindings)
    (values
     (do-uri-template template
       :parts (lambda (&rest parts)
		`(:sequence :start-anchor ,@parts :end-anchor))
       :op (lambda (op &rest vars)
	     `(:sequence
	       ,@(when (op-prefix op) `(,(op-prefix op)))
	       ,@(reduce (lambda (exp var)
			   (push (var-binding op var) bindings)
			   `((:named-register ,(var-name var)
					      ,(var-regex op))
			     ,@(when exp `((:greedy-repetition
					    0 1 (:sequence ,(op-separator op)
							   ,@exp))))))
			 (reverse vars) :initial-value nil)))
       :explode (lambda (var) `(explode ,var))
       :prefix (lambda (n var) `(prefix ,n ,var))
       :var #'uri-var)
     (nreverse bindings))))

(defun uri-template-matcher (template body)
  (let ((string (gensym "STRING-")))
    (multiple-value-bind (regex vars) (uri-template-regex template)
      `(lambda (,string)
	 (cl-ppcre:register-groups-bind ,vars
	     (,(cl-ppcre:create-scanner regex) ,string)
           (declare (ignorable ,@(mapcar (lambda (v) (uri-var (var-name v)))
                                         vars)))
	   ,@body)))))

(defun compile-uri-template-matcher (template body)
  (compile nil (uri-template-matcher template body)))

(defun uri-template-call (template function string)
  (multiple-value-bind (match registers)
      (cl-ppcre:scan-to-strings (uri-template-regex template) string)
    (when match
      (apply function (coerce registers 'list)))))

(defmacro uri-template-bind (vars (template string) &body body)
  `(cl-ppcre:register-groups-bind ,vars ((uri-template-regex ,template)
					 ,string)
     ,@body))

(defmacro uri-template-match ((template string) &body body)
  (multiple-value-bind (regex vars) (uri-template-regex template)
    `(cl-ppcre:register-groups-bind ,vars (,(cl-ppcre:create-scanner regex)
					    ,string)
       ,@body)))
