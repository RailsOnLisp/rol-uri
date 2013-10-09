;;
;;  Triangle
;;
;;  Copyright 2012,2013 Thomas de Grivel <thomas@lowh.net>
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

(in-package :L>uri)

;;  RFC 6570 - URI Template

(defvar *separator* #\,)

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

;;  Cache

(defvar *uri-templates* (make-hash-table :test 'equal :weakness :value))

;;  Reader

(defun uri-template (string)
  (or (gethash string *uri-templates*)
      (setf (gethash string *uri-templates*)
	    (labels ((parse/var (s)
		       (cl-ppcre:register-groups-bind (name prefix explode)
			   (#.(str "(" #1="(?:[0-9A-Za-z_]|%[0-9][0-9])"
				   "(?:[.]?" #1# ")*)(?::(.*)|([*]))?$")
			      s)
			 (let ((sym (intern (string-upcase name))))
			   (cond (prefix  `(prefix ,(parse-integer prefix) ,sym))
				 (explode `(explode ,sym))
				 (t       sym)))))
		     (parse/part (s)
		       (cond ((char= #\{ (char s 0))
			      (let ((start 1)
				    (end (1- (length s))))
				(assert (char= #\} (char s end)))
				(let ((op (find (char s 1) +operator+)))
				  (when op (incf start))
				  (let ((vars (mapcar #'parse/var
						      (cl-ppcre:split "," s
								      :start start
								      :end end))))
				    `(,op ,@vars)))))
			     (t s))))
	      (let (parts chars)
		(flet ((push-part ()
			 (when chars
			   (push (parse/part (coerce (nreverse chars) 'string))
				 parts)
			   (setf chars nil))))
		  (dotimes (i (length string))
		    (let ((c (char string i)))
		      (cond
			((char= #\{ c) (push-part) (push c chars))
			((char= #\} c) (push c chars) (push-part))
			(t (push c chars)))))
		  (push-part))
		(make-instance 'uri-template :parts (nreverse parts)))))))

#+nil
(define-compiler-macro uri-template (&whole whole string)
  (if (stringp string)
      (uri-template string)
      whole))

#+test
(uri-template-parts (uri-template "/assets/{+path:4}"))

;;  Printer

(defvar *op*)

(defmacro ignore-unbound (&body body)
  `(handler-case (locally (declare (sb-ext:muffle-conditions warning))
		   ,@body)
     (unbound-variable () nil)
     (warning (w) (format t "~&~S~%" w))))

(defun uri-template-parse (template &key (parts #'list) (string #'identity)
				      (op #'list) explode prefix
				      (var #'identity))
  (labels ((parse-var (x)
	     (if (symbolp x)
		 (funcall var x)
		 (destructuring-bind (a b &optional c) x
		   (ecase a
		     ((explode) (let ((*separator* (explode-separator *op*)))
				  (funcall explode (funcall var b))))
		     ((prefix) (funcall prefix b (funcall var c)))))))
	   (parse-part (part)
	     (typecase part
	       (string (funcall string part))
	       (cons (destructuring-bind (op-char &rest vars) part
		       (let ((*op* op-char)
			     (*separator* (op-separator op-char)))
			 (apply op op-char (remove-if
					    #'null
					    (mapcar #'parse-var vars)))))))))
    (when (stringp template)
      (setq template (uri-template template)))
    (apply parts (mapcar #'parse-part (uri-template-parts template)))))

(defun uri-template-string (template)
  (uri-template-parse
   template
   :parts #'str
   :op (lambda (op &rest vars) (format nil "{~@[~C~]~{~A~^,~}}" op vars))
   :explode (lambda (var) (format nil "~A*" var))
   :prefix (lambda (n var) (format nil "~A:~D" var n))
   :var #'string-downcase))

#+test
(uri-template-string (uri-template "/assets/{+path*,dir*}"))

(defmethod print-object ((o uri-template) stream)
  (prin1 `(uri-template ,(uri-template-string o)) stream))

;;  Expansion

(defvar *op*)
(defvar *separator*)

(defun prefix (n s)
  (when s
    (if (<= (length s) n)
	s
	(subseq s 0 n))))

(defgeneric expand-value (x)
  (:documentation "Prints the expansion of X to *STANDARD-OUTPUT*"))

(defmethod expand-value ((x null)))

(defmethod expand-value ((x string))
  x)

(defmethod expand-value ((x number))
  (format nil "~D" x))

(defun expand-alist (alist separator assoc stream)
  (unless (endp alist)
    (princ (caar alist) stream)
    (princ assoc stream)
    (princ (cdar alist) stream)
    (dolist (a (cdr alist))
      (princ separator stream)
      (princ (car a) stream)
      (princ assoc stream)
      (princ (cdr a) stream))))

(defun expand-list (list separator stream)
  (unless (endp list)
    (princ (car list) stream)
    (dolist (a (cdr list))
      (princ separator stream)
      (princ a stream))))

(defmethod expand-value ((x cons))
  (with-output-to-string (o)
    (if (consp (car x))
	(expand-alist x *separator* #\= o)
	(expand-list x *separator* o))))

(defun uri-template-expander (template)
  (let ((vars))
    (values
     (uri-template-parse
      template
      :parts (lambda (&rest p) `(with-output-to-string (o) ,@p))
      :string (lambda (s) `(princ ,s o))
      :op (lambda (op &rest v) `(let ((*separator* ,*separator*))
				  ,@(when-let ((pre (op-prefix op)))
					      `((write-char ,pre o)))
				  (expand-list (remove-if #'null (list ,@v))
					       ,*separator* o)))
      :explode (lambda (v) `(let ((*separator* ,*separator*)) ,v))
      :prefix (lambda (n v) `(prefix ,n ,v))
      :var (lambda (v) (pushnew v vars) `(expand-value (ignore-unbound ,v))))
     vars)))

(defmethod slot-unbound (class
			 (template uri-template)
			 (slot (eql 'expand-function)))
  (setf (slot-value template 'expand-function)
	(let ((code (multiple-value-bind (proc vars)
			(uri-template-expander template)
		      `(lambda (&key ,@vars) ,proc))))
	  (compile nil code))))

(defun uri-template-expand (template &rest env &key &allow-other-keys)
  (when (stringp template)
    (setf template (uri-template template)))
  (apply (uri-template-expand-function template) env))

#+test nil
(time
 (let ((template (uri-template "/assets/{+path}")))
   (uri-template-expand template :path "plop")))

(time
 (let ((template (uri-template "/assets/{+path}")))
   (dotimes (i 8)
     (uri-template-expand template :path i))))

(time
 (dotimes (i 1000)
   (let ((template (uri-template (format nil "/assets/{+path}/~D" (mod i 10)))))
     (uri-template-expand template :path i))))

;;  Destructuring regex

(defun uri-template-regex (template)
  (let ((template (etypecase template
		    (string (uri-template template))
		    (cons template)))
	(registers nil))
    (values
     `(:sequence
       ,@(mapcar (lambda (token)
		   (etypecase token
		     (string token)
		     (symbol (push token registers)
			     '(:register (:greedy-repetition
					  1 nil (:inverted-char-class #\/))))))
		 template)
       (:greedy-repetition 0 1 "/")
       :end-anchor)
     (nreverse registers))))
