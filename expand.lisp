(defvar *op*)
(defvar *uri-env*)
(defvar *separator*)

(defgeneric expand-value (x))


(defun expand-uri-template (template &optional env)
  (macrolet ((op (op &body body) `(let ((*op* ,op)) ,@body))
	     (var (sym) `(expand-value (getf *uri-env* ,sym)))
	     (explode (var) `(let ((*separator* (op-explode-separator *op*)))
			       ,var)))
    (flet ((prefix (n s) (if (<= (length s) n) s (subseq s 0 n))))
      (let ((*op* nil)
	    (*separator* #\,)
	    (*uri-env* env))
	(uri-template-parts template)))))

(defgeneric expand-value (x)
  (:documentation "Returns the expansion of X as a string"))

(defmethod expand-value ((x string))
  x)

(defun join-alist (alist joiner)
  (with-output-to-string (out)
    (format out "~A=~A" (caar alist) (cdar alist))
    (dolist (i (cdr alist))
      (format out "~A~A=~A" joiner (car i) (cdr i)))))

(defun join-list (list joiner)
  (with-output-to-string (out)
    (format out "~A" (first list))
    (dolist (i (cdr list))
      (format out "~A~A" joiner i))))

(defmethod expand-value ((x cons) (joiner character))
  (if (consp (car x))
      (join-alist x joiner)
      (join-list x joiner)))

(defun var (name &key prefix explode)
  (declare (type symbol name))
  (cond (prefix (prefix (var name :explode explode) prefix))
	(explode (explode (var name :prefix prefix)))
	(t (getf *env* name))))

(defun op (*op* &rest vars)
  (declare (type character *op*))
  (when *op* (write-char *op*))
  (dolist (v vars)
    (destructuring-bind (f &rest args) v
      (declare (type (member var) f))
      (apply (symbol-function f) args))))

(defun uri-template-expand (template &optional stream)
  (if (null stream)
      (with-output-to-string (s) (uri-template-expand template s))
      (dolist (part (uri-template-parts template))
	(typecase part
	  (string (write-string part stream))
	  (cons (destructuring-bind (f &rest args) part
		  (declare (type (member op) f))
		  (apply (symbol-function f) args)))))))

(defmacro uri-template-bind ((template target) &body body)
  (multiple-value-bind (regex vars) (uri-template-regex template)
    `(cl-ppcre:register-groups-bind
	 ,vars
	 (',regex ,target)
       ,@body)))
