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

(defmacro case-char (char &body cases)
  (let ((g!char (gensym "CHAR-")))
    `(let ((,g!char ,char))
       (cond
	 ,@(mapcar (lambda (case-decl)
		     (destructuring-bind (match &rest match-body) case-decl
		       (typecase match
			 (string `((find ,g!char ,match) ,@match-body))
			 (null `((null ,g!char) ,@match-body))
			 (t `(,match ,@match-body)))))
		   cases)))))

(defun rope-merge (rope)
  (let (result last str merged)
    (labels ((collect (x)
	       (if last
		   (setf (cdr last) (cons x nil)
			 last (cdr last))
		   (setf last (cons x nil)))
	       (unless result
		 (setf result last))
	       (setf str nil))
	     (collect-str ()
	       (typecase str
		 (string (collect str)
			 (setf str nil))
		 (stream (collect (get-output-stream-string str))
			 (setf merged t
			       str nil)))))
      (dolist (x rope)
	(when (keywordp x)
	  (setf x (symbol-name x)))
	(cond ((stringp x)
	       (typecase str
		 (string (let ((w str))
			   (setf str (make-string-output-stream))
			   (write-string w str))
			 (write-string x str))
		 (stream (write-string x str))
		 (null (setf str x))))
	      ((null x)
	       (setf merged t))
	      (t
	       (collect-str)
	       (collect x))))
      (collect-str)
      (if merged
	  (values result t)
	  (values rope nil)))))

(defun rope-nmerge (rope)
  (labels ((iter (x)
	     (cond ((and (stringp (car x))
			 (stringp (cadr x)))
		    (setf (car x) (concatenate 'string (car x) (cadr x))
			  (cdr x) (cddr x))
		    (iter x))
		   ((cdr x)
		    (iter (cdr x))))))
    (iter rope))
  rope)

;;  STR

(defgeneric atom-str (x))

(defmethod atom-str (x)
  (format nil "~A" x))

(defmethod atom-str ((x null))
  "")

(defmethod atom-str ((x symbol))
  (string-downcase (symbol-name x)))

(defmethod atom-str ((x string))
  x)

(defmethod atom-str ((x pathname))
  (namestring x))

(defmethod atom-str ((x integer))
  (format nil "~D" x))

(defun walk-str (fn str)
  (labels ((walk (x)
	     (typecase x
	       (string (funcall fn x))
	       (sequence (map nil #'walk x))
	       (t (funcall fn (atom-str x))))))
    (walk str)))

(defun str (&rest parts)
  (with-output-to-string (s)
    (walk-str (lambda (x) (write-string x s))
	      parts)))

(define-compiler-macro str (&whole form &rest parts)
  (let ((merged (rope-merge parts)))
    (if (eq merged parts)
	form
	`(str ,@merged))))

(defun write-str (stream &rest parts)
  (walk-str (lambda (x) (write-string x stream))
	    parts))

(defgeneric to-str (x))

(defmethod to-str (x)
  (atom-str x))

(defmethod to-str ((x sequence))
  (with-output-to-string (out)
    (labels ((str<< (y)
	       (if (typep y 'sequence)
		   (map nil #'str<< y)
		   (write-string (atom-str y) out))))
      (str<< x))))


(defun write-rope (rope &optional (stream *standard-output*))
  (dolist (x rope)
    (write-string x stream)))

(define-compiler-macro write-rope (&whole form rope &rest stream)
  (let ((merged (rope-merge rope)))
    (if (eq merged rope)
	form
	`(write-rope ,merged ,@stream))))
