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

;;  pstr

(defstruct pstr
  (str "" :type string :read-only t)
  (pos 0 :type positive-fixnum))

(defmacro with-pstr ((str pos) x &body body)
  (let ((g!pstr (gensym "PSTR-")))
    `(let ((,g!pstr ,x))
       (let ((,str (pstr-str ,g!pstr))
	     (,pos (pstr-pos ,g!pstr)))
	 ,@body))))

(defun pstr-copy (x)
  (declare (type pstr x))
  (make-pstr :str (pstr-str x) :pos (pstr-pos x)))

(defun pstr-eat-n (x n)
  (declare (type pstr x)
	   (type positive-fixnum n))
  (when (< (length (pstr-str x)) (+ (pstr-pos x) n))
    (error "pstr-eat-n past end of string"))
  (incf (pstr-pos x) n)
  x)

(defun pstr-peek (x &optional (n 0))
  (with-pstr (s p) x
    (let ((i (+ p n)))
      (when (< i (length s))
	(char s i)))))

(defun pstr-eat-char (x char)
  (when (char= char (pstr-peek x))
    (pstr-eat-n x 1)
    char))

(defun pstr-eat-string (x string)
  (with-pstr (s p) x
    (let* ((len (length string))
	   (e (+ p len)))
      (when (and (<= e (length s))
		 (string= s string :start1 p :end1 e))
	(pstr-eat-n x len)
	string))))

(define-constant +hexdigits+ "0123456789ABCDEFabcdef"
  :test 'string=)

(defun pstr-eat-pct-encoded (x)
  (when (and (char= #\% (pstr-peek x))
	     (find (pstr-peek x 1) +hexdigits+)
	     (find (pstr-peek x 2) +hexdigits+))
    (with-pstr (s p) x
      (prog1 (parse-integer s :start p :end (+ 2 p) :radix 16)
	(pstr-eat-n x 3)))))

(defun pstr-eat-if (n fn x)
  (when-let ((r (funcall fn (pstr-peek x))))
    (pstr-eat x n)
    r))
