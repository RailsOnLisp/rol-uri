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

;;  Canonical URI

(defun canonical-document-uri (uri)
  (or (when (string= "/" uri)
	uri)
      (cl-ppcre:regex-replace
       "/$" (cl-ppcre:regex-replace "//" uri "/")
       "")))

;;  RFC 3986 - URI

(define-constant +gen-delims+ ":/?#[]@" :test 'string=)
(define-constant +sub-delims+ "!$&'()*+,;=" :test 'string=)
(define-constant +reserved+ (str +gen-delims+ +sub-delims+)
  :test 'string=)
(define-constant +unreserved+ (str "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
				   "abcdefghijklmnopqrstuvwxyz"
				   "0123456789-._~")
  :test 'string=)

(defun unreserved-char-p (c)
  (or (<= (char-code #\A) (char-code c) (char-code #\Z))
      (<= (char-code #\a) (char-code c) (char-code #\z))
      (<= (char-code #\0) (char-code c) (char-code #\9))
      (find c "-._~")))

(defun not-reserved-char-p (c)
  (not (find c +reserved+)))

(defun %-encode-char (c &key stream (reserved +reserved+))
  (if (null stream)
      (with-output-to-string (s)
	(%-encode-char c :stream s :reserved reserved))
      (if (find c reserved)
	  (let ((b (trivial-utf-8:string-to-utf-8-bytes
		    (make-string 1 :initial-element c))))
	    (dotimes (i (length b))
	      (write-char #\% stream)
	      (write (svref b i) :base 16 :case :upcase :stream stream)))
	  (write-char c stream))))

(defun uri-char-p (c)
  (or (unreserved-char-p c)
      (find c +reserved+)))

(defun hex-digit-p (c)
  (when (or (char<= #\0 c #\9)
	    (char<= #\A c #\Z)
	    (char<= #\a c #\z))
    c))

(defun %-encode-bytes (bytes &optional stream)
  (let ((*print-base* 16)
	(len (length bytes)))
    (labels ((eat (i)
	       (when (< i len)
		 (write-char #\% stream)
		 (write (the (unsigned-byte 8) (aref bytes i))
			:base 16 :stream stream)
		 (eat (1+ i)))))
      (eat 0))))

(defun %-encode (string &optional stream (allowed-char-p #'uri-char-p))
  (let ((len (length string)))
    (labels ((eat (i)
	       (cond ((<= len i) nil)
		     ((and (< (+ 2 i) len)
			   (char= #\% (char string i))
			   (hex-digit-p (char string (+ 1 i)))
			   (hex-digit-p (char string (+ 2 i))))
		      (write-string string stream :start i :end (+ 3 i))
		      (eat (+ 3 i)))
		     ((funcall allowed-char-p (char string i))
		      (write-char (char string i) stream)
		      (eat (1+ i)))
		     (t
		      (%-encode-bytes
		       (trivial-utf-8:string-to-utf-8-bytes
			(subseq string i (1+ i)))
		       stream)
		      (eat (1+ i))))))
      (if (null stream)
	  (with-output-to-string (out)
	    (setq stream out)
	    (eat 0))
	  (eat 0)))))

#+test
(%-encode "plop/%é%C3")

(defun unaccent (c)
  (or (cl-ppcre:register-groups-bind (name)
	  ("(.*) WITH .*" (cl-unicode:unicode-name c))
	(cl-unicode:character-named name))
      c))

(defun to-url (str)
  (string-trim
   "-"
   (with-output-to-string (out)
     (let ((len (length str)))
       (labels ((out (c)
		  (write-char (unaccent (char-downcase c)) out))
		(nohyphen (i)
		  (when (< i len)
		    (let ((c (char str i)))
		      (if (alphanumericp c)
			  (progn (out c)
				 (hyphen (1+ i)))
			  (nohyphen (1+ i))))))
		(hyphen (i)
		  (when (< i len)
		    (let ((c (char str i)))
		      (if (alphanumericp c)
			  (progn (out c)
				 (hyphen (1+ i)))
			  (progn (write-char #\- out)
				 (nohyphen (1+ i))))))))
	 (nohyphen 0))))))
