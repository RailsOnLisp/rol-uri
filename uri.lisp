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

(defun char-unreserved-p (c)
  (or (<= (char-code #\A) (char-code c) (char-code #\Z))
      (<= (char-code #\a) (char-code c) (char-code #\z))
      (<= (char-code #\0) (char-code c) (char-code #\9))
      (find c "-._~")))

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
