;;; ffi.lisp ---

;; JSCL is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; JSCL is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with JSCL.  If not, see <http://www.gnu.org/licenses/>.

(/debug "loading ffi.lisp!")

(defvar *root* (%js-vref "window"))

(define-setf-expander oget (object key &rest keys)
  (let* ((keys (cons key keys))
         (g!object (gensym))
         (g!keys (mapcar (lambda (s)
                           (declare (ignore s))
                           (gensym))
                         keys))
         (g!value (gensym)))
    (values `(,g!object ,@g!keys)
            `(,object ,@keys)
            `(,g!value)
            `(oset ,g!value ,g!object ,@g!keys)
            `(oget ,g!object ,@g!keys))))

(define-setf-expander oget* (object key &rest keys)
  (let* ((keys (cons key keys))
         (g!object (gensym))
         (g!keys (mapcar (lambda (s)
                           (declare (ignore s))
                           (gensym))
                         keys))
         (g!value (gensym)))
    (values `(,g!object ,@g!keys)
            `(,object ,@keys)
            `(,g!value)
            `(oset* ,g!value ,g!object ,@g!keys)
            `(oget* ,g!object ,@g!keys))))

(setf #j:eval_in_lisp (lambda (form) (eval (read-from-string form))))

(defmacro bind (fn &rest args)
  (let* ((has-this
	  (and (listp fn) (eql (first fn) 'oget)))
	 (this-arg
	  (if (and has-this (nth 3 fn))
	      (butlast fn)
	      '*root*))
	 (bind-fn
	  (append (if has-this fn (list 'oget fn))
		  '("bind"))))
    `(,bind-fn ,this-arg ,@args)))

(defun alist-to-js-object (alist &key (key #'car) (value #'cadr))
  (let ((obj (new)))
    (dolist (key-value alist obj)
      (setf (oget obj (funcall key key-value))
	    (funcall value key-value)))))

(defun hash-table-to-js-object (hash-table &key (key #'identity) (value #'identity))
  (let ((obj (new)))
    (maphash (lambda (hash-key hash-value)
	       (setf (oget obj (funcall key hash-key))
		     (funcall value hash-value)))
	     hash-table)
    object))	     

(defun symbol-to-js-identifier (symbol)
  (with-output-to-string (js-str)
    (let ((upcase nil)
	  (symbol-str (string symbol))
	  (symbol-char nil))
      (dotimes (i (length symbol-str))
	(setf symbol-char (char symbol-str i))
	(cond
	  ((char= symbol-char #\-)
	   (setf upcase t))
	  (upcase
	   (setf upcase nil)
	   (write-char symbol-char js-str))
	  (t
	   (write-char (char-downcase symbol-char) js-str)))))))

(defun make-js-object (&rest key-values)
  (let ((obj (new)))
    (do* ((tail key-values (cddr tail))
	  (key (first tail) (first tail))
	  (value (second tail) (second tail)))
	 ((null tail) obj)
      (setf (oget obj key)
	    value))))

(defun parse-body (body &key (parse-docstring t))
  (let* ((docstring
	  (when (and (stringp (first body)) (rest body))
	    (first body)))
	 (declarations nil)
	 (last-declaration nil)
	 (done nil))
    (do* ((form (if docstring (rest body) body) (if done form (rest form)))
	  (expr (first form) (first form)))
	 ((or done (null form))
	  (if parse-docstring
	      (values form declarations docstring)
	      (values form declarations)))
      (cond
	((and (listp expr) (eql 'declare (first expr)))
	 (if declarations
	     (setf (cdr last-declaration) (list expr)
		   last-declaration (cdr last-declaration))
	     (setf declarations (list expr)
		   last-declaration declarations)))
	(t
	 (setf done t))))))

(defmacro do-js-object ((binding-form js-object &optional result-form) &body body)
  (destructuring-bind (key-var &optional value-var)
      (if (listp binding-form) binding-form (list binding-form))
    (multiple-value-bind (body-form declarations) (parse-body body :parse-docstring nil)
      (let ((keys (gensym "keys"))
	    (i (gensym "i"))
	    (len (gensym "len")))
	`(let* ((,keys (#j:Object:keys ,js-object))
		(,len (length ,keys)))
	   (dotimes (,i ,len ,result-form)
	     ,@declarations
	     (let* ((,key-var (string (aref ,keys ,i)))
		    ,@(when value-var `((,value-var (oget ,js-object ,key-var)))))
	       ,@body-form)))))))
  
(defun map-js-object (function js-object)
  (do-js-object ((key value) js-object)
    (funcall function key value)))

(defun js-object-to-alist (js-object &key (make-assoc #'list))
  (let ((alist '()))
    (do-js-object ((js-key js-value) js-object alist)
      (push (funcall make-assoc js-key js-value) alist))))

(defun js-object-to-hash-table (js-object &key (key #'identity) (value #'identity) (test #'string=))
  (let ((hash-table (make-hash-table :test test)))
    (do-js-object ((js-key js-value) js-object hash-table)
      (setf (gethash hash-table (funcall key js-key))
	    (funcall value js-value)))))

(defmacro this-bind (this-var &body body)
  `(let ((,this-var this))
     ,@body))
