;;; SYMBOL-TO-JS-IDENTIFIER
(test-equal (symbol-to-js-identifier :my-js-var) "myJsVar")
(test-equal (symbol-to-js-identifier 'my-js-var) "myJsVar")

;;; ALIST-TO-JS-OBJECT

;; Default args

; 0 case
(test-equal (length (#j:Object:keys (alist-to-js-object '()))) 0)

; 1 case
(test-equal (length (#j:Object:keys (alist-to-js-object '(("a" 11))))) 1)
(test-property-equal (alist-to-js-object '(("a" 11))) "a" 11)

; 3 case
(test-equal (length (#j:Object:keys (alist-to-js-object '(("a" 11) ("b" 22) ("c" 33))))) 3)
(test-property-equal (alist-to-js-object '(("a" 11) ("b" 22) ("c" 33))) "a" 11)
(test-property-equal (alist-to-js-object '(("a" 11) ("b" 22) ("c" 33))) "b" 22)
(test-property-equal (alist-to-js-object '(("a" 11) ("b" 22) ("c" 33))) "c" 33)

;; CDR valued ALIST

; 0 case
(test-equal (length (#j:Object:keys (alist-to-js-object '() :value #'cdr))) 0)

; 1 case
(test-equal (length (#j:Object:keys (alist-to-js-object '(("a" . 11)) :value #'cdr))) 1)
(test-property-equal (alist-to-js-object '(("a" . 11)) :value #'cdr) "a" 11)

; 3 case

(test-equal (length (#j:Object:keys
		     (alist-to-js-object '(("a" . 11) ("b" . 22) ("c" . 33)) :value #'cdr)))
	    3)

(test-property-equal (alist-to-js-object '(("a" . 11) ("b" . 22) ("c" . 33)) :value #'cdr)
		     "a"
		     11)

(test-property-equal (alist-to-js-object '(("a" . 11) ("b" . 22) ("c" . 33)) :value #'cdr)
		     "b"
		     22)
(test-property-equal (alist-to-js-object '(("a" . 11) ("b" . 22) ("c" . 33)) :value #'cdr)
		     "c"
		     33)

;; With :KEY arg

; 0 case
(test-equal (length (#j:Object:keys (alist-to-js-object '() :key #'caar))) 0)

; 1 case
(test-equal (length (#j:Object:keys (alist-to-js-object '((("a") 11)) :key #'caar))) 1)
(test-property-equal (alist-to-js-object '((("a") 11)) :key #'caar) "a" 11)

; 3 case
(test-equal (length (#j:Object:keys
		     (alist-to-js-object '((("a") 11) (("b") 22) (("c") 33)) :key #'caar)))
	    3)

(test-property-equal (alist-to-js-object '((("a") 11) (("b") 22) (("c") 33)) :key #'caar)
		     "a"
		     11)
(test-property-equal (alist-to-js-object '((("a") 11) (("b") 22) (("c") 33)) :key #'caar)
		     "b"
		     22)
(test-property-equal (alist-to-js-object '((("a") 11) (("b") 22) (("c") 33)) :key #'caar)
		     "c"
		     33)






