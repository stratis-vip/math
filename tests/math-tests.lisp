(in-package :math/tests)


(defsuite math-tests)
(in-suite math-tests)

(test sanity-test
      (check t)
  )
 
(test isotita
  (check-for-all ((a -5 10)
		  (b -5 10)
		  (c -5 10))
    (= (+ a c) (+ c a ))
    (= (+ a (+ b c)) (+ (+ a b) c))
    (= (* a b) ( * b a))
    (= (* a (* b c) ) ( * ( * a b) c)))
  
  )

(test sinepagogi
  (check-for-all ((a -10 10)
		  (b -10 10)
		  (c -10 10))
    (implies (and
	      (equal a b)
	      (equal b c))
	     (equal a c))
    (implies (= a b) (= (+ a c) (+ b c)))
    (implies (= a b) (= (* a c) (* b c)))) 

  )
