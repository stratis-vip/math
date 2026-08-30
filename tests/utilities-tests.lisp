;;; file MATH:tests/utilities-tests.lisp -- tests about utilities
;;;
;;; Code:

(in-package :math/tests)


(defsuite utilities-tests)
(in-suite utilities-tests)


(test has-more-than-n-p
 
  ;;invalid input retrurn nil
  (check-not (has-more-than-n-p 34 1))
  (check-not (has-more-than-n-p '(3) :b))
  (check-not (has-more-than-n-p '(3) 0))
  (check-not (has-more-than-n-p nil 1))

  ;;n = length list
  (check-not (has-more-than-n-p '(1 2 3 4) 4))
  (check-not (has-more-than-n-p '(1 2 3 4) 5 :equal t))
  (check-not (has-more-than-n-p '(1 2 3 4) 1 :equal t))
  
  (check (has-more-than-n-p '(1 2 3 4) 4 :equal t))
  
  ;;happy end
  (check (has-more-than-n-p (loop for i from 1 to 50000 collect i) 2))
  (check (has-more-than-n-p '(2 3) 1))

  )

;;; file MATH:tests/utilities-tests.lisp ends here
