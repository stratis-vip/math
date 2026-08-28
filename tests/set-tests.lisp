(in-package :math/tests)

(clear-suites)
(defsuite set-tests)
(in-suite set-tests)

(test sanity-test
  (check t))

(defun get-value (x)
  (funcall (lambda (x) x ) x))

(test math-set
  (raise-error (math::make-math-set :kind (get-value :best-set)) type-error))


(test has-no-duplicates-p
  (check (has-no-duplicates-p '()))                   ;'() has no duplicates
  (check (has-no-duplicates-p '( 1 2 3 4)))
  (check (has-no-duplicates-p '(nil)))                ; 1meber list
  (check (has-no-duplicates-p '("a" "b" "c") :test #'string= )) ;setting test for strings
  
  (check-not (has-no-duplicates-p '(1 2 3 4 1)))      ; 2 same at start and end
  (check-not (has-no-duplicates-p '(nil 1 2 3 4 nil)))
  (check-not (has-no-duplicates-p '(1 2 3 4 4)))      ; 2 same at end
  (check-not (has-no-duplicates-p '(1 1 2 3 4)))      ; 2 same at start
  (check-not (has-no-duplicates-p (get-value  34)))   ; wrong type
  (check-not (has-no-duplicates-p '("a" "b" "c" "a") :test #'string= ))) ;setting test for strings

(test explicit-sets
  ;;sime wrong make-explicit
  (raise-error (make-explicit-set 45) type-error) ;; members not a list
  (raise-error (make-explicit-set '(1 2 2 1)) simple-error) ;; members has duplicates
  (raise-error (make-explicit-set '(1 2) :test 45) simple-type-error) ;; not a function at TEST
  (raise-error (make-explicit-set '(1 2) :test #'char= :type 'helicopter) simple-type-error) ;; HELICOPTER not a type  (member (number list string symbol char))

  (check (null (math::math-set-members *empty-set*)))
  (check (eql :explicit (math::math-set-kind *empty-set*)))

  (check (explicit-set-p *empty-set*))
  (check (predicate-set-p *N*))
  (check-not (explicit-set-p *N*))

  (check (math-set-p *empty-set*))
  (check (math-set-p *R*))
  (let ((wrong-set (make-explicit-set '(1 2 3))))
    (setf (math::math-set-members wrong-set) '(1 2 3 2))
    (check-not (math-set-p wrong-set))))


(test set->list-tests
  (let ((s-1234 (make-explicit-set '(1 2 3 4) :test #'=)))
    (check (equal '(1 2 3 4) (set->list s-1234)))
    (raise-error (set->list *N*) simple-error)))

(test belongs
  (let ((set-1234 (make-explicit-set '(1 2 3 4)))
	(set-strings (make-explicit-set '("a" "b") :test #'string=)))
    (check (belongs 1 set-1234))
    (check (belongs "a" set-strings))
    (check-not (belongs 3 *empty-set*))
    (check-not (belongs 3 '(1 2 3))) ;;not a set
    ))

(test sub-set-p-tests
  (let ((A (list->set '(1 2)))
	(B (list->set '(1 2 3)))
	(C (list->set '(1 2 3 4))))
    ;; A⊆A
    (check (sub-set-p A A))   

    ;;A⊆C
    (check (sub-set-p A C))

    ;; A⊆B , B⊆C ⇒ A⊆C 
    (check (implies (and (sub-set-p A B) (sub-set-p B C))
		    (sub-set-p  A C)))
    ;; ∀A, ∅⊆A
    (check (sub-set-p *empty-set* *empty-set*))
    (check (sub-set-p *empty-set* A))
    (check (equal-sets A A))
    (check (equal-sets *empty-set* *empty-set*))
    (check-not (equal-sets A B))))
