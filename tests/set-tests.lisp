(in-package :math/tests)


(defsuite set-tests)
(in-suite set-tests)


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
  (let ((A (list->set '(1 2)     :test #'=))
	(B (list->set '(1 2 3)   :test #'=))
	(C (list->set '(1 2 3 4 ) :test #'=))
	(D (list->set '(1 2 3 4) :test #'eql)))

    ;;different tests are not allowed
    (raise-error (sub-set-p A D) simple-error)
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

(test universe
  (let ((universe-0-10 (list->universe '(0 1 2 3 4 5 6 7 8 9 10) :test #'=)) ;Ω={0,1,2.., 10}
	(A-0-3 (list->set '(0 1 2 3) :test #'=))  ;A={0,1,2,3}      
	(B-0-5 (list->set '(0 1 2 3 4 5) :test #'=)) ) ;B={0,1,2,3,4,5}
    
    (check (universe-p universe-0-10))
    (check-not (universe-p a-0-3)) ;Α-0-3 is not a UNIVERSAL-SET
    (check-not
     (sub-universe-p
      (list->set '(10 11) :test #'=)
      universe-0-10))  ;{10, 11}⊄{1,2.., 10}

    (check (sub-universe-p A-0-3 universe-0-10))    ;; A⊆Ω
    (check (sub-universe-p B-0-5 universe-0-10))    ;; B⊆Ω 

    ;; A⊆B ∧ B⊆Ω ⇒ A⊆Ω
    (check
     (implies (and (sub-set-p a-0-3 b-0-5) (sub-universe-p b-0-5 universe-0-10))
	      (sub-universe-p a-0-3 universe-0-10)))
    
    ;; ∅ ⊆ Ω
    (check (sub-universe-p *empty-set* universe-0-10)) 
    ;;∅ ⊆ R
    (check (sub-universe-p *empty-set* (make-universe *R*)))))

(test set-union-test
  (check (empty-set-p (set-union *empty-set* *empty-set*)))

  ;;errors
  ;;not a valid set
  (raise-error (set-union 34 45) simple-error)
  (raise-error (set-union (list->set '()) 45) simple-error)

  ;;no same tests
  (raise-error (set-union (list->set '() :test #'=) (list->set '(3 4) :test #'eq)) simple-error)
  (check (equal-sets (list->set '(3 4) :test #'=)
		     (set-union (list->set '() :test #'=) (list->set '(3 4) :test #'=))))
  
  (check (equal-sets (list->set '(1 2 3 4 5 0))
		     (set-union (list->set '(0))
				(list->set '(1))
				(list->set '(3))
				(list->set '(5))
				(list->set '(4))
				(list->set '(2)))))

  (let ((s-1234 (list->set '(1 2 3 4) :test #'=)))
    ;;union {1,2,3,4} ∪ ℤ ⇒ ℤ
    (check (eq *Z* (set-union s-1234 *Z*)))

    ;;union of sets with some same members
    (check (equal-sets s-1234 (set-union (list->set '(1 2 3) :test #'=) s-1234)))

    ;;union of sets with the same members
    (check (equal-sets s-1234 (set-union s-1234  s-1234))))
  
  )

(test set-intersection-test
  (check  (empty-set-p (set-intersection *empty-set* *empty-set*)))

  ;;errors
  ;;not a valid set
  (raise-error (set-intersection 34 45) simple-error)
  (raise-error (set-intersection (list->set '()) 45) simple-error)

  ;;no same tests
  (let ((s-34 (list->set '(3 4) :test #'=))
	(s-1-9 (list->set '(1 2 3 4 5 6 7 8 9) :test #'=)))
    (raise-error (set-intersection (list->set '() :test #'eq) s-34) simple-error)
    (check (equal-sets s-34
		       (set-intersection (list->set '(3 4 5 6) :test #'=) s-1-9)))

    (check (eq s-34 (set-intersection s-34 (make-predicate-set #'integerp :test #'=))))

    ;; {3,4} ⋂ {12,3,4} ⋂ {1,2,3,4,5,6,7,8,9} = {3,4}
    (check (eq s-34 (set-intersection s-34 (list->set '(12 3 4) :test #'= ) s-1-9)))

    ;; {3,4} ⋂ {7,8} = ∅
    (check (empty-set-p (set-intersection s-34 (list->set '(7 8) :test #'=)))))
  
  )

(test set-complement-test
  (check (equal-sets (list->set '(3 4) :test #'=)
		     (set-complement (list->set '(1 2) :test #'=) (list->set '(1 2 3 4) :test #'=))))

  )
