(in-package :math/tests)

;;; ============================================================================
;;; SET TESTS
;;; ============================================================================

(defsuite set-tests)
(in-suite set-tests)

;;; ============================================================================
;;; HELPERS
;;; ============================================================================

(defun get-value (x)
(funcall (lambda (x) x) x))

;;; ============================================================================
;;; LIST -> MATH NOTATION
;;; ============================================================================

(test list->math-notation
  ;; numbers
  (check (string= "{1, 2, 3}"
		  (math::list->math-notation '(1 2 3))))

  ;; symbols
  (check (string= "{A, B, C}"
		  (math::list->math-notation '(A B C) 'symbol)))

 
  ;; characters
  (check (string= "{'a', 'b'}"
		  (math::list->math-notation '(#\a #\b) 'char)))

  ;; lists
  (check (string= "{(1 2), (3 4)}"
		  (math::list->math-notation '((1 2) (3 4)) 'list)))

  ;; empty list
  (check (string= "{}"
		  (math::list->math-notation '())))

  ;; invalid type
  (raise-error
      (math::list->math-notation '(1 2) 'helicopter)
      simple-error))

;;; ============================================================================
;;; MATH-SET
;;; ============================================================================

(test math-set
  ;; invalid KIND
  (raise-error
      (math::make-math-set :kind (get-value :best-set))
      type-error)

  ;; valid explicit set
  (let ((set (math::make-math-set
	      :kind :explicit
	      :members '(1 2 3)
	      :test #'eql)))
    (check (math::math-set-p set))
    (check (eq :explicit (math::math-set-kind set)))
    (check (equal '(1 2 3) (math::math-set-members set))))

  ;; valid predicate set
  (let ((set (math::make-math-set
	      :kind :predicate
	      :members #'integerp
	      :test #'=)))
    (check (math::math-set-p set))
    (check (eq :predicate (math::math-set-kind set)))))

(test math-set-p
  ;; valid explicit set
  (check
   (math::math-set-p
    (make-explicit-set '(1 2 3))))

  ;; valid predicate set
  (check
   (math::math-set-p
    (make-predicate-set #'integerp)))

  ;; explicit set with duplicates is invalid
  (let ((set (make-explicit-set '(1 2 3))))
    (setf (math::math-set-members set) '(1 2 2 3))
    (check-not (math::math-set-p set)))
  
  ;; predicate set with non-function members is invalid
  (let ((set (make-predicate-set #'integerp)))
    (setf (math::math-set-members set) '(1 2 3))
    (check-not (math::math-set-p set))))

;;; ============================================================================
;;; EXPLICIT SETS
;;; ============================================================================

(test explicit-sets
  ;; members must be a list
  (raise-error
      (make-explicit-set 45)
      type-error)

  ;; duplicate members
  (raise-error
      (make-explicit-set '(1 2 2 1))
      simple-error)

  ;; TEST must be a function
  (raise-error
      (make-explicit-set '(1 2) :test 45)
      simple-type-error)

  ;; TYPE must be valid
  (raise-error
      (make-explicit-set '(1 2)
			 :test #'char=
			 :type 'helicopter)
      simple-type-error)

  ;; empty set
  (check (null (math::math-set-members *empty-set*)))
  (check (eql :explicit
	      (math::math-set-kind *empty-set*)))

  ;; predicates
  (check (explicit-set-p *empty-set*))
  (check (predicate-set-p *N*))
  (check-not (explicit-set-p *N*))

  ;; math-set-p
  (check (math-set-p *empty-set*))
  (check (math-set-p *R*))

  ;; manually corrupt an explicit set
  (let ((wrong-set (make-explicit-set '(1 2 3))))
  (setf (math::math-set-members wrong-set) '(1 2 3 2))
    (check-not (math-set-p wrong-set))))

;;; ============================================================================
;;; PREDICATE SETS
;;; ============================================================================

(test predicate-sets
  ;; predicate must be a function
  (raise-error
      (make-predicate-set 45)
      type-error)

  ;; normal predicate set
  (let ((set (make-predicate-set #'integerp)))
    (check (predicate-set-p set))
    (check-not (explicit-set-p set))
    (check (math-set-p set)))

  ;; predefined sets
  (check (predicate-set-p *N*))
  (check (predicate-set-p *Z*))
  (check (predicate-set-p *R*))

  ;; predicates actually work
  (check (funcall (math::math-set-members *N*) 0))
  (check (funcall (math::math-set-members *N*) 10))
  (check-not (funcall (math::math-set-members *N*) -1))
  
  (check (funcall (math::math-set-members *Z*) -10))
  (check (funcall (math::math-set-members *R*) 3.14)))

;;; ============================================================================
;;; EMPTY SET
;;; ============================================================================

(test empty-set
  (check (empty-set-p *empty-set*))
  
  (check-not (empty-set-p *N*))
  (check-not (empty-set-p *Z*))
  (check-not (empty-set-p *R*))

  ;; explicitly constructed empty set
  (check (empty-set-p (make-explicit-set '()))))

;;; ============================================================================
;;; SET -> LIST
;;; ============================================================================

(test set->list-tests
  (let ((s-1234 (make-explicit-set '(1 2 3 4) :test #'=)))

    (check (equal '(1 2 3 4)
		  (set->list s-1234)))

    ;; predicate sets cannot be converted to a finite list
    (raise-error
	(set->list *N*)
	simple-error)))


;;; ============================================================================
;;; BELONGS
;;; ============================================================================

(test belongs
(let ((set-1234
(make-explicit-set '(1 2 3 4)))
(set-strings
(make-explicit-set '("a" "b") :test #'string=)))


;; explicit set
(check (belongs 1 set-1234))
(check (belongs 4 set-1234))
(check-not (belongs 5 set-1234))

;; string equality
(check (belongs "a" set-strings))
(check (belongs "b" set-strings))
(check-not (belongs "c" set-strings))

;; empty set
(check-not (belongs 3 *empty-set*))

;; predicate sets
(check (belongs 10 *Z*))
(check-not (belongs 3.5 *Z*))

;; invalid set
(check-not (belongs 3 '(1 2 3)))))


;;; ============================================================================
;;; SUBSET
;;; ============================================================================

(test sub-set-p-tests
(let ((A (list->set '(1 2)
:test #'=))
(B (list->set '(1 2 3)
:test #'=))
(C (list->set '(1 2 3 4)
:test #'=))
(D (list->set '(1 2 3 4)
:test #'eql)))


;; reflexivity
(check (sub-set-p A A))
(check (sub-set-p B B))

;; proper subsets
(check (sub-set-p A B))
(check (sub-set-p A C))
(check (sub-set-p B C))

;; transitivity
(check
 (implies
  (and (sub-set-p A B)
       (sub-set-p B C))
  (sub-set-p A C)))

;; empty set
(check (sub-set-p *empty-set* *empty-set*))
(check (sub-set-p *empty-set* A))
(check (sub-set-p *empty-set* *N*))

;; not a subset
(check-not (sub-set-p B A))

;; different equality tests
(raise-error
 (sub-set-p A D)
 simple-error)

;; predicate subset
(check (sub-set-p A *Z*))

;; explicit set containing non-integers is not subset of Z
(let ((R-set (list->set '(1 2.5)
                        :test #'=)))
  (check-not (sub-set-p R-set *Z*)))))


;;; ============================================================================
;;; EQUAL SETS
;;; ============================================================================

(test equal-sets
(let ((A (list->set '(1 2 3) :test #'=))
(B (list->set '(3 2 1) :test #'=))
(C (list->set '(1 2 4) :test #'=))
(D (list->set '(1 2 3) :test #'eql)))


;; same members, different order
(check (equal-sets A B))

;; same object
(check (equal-sets A A))

;; different members
(check-not (equal-sets A C))

;; empty sets
(check (equal-sets *empty-set*
                   *empty-set*))

;; different equality tests
(raise-error
 (equal-sets A D)
 simple-error)))


;;; ============================================================================
;;; UNIVERSE
;;; ============================================================================

(test universe
(let ((universe-0-10
(list->universe
'(0 1 2 3 4 5 6 7 8 9 10)
:test #'=))
(A-0-3
(list->set '(0 1 2 3)
:test #'=))
(B-0-5
(list->set '(0 1 2 3 4 5)
:test #'=)))


;; universe-p
(check (universe-p universe-0-10))
(check-not (universe-p A-0-3))

;; universe -> list
(check
 (equal
  '(0 1 2 3 4 5 6 7 8 9 10)
  (universe->list universe-0-10)))

;; subsets of universe
(check (sub-universe-p A-0-3 universe-0-10))
(check (sub-universe-p B-0-5 universe-0-10))

;; not a subset
(check-not
 (sub-universe-p
  (list->set '(10 11) :test #'=)
  universe-0-10))

;; transitivity
(check
 (implies
  (and (sub-set-p A-0-3 B-0-5)
       (sub-universe-p B-0-5 universe-0-10))
  (sub-universe-p A-0-3 universe-0-10)))

;; empty set is subset of universe
(check
 (sub-universe-p *empty-set*
                 universe-0-10))

;; R as universe
(check
 (sub-universe-p
  *empty-set*
  (make-universe *R*))))


;; invalid universe
(raise-error
(make-universe 45)
simple-error)

;; invalid universe->list
(raise-error
    (universe->list 45)
    simple-error))

;;; ============================================================================
;;; UNION
;;; ============================================================================

(test set-union-test
;; empty ∪ empty = empty
  (check
   (empty-set-p
    (set-union *empty-set*
	       *empty-set*)))

;; invalid sets
  (raise-error
      (set-union 34 45)
      simple-error)

  (raise-error
      (set-union (list->set '()) 45)
      simple-error)

  ;; incompatible tests
  (raise-error
      (set-union
       (list->set '() :test #'=)
       (list->set '(3 4) :test #'eq))
      simple-error)
  
  ;; explicit ∪ explicit
  (let ((A (list->set '(1 2 3) :test #'=))
	(B (list->set '(3 4 5) :test #'=)))
    

    (check
     (equal-sets
      (list->set '(1 2 3 4 5) :test #'=)
      (set-union A B)))

    ;; duplicate members
    (check
     (equal-sets
      A
      (set-union A A)))
    
    ;; empty is identity
    (check
     (equal-sets
      A
      (set-union A (make-explicit-set nil :test #'=)))))


  ;; explicit ∪ predicate
  (let ((A (list->set '(1 2 3) :test #'=)))


    ;; A ⊆ Z => A ∪ Z = Z
    (check
     (eq *Z*
	 (set-union A *Z*)))

    ;; members outside Z
    (let ((B (list->set '(1 2.5) :test #'=)))
      (check
       (belongs 2.5
		(set-union B *Z*)))))


;; predicate ∪ explicit
(let ((A (list->set '(1 2 3) :test #'=)))
(check
(eq *Z*
(set-union *Z* A))))

;; predicate ∪ predicate
(let ((positive
(make-predicate-set
(lambda (x)
(> x 0))
:test #'=))
(even
(make-predicate-set
(lambda (x)
(and (integerp x)
(= (mod x 2) 0)))
:test #'=)))


(let ((union (set-union positive even)))
  (check (predicate-set-p union))
  (check (belongs 3 union))
  (check (belongs 4 union))
  (check-not (belongs -3 union))))


;; multiple sets
(check
(equal-sets
(list->set '(1 2 3 4 5) :test #'=)
(set-union
(list->set '(1 2) :test #'=)
(list->set '(2 3) :test #'=)
(list->set '(3 4) :test #'=)
(list->set '(4 5) :test #'=)))))

;;; ============================================================================
;;; INTERSECTION
;;; ============================================================================

(test set-intersection-test
  ;; empty ∩ empty = empty
  (check
   (empty-set-p
    (set-intersection
     *empty-set*
     *empty-set*)))

  ;; invalid sets
  (raise-error
      (set-intersection 34 45)
      simple-error)

  (raise-error
      (set-intersection (list->set '()) 45)
      simple-error)

  ;; incompatible tests
  (raise-error
      (set-intersection
       (list->set '() :test #'=)
       (list->set '(1 2) :test #'eq))
      simple-error)
  
  ;; explicit ∩ explicit
  (let ((A (list->set '(1 2 3 4) :test #'=))
	(B (list->set '(3 4 5 6) :test #'=)))


    (check
     (equal-sets
      (list->set '(3 4) :test #'=)
      (set-intersection A B)))
    
    ;; A ∩ A = A
    (check
     (equal-sets
      A
      (set-intersection A A)))
    
    ;; A ∩ empty = empty
    (check
     (empty-set-p
      (set-intersection A (make-explicit-set nil :test #'=)))))


;; explicit ∩ predicate
(let ((A (list->set '(1 2 3 4) :test #'=)))
(check
(equal-sets
A
(set-intersection A *Z*)))


;; only integers survive
(let ((B (list->set '(1 2.5 3) :test #'=)))
  (check
   (equal-sets
    (list->set '(1 3) :test #'=)
    (set-intersection B *Z*)))))


;; predicate ∩ explicit
(let ((A (list->set '(1 2 3 4) :test #'=)))
(check
(equal-sets
A
(set-intersection *Z* A))))

;; predicate ∩ predicate
(let ((positive
(make-predicate-set
(lambda (x)
(> x 0))
:test #'=))
(even
(make-predicate-set
(lambda (x)
(and (integerp x)
(= (mod x 2) 0)))
:test #'=)))


(let ((intersection
        (set-intersection positive even)))
  (check (predicate-set-p intersection))
  (check (belongs 2 intersection))
  (check (belongs 4 intersection))
  (check-not (belongs 3 intersection))
  (check-not (belongs -2 intersection))))


;; multiple intersections
(let ((A (list->set '(1 2 3 4 5 6) :test #'=))
(B (list->set '(2 3 4 5) :test #'=))
(C (list->set '(3 4 5 6) :test #'=)))


(check
 (equal-sets
  (list->set '(3 4 5) :test #'=)
  (set-intersection A B C)))))


;;; ============================================================================
;;; COMPLEMENT
;;; ============================================================================

(defparameter *empty-set-test=* (make-explicit-set nil :test #'=))

(test set-complement-test
  ;; A \ B, with both explicit
  (let ((A (list->set '(1 2) :test #'=))
	(OMEGA (list->set '(1 2 3 4) :test #'=)))

    ; Α\Ω = ∅
    (check (empty-set-p (set-complement A OMEGA))))


  ;; A\A =  ∅
  (let ((A (list->set '(1 2 3) :test #'=)))
    (check (empty-set-p (set-complement A A))))

  ;;  ∅ \ A =  ∅
  (let ((A (list->set '(1 2 3) :test #'=)))
    (check
     (empty-set-p (set-complement *empty-set-test=* A))))


  ;; invalid sets
  (raise-error (set-complement 34 45) simple-error)

  (raise-error (set-complement (list->set '(1 2)) 45) simple-error)
  
  ;; incompatible equality tests
  (raise-error
      (set-complement 
       (list->set '(1 2) :test #'=)
       (list->set '(1 2 3) :test #'eq))
      simple-error)

  ;; predicate \ explicit
  (let ((A (list->set '(1 2 3) :test #'=)))
    (let ((result (set-complement *Z* A)))
      
      (check (predicate-set-p result))
      ;; 2.5 is in Z? no
      (check-not (belongs 2.5 result))
      
      ;; 4 is in Z but not in A
      (check (belongs 4 result))
      ;; 2 is in A
      (check-not (belongs 2 result))))

   ;; predicate \ predicate
  (let ((positive (make-predicate-set (lambda (x) (> x 0)) :test #'=))
	(even 	  (make-predicate-set (lambda (x) (and (integerp x)
						       (= (mod x 2) 0)))
	   :test #'=)))


    (let ((result
	    (set-complement even positive))) ;;only positives even
      ;; positive AND NOT even
      (check-not (belongs 1 result))
      (check-not (belongs 3 result))
      (check-not (belongs 2 result))
      (check-not (belongs -1 result))))


  ;; multiple sets:
  ;; (A ∪ B) \ C
  (let ((A (list->set '(1 2) :test #'=))
	(B (list->set '(3 4) :test #'=))
	(OMEGA (list->set '(1 2 3 4 5 6 7 8) :test #'=)))


    (check
     (equal-sets
      (list->set '(5 6 7 8) :test #'=)
      (set-complement OMEGA A B )))))


;;; ============================================================================
;;; HELPER FUNCTIONS
;;; ============================================================================

(test append-explicit-sets
(let ((A (list->set '(1 2 3) :test #'=))
(B (list->set '(3 4 5) :test #'=)))


(check
 (equal-sets
  (list->set '(1 2 3 4 5) :test #'=)
  (math::append-explicit-sets A B)))))


;;; ============================================================================
;;; END
;;; ============================================================================
