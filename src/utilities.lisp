;;; file MATH:src/utilities.lisp --- all utilities of math package
;;;
;;; Code:

(in-package :math)


;;; SETS

(defun set->list (s)
  "Returns the MEMBERS of the set, ONLY if set is explicit. Else raise a SIMPLE-ERROR"
  (unless (explicit-set-p s)
    (error "set->list applies only at explicit sets!"))
  (math-set-members s))


(defun list->set (lst &key (test #'eql))
  "Creates an explicit set with members the items of LST and test function
the provided TEST (default to #'EQL)"
  (make-explicit-set lst :test test))

(defun sub-set-p (s1 s2)
  "Checks if S1 set is a SUBSET of S2 (s1 ⊆ s2)
if s1 is a predicate set or both are predicate sets function
can't determine the subset relation, and raise an error.
Sets must have the same test function"
  (cond
    ;; empty set is subset of all sets 
    ((empty-set-p s1) t)

    ;; only empty set is subset of empty set
    ((empty-set-p s2) nil)

    ;; explicit ⊆ explicit
    ((and (explicit-set-p s1)
      (explicit-set-p s2))
     (unless (eq (math-set-test s1)
		 (math-set-test s2))
       (error "Cannot check explicit sets with different equality tests."))

     (let ((l1 (set->list s1))
	   (l2 (set->list s2))
	   (test (math-set-test s1)))
       (every (lambda (x)
		(member x l2 :test test))
	      l1)))
  
    ;; explicit ⊆ predicate
    ((and (explicit-set-p s1)
          (predicate-set-p s2))
     (let ((l1 (set->list s1))
           (predicate (math-set-members s2)))
       (reduce
        (lambda (acc x)
          (and acc
               (funcall predicate x)))
        l1
        :initial-value t)))

    ;; predicate ⊆ predicate
    ((and (predicate-set-p s1)
          (predicate-set-p s2))
     (error
      "Can't determine subset relation between two predicate sets."))

    ;; predicate ⊆ explicit
    (t
     (error
      "Cannot determine whether a predicate set is a subset of an explicit set."))))

(defun equal-sets (s1 s2)
  "Test if S1 = S2, based in the property
A⊆B ∧ B⊆A ⇒ A = B"
  (and (sub-set-p s1 s2)
       (sub-set-p s2 s1)))

(defun belongs (x set)
  "Returns T if item X is member of SET."
  (cond
    ((not (math-set-p set)) nil)
    (t (if (explicit-set-p set)
      (when (member x (set->list set) :test (math-set-test set)) t)
      (when (funcall (math-set-members set) x) t ) ))))

(defun sub-universe-p (s1 universe)
  "Cehcks if S1 is a subset of UNIVERSAL-SET UNIVERSE.
Returns T if it is, nil if not.
Raise an error in invalid input"
  (unless (math-set-p s1)
    (error "set ~A is not valid!" s1))
  (unless (universe-p universe)
    (error "Universe ~A is not valid!" universe))

  (sub-set-p s1 (universal-set-omega universe))
  )



;;;STRINGS

(defun count-chars (phrase chars)
  (check-type phrase string)
  (check-type chars list)
  (when (or (null chars) (string= phrase ""))
    (return-from count-chars 0))
    (count-if (lambda (X) (member x chars :test #'char=)) (coerce phrase 'list)))





;;; MATH:src/utilities.lisp ends here
