;;; file MATH:src/utilities.lisp --- all utilities of math package
;;;
;;; Code:

(in-package :math)


;;; SETS

(defun set->list (s)
  "Returns the MEMBERS of the set, ONLY if set is explicit. Else raise a SIMPLE-ERROR"
  (unless (explicit-set-p s)
    (error "set->list applies only at finite sets!"))
  (math-set-members s))


(defun list->set (lst &key (test #'eql))
  "Creates an explicit set with members the items of LST and test function
the provided TEST (default to #'EQL)"
  (make-explicit-set lst :test test))

(defun sub-set-p (s1 s2)
  "Checks if S1 set is a SUBSET of S2 (s1 ⊆ s2)
if s1 is a predicate set or both are predicate sets function
can't determine the subset relation, and raise an error."
  (cond
    ;; explicit ⊆ explicit
    ((and (explicit-set-p s1)
          (explicit-set-p s2))
     (let ((l1 (set->list s1))
           (l2 (set->list s2))
	   (f-test (math-set-test s2)))
       (reduce
        (lambda (acc x)
          (and acc
               (not (null (member x l2 :test f-test)))))
        l1
        :initial-value t)))

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
      "Can't determine subset relation between a predicate set and a explicit set."))))

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
      (funcall (math-set-members set) x)))))










;;; MATH:src/utilities.lisp ends here
