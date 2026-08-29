;;; File MATH:src/sets.lisp -- Sets definitions and creation 
;;;
;;; Code:

(in-package :math)

(defun list->math-notation (lst &optional (type 'number))
  "Converts a list of members to mathematical set notation.
TYPE determines how each member is printed."
  (let ((formatter
          (case type
            ((number)
             (lambda (x)
               (format nil "~a" x)))

            ((symbol)
             (lambda (x)
               (symbol-name x)))

            ((string)
             (lambda (x)
               (format nil "\"~a\"" x)))

	    ((char)
	     (lambda (x)
               (format nil "'~a'" x)))
	    
            ((list)
             (lambda (x)
               (format nil "~a" x)))

            (otherwise
             (error "Unknown member type: ~S" type)))))

    (format nil
            "{~{~a~^, ~}}"
            (mapcar formatter lst))))

(defstruct (math-set (:predicate nil))
  "A MATH-SET is a collection of items - members - well defined and different everyone of other.

SLOTS of MATH-SET
  KIND          if a set has spesific members like {1, 2, 3, 4} then the set is of KIND :EXPLICIT
                if a set define it's members based on a predicate like #'realp then the set is of KIND :PREDICATE
  MEMBERS       a LIST of items if :EXPLICIT or the predicate function if :PREDICATE
  TEST          The FUNCTION used for equality/membership
                 comparisons of an :EXPLICIT set.
                It is ignored for :PREDICATE sets.
  DOCUMENTATION a STRING or nil if a math-notation is provided //TO BE DEVELOPED

MATH-SET-P predicate defined in this file
"
  (kind :explicit :type (member :explicit :predicate))
  (members nil :type (or list function))
  (test #'eql :type function)
  (documentation "" :type string))

(defun math-set-p (set )
  (and (typep set 'math-set)
       (let ((members (math-set-members set))
	     (test-func (math-set-test set)))
	 (case (math-set-kind set)
	   (:explicit (and (listp members)
			   (has-no-duplicates-p members :test test-func)))
	   (:predicate (functionp members))
	   (otherwise nil)))))

(defun make-explicit-set (members &key (test #'eql) (type 'number))
  "Creates an explicit MATH-SET with members MEMBERS"
  (check-type test function)
  (check-type type (member number list string symbol char))
  (multiple-value-bind (value position)
      (has-no-duplicates-p members :test test) 
    (unless value
      (error "MEMBERS must be a list without duplicates! Duplicate element \"~S\" at position ~d"
	     (elt members position) position))
    
    (make-math-set :kind :explicit
		   :members members
		   :test test
		   :documentation (list->math-notation members type))))

(defun explicit-set-p (set)
  (if (not (math-set-p set))
      nil
      (eq :explicit  (math-set-kind set))))

;;some explicit sets
(defparameter *empty-set* (make-explicit-set '()))

(defun empty-set-predicate (x)
  (declare (ignore x))
  nil)

(defun empty-set-p (set)
  (case (math-set-kind set)
    (:explicit (null (math-set-members set)))
    (:predicate nil)
    (otherwise nil)))

(defun make-predicate-set (predicate &key (test #'eql) (doc ""))
  "Creates a MATH-SET with members satisifies PREDICATE function."
  (check-type predicate function)
  (make-math-set :kind :predicate
		 :members predicate
		 :test test
		 :documentation doc))


(defun predicate-set-p (set)
  (eq :predicate (math-set-kind set)))



;;some predicate sets
(defparameter *N* (make-predicate-set
		   (lambda (x) (and (integerp x) (>= x 0)))
		   :test #'=
		   :doc "{x ∈ ℤ | x ≥ 0}"))
(defparameter *Z* (make-predicate-set #'integerp
				      :test #'=
				      :doc "{x ∈ ℤ}"))
(defparameter *R* (make-predicate-set #'realp
				      :test #'=
				      :doc "{x ∈ ℝ}"))


;;; UNIVERSAL-SET
(defstruct universal-set
  omega 
  )

(defun make-universe (set)
  "Creates a UNIVERSAL-SET and sets OMEGA to SET.
Returns s UNIVERSAL-SET object or raise an error in invalid input
"
  (if (math-set-p set)
      (make-universal-set :omega set)
      (error "Not a valid set ~A" set)))

(defun universe-p (universe)
  "Checks if UNIVERSE is a valid UNIVERSAL-SET object.
Returns t or NIL."
  (ignore-errors
   (math-set-p (universal-set-omega universe))))

(defun universe->list (universe)
  "Converts UNIVERSAL-SET object UNIVERSE to a list,
if its OMEGA set is explicit!"
  (unless (universe-p universe)
    (error "UNIVERSE ~A is not valid!" universe))
  (set->list (universal-set-omega universe)))

(defun list->universe (lst &key (test #'eql))
  "Create a UNIVERSAL-SET object with an explicit SET
with LST members as OMEGA "
  (make-universe (list->set lst :test test)))


;;; SET FUNCTIONS

(defun set-union (s1 &rest args)
  "UNION of 1 or more sets.
Returns a new explicit SET if all sets are explicit,
otherwise returns a predicate SET.

Signals an error if any argument is not a valid set
or if the sets have different equality tests."
  (let ((all-sets (cons s1 args)))
    ;; 1. Check all sets validity
    (dolist (set all-sets)
      (unless (math-set-p set)
        (error "~S is not a valid set." set)))

    ;; 2. Check all equality tests consistency
    (let ((test (math-set-test s1)))
      (dolist (set args)
        (unless (eq test (math-set-test set))
          (error "Cannot union sets with different equality tests."))))

    ;; 3. Reduce over all arguments cleanly
    (reduce #'set-union-h args :initial-value s1)))

(defun set-union-h (s1 s2)
  "HELPER FUNCTION
UNION of 2 sets S1 and S2 is another SET which include both sets members. all checks
are doen in set-union function whicj call this helper."
  (cond
    ;;explicit - explicit
    ((and (explicit-set-p s1) (explicit-set-p s2))
     (append-explicit-sets s1 s2))

    ;;explicit - predicate
    ((and (explicit-set-p s1) (predicate-set-p s2))
     (if (sub-set-p s1 s2)
	 s2
	 (make-predicate-set
	  (lambda (x)
	    (or (member x (set->list s1) :test (math-set-test s1))
		(funcall (math-set-members s2) x))))))

    ;;predicate - predicate
    ((and (predicate-set-p s1) (predicate-set-p s2))
     (make-predicate-set (lambda (x) (or (funcall (math-set-members s1) x)
					 (funcall (math-set-members s2) x)))
			 :test (math-set-test s1)))

    ;;predicate - explicit
    ((and (predicate-set-p s1) (explicit-set-p s2))
     (if (sub-set-p s2 s1)
	 s1
	 (make-predicate-set (lambda (x) (or (member x (set->list s2) :test (math-set-test s2))
					     (funcall (math-set-members s1) x))))))))

(defun set-intersection (s1 &rest args)
  "INTERSECTION of 1 or more sets.
Returns a new explicit SET if all sets are explicit,
otherwise returns a predicate SET.

Signals an error if any argument is not a valid set
or if the sets have different equality tests."
  (let ((all-sets (cons s1 args)))
    ;; 1. Check all sets validity
    (dolist (set all-sets)
      (unless (math-set-p set)
        (error "~S is not a valid set." set)))

    ;; 2. Check all equality tests consistency
    (let ((test (math-set-test s1)))
      (dolist (set args)
        (unless (eq test (math-set-test set))
          (error "Cannot intersect sets with different equality tests."))))

    ;; 3. Reduce over all arguments cleanly
    (reduce #'set-intersection-h args :initial-value s1)))

(defun set-intersection-h (s1 s2)
  "HELPER FUNCTION
INTERSECTION of two sets "
  (unless (and  (math-set-p s1) (math-set-p s2))
    (error "s1 and s2 must be valid sets!"))
  (unless (eq (math-set-test s1)
	      (math-set-test s2))
    (error "Cannot intersect sets with different equality tests."))
  (cond
    ;; both explicit
    ((and (explicit-set-p s1) (explicit-set-p s2))
     (let ((l1 (math-set-members s1))
	   (l2 (math-set-members s2))
	   (test (math-set-test s1)))
       (list->set (remove-if-not (lambda (x) (member x l2 :test test)) l1) :test test)))

    ;;explicit n predicate
    ((and (explicit-set-p s1) (predicate-set-p s2))
     (let ((l1 (math-set-members s1))
	   (predicate (math-set-members s2))
	   (test (math-set-test s1)))
       (if (sub-set-p s1 s2)
	   s1
	   (list->set (remove-if-not (lambda (x) (funcall predicate x)) l1) :test test))))

    ;;predicate n explicit
    ((and (predicate-set-p s1) (explicit-set-p s2))
     (let ((l2 (math-set-members s2))
	   (predicate (math-set-members s1))
	   (test (math-set-test s2)))
       (if (sub-set-p s2 s1)
	   s2
	   (list->set (remove-if-not (lambda (x) (funcall predicate x)) l2) :test test))))

    ;;predicate n predicate
    ((and (predicate-set-p s1) (predicate-set-p s2))
     (let ((p1 (math-set-members s1))
	   (p2 (math-set-members s2))
	   (test (math-set-test s1)))
       (make-predicate-set (lambda (x) (and (funcall p1 x) (funcall p2 x))) :test test)))))

(defun set-complement (omega s &rest args)
  "Relative COMPLEMENT of one or more sets with respect to OMEGA.
Computes OMEGA \\ (S ∪ arg1 ∪ arg2 ∪ ...).
Returns a new explicit SET if all sets are explicit,
otherwise returns a predicate SET.
Signals an error if any argument is not a valid set
or if the sets have different equality tests."
  (let ((all-sets (cons s args)))          ; the sets to be subtracted from omega
    ;; Check all sets validity (including omega)
    (unless (math-set-p omega)
      (error "~S is not a valid set." omega))
    (dolist (set all-sets)
      (unless (math-set-p set)
        (error "~S is not a valid set." set)))
    ;; Check equality tests
    (let ((test (math-set-test omega)))
      (dolist (set all-sets)
        (unless (eq test (math-set-test set))
          (error "Cannot complement sets with different equality tests."))))
    ;; OMEGA \ (S ∪ arg1 ∪ arg2 ∪ ...)
    (set-complement-h
     omega
     (if args
         (apply #'set-union s args)
         s))))

;; (defun set-complement (s1 s2 &rest args)
;;   "COMPLEMENT of 2 or more sets.
;; Returns a new explicit SET if all sets are explicit,
;; otherwise returns a predicate SET.

;; Signals an error if any argument is not a valid set
;; or if the sets have different equality tests."
;;   (let ((all-sets (cons s1 (cons s2 args))))

;;     ;; Check all sets validity
;;     (dolist (set all-sets)
;;       (unless (math-set-p set)
;;         (error "~S is not a valid set." set)))

;;     ;; Check equality tests
;;     (let ((test (math-set-test s1)))
;;       (dolist (set (rest all-sets))
;;         (unless (eq test (math-set-test set))
;;           (error "Cannot complement sets with different equality tests."))))

;;     ;; (S1 ∪ S2 ∪ ... ∪ Sn-1) \ Sn
;;     (set-complement-h
;;      (apply #'set-union
;;             (subseq all-sets 0 (1- (length all-sets))))
;;      (car (last all-sets)))))

(defun set-complement-h (omega s)
  "Relative complement of set S with respect to OMEGA (i.e. OMEGA \\ S)."
  (unless (and (math-set-p omega) (math-set-p s))
    (error "omega and s must be valid sets!"))
  (unless (eq (math-set-test omega)
              (math-set-test s))
    (error "Cannot find the complement of sets with different equality tests."))
  (cond
    ;; both explicit
    ((and (explicit-set-p omega) (explicit-set-p s))
     (let ((l-omega (math-set-members omega))
           (l-s     (math-set-members s))
           (test    (math-set-test omega)))
       (list->set (remove-if (lambda (x) (member x l-s :test test))
                             l-omega)
                  :test test)))

    ;; omega explicit, s predicate
    ((and (explicit-set-p omega) (predicate-set-p s))
     (let ((l-omega   (math-set-members omega))
           (predicate (math-set-members s))
           (test      (math-set-test omega)))
       (list->set (remove-if (lambda (x) (funcall predicate x))
                             l-omega)
                  :test test)))

    ;; omega predicate, s explicit
    ((and (predicate-set-p omega) (explicit-set-p s))
     (let ((predicate (math-set-members omega))
           (l-s       (math-set-members s))
           (test      (math-set-test omega)))
       (make-predicate-set
        (lambda (x)
          (and (funcall predicate x)
               (not (member x l-s :test test))))
        :test test)))

    ;; both predicate
    ((and (predicate-set-p omega) (predicate-set-p s))
     (let ((p-omega (math-set-members omega))
           (p-s     (math-set-members s))
           (test    (math-set-test omega)))
       (make-predicate-set
        (lambda (x)
          (and (funcall p-omega x)
               (not (funcall p-s x))))
        :test test)))))

;; (defun set-complement-h (s1 s2)
;;   "Complement of set S1 Over S2"
;;   (unless (and  (math-set-p s1) (math-set-p s2))
;;     (error "s1 and s2 must be valid sets!"))
;;   (unless (eq (math-set-test s1)
;; 	      (math-set-test s2))
;;     (error "Cannot find the complemet of  sets with different equality tests."))
;;   (cond
;;     ;; both explicit
;;     ((and (explicit-set-p s1) (explicit-set-p s2))
;;      (let ((l1 (math-set-members s1))
;; 	   (l2 (math-set-members s2))
;; 	   (test (math-set-test s1)))
;;        (list->set (remove-if (lambda (x) (member x l2 :test test)) l1) :test test)))

;;     ;;explicit n predicate
;;     ((and (explicit-set-p s1) (predicate-set-p s2))
;;      (let ((l1 (math-set-members s1))
;; 	   (predicate (math-set-members s2))
;; 	   (test (math-set-test s1)))
;;        (make-predicate-set (lambda (x) (and (funcall predicate x)
;; 					    (not (member x l1 :test test)))))))

;;     ;;predicate n explicit
;;     ((and (predicate-set-p s1) (explicit-set-p s2))
;;      (let ((l2 (math-set-members s2))
;; 	   (predicate (math-set-members s1))
;; 	   (test (math-set-test s2)))
;;        (make-predicate-set (lambda (x) (and (not (funcall predicate x))
;; 					    (member x l2 :test test))))))
    
;;     ;;predicate n predicate
;;     ((and (predicate-set-p s1) (predicate-set-p s2))
;;      (let ((p1 (math-set-members s1))
;; 	   (p2 (math-set-members s2))
;; 	   (test (math-set-test s1)))
;;        (make-predicate-set (lambda (x) (and (not (funcall p1 x)) (funcall p2 x))) :test test)))))

;; (defun append-explicit-sets (s1 s2)
;;   (let ((l1 (math-set-members s1))
;; 	(l2 (math-set-members s2))
;; 	(test (math-set-test s1)))
;;     (list->set (remove-duplicates (append l1 l2) :test test) :test test )))

;;;MATH:src/sets.lisp ends here
