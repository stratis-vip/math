;;; file MATH:src/package.lisp  --- package definition of MATH
;;;
;;; Code:

(defpackage :math
	    (:use :cl)
	    (:export

	     ;; sturctures
	     :make-explicit-set
	     :make-predicate-set
	     :equal-sets
	     :belongs

	     ;; defined sets
	     :*empty-set*
	     :*N*
	     :*Z*
	     :*R*

	     ;; predicates (file predicates.lisp)
	     :has-no-duplicates-p ;;file lists.lisp
	     :math-set-p
	     :empty-set-p
	     :explicit-set-p
	     :predicate-set-p
	     :sub-set-p

	     
	     ;; utilities (file utilities.lisp)
	     :set->list
	     :list->set
	     

	     ))

;;; file MATH:src/package.lisp ends here
