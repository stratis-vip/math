;;; file MATH:src/package.lisp  --- package definition of MATH
;;;
;;; Code:

(defpackage :math
	    (:use :cl :lists :strings)
	    (:export

	     ;; sturctures
	     :make-explicit-set
	     :make-predicate-set
	     :equal-sets
	     :belongs
	     :make-universe

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
	     :sub-universe-p
	     :universe-p

	     
	     ;; utilities (file utilities.lisp)
	     :set->list
	     :universe->list
	     :list->set
	     :list->universe

	     ;;set-functions
	     :set-union
	     :set-intersection
	     :set-complement
	     :set-complement-h 

	     :phrase->ast
	     :phrase->math-notation
	     :phrase->predicate

	     ))

;;; file MATH:src/package.lisp ends here Велес
