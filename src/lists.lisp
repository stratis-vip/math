;;; file MATH:src/lists.lisp --- all predicates - utilities on lists 
;;;
;;; Code:

(in-package :math)

;;; PREDICATES

(defun has-no-duplicates-p (lst &key (test #'eql))
  "Checks if the list LST has duplicates members.
Returns T if list has no duplicates and (VALUES nil, duplicate-position),
where duplicate-position is the first occurence of the duplicate item"
  (if (not (listp lst))
      nil
       (labels ((helper (ls container n)
		  (cond
		    ((null ls) t)
		    ((member (first ls) container :test test) (values nil n))
		    (t (helper (rest ls) (cons (first ls) container) (1+ n))))))
	 (helper (rest lst) (list (first lst)) 1))))

;;; MATH:src/lists.lisp ends here
