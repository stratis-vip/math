;;; file MATH:tests/possibilities-tests.lisp -- tests about possibilities
;;;
;;; Code:

(in-package :math/tests)


(defsuite possibilities-tests)
(in-suite possibilities-tests)


(defun get-value (x)
  (funcall (lambda (x) x ) x))


;;; file MATH:tests/possibilities-tests.lisp ends here
