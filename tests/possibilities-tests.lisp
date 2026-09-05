;;; file MATH:tests/possibilities-tests.lisp -- tests about possibilities
;;;
;;; Code:

(in-package :math/tests)


(defsuite possibilities-tests)
(in-suite possibilities-tests)


(test simple-event-p
  ;;invalid input retrurn nil
  (check-not (simple-event-p 34))
  (check-not (simple-event-p '(3)))

  ;;empty-set is not a simple event
  (check-not (simple-event-p *empty-set*))

  ;;predicate sets are not events
  (check-not (simple-event-p *Z*))
  
  ;;happy end
  (check (simple-event-p (list->set '(3))))
  (check (simple-event-p (list->set '(Κ)))))


(test complex-event-p
  ;;invalid input retrurn nil
  (check-not (complex-event-p 34))
  (check-not (complex-event-p '(3)))

  ;;empty-set is not a complex event
  (check-not (complex-event-p *empty-set*))

  ;;predicate sets are not events
  (check-not (complex-event-p *Z*))
  
  ;;happy end
  (check (complex-event-p (list->set '(3 4 5))))
  (check (complex-event-p (list->set '(Κ Γ)))))

(test mutual-exclusive-p
  ;;invalid input retrurn nil
  (check-not (mutual-exclusive-p *N* 43))
  (check-not (mutual-exclusive-p 34 *N*))

  ;;1 common item -> NIL
  (check-not (mutual-exclusive-p (list->set '(1 2 3) :test #'=) (list->set '(3 4 5) :test #'=)))
  (check-not (mutual-exclusive-p (list->set '(1 -2 -3) :test #'=) (make-predicate-set #'plusp :test #'=)))
  
  ;;empty set is always mutual exclusive
  (check (mutual-exclusive-p *empty-set* *empty-set*))
  (check (mutual-exclusive-p *empty-set-test=* *N*))
  (check (mutual-exclusive-p *Z* *empty-set-test=* ))
   
  ;;happy end
  (check (mutual-exclusive-p (list->set '(1 3) :test #'=) (list->set '( 5 6 ) :test #'=)))
  (check (mutual-exclusive-p (list->set '(-1 -2) :test #'=) (make-predicate-set #'plusp :test #'=)))
  )

;;; file MATH:tests/possibilities-tests.lisp ends here
