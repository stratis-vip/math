(in-package :math)

(defstruct question
  text
  solution)

(defun make-question* (text &rest solutions)
  (make-question
   :text text
   :solution solutions )
  )

;;;=================================================================
;;;                              EXERCISES
;;;=================================================================

(defstruct exercise
  (number nil :type (or null (integer 1) string))
  (text nil :type string)
    (subs nil :type list)
  )

(defun make-exercise* (number text &rest subs)
  "Creates a EXERCISE struct"
  (unless (or (stringp number)  (integerp number) (plusp number))
    (error "number must be an integer bigger than 1 or a string"))
  (make-exercise
   :text text
   :number number
   :subs  subs  ; list of question or exercise 
   ))

(defun make-sub-exercise* (number text &rest subs)
  "An Alias to make-exercise* just for clarity, since any sub-exercise
is an exercise itself."
  (apply #'make-exercise* number text subs))

;;;=================================================================
;;;                              SOLUTIONS
;;;=================================================================

(defstruct solution
  text
  steps ;list of solution-steps
  )

(defun make-solution* (text &rest steps)
  (make-solution
   :text text
   :steps steps)
  )

(defstruct solution-step
  text
  code
  symbol
  result
  presentation)

(defun make-solution-step* (text code symbol &optional (presentation nil) )
  (make-solution-step
   :text text
   :code code
   :symbol symbol
   :presentation presentation)
  )


(defun write-solution-step (step env depth)
  (let* ((rep-code (solution-step-presentation step))
	 (has-presentation-code (not (null rep-code))))
    
    (push (solve-step step env) (car  env))
    
    (when has-presentation-code
      (setf (solution-step-presentation step)
	    (eval (rewrite-s-form rep-code env))))
    
    (format t "~VT *~A~%" depth (solution-step-text step))
    (format t "~VT code Lisp: ~A~%~%" depth (solution-step-code step))
    (format t "~VT Αποτέλεσμα: ~A~%~%" depth (solution-step-presentation step))))
;  (format t "DEBUG: env is ~A~%~%" env)
  

(defun write-solution (slt env depth)
  (format t "~VT" depth)
  (format t "~VTΛύση: ~A~%" (+ 2 depth) (solution-text slt) )
  (dolist (x (solution-steps slt))
    (write-solution-step x env (+ 4 depth)))
  )

(defun write-question (qst env depth)
  (format t "~VT~A~%"
          (* 2 depth)
          (question-text qst))
  (dolist (solution (question-solution qst))
    (write-solution solution env (1+ depth))))

(defun Write-exercise (xrs env &optional (depth 0))
  (format t "~VT" depth)

  (format t "~A ~A~%" (exercise-number xrs) (exercise-text xrs))
  (dolist (x (exercise-subs xrs))
    (if (exercise-p x)
	(write-exercise x env (1+ depth))
	(write-question x env (1+ depth))))
  )

(defun solve-step (sstep env)
  (let* ((code (solution-step-code sstep))
         (rewritten-code (rewrite-s-form code env))
         (result (eval rewritten-code)))

    (setf (solution-step-result sstep)
          result)
   
      (cons (solution-step-symbol sstep)
	    result)
   ))

(defun rewrite-s-form (s-form env)
  (cond
    ;; symbol
    ((symbolp s-form)
     (let ((binding (assoc s-form (car  env))))
       (if binding
           (cdr binding)
           s-form)))

    ;; atom
    ((atom s-form)
     s-form)

    ;; quoted form
    ((eq (car s-form) 'quote)
     s-form)

    ;; ordinary list
    (t
     (cons (car s-form)
	   (mapcar (lambda (form)
		     (rewrite-s-form form env))
		   (cdr s-form))))
    ))

(defun solve-exercise (xrs)
  (let ((env (list nil)))
    (write-exercise xrs env))
  )
