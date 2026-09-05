;;; file MATH:src/possibilities.lisp -- main file about possibilities
;;;
;;; Code:

(in-package :math)

(defun simple-event-p (s)
  (and (math-set-p s)
       (explicit-set-p s)
       (has-more-than-n-p (math-set-members s) 1 :equal t)))

(defun complex-event-p (s)
  (and (math-set-p s)
       (explicit-set-p s)
       (has-more-than-n-p (math-set-members s) 1)))

(defun mutual-exclusive-p (a b)
  (and (math-set-p a)
       (math-set-p b)
       (empty-set-p (set-intersection a b))
       ;; (if (or (empty-set-p a) (empty-set-p b))
       ;; 	   nil
       ;; 	   (empty-set-p (set-intersection a b)))
       ))



(defun one-of (list len)
    (elt list (random len)))

(defun calculate-statistics (omega retries)
  (declare (optimize (speed 3) (safety 1))
           (type fixnum retries))
  (let* ((omega (coerce omega 'simple-vector))
         (n (length omega))
         (counts (make-array n :element-type 'fixnum :initial-element 0)))
    (declare (type simple-vector omega)
             (type fixnum n)
             (type (simple-array fixnum (*)) counts))

    (dotimes (i retries)
      (incf (the fixnum (aref counts (random n)))))

    (let* ((retries-f (coerce retries 'double-float))
           (sum 0.0d0)
           (sum-sq 0.0d0)
           (results '()))
      (declare (type double-float retries-f sum sum-sq))

      (dotimes (i n)
        (let* ((cc (aref counts i))
               (freq (/ (coerce cc 'double-float) retries-f)))
          (declare (type fixnum cc)
                   (type double-float freq))
          (incf sum freq)
          (incf sum-sq (* freq freq))
          (push (list (aref omega i) cc freq) results)))

      (let* ((mean (/ sum n))
             (var (- (/ sum-sq n) (* mean mean)))
             (std (sqrt (max 0.0d0 var))))
        (values (nreverse results) mean std)))))

(defun statistics (omega retries)
  (multiple-value-bind (res mean std)
      (calculate-statistics omega retries)
    (format t "Mean: ~,6f~%Std:  ~,6f~%" mean std)
    (mapcar (lambda (x) (list (first x) (second x) (format nil "~,2f%" (* 100 (third x)))))  res)))

(defun benchmark (fn &rest args)
  (let ((start (get-internal-real-time)))
    (apply fn args)
    (float (/ (- (get-internal-real-time) start)
              internal-time-units-per-second)
           1.0d0)))

  
;;; File MATH:src/possibilities.lisp ends here
