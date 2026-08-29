# School Math Library

A small math library, written for practicing.

# Overview

It based in hellenic scholl books, of Λύκειο.

# Usage

## Prerequisites

This library needs (asdf will auto install them if they are in your local system), these packages installed:
- [lists utilities](https://github.com/stratis-vip/lists/)
- [strings utilities](https://github.com/stratis-vip/strings/)
- [review Testing Library](https://github.com/stratis-vip/review/)

## Installation

You need to clone this repo to a position that common lisp recognize (usually to ~/common-lisp/).
After that

```lisp
(asdf:load-system :math)       ;; math Library
(asdf:load-system :math/tests) ;; Tests of math Library
(asdf:test-system :math/tests) ;; Run all tests
```

# Documentation

## Sets Theory

You can create the math type of a set, by using the set parser. For example the phrase->math-notation function will take a phrase in Eglish or in Greek and create a math set-definition. 
```lisp
(phrase->math-notation "Το σύνολο των πραγματικών αριθμών Χ που είναι μεγαλύτεροι ή ίσοι από το 0") ;=> {x ∈ ℝ | x ≥ 0}

(phrase->math-notation "The set of integer numbers y are greater or equal from 0") ;=> {y ∈ ℤ | y ≥ 0}
```
Every phrase must follow the grammar rules to be parsed correctly.
```lisp 
;;; The grammar describes the English/Greek phrases accepted by the parser.
;;;
;;; Notation:
;;;   :=  means "is defined as"
;;;   |   means "or"
;;;   { } means a placeholder for another grammar element
;;;   [ ] means an optional part
;;;
;;; SET-EXPRESSION
;;;   := THE SET OF {DOMAIN} NUMBERS {VARIABLE} [ARE {CONDITION}]
;;;
;;; DOMAIN
;;;   := REAL
;;;    | INTEGER
;;;    | NATURAL
;;;    | RATIONAL
;;;
;;; CONDITION
;;;   := PROPERTY
;;;    | COMPARISON
;;;    | CONDITION AND CONDITION
;;;    | CONDITION OR CONDITION
;;;
;;; PROPERTY
;;;   := EVEN
;;;    | ODD
;;;
;;; COMPARISON
;;;   := GREATER FROM {NUMBER}
;;;    | GREATER OR EQUAL FROM {NUMBER}
;;;    | EQUAL TO {NUMBER}
;;;    | LESS FROM {NUMBER}
;;;    | LESS OR EQUAL FROM {NUMBER}
;;;
;;; The condition is optional. Therefore both of the following are valid:
;;;
;;;   "the set of integer numbers x"
;;;   "the set of integer numbers x are greater from 10"
;;;
;;; When the condition is omitted, the SET-EXPRESSION has CONDITION = NIL.
```

There are 2 distinguish type of sets in this library:
- EXPLICIT sets:  based on a list of their members. 
```lisp
;;; set A = {1, 2, 3, 4} 
(defparameter A (make-explicit-set '(1 2 3 4) :test #'=))
```

- PREDICATE sets: based on a predicate function that every member must satisfy. 
```lisp
;;; set R⁺={x ∈ ℝ | x ≥ 0}
(defparameter R+ (make-predicate-set (lamba (x) (and (realp x) (>= x 0 )) :test #'=)))
```

### Functions 
There are 3 functions 
* union
```lisp 
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
	```
* intersection
```lisp
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
	```
	
* complement 

```lisp
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
		 ```
