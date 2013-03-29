;; Structure & Interpretation of Computer Programs

;; CHAPTER 1 - ELEMENTS OF PROGRAMMING
;; ===================================

;; Expressions
;; -----------
;;
;; expressions are normally prefix for the interpreter. Prefix notation has
;; several advantages, including that it can take arbitrary number of arugments.
;; For example, the function:

(define (sum-many a b c d e f g) (+ a b c d e f g))

;; can sum several items. Another advantage is that it allows combinations to be
;; nested, as in:

(define (nested x y)
  (+ (* x y) (- x y)))

;; Naming and the Environment
;; --------------------------
;;
;; We can name things using the `define` keyword. In the below example, we can
;; now refer to `2` with the name `size`

;; Evaluating Combinations
;; -----------------------

;; The general procedure the compiler uses is:
;;   1. Evaluate the subexpressions of the combination
;;   2. apply the procedure that is the value of the leftmost subexpression
;;      (the operator) to the arguments are are the values of other s-expressions
;;      (the operands)
;; 
;; The implication of this rule is that evaluation is recursive. Also, _special
;; forms_, such as `define`, do not follow this rule of evaluation.

(define size 2)

;; The concept of a procedure definition allows for a procedure to be called
;; by giving it a name. For example:

(define (square x) (* x x))

;; The general format is (define (<name> <formal parameters>) <body>)

;; Besides using the function `square` by calling it, we can also use it as
;; a building block for other procedures, for example:

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;; THE SUBSTITUTION MODEL
;; ======================


;; Exercise 1.3 Define a procedure that takes three numbers as arguments
;; and returns the sum of the squares of the two larger numbers

(define (ex1.3 x y z)
  (cond ))
