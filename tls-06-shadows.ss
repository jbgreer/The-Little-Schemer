;; The Little Schemer, 4th Edition
;; Chapter 6 Shadows

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; add1 add 1 to a numeric argument
(define add1
  (lambda (n)
    (+ 1 n)))

;; sub1 substracts 1 from a numeric argument
(define sub1
  (lambda (n)
    (- n 1)))

;; o+ adds two numbers together
(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

;; o- subtracts the second number from the first number
(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

;; tuples
(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup)
	       (addtup (cdr tup)))))))

;; multiplication n x m
(define o*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (o* n (sub1 m)))))))

;; tup+ add up numbers in 2 tuples pairwise.  assumes both tuples are of equal length
(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2)) '())
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons
	    (o+ (car tup1) (car tup2))
	    (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (o< (sub1 n) (sub1 m))))))

;; numeric equality using zero?
(define o=
  (lambda (n m)
    (cond
     ((zero? m) (zero? n))
     ((zero? n) #f)
     (else (o= (sub1 m) (sub1 n))))))

;; numeric equality using o< and o>
(define o=
  (lambda (n m)
    (cond
     ((o< n m) #f)
     ((o> n m) #f)
     (else #t))))

;; exponentiation
(define o^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (o* n (o^ n (sub1 m)))))))

;; division with no remainder
(define o%
  (lambda (n m)
    (cond
     ((o< n m) 0)
     (else (add1 (o% (o- n m) m))))))

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((null? lat) '())
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat)
		 (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat)
		 (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (cons (car lat)
				(all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (o= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

;; simpler
(define one?
  (lambda (n)
    (o= n 1)))

;; rempick using one?
(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat)
		 (rempick (sub1 n) (cdr lat)))))))

;; Ch 6
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) '+)
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) '*)
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) '^)
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp)))))))))
  
;; simplified
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(numbered? '(1 + (3 ^ 2)))

;; The Seventh Commandment
;; Recur on the subparts that are of the same nature:
;; - On the sublists of a list
;; - On the subexpressions of an arithmetic expression

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) '+) (o+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) '*) (o* (value (car nexp)) (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) '^) (o^ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

(value '((2 ^ 5) + 13))


;; The Eighth Commandment
;; Use helper functions to abstract from representation

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define expt
  (lambda (b n)
    (cond
     ((zero? n) 1)
     (* b (expt b (- n 1))))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) '+)
      (o+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) '*)
      (o* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) '^)
      (o^ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

(value '(* 4 2))
(value '(^ (* 1 2) (* 4 2)))

