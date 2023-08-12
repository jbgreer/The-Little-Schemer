;; The Little Schemer, 4th Edition
;; Ch. 8 Lambda the Ultimate


;; rember-f : remove the first matching atom a from a list l, using function test? to indicate match
(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? a (car l)) (cdr l))
     (else (cons (car l)
		 (rember-f test? a (cdr l)))))))

(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake))) ;; => (lemonade and (cake))
(rember-f = 2 '(1 2 3 4 )) ;; (1 3 4)


;; a function with value of a function comparing a and x
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

;; a particular instance of the function eq?-c.  the name is arbitrary
(define eq?-salad
  (eq?-c 'salad))

(eq?-salad 'salad) ;; => #t : in this case, x has value 'salad.  
(eq?-salad 'tuna)  ;; => #f : in this case, s has value 'tuna

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else (cons (car l)
		   ((rember-f test?) a (cdr l))))))))     ;; NOTE (rember-f test?) is a fn...

(define rember-eq? (rember-f eq?))
(define rember-equal? (rember-f equal?))

(rember-eq? 2 '(1 2 3 4 ))
(rember-equal? '(pop corn) '(lemonade (pop corn) and (cake)))

((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?)) ;; here eq? as argument is quoted

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old) (cons new
				  (cons old (cdr l))))
       (else (cons (car l)
		   ((insertL-f test?) new old (cdr l))))))))

(define insertL-equal? (insertL-f equal?))
(insertL-equal? '(pop corn) 'and '(lemonade and (cake)))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old) (cons old
				  (cons new (cdr l))))
       (else (cons (car l)
		   ((insertR-f test?) new old (cdr l))))))))

(define insertR-equal? (insertL-f equal?))
(insertR-equal? 'and '(pop corn) '(lemonade (pop corn) (cake)))


(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(seqL 'a 'b '(b c))  ;; => (a b b c); note in insert-g we pass (cdr l)

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(seqR 'b 'a '(a c)) ;; => (a b a c); note in insert-g we pass (cdr l)

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((equal? (car l) old) (seq new old (cdr l)))
       (else (cons (car l)
		   ((insert-g seq) new old (cdr l))))))))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

(insertL '(pop corn) 'and '(lemonade and (cake)))
(insertR 'and '(pop corn) '(lemonade (pop corn) (cake)))

;; anonymous seq functions
(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

(insertL '(pop corn) 'and '(lemonade and (cake)))
(insertR 'and '(pop corn) '(lemonade (pop corn) (cake)))

;; helper for subst
(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))
(subst '(pop corn) 'and '(lemonade and (cake)))

;; also
(define subst
  (insert-g
  (lambda (new old l)
    (cons new l))))

(subst '(pop corn) 'and '(lemonade and (cake)))


;; The Ninth Commandment
;; Abstract common patterns with a new function


(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x '+) +)
     (else *))))   ;; omitting ^

;; omitting value function.... don't want to pull in previous fns

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
       (else
	(cons (car lat) ((multirember-f test?) a (cdr lat))))))))

((multirember-f equal?) 'tuna '(shrimp salad tuna salad and tuna))


;; takes a function and a lat
(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) '())
       ((test? (car lat)) (multiremberT test? (cdr lat)))
       (else (cons (car lat)
		   (multiremberT test? (cdr lat)))))))

;; function for multiremberT
(define eq?-tuna
  (eq?-c 'tuna))

(eq?-tuna 'tuna)  ;; => #t
  
(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))



