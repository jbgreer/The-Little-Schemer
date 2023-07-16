;; The Little Schemer, 4th Edition
;; Chapter 4: Numbers Games

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? 14)                                         ; #t because all numbers are atoms

(atom? -3)                                         ; #t

(atom? 3.14159265)                                 ; #t

;; add1 add 1 to a numeric argument
(define add1
  (lambda (n)
    (+ 1 n)))

;; add1 adds 1 to the argument
(add1 67)                                          ; 68

(add1 '())                                         ; undefined, as () is not a number

;; sub1 substracts 1 from a numeric argument
(define sub1
  (lambda (n)
    (- n 1)))

(sub1 5)                                           ; 4

;; zero?
(zero? 0)                                          ; #t

(zero? 42)                                         ; #f

;; o+ adds two numbers together
(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

(o+ 46 12)                                         ; 58

;; o- subtracts the second number from the first number
(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

(o- 14 3)                                           ; 11

(o- 18 25)

;; tuples
(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup)
	       (addtup (cdr tup)))))))


(addtup '(1 2 3))                                    ; 6

;; multiplication n x m
(define o*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (o* n (sub1 m)))))))

(o* 2 2)                                            ; 6

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

(tup+ '(3 6 9 11 4) '(8 5 2 0))                       ; (11 11 11 11 4)


(define o>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (o> (sub1 n) (sub1 m))))))

(o> 12 40)                                             ; #f

(o> 120 40)                                             ; #t

(define o<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (o< (sub1 n) (sub1 m))))))

(o< 12 40)

(o< 12 12)

(o< 120 40)

;; numeric equality using zero?
(define o=
  (lambda (n m)
    (cond
     ((zero? m) (zero? n))
     ((zero? n) #f)
     (else (o= (sub1 m) (sub1 n))))))

(o= 12 12)

;; numeric equality using o< and o>
(define o=
  (lambda (n m)
    (cond
     ((o< n m) #f)
     ((o> n m) #f)
     (else #t))))

(o= 12 12)

;; exponentiation
(define o^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (o* n (o^ n (sub1 m)))))))

(o^ 2 4)

;; division with no remainder
(define o%
  (lambda (n m)
    (cond
     ((o< n m) 0)
     (else (add1 (o% (o- n m) m))))))

(o% 15 4)

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(length '(1 2 4 8 16))

(define pick
  (lambda (n lat)
    (cond
     ((null? lat) '())
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(pick 4 '(ham and cheese on rye with mustard))


(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat)
		 (rempick (sub1 n) (cdr lat)))))))

(rempick 4 '(ham and cheese on rye with mustard))


(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat)
		 (no-nums (cdr lat)))))))

(no-nums '(1 and 2 plus 3 after 4))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (cons (car lat)
				(all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(all-nums '(1 and 2 plus 3 after 4))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (o= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))

(eqan? 'a 'a)

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(occur 'a '(a ham and a cheese sandwich with a pickle))

(define one?
  (lambda (n)
    (cond
     ((zero? n) #f)
     (else (zero? (sub1 n))))))

(one! 1)

;; simpler
(define one?
  (lambda (n)
    (o= n 1)))

(one? 1)

;; rempick using one?
(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat)
		 (rempick (sub1 n) (cdr lat)))))))

(rempick 3 '(lemon meringue salty pie))
