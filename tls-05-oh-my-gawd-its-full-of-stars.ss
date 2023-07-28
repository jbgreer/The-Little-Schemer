;; The Little Schemer, 4th Edition
;; Chapter 5: *Oh My Gawd* It's Full of Stars


;; rember* - recursively 
(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
     (else (cons (rember* a (car l))
	    (rember* a (cdr l)))))))

(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))


(define lat?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((atom? (car lat)) (lat? (cdr lat)))
     (else #f))))

(lat? '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
 
(lat? '(tomato sauce bean))


(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
     (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))

;; The First Commandment, Final Version
;; When recurring on a list of atoms, lat, ask 2 qs:  (null? lat) and else
;; When recurring on a number, n, ask 2 qs: (zero? n) and else
;; When recurring on a list of S-expressions, ask 3 qs: (null? l), (atom? (car l)) and else

;; The Fouth Commandment, Final Version
;; Always change at least one argument while recurring.
;; When recurring on a list of atoms, lat, use (cdr lat).
;; When recurring on a number, n, use (sub1 n).
;; And when recurring on a list of S-expressions, use (car l) and (cdr l)
;; if neither (null? l) nor (atom? (car l)) are true.
;;
;; It must be changed to be closer to termination.  The changing argument
;; must be tested in the termination condition:
;;
;; when using cdr, test termination with null? and
;; when using sub1, test termination with zero?


;; o+ adds two numbers together
(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(occur* 'banana '((banana) (split (((banana ice))) (cream (banana)) sherbert)))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(subst* 'orange 'banana '((banana) (split (((banana ice))) (cream (banana)) sherbert)))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
       (else (cons (car l) (insertL* new old (cdr l))))))
     (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(insertL* 'pecker 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if a ((wood chuck))) could chuck wood))


(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or (eq? (car l) a) (member* a (cdr l))))
     (else (or (member* a (car l)) (member* a (cdr l)))))))

(member* 'chips '((potato) (chips ((with) fish) (chips))))


(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(leftmost '((potato) (chips ((with) fish) (chips))))
