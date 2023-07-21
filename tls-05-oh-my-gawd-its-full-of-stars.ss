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

