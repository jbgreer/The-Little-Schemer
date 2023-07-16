; Chapter 2: Do it, do it again, do it again....

; use TLS definition of atom?
(define my-atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; lat? idenfifies lists-of-atoms
(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((my-atom? (car l)) (lat? (cdr l)))
     (else #f))))

(lat? '())                                    ; #t.  

(lat? 'a)                                     ; not defined as a is not a list

(lat? '(bacon and eggs))                      ; #t

(lat? '((bacon) and eggs))                    ; #f
(lat? '(bacon (and) eggs))                    ; #f
(lat? '(bacon and (eggs)))                    ; #f

(atom? '())


; or checks questions one at a time and stops when one answer is true
(or (null? '()) (atom? '(d e f g)))     ; #t because (null? '()) is #t

(or (atom? '(d e f g)) (null? '()))     ; #t

(define f
  (lambda (x)
    f(x)))

(or (null? '()) (f 1))                  ; #t, because evaluation short circuits and order of eval is left to right

(or #t)                                 ; #t

(or #t #f)                              ; #t

(or #f)                                 ; #f

(or #f #f #t)                           ; #t


; member? is atom a in list of atoms

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
	       (member? a (cdr lat)))))))

(member? 'a '(a b c))                    ; #t
(member? '() '())                        ; #f
(member? 'a '())                         ; #f
(member? '(a) '((a)))                    ; #f
(member? '(a) '(a b c))                  ; #f
(member? '() '(a b c))                   ; #f


; THE FIRST COMMANDMENT: (preliminary) Always ask null? as the first question in expressing any function

