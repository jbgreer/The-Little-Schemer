; TLS Chapter 3: cons the magnificent

; member? from Ch 2
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
	       (member? a (cdr lat)))))))

; rember : remove a member
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (cdr lat))
     (else (cons (car lat)
		 (rember a (cdr lat)))))))

(rember 'mint '(lamb chop and mint jelly))

					; firsts
(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l))
		 (firsts (cdr l)))))))

(firsts '((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant)))

(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (cdr (car l)))
		 (seconds (cdr l)))))))

(seconds '((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant)))

; THE THIRD COMMANDMENT: When building a list, describe the first typical element, and then cons it onto the natural recursion

; insert new atom to the right of first occurrence of old atom in lat
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat))
      (cons old
	    (cons new (cdr lat))))
     (else (cons (car lat)
		 (insertR new old (cdr lat)))))))

(insertR 'topping 'fudge '(ice cream with fudge for dessert))

; insert new atom to left of first occurrence of old atom in lat
(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat))
      (cons new lat))
     (else (cons (car lat)
		 (insertL new old (cdr lat)))))))

(insertL 'topping 'fudge '(ice cream with fudge for dessert))

; substitute first occurence of old atom with new atom in lat
(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat))
      (cons new (cdr lat)))
     (else (cons (car lat)
		 (subst new old (cdr lat)))))))

(subst 'caramel 'fudge '(ice cream with fudge for dessert))

; substitute the first occurrence of either o1 or o2 with new in the atom lat
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or (eq? o1 (car lat)) (eq? o2 (car lat)))
      (cons new (cdr lat)))
     (else (cons (car lat)
		 (subst2 new o1 o2 (cdr lat)))))))

(subst2 'topping 'foo 'cream '(ice cream with fudge for dessert))

; multirember remove all instances of atom a from lat
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (multirember a (cdr lat)))
     (else (cons (car lat)
		 (multirember a (cdr lat)))))))

(multirember 'foo '(foo bar foo quux))

(multirember 'cup '(coffee cup tea cup and hick cup))

; multiinsertR
(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons (car lat)
				(cons new
				      (multiinsertR new old (cdr lat)))))
     (else (cons (car lat)
		 (multiinsertR new old (cdr lat)))))))

(multiinsertR 'cup 'spoon '(coffee cup tea cup and hick cup))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new
				(cons old
				      (multiinsertL new old (cdr lat)))))
     (else (cons (car lat)
		 (multiinsertL new old (cdr lat)))))))

(multiinsertL 'spoon 'cup '(coffee cup tea cup and hick cup))

; THE FOURTH COMMANDMENT: (preliminary) Always change at least one argument while recurring
; IT must be changed to be closer to termination.  The changing argument must be tested in the
; termination condition: when using cdr, test termination with null?

; multisubst - replace all instances of atom old with atom new in lat
(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new
				(multisubst new old (cdr lat))))
     (else (cons (car lat)
		 (multisubst new old (cdr lat)))))))

(multisubst 'spoon 'cup '(coffee cup tea cup and hick cup))
