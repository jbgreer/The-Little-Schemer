;; The Little Schemer, 4th Edition
;; Chapter 7: Friends and Relations

;; is atom a in list of atoms lat?
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (equal? (car lat) a)
	       (member? a (cdr lat)))))))

(member? 'a '(a))
(member? 'a '(b))
(member? 'a '(x y z a))

;; Sets: a set is a list of atoms in which no atom appears more than once
(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(set? '())
(set? '(a b c))
(set? '[a a b])
(set? '(a b c a))
(set? '(a b c b))
(set? '(a b c c))
(set? '(apple 3 pear 4 9 apple 3 4))

;; create a set from a list of atoms
;; note this creates a list starting with the last occurence of the atom in lat
(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat))
      (makeset (cdr lat)))
     (else (cons (car lat)
		 (makeset (cdr lat)))))))

(makeset '(apple peach pear peach plum apple lemon peach))

;; subset? checks if every member of set s1 is a member of set s2
(define subset?
  (lambda (s1 s2)
    (cond
     ((null? s1) #t)
     (else
      (and (member? (car s1) s2) (subset? (cdr s1) s2))))))

(subset? '(5 chicken wings) '(5 burgers 2 pieces fried chicken and light duckling wings))

;; eqset? checks if two sets are equal - contain the same members
(define eqset?
  (lambda (s1 s2)
    (and (subset? s1 s2) (subset? s2 s1))))

(eqset? '() '())
(eqset? '(a) '())
(eqset? '() '(a))
(eqset? '(a) '(a))
(eqset? '(a) '(b))
(eqset? '(b) '(a))
(eqset? '(a b) '(a b))
(eqset? '(a b) '(a))
(eqset? '(a b) '(b))
(eqset? '(a) '(a b))
(eqset? '(b) '(a b))

;; intersect?   does at least one member of s1 exist in s2?
(define intersect?
  (lambda (s1 s2)
    (cond
     ((null? s1) #f)
     (else (or (member? (car s1) s2) (intersect? (cdr s1) s2))))))

(intersect? '() '())
(intersect? '() '(a))
(intersect? '(a) '(a))
(intersect? '(a) '(a b))
(intersect? '(b) '(a b))

;; intersect - what is the intersection of s1 and s2
(define intersect
  (lambda (s1 s2)
    (cond
     ((or (null? s1) (null? s2)) '())
     ((member? (car s1) s2)
      (cons (car s1) (intersect (cdr s1) s2)))
     (else (intersect (cdr s1) s2)))))
   
(intersect '() '())
(intersect '() '(a))
(intersect '(a) '(a))
(intersect '(a) '(a b))
(intersect '(b) '(a b))
(intersect '(a b) '(a b))

;; union
(define union
  (lambda (s1 s2)
    (cond
     ((null? s1) s2)
     ((member? (car s1) s2) (union (cdr s1) s2))
     (else (cons (car s1) (union (cdr s1) s2))))))

(union '() '())
(union '() '(a))
(union '(a) '(a))
(union '(a) '(a b))
(union '(b) '(a b))
(union '(a b) '(a b))

;; intersectall - intersection of multiple sets.  assume all sets are non-empty
(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(intersectall '((a) (a b) (a c)))

;; a-pair?  true if a list has only 2 S-expressions
(define a-pair?
  (lambda (l)
    (cond
     ((atom? l) #f)
     ((null? l) #f)
     ((null? (cdr l)) #f)
     ((null? (cdr (cdr l))) #t)
     (else #f))))

(a-pair? '(a b))

;; first member of a pair
(define first
  (lambda (p)
    (car p)))

;; second member of a pair
(define second
  (lambda (p)
    (car (cdr p))))
     
;; build a pair from two S-expressions
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

;; Chapter 3: firsts
(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (firsts (cdr l)))))))
      

;; fun? is a mapping between a set of values and values
(define fun?
  (lambda (rel)
     (set? (firsts rel))))

(fun? '((d 4) (b 0) (b 9) (e 5)))
(fun? '((d 4) (b 0) (a 9) (e 5)))

;; revrel - given a list of relations, reverser them
(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else
      (cons (build (second (car rel))
		   (first (car rel)))
	    (revrel (cdr rel)))))))

(revrel '((8 i) (pie pumpkin) (sick got )))

;; helper - reverse a pair
(define revpair
  (lambda (p)
    (build (second p) (first p))))

(revpair '(a b))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (revpair (car rel) (revrel (cdr rel))))))))

;; one-to-one function
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))



