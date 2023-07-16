; What is an atom?
(atom? 'atom)   ; #t because atom is a string of characters beginning with a

(atom? 'turkey) ; #t because turkey is a string of characrters beginning with a letter

(atom? 1234)    ; #t because 1492 is a string of digits

(atom? 'u)      ; #t because u is a string of one character which is a letter

(atom? '*abc$)  ; #t because that's a string of characters that doesn't start w/( or )

; What is a list?
(list? '(atom)) ; #t because atom is enclosed by parens

(list? '(atom turkey or))  ; #t because it is a collection of atoms enclosed in parens

(list? '(atom turkey) or)   ; #f because that is 2 S-expressions not enclosed by parens

(list? '((atom turkey or)))  ; #t

; What is an S-Expression
xyz                          ; #t all atoms are S-expressions

(x y z)                      ; #t all lists are S-expressions

((x y) z)                    ; #t all lists are S-expressions

(how are you doing so far)   ; #t all lists are S-expressions; how are you doing so far = 6 S-expressions

(list? '(((how) are) ((you) (doing so)) far))    ; #t because it is a colllection of S-expressions enclosed by parens

(list? '())                   ; #t because parens enclose 0 S-Expressions; this is called the null or empty list

; Note: Chez Scheme has different ideas about atoms vs lists
(atom? '())                   ; #t in Chez Scheme

(define my-atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(my-atom? '())                ; #f in TLS because "() is just a list"

(list? '(() () ()))           ; #t


; CAR
(car '(a b c))                ; a  The car of a list is the first atom of the list

(car '((a b c) x y z))        ; (a b c)

(car 'hotdog)                 ; undefined  car only operates on lists

(car '())                     ; undefined  car only operates on non-empty lists

; THE LAW OF CAR: The primitive car is defined only for non-empty lists

(car '(((hotdogs)) (and) (pickle) relish))  ; ((hotdogs)

(car (car '(((hotdogs)) (and))))            ; (hotdogs)

; cdr "could-er"

(cdr '(a b c))                ; (b c)  cdr returns the list without the car

(cdr '(hamburger))            ; ()

(cdr '((x) t r))              ; (t r)

(cdr 'hotdogs)                ; undefined.  cdr only operates on lists

(cdr '())                     ; undefined.  cdr only operates on non-empty lists

; THE LAW OF CDR: The primitive cdr is only defined for non-empty lists.  The cdr of a non-empty list is always another list

(car (cdr '((b) (x y) ((c)))))     ; (x y)

(cdr (cdr '((b) (x y) ((c)))))     ; (((c)))

(cdr (car '(a (b (c)) d)))         ; undefined, since car always returns an atom, and cdr requires a list


; cons takes an S-expression and a list and adds the S-Expression to the front of the list

(cons 'peanut '(butter and jelly))      ; (peanut butter and jelly, because cons add an atom to the front of a list

(cons '(banana and) '(peanut butter and jelly))    ; ((banana and) peanut butter and jelly)

(cons '((help) this) '(is very ((hard) to learn))) ; (((help) this) is very ((hard) to learn))

; aside
(cons '() '())                          ; (())

(cons '(a b (c)) '())                   ; (a b (c)) because () is a list

(cons 'a '())                           ; (a)

(cons '((a b c)) b)                     ; undefined, because b is not a list

(cons 'a 'b)                            ; undefined because b is not a list

; THE LAW OF CONS: The primitive cons takes 2 arguments.  The second argument must be a list.  The result is a list

(cons 'a (car '((b) c d)))              ; (a b)

(cons 'a (cdr '((b) c d)))              ; (a c d)


; null

(null? '())                             ; #t

(null? (quote ()))                      ; #t, because (quote ()) is a notation for the null list, also '()

(null? '(a b c))                        ; #f because it is a non-empty list

(null? 'a)                              ; #f but undefined

; THE LAW OF NULL?  The primitive null? is only defined for lists.


; eq? TLS takes two non-numeric atoms, but Chez allows numeric

(eq? 'Harry 'Harry)                     ; #t because they are the same atom

(eq? 'margine 'butter)                  ; #f

(eq? 1 1)                               ; #t in Chez

(eq? '() '(strawberry))                 ; undefined, but Chez allows it and returns #f

; THE LAW OF EQ?  The primitive eq? takes two non-numeric atoms

(eq? (car '(Mary had a little lamb chop)) 'Mary)     ; #t

(eq? (cdr '(soured milk)) 'milk)         ; undefined in TLS, but in Chez #f

