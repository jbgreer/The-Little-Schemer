;; What is an atom
(atom? 'a)
(atom? 1)
(atom? '())
(atom? '(a b c))

;; Aside: CONS should not evaluate its arguments
(cons 'a '())
(cons 'a '(1))
(cons 'a '(1 2))
(cons '(a) '())
(cons 'a '(+ 1 2))
(cons '(+ 1 2) '(+ 3 4))
(cons '+ '(1 2))
(eval (cons '+ '(1 2)))
