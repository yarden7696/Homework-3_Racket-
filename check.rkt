#lang pl 
#| role : we do all the assianment together |#


;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
    [Set  SET]
    [Smult Number SOL] 
    [Inter SOL SOL]
    [Union SOL SOL]
    [IdS    Symbol]
    [WithS  Symbol SOL SOL])


 
;; Parser 
;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable. 
(: parse-sexprS : Sexpr -> SOL)
;; to convert s-expressions into SOLs
(define (parse-sexprS sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set ns)] ;; if we got list of numbers then we need to call 'Set' constructor
    [(symbol: name)(IdS name)] ;; calling 'IdS' constructor
    [(cons 'with more);; if we have a pair that his first element is the word 'with and the second elem is something
     (match sexpr ;; we need to check this phrase
       [(list 'with (list(symbol: name) named) body) ;; valid phrase 
                 (WithS name (parse-sexprS named) (parse-sexprS body))] 
       [else (error 'parse-sexprS "bad `with' syntax in ~s" sexpr)])] ;;invalid phrase
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexprS rhs))];;calling 'Smult' cons with sc and send rhs to 'parse-sexprS'  
    [(list 'intersect lhs rhs) (Inter (parse-sexprS lhs) (parse-sexprS rhs))];; calling 'Inter' const and send lhs,rhs to 'parse-sexprS' 
    [(list 'union lhs rhs) (Union (parse-sexprS lhs) (parse-sexprS rhs))];; calling 'Union' const and send lhs,rhs to 'parse-sexprS'
    [else (error 'parse-sexprS "bad syntax in ~s" sexpr)]))

  
 
(: parseS : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parseS str)
  (parse-sexprS (string->sexpr str)))


(test (parseS "{1 3 4 1 4 4 2 3 4 1 2 3}") => (Set '(1 3 4 1 4 4 2 3 4 1 2 3)))
(test (parseS "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{with S {intersect {1 2 3} {4 2 3}} {union S S}}") =error> "bad `with' syntax in")
(test (parseS "{}") => (Set '()))
(test (parseS "{{1 2 3} {4 2 3}}") =error> "bad syntax in")
(test (parseS "{scalar-mult 3 {4 2 3}}") => (Smult 3 (Set '(4 2 3))))
(test (parseS "{with {S {intersect {1 2 1 3 7 3} {union {1 2 3} {4 2 3}}}} {union S S}}") =>
(WithS 'S (Inter (Set '(1 2 1 3 7 3)) (Union (Set '(1 2 3)) (Set '(4 2 3)))) (Union (IdS 'S) (IdS 'S))))
(test (parseS "{with S {intersect {1 2 3} {4 2 3}} {union S S}}") =error> "parse-sexprS: bad `with' syntax in")




(: ismember? : Number SET  -> Boolean)
(define (ismember? n l)
  (if(null? l)
     #f
     (if(=(first l) n) #t
     (ismember? n (rest l)))))
     
(test (ismember? 1 '(3 4 5)) => #f)
(test (ismember? 1 '()) => #f)
(test (ismember? 1 '(1)) => #t)
(test (ismember? 1 '(3 1 5)) => #t)
(test (ismember? 1 '(3 4 1)) => #t)
(test (ismember? 3 '(3 3 5)) => #t)
(test (ismember? 4 '(3 5 6 4)) => #t) 
(test (ismember? 4 '(3 5 6 9)) => #f)
(test (ismember? 4 '(3 5 6 9)) => #f)
(test (ismember? -4 '(3 5 -4 9)) => #t)


(: mul : Number Number -> Number)
    (define (mul n1 n2)
       (* n1 n2))

(: set-smult : Number (Listof Number) -> SET)
(define (set-smult n l)
  (if (null? l)
      l
  (cons (mul n (first l)) (set-smult n (rest l)))))

(test (set-smult 3 '(3 4 5)) => '(9 12 15))
(test (set-smult 2 '()) => '())
(test (set-smult 0 '(3 4 5)) => '(0 0 0))
(test (set-smult -1 '(-3 -4 -5)) => '(3 4 5))
(test (set-smult -1 '(3 4 5)) => '(-3 -4 -5))




(: remove-duplicates-helper : SET SET  -> SET)
(define (remove-duplicates-helper l res)
  (if (null? l)
      res
  (if (ismember? (first l) (rest l))
      (remove-duplicates-helper (rest l) res) ;; element appears two times and more
   (remove-duplicates-helper (rest l) (append res (cons (first l) '())))))) ;; element appears only one time


(: remove-duplicates : SET  -> SET)
(define (remove-duplicates t)
(remove-duplicates-helper t '()))

(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))
(test (remove-duplicates '()) => '())
(test (remove-duplicates '(3 4 5 1 4 3 7)) => '(5 1 4 3 7))



(: create-sorted-set : SET -> SET)
(define (create-sorted-set l) 
  (let ([l (remove-duplicates l)])
   (sort l <)))

(test (create-sorted-set '(3 4 5)) => '(3 4 5))
(test (create-sorted-set '(3 2 3 5 6)) => '(2 3 5 6))
(test (create-sorted-set '()) => '())
(test (create-sorted-set '(3 2 3 5 9 2 6)) => '(2 3 5 6 9))





(: set-union : SET SET -> SET)
(define (set-union A B)
  (let ([ans (append B A)])
    (create-sorted-set ans)))
     

(test (set-union '(3 4 5) '(3 4 5)) => '(3 4 5)) 
(test (set-union '(3 4 5) '()) => '(3 4 5))
(test (set-union '(3 4 5) '(1)) => '(1 3 4 5))
(test (set-union '(3 4 5) '(1 2)) => '(1 2 3 4 5))
(test (set-union '(3 4 1) '(1 2)) => '(1 2 3 4))
(test (set-union '(3 4 1) '(1 2 1)) => '(1 2 3 4))
(test (set-union '(1 2 3) '(4 2 3)) => '(1 2 3 4))



(: set-intersection : SET SET -> SET)
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  (filter mem-filter B))

(test (set-intersection '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-intersection '(3 4 5) '(3)) => '(3))
(test (set-intersection '(3 4 5) '(1)) => '())
(test (set-intersection '(3 4 6) '(3 4 5)) => '(3 4))
(test (set-intersection '(3 4 6) '(6 3 4 5)) => '(6 3 4))
(test (set-intersection '(4 3 6) '(6 3 4 5)) => '(6 3 4))








(: substS : SOL Symbol SOL -> SOL)
(define (substS expr from to)
  (cases expr
    [(Set n) expr]
    [(Smult n s) (Smult n (substS s from to))]
    [(Inter l r) (Inter (substS l from to) (substS r from to))]
    [(Union l r) (Union (substS l from to) (substS r from to))]
    [(IdS name) (if (eq? name from) to expr)]
    [(WithS bound-id named-expr bound-body)
     (WithS bound-id
           (substS named-expr from to)
           (if (eq? bound-id from)
            bound-body
            (substS bound-body from to)))]))




(: eval : SOL -> SET)
;; evaluates SOL expressions by reducing them to set values
(define (eval expr)
  (cases expr
    [(Set S) (create-sorted-set S)]  ;; sort and remove-duplicates
    [(Smult n set) (set-smult  n (eval set))]
    [(Inter l r) (set-intersection (eval l) (eval r))]
    [(Union l r) (set-union (eval l) (eval r))]
    [(WithS name named body)
     (eval (substS body 
                   name
                   (Set (eval named))))]
    [(IdS name) (error 'eval "free identifier: ~s" name)]))


(: run : String -> SET)
;; evaluate a SOL program contained in a string
(define (run str)
  (eval (parseS str)))
    
(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
;;(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4)) 
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {with {x {4 5 7 6 9 8 8 8}}
                    {union x S}}}")
      => '(2 3 4 5 6 7 8 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
              {union {scalar-mult 3 B}
                 {4 5 7 9 8 8 8}}}")
      =error> "eval: free identifier:") 