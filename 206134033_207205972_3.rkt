#lang pl 

#| role : we do all the assianment together |#

;; -----------------------------------Question 1------------------------------------------
#| This question was not difficult for us and took us an average of 5 minutes

<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL>}
        |  { union <SOL> <SOL> } 
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
       
<NumList> :: =  λ | <NUM> ;; where λ stands for the empty word, i.e., { } is the empty set

;;we will create sqence of number by calling NUM that calling DIG
;; as the example {1 3 4 1 4 }

 <DIG> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

 <NUM> ::= <DIG>
            | <DIG> <NUM>
            

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
;;This question was not difficult for me and took me an average of 5 minutes
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
    [Set  SET] ;; Set is a list of numbers so he got Set
    [Smult Number SOL] ;; Smul- miltiply by scalar so he got number and SOL
    [Inter SOL SOL] ;;Inter is a binary operator so he needs to get 2 SOL
    [Union SOL SOL] ;;Union is a binary operator so he needs to get 2 SOL
    [IdS    Symbol] 
    [WithS  Symbol SOL SOL]) ;; WithS get name variable, value and body


;; -----------------------------------Question 3------------------------------------------
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

#|
Input : Number SET
output : Boolean
This function receives number and SET and check whether the number appears in the SET. 
We create a recursive function that that run on the SET and check if the number is there.
This question was not difficult for us and took us an average of 10 minutes.
|#

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



#|
Input : SET
output : SET with only one appear of each element
This function receives SET and call 'remove-duplicates-helper' function.
The helper function gets 2 SETS (the first one is the original SET and the second init with '() SET.
The second condition check if the element appears two times and more- if true : we remove the element by
calling the 'remove-duplicates-helper' with rest and (res).
else (element appears only one time) we call 'remove-duplicates-helper' with rest and add the single
element with res. 
This question took us an average of 40 minutes, the dificult was to underestand the append syntax.
We didnt know that we need to use cons within the append to add the first element into the res.
|#
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


#|
Input : SET 
output : sorted SET  
This function gets SET and sort it by using sort function (after we delete duplicate elements using
'remove-duplicates' function).
This question was not difficult for us and took us an average of 5 minutes.
|#
(: create-sorted-set : SET -> SET)
(define (create-sorted-set l)
  (let ([l (remove-duplicates l)])
   (sort l <)))

(test (create-sorted-set '(3 4 5)) => '(3 4 5))
(test (create-sorted-set '(3 2 3 5 6)) => '(2 3 5 6))
(test (create-sorted-set '()) => '())
(test (create-sorted-set '(3 2 3 5 9 2 6)) => '(2 3 5 6 9))



#|
Input : SET SET
output : SET contains both SETS when in the result SET there are no repeats of repeating elements
This function create a new SET(append B A) and save it as ans, then it called 'remove-duplicates'
function to delete repetitive elements with (reverse ans) caz 'remove-duplicates' function delete the first
element and here we want to delete the last appearance.
At the end we reverse again the final answer.
This question was not difficult for us and took us an average of 10 minutes.
|#
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


#|
Input : SET SET
output : SET containing only elements that common to both SETS
This function has internal function 'mem-filter' that use 'ismember?' function.
filter method 'clean' the SET by operate 'mem-filter' with SET B.
The 'mem-filter' func runs over all elements of B SET and check if B member apprears in A SET, caz
'mem-filter' func compare the B member(it got as input) with all elements in A SET.
This question was not difficult for us and took us an average of 5 minutes.
|#
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


#|
Input : Number (Listof Number)
output : SET 
This function multiplies the entire SET by the number (n) it received.
The 'mul' function performs a multiply of two numbers, each time we multiply the first element of l with n
and then recursively send the rest of the SET of l elements.
The connection between the calculated value and the other elements will be made by cons. 
This question was not difficult for us and took us an average of 15 minutes.
|#
(: mul : Number Number -> Number)
    (define (mul n1 n2)
       (* n1 n2))

(: set-smult : Number (Listof Number) -> SET)
(define (set-smult n l)
  (if (null? l) ;; stop condition
      l
  (cons (mul n (first l)) (set-smult n (rest l)))))

(test (set-smult 3 '(3 4 5)) => '(9 12 15))
(test (set-smult 2 '()) => '())
(test (set-smult 0 '(3 4 5)) => '(0 0 0))
(test (set-smult -1 '(-3 -4 -5)) => '(3 4 5))
(test (set-smult -1 '(3 4 5)) => '(-3 -4 -5))

;; -----------------------------------Question 2------------------------------------------

;; Parser

#|
Input : Sexpr
output : SOL
This function receives Sexpr and converts it to SOL.
First we check the Sexpr we received and second we send it to the appropriate constructor
This question was not difficult for us and took us an average of half hour.
|#

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


;; -----------------------------------Question 4------------------------------------------
;; Substation 
#|
------------------------------------------------------
 Formal specs for `subst':
   (`Set' is a <NumList>, E, E1, E2 are <SOL>s, `x' is some <id>,
   `y' is a *different* <id>)
      Set[v/x]              = Set
      {smult n E}[v/x]      = {smult n E[v/x]}
      {inter E1 E2}[v/x]    = {inter E1[v/x] E2[v/x]}
      {union E1 E2}[v/x]    = {union E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#


#|
Input : SOL Symbol SOL
output : SOL
This function replaces all free instances by checking the expr type (using cases that check the
constructor type).
This question was not difficult for us and took us an average of 20 minutes.
|#
(: substS : SOL Symbol SOL -> SOL)
(define (substS expr from to)
  (cases expr
    [(Set n) expr] ;; case Set
    [(Smult n s) (Smult n (substS s from to))] ;; case Smult - send s to substS with from and to
    [(Inter l r) (Inter (substS l from to) (substS r from to))] ;; case Inter-send l,r to substS with from and to
    [(Union l r) (Union (substS l from to) (substS r from to))] ;; case Union-send l,r to substS with from and to
    [(IdS name) (if (eq? name from) to expr)] ;; case IdS
    [(WithS bound-id named-expr bound-body) ;; case WithS
     (WithS bound-id ;; variable 
           (substS named-expr from to) ;; value 
           (if (eq? bound-id from) ;; body
            bound-body
            (substS bound-body from to)))]))

     
    

;; -----------------------------------Question 5------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    ;; Please complete the missing parts in the formal specifications below

    eval({ N1 N2 ... Nl })  =  (sort (create-set (N1 N2 ... Nl)))
                               where create-set removes all duplications from
                              the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E}) =  (K*N1 K*N2 ... K*Nl) if (N1 N2 ... Nl) = eval(E,) is a sorted set AND
                                = error! otherwise (if S is not a sorted set)
    eval({intersect E1 E2}) = (sort (create-set (set-intersection (eval(E1,) , eval(E2,))))
                                    if both E1 and E2 evaluate to sorted sets
                                = error! otherwise
    eval({union E1 E2}) = (sort (create-set (eval(E1,) , eval(E2,))))
                                  if both E1 and E2 evaluate to sorted sets
                             = error! otherwise
    eval({with {x E1} E2}) = eval(E2,extend(x,eval(E1,),)) 
|#



;;---------  the eval procedure ------------------------------

#|
Input : SOL 
output : SET
This function returns SET. it calculates the SET according to the constructor
(an explanation of each possible constructor pattern is written in the code).
This question took us an average of 15 minutes.
|#
(: eval : SOL -> SET)
;; evaluates SOL expressions by reducing them to set values
(define (eval expr)
  (cases expr ;; checking the patterns of expr
    [(Set S) (create-sorted-set S)]  ;; sort and remove-duplicates by calling 'create-sorted-set'
    [(Smult n set) (set-smult  n (eval set))];;Smult using 'set-smult that get number n and Set set
    [(Inter l r) (set-intersection (eval l) (eval r))];;Inter using 'set-intersection with eval on l and r sides 
    [(Union l r) (set-union (eval l) (eval r))];;Union using 'set-union with eval on l and r sides 
    [(WithS name named body) ;; case WithS
     (eval (substS body ;; body
                   name ;; varible
                   (Set (eval named))))] ;; value
    [(IdS name) (error 'eval "free identifier: ~s" name)])) ;; case IdS



(: run : String -> SET)
;; evaluate a SOL program contained in a string
(define (run str)
  (eval (parseS str)))

 
(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}} {with {x {4 5 7 6 9 8 8 8}} {union x S}}}") => '(2 3 4 5 6 7 8 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
              {union {scalar-mult 3 B}
                 {4 5 7 9 8 8 8}}}")
      =error> "eval: free identifier:")