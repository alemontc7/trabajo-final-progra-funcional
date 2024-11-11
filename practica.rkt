#lang play
(print-only-errors #t)
#|
<expr> ::= <number?> | <boolean?> | <symbol?>
         | {+ <expr> <expr>}
         | {- <expr> <expr>}
         | {with {<sym> <expr>} <expr>}
         | {id <sym>}
         | {fun {<id>} <expr>} ; definicion de funcion
         | {<expr> <expr>}; aplicacion de funcion

F1WAE
--> funciones no son valores, no las produce interp, map? filter?
(filter even? (list 1 2 3 4))
(map even? (list 1 2 3 4))
--> se llaman solo por el nombre
((addN 3) 4)
--> espacio especial, especifico para los fundefs


; por que hablamos de orden
add1: num -> num     ; primer grado

map : fun list[a] -> list[b]
map: (a -> b) list[a] -> list[b]  ; segundo grado

addN: n -> fun
addN: n -> (n -> n)

compose: (a -> b) -> (b -> c) -> a -> c
(f (g (x)))


FAE
--> funciones como valores, funciones de primera clase, tiene todos los derechos de los valores
3 retornado, usado como argumento, evaluar, almacenarlo
--> las funciones pueden definirse como cualquier valor, no necesito fundefs.
funciones pueden ir en el env. 
|#


(deftype Expr
  [num n]
  [bool b]
  [add l r]
  [sub l r]
  [id x]
  [with x e b]
  [fun param body]
  [app fun-name expr]
  )

(deftype Env
  (mtEnv)
  (aEnv id val env)
  )

(define empty-env (mtEnv))
(define extend-env aEnv)
(define (env-lookup x env)
  (match env
    [(mtEnv) (error "free identifier")]
    [(aEnv id val e) (if (symbol=? id x)
                         val
                         (env-lookup x e))]
    )
  )


; parse : Src -> Expr
(define (parse src)
  (match src
    [(? number?) (num src)] ; si pasa un filtro
    [(? boolean?) (bool src)]
    [(? symbol?) (id src)]
    [(list '+ s1 s2) (add (parse s1) (parse s2))]
    [(list '- s1 s2) (sub (parse s1) (parse s2))]
    [(list 'with (list x e) b) (with x (parse e) (parse b))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list fname arg) (app (parse fname) (parse arg))]
    )
  )

; Como puedo representar a una funcion como valor? 

(deftype Val
  (numV n)
  (boolV b)
  (closureV arg body env); fun + env
  )

; interp :: Expr Fundefs Env -> Valor
; evaluates an arithmetic expression
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(id x) (env-lookup x env)]
    [(add l r) (numV+ (interp l env) (interp r env))]
    [(sub l r) (numV- (interp l env) (interp r env))]
    [(fun x b) (closureV x b env)]
    [(with x ne b)
     (interp b (extend-env x (interp ne env) env))]
    [(app f e)
     (def (closureV arg body fenv) (interp f env))
     (interp body (extend-env arg (interp e env) fenv))
     ]
    )
  )

; funciones auxiliares
(define (numV+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))

(define (numV- n1 n2)
  (numV (- (numV-n n1) (numV-n n2))))

; run: Src list[Fundefs]? -> Val
(define (run prog)
  (let ([res (interp (parse prog) empty-env)])
    (match res
      [(numV n) n]
      [(boolV b) b ]
      [(closureV x b env) res]
      )
      ))

(test (run '{+ 1 2}) 3)
(test (run '{+ {- 5 2} 1}) 4)
(test/exn (run 'x) "free identifier")

(run '{fun {x} {+ x 1}})
(run '{{fun {x} {+ x 1}} 2})
(run '{with {foo {fun {x} {+ x 1}}} foo})
(run '{with {foo {fun {x} {+ x 1}}} {foo 2}})

(run '{with {foo {fun {x} {x 1}}}
            {with {fee {fun {y} {+ y 1}}} {foo fee}}})

(run '{{fun {x} {x 1}} {fun {y} {+ y 1}}})

(run '{with {addN {fun {n} {fun {m} {+ n m}}}}
            {{addN 3} 2}})

(run '{with {addN {fun {n} {fun {m} {+ n m}}}}
            {addN 100000}})











