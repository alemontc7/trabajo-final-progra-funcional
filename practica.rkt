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

Hola
|#


(deftype Expr
  [num n]
  [bool b]
  [String s]
  [prim op args]
  [id x]
  [with x e b]
  [fun param body]
  [if-tf c t f]                         ; (if-tf <F1WAE> <F1WAE> <F1WAE>)
  [app fun-name expr]
  [list-expr args]
  )

(define primitives
  (list
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ /)
   (cons 'and (λ (a b) (and a b))) ;usamos lambda por que al intentar hacer esto:
   ;(cons 'and and) el and no era un procedure, por lo que decidimos hacer nuestra propia procedura
   ; Los lenguajes com o python permiten que el usuario pueda escribir tanto and como &&, por lo que
   ; decidimos permitir que el usuario pueda escribir cualquiera de las dos opciones para todos
   ; los casos de operaciones binarias
   (cons '&& (λ (a b) (and a b)))
   (cons 'or (λ (a b) (or a b)))
   (cons '|| (λ (a b) (or a b)))
   (cons 'not (λ (a) (not a)))
   (cons '! (λ (a) (not a)))
   (cons 'xor (λ (a b) (xor a b)))
   (cons '!|| (λ (a b) (xor a b)))
   (cons 'strApp string-append)
   ;(cons 'strLen (λ(x) (string-length x)))
   (cons 'strAt (λ(string index) (string-ref string index)))
   (cons 'str=? string=?)
   ))

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
    [(? string?) (String src)]
    [(? symbol?) (id src)]
    [(cons 'list elems) (list-expr (map parse elems))]
    [(list 'with (list x e) b) (with x (parse e) (parse b))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list fname arg) (app (parse fname) (parse arg))]
    [(list 'if-tf c t f) (if-tf (parse c) (parse t) (parse f))] ;ESTE ES IGUAL AL PRIM OP ARGS EN CUANDO A ARGS
    ;YA QUE C T y F son args
    [(cons op args) (prim op (map parse args))] ; (num 1) (num 2) (num 3) (num 4)
    )
  )

; Como puedo representar a una funcion como valor? 

(deftype Val
  (valV v)
  (boolV b)
  (StringV s)
  (listV elems)
  (closureV arg body env); fun + env
  )

; interp :: Expr Fundefs Env -> Valor
; evaluates an arithmetic expression
(define (interp expr env)
  (match expr
    [(num n) (valV n)]
    [(bool b) (valV b)]
    [(String s) (valV s)]
    [(id x) (env-lookup x env)]
    [(prim op args) (prim-ops op (map (λ (a) (interp a env)) args))]
    [(fun x b) (closureV x b env)]
    [(with x ne b)
     (interp b (extend-env x (interp ne env) env))]
    [(if-tf c t f) (if (valV (interp c env))
                         (interp t env)
                         (interp f env))]
    [(app f e)
     (def (closureV arg body fenv) (interp f env))
     (interp body (extend-env arg (interp e env) fenv))
     ]
    [(list-expr elems) (listV (map (λ(a) (interp a env)) elems))]
    )
  )

(define (prim-ops op args) ; ((valV 1) (valV 2) (valV 3) (valV 4))
  (let ([vals (map (λ (val) (valV-v val)) args)]) ; (1 2 3 4)
    (valV (apply (cdr (assq op primitives)) vals)) ; 10 -> (+ 1 2 3 4)
  )) ; (valV 10)


; run: Src list[Fundefs]? -> Val
(define (run prog)
  (let ([res (interp (parse prog) empty-env)])
    (match res
      [(valV n) n]
      [(boolV b) b ]
      [(StringV s) s]
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










