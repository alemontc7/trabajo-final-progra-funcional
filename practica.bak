#lang play
(print-only-errors #t)
#|
bnf
<expr> ::= <number?> | <boolean?> | <string?> | <symbol?>
         | {+ <expr> <expr>}
         | {- <expr> <expr>}
         | {* <expr> <expr>}
         | {/ <expr> <expr>}
         | {< <expr> <expr>}
         | {> <expr> <expr>}
         | {== <expr> <expr>}
         | {<= <expr> <expr>}
         | {>= <expr> <expr>}
         | {!= <expr> <expr>}
         | {abs <expr>}
         | {expt <expr> <expr>}
         | {sqrt <expr>}
         | {and <expr> <expr>} | {&& <expr> <expr>}
         | {or <expr> <expr>} | {|| <expr> <expr>}
         | {not <expr>} | {! <expr>}
         | {xor <expr> <expr>} | {!|| <expr> <expr>}
         | {nand <expr> <expr>} | {~& <expr> <expr>} 
         | {equiv <expr> <expr>} | {<-> <expr> <expr>} 
         | {strApp <expr> <expr>}
         | {strAt <expr> <expr>}
         | {str=? <expr> <expr>}
         | {str-upper <expr>}
         | {str-lower <expr>}
         | {str-len <expr>}
         | {str-substring <expr> <expr> <expr>}
         | {str-reverse <expr>}
         | {with {<sym> <expr>} <expr>}
         | {id <sym>}
         | {fun {<id>} <expr>} ; definicion de funcion
         | {<expr> <expr>}; aplicacion de funcion
         | {if-tf <expr> <expr> <expr>}

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
  [seqn exprs]
  )

(define numeric-primitives
  (list
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ /)
   (cons '< <)
   (cons '> >)
   (cons '== =)
   (cons '<= <=)
   (cons '>= >=)
   (cons '!= (λ (x y) (not (= x y))))
   (cons 'abs abs)
   (cons 'expt expt)
   (cons 'sqrt sqrt)
   
   ))

(define boolean-primitives
  (list
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
   (cons 'nand (λ (a b) (not (and a b))))
   (cons '~& (λ (a b) (not (and a b))))
   (cons 'equiv (λ (a b) (equal? a b)))
   (cons '<-> (λ (a b) (equal? a b)))
   )
  )

(define string-primitives
  (list
   (cons 'strApp string-append)
   (cons 'strAt (λ(string index) (string-ref string index)))
   (cons 'str=? string=?)
   (cons 'str-upper string-upcase)
   (cons 'str-lower string-downcase)
   (cons 'str-len string-length)
   (cons 'str-substring substring)
   (cons 'str-reverse (λ (s) (list->string (reverse (string->list s)))))
   )
)

(define list-primitives
  (list
   (cons 'my-map
         (λ (f lst)
           (if (and (list? lst) (closureV? f))
               (listV (map (λ (elem)
                             (valV-v (interp (app f (valV elem)) (closureV-env f))))
                           (listV-elems lst)))
               (error "my-map requires a function and a list"))))
   (cons 'my-reject
         (λ (f lst)
           (if (and (list? lst) (closureV? f))
               (listV (filter (λ (elem)
                                (not (valV-v (interp (app f (valV elem)) (closureV-env f)))))
                              (listV-elems lst)))
               (error "my-reject requires a function and a list"))))
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


(define (primitive? op)
  (or (assoc op numeric-primitives) (assoc op boolean-primitives) (assoc op string-primitives) (assoc op list-primitives))
  )

; parse : Src -> Expr
(define (parse src)
  (match src
    [(? number?) (num src)] ; si pasa un filtro
    [(? boolean?) (bool src)]
    [(? string?) (String src)]
    [(? symbol?) (id src)]
    [(cons 'list elems) (list-expr (map parse elems))]
    [(list 'with (list x e) b) (app (fun x (parse b)) (parse e))]
    [(list 'fun (list x) b) (fun x (parse b))]
    ;[(list fname arg)
    ; (if (primitive? fname)
     ;    (prim fname (map parse arg))
      ;   (app (parse fname) (parse arg))
      ;)
     ;]
    [(cons 'seqn exprs) (seqn (map parse exprs))]
    [(list 'iff c t f) (if-tf (parse c) (parse t) (parse f))] ;ESTE ES IGUAL AL PRIM OP ARGS EN CUANDO A ARGS
    ;function application
    [(list fname arg) (if (primitive? fname)
                       (prim fname (list (parse arg)))
                       (app (parse fname) (parse arg)))]
    ; primitive op
    ; El interp confundia un primitvo que tomaba un argumento por un free identifier y devolvia un app f e
    ; por lo que decidimos aumentar ese if extra en ambos casos para evitar confusiones 
    [(cons op args) (if (primitive? op)
                       (prim op (map parse args))
                       (app (parse op) (map parse args)))]
    
    
    
    ;YA QUE C T y F son args
    ;[(cons op args) (prim op (map parse args))] ; (num 1) (num 2) (num 3) (num 4)
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
    [(prim op args) (prim-ops op (map (λ (a) (interp a env)) args))]
    [(id x) (env-lookup x env)]
    [(fun x b) (closureV x b env)]
    [(with x ne b)
     (interp b (extend-env x (interp ne env) env))]
    [(if-tf c t f) (if (valV-v (interp c env))
                         (interp t env)
                         (interp f env))]
    [(app f e)
     (def (closureV arg body fenv) (interp f env))
     (interp body (extend-env arg (interp e env) fenv))
     ]
    [(valV v) v] ; Manejo explícito para evitar duplicados de valV
    [(listV lst) (listV lst)]
    [(closureV arg body fenv) (closureV arg body fenv)]
    [(list-expr elems) (listV (map (λ(a) (interp a env)) elems))]
    [(seqn exprs) 
     (if (empty? exprs)
         (error "seqn requires at least one expression")
         (for/fold ([result (valV 0)])  ; valor inicial por defecto
                  ([expr exprs])
           (interp expr env)))]  ; retorna el último valor
    )
  )



(define (prim-ops op args) 
  (if (eq? op 'my-map)
      ; Caso específico para my-map
      (let ([f (car args)]  ; Primer argumento: función (closureV)
            [lst (cadr args)]) ; Segundo argumento: lista (listV)
        (if (and (closureV? f) (listV? lst))
            ; Mapear la función sobre los elementos de la lista
            (listV (map (λ (elem)
                          ; Interpretamos el cierre con cada elemento
                          (interp (app f (valV elem)) (closureV-env f)))
                        (listV-elems lst)))
            (error "my-map requires a function and a list")))
      ; Caso específico para my-reject
      (if (eq? op 'my-reject)
          (let ([f (car args)]  ; Primer argumento: función (closureV)
                [lst (cadr args)]) ; Segundo argumento: lista (listV)
            (if (and (closureV? f) (listV? lst))
                ; Filtrar los elementos que NO cumplen la condición
                (listV (filter (λ (elem)
                                 (not (valV-v (interp (app f (valV elem)) (closureV-env f)))))
                               (listV-elems lst)))
                (error "my-reject requires a function and a list")))
      ; Caso general para otras primitivas
      (let ([vals (map (λ (val)
                         (cond
                           [(valV? val) (valV-v val)] ; Si es valV, desempaqueta
                           [(closureV? val) val]      ; Si es closureV, lo deja igual
                           [else val]))               ; Caso base, pasa el valor como está
                       args)])
        (match op 
          [(? (λ (o) (assoc o numeric-primitives)))
           (valV (apply (cdr (assq op numeric-primitives)) vals))]
          [(? (λ (o) (assoc o boolean-primitives)))
           (valV (apply (cdr (assq op boolean-primitives)) vals))]
          [(? (λ (o) (assoc o string-primitives)))
           (valV (apply (cdr (assq op string-primitives)) vals))]
          [(? (λ (o) (assoc o list-primitives)))
           (valV (apply (cdr (assq op list-primitives)) vals))])))))





; run: Src list[Fundefs]? -> Val
(define (run prog)
  (let ([res (interp (parse prog) empty-env)])
    (match res
      [(valV n) n]
      [(boolV b) b ]
      [(StringV s) s]
      [(listV lst) lst]
      [(closureV x b env) res]
      )
      ))

#|(test (run '{+ 1 2}) 3)
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
            {addN 100000}})|#


; Numeric Primitives Tests
(test (run '{+ 1 2}) 3)
(test (run '{- 10 5}) 5)
(test (run '{* 4 3}) 12)
(test (run '{/ 10 2}) 5)
(test (run '{< 3 5}) #t)
(test (run '{> 7 2}) #t)
(test (run '{== 5 5}) #t)
(test (run '{<= 4 4}) #t)
(test (run '{>= 6 5}) #t)
(test (run '{!= 5 3}) #t)
(test (run '{abs -5}) 5)
(test (run '{abs 5}) 5)
(test (run '{expt 2 3}) 8)
(test (run '{sqrt 16}) 4)

; Boolean Primitives Tests
(test (run '{and #t #t}) #t)
(test (run '{&& #t #t}) #t)
(test (run '{or #f #t}) #t)
(test (run '{|| #f #t}) #t)
(test (run '{not #t}) #f)
(test (run '{! #t}) #f)
(test (run '{xor #t #f}) #t)
(test (run '{nand #t #t}) #f)
(test (run '{equiv #t #t}) #t)

; String Primitives Tests
(test (run '{strApp "Hello" " World"}) "Hello World")
(test (run '{strAt "Hello" 0}) #\H)
(test (run '{str=? "hello" "hello"}) #t)
(test (run '{str-upper "hello"}) "HELLO")
(test (run '{str-lower "HELLO"}) "hello")
(test (run '{str-len "hello"}) 5)
(test (run '{str-substring "hello" 0 2}) "he")
(test (run '{str-reverse "hello"}) "olleh")