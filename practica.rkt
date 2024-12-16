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
  [delay e]     ; Nueva cláusula para delay
  [force e]     ; Nueva cláusula para force
  [lazy param body]
  [force-lazy e]
  [stl-op op args] ;Similar a C++ donde tenemos una stl que tiene las operacions de map, vector, y string, aqui tambien tendremos una "stl"
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

(define stl-operations
  (list
   (cons 'strApp string-append)
   (cons 'strAt (λ(string index) (string-ref string index)))
   (cons 'str=? string=?)
   (cons 'str-upper string-upcase)
   (cons 'str-lower string-downcase)
   (cons 'str-len string-length)
   (cons 'str-substring substring)
   (cons 'str-reverse (λ (s) (list->string (reverse (string->list s)))))
   (cons 'my-map '())
   (cons 'my-reject '())
   (cons 'head '())
   (cons 'tail '())
   (cons 'empty '())
   )
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


(define (primitive? op)
  (or (assoc op numeric-primitives) (assoc op boolean-primitives))
  ;aqui ya no tomamos en cuenta los string primitives ni los lists primitives por que conceptualmente no son primitivas
  ; y forman parte de la stl de nuestro lenguaje
  )

; creamos un nuevo tipo de "verificador o checker de operacion"

(define (stl? op)
  (or (assoc op stl-operations) #f))


; wrap-withs procesa un bloque `with` con múltiples variables
; usamos foldr para ir de derecha a izquierda, anidando cada variable 
; como un `fun` que envuelve al cuerpo. 
; ejemplo:
; (wrap-withs '((x 1) (y 2)) '(+ x y))
; resultado:
; (app (fun 'x (app (fun 'y '(+ x y)) (parse 2))) (parse 1))

(define (wrap-withs vars body)
  (foldr (λ (var total-body)
           ; aquí match nos ayuda a manejar el formato (id expr)
           (match var
             [(list id expr) 
              ; construimos un `app` con un `fun` que toma el id
              (app (fun id total-body) (parse expr))]))
         (parse body) ; el caso base es el cuerpo parseado
         vars)) ; iteramos sobre las variables

; wrap-funs convierte una lista de parámetros en una función currificada
; usamos foldr porque la función más interna será la última de la lista,
; anidando los parámetros de derecha a izquierda.
; ejemplo:
; (wrap-funs '(x y z) '(+ x y z))
; resultado:
; (fun 'x (fun 'y (fun 'z '(+ x y z))))

(define (wrap-funs arguments body)
  (foldr (λ (arg total-body) 
           ; creamos un fun que toma el argumento actual y envuelve al resto
           (fun arg total-body))
         body ; caso base: el cuerpo
         arguments)) ; iteramos sobre los argumentos

; wrap-invokes convierte múltiples argumentos en aplicaciones anidadas
; usamos foldr para aplicar los argumentos de derecha a izquierda, creando
; aplicaciones parciales hasta llegar a la aplicación completa.
; ejemplo:
; (wrap-invokes 'f '(1 2 3))
; resultado:
; (app (app (app 'f 1) 2) 3)

(define (wrap-invokes fun args)
  (foldl (λ (arg total-body) 
           ; cada iteración aplica un argumento al resultado acumulado
           (app total-body arg))
         fun ; caso base: la función misma
         args)) ; iteramos sobre los argumentos


(define (wrap-lazy arguments body)
  (foldr (λ (arg total-body)
           (lazy arg total-body))  ; Envolver cada parámetro en un `lazy`
         body  ; Caso base: el cuerpo
         arguments))  ; Iterar sobre los argumentos


(define (wrap-invokes-lazy fun args)
  (foldl (λ (arg total-body)
           (app total-body arg))  ; Las aplicaciones siguen siendo normales
         fun  ; Caso base: la función misma
         args))  ; Iterar sobre los argumentos



; parse : Src -> Expr
(define (parse src)
  (match src
    [(? number?) (num src)] ; si pasa un filtro
    [(? boolean?) (bool src)]
    [(? string?) (String src)]
    [(? symbol?) (id src)]
    [(cons 'list elems) (list-expr (map parse elems))]
    [(list 'with variables body) (wrap-withs variables body)]
    ;[(list 'rec-y (list x e) b) (parse `(with ((,x (Y (fun (,x) ,e)))),b))]
    [(list 'abs arg) (prim 'abs (list (parse arg)))]
    [(list 'sqrt arg) (prim 'sqrt (list (parse arg)))]
    [(list 'not arg) (prim 'not (list (parse arg)))]
    [(list 'str-upper str) (stl-op 'str-upper (list (parse str)))]
    [(list 'str-lower str) (stl-op 'str-lower (list (parse str)))]
    [(list 'str-len str) (stl-op 'str-len (list (parse str)))]
    [(list 'str-reverse str) (stl-op 'str-reverse (list (parse str)))]
     [(list 'head lst) (stl-op 'head (list (parse lst)))]
    [(list 'tail lst) (stl-op 'tail (list (parse lst)))]
    [(list 'empty lst) (stl-op 'empty (list (parse lst)))]
    
    [(list 'rec-y (list x e) b)  (parse `(with ((,x (Y (fun (,x) ,e)))) ,b))]
    [(list 'fun args body) (wrap-funs args (parse body))]
    [(list 'lazy args body) (wrap-lazy args (parse body))] 
    [(cons 'seqn exprs) (seqn (map parse exprs))]
    [(list 'iff c t f) (if-tf (parse c) (parse t) (parse f))] ;ESTE ES IGUAL AL PRIM OP ARGS EN CUANDO A ARGS
    [(list 'lazy e) (lazy (parse e))]
    [(list 'delay e) (delay (parse e))]   ; Añadir soporte para delay
    [(list 'force e) (force (parse e))]   ; Añadir soporte para force
    [(list 'force-lazy e) (force-lazy (parse e))] 
    [(list fun args) (match args
                       [(? number?) (app (parse fun) (parse args))] ;para cuando nos topemos con (invoke my-fun 42)
                       ; si obviamos este caso obtendremos un error
                       ;   match: no matching clause for 2
                       ; y asi para el otro tipo igual
                       [(? boolean?) (app (parse fun) (parse args))]
                       [(? symbol?) (app (parse fun) (parse args))]
                       [(cons h t) (if (symbol? (first args))
                                             (app (parse fun) (parse args)) ;caso un arg        
                                             (wrap-invokes (parse fun) (map parse args)))] ; caso muchos args
                                             ; como son varios argumentos, parseamos todo con un args
                       )
     ]
    [(list lazy args)  ; Caso para invocar funciones perezosas
 (match args
   [(? number?) (app (parse lazy) (parse args))]
   [(? boolean?) (app (parse lazy) (parse args))]
   [(? symbol?) (app (parse lazy) (parse args))]
   [(cons h t)
    (if (symbol? (first args))
        (app (parse lazy) (parse args))
        (wrap-invokes-lazy (parse lazy) (map parse args)))])]
    [(cons op args) (cond
                  [(primitive? op) (prim op (map parse args))]
                  [(stl? op) (stl-op op (map parse args))])]
    
   
    )
  )
#|


|#

(define Y-expr
  (parse '{fun {f} 
            {with [{h {fun {g} {fun {n} {{f {g g}} n}}}}] 
              {h h}}}))



; Como puedo representar a una funcion como valor? 

(deftype Val
  (valV v)
  (boolV b)
  (StringV s)
  (listV elems)
  (closureV arg body env); fun + env
  (promV expr env cache) ; promise
  )


; interp :: Expr Fundefs Env -> Valor
; evaluates an arithmetic expression
(define (interp expr env)
  (match expr
    [(num n) (valV n)]
    [(bool b) (valV b)]
    [(String s) (valV s)]
    [(prim op args) (prim-ops op (map (λ (a) (interp a env)) args))]
    [(stl-op op args)
     (stl-ops op (map (λ (a) (interp a env)) args))
    ]
    [(id x) (env-lookup x env)]
    [(fun x b) (closureV x b env)]
    
    [(lazy param body) 
     (promV (fun param body) env '())]
    
    [(with x ne b)
     (interp b (extend-env x (interp ne env) env))]
    [(if-tf c t f) (if (valV-v (interp c env))
                         (interp t env)
                         (interp f env))]
    [(force-lazy e)
  (let ([prom (interp e env)])  ; Interpreta `e` para obtener una promesa
    (if (promV? prom)
        (let ([lazy-fun (interp (promV-expr prom) (promV-env prom))])
          (match lazy-fun
            [(closureV param body fenv)
             (closureV param body fenv)]  ; Devuelve el cierre si no tiene argumentos
            [else
             (error "force-lazy: expected a closure")]))
        (error "force-lazy: cannot force a non-promise")))]
    
    [(app f arg)
  (let ([f-val (interp f env)])  ; Interpreta la función (puede ser closureV o promV)
    (match f-val
      [(closureV param body fenv)  ; Si es un cierre, aplica el argumento
       (interp body (extend-env param (interp arg env) fenv))]
      [(promV expr fenv cache)  ; Si es una promesa (caso `lazy`), fuerza y aplica
       (let ([lazy-fun (interp (force-lazy expr) fenv)])  ; Fuerza la promesa
         (match lazy-fun
           [(closureV param body lfenv)
            (interp body (extend-env param (interp arg env) lfenv))]  ; Aplica el argumento al cierre
           [else
            (error "Expected a closure in force-lazy evaluation")]))]
      [else
       (error "No se puede aplicar el valor a una función o promesa")]))]

    
    [(delay e) (promV e env '())]  ; Devuelve una promesa (perezosa) que se evaluará más tarde
    [(force e) 
     (let ([prom (interp e env)])
       (if (promV? prom)
           (valV-v (interp (promV-expr prom) env))  ; Evalúa la expresión de la promesa si es una promesa
           (error "Cannot force a non-promise")))]

    
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

(define initial-env
  (extend-env 'Y (interp Y-expr empty-env) empty-env))



(define (prim-ops op args) 
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
           (valV (apply (cdr (assq op boolean-primitives)) vals))])))

(define (stl-ops op args)
  (match op
    ; Caso específico para my-map
    ['my-map
     (let ([f (car args)]   ; Primer argumento: función (closureV)
           [lst (cadr args)]) ; Segundo argumento: lista (listV)
       (if (and (closureV? f) (listV? lst)) ; si ambos son elementos válidos...
           ; mapeamos la función sobre los elementos de la lista
           (listV (map valV (map (λ (elem)
                         ; Interpretamos el cierre con cada elemento
                         (valV-v (interp (app f (valV elem)) (closureV-env f)))) ; interpretamos la expr resultante
                       ; de la aplicación de f con elem en el contexto del env del closure de ese f
                       (listV-elems lst))))
           (error "my-map requires a function and a list")))] ; si no nos dan una fun y una lst, error nomás

    ; Caso específico para my-reject
    ['my-reject
     (let ([f (car args)]   ; Primer argumento: función (closureV)
           [lst (cadr args)]) ; Segundo argumento: lista (listV)
       (if (and (closureV? f) (listV? lst))
           ; Filtrar los elementos que NO cumplen la condición
           (listV (filter (λ (elem)
                                        (not (valV-v (interp (app f (valV elem)) (closureV-env f))))) ; interpretamos
                                      (listV-elems lst)))
           (error "my-reject requires a function and a list")))]

    ; Caso específico para car
    ;['car]
    ['head
     (let ([lst (car args)]) ; sacamos la lst 
       (if (listV? lst) ; check si es un val listV
           (if (empty? (listV-elems lst)) ; si esta vacia la listV-elems,osea la real
               (error "head: empty list")
               (valV (car (listV-elems lst)))) ; si no, devolvemos el prim elem de esa list
           (error "head: requires a listV"))) ; si no es una listV, error
     ]
    ['tail
     (let ([lst (car args)]) ; sacamos la lst 
       (if (listV? lst) ; check si es un val listV
           (if (empty? (listV-elems lst)) ; si esta vacia la listV-elems,osea la real
               (error "head: empty list")
               (cdr (listV-elems lst))) ; si no, devolvemos el resto de elems de esa list; mapeamos para que nos devuelva con valv
           (error "head: requires a listV"))) ; si no es una listV, error
     ]
    ['empty
     (let ([lst (car args)])
       (if (listV? lst)
           (if (empty? (listV-elems lst))
               #t ; vacía
               #f) ; no vacía
       (error "empty: requires a listV")))
     ]


    


    ; Caso general para operaciones del STL
    [_
     (let ([vals (map (λ (val)
                        (cond
                          [(valV? val) (valV-v val)] ; Si es valV, desempaqueta
                          [else val]))               ; Caso base, pasa el valor como está
                      args)]
           [func (assoc op stl-operations)]) ; Busca en las operaciones del STL
       (if func
           (valV (apply (cdr func) vals)) ; Aplica la operación genérica
           (error "Unknown STL operation")))])) ; Si no es reconocida, error



; run: Src list[Fundefs]? -> Val
(define (run prog)
  (let ([res (interp (parse prog) initial-env)])
    (match res
      [(valV n) n]
      [(boolV b) b ]
      [(StringV s) s]
      [(listV lst) lst]
      [(closureV x b env) res]
      [_ res]
      )
      ))


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

; Delay and Force Tests (PROBLEMA 3)
(test (run '{delay {+ 1 2}}) (promV (prim '+ (list (num 1) (num 2))) initial-env '()))
(test (run '{force (delay {+ 1 2})}) 3)
(test (run '{delay {+ 5 10}}) (promV (prim '+ (list (num 5) (num 10))) initial-env '()))
(test (run '{force (delay {+ 5 10})}) 15)
(test (run '{force (delay {strApp "Hello" " World"})}) "Hello World")
(test (run '{delay {== 5 5}}) (promV (prim '== (list (num 5) (num 5))) initial-env '()))
(test (run '{force (delay {== 5 5})}) #t)

; My-map and My-reject Tests (PROBLEMA 1)
(test (run '{my-map {fun {x} {+ x 1}} {list 1 2 3 4}}) (list (valV 2) (valV 3) (valV 4) (valV 5)))
(test (run '{my-reject {fun {x} {> x 2}} {list 1 2 3 4}}) (list (valV 1) (valV 2)))

;Head and Tail Tests
(test (run '{head {list 1 2 3}}) (valV 1)) ;
(test/exn (run '{head {list}}) "head: empty list") 
(test/exn (run '{head 42}) "head: requires a listV") 
(test (run '{head {list 5}}) (valV 5))
(test (run '{head {list {list 1 2} 3 4}}) (listV (list (valV 1) (valV 2))))
(test (run '{head {list "hello" "world"}}) (valV "hello"))
(test (run '{head {list #t #f #t}}) (valV #t))

(test (run '{empty {list}}) #t)




; Evaluacion perezosa con keyword lazy Tests (PROBLEMA 5)
;(test (run '{lazy {+ 1 2}}) (promV (prim '+ (list (num 1) (num 2))) initial-env #f))
;(test (run '{force {lazy {+ 1 (force {lazy {+ 2 3}})}}}) 6)

; RECURSIVIDAD USANDO Y COMBINATOR Y LAMBDA CALCULO

(test (run '{
  rec-y {fact {fun {n}
               {iff {== n 0}
                    1
                    {* n {fact {- n 1}}}}}}
  {fact 5}}
  ) 120)

(test (run '{
  rec-y {sum {fun {n}
              {iff {== n 0}
                   0
                   {+ n {sum {- n 1}}}}}}
  {sum 10}}
  ) 55)

(test (run '{
  rec-y {fib {fun {n}
              {iff {== n 0}
                   0
                   {iff {== {- n 1} 0}
                        1
                        {+ {fib {- n 1}} {fib {- n 2}}}}}}}
  {fib 10}}
  ) 55) 

(test (run '{
  rec-y {power-base2 {fun {exp}
                      {iff {== 0 exp}
                           1
                           {* 2 {power-base2 {- exp 1}}}}}}
  {power-base2 2}} ) 4)


; TESTS WITH N

(test (run '{with {{x 3} {y 4}} {+ x y}}) 7)
(test (run '{with {{x 3} {y 4} {z 8} {w 9}} {+ x y z w}}) 24)
(test (run '{with {{x {fun {a} {+ a 1}}} {y {fun {a} {+ a 1}}}} {+ {y 8} {x 6}}}) 16)

(test (run '{{fun {x y} {+ x y}} {10 20}}) 30)
(test (run '{{fun {a b c} {+ a {- b c}}} {10 9 8}}) 11)
(test (run '{with {{f {fun {x y} {* x y}}}} {f {10 5}}}) 50)
(test (run '{with {{add {fun {a b c} {+ a {+ b c}}}}} {+ {add {1 2 3}} {add {4 5 6}} {add {7 8 9}}}}) 45)

; PROBLEMA 5
(run '{lazy (x y) {+ x y}})
(run '{force-lazy (lazy (x) {+ x 5})})