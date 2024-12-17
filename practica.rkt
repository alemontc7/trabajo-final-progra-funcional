#lang play
(print-only-errors #t)
#|
bnf
<expr> ::= <number?> | <boolean?> | <string?> | <symbol?>
         | {+ <expr> ... <expr>}
         | {- <expr> ... <expr>}
         | {* <expr> ... <expr>}
         | {/ <expr> ... <expr>}
         | {< <expr> <expr>}
         | {> <expr> <expr>}
         | {== <expr> <expr>}
         | {<= <expr> <expr>}
         | {>= <expr> <expr>}
         | {!= <expr> <expr>}
         | {abs <expr>}
         | {expt <expr> <expr>}
         | {sqrt <expr>}
         | {mod <expr> <expr>}
         | {and <expr> ... <expr>} | {&& <expr> ... <expr>}
         | {or <expr> ... <expr>} | {|| <expr> ... <expr>}
         | {not <expr>} | {! <expr>}
         | {xor <expr> ... <expr>} | {!|| <expr> ... <expr>}
         | {nand <expr> ... <expr>} | {~& <expr> ... <expr>} 
         | {equiv <expr> <expr>} | {<-> <expr> <expr>} 
         | {strApp <expr> ... <expr>}
         | {strAt <expr> <expr>}
         | {str=? <expr> <expr>}
         | {str-upper <expr>}
         | {str-lower <expr>}
         | {str-len <expr>}
         | {str-substring <expr> <expr> <expr>}
         | {str-reverse <expr>}
         | {with {{<id> <expr>} ... {<id> <expr>}} <expr>}
         | {id <sym>}
         | {fun {{<id>} ... {<id>} } <expr>} ; definicion de funcion
         | {app <expr> {<expr> ...<expr>}} ;aplicacion de funcion
         | {when <expr> <expr> <expr>} ;if
         | {rec <id> <expr> <expr>}
         | {list <expr>}
         | {seqn <expr> ... <expr>}
         | {head <expr>}
         | {tail <expr>}
         | {empty? <expr>}
         | {my-map <expr> <expr>}
         | {my-reject <expr> <expr>}
         | {lazy <expr> {<expr> ...<expr>}}
         | {delay <expr>}
         | {force <expr>}
|#


(deftype Expr
  [num n]
  [bool b]
  [String s]
  [prim op args]
  [id x]
  [with x e b]
  [fun param body]
  [if-tf c t f]                        
  [app fun-name expr]
  [list-expr args]
  [seqn exprs]
  [delay e]     
  [force e]     
  [lazy fname args]
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
   (cons 'mod modulo)
   
   ))

(define boolean-primitives
  (list
   (cons 'and (λ args (foldl (λ (bool total) (and total bool)) #t args)))
   (cons '&& (λ args (foldl (λ (bool total) (and total bool)) #t args)))
   (cons 'or (λ args (foldl (λ (bool total) (or total bool)) #f args)))
   (cons '|| (λ args (foldl (λ (bool total) (or total bool)) #f args)))
   (cons 'not (λ (a) (not a)))
   (cons '! (λ (a) (not a)))
   (cons 'xor (λ args (foldl (λ (bool total) (xor total bool)) #f args)))
   (cons '!|| (λ args (foldl (λ (bool total) (xor total bool)) #f args)))
   (cons 'nand (λ args (foldl (λ (bool total) (not (and total bool))) #f args)))
   (cons '~& (λ args (foldl (λ (bool total) (not (and total bool))) #f args)))
   (cons 'equiv (λ (a b) (equal? a b)))
   (cons '<-> (λ (a b) (equal? a b)))
   )
  )

(define stl-operations
  (list
   (cons 'strApp (λ args (apply string-append args)))
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
)

(define (stl? op)
  (or (assoc op stl-operations) #f)
)


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
   vars)
) ; iteramos sobre las variables

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
         arguments)
 ) ; iteramos sobre los argumentos

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
   args)
) ; iteramos sobre los argumentos

(define (wrap-lazy-invokes fun args)
  (foldl (λ (arg total-body) 
           ; cada iteración aplica un argumento al resultado acumulado
           (lazy total-body arg))
         fun ; caso base: la función misma
         args)
 ) ; iteramos sobre los argumentos



; parse : Src -> Expr
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(? string?) (String src)]
    [(? symbol?) (id src)]
    [(cons 'list elems) (list-expr (map parse elems))]
    [(list 'with variables body) (wrap-withs variables body)]   
    [(list 'rec-y (list x e) b)  (parse `(with ((,x (Y (fun (,x) ,e)))) ,b))]
    [(list 'fun args body) (wrap-funs args (parse body))]
    [(cons 'seqn exprs) (seqn (map parse exprs))]
    [(list 'when c t f) (if-tf (parse c) (parse t) (parse f))]
    [(list 'delay e) (delay (parse e))]
    [(list 'force e) (force (parse e))]
    [(list fun args) 
     (cond
       [(primitive? fun) (prim fun (list (parse args)))]
       [(stl? fun) (stl-op fun (list (parse args)))]
       [else 
        (match args
          [(or (? number?) (? boolean?) (? symbol?)) 
           (app (parse fun) (parse args))]
          [(cons h t) (if (symbol? (first args))
                          (app (parse fun) (parse args))
                          (wrap-invokes (parse fun) (map parse args)))
          ]
        )
       ]
      )
    ]
    [(list 'lazy fun args)
     (cond
       [(primitive? fun) (lazy (prim fun (list (parse args))))]
       [(stl? fun) (lazy (stl-op fun (list (parse args))))]
       [else
        (match args
          [(or (? number?) (? boolean?) (? symbol?)) (lazy (parse fun) (parse args))]
          [(cons h t) (if (symbol? (first args))
                      (lazy (parse fun) (parse args))
                      (wrap-lazy-invokes (parse fun) (map parse args)))
          ]
         )
       ]
      )
    ]
    [(cons op args)
     (cond
       [(primitive? op) (prim op (map parse args))]
       [(stl? op) (stl-op op (map parse args))]
       [else (app (parse op) (map parse args))]
      )
    ]
    )
  )
#|


|#

(define Y-expr
  (parse '{fun {f} 
            {with [{h {fun {g} {fun {n} {{f {g g}} n}}}}] 
              {h h}}}))



(deftype Val
  (valV v)
  (boolV b)
  (StringV s)
  (listV elems)
  (closureV arg body env)
  (promiseV expr env cache)
  )


; interp :: Expr Fundefs Env -> Valor
; evaluates an arithmetic expression
(define (interp expr env)
  (match expr
    [(num n) (valV n)]
    [(bool b) (valV b)]
    [(String s) (valV s)]
    [(prim op args)
     (let* ([interpreted-args (map (λ (a) (interp a env)) args)]
            [no-promise-args (map strict interpreted-args)])
       (prim-ops op no-promise-args))]
    [(stl-op op args)
     (stl-ops op (map (λ (a) (interp a env)) args))
    ]
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
    [(lazy f e)
     (def (closureV arg body fenv) (strict (interp f env)))
     (interp body (extend-env arg (promiseV e env (box #f)) fenv))
    ]
    [(delay e) (promiseV e env '())]
    [(force e) 
     (let ([prom (interp e env)])
       (if (promiseV? prom)
           (valV-v (interp (promiseV-expr prom) env))
           (error "Cannot force a non-promise")))
    ]
    
    [(valV v) v]
    [(listV lst) (listV lst)]
    [(closureV arg body fenv) (closureV arg body fenv)]
    [(list-expr elems)
     (listV (map (λ(a)
                   (let ([val (strict (interp a env))])
                     (if (valV? val)
                         (valV-v val)
                         val
                      )
                    )
                  )         
             elems))
    ]
    [(seqn exprs) 
     (if (empty? exprs)
         (error "seqn requires at least one expression")
         (for/fold ([result (valV 0)])
                  ([expr exprs])
           (interp expr env)))
    ]
    )
  )

(define (strict val)
  (match val
    [(promiseV e env cache)
     (if (unbox cache) ; si el cache ya tiene un valor
         (begin
           (printf "Cache opened: ~a~n" (unbox cache))
           (unbox cache)) ; devuelve el valor almacenado
         (let ([evaluated (strict (interp e env))]) ; evalua la promesa
           (begin
             (set-box! cache evaluated) ; guarda el resultado en el cache
             (printf "Cache registry: storing ~a~n" evaluated)
             evaluated)))]
    [_ val]))

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
           (listV (map (λ (elem)
                         ; Interpretamos el cierre con cada elemento
                         (valV-v (interp (app f (valV elem)) (closureV-env f)))) ; interpretamos la expr resultante
                       ; de la aplicación de f con elem en el contexto del env del closure de ese f
                       (listV-elems lst)))
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
               (error "tail: empty list")
               (listV
                (if (= (length (listV-elems lst)) 1)
                    '() ; si tiene solo un elemento, devuelve lista vacía como listV
                    (cdr (listV-elems lst))))) ; si no, devuelve el resto de los elementos
           (error "tail: requires a listV"))) ; si no es una listV, error
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
           (error "Unknown STL operation")))
     ]
    )
  ) ; Si no es reconocida, error



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


; PROBLEM 1 TESTS
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
(test (run '{mod 8 2}) 0)

; Boolean primitives tests
(test (run '{and #t #t}) #t)
(test (run '{&& #t #t}) #t)
(test (run '{or #f #t}) #t)
(test (run '{|| #f #t}) #t)
(test (run '{not #t}) #f)
(test (run '{xor #t #f}) #t)
(test (run '{nand #t #t}) #f)
(test (run '{equiv #t #t}) #t)

; String standard-library tests
(test (run '{strApp "Hello" " World"}) "Hello World")
(test (run '{strAt "Hello" 0}) #\H)
(test (run '{str=? "hello" "hello"}) #t)
(test (run '{str-upper "hello"}) "HELLO")
(test (run '{str-lower "HELLO"}) "hello")
(test (run '{str-len "hello"}) 5)
(test (run '{str-substring "hello" 0 2}) "he")
(test (run '{str-reverse "hello"}) "olleh")

; When sentence (if) tests

(test (run '{when {== 5 5} {+ 1 1} {+ 2 2}}) 2)
(test (run '{when {== 5 5} {+ 1 1} {+ 2 2}}) 2)
(test (run '{when {!= 5 5} {+ 1 1} {+ 2 2}}) 4)
(test (run '{when {and #t #t} {+ 10 10 10} {- 20 5}}) 30)
(test (run '{when {> 10 5} {when {== 1 1} 42 0} -1}) 42)

; Seqn sentence tests

(test (run '{seqn {+ 1 2} {- 10 3} {+ 5 5}}) 10)
(test (run '{seqn {and #t #f} {or #f #t} {not #t}}) #f)
(test (run '{seqn {with {{x 10}} {+ x x}} {+ 2 3}}) 5)
(test (run '{seqn {fun {x} {+ x x}} {{fun {y} {* y y}} 3}}) 9)


; List sentence tests

(test (run '{list}) '())
(test (run '{list 1 2 3 4}) '(1 2 3 4))
(test (run '{list {+ 1 1} {- 5 2} {* 2 3}}) '(2 3 6))
(test (run '{list #t #f "hola" "mundo"}) '(#t #f "hola" "mundo"))
(test (run '{list {+ 1 1} {- 5 2} {* 2 3}}) '(2 3 6))


; Head (car) sentence tests

(test (run '{head {list 1 2 3}}) 1) ;
(test/exn (run '{head {list}}) "head: empty list") 
(test/exn (run '{head 42}) "head: requires a listV") 
(test (run '{head {list 5}}) 5)
(test (run '{head {list {list 1 2} 3 4}}) (listV '(1 2)))
(test (run '{head {list "hello" "world"}}) "hello")
(test (run '{head {list #t #f #t}}) #t)
(test (run '{empty {list}}) #t)

; Tail (cdr) sentence tests

(test (run '{tail {list 1 2 3}}) '(2 3))
(test (run '{tail {list 1}}) '())
(test (run '{tail {list "hola" "mundo"}}) '("mundo"))
(test/exn (run '{tail {list}}) "tail: empty list")

; Empty sentence tests

(test (run '{empty {list}}) #t)
(test (run '{empty {list 1 2 3}}) #f)
(test (run '{empty {tail {list 1}}}) #t)

; Map sentence tests

(test (run '{my-map {fun {x} {+ x 1}} {list 1 2 3 4}}) '(2 3 4 5))
(test (run '{my-map {fun {x} {+ x 1}} {list 0 -1 2 5}}) '(1 0 3 6))
(test (run '{my-map {fun {x} {* x 2}} {list 1 2 3 4}}) '(2 4 6 8))
(test (run '{my-map {fun {x} {not {== x 0}}} {list 0 1 2 0}}) '(#f #t #t #f))
(test (run '{my-map {fun {x} {+ x 10}} {list 10 20 30 40}}) '(20 30 40 50))
(test (run '{my-map {fun {x} {+ x 1}} {list}}) '())


; Reject Sentence tests

(test (run '{my-reject {fun {x} {> x 2}} {list 1 2 3 4}}) '(1 2))
(test (run '{my-reject {fun {x} {< x 0}} {list -1 0 1 2}}) '(0 1 2))
(test (run '{my-reject {fun {x} {not {== {mod x 2} 0}}} {list 1 2 3 4 5 6}}) '(2 4 6))
(test (run '{my-reject {fun {x} {not {== x 0}}} {list 0 1 0 2 3 0}}) '(0 0 0))
(test (run '{my-reject {fun {x} {not {str=? x "Test"}}} {list "Test" "Fail" "Test"}}) '("Test" "Test"))
; 

; PROBLEM 2 TESTS

(test (run '{with {{x 3} {y 4}} {+ x y}}) 7)
(test (run '{with {{x 3} {y 4} {z 8} {w 9}} {+ x y z w}}) 24)
(test (run '{with {{x {fun {a} {+ a 1}}} {y {fun {a} {+ a 1}}}} {+ {y 8} {x 6}}}) 16)

(test (run '{{fun {x y} {+ x y}} {10 20}}) 30)
(test (run '{{fun {a b c} {+ a {- b c}}} {10 9 8}}) 11)
(test (run '{with {{f {fun {x y} {* x y}}}} {f {10 5}}}) 50)
(test (run '{with {{add {fun {a b c} {+ a {+ b c}}}}} {+ {add {1 2 3}} {add {4 5 6}} {add {7 8 9}}}}) 45)


; PROBLEM 3 TESTS

(test (run '{delay {+ 1 2}}) (promiseV (prim '+ (list (num 1) (num 2))) initial-env '()))
(test (run '{force (delay {+ 1 2})}) 3)
(test (run '{delay {+ 5 10}}) (promiseV (prim '+ (list (num 5) (num 10))) initial-env '()))
(test (run '{force (delay {+ 5 10})}) 15)
(test (run '{force (delay {strApp "Hello" " World"})}) "Hello World")
(test (run '{delay {== 5 5}}) (promiseV (prim '== (list (num 5) (num 5))) initial-env '()))
(test (run '{force (delay {== 5 5})}) #t)

; PROBLEM 4 TESTS

(test (run '{
  rec-y {fact {fun {n}
               {when {== n 0}
                    1
                    {* n {fact {- n 1}}}}}}
  {fact 5}}
  ) 120)

(test (run '{
  rec-y {sum {fun {n}
              {when {== n 0}
                   0
                   {+ n {sum {- n 1}}}}}}
  {sum 10}}
  ) 55)

(test (run '{
  rec-y {fib {fun {n}
              {when {== n 0}
                   0
                   {when {== {- n 1} 0}
                        1
                        {+ {fib {- n 1}} {fib {- n 2}}}}}}}
  {fib 10}}
  ) 55) 

(test (run '{
  rec-y {power-base2 {fun {exp}
                      {when {== 0 exp}
                           1
                           {* 2 {power-base2 {- exp 1}}}}}}
  {power-base2 2}} ) 4)

; PROBLEM 5 TESTS

(test (run '{lazy {fun {x} {+ x x}} {- 15 5}}) 20)
(test (run '{with [{f {fun {x} {+ x x}}}] {lazy f 10}}) 20)
(test (run '{lazy {fun {a b} {+ a b}} {3 {+ 1 1}}}) 5)
(test (run '{lazy {fun {a b c} {+ a {- b c}}} {3 2 1}}) 4)
(test (run '{with [{f {fun {x y} {+ x y}}}] {lazy f {10 5}}})15)





