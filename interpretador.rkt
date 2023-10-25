#lang eopl

;; Estiven Andres Martinez Granados
;; 2179687-3743
;; Taller 3 FLP

;;Especificación de Gramatica

(define (evaluar-programa programa)
  (evaluar-expresion programa))

(define (evaluar-expresion expresion)
  (cond
   [(numero-lit? expresion) (evaluar-numero-lit expresion)]
   [(texto-lit? expresion) (evaluar-texto-lit expresion)]
   [(var-exp? expresion) (evaluar-var-exp expresion)]
   [(primapp-bin-exp? expresion) (evaluar-primapp-bin-exp expresion)]
   [(primapp-un-exp? expresion) (evaluar-primapp-un-exp expresion)]
   [else #f]))


(define (numero-lit? expresion)
  (and (number? expresion) (not (integer? expresion))))

(define (evaluar-numero-lit expresion)
  expresion)

(define (texto-lit? expresion)
  (string? expresion))

(define (evaluar-texto-lit expresion)
  expresion)

(define (var-exp? expresion)
  (and (symbol? expresion)
       (char=? (string-ref (symbol->string expresion) 0) #\@)))

(define (evaluar-var-exp expresion)
  ; Implementa aquí la lógica para obtener el valor de la variable
  #f) ; Devuelve #f si la variable no está definida


(define (primapp-bin-exp? expresion)
  (and (list? expresion)
       (= (length expresion) 3)
       (symbol? (cadr expresion))
       (or (eq? '+ (cadr expresion))
           (eq? '- (cadr expresion))
           (eq? '/ (cadr expresion))
           (eq? '* (cadr expresion))
           (eq? 'concat (cadr expresion)))
       (evaluar-expresion (car expresion))
       (evaluar-expresion (caddr expresion))))

(define (evaluar-primapp-bin-exp expresion)
  (let ((operador (cadr expresion))
        (exp1 (car expresion))
        (exp2 (caddr expresion)))
    (cond
     [(eq? '+ operador) (+ (evaluar-expresion exp1) (evaluar-expresion exp2))]
     [(eq? '- operador) (- (evaluar-expresion exp1) (evaluar-expresion exp2))]
     [(eq? '/ operador) (/ (evaluar-expresion exp1) (evaluar-expresion exp2))]
     [(eq? '* operador) (* (evaluar-expresion exp1) (evaluar-expresion exp2))]
     [(eq? 'concat operador) (string-append (evaluar-expresion exp1) (evaluar-expresion exp2))])))

(define (primapp-un-exp? expresion)
  (and (list? expresion)
       (eq? (length expresion) 2)
       (symbol? (car expresion))
       (or (eq? 'longitud (car expresion))
           (eq? 'add1 (car expresion))
           (eq? 'sub1 (car expresion))
           (eq? 'evaluar-expresion (car expresion))) 
       (evaluar-expresion (cadr expresion))))


(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

(define (evaluar-primapp-un-exp expresion)
  (let ((operador (car expresion))
        (exp (cadr expresion)))
    (cond
     [(eq? 'longitud operador) (string-length (evaluar-expresion exp))]
     [(eq? 'add1 operador) (add1 (evaluar-expresion exp))]
     [(eq? 'sub1 operador) (sub1 (evaluar-expresion exp))])))

; Ejemplo de uso
(evaluar-programa '(+ 2 3)) ; Imprime 5
(evaluar-programa '("Hello" " " "World")) ; Imprime "Hello World"
(evaluar-programa '@x) ; Imprime el valor de la variable @x


;Especificación Léxica

(define interpretador
  '(
    ;; Define el token 'white-sp' para espacios en blanco y tabuladores.
    (white-sp
     (whitespace) skip)
     
    ;; Define el token 'comment' para comentarios que comienzan con '%'.
    (comment
     ("%" (arbno (not #\newline))) skip)
     
    ;; Define el token 'identifier' para identificadores.
    (identifier
     ;; Un identificador comienza con una letra y puede contener letras, dígitos o '?'.
     (letter (arbno (or letter digit "?"))) symbol)
     
    ;; Define el token 'number' para números.
    (number
     ;; Un número debe comenzar con un dígito y puede contener dígitos adicionales.
     (digit (arbno digit)) number)
     
    ;; Define el token 'number' para números negativos.
    (number
     ;; Un número negativo comienza con '-' seguido de un dígito y dígitos adicionales.
     ("-" digit (arbno digit)) number)
  )
)

;; ----------------------------------------------- 2 -----------------------------------------------------

(define initial-environment
  '(
    (@a 1)
    (@b 2)
    (@c 3)
    (@d "hola")
    (@e "FLP")
   ))

(define (buscar-variable variable ambiente)
  (cond
    [(null? ambiente) "Error, la variable no existe"]
    [(eq? variable (caar ambiente)) (cdar ambiente)]
    [else (buscar-variable variable (cdr ambiente))]
    ))

(define (evaluar-expresionn expresion ambiente)
  (cond
    [(numero-lit? expresion) expresion]
    [(texto-lit? expresion) expresion]
    [(var-exp? expresion) (buscar-variable expresion ambiente)]
    [(primapp-bin-exp? expresion) (evaluar-primapp-bin-exp expresion ambiente)]
    [(primapp-un-exp? expresion) (evaluar-primapp-un-exp expresion ambiente)]
    [else "Expresión no válida"]
    ))



(evaluar-expresionn '@a initial-environment) ; Salida esperada: 1
(evaluar-expresionn '@b initial-environment) ; Salida esperada: 2
(evaluar-expresionn '@e initial-environment) ; Salida esperada: "FLP"

(evaluar-expresionn '@f initial-environment) ; Salida esperada: "Error, la variable no existe"

;; --------------------------------------------------- 3 -----------------------------------

(define (valor-verdad? expresion)
  (not (= expresion 0)))

(valor-verdad? 0) ; Salida esperada: #f
(valor-verdad? 5) ; Salida esperada: #t
(valor-verdad? -3) ; Salida esperada: #t

;; --------------------------------------------------- 4 -----------------------------------

(define (eval-exp exp)
  (cond
    [(number? exp) exp]
    [(list? exp)
     (case (car exp)
       [(+) (+ (eval-exp (cadr exp)) (eval-exp (caddr exp)))]
       [(-) (- (eval-exp (cadr exp)) (eval-exp (caddr exp)))]
       [(*) (* (eval-exp (cadr exp)) (eval-exp (caddr exp)))]
       [(/) (/ (eval-exp (cadr exp)) (eval-exp (caddr exp)))]
       [(~) (string-length (eval-exp (cadr exp)))]
       [(condicional-exp)
        (if (eval-exp (cadr exp))
            (eval-exp (caddr exp))
            (eval-exp (cadddr exp)))]
       [else "Expresión no válida"])] ; Devuelve la cadena directamente
    [else "Expresión no válida"])) ; Devuelve la cadena directamente



(define example1 '(condicional-exp (+ 2 3) 2 3))
(define example2 '(condicional-exp (string=? (number->string (eval-exp '(+ 2 3))) "4") 2 3))

(define result1 (eval-exp example1))
(define result2 (eval-exp example2))

result1
result2

;; --------------------------------------------------- 5 -----------------------------------

(define (evaal-exp exp)
  (cond
    [(number? exp) exp]
    [(symbol? exp) (lookup-variable-value exp)]
    [(list? exp)
     (case (car exp)
       [(+) (+ (eval-exp (cadr exp)) (eval-exp (caddr exp)))]
       [(-) (- (eval-exp (cadr exp)) (eval-exp (caddr exp)))]
       [(*) (* (eval-exp (cadr exp)) (eval-exp (caddr exp)))]
       [(/) (/ (eval-exp (cadr exp)) (eval-exp (caddr exp)))]
       [(~) (string-length (eval-exp (cadr exp)))]
       [(variableLocal-exp)
        (let ((bindings (cadr exp))
              (body (caddr exp)))
          (eval-body body bindings))]
       [else ("Expresión no válida")])]
    [else ("Expresión no válida")]))


(define variable-table '())

(define (lookup-variable-value id)
  (cdr (assoc id variable-table)))

(define (eval-body body bindings)
  (let ((old-table variable-table))
    (set! variable-table (extend-variable-table bindings))
    (let ((result (eval-exp body)))
      (set! variable-table old-table)
      result)))

(define (extend-variable-table bindings)
  (append bindings variable-table))


(define example '(variableLocal-exp ((@x 2) (@y 3) (@a 7)) ((+ (@a) (@b)))))

(define result (eval-exp example))

result

;; --------------------------------------------------- 6 -----------------------------------

(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   ;;(exp eexpresion)
   ;;(amb ambiente?)
   ))

(define (evaaal-exp exp)
  (cond
    [(number? exp) exp]
    [(symbol? exp) (lookup-variable-value exp)]
    [(list? exp)
     (case (car exp)
       [(+) (+ (eval-exp (cadr exp)) (eval-exp (caddr exp)))]
       [(-) (- (eval-exp (cadr exp)) (eval-exp (caddr exp)))]
       [(*) (* (eval-exp (cadr exp)) (eval-exp (caddr exp)))]
       [(/) (/ (eval-exp (cadr exp)) (eval-exp (caddr exp)))]
       [(~) (string-length (eval-exp (cadr exp)))]
       [(procedimiento-ex)
        (make-cerradura (cadr exp) (caddr exp) variablee-table)]
       [else ("Expresión no válida")])]
    [else ("Expresión no válida")]))


(define variablee-table '())

(define (lookup-variablee-value id)
  (cdr (assoc id variablee-table)))

(define (evaal-body body)
  (let ((old-table variable-table))
    (set! variable-table (extend-variable-table body))
    (let ((result (eval-exp (cadr body))))
      (set! variable-table old-table)
      result)))

(require racket/list)

;;(define (extend-variablee-table ids)
  ;;(for/list ([id ids])
    ;;(cons id (lookup-variable-value id)))


(define examplee '(procedimiento-ex (@x @y @z) haga ((+ (+ (@x) (@y)) (@z))) ))

(define reesult (eval-exp example))

result

;; -------------------------------------------   --------------------------------

(define (areaCirculo radio)
  (* 3.14159 radio radio))

(define @radio 2.5)
(define @areaCirculo (lambda (radio) (areaCirculo radio)))

(@areaCirculo @radio)

;;----

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

"Factorial de 5: "
(factorial 5)


"Factorial de 10: "
(factorial 10)


;; ----------------

(define (restar a b)
  (if (= b 0)
      a
      (restar (sub1 a) (sub1 b))))

(define (multiplicar a b)
  (if (= b 0)
      0
      (restar (multiplicar a (sub1 b)) a)))

"Resta de 10 y 3: "
(restar 10 3)

"Multiplicación de 10 y 3: "
(multiplicar 10 3)

;;---

(define (@integrantes)
  "Robinson-y-Sara")

(define (@saludar mensaje funcion)
  (lambda ()
    (string-append "Hola:" (funcion) mensaje)))

(define (@decorate mensajeAdicional)
  (let ((mensajeDecorador (string-append "-ProfesoresFLP" mensajeAdicional)))
    (@saludar mensajeDecorador @integrantes)))

(@decorate "-OtroMensajeAdicional")

