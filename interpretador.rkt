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
;;
;;
;;
;;
;;
;;