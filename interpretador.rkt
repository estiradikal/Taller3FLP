#lang eopl

;; Estiven Andres Martinez Granados
;; 2179687-3743
;; Taller 3 FLP

;Especificación Léxica

(define interpreter
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



;;
;;
;;
;;
;;
;;
;;
;;