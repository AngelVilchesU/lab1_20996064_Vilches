#lang racket

(require "TDA_paradigmadocs.rkt")

; Implementación del menú principal

; Representación:
; (string X string X string X date)

; Constructor:
; Descripción: Permite el registro de un usuario a la plataforma creada mediante la solicitud de el nombre de...
;              ... el nombre de la plataforma, nombre de usuario, contraseña y fecha.
; Dominio: paradigmadocs X string X string X date
; Recorrido: Actualización de paradigmadocs
; Tipo de recursión: Recursión natural la cual contempla en el caso base la nula existencia de parametros para...
;                    ... insertar lo ingresado, caso contrario se llama a si misma dejando estados pendientes...
;                    ... considerando el "resto" de la lista cada vez. No sin antes obtenener la lista ubicada...
;                    ... en la sexta posición de paradigmadocs donde se ubica la información de usuarios

(define register
  (lambda (paradigmadocs usuario contraseña dia mes año)
    (if (null? (get-dato paradigmadocs 6))
        (list usuario contraseña dia mes año)
        (list (car paradigmadocs) (register (cdr paradigmadocs) register))
        )))