#lang racket

(require "TDA_paradigmadocs.rkt")
(require "TDA_fecha.rkt")

; Implementación de los requisitos funcionales

; Descripción: Permite el registro de un usuario a la plataforma creada mediante la solicitud de el nombre de...
;              ... el nombre de la plataforma, nombre de usuario, contraseña y fecha.
; Dominio: paradigmadocs X date X string X string
; Recorrido: Actualización de paradigmadocs
; Tipo de recursión: Recursión natural la cual contempla en el caso base la nula existencia de parametros para...
;                    ... insertar lo ingresado, caso contrario se llama a si misma dejando estados pendientes...
;                    ... considerando el "resto" de la lista cada vez. No sin antes obtenener la lista ubicada...
;                    ... en la sexta posición de paradigmadocs donde se ubica la información de usuarios

(define register
  (lambda (paradigmadocs dia mes año usuario contraseña)
    (if (fecha? (crear-fecha dia mes año))
        (if (null? (get-dato paradigmadocs 6))
        (list usuario contraseña (crear-fecha dia mes año))
        (list (car paradigmadocs) (register (cdr (get-dato paradigmadocs 6)) dia mes año usuario contraseña))
            )#f)))



#|
EJEMPLOS register:
(register '("gDocs" 16 10 2021 "encryptFn" "encryptFn" () ()) 17 10 2021 "Angel" "contraseña") ; Las funciones "encryptFn" deben ser llamadas sin comillas (procedimiento) una vez realizada la función enriptadora
(register '("gWord" 17 10 2021 "encryptFn" "encryptFn" () ()) 19 11 2021 "Milky" "guau")
(register '("gTXT" 18 10 2021 "encryptFn" "encryptFn" () ()) 27 13 2021 "Laysa" "A") ; Este ejemplo expresa una situación no valida pues la fecha posee inconsistencias graves





|#