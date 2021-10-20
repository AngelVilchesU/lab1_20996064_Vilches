#lang racket

(require "TDA_paradigmadocs.rkt")
(require "TDA_fecha.rkt")
(require "TDA_usuarios.rkt")

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
    (if (equal? (usuario-repetido? (get-dato paradigmadocs 4) usuario) #f)    
       (modificar-lista-usuarios paradigmadocs (crear-usuario dia mes año usuario contraseña))
       paradigmadocs)))
       


#|
EJEMPLOS register:
(register (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") 17 10 2021 "Angel" "contraseña") ; Las funciones "encryptFn" deben ser llamadas sin comillas (procedimiento) una vez realizada la función enriptadora
(register (register (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") 17 10 2021 "Angel" "contraseña") 19 11 2021 "Milky" "guau")
(register (register (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") 17 10 2021 "Angel" "contraseña") 19 11 2021 "Angel" "cerealTrix") ; Este ejemplo expresa una situación no valida pues el nombre de usuario ya existe


|#