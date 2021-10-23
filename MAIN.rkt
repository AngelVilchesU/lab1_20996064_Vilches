#lang racket

(require "TDA_paradigmadocs.rkt")
(require "TDA_fecha.rkt")
(require "TDA_usuarios.rkt")

; Implementación de los requisitos funcionales

; Descripción: Función que permite el registro de un usuario a la plataforma creada mediante la solicitud de el nombre de...
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
       
; Descripción: Función que permite autenticar un usuario y de ello la ejecución de comandos definido en la plataforma...
;              ... dependiendo de la correcta validación del usuario
; Dominio: paradigmadocs X string X string X function
; Recorrido: función (parcial) y paradigmadocs actualizado (final)

(define login
  (lambda (paradigmadocs usuario contraseña operación)
    (if (and (equal? (usuario-repetido? (get-dato paradigmadocs 4) usuario) #t ) (equal? (contraseña-repetida? (get-dato paradigmadocs 4) contraseña) #t ))
        (set-sesion-act (get-dato paradigmadocs 4) usuario paradigmadocs)
        
        
        #f)

    ))



    #|
        (if (procedure? operación)

            (if (equal? ))










            
            #f)

       ))
|#









#|
EJEMPLOS register:
(register (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") 17 10 2021 "Angel" "contraseña") ; Las funciones "encryptFn" deben ser llamadas sin comillas (procedimiento) una vez realizada la función enriptadora
(register (register (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") 17 10 2021 "Angel" "contraseña") 19 11 2021 "Milky" "guau")
(register (register (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") 17 10 2021 "Angel" "contraseña") 19 11 2021 "Angel" "cerealTrix") ; Este ejemplo expresa una situación no valida pues el nombre de usuario ya existe




> (define paradigmadocs (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn"))
> (define usuario1 (crear-usuario 19 10 2021 "Angel" "contraseña"))
> (define paradigmadocs2 (modificar-lista-usuarios paradigmadocs usuario1))
> paradigmadocs2
'("gDocs" (16 10 2021) "encryptFn" "encryptFn" (((19 10 2021) "Angel" "contraseña" 0)) ())
> (login paradigmadocs2 "Angel" "contraseña" "a")
(get-dato paradigmadocs2 4)



|#