#lang racket

(require "TDA_paradigmadocs.rkt")
(require "TDA_fecha.rkt")
(require "TDA_usuarios.rkt")
(require "TDA_documentos.rkt")

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
        (modificar-lista-usuarios paradigmadocs (crear-usuario dia mes año usuario contraseña) )
        paradigmadocs)))
       
; Descripción: Función que permite autenticar un usuario y de ello la ejecución de comandos definido en la plataforma...
;              ... dependiendo de la correcta validación del usuario
; Dominio: paradigmadocs X string X string X function
; Recorrido: función (parcial) y paradigmadocs actualizado (final)

(define login
  (lambda (paradigmadocs usuario contraseña operación)
    (if (and (equal? (usuario-repetido? (get-dato paradigmadocs 4) usuario) #t ) (equal? (contraseña-repetida? (get-dato paradigmadocs 4) contraseña) #t ))
        (if (procedure? operación)
            (if (equal? operación create)
                "create (entradas)" ;(agregar-y-remover (get-dato paradigmadocs 4) usuario paradigmadocs)|||||||||||||||||||||||||||||||||||||
                (if (equal? operación share)
                    "share (entradas)" ;(agregar-y-remover (get-dato paradigmadocs 4) usuario paradigmadocs)
                    (if (equal? operación add)
                        "add (entradas)" ;(agregar-y-remover (get-dato paradigmadocs 4) usuario paradigmadocs)
                        (if (equal? operación restoreVersion)
                            "restoreVersion (entradas)" ;(agregar-y-remover (get-dato paradigmadocs 4) usuario paradigmadocs)
                            (if (equal? operación revokeAllAccesses)
                                "revokeAllAccesses (entradas)" ;(agregar-y-remover (get-dato paradigmadocs 4) usuario paradigmadocs)
                                (if (equal? operación search)
                                    "search (entradas)" ;(agregar-y-remover (get-dato paradigmadocs 4) usuario paradigmadocs)
                                    (if (equal? operación paradigmadocs->string)
                                        "paradigmadocs->string (entradas)" ;(agregar-y-remover (get-dato paradigmadocs 4) usuario paradigmadocs)
                                        #f)
                                    )
                                )
                            )
                        )
                    )
                )
            #f)
        #f)
    )
  )

(define create
  (lambda (paradigmadocs dia mes año nombre-documento contenido)
    (list (get-dato paradigmadocs 0)
          (get-dato paradigmadocs 1)
          (get-dato paradigmadocs 2)
          (get-dato paradigmadocs 3)
          (get-dato paradigmadocs 4)
          (set-lista (get-dato paradigmadocs 5)
                     (crear-documento
                      dia mes año nombre-documento (buscar-usuario-activo (get-dato paradigmadocs 4)) (ID-documento (get-dato paradigmadocs 5) 0) contenido 0)
                                              ))))











(define share
  "a")
(define add
  "a")
(define restoreVersion
  "a")
(define revokeAllAccesses
  "a")
(define search
  "a")
(define paradigmadocs->string
  "a")








#|
EJEMPLOS register:
(register (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") 17 10 2021 "Angel" "contraseña") ; Las funciones "encryptFn" deben ser llamadas sin comillas (procedimiento) una vez realizada la función enriptadora
(register (register (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") 17 10 2021 "Angel" "contraseña") 19 11 2021 "Milky" "guau")
(register (register (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") 17 10 2021 "Angel" "contraseña") 19 11 2021 "Angel" "cerealTrix") ; Este ejemplo expresa una situación no valida pues el nombre de usuario ya existe


> (define paradigmadocs (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn"))
> (define paradigmadocs2 (register paradigmadocs 19 10 2021 "Angel" "contraseña"))
> (define paradigmadocs3 (register paradigmadocs2 20 10 2021 "Jaime" "pinturaceresita"))
> (define paradigmadocs4 (login paradigmadocs3 "Jaime" "pinturaceresita" "a"))
> (define paradigmadocs5 (agregar-y-remover (get-dato paradigmadocs3 4) "Jaime" paradigmadocs3))
> (define paradigmadocs6 (create paradigmadocs5 26 10 2021 "Doc" "AAAA"))
> 

---
> (define a '("gDocs" (16 10 2021) "encryptFn" "encryptFn" (((19 10 2021) "Angel" "contraseña" 0) ((20 10 2021) "Jaime" "pinturaceresita" 1)) ()))
> (get-dato-doc (get-dato a 5) 5)
|#