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
    (lambda (fecha nombre-documento contenido)
    (if (and (equal? (usuario-repetido? (get-dato paradigmadocs 4) usuario) #t ) (equal? (esta-contraseña? (get-dato paradigmadocs 4) contraseña) #t ))
        (if (procedure? operación)
            (if (equal? operación create)
                ((create (agregar-y-remover usuario paradigmadocs)) fecha nombre-documento contenido)
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
  )
; Descripción: Función que permite a un usuario con sesión iniciada en la plataforma definida crear un documento de texto de modo que en este...
;              ... se registre al autor, fecha de creación, nombre del documento y su contenido retornando una versión actualizada de acuerdo...
;              ... a los parámetros ingresados en paralelo con la eliminación de la sesión activa del usuario/creador correspondiente
; Dominio: paradigmadocs X (date) X nombre-documento X contenido
; Recorrido: Actualización de paradigmadocs

(define create
  (lambda (paradigmadocs)
    (lambda (fecha nombre-documento contenido)
      (if (buscar-usuario-activo (get-dato paradigmadocs 4))
          (agregar-y-remover (buscar-usuario-activo (get-dato paradigmadocs 4))
                             (list (get-dato paradigmadocs 0)
                                   (get-dato paradigmadocs 1)
                                   (get-dato paradigmadocs 2)
                                   (get-dato paradigmadocs 3)
                                   (get-dato paradigmadocs 4)
                                   (set-lista (get-dato paradigmadocs 5)
                                              (crear-documento
                                               (get-dia fecha) (get-mes fecha) (get-año fecha) nombre-documento (buscar-usuario-activo (get-dato paradigmadocs 4)) (ID-documento (get-dato paradigmadocs 5) 0) ((get-dato paradigmadocs 2) contenido) 0)
                                              )
                                   )
                             )
          #f)
      )
    )
  )













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
; ...previamente definida la plataforma a emplear: "(define emptyGDocs (paradigmadocs "gDocs" 16 10 2021 encryptFn encryptFn))"
(define emptyGDocs1 (register emptyGDocs 19 10 2021 "Angel" "contraseña"))
(define emptyGDocs2 (register emptyGDocs1 20 10 2021 "Jaime" "pinturaceresita"))
(define emptyGDocs-novalido (register emptyGDocs2 19 10 2021 "Angel" "USUARIO REPETIDO"))
PENDIENTE
; El tercer ejemplo expresa una situación no valida (nombre de usuario repetido) por lo que no se agrerga el usuario

EJEMPLOS login:
(define emptyGDocs3 ((login emptyGDocs2 "Jaime" "pinturaceresita" create) (30 08 2021) "doc0" "HOLA"))






> (define paradigmadocs4 (login paradigmadocs3 "Jaime" "pinturaceresita" "a"))
> (define paradigmadocs5 (agregar-y-remover "Jaime" paradigmadocs3))
> (define paradigmadocs6 (create paradigmadocs5 26 10 2021 "Doc" "AAAA"))
> 

---
> (define a '("gDocs" (16 10 2021) "encryptFn" "encryptFn" (((19 10 2021) "Angel" "contraseña" 0) ((20 10 2021) "Jaime" "pinturaceresita" 1)) ()))
> (get-dato-doc (get-dato a 5) 5)
|#