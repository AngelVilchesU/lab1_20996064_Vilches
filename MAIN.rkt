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
  (lambda (paradigmadocs fecha usuario contraseña)
    (if (equal? (esta-usuario? (get-dato paradigmadocs 4) usuario) #f)   
        (set-act-list-usuarios-paradigmadocs paradigmadocs
                                             (crear-usuario
                                              fecha
                                              usuario
                                              contraseña))
        paradigmadocs)
    )
  )
       
; Descripción: Función que permite autenticar un usuario y de ello la ejecución de comandos definido en la plataforma...
;              ... dependiendo de la correcta validación del usuario
; Dominio: paradigmadocs X string X string X function
; Recorrido: función (parcial) y paradigmadocs actualizado (final)

(define login
  (lambda (paradigmadocs usuario contraseña operacion)
    (if (and (equal? (esta-usuario? (get-dato paradigmadocs 4) usuario) #t ) (equal? (esta-contraseña? (get-dato paradigmadocs 4) contraseña) #t ))
        (if (procedure? operacion)
            (if (equal? operacion create)
                (create (agregar-y-remover usuario paradigmadocs))
                (if (equal? operacion share)
                    (share (agregar-y-remover usuario paradigmadocs))
                    (if (equal? operacion add)
                        (add (agregar-y-remover usuario paradigmadocs))
                        (if (equal? operacion restoreVersion)
                            (restoreVersion (agregar-y-remover usuario paradigmadocs))
                            (if (equal? operacion revokeAllAccesses)
                                (revokeAllAccesses (agregar-y-remover usuario paradigmadocs))
                                (if (equal? operacion search)
                                    (search (agregar-y-remover usuario paradigmadocs))
                                    (if (equal? operacion paradigmadocs->string)
                                        "paradigmadocs->string (entradas)" ;(agregar-y-remover (get-dato paradigmadocs 4) usuario paradigmadocs)
                                        #f)
                                    )
                                )
                            )
                        )
                    )
                )
            paradigmadocs)
        paradigmadocs)      
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
                                   (anexar-listas (get-dato paradigmadocs 5)
                                                  (crear-documento
                                                   fecha
                                                   nombre-documento
                                                   (buscar-usuario-activo (get-dato paradigmadocs 4))
                                                   (ID-version-doc (get-dato paradigmadocs 5) -1)
                                                   ((get-dato paradigmadocs 2) contenido)
                                                   0))))
          create)
      )
    )
  )

; Descripción: Función que permite a un usuario con sesión iniciada en la plataforma definida compartir un documento con determinado acceso o permiso...
;              ... (leer, escribir o comentar) retornando una versión actualizada de acuerdo a los parámetros ingresados en paralelo con la eliminación...
;              ... de la sesión activa del usuario/creador correspondiente
; Dominio: paradigmadocs X idDocs X lista-usuario-acceso
; Recorrido: Actualización de paradigmadocs

(define share
  (lambda (paradigmadocs)
    (lambda (idDocs access . usuario-acceso)
      (if (buscar-usuario-activo (get-dato paradigmadocs 4))
          (if (buscar-Id-documento (get-dato paradigmadocs 5) idDocs)
              (if (equal? (buscar-usuario-activo (get-dato paradigmadocs 4)) (car (cdr (buscar-Id-documento (get-dato paradigmadocs 5) idDocs))))
                  (agregar-y-remover (buscar-usuario-activo (get-dato paradigmadocs 4))
                                     (agregar-y-remover-compartido idDocs (apply list access usuario-acceso) paradigmadocs))
                  #f)
              #f)
          share)
      )
    )
  )

; Descripción: Función que permite a un usuario con sesión iniciada en la plataforma definida añadir texto al final de un documento con determinado...
;              ... veríficando los permisos de edición correspondientes retornando una versión actualizada de acuerdo a lo ingresado en paralelo con la eliminación...
;              ... de la sesión activa del usuario/creador correspondiente
; Dominio: paradigmadocs X idDocs X fecha X contenido
; Recorrido: Actualización de paradigmadocs

(define add
  (lambda (paradigmadocs)
    (lambda (idDocs fecha contenido)
      (if (buscar-usuario-activo (get-dato paradigmadocs 4))
          (if (buscar-Id-documento (get-dato paradigmadocs 5) idDocs)
              (if (or (equal? (buscar-usuario-activo (get-dato paradigmadocs 4)) (car (cdr (buscar-Id-documento (get-dato paradigmadocs 5) idDocs))))
                      (buscar-usuario-editor (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) idDocs) 5) (buscar-usuario-activo (get-dato paradigmadocs 4))))
                  (agregar-y-remover (buscar-usuario-activo (get-dato paradigmadocs 4))
                                     (agregar-remover-doc idDocs fecha contenido paradigmadocs))
                  (agregar-y-remover (buscar-usuario-activo (get-dato paradigmadocs 4)) paradigmadocs)) ;retorna la plataforma previo a la operación
              (agregar-y-remover (buscar-usuario-activo (get-dato paradigmadocs 4)) paradigmadocs)) ;retorna la plataforma previo a la operación
          add)
      )
    )
  )




(define restoreVersion
  (lambda (paradigmadocs)
    (lambda (idDoc idVersion)
      ; condicional: si existe un usuario activo
      (if (buscar-usuario-activo (get-dato paradigmadocs 4))
          ; condicional: si el documento existe
          (if (buscar-Id-documento (get-dato paradigmadocs 5) idDoc)
              ; condicional: si el usuario ingresado es el propietario del documento
              (if (equal? (buscar-usuario-activo (get-dato paradigmadocs 4)) (car (cdr (buscar-Id-documento (get-dato paradigmadocs 5) idDoc))))
                  ; condicional: si la versión del documento existe
                  (if (n-version (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) idDoc) 4) idVersion)
                      (agregar-y-remover (buscar-usuario-activo (get-dato paradigmadocs 4))
                                         (agregar-remover-version idDoc idVersion paradigmadocs))
                      (agregar-y-remover (buscar-usuario-activo (get-dato paradigmadocs 4)) paradigmadocs))
                  (agregar-y-remover (buscar-usuario-activo (get-dato paradigmadocs 4)) paradigmadocs))
              (agregar-y-remover (buscar-usuario-activo (get-dato paradigmadocs 4)) paradigmadocs))
          restoreVersion)
      )
    )
  )


(define revokeAllAccesses
  (lambda (paradigmadocs)
    ; condicional: si existe un usuario activo
    (if (buscar-usuario-activo (get-dato paradigmadocs 4))
        (agregar-y-remover (buscar-usuario-activo
                            (get-dato paradigmadocs 4))
                            (set-act-list-doc-paradigmadocs paradigmadocs
                                                            (concatenador (get-dato paradigmadocs 5)
                                                                    (map set-list-compartido
                                                                         (filtrador-doc (get-dato paradigmadocs 5)
                                                                                        (buscar-usuario-activo (get-dato paradigmadocs 4)))))))
    
        revokeAllAccesses)
    )
  )


(define search
  (lambda (paradigmadocs)
    (lambda (frase)
      ; condicional: si existe un usuario activo
      (if (buscar-usuario-activo (get-dato paradigmadocs 4))
          (filter (llamado-valido? (buscar-usuario-activo (get-dato paradigmadocs 4))) (filter (verificador-de-substring frase) (get-dato paradigmadocs 5)))
          null)
      )
    )
  )


(define paradigmadocs->string
  "a")








#|
; EJEMPLOS register:
; ...previamente definida la plataforma a emplear: "(define emptyGDocs (paradigmadocs "gDocs" (crear-fecha 16 10 2021) encryptFn encryptFn))"
(define emptyGDocs1 (register emptyGDocs (crear-fecha 19 10 2021) "Angel" "contraseña"))
(define emptyGDocs2 (register emptyGDocs1 (crear-fecha 20 10 2021) "Jaime" "pinturaceresita"))
(define emptyGDocs-novalido (register emptyGDocs2 (crear-fecha 19 10 2021) "Angel" "USUARIO REPETIDO"))
; El tercer ejemplo expresa una situación no valida (nombre de usuario repetido) por lo que no se agrerga el usuario

; EJEMPLO SCRIPT ----------------------------------------------------------------------------------------------------------------------------------
(define gDocs1 (register (register (register emptyGDocs (crear-fecha 25 10 2021) "user1" "pass1") (crear-fecha 25 10 2021) "user2"
"pass2") (crear-fecha 25 10 2021) "user3" "pass3"))
; -------------------------------------------------------------------------------------------------------------------------------------------------

; EJEMPLOS login-create:
(define emptyGDocs3 ((login emptyGDocs2 "Jaime" "pinturaceresita" create) (crear-fecha 30 08 2021) "doc0" "HOLA"))
(define emptyGDocs4 ((login emptyGDocs3 "Angel" "contraseña" create) (crear-fecha 29 10 2021) "doc1" "ADIOS"))
(define emptyGDocs-novalido ((login emptyGDocs2 "Jaime" "claveinvalida" create) (crear-fecha 30 08 2021) "doc2" "Un texto que no es"))
; El tercer ejemplo expresa una situación no valida (contraseña no valida)

; EJEMPLOS SCRIPT ---------------------------------------------------------------------------------------------------------------------------------
(define gDocs2 ((login gDocs1 "user1" "pass1" create) (crear-fecha 30 08 2021) "doc0" "contenido doc0"))
(define gDocs3 ((login gDocs2 "user1" "pass1" create) (crear-fecha 30 08 2021) "doc1" "contenido doc1"))
(define gDocs4 ((login gDocs3 "user2" "pass2" create) (crear-fecha 30 08 2021) "doc2" "contenido doc2"))
(define gDocs5 ((login gDocs4 "user3" "pass3" create) (crear-fecha 30 08 2021) "doc3" "contenido doc3"))
; -------------------------------------------------------------------------------------------------------------------------------------------------

; EJEMPLOS login-share:

(define emptyGDocs5 ((login emptyGDocs4 "Jaime" "pinturaceresita" share) 0 "Angel" "R"))

; EJEMPLOS SCRIPT ---------------------------------------------------------------------------------------------------------------------------------
(define gDocs5 ((login gDocs4 "user1" "pass1" share) 1 (access "user2" #\r)))
(define gDocs6 ((login gDocs5 "user2" "pass2" share) 0 (access "user1" #\r) (access "user2" #\w)))
(define gDocs7 ((login gDocs6 "user3" "pass3" share) 0 (access "user1" #\c)))
; -------------------------------------------------------------------------------------------------------------------------------------------------



|#




(define emptyGDocs (paradigmadocs "gDocs" (crear-fecha 16 10 2021) encryptFn encryptFn))

(define gDocs1 (register (register (register emptyGDocs (crear-fecha 25 10 2021) "user1" "pass1") (crear-fecha 25 10 2021) "user2"
"pass2") (crear-fecha 25 10 2021) "user3" "pass3"))

(define gDocs2 ((login gDocs1 "user1" "pass1" create) (crear-fecha 30 08 2021) "doc0" "contenido doc0"))
(define gDocs3 ((login gDocs2 "user1" "pass1" create) (crear-fecha 30 08 2021) "doc1" "contenido doc1"))
(define gDocs4 ((login gDocs3 "user2" "pass2" create) (crear-fecha 30 08 2021) "doc2" "contenido doc2"))
(define gDocs5 ((login gDocs4 "user3" "pass3" create) (crear-fecha 30 08 2021) "doc3" "contenido doc3"))

(define gDocs6 ((login gDocs5 "user1" "pass1" share) 1 (access "user2" #\r)))
(define gDocs7 ((login gDocs6 "user3" "pass3" share) 3 (access "user1" #\c)))
(define gDocs8 ((login gDocs7 "user2" "pass2" share) 2 (access "user1" #\r) (access "user3" #\w)))

(define gDocs9 ((login gDocs8 "user1" "pass1" add) 0 (crear-fecha 30 11 2021) "mas contenido en doc0"))
(define gDocs10 ((login gDocs9 "user3" "pass3" add) 3 (crear-fecha 30 11 2021) "mas contenido en doc3"))
(define gDocs11 ((login gDocs10 "user3" "pass3" add) 0 (crear-fecha 30 11 2021) "mas contenido en doc3"));situación no valida (permisos)
(define gDocs12 ((login gDocs11 "user3" "pass3" add) 3 (crear-fecha 30 11 2021) "avena con agua"))

(define gDocs13 ((login gDocs12 "user3" "pass3" restoreVersion) 3 0))
(define gDocs14 ((login gDocs13 "user1" "pass1" restoreVersion) 0 0))
(define gDocs15 ((login gDocs14 "user3" "pass3" restoreVersion) 3 1))

(define gDocs16 (login gDocs15 "user2" "pass2" revokeAllAccesses))

((login gDocs16 "user1" "pass1" search) "contenido")



