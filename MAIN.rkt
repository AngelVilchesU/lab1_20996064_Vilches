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
                                        (paradigmadocs->string (agregar-y-remover usuario paradigmadocs))
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
          (filter (llamado-valido? (buscar-usuario-activo (get-dato paradigmadocs 4))) (filter (llamado-verificador-de-substring frase) (get-dato paradigmadocs 5)))
          null)
      )
    )
  )


(define paradigmadocs->string
  (lambda (paradigmadocs)
    ; condicional: si existe un usuario activo
    (if (buscar-usuario-activo (get-dato paradigmadocs 4))
        (string-append (usuario->string (get-dato paradigmadocs 4) (buscar-usuario-activo (get-dato paradigmadocs 4)))
                       (string-join (map (documento->string (buscar-usuario-activo (get-dato paradigmadocs 4)))
                                         (map (propietario-compartido (buscar-usuario-activo (get-dato paradigmadocs 4))) (get-dato paradigmadocs 5)))))
        (string-append "Nombre plataforma: " (get-dato paradigmadocs 0) "\n"
                       "Fecha creación: " (fecha->string (get-dato paradigmadocs 1)) "\n"
                       (string-join (usuarios->string (get-dato paradigmadocs 4)))
                       (string-join (documentos->string (get-dato paradigmadocs 5))))
    )
  ))
  
; CASO LOG
; IMPRIMIR USUARIO Y FECHA
; IMPRIMIR DOCUMENTO Y SUS COMPONENTES (EN ORDEN E "ITERATIVO")






; CREACIÓN DE LA PLATAFORMA PARADIGMADOCS

(define gDocs-0 (paradigmadocs "Paradigmadocs" (crear-fecha 16 10 2021) encryptFn encryptFn))

; EJEMPLOS DE LA FUNCIÓN REGISTER

(define gDocs-1 (register
                 (register
                  (register
                   gDocs-0
                   (crear-fecha 25 10 2021) "user1" "pass1")
                  (crear-fecha 25 10 2021) "user2" "pass2")
                 (crear-fecha 25 10 2021) "user3" "pass3"))
(define gDocs-2 (register
                 gDocs-1 (crear-fecha 26 10 2021) "user4" "pass4"))
(define gDocs-3 (register
                 gDocs-2 (crear-fecha 27 10 2021) "user5" "pass5"))

; EJEMPLOS DE LA FUNCIÓN LOGIN-CREATE

(define gDocs-4 ((login gDocs-3 "user1" "pass1" create)
                 (crear-fecha 28 10 2021) "Documento 0" "Primer contenido del Documento 0 "))
(define gDocs-5 ((login gDocs-4 "user1" "pass1" create)
                 (crear-fecha 28 10 2021) "Documento 1" "Primer contenido del Documento 1 "))
(define gDocs-6 ((login gDocs-5 "user2" "pass2" create)
                 (crear-fecha 29 10 2021) "Documento 2" "Primer contenido del Documento 2 "))
(define gDocs-7 ((login gDocs-6 "user3" "pass3" create)
                 (crear-fecha 29 10 2021) "Documento 3" "Primer contenido del Documento 3 "))
(define gDocs-8 ((login gDocs-7 "user5" "pass5" create)
                 (crear-fecha 30 10 2021) "Documento 4" "Primer contenido del Documento 4 "))

; EJEMPLOS DE LA FUNCIÓN LOGIN-SHARE

(define gDocs-9 ((login gDocs-8 "user1" "pass1" share)
                 1 (access "user4" #\r)))
(define gDocs-10 ((login gDocs-9 "user2" "pass2" share)
                  2 (access "user5" #\c)))
(define gDocs-11 ((login gDocs-10 "user3" "pass3" share)
                  3 (access "user1" #\w) (access "user2" #\w) (access "user4" #\r)))
(define gDocs-12 ((login gDocs-11 "user5" "pass5" share)
                  4 (access "user4" #\w) (access "user1" #\w) (access "user2" #\r) (access "user3" #\c)))

; EJEMPLOS DE LA FUNCIÓN LOGIN-ADD

(define gDocs-13 ((login gDocs-12 "user1" "pass1" add)
                  0 (crear-fecha 01 11 2021) "Segundo contenido del Documento 0 "))
(define gDocs-14 ((login gDocs-13 "user2" "pass2" add)
                  2 (crear-fecha 01 11 2021) "Segundo contenido del Documento 2 "))
(define gDocs-15 ((login gDocs-14 "user3" "pass3" add)
                  3 (crear-fecha 01 11 2021) "Segundo contenido del Documento 3 "))
(define gDocs-16 ((login gDocs-15 "user1" "pass1" add)
                  0 (crear-fecha 02 11 2021) "Tercer contenido del Documento 0 "))
(define gDocs-17 ((login gDocs-16 "user2" "pass2" add)
                  2 (crear-fecha 02 11 2021) "Tercer contenido del Documento 2 "))
(define gDocs-18 ((login gDocs-17 "user1" "pass1" add)
                  0 (crear-fecha 02 11 2021) "Cuarto contenido del Documento 0 "))
(define gDocs-19 ((login gDocs-18 "user4" "pass4" add)
                  4 (crear-fecha 02 11 2021) "Segundo contenido del Documento 4 "))

; EJEMPLOS DE LA FUNCIÓN LOGIN-RESTOREVERSION

(define gDocs-20 ((login gDocs-19 "user3" "pass3" restoreVersion)
                  3 0))
(define gDocs-21 ((login gDocs-20 "user1" "pass1" restoreVersion)
                  0 2))
(define gDocs-22 ((login gDocs-21 "user3" "pass3" restoreVersion)
                  3 0))

; EJEMPLOS DE LA FUNCIÓN LOGIN-REVOKEALLACCESSES

(define gDocs-23 (login gDocs-22 "user2" "pass2" revokeAllAccesses))
(define gDocs-24 (login gDocs-23 "user3" "pass3" revokeAllAccesses))
(define gDocs-25 (login gDocs-24 "user5" "passerronea" revokeAllAccesses))

; EJEMPLOS DE LA FUNCIÓN LOGIN-SEARCH

;((login gDocs-25 "user1" "pass1" search) "contenido")
;((login gDocs-25 "user1" "pass1" search) "Cuarto")
;((login gDocs-25 "user5" "pass5" search) "Doc")

; EJEMPLOS DE LA FUNCIÓN LOGIN-PARADIGMADOCS->STRING

;(login gDocs-25 "user2" "pass2" paradigmadocs->string)
;(login gDocs-25 "user1" "pass1" paradigmadocs->string)
;(login gDocs-25 "user4" "pass4" paradigmadocs->string)

; EJEMPLOS DE LA FUNCIÓN PARADIGMADOCS->STRING

;(paradigmadocs->string gDocs-25)
;(paradigmadocs->string gDocs-12)
;(paradigmadocs->string gDocs-1)
