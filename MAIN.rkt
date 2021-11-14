#lang racket

(require "TDA_paradigmadocs.rkt")
(require "TDA_fecha.rkt")
(require "TDA_usuarios.rkt")
(require "TDA_documentos.rkt")

; Implementación de los requisitos funcionales

; Descripción: Función que permite el registro de un usuario a la plataforma creada mediante la solicitud de el nombre de...
;              ... de la plataforma, fecha, nombre de usuario y contraseña.
; Dominio: paradigmadocs X date X string X string
; Recorrido: Actualización de paradigmadocs
; Tipo de recursión: Recursión natural (empleada en la función "esta-usuario?") la cual contempla como caso base la nula existencia...
;                    ... de listas de usuarios contenidas en una lista general de las mismas para luego considerar los condicionales,...
;                    ... en caso de que el caso base sea falso, los cuales establecen una comparación entre los usuarios de la lista...
;                    ... a evaluar, retornando el valor booleano "#t" en caso de que el usuario se encuentre y como caso contrario...
;                    ... un último condicional evaluando la nula existencia de el resto de las listas ya aludidas. De forma que, si...
;                    ... no quedan más lista se retorna el valor booleano "#f" (no se encuentra) y en caso que aún no se hayan evaluado...
;                    ... todas las listas retornar el caso recursivo considerando el nombre de la función ("esta-usuario?") y sus entradas...
;                    ... como el resto de la lista ("cdr") y el usuario a comparar.

(define register
  (lambda (paradigmadocs fecha usuario contraseña)
    ; ¿El nombre de usuario a registrar se encuentra ya registrado?
    (if (equal? (esta-usuario? (get-dato paradigmadocs 4) usuario) #f)
        ; Caso en donde el usuario no se encuentra registrado: Se registra
        (set-act-list-usuarios-paradigmadocs paradigmadocs
                                             (crear-usuario
                                              fecha
                                              usuario
                                              contraseña))
        ; Caso en donde el usuario se encuentra registrado: No se registra
        paradigmadocs)
    )
  )
       
; Descripción: Función que permite autenticar un usuario (activar usuario) y de ello la ejecución de comandos definido en la plataforma...
;              ... dependiendo de la correcta validación del usuario
; Dominio: paradigmadocs X string X string X function
; Recorrido: función (parcialmente evaluada)
; Funciones de orden superior empleadas en el llamado de las operaciones ingresadas
; Tipo de recursión: Recursión natural (empleada en la función "usuario-contraseña-valida?") la cual considera como caso base la nula...
;                    ... existencia de listas contenedoras de usuarios en una lista general de las mismas para evaluar dos condicionales...
;                    ... en donde el primero compara el usuario y contraseña ingresado con los existentes en la lista para, en caso de...
;                    ... ser verdaderos se retorna el valor booleano "#t" y caso contrario el segundo condicional evaluando la nula...
;                    ... existencia del resto de listas en la lista general para finalmente, en caso de ser falso se realiza el llamado...
;                    ... recursivo considerando como parámetros, el resto de la lista, usuario y contraseña.

(define login
  (lambda (paradigmadocs usuario contraseña operacion)
    ; ¿El usuario existe y su correspondiente contraseña es válida para el mismo?
    (if (usuario-contraseña-valida? (get-dato paradigmadocs 4) usuario contraseña)
        ; Caso verdadero: Se procede a evalúar si la operación ingresada es un procedimiento/función y con ello su retorno parcialmente evaluado (si es un proceso)
        ; Es importante comentar que una vez ubicada la operación, esta es llamada otorgando (junto con lo necesario para el funcionamiento de la función) la...
        ; ... plataforma con el usuario activo/logeado (función "set-n-remov")
        (if (equal? operacion create)
            (create (set-n-remov usuario paradigmadocs))
            (if (equal? operacion share)
                (share (set-n-remov usuario paradigmadocs))
                (if (equal? operacion add)
                    (add (set-n-remov usuario paradigmadocs))
                    (if (equal? operacion restoreVersion)
                        (restoreVersion (set-n-remov usuario paradigmadocs))
                        (if (equal? operacion revokeAllAccesses)
                            (revokeAllAccesses (set-n-remov usuario paradigmadocs))
                            (if (equal? operacion search)
                                (search (set-n-remov usuario paradigmadocs))
                                (if (equal? operacion paradigmadocs->string)
                                    (paradigmadocs->string (set-n-remov usuario paradigmadocs))
                                    paradigmadocs)
                                )
                            )
                        )
                    )
                )
            )
        ; Caso falso: Se procede a retornar la operación según lo expresado en el documento del proyecto
        (operacion paradigmadocs))
    )
  )

; Descripción: Función que permite a un usuario con sesión iniciada en la plataforma definida crear un documento de texto de modo que en este...
;              ... se registre al autor, fecha de creación, nombre del documento y su contenido (como string) retornando una versión actualizada...
;              ... de la plataforma empleada de acuerdo a los parámetros ingresados en paralelo con la eliminación de la sesión activa del...
;              ... usuario/creador correspondiente
; Dominio: paradigmadocs X date X nombre-documento X contenido
; Recorrido: Actualización de paradigmadocs

(define create
  (lambda (paradigmadocs)
    (lambda (fecha nombre-documento contenido)
      ; ¿Existe un usuario autenticado/logeado/activo en la plataforma?
      (if (buscar-usuario-activo (get-dato paradigmadocs 4))
          ; Caso verdadero: Mediante getters y setters se crea un documento de acuerdo con la información ingresada (consultar TDA_documentos para la representación)...
          ;                 ... luego se remueve la sesión activa del usuario autenticado retornando la plataforma modificada. Es importante comentar que el contenido...
          ;                 ... (string) ingresado es encriptado en la plataforma
          (set-n-remov (buscar-usuario-activo (get-dato paradigmadocs 4))
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
          ; Caso falso: No se realiza ninguna operación/cambio sobre la plataforma según lo explicitado en el documento del proyecto
          paradigmadocs)
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
      ; ¿Existe un usuario autenticado/logeado/activo en la plataforma?
      (if (buscar-usuario-activo (get-dato paradigmadocs 4))
          ; Caso verdadero: Se procede a buscar el documento mediante su identificador único y, si el usuario autenticado corresponde al autor del documento...
          ;                 ... mediente getters y setters es modificada la plataforma (inserción de usuario compartido con su respectivo acceso) considerando...
          ;                 ... la eliminación de la sesión activa del usuario
          (if (buscar-Id-documento (get-dato paradigmadocs 5) idDocs)
              (if (buscar-usuario-propietario (buscar-Id-documento (get-dato paradigmadocs 5) idDocs) (buscar-usuario-activo (get-dato paradigmadocs 4)))
                  ; Si el usuario figura como el propietario del documento...
                  (set-n-remov (buscar-usuario-activo (get-dato paradigmadocs 4))
                               (set-n-remov-compartido idDocs (apply list access usuario-acceso) paradigmadocs))
                  ; Si el usuario no figura como el propietario del documento...
                  (set-n-remov (buscar-usuario-activo (get-dato paradigmadocs 4))
                               paradigmadocs)) ;retorna la plataforma previo a la operación
              ; Si el documento no existe...
              (set-n-remov (buscar-usuario-activo (get-dato paradigmadocs 4))
                               paradigmadocs)) ;retorna la plataforma previo a la operación
          ; Caso falso: No se realiza ninguna operación/cambio sobre la plataforma según lo explicitado en el documento del proyecto
          paradigmadocs)
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
      ; ¿Existe un usuario autenticado/logeado/activo en la plataforma?
      (if (buscar-usuario-activo (get-dato paradigmadocs 4))
          ; Caso verdadero: Se procede a buscar el documento mediante su identificador único y, si el usuario autenticado corresponde al autor del documento o...
          ;                 ... este posee permisos de escritura sobre el mismo, mediante getters y setters se registra el cambio en el documento reflejado como...
          ;                 ... una nueva versión del mismo (consultar TDA_documentos para la representación) removiendo la sesión activa del usuario
          ; Es importante comentar que el contenido (string) ingresado es encriptado en la plataforma
          (if (buscar-Id-documento (get-dato paradigmadocs 5) idDocs)
              (if (or (buscar-usuario-propietario (buscar-Id-documento (get-dato paradigmadocs 5) idDocs) (buscar-usuario-activo (get-dato paradigmadocs 4)))
                      (buscar-usuario-editor (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) idDocs) 5) (buscar-usuario-activo (get-dato paradigmadocs 4))))
                  ; Si el usuario es propietario o editor del documento...
                  (set-n-remov (buscar-usuario-activo (get-dato paradigmadocs 4))
                               (agregar-remover-doc idDocs fecha contenido paradigmadocs))
                  ; Si el usuario no es propietario o editor del documento...
                  (set-n-remov (buscar-usuario-activo (get-dato paradigmadocs 4))
                               paradigmadocs)) ;retorna la plataforma previo a la operación
              ; Si el documento no existe...
              (set-n-remov (buscar-usuario-activo (get-dato paradigmadocs 4))
                           paradigmadocs)) ;retorna la plataforma previo a la operación
          ; Caso falso: No se realiza ninguna operación/cambio sobre la plataforma según lo explicitado en el documento del proyecto
          paradigmadocs)
      )
    )
  )

; Descripción: Función que permite restaurar una versión anterior de un documento de modo que la versión a restaurar se fije como la versión actual y la activa...
;              ... figure como una versión más en el historial de las mismas. Para ello, solo el propietario del documento puede realizar esta operación en paralelo...
;              ... con su correcta identificación mediante el login retornando una versión modificada de la plataforma junto con la eliminación de la sesión activa...
;              ... del usuario
; Dominio: paradigmadocs X integer X integer
; Recorrido: Actualización de paradigmadocs

(define restoreVersion
  (lambda (paradigmadocs)
    (lambda (idDoc idVersion)
      ; ¿Existe un usuario autenticado/logeado/activo en la plataforma?
      (if (buscar-usuario-activo (get-dato paradigmadocs 4))
          ; Caso verdadero: Se procede a buscar el documento mediante su identificador único junto con la verificación que refleje que el usuario autenticado...
          ;                 ... corresponde al autor del documento y la existencia de la versión indicada para luego mediante getters y setters aplicar el cambio...
          ;                 ... agregando una lista de versión a la lista general nueva con el cambio, removiendo la anterior y actualizando el valor de las versiones...
          ;                 ... o identificador de cada lista en paralelo con la eliminación del usuario activo
          (if (buscar-Id-documento (get-dato paradigmadocs 5) idDoc)
              (if (buscar-usuario-propietario (buscar-Id-documento (get-dato paradigmadocs 5) idDoc) (buscar-usuario-activo (get-dato paradigmadocs 4)))
                  (if (get-n-version-lista (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) idDoc) 4) idVersion)
                      (set-n-remov (buscar-usuario-activo (get-dato paradigmadocs 4))
                                   (agregar-remover-version idDoc idVersion paradigmadocs))
                      ; Si la versión del documento no existe...
                      (set-n-remov (buscar-usuario-activo (get-dato paradigmadocs 4))
                                   paradigmadocs)) ;retorna la plataforma previo a la operación
                  ; Si el usuario autenticado no figura como propietario del documento...
                  (set-n-remov (buscar-usuario-activo (get-dato paradigmadocs 4))
                               paradigmadocs)) ;retorna la plataforma previo a la operación
              ; Si el documento no existe...
              (set-n-remov (buscar-usuario-activo (get-dato paradigmadocs 4))
                           paradigmadocs)) ;retorna la plataforma previo a la operación
          ; Caso falso: No se realiza ninguna operación/cambio sobre la plataforma según lo explicitado en el documento del proyecto
          paradigmadocs) 
      )
    )
  )

; Descripción: Función que permite al propietario/autor de un documento revocar todos los accesos a sus documentos implementando la función...
;              ... de forma declarativa aplicando la función "map" y retornando la plataforma modificada con la eliminación del usuario activo
; Dominio: paradigmadocs
; Recorrido: Actualización de paradigmadocs

(define revokeAllAccesses
  (lambda (paradigmadocs)
    ; ¿Existe un usuario autenticado/logeado/activo en la plataforma?
    (if (buscar-usuario-activo (get-dato paradigmadocs 4))
        ; Caso verdadero: Se filtran los documentos en los que figure el usuario activo como propietario del mismo para luego empleando "map"...
        ;                 ... se aplica un setter que agrega una lista vacia en la posición de usuarios compartidos para luego anexarla a la...
        ;                 ... lista de todos los documentos priorizando agregar, entre dos documentos iguales (original y modificado) el modificado...
        ;                 ... y finalmente eliminar la sesión activa del usuario
        (set-n-remov (buscar-usuario-activo (get-dato paradigmadocs 4))
                     (set-act-list-doc-paradigmadocs paradigmadocs
                                                     (concatenador (get-dato paradigmadocs 5)
                                                                   (map set-list-compartido
                                                                        (filtrador-doc (get-dato paradigmadocs 5)
                                                                                       (buscar-usuario-activo (get-dato paradigmadocs 4)))))))
        ; Caso falso: No se realiza ninguna operación/cambio sobre la plataforma según lo explicitado en el documento del proyecto
        paradigmadocs)
    )
  )

; Descripción: Función que permite a un usuario activo en la plataforma buscar documentos en donde exista un texto especifíco y dicho documento...
;              ... refleje al usuario como propietario y/o usuario compartido. De esta forma, se evalúan todas las versiones de los documentos...
;              ... considerando los accesos otorgados e implementando la función de forma declarativa empleando la función "filter"
; Dominio: paradigmadocs X frase
; Recorrido: Lista

(define search
  (lambda (paradigmadocs)
    (lambda (frase)
      ; ¿Existe un usuario autenticado/logeado/activo en la plataforma?
      (if (buscar-usuario-activo (get-dato paradigmadocs 4))
          ; Caso verdadero: Se emplea la función filter para aplicar a cada lista correspondiente a un documento contenido en una lista general de los mismos...
          ;                 ... la función currificada "llamado-verificador-de-substring" la cual, una vez otorgados los parámetros de entrada, llama a la función...
          ;                 ... "verificador-de-substring" encargada de revisar recursivamente el contenido de todas las versiones del documento extraido...
          ;                 ... retornando un valor booleando el cual es considerado por la función filter para filtrar dichos documentos que cumplen esta primera...
          ;                 ... condición. A continuación, las listas resultantes son nuevamente filtradas por un nuevo filter el cual aplica la función...
          ;                 ... "valido?" la cual se encarga de retornar un valor booleano de acuerdo con si el usuario autenticado figura como propietario y/o...
          ;                 ... usuario compartido (con un acceso válido del tipo escritura o lectura) para así retornar una lista de documentos los cuales cumplen..
          ;                 ... todos los requisitos considerando en el enunciado del proyecto
          (filter (valido? (buscar-usuario-activo (get-dato paradigmadocs 4)))
                  (filter (llamado-verificador-de-substring frase) (get-dato paradigmadocs 5)))
          ; Caso falso: Se retorna null según lo explicitado en el documento del proyecto
          null)
      )
    )
  )

; Descripción: Función que permite entregar una representación comprensible para el usuario de la plataforma (tipo paradigmadocs) trabajada. Para ello,...
;              ... si un usuario autenticado ejecuta la presente función obtendra un string el cual considera los datos de dicho usuario tales como...
;              ... nombre de usuario, fecha de creación, documentos, versiones, usuarios compartidos y documentos compartidos con el mismo. Por otro lado...
;              ... Si un usuario no autenticado ejecuta la presente función obtendrá un string el cual considera todo contenido en la plataforma paradigmadocs....
;              ... Es importante comentar que el string entregado al ser impreso por la función write o display reflejará de forma clara y comprensible los...
;              ... antecedentes, del mismo modo, el contenido en los documentos (versiones) es desencriptado para la correcta representación
; Dominio: paradigmadocs
; Recorrido: string

(define paradigmadocs->string
  (lambda (paradigmadocs)
    ; ¿Existe un usuario autenticado/logeado/activo en la plataforma?
    (if (buscar-usuario-activo (get-dato paradigmadocs 4))
        ; Caso verdadero: Mediante recursión y funciones estilo "apply-to-all" se busca, filtra y concatena los datos vinculados al usuario a retornar (string)
        (string-append (usuario->string (get-dato paradigmadocs 4) (buscar-usuario-activo (get-dato paradigmadocs 4)))
                       (string-join (map (documento->string (buscar-usuario-activo (get-dato paradigmadocs 4)))
                                         (map (filtrador-propietario-compartido (buscar-usuario-activo (get-dato paradigmadocs 4)))
                                              (get-dato paradigmadocs 5)))))
        ; Caso falso: Mediante recursión se concatenan los datos vinculados a la plataforma a retornar (string)
        (string-append "Nombre plataforma: " (get-dato paradigmadocs 0) "\n"
                       "Fecha creación: " (fecha->string (get-dato paradigmadocs 1)) "\n"
                       (string-join (usuarios->string (get-dato paradigmadocs 4)))
                       (string-join (documentos->string (get-dato paradigmadocs 5))))
        )
    )
  )
  

;--------------------------------------------------------------------------------------------EJEMPLOS--------------------------------------------------------------------------------------------
; CREACIÓN DE LA PLATAFORMA PARADIGMADOCS

(define gDocs-0 (paradigmadocs "Paradigmadocs" (date 16 10 2021) encryptFn encryptFn))

; EJEMPLOS DE LA FUNCIÓN REGISTER

(define gDocs-1 (register
                 (register
                  (register
                   gDocs-0
                   (date 25 10 2021) "user1" "pass1")
                  (date 25 10 2021) "user2" "pass2")
                 (date 25 10 2021) "user3" "pass3"))
(define gDocs-2 (register
                 gDocs-1 (date 26 10 2021) "user4" "pass4"))
(define gDocs-3 (register
                 gDocs-2 (date 27 10 2021) "user5" "pass5"))

; EJEMPLOS DE LA FUNCIÓN LOGIN-CREATE

(define gDocs-4 ((login gDocs-3 "user1" "pass1" create)
                 (date 28 10 2021) "Documento 0" "Primer contenido del Documento 0 "))
(define gDocs-5 ((login gDocs-4 "user1" "pass1" create)
                 (date 28 10 2021) "Documento 1" "Primer contenido del Documento 1 "))
(define gDocs-6 ((login gDocs-5 "user2" "pass2" create)
                 (date 29 10 2021) "Documento 2" "Primer contenido del Documento 2 "))
(define gDocs-7 ((login gDocs-6 "user3" "pass3" create)
                 (date 29 10 2021) "Documento 3" "Primer contenido del Documento 3 "))
(define gDocs-8 ((login gDocs-7 "user5" "pass5" create)
                 (date 30 10 2021) "Documento 4" "Primer contenido del Documento 4 "))

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
                  0 (date 01 11 2021) "Segundo contenido del Documento 0 "))
(define gDocs-14 ((login gDocs-13 "user2" "pass2" add)
                  2 (date 01 11 2021) "Segundo contenido del Documento 2 "))
(define gDocs-15 ((login gDocs-14 "user3" "pass3" add)
                  3 (date 01 11 2021) "Segundo contenido del Documento 3 "))
(define gDocs-16 ((login gDocs-15 "user1" "pass1" add)
                  0 (date 02 11 2021) "Tercer contenido del Documento 0 "))
(define gDocs-17 ((login gDocs-16 "user2" "pass2" add)
                  2 (date 02 11 2021) "Tercer contenido del Documento 2 "))
(define gDocs-18 ((login gDocs-17 "user1" "pass1" add)
                  0 (date 02 11 2021) "Cuarto contenido del Documento 0 "))
(define gDocs-19 ((login gDocs-18 "user4" "pass4" add)
                  4 (date 02 11 2021) "Segundo contenido del Documento 4 "))

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

; Para evitar la sobrecarga de información de la pantalla de interacciones los siguientes ejemplos estarán comentados
; EJEMPLOS DE LA FUNCIÓN LOGIN-SEARCH

;((login gDocs-24 "user1" "pass1" search) "contenido")
;((login gDocs-24 "user1" "pass1" search) "Cuarto")
;((login gDocs-24 "user5" "pass5" search) "Doc")

; EJEMPLOS DE LA FUNCIÓN LOGIN-PARADIGMADOCS->STRING

;(login gDocs-24 "user2" "pass2" paradigmadocs->string)
;(login gDocs-24 "user1" "pass1" paradigmadocs->string)
;(login gDocs-24 "user4" "pass4" paradigmadocs->string)

; EJEMPLOS DE LA FUNCIÓN PARADIGMADOCS->STRING

;(paradigmadocs->string gDocs-24)
;(paradigmadocs->string gDocs-12)
;(paradigmadocs->string gDocs-1)
