#lang racket

(require "TDA_fecha.rkt")
(require "TDA_paradigmadocs.rkt")
(require "TDA_usuarios.rkt")

; Implementación del TDA documento

; Representación
; (integer X string X date X string X ( (integer X date X string) (...) ) X ( (string X access) (...) ) ) 
; (list ID-documento "AUTOR" (dia mes año) "NOMBRE-DOCUMENTO" ( (version-documento (dia mes año) "CONTENIDO-DOCUMENTO") (...) ) ( ("USUARIO-COMPARTIDO" TIPO-DE-ACCESO) (...) ) )

; Constructor:
; Descripción: Permite la creación de un documento (lista) nuevo mediante la solicitud de los datos...
;              ...fecha, nombre de documento, usuario, ID-documento, contenido y versión del documento
; Dominio: ( (dia X mes X año) X string X string X FN-ID-documento X contenido X FN-versión-documento)
; Recorrido: Lista con los datos como en la representación
; Importante: ID-documento/versión, "AUTOR" en paralelo con listas inicialmente vacias correspondientes a contenedores de usuarios compartidos y versiones
;             ... es ingresado automáticamente y estas listas aludidas serán operadas según se ejecute determinada función u operación. Adicionalmente, ...
;             ... "FN-ID-documento" y "FN-versión-documento" son ingresadas automáticamente

(define crear-documento
  (lambda (fecha nombre-documento usuario ID-documento contenido version-documento)
    (if (documento? (list ID-documento usuario fecha nombre-documento (list (list version-documento fecha contenido )) '() ))
        (list ID-documento usuario fecha nombre-documento (list (list version-documento fecha contenido)) (list ) )
        null)
    )
  )

; Pertenencia:
; Descripción: Función que permite verificar si un documento se encuentra bien definido
; Dominio: Lista correspondiente a la información de un documento
; Recorrido: Booleando verificador

(define documento?
  (lambda (lista-documento)
    ; ¿Es lista?
    (if (list? lista-documento)
        (if (= (length lista-documento) 6)
            ; ¿El primer elemento en la lista en un entero?
            (if (integer? (car lista-documento))
                ; ¿El segundo elemento en la lista es un string?
                (if (string? (car (cdr lista-documento)))
                    ; ¿El tercer elemento en la lista corresponde a una fecha de acuerdo con el TDA_fecha?
                    (if (fecha? (car (cdr (cdr lista-documento))))
                        ; ¿El cuarto elemento en la lista es un string?
                        (if (string? (car (cdr (cdr (cdr lista-documento)))))
                            ; ¿El quinto y sexto elemento en la lista corresponden a listas?
                            (if (and (list? (car (cdr (cdr (cdr (cdr lista-documento)))))) (list? (car (cdr (cdr (cdr (cdr (cdr lista-documento))))))))
                                #t ; Si lo anterior se cumple...
                                #f)
                            #f)
                        #f)
                    #f)
                #f)
            #f)
        #f)
    )
  )

; Selectores:
; Descripición: Capa que permite la obtención de un determinado elemento en la lista de documentos
; Dominio: Lista X entero
; Recorrido: Elemento especifíco de acuerdo con su posición
; Recursión: Recursión de cola cuyo caso base contempla la entrega del primer elemento de la lista ingresada en caso de que el valor...
;            ... correspondiente a la posición buscada sea cero, caso contrario, se llama a la función empleando "cdr" de modo que...
;            ... se obtiene lo que puede considerarse como el resto de la lista restando un digito al n ingresado para "calibrar"...
;            ... la busqueda. El proceso se repite hasta conseguír dicho elemento.

(define get-dato-doc
  (lambda (lista-documentos n)
    ; Caso base: ¿El elemento a buscar corresponde a la ubicación 0?
    (if (= n 0)
        (car lista-documentos)
        ; Caso contrario y/o recursivo, se retorna el resto de la lista de documentos y se resta en uno el valor de la ubicación buscada
        (get-dato-doc (cdr lista-documentos) (- n 1))
        )
    )
  )

; Descripción: Permite obtener la versión de un documento contenido en una lista de listas
; Dominio: list
; Recorrido: integer

(define get-version-doc
  (lambda (lista-version-doc)
    (car lista-version-doc)
    )
  )

; Descripción: Función que permite obtener la última lista de listas contenedoras de versiones en un documento especifíco
; Dominio: lista de listas de versiones
; Recorrido: lista
; Tipo de recursión: Recursión natural/lineal

(define ultima-version
  (lambda (lista-versiones)
    (if (null? (cdr lista-versiones))
        (car lista-versiones)
        (ultima-version (cdr lista-versiones)) 
        )
    )
  )

; Descripción: Función que permite obtener la enésima versión de un documento (en caso de existir) contenido en una lista de listas 
; Dominio: lista de versiones X versión
; Recorrido: lista
; Tipo de recursión: Recursión natural/lineal
; Importante: La presente función se ejecuta de forma conjunta con la función "n-version" definida más adelante en el espectro de...
;             ... de "Otras funciones"

(define n-version-lista
  (lambda (lista-versiones version)
    (if (equal? (car (car lista-versiones)) version)
            (car lista-versiones)
            (if (null? (cdr lista-versiones))
                #f      
                (n-version-lista (cdr lista-versiones) version))
        )
    )
  )

; Descripción:
; Dominio:
; Recorrido:

(define get-usuario-compartido
  (lambda (lista-usuarios-compartido)
    (car lista-usuarios-compartido)
    )
  )

; Descripción:
; Dominio:
; Recorrido:

(define get-acceso-compartido
  (lambda (lista-usuarios-compartido)
    (car (cdr lista-usuarios-compartido))
    )
  )

; Descripción:
; Dominio:
; Recorrido:

(define get-contenido
  (lambda (lista-version)
    (car (cdr (cdr lista-version))
    )
  ))


; Modificador:
; Descripción: Función que permite la inserción de un valor entero de versión a una lista de versionamiento pertenenciente al TDA
; Dominio: lista X versión
; Recorrido: lista de versión modificada/actualizada

(define set-version-doc
  (lambda (lista-doc version)
    (list version
          (get-dato-doc lista-doc 1)
          (get-dato-doc lista-doc 2))
    )
  )

; Descripción: Función que permite la inserción de una lista de usuarios compartidos de acuerdo al TDA (enfocado a la función revokeAllAccess)
; Dominio: lista
; Recorrido: lista de documentos modificada/actualizada

(define set-list-compartido
  (lambda (lista-doc)
    (list (get-dato-doc lista-doc 0)
          (get-dato-doc lista-doc 1)
          (get-dato-doc lista-doc 2)
          (get-dato-doc lista-doc 3)
          (get-dato-doc lista-doc 4)
          (list ))
    )
  )

; Descripción: Función que permite modificar la lista de usuarios compartidos asignada de acuerdo con el ID del documento, los usuarios y accesos ingresados
; Dominio: Lista de usuarios compartidos X id X usuario X acceso X paradigmadocs
; Recorrido: Lista de usuarios compartidos actualizada
; Tipo de recursión: Recursión natural/lineal

(define agregar-usuario-compartido
  (lambda (lista-base id-doc usuario-acceso paradigmadocs)
    ; ¿La lista de listas del TDA-documentos es nula?
    (if (null? lista-base)
        #f
        ; ¿Existe el documento a buscar de acuerdo con su ID?
        (if (buscar-Id-documento (get-dato paradigmadocs 5) id-doc)
            ; "set-act-list-doc-paradigmadocs": Función ubicada en TDA_paradigmadocs
            (set-act-list-doc-paradigmadocs paradigmadocs
                                            (anexar-listas (get-dato paradigmadocs 5)
                                                                      (list (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 0)
                                                                            (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 1)
                                                                            (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 2)
                                                                            (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 3)
                                                                            (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 4)
                                                                            (append (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 5)
                                                                                    usuario-acceso))))
            ; ¿El resto de la lista de listas del TDA-documentos es nula?
            (if (null? (cdr lista-base))
                #f
                ; Llamado recursivo entregando el resto de las listas en la lista "base", el ID, usuario y acceso junto con la plataforma
                (agregar-usuario-compartido (cdr lista-base) id-doc usuario-acceso paradigmadocs))
            )
        )
    )
  )

; Descripción: Función que permite modificar la lista de documentos asignada de acuerdo con la ID del documento, fecha y contenido ingresado
; Dominio: Lista de documentos X id X fecha X contenido X paradigmadocs
; Recorrido: Lista de documentos actualizada
; Tipo de recursión: Recursión natural/lineal

(define agregar-version-doc
  (lambda (lista-base id-doc fecha contenido paradigmadocs)
    ; ¿La lista de listas del TDA-documentos es nula?
    (if (null? lista-base)
        #f
        ; ¿Existe el documento a buscar de acuerdo con su ID?
        (if (buscar-Id-documento (get-dato paradigmadocs 5) id-doc)
            ; "set-act-list-doc-paradigmadocs": Función ubicada en TDA_paradigmadocs
            (set-act-list-doc-paradigmadocs paradigmadocs
                                            (anexar-listas (get-dato paradigmadocs 5)
                                                           (list (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 0)
                                                                 (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 1)
                                                                 (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 2)
                                                                 (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 3)
                                                                 (append (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 4)
                                                                         (list
                                                                          (list (ID-version-doc (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 4) -1)
                                                                                fecha
                                                                                (encryptFn
                                                                                 (string-append (encryptFn (get-dato-doc (ultima-version (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 4)) 2))
                                                                                                contenido)))))
                                                                 (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 5))))
            ; ¿El resto de la lista de listas del TDA-documentos es nula?
            (if (null? (cdr lista-base))
                #f
                ; Llamado recursivo entregando el resto de las listas en la lista "base", la fecha y contenido junto con la plataforma
                (agregar-version-doc (cdr lista-base) fecha contenido paradigmadocs))
            )
        )
    )
  )

; Descripción: Función que permite modificar la lista de documentos asignada de acuerdo con la ID del documento y versión del mismo ingresado
; Dominio: Lista de documentos X id X id-version X paradigmadocs
; Recorrido: Lista de documentos actualizada
; Tipo de recursión: Recursión natural/lineal

(define restaurar
  (lambda (lista-base id-doc id-version paradigmadocs)
    ; ¿La lista de listas del TDA-documentos es nula?
    (if (null? lista-base)
        #f
        ; ¿Existe el documento a buscar de acuerdo con su ID?
        (if (buscar-Id-documento (get-dato paradigmadocs 5) id-doc)
            ; "set-act-list-doc-paradigmadocs": Función ubicada en TDA_paradigmadocs
            (set-act-list-doc-paradigmadocs paradigmadocs
                                            (anexar-listas (get-dato paradigmadocs 5)
                                                           (list (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 0)
                                                                 (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 1)
                                                                 (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 2)
                                                                 (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 3)
                                                                 (reverse (correlativo (remove-n-version-lista
                                                                                        (append (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 4)
                                                                                                (list
                                                                                                 (set-version-doc (n-version-lista
                                                                                                                   (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 4)
                                                                                                                   id-version)
                                                                                                                  (ID-version-doc
                                                                                                                   (get-dato-doc
                                                                                                                    (buscar-Id-documento
                                                                                                                     (get-dato paradigmadocs 5)
                                                                                                                     id-doc)
                                                                                                                    4) -1 ))))
                                                                                        id-version)
                                                                                       0
                                                                                       (list ) ))
                                                                 (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 5))))
            ; ¿El resto de la lista de listas del TDA-documentos es nula?
            (if (null? (cdr lista-base))
                #f
                ; Llamado recursivo entregando el resto de las listas en la lista "base", la ID y versión del documento junto con la plataforma
                (restaurar (cdr lista-base) id-doc id-version paradigmadocs))
            )
        )
    )
  )

; Otras funciones:
; Descripción: Función que permite calcular una ID y/o versión de documento considerando para el primer caso el número de documentos distintos...
;              ... y para el segundo el número de versiones distintas existentes (aplica para ambos casos)
; Dominio: Lista de listas y un acumulador (inicialmente siempre uno negativo o "-1" para que la primera versión sea 0)
; Recorrido: integer
; Tipo de recursión: Recursión de cola (sin estados pendientes)
; Importante: El acumulador o "ID-acc" es ingresado automáticamente de acuerdo con las operaciones realizadas por el usuario

(define ID-version-doc
  (lambda (lista-documentos ID-acc)
    ; ¿La lista contenedora de listas de documentos es nula?
    (if (null? lista-documentos)
        (add1 ID-acc)
        ; Caso contrario y/o recursivo retorna la función con el resto de la lista sumando uno al acumulador de ID
        (ID-version-doc (cdr lista-documentos) (add1 ID-acc))
        )
    )
  )

; Descripción: Función que permite el ingreso de dos parámetros como lista correspondientes al usuario con su respectivo acceso a determinado documento
; Dominio: usuario X acceso
; Recorrido: lista

(define (access . usuario-acceso)
      usuario-acceso)
  
; Descripción: Función que permite retornar la lista en donde se encuentra determinado identificador de documento de forma recursiva
; Dominio: lista X integer
; Recorrido: lista
; Tipo de recursión: Recursión natural/lineal

(define buscar-Id-documento
  (lambda (lista-base Id-documento)
    ; ¿El id del documento es equivalente al contenido en la primer lista de listas contenedora de documentos?
    (if (equal? Id-documento (get-dato-doc (car lista-base) 0))
        (car lista-base)
        ; ¿El resto de la lista es nulo?
        (if (null? (cdr lista-base))
            #f
            ; Llamado recursivo retornando la función, el resto de la lista y el Id ingresado
            (buscar-Id-documento (cdr lista-base) Id-documento)
            )
        )
    )
  )

; Descripción: Función que permite remover la lista de usuarios compartidos obsoleta, no actualizada o repetida una vez insertada la correcta
; Dominio: lista X id X paradigmadocs
; Recorrido: plataforma con el cambio efectuado (lista usuarios compartidos)
; Tipo de recursión: Recursión natural/lineal
; Es importante comentar que la presente función no se encuetra en "TDA_paradigmadocs" puesto que se produciría un error en la ejecución...
; ... del código a causa de un "llamado entrelazado" entre los dos archivos

(define remov-lista-repet
  (lambda (lista-base id-doc paradigmadocs)
    ; ¿La lista es nula?
    (if (null? lista-base)
        lista-base
        ; ¿Existe el documento cuyo identificador es equivalente al ingresado?
        (if (buscar-Id-documento (get-dato paradigmadocs 5) id-doc)
            (list (get-dato paradigmadocs 0)
                  (get-dato paradigmadocs 1)
                  (get-dato paradigmadocs 2)
                  (get-dato paradigmadocs 3)
                  (get-dato paradigmadocs 4)
                  (remove (buscar-Id-documento (get-dato paradigmadocs 5) id-doc)
                          (get-dato paradigmadocs 5)))
            ; Caso recursivo retornando la función con el resto de la lista, identificador y plataforma
            (remov-lista-repet (cdr lista-base) id-doc paradigmadocs))
        )
    )
  )


; Descripción: Función que realiza un llamado a las funciones "agregar-usuario-compartido" y "remov-lista-repet" definidas anteriormente...
;              ... para agregar/modificar y eliminar determinado documento en base a su actualización (se inserta una versión actualizada...
;              ... y remueve la no actualizada)
; Dominio: id X usuario-acceso X paradigmadocs
; Recorrido: actualización de paradigmadocs

(define agregar-y-remover-compartido
  (lambda (id-doc usuario-acceso paradigmadocs)
    (remov-lista-repet (get-dato (agregar-usuario-compartido (get-dato paradigmadocs 5) id-doc usuario-acceso paradigmadocs) 5)
                       id-doc
                       (agregar-usuario-compartido (get-dato paradigmadocs 5) id-doc usuario-acceso paradigmadocs))
    )
  )

; Descripción: Función que realiza un llamado a las funciones "agregar-version-doc" y "remov-lista-repet" definidas anteriormente...
;              ... para agregar/modificar y eliminar determinado documento en base a su actualización (se inserta una versión actualizada...
;              ... y remueve la no actualizada)
; Dominio: id X fecha X contenido X paradigmadocs
; Recorrido: actualización de paradigmadocs

(define agregar-remover-doc
  (lambda (id-doc fecha contenido paradigmadocs)
    (remov-lista-repet (get-dato (agregar-version-doc (get-dato paradigmadocs 5) id-doc fecha contenido paradigmadocs) 5)
                       id-doc
                       (agregar-version-doc (get-dato paradigmadocs 5) id-doc fecha contenido paradigmadocs))
    )
  )

; Descripción: Función que permite veríficar la existencia de la enésima versión de un documento contenido en una lista de listas
; Dominio: lista de versiones X versión
; Recorrido: Booleano
; Tipo de recursión: Recursión natural/lineal

(define n-version
  (lambda (lista-versiones version)
    ; ¿La primera lista contenida en la lista de listas posee una versión equivalente a la solicitada?
    (if (equal? (car (get-version-doc lista-versiones)) version)
            #t
            ; ¿El resto de las listas de lista es nulo?
            (if (null? (cdr lista-versiones))
                #f
                ; Llamado recursivo retornando la función, el resto de listas y la versión a buscar
                (n-version (cdr lista-versiones) version))
        )
    )
  )

; Descripción: Función que permite obtener una validación booleana de acuerdo con la existencia (o no) de un usuario en la lista...
;              ... reservada para contener listas de usuarios compartidos con sus respectivos accesos...
;              ... (para la presente solo se busca una validación de acceso de edición)
; Dominio: lista de listas de usuarios compartidos X usuario
; Recorrido: Booleano
; Tipo de recursión: Recursión natural/líneal

(define buscar-usuario-editor
  (lambda (lista-base usuario)
    ; ¿La lista de listas es nula?
    (if (null? lista-base)
        #f
        ; ¿El primer elemento de la primera lista considerada es equivalente al usuario buscado y su segundo elemento corresponde al permiso y/o acceso de escritura?
        (if (and (equal? (get-usuario-compartido (car lista-base)) usuario) (equal? (get-acceso-compartido (car lista-base)) #\r))
            #t
            ; Llamado recursivo retornando la función, resto de las listas y el usuario
            (buscar-usuario-editor (cdr lista-base) usuario))
        )
    )
  )

; Descripción: Función que permite la eliminación de determinada lista contenida en lista contenedora de versiones de documentos
; Dominio: lista base de versiones X entero
; Recorrido: lista de versiones actualizada
; Tipo de recursión: recursión natural/lineal

(define remove-n-version-lista
  (lambda (lista-versiones version)
    ; ¿La primera lista contenida en la lista de listas posee en su primer elemento el usuario solicitado?
    (if (equal? (get-version-doc (car lista-versiones)) version)
        (remove (car lista-versiones) lista-versiones)
        ; ¿El resto de listas contenidas en la lista de listas es nulo?
        (if (null? (cdr lista-versiones))
            #f
            ; Llamado recursivo contatenando la lista que no corresponde a la buscada (para no perderla) y el llamado con el resto de listas y la versión buscada
            (cons (car lista-versiones) (remove-n-version-lista (cdr lista-versiones) version))
            )
        )
    )
  )

; Descripción: Función que realiza un llamado a las funciones "restaurar" y "remov-lista-repet" definidas anteriormente...
;              ... para agregar/modificar y eliminar determinada versión de documento en base a su actualización (se inserta una versión actualizada...
;              ... y remueve la no actualizada)
; Dominio: id X id versión X paradigmadocs
; Recorrido: actualización de paradigmadocs

(define agregar-remover-version
  (lambda (id-doc id-version paradigmadocs)
    (remov-lista-repet (get-dato (restaurar (get-dato paradigmadocs 5) id-doc id-version paradigmadocs) 5)
                       id-doc
                       (restaurar (get-dato paradigmadocs 5) id-doc id-version paradigmadocs))
    )
  )

; Descripción: Función que permite modificar mediante recursión y setters la versión de determinado documento contenido es el versionamiento
; Dominio: lista base de versiones X 0 X list
; Recorrido: actualización de la lista base de versiones
; Tipo de recursión: Recursión de cola (sin estados pendientes)

(define correlativo
  (lambda (lista-versiones acc nueva-lista-versiones)
    (if (null? lista-versiones)
        nueva-lista-versiones
        (correlativo (cdr lista-versiones) (add1 acc) (append (cons (set-version-doc (car lista-versiones) acc) nueva-lista-versiones) null))
        )
    )
  )

; Descripción: Función que permite "filtrar" aquellos documentos cuyo autor es el usuario ingresado retornando así, de una lista de listas de...
;              ... documentos, solo los que figuren como propietario quien haya iniciado sesión y ejecutado el procedimiento "revokeAllAccesses"
; Dominio: lista de listas de documentos X usuario
; Recorrido: lista de listas "filtrada"

(define filtrador-doc
  (lambda (lista-doc-base usuario)
    (if (null? lista-doc-base)
        null
        (if (equal? usuario (get-dato-doc (car lista-doc-base) 1))
            (cons (car lista-doc-base) (filtrador-doc (cdr lista-doc-base) usuario))
            (filtrador-doc (cdr lista-doc-base) usuario))
        )
    )
  )

; Descripción: Función que permite concatenar (sin repetición) los elementos filtrados (con la función anterior) ya modificados...
;              ... con los existentes inicialmente en la lista base otorgada priorizando, de los elementos repetidos, los modificados...
;              ... , es decir, de dos listas de listas, al momento de concatenar y encontrarse con dos listas de documentos con el mismo...
;              ... identificador, se prioriza y por ende concatena solo la que se encuentre modificada o en la segunda lista ingresada
; Dominio: lista base de documentos X lista base de documentos filtrados modificados
; Recorrido: lista concatenada sin repetición

(define concatenador
  (lambda (lista-doc-base lista-doc)
    (if (null? lista-doc-base)
        lista-doc
        (if (null? lista-doc)
            lista-doc-base
            (if (equal? (get-dato-doc (car lista-doc-base) 0) (get-dato-doc (car lista-doc) 0))
                (cons (car lista-doc) (concatenador (cdr lista-doc-base) (cdr lista-doc)))
                (cons (car lista-doc-base) (concatenador (cdr lista-doc-base) lista-doc)))
            )
        )
    )
  )

;-------------------------------------------------------------------------------------------------------------------------------------------------------------

(define complemento
  (lambda (frase lista-versiones)
    (if (null? lista-versiones)
        #f
        (if (string-contains? (encryptFn (get-contenido (car lista-versiones))) frase)
            #t
            (if (null? (cdr lista-versiones))
                #f
                (complemento frase (cdr lista-versiones)))
            )
        )
    )
  )

(define verificador-de-substring
  (lambda (frase)
    (lambda (documento)
      (complemento frase (get-dato-doc documento 4))
      )
    )
  )

(define llamado-valido?
  (lambda (usuario)
    (lambda (documento)
      (valido? usuario documento)
      )
    )
  )

(define valido?
  (lambda (usuario documento)
    (if (null? documento)
        #f
        (if (or (buscar-usuario-compartido (get-dato-doc documento 5) usuario) (buscar-usuario-propietario documento usuario) )
            #t
            #f
    )
  )))


(define buscar-usuario-compartido
  (lambda (lista-base usuario)
    ; ¿La lista de listas es nula?
    (if (null? lista-base)
        #f
        ; ¿El primer elemento de la primera lista considerada es equivalente al usuario buscado y su segundo elemento corresponde al permiso y/o acceso de escritura?
        (if (or (and (equal? (get-usuario-compartido (car lista-base)) usuario) (equal? (get-acceso-compartido (car lista-base)) #\r))
                (and (equal? (get-usuario-compartido (car lista-base)) usuario) (equal? (get-acceso-compartido (car lista-base)) #\c))
                (and (equal? (get-usuario-compartido (car lista-base)) usuario) (equal? (get-acceso-compartido (car lista-base)) #\w)))
                #t
            ; Llamado recursivo retornando la función, resto de las listas y el usuario
            (buscar-usuario-compartido (cdr lista-base) usuario))
        )
    )
  )

(define buscar-usuario-propietario
  (lambda (documento usuario)
    ; ¿La lista de listas es nula?
    (if (null? documento)
        #f
        ; ¿El primer elemento de la primera lista considerada es equivalente al usuario buscado y su segundo elemento corresponde al permiso y/o acceso de escritura?
        (if (equal? (get-dato-doc documento 1) usuario)
                #t
            #f
        )
    )
  ))



















#|
'(1 "user1" (30 8 2021) "doc1" ((0 (30 8 2021) "1cod odinetnoc")) (("user2" #\r))) -> '((0 (30 8 2021) "1cod odinetnoc"))
(filter (verificador-de-substring "contenid") '((0 (30 8 2021) "1cod odinetnoc")))

(filter (verificador-de-substring "contenid") '('(1 "user1" (30 8 2021) "doc1" ((0 (30 8 2021) "1cod odinetnoc")) (("user2" #\r)))))
(filter (verificador-de-substring "contenid") (list (list 1 "user1" (list 30 8 2021) "doc1" (list (list 0 (list 30 8 2021) "1cod odinetnoc")) (list (list "user2" #\r)))))
|#



#|

;EJEMPLOS CONSTRUCTOR:
; ...previamente definida la plataforma a emplear: "(define emptyGDocs (paradigmadocs "gDocs" (crear-fecha 16 10 2021) encryptFn encryptFn))"
; ...adicionalmente: (define ejemplo-crear-usuario-1 (crear-usuario (crear-fecha 19 10 2021) "Angel" "contraseña"))
; ...adicionalmente: (define ejemplo-crear-usuario-2 (crear-usuario (crear-fecha 20 10 2021) "Jaime" "pinturaceresita"))
; ...adicionalmente: (define ejemplo-set-act-list-usuarios-paradigmadocs-2 (set-act-list-usuarios-paradigmadocs emptyGDocs ejemplo-crear-usuario-2))
; ...adicionalmente: (define ejemplo-set-act-list-usuarios-paradigmadocs-3 (set-act-list-usuarios-paradigmadocs ejemplo-set-act-list-usuarios-paradigmadocs-2 ejemplo-crear-usuario-1))
; ...adicionalmente: (define paradigmadocs (agregar-y-remover "Angel" ejemplo-set-act-list-usuarios-paradigmadocs-3))
; lo anterior es basicamente register

(define ejemplo-crear-documento-1 (crear-documento
                                   (crear-fecha 25 10 2021) "Gdocs" (buscar-usuario-activo (get-dato paradigmadocs 4)) (ID-version-doc (get-dato paradigmadocs 5) 0) "PRIMER TEXTO" 0))
(define ejemplo-crear-documento-2 (crear-documento
                                   (crear-fecha 30 12 2021) "DOC" (buscar-usuario-activo (get-dato paradigmadocs 4)) (ID-version-doc (get-dato paradigmadocs 5) 0) "INFORME" 0))
(define ejemplo-crear-documento-3 (crear-documento
                                   (crear-fecha 4 5 2025) "TXT" (buscar-usuario-activo (get-dato ejemplo-set-act-list-usuarios-paradigmadocs-3 4)) (ID-version-doc (get-dato ejemplo-set-act-list-usuarios-paradigmadocs-3 5) 0) "BORRADOR" 0))
; El tercer ejemplo expresa una situación no valida

;EJEMPLOS PERTENENCIA:
(define ejemplo-documento?-1 (documento? ejemplo-crear-documento-1))
(define ejemplo-documento?-2 (documento? ejemplo-crear-documento-2))
(define ejemplo-documento?-3 (documento? ejemplo-crear-documento-3))

;EJEMPLOS SELECTORES:
(define ejemplo-get-dato-doc-1 (get-dato-doc ejemplo-crear-documento-1 0))
(define ejemplo-get-dato-doc-1 (get-dato-doc ejemplo-crear-documento-1 4))
(define ejemplo-get-dato-doc-2 (get-dato-doc ejemplo-crear-documento-2 3))

;EJEMPLO MODIFICADORES:
(define ejemplo-modificar-lista-compartidos-1 (modificar-lista-compartidos paradigmadocs (list (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 0)
(define ejemplo-modificar-lista-compartidos-2 (modificar-lista-compartidos paradigmadocs (list "Cale" #\c)))
(define ejemplo-modificar-lista-compartidos-3 (modificar-lista-compartidos ejemplo-modificar-lista-compartidos-2 (list "Laysa" #\r)))  
; Función que implementa la función share de forma muy similar, sin embargo se debe considerar el usuario activo, id del documento y la unión de listas más que listas de listas.
; Por ende, su implementación está orientada al de una función complementaria dentro de share

;EJEMPLO OTRAS FUNCIONES:
(define ejemplo-ID-version-doc-1 (ID-version-doc (get-dato ejemplo-modificar-lista-compartidos-1 5) 0 ))
(define ejemplo-ID-version-doc-2 (ID-version-doc (get-dato ejemplo-modificar-lista-compartidos-2 5) 0 ))
(define ejemplo-ID-version-doc-3 (ID-version-doc (get-dato ejemplo-modificar-lista-compartidos-3 5) 0 ))

; Lo que se espera es ingresar la lista de listas contenedoras del versionamiento de determinado documento de forma que de acuerdo con...
; ...el número de listas en su interior (versiones) se indique el número de versión que debe adoptar la siguiente lista para continuar de..
; ...forma correlativa. Es importante comentar que no es posible ingresar un parámetro genérico para efectos de ejemplificación de la presente...
; ...función, pues el elemento se encuentra y/o determina en MAIN.rkt de modo que habría que anexar dicho archivo a este documento, sin embargo,
; ...aquello ocasionaría un error ya que se produce un llamado de ambas partes para funcionar (no válido)
(define ejemplo-version-documento-1 (version-documento '( '(0 (30 8 2021) "2cod odinetnoc") '(0 (30 8 2021) "A") ) 0))
(define ejemplo-version-documento-2 (version-documento '( '(0 (30 8 2021) "2cod odinetnoc")) 0))
(define ejemplo-version-documento-3 (version-documento '( '(0 (30 8 2021) "2cod odinetnoc") '(0 (30 8 2021) "A") '(0 (30 8 2021) "UWU") ) 0))





|#











(provide (all-defined-out))