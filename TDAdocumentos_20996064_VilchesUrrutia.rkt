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

(define get-ultima-version
  (lambda (lista-versiones)
    (if (null? (cdr lista-versiones))
        (car lista-versiones)
        (get-ultima-version (cdr lista-versiones)) 
        )
    )
  )

; Descripción: Función que permite obtener la enésima versión de un documento (en caso de existir) contenido en una lista de listas 
; Dominio: lista de versiones X versión
; Recorrido: lista
; Tipo de recursión: Recursión natural/lineal
; Importante: La presente función se ejecuta de forma conjunta con la función "n-version" definida más adelante en el espectro de...
;             ... de "Otras funciones"

(define get-n-version-lista
  (lambda (lista-versiones version)
    (if (equal? (car (car lista-versiones)) version)
            (car lista-versiones)
            (if (null? (cdr lista-versiones))
                #f      
                (get-n-version-lista (cdr lista-versiones) version))
        )
    )
  )

; Descripción: Función que permite obtener el usuario en una lista de usuario compartido perteneciente a una lista contenedora de las mismas
; Dominio: lista
; Recorrido: string

(define get-usuario-compartido
  (lambda (lista-usuarios-compartido)
    (car lista-usuarios-compartido)
    )
  )

; Descripción: Función que permite obtener el acceso en una lista de usuario compartido perteneciente a una lista contenedora de las mismas
; Dominio: lista
; Recorrido: char

(define get-acceso-compartido
  (lambda (lista-usuarios-compartido)
    (car (cdr lista-usuarios-compartido))
    )
  )

; Descripción: Función que permite obtener la fecha en una lista de versión perteneciente a una lista contenedora de las mismas
; Dominio: lista
; Recorrido: fecha

(define get-fecha-list-version
  (lambda (lista-version)
    (car (cdr lista-version))
    )
  )

; Descripción: Función que permite obtener el contenido en una lista de versión perteneciente a una lista contenedora de las mismas
; Dominio: lista
; Recorrido: string

(define get-contenido
  (lambda (lista-version)
    (car (cdr (cdr lista-version)))
    )
  )

; Descripción: Función que permite obtener la primera lista de una lista de listas (enfocado en recursión)
; Dominio: lista de listas
; Recorrido: primera lista

(define get-primera-lista
  (lambda (lista-de-listas)
    (car lista-de-listas)
    )
  )
  


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
                                                                                ((get-dato paradigmadocs 2)
                                                                                 (string-append ((get-dato paradigmadocs 3) (get-dato-doc (get-ultima-version (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 4)) 2))
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
                                                                 (reverse (correlativo (remove-get-n-version-lista
                                                                                        (append (get-dato-doc (buscar-Id-documento (get-dato paradigmadocs 5) id-doc) 4)
                                                                                                (list
                                                                                                 (set-version-doc (get-n-version-lista
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

(define set-n-remov-compartido
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
        (if (and (equal? (get-usuario-compartido (car lista-base)) usuario) (equal? (get-acceso-compartido (car lista-base)) #\w))
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

(define remove-get-n-version-lista
  (lambda (lista-versiones version)
    ; ¿La primera lista contenida en la lista de listas posee en su primer elemento el usuario solicitado?
    (if (equal? (get-version-doc (car lista-versiones)) version)
        (remove (car lista-versiones) lista-versiones)
        ; ¿El resto de listas contenidas en la lista de listas es nulo?
        (if (null? (cdr lista-versiones))
            #f
            ; Llamado recursivo contatenando la lista que no corresponde a la buscada (para no perderla) y el llamado con el resto de listas y la versión buscada
            (cons (car lista-versiones) (remove-get-n-version-lista (cdr lista-versiones) version))
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

; Descripción: Función que permite identificar si un cualquiera ingresado string forma parte de del contenido/string ubicado en un documento 
; Dominio: string X lista
; Recorrido: Booleano

(define verificador-de-substring
  (lambda (frase lista-versiones)
    (if (null? lista-versiones)
        #f
        (if (string-contains? (encryptFn (get-contenido (car lista-versiones))) frase)
            #t
            (if (null? (cdr lista-versiones))
                #f
                (verificador-de-substring frase (cdr lista-versiones)))
            )
        )
    )
  )

; Descripción: Función que llama a la función "verificador-de-substring" ya definida recibiendo sus entradas de forma currificada e insertandolas...
;              ... a la función aluda de forma "lineal"
; Dominio: string X lista
; Recorrido: procedure

(define llamado-verificador-de-substring
  (lambda (frase)
    (lambda (documento)
      (verificador-de-substring frase (get-dato-doc documento 4))
      )
    )
  )

; Descripción: Función que permite identificar si un usuario es propietario o figura como usuario compartido en determinado documento
; Dominio: string X lista
; Recorrido: Booleano

(define valido?
  (lambda (usuario)
    (lambda (documento)
      (if (null? documento)
          #f
          (if (or (buscar-usuario-compartido (get-dato-doc documento 5) usuario) (buscar-usuario-propietario documento usuario) )
              #t
              #f)
          )
      )
    )
  )

; Descripción: Función que permite a verificar recursivamente si un usuario determinado se encuentra en la lista de listas contenedoras de usuarios...
;              ... compartidos con un acceso válido
; Dominio: Lista X string
; Recorrido: Booleano
; Tipo de recursión: Recursión natural

(define buscar-usuario-compartido
  (lambda (lista-base usuario)
    ; ¿La lista de listas es nula?
    (if (null? lista-base)
        #f
        ; ¿El primer elemento de la primera lista considerada es equivalente al usuario buscado y su segundo elemento corresponde al permiso y/o acceso de escritura?
        (if (or (and (equal? (get-usuario-compartido (car lista-base)) usuario) (equal? (get-acceso-compartido (car lista-base)) #\r))
                (and (equal? (get-usuario-compartido (car lista-base)) usuario) (equal? (get-acceso-compartido (car lista-base)) #\w)))
                #t
            ; Llamado recursivo retornando la función, resto de las listas y el usuario
            (buscar-usuario-compartido (cdr lista-base) usuario))
        )
    )
  )

; Descripción: Función que permite a verificar si un usuario determinado figura en un documento como propietario
; Dominio: Lista X string
; Recorrido: Booleano

(define buscar-usuario-propietario
  (lambda (documento usuario)
    ; ¿La lista de listas es nula?
    (if (null? documento)
        #f
        ; ¿El primer elemento de la primera lista considerada es equivalente al usuario buscado y su segundo elemento corresponde al permiso y/o acceso de escritura?
        (if (equal? (get-dato-doc documento 1) usuario)
            #t
            #f)
        )
    )
  )

; Descripción: Función que permite convertir un documento a un string considerando como condicional un usuario el cual debe figurar como propietario y/o usuario...
;              ... compartido
; Dominio (currificado): usuario X documento
; Recorrido: string

(define documento->string
  (lambda (usuario)
    (lambda (documento)
      (if (null? documento)
          ""
          (if (or (buscar-usuario-propietario documento usuario) (buscar-usuario-compartido (get-dato-doc documento 5) usuario))
              (string-append "-> ID del Documento: " (number->string (get-dato-doc documento 0)) "\n"
                             "Propietario: " (get-dato-doc documento 1) "\n"
                             "Fecha de creación: " (fecha->string (get-dato-doc documento 2)) "\n"
                             "Nombre del documento: " (get-dato-doc documento 3) "\n"
                             (string-join (versiones->string (get-dato-doc documento 4)))
                             (string-join (compartidos->string (get-dato-doc documento 5))))
                
              "")
          )
      )
    )
  )

; Descripción: Función que permite convertir la información de un documento (versiones, fechas y contenidos de este) a string contenido en una lista...
;              ... (aplicar "string-join" al resultado para expresar el string)
; Dominio: lista de versiones
; Recorrido: lista
; Tipo de recursión: Recursión natural/lineal

(define versiones->string
  (lambda (lista-base-versiones)
    (if (null? lista-base-versiones)
        null
        (cons (string-append "Nro. versión: " (number->string (get-version-doc (get-primera-lista lista-base-versiones))) "\n"
                             "Fecha de creación: " (fecha->string (get-fecha-list-version (get-primera-lista lista-base-versiones))) "\n"
                             "Contenido: " (encryptFn (get-contenido (get-primera-lista lista-base-versiones))) "\n")
            (versiones->string (cdr lista-base-versiones)))
            )
        )
    )

; Descripción: Función que permite convertir la información de un documento (usuarios compartidos y sus accesos) a string contenido en una lista...
;              ... (aplicar "string-join" al resultado para expresar el string)
; Dominio: lista de usuarios compartidos
; Recorrido: lista
; Tipo de recursión: Recursión natural/lineal

(define compartidos->string
  (lambda (lista-base-compartidos)
    (if (null? lista-base-compartidos)
        null
        (cons (string-append "Usuario compartido: " (get-usuario-compartido (get-primera-lista lista-base-compartidos)) "\n"
                             "Acceso concedido: " (string (get-acceso-compartido (get-primera-lista lista-base-compartidos))) "\n")
            (compartidos->string (cdr lista-base-compartidos)))
        )
    )
  )

; Descripción: Función que retorna el documento en caso de que en este el usuario ingresado figure como propietario y/o usuario compartido
; Dominio: string X lista
; Recorrido: list

(define filtrador-propietario-compartido
  (lambda (usuario)
    (lambda (documento)
      (if (or (buscar-usuario-propietario documento usuario) (buscar-usuario-compartido (get-dato-doc documento 5) usuario))
          documento
          null)
      )
    )
  )

; Descripción: Función que permite convertir todos los documentos a string contenido en una lista valiendose de las funciones definidas anteriormente...
;              ... (aplicar "string-join" al resultado para expresar el string)
; Dominio: lista de documentos
; Recorrido: lista de string
; Tipo de recursión: Recursión natural

(define documentos->string
    (lambda (lista-documentos)
      (if (null? lista-documentos)
          null
          (cons (string-append "-> ID del Documento: " (number->string (get-dato-doc (get-primera-lista lista-documentos) 0)) "\n"
                               "Propietario: " (get-dato-doc (get-primera-lista lista-documentos) 1) "\n"
                               "Fecha de creación: " (fecha->string (get-dato-doc (get-primera-lista lista-documentos) 2)) "\n"
                               "Nombre del documento: " (get-dato-doc (get-primera-lista lista-documentos) 3) "\n"
                               (string-join (versiones->string (get-dato-doc (get-primera-lista lista-documentos) 4)))
                               (string-join (compartidos->string (get-dato-doc (get-primera-lista lista-documentos) 5))))
                (documentos->string (cdr lista-documentos)))
          )
      )
  )






;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;EJEMPLOS CONSTRUCTOR:
; Las siguientes funciones son necesarias para la correcta ejecución de los ejemplos. Asimismo, las funciones comentadas se encuentran definidas y exportadas...
; ... desde los TDA_usuarios y TDA_paradigmadocs.
; ...previamente definida la plataforma a emplear: "(define emptyGDocs (paradigmadocs "gDocs" (date 16 10 2021) encryptFn encryptFn))"
; ...adicionalmente: (define ejemplo-crear-usuario-1 (crear-usuario (date 19 10 2021) "Angel" "contraseña"))
; ...adicionalmente: (define ejemplo-crear-usuario-2 (crear-usuario (date 20 10 2021) "Jaime" "pinturaceresita"))
; ...adicionalmente: (define ejemplo-set-act-list-usuarios-paradigmadocs-2 (set-act-list-usuarios-paradigmadocs emptyGDocs ejemplo-crear-usuario-2))
; ...adicionalmente: (define ejemplo-set-act-list-usuarios-paradigmadocs-3 (set-act-list-usuarios-paradigmadocs ejemplo-set-act-list-usuarios-paradigmadocs-2 ejemplo-crear-usuario-1))
; ...adicionalmente:
(define plataforma (set-n-remov "Angel" ejemplo-set-act-list-usuarios-paradigmadocs-3))
; lo anterior es basicamente register (con el usuario "Angel" logeado)

(define ejemplo-crear-documento-1 (crear-documento
                                   (date 25 10 2021) "Gdocs" (buscar-usuario-activo (get-dato plataforma 4)) (ID-version-doc (get-dato plataforma 5) -1) "PRIMER TEXTO" 0))
(define ejemplo-crear-documento-2 (crear-documento
                                 (date 30 12 2021) "DOC" (buscar-usuario-activo (get-dato plataforma 4)) (ID-version-doc (get-dato plataforma 5) 0) "INFORME" 0))
(define ejemplo-crear-documento-3 (crear-documento
                                (date 4 5 2025) "TXT" (buscar-usuario-activo (get-dato ejemplo-set-act-list-usuarios-paradigmadocs-3 4)) (ID-version-doc (get-dato ejemplo-set-act-list-usuarios-paradigmadocs-3 5) 0) "BORRADOR" 0))
; El tercer ejemplo expresa una situación no valida

;EJEMPLOS PERTENENCIA:
(define ejemplo-documento?-1 (documento? ejemplo-crear-documento-1))
(define ejemplo-documento?-2 (documento? ejemplo-crear-documento-2))
(define ejemplo-documento?-3 (documento? ejemplo-crear-documento-3))

;EJEMPLOS SELECTORES:
(define ejemplo-get-dato-doc-1 (get-dato-doc ejemplo-crear-documento-1 0))
(define ejemplo-get-dato-doc-2 (get-dato-doc ejemplo-crear-documento-1 4))
(define ejemplo-get-dato-doc-3 (get-dato-doc ejemplo-crear-documento-2 3))

(define ejemplo-get-version-doc-1 (get-version-doc (get-primera-lista (get-dato-doc ejemplo-crear-documento-1 4))))
(define ejemplo-get-version-doc-2 (get-version-doc (get-primera-lista (get-dato-doc ejemplo-crear-documento-2 4))))
(define ejemplo-get-version-doc-3 (get-version-doc (list 7 (list 25 10 2021) "EJEMPLO DE ARGUMENTO IDEAL")))

(define ejemplo-get-ultima-version-1 (get-ultima-version (list (list 0 (list 25 10 2021) "PRIMERA VERSIÓN")
                                                       (list 1 (list 25 10 2021) "SEGUNDA Y ÚLTIMA VERSIÓN"))))
(define ejemplo-get-ultima-version-2 (get-ultima-version (list (list 0 (list 25 10 2021) "PRIMERA VERSIÓN")
                                                       (list 1 (list 25 10 2021) "SEGUNDA VERSIÓN")
                                                       (list 2 (list 25 10 2021) "TERCERA Y ÚLTIMA VERSIÓN"))))
(define ejemplo-ultima-version-3 (get-ultima-version (list (list 0 (list 25 10 2021) "PRIMERA VERSIÓN")
                                                       (list 1 (list 25 10 2021) "SEGUNDA VERSIÓN")
                                                       (list 2 (list 25 10 2021) "TERCERA VERSIÓN")
                                                       (list 3 (list 25 10 2021) "CUARTA VERSIÓN")
                                                       (list 4 (list 25 10 2021) "QUINTA Y ÚLTIMA VERSIÓN"))))

(define ejemplo-get-n-version-lista-1 (get-n-version-lista (list (list 0 (list 25 10 2021) "PRIMERA VERSIÓN")
                                                       (list 1 (list 25 10 2021) "SEGUNDA VERSIÓN")
                                                       (list 2 (list 25 10 2021) "TERCERA VERSIÓN")
                                                       (list 3 (list 25 10 2021) "CUARTA VERSIÓN")
                                                       (list 4 (list 25 10 2021) "QUINTA Y ÚLTIMA VERSIÓN"))
                                                   0))
(define ejemplo-get-n-version-lista-2 (get-n-version-lista (list (list 0 (list 25 10 2021) "PRIMERA VERSIÓN")
                                                       (list 1 (list 25 10 2021) "SEGUNDA VERSIÓN")
                                                       (list 2 (list 25 10 2021) "TERCERA VERSIÓN")
                                                       (list 3 (list 25 10 2021) "CUARTA VERSIÓN")
                                                       (list 4 (list 25 10 2021) "QUINTA Y ÚLTIMA VERSIÓN"))
                                                   2))
(define ejemplo-get-n-version-lista-3 (get-n-version-lista (list (list 0 (list 25 10 2021) "PRIMERA VERSIÓN")
                                                       (list 1 (list 25 10 2021) "SEGUNDA VERSIÓN")
                                                       (list 2 (list 25 10 2021) "TERCERA VERSIÓN")
                                                       (list 3 (list 25 10 2021) "CUARTA VERSIÓN")
                                                       (list 4 (list 25 10 2021) "QUINTA Y ÚLTIMA VERSIÓN"))
                                                   4))
(define ejemplo-get-usuario-compartido-1 (get-usuario-compartido (list "USUARIO" #\r)))
(define ejemplo-get-usuario-compartido-2 (get-usuario-compartido
                                          (get-primera-lista
                                           (get-dato-doc (list 0 "Angel" (list 25 10 2021) "Gdocs" (list (list 0 (list 25 10 2021) "PRIMER TEXTO")) (list (list "USUARIO" #\r))) 5))))
(define ejemplo-get-usuario-compartido-3 (get-usuario-compartido
                                          (get-primera-lista (list (list "USUARIO" #\r)))))

(define ejemplo-get-acceso-compartido-1 (get-acceso-compartido (list "USUARIO" #\r)))
(define ejemplo-get-acceso-compartido-2 (get-acceso-compartido
                                          (get-primera-lista
                                           (get-dato-doc (list 0 "Angel" (list 25 10 2021) "Gdocs" (list (list 0 (list 25 10 2021) "PRIMER TEXTO")) (list (list "USUARIO" #\r))) 5))))
(define ejemplo-get-acceso-compartido-3 (get-acceso-compartido
                                          (get-primera-lista (list (list "USUARIO" #\r)))))

(define ejemplo-get-contenido-1 (get-contenido (get-primera-lista (get-dato-doc ejemplo-crear-documento-1 4))))
(define ejemplo-get-contenido-2 (get-contenido (get-primera-lista (get-dato-doc ejemplo-crear-documento-2 4))))
(define ejemplo-get-contenido-3 (get-contenido (get-primera-lista (list (list 0 (list 25 10 2021) "PARA OBTENER OTRAS LISTAS SE APLICA RECURSIÓN")))))

(define ejemplo-get-primera-lista-1 (get-primera-lista (list (list 0 (list 25 10 2021) "PRIMERA VERSIÓN")
                                                       (list 1 (list 25 10 2021) "SEGUNDA Y ÚLTIMA VERSIÓN"))))
(define ejemplo-get-primera-lista-2 (get-primera-lista (list (list 0 (list 25 10 2021) "PARA OBTENER OTRAS LISTAS SE APLICA RECURSIÓN"))))
(define ejemplo-get-primera-lista-3 (get-primera-lista (list (list "USUARIO" #\r)
                                                             (list "USUARIO2" #\w))))

(define ejemplo-set-version-doc-1 (set-version-doc (list 0 (list 25 10 2021) "ESTA DEBE SER LA VERSIÓN TRES")
                                                   (ID-version-doc (list
                                                                    (list 0 (list 25 10 2021) "VERSIÓN CERO")
                                                                    (list 1 (list 25 10 2021) "VERSIÓN UNO")
                                                                    (list 2 (list 25 10 2021) "VERSIÓN DOS"))
                                                                   -1)))
(define ejemplo-set-version-doc-2 (set-version-doc (list 0 (list 25 10 2021) "ESTA DEBE SER LA VERSIÓN UNO")
                                                   (ID-version-doc (list
                                                                    (list 0 (list 25 10 2021) "VERSIÓN CERO"))
                                                                   -1)))
(define ejemplo-set-version-doc-3 (set-version-doc (list 0 (list 25 10 2021) "ESTA DEBE SER LA VERSIÓN UNO")
                                                   (ID-version-doc (get-dato-doc ejemplo-crear-documento-1 4)
                                                                   -1)))

(define ejemplo-set-list-compartido-1 (set-list-compartido ejemplo-crear-documento-1))
(define ejemplo-set-list-compartido-2 (set-list-compartido ejemplo-crear-documento-2))
;(define ejemplo-set-list-compartido-3 (set-list-compartido ejemplo-crear-documento-3))
; El tercer ejemplo representa una situación no válida

; EJEMPLO agregar-usuario-compartido
; EJEMPLO agregar-usuario-compartido
; EJEMPLO agregar-usuario-compartido

; EJEMPLO agregar-version-doc
; EJEMPLO agregar-version-doc
; EJEMPLO agregar-version-doc

; EJEMPLO restaurar
; EJEMPLO restaurar
; EJEMPLO restaurar

(define ejemplo-ID-version-doc-1 (ID-version-doc (list
                                                  (list 0 (list 25 10 2021) "VERSIÓN CERO")
                                                  (list 1 (list 25 10 2021) "VERSIÓN UNO")
                                                  (list 2 (list 25 10 2021) "VERSIÓN DOS"))
                                                 -1))
(define ejemplo-ID-version-doc-2 (ID-version-doc (list (list 0 (list 25 10 2021) "VERSIÓN CERO")
                                                       (list 1 (list 25 10 2021) "VERSIÓN UNO")
                                                       (list 2 (list 25 10 2021) "VERSIÓN DOS")
                                                       (list 3 (list 25 10 2021) "VERSIÓN TRES")
                                                       (list 4 (list 25 10 2021) "VERSIÓN CUATRO"))
                                                 -1))
(define ejemplo-ID-version-doc-3 (ID-version-doc (get-dato-doc ejemplo-crear-documento-1 4)
                                                                   -1))
(define ejemplo-access-1 (access (list "USER1" "PASS1")))
(define ejemplo-access-2 (access (list "USER1" "PASS1") (list "USER2" "PASS2")))
(define ejemplo-access-3 (access (list "USER1" "PASS1") (list "USER2" "PASS2") (list "USER3" "PASS3") (list "USER4" "PASS4")))

(define ejemplo-buscar-Id-documento-1 (buscar-Id-documento (list (list 0 (list 25 10 2021) "PRIMERA VERSIÓN")
                                                       (list 1 (list 25 10 2021) "SEGUNDA VERSIÓN")
                                                       (list 2 (list 25 10 2021) "TERCERA VERSIÓN")
                                                       (list 3 (list 25 10 2021) "CUARTA VERSIÓN")
                                                       (list 4 (list 25 10 2021) "QUINTA Y ÚLTIMA VERSIÓN"))
                                                           3))
(define ejemplo-buscar-Id-documento-2 (buscar-Id-documento (list (list 0 (list 25 10 2021) "PRIMERA VERSIÓN")
                                                       (list 1 (list 25 10 2021) "SEGUNDA VERSIÓN")
                                                       (list 2 (list 25 10 2021) "TERCERA VERSIÓN")
                                                       (list 3 (list 25 10 2021) "CUARTA VERSIÓN")
                                                       (list 4 (list 25 10 2021) "QUINTA Y ÚLTIMA VERSIÓN"))
                                                           1))
(define ejemplo-buscar-Id-documento-3 (buscar-Id-documento (list (list 0 (list 25 10 2021) "PRIMERA VERSIÓN")
                                                       (list 1 (list 25 10 2021) "SEGUNDA VERSIÓN")
                                                       (list 2 (list 25 10 2021) "TERCERA VERSIÓN")
                                                       (list 3 (list 25 10 2021) "CUARTA VERSIÓN")
                                                       (list 4 (list 25 10 2021) "QUINTA Y ÚLTIMA VERSIÓN"))
                                                           4))

; EJEMPLO remov-lista-repet
; EJEMPLO remov-lista-repet
; EJEMPLO remov-lista-repet

; EJEMPLO set-n-remov-compartido
; EJEMPLO set-n-remov-compartido
; EJEMPLO set-n-remov-compartido

; EJEMPLO agregar-remover-doc
; EJEMPLO agregar-remover-doc
; EJEMPLO agregar-remover-doc


(define ejemplo-buscar-usuario-editor-1 (buscar-usuario-editor (list (list "USUARIO" #\r)
                                                             (list "USUARIO2" #\w)
                                                             (list "USUARIO3" #\c)
                                                             (list "USUARIO4" #\w))
                                                               "USUARIO2"))
(define ejemplo-buscar-usuario-editor-2 (buscar-usuario-editor (list (list "USUARIO" #\r)
                                                             (list "USUARIO2" #\w)
                                                             (list "USUARIO3" #\c)
                                                             (list "USUARIO4" #\w))
                                                               "USUARIO"))
(define ejemplo-buscar-usuario-editor-3 (buscar-usuario-editor (list (list "USUARIO" #\r)
                                                             (list "USUARIO2" #\w)
                                                             (list "USUARIO3" #\c)
                                                             (list "USUARIO4" #\w))
                                                               "USUARIO4"))

; EJEMPLO remove-get-n-version-lista
; EJEMPLO remove-get-n-version-lista
; EJEMPLO remove-get-n-version-lista

; EJEMPLO agregar-remover-version
; EJEMPLO agregar-remover-version
; EJEMPLO agregar-remover-version

(define ejemplo-correlativo-1 (correlativo (list (list 1 (list 25 10 2021) "PRIMERA VERSIÓN")
                                                       (list 1 (list 25 10 2021) "SEGUNDA VERSIÓN")
                                                       (list 5 (list 25 10 2021) "TERCERA VERSIÓN")
                                                       (list 0 (list 25 10 2021) "CUARTA VERSIÓN")
                                                       (list 1 (list 25 10 2021) "QUINTA Y ÚLTIMA VERSIÓN"))
                                           0
                                           (list )))
(define ejemplo-correlativo-2 (correlativo (list (list -1 (list 25 10 2021) "PRIMERA VERSIÓN")
                                                       (list -2 (list 25 10 2021) "SEGUNDA VERSIÓN")
                                                       (list -3 (list 25 10 2021) "TERCERA VERSIÓN")
                                                       (list -4 (list 25 10 2021) "CUARTA VERSIÓN")
                                                       (list -5 (list 25 10 2021) "QUINTA Y ÚLTIMA VERSIÓN"))
                                           0
                                           (list )))
(define ejemplo-correlativo-3 (correlativo (list (list 1000 (list 25 10 2021) "PRIMERA VERSIÓN")
                                                       (list 115 (list 25 10 2021) "SEGUNDA VERSIÓN")
                                                       (list 550 (list 25 10 2021) "TERCERA VERSIÓN")
                                                       (list 0 (list 25 10 2021) "CUARTA VERSIÓN")
                                                       (list 19 (list 25 10 2021) "QUINTA Y ÚLTIMA VERSIÓN"))
                                           0
                                           (list )))

(define ejemplo-filtrador-doc-1 (filtrador-doc (list ejemplo-crear-documento-1 ejemplo-crear-documento-2) "Angel"))
(define ejemplo-filtrador-doc-2 (filtrador-doc (list ejemplo-crear-documento-1 ejemplo-crear-documento-2) "Benjamin"))
(define ejemplo-filtrador-doc-3 (filtrador-doc (list ejemplo-crear-documento-1 ejemplo-crear-documento-2) "ANGEL"))

(define ejemplo-concatenador-1 (concatenador (list ejemplo-crear-documento-1 ejemplo-crear-documento-2)
                                             (filtrador-doc (list ejemplo-crear-documento-1 ejemplo-crear-documento-2) "Angel")))
(define ejemplo-concatenador-2 (concatenador (list ejemplo-crear-documento-1 ejemplo-crear-documento-2)
                                             (filtrador-doc (list ejemplo-crear-documento-1 ejemplo-crear-documento-2) "Benjamin")))
(define ejemplo-concatenador-3 (concatenador (list ejemplo-crear-documento-1 ejemplo-crear-documento-2)
                                             (map set-list-compartido
                                                  (filtrador-doc (list ejemplo-crear-documento-1 ejemplo-crear-documento-2) "Angel"))))

(define ejemplo-verificador-de-substring-1 (verificador-de-substring "PRIMERA"
                                                                     (list (list 0 (list 25 10 2021) (encryptFn "PRIMERA VERSIÓN"))
                                                                           (list 1 (list 25 10 2021) (encryptFn "SEGUNDA VERSIÓN"))
                                                                           (list 2 (list 25 10 2021) (encryptFn "TERCERA VERSIÓN"))
                                                                           (list 3 (list 25 10 2021) (encryptFn "CUARTA VERSIÓN"))
                                                                           (list 4 (list 25 10 2021) (encryptFn "QUINTA Y ÚLTIMA VERSIÓN")))
                                                                     ))
(define ejemplo-verificador-de-substring-2 (verificador-de-substring "QUINTA"
                                                                     (list (list 0 (list 25 10 2021) (encryptFn "PRIMERA VERSIÓN"))
                                                                           (list 1 (list 25 10 2021) (encryptFn "SEGUNDA VERSIÓN"))
                                                                           (list 2 (list 25 10 2021) (encryptFn "TERCERA VERSIÓN"))
                                                                           (list 3 (list 25 10 2021) (encryptFn "CUARTA VERSIÓN"))
                                                                           (list 4 (list 25 10 2021) (encryptFn "QUINTA Y ÚLTIMA VERSIÓN")))
                                                                     ))
(define ejemplo-verificador-de-substring-3 (verificador-de-substring "FRASE"
                                                                     (list (list 0 (list 25 10 2021) (encryptFn "PRIMERA VERSIÓN"))
                                                                           (list 1 (list 25 10 2021) (encryptFn "SEGUNDA VERSIÓN"))
                                                                           (list 2 (list 25 10 2021) (encryptFn "TERCERA VERSIÓN"))
                                                                           (list 3 (list 25 10 2021) (encryptFn "CUARTA VERSIÓN"))
                                                                           (list 4 (list 25 10 2021) (encryptFn "QUINTA Y ÚLTIMA VERSIÓN")))
                                                                     ))

(define ejemplo-llamado-verificador-de-substring-1 ((llamado-verificador-de-substring "PRIMERA") ejemplo-crear-documento-1))
(define ejemplo-llamado-verificador-de-substring-2 ((llamado-verificador-de-substring "IRP") ejemplo-crear-documento-1)) ;RECORDAR EXISTENCIA DE ENCRYPTFN
(define ejemplo-llamado-verificador-de-substring-3 ((llamado-verificador-de-substring "XET") ejemplo-crear-documento-1)) ;RECORDAR EXISTENCIA DE ENCRYPTFN
#|
(define ejemplo-llamado-valido?-1 ((llamado-valido? "ANGEL") ejemplo-crear-documento-1))
(define ejemplo-llamado-valido?-2 ((llamado-valido? "Angel") ejemplo-crear-documento-1))
(define ejemplo-llamado-valido?-3 ((llamado-valido? "USER") ejemplo-crear-documento-1))

(define ejemplo-valido?-1 (valido? "ANGEL" ejemplo-crear-documento-1))
(define ejemplo-valido?-2 (valido? "Angel" ejemplo-crear-documento-1))
(define ejemplo-valido?-3 (valido? "USER" ejemplo-crear-documento-1))
|#
(define ejemplo-buscar-usuario-compartido-1 (buscar-usuario-compartido (list (list "USUARIO" #\r)
                                                                             (list "USUARIO2" #\w)
                                                                             (list "USUARIO3" #\c)
                                                                             (list "USUARIO4" #\w))
                                                                       "USUARIO3"))
(define ejemplo-buscar-usuario-compartido-2 (buscar-usuario-compartido (list (list "USUARIO" #\r)
                                                                             (list "USUARIO2" #\w)
                                                                             (list "USUARIO3" #\c)
                                                                             (list "USUARIO4" #\w))
                                                                       "USUARIO4"))
(define ejemplo-buscar-usuario-compartido-3 (buscar-usuario-compartido (list (list "USUARIO" #\r)
                                                                             (list "USUARIO2" #\w)
                                                                             (list "USUARIO3" #\c)
                                                                             (list "USUARIO4" "ESCRIBIR"))
                                                                       "USUARIO4"))

(define ejemplo-buscar-usuario-propietario-1 (buscar-usuario-propietario ejemplo-crear-documento-1 "Angel"))
(define ejemplo-buscar-usuario-propietario-2 (buscar-usuario-propietario ejemplo-crear-documento-1 "Cale"))
(define ejemplo-buscar-usuario-propietario-3 (buscar-usuario-propietario ejemplo-crear-documento-1 "A"))

(define ejemplo-documento->string-1 ((documento->string "user1") (list 4 "user1" (list 30 12 2021) "gDocs" (list (list 0 (list 30 12 2021) "CONTENIDO")) (list ))))
(define ejemplo-documento->string-2 ((documento->string "Angel") ejemplo-crear-documento-1))
(define ejemplo-documento->string-3 ((documento->string "Jaime") ejemplo-crear-documento-2))

(define ejemplo-versiones->string-1 (versiones->string (list (list 0 (list 25 10 2021) (encryptFn "PRIMERA VERSIÓN"))
                                                             (list 1 (list 25 10 2021) (encryptFn "SEGUNDA VERSIÓN"))
                                                             )))
(define ejemplo-versiones->string-2 (versiones->string (list (list 0 (list 25 10 2021) (encryptFn "PRIMERA VERSIÓN"))
                                                                           (list 1 (list 25 10 2021) (encryptFn "SEGUNDA VERSIÓN"))
                                                                           (list 2 (list 25 10 2021) (encryptFn "TERCERA VERSIÓN"))
                                                                           (list 3 (list 25 10 2021) (encryptFn "CUARTA VERSIÓN"))
                                                                           )))
(define ejemplo-versiones->string-3 (versiones->string (list (list 0 (list 25 10 2021) (encryptFn "PRIMERA VERSIÓN"))
                                                                           (list 1 (list 25 10 2021) (encryptFn "SEGUNDA VERSIÓN"))
                                                                           (list 2 (list 25 10 2021) (encryptFn "TERCERA VERSIÓN"))
                                                                           (list 3 (list 25 10 2021) (encryptFn "CUARTA VERSIÓN"))
                                                                           (list 4 (list 25 10 2021) (encryptFn "QUINTA Y ÚLTIMA VERSIÓN")))))








;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(provide (all-defined-out))
