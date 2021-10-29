#lang racket

(require "TDA_fecha.rkt")
(require "TDA_paradigmadocs.rkt")
(require "TDA_usuarios.rkt")

; Implementación del TDA documento

; Representación
; (integer X string X date X string X ( (string access) (...) ) X ( (integer X date X string) (...) ) )
; (list ID-documento "AUTOR" (DIA MES AÑO) "NOMBRE-DOCUMENTO" ( ("USUARIO-COMPARTIDO" TIPO-DE-ACCESO) (...) )  ( (VERSIÓN-DOCUMENTO (DIA MES AÑO) "CONTENIDO-DOCUMENTO") (...) ))

; Constructor:
; Descripción: Permite la creación de un documento (lista) nuevo mediante la solicitud de los datos...
;              ...fecha, nombre de documento y contenido
; Dominio: (dia X mes X año X string X string X string X FN-ID-documento)
; Recorrido: Lista con los datos como en la representación
; Importante: ID-documento, "AUTOR" en paralelo con listas inicialmente vacias correspondientes a contenedores de usuarios compartidos y versiones es ingresado automáticamente...
;             ... y estas listas aludidas serán operadas según se ejecute determinada función u operación. Adicionalmente, "FN-ID-documento" es ingresado automáticamente

(define crear-documento
  (lambda (dia mes año nombre-documento usuario ID-documento contenido versión-documento)
    (if (documento? (list ID-documento usuario (crear-fecha dia mes año) nombre-documento (list (list versión-documento (crear-fecha dia mes año) contenido )) '() ))
        (list ID-documento usuario (crear-fecha dia mes año) nombre-documento (list (list versión-documento (crear-fecha dia mes año) contenido )) '() )
        null)
    )
  )

; Pertenencia:
; Descripción: Función que permite verificar si un documento se encuentra bien definido
; Dominio: Lista correspondiente a la información de un documento
; Recorrido: Booleando verificador

(define documento?
  (lambda (lista-documento)
    (if (integer? (car lista-documento))
        (if (string? (car (cdr lista-documento)))
            (if (fecha? (car (cdr (cdr lista-documento))))
                (if (string? (car (cdr (cdr (cdr lista-documento)))))
                    (if (and (list? (car (cdr (cdr (cdr (cdr lista-documento)))))) (list? (car (cdr (cdr (cdr (cdr (cdr lista-documento))))))))
                        #t
                        #f)
                    #f)
                #f)
            #f)
        #f)
    )
  )

; Selectores:
; Descripición: Capa que permite la obtención de una determinada lista en la lista de documentos
; Dominio: Lista
; Recorrido: Lista especifíca
; Recursión: Recursión de cola cuyo caso base contempla la entrega del primer elemento de la lista ingresada en caso de que el valor...
;            ... correspondiente a la posición buscada sea cero, caso contrario, se llama a la función empleando "cdr" de modo que...
;            ... se obtiene lo que puede considerarse como el resto de la lista restando un digito al n ingresado para "calibrar"...
;            ... la busqueda. El proceso se repite hasta conseguír dicho elemento.

(define get-dato-doc
  (lambda (lista-documentos n)
    (if (= n 0)
        (car lista-documentos)
        (get-dato-doc (cdr lista-documentos) (- n 1))
        )
    )
  )

; Modificador:
; Descripción: Función que permite insertar una lista mediante la obtención de una lista de listas (existen dos)
; Dominio: Lista base y lista a agregar
; Recorrido: Lista con datos actualizados

(define set-lista
  (lambda (lista-base lista-agregar)
    (append lista-base (list lista-agregar)
            )
    )
  )


; Otras funciones:
; Descripción: Función que permite insertar una ID del documento considerando el número de documentos distintos
; Dominio: Lista de listas
; Recorrido: integer
; Tipo de recursión: Recursión de cola (sin estados pendientes)

(define ID-documento
  (lambda (lista-documentos ID-acc)
    (if (null? lista-documentos)
        (add1 ID-acc)
        (ID-documento (cdr lista-documentos) (add1 ID-acc))
        )
    )
  )

; Descripción: Función que permite insertar una versión del documento de acuerdo con el número de listas existentes (representado las versiones)...
;              ... según la representación realizada
; Dominio: Lista de versiones y un acumulador (inicialmente siempre cero)
; Recorrido: integer
; Tipo de recursión: Recursión de cola (sin estados pendientes)

(define versión-documento
  (lambda (lista-base V-acc)
    (if (null? lista-base)
        (add1 V-acc)
        (versión-documento (cdr lista-base) (add1 V-acc))
        )
    )
  )

#|

;EJEMPLOS CONSTRUCTOR:
; ...previamente definida la plataforma a emplear: "(define emptyGDocs (paradigmadocs "gDocs" 16 10 2021 encryptFn encryptFn))"
; ...adicionalmente: (define ejemplo-crear-usuario-1 (crear-usuario 19 10 2021 "Angel" "contraseña"))
; ...adicionalmente: (define ejemplo-crear-usuario-2 (crear-usuario 20 10 2021 "Jaime" "pinturaceresita"))
; ...adicionalmente: (define ejemplo-modificar-lista-usuarios-2 (modificar-lista-usuarios emptyGDocs ejemplo-crear-usuario-2))
; ...adicionalmente: (define ejemplo-modificar-lista-usuarios-3 (modificar-lista-usuarios ejemplo-modificar-lista-usuarios-2 ejemplo-crear-usuario-1))
; ...adicionalmente: (define paradigmadocs (agregar-y-remover "Angel" ejemplo-modificar-lista-usuarios-3))
; lo anterior es basicamente register

(define ejemplo-crear-documento-1 (crear-documento
                                   25 10 2021 "Gdocs" (buscar-usuario-activo (get-dato paradigmadocs 4)) (ID-documento (get-dato paradigmadocs 5) 0) "PRIMER TEXTO" 0))
(define ejemplo-crear-documento-2 (crear-documento
                                   30 12 2021 "DOC" (buscar-usuario-activo (get-dato paradigmadocs 4)) (ID-documento (get-dato paradigmadocs 5) 0) "INFORME" 0))
(define ejemplo-crear-documento-3 (crear-documento
                                   4 5 2025 "TXT" (buscar-usuario-activo (get-dato ejemplo-modificar-lista-usuarios-3 4)) (ID-documento (get-dato ejemplo-modificar-lista-usuarios-3 5) 0) "BORRADOR" 0))
; El tercer ejemplo expresa una situación no valida

;EJEMPLOS PERTENENCIA:
(define ejemplo-documento?-1 (documento? ejemplo-crear-documento-1))
(define ejemplo-documento?-2 (documento? ejemplo-crear-documento-2))
(define ejemplo-documento?-3 (documento? ejemplo-crear-documento-3))

;EJEMPLOS 

|#

(provide (all-defined-out))