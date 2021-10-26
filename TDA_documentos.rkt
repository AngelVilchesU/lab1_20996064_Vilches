#lang racket

(require "TDA_fecha.rkt")

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
  (lambda (dia mes año nombre-documento usuario ID-documento)
    (if (fecha? (crear-fecha dia mes año))
        (list ID-documento usuario (crear-fecha dia mes año) nombre-documento '() '() )
        '()
        )
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

(define get-lista
  (lambda (lista-documentos n)
    (if (= n 0)
        (car lista-documentos)
        (get-lista (cdr lista-documentos) (- n 1))
        )
    )
  )

; Otras funciones:
; Descripción: Función que permite insertar una ID del documento considerando el número de documentos distintos
; Dominio: Lista de listas
; Recorrido: integer

(define ID-documento
  (lambda (lista-documentos ID-acc)
    (if (null? lista-documentos)
        (add1 ID-acc)
        (ID-documento (cdr lista-documentos) (add1 ID-acc))
        )
    )
  )

#|
(define versión-documento
  (lambda ))
|#
#|

;EJEMPLOS CONSTRUCTOR:






(define doc (crear-documento 25 10 2021 "doc" "ANGEL" (ID-documento '( '() '() ) 0)))



|#