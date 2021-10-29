#lang racket

(require "TDA_fecha.rkt")

; Implementación del TDA paradigmadocs

; Representación:
; (string X date X EncryptFunction X DecryptFunction X (<<<TDA-usuarios>>>) X (<<<TDA-documentos>>>))
; (list nombre-plataforma dia mes año EncryptFunction DecryptFunction '() '() )

; Contructor:
; Descripción: Permite crear la plataforma que contendrá datos referentes a distintos TDA a implementar. Algunos de estos son...
;              ... usuarios y su información, sesión activa, documentos, entre otras. 
; Dominio: (string X integer X integer X integer X EncryptFunction X DecryptFunction)
; Recorrido: (string X integer X integer X integer X EncryptFunction X DecryptFunction X '() '() )

(define paradigmadocs
  (lambda (nombre dia mes año EncryptFunction DecryptFunction)
    (if (paradigmadocs? (list nombre (crear-fecha dia mes año) EncryptFunction DecryptFunction '() '() ))
        (list nombre (crear-fecha dia mes año) EncryptFunction DecryptFunction '() '())
        null)
    )
  )

; Pertenencia:
; Descripción: Función que permite verificar si una plataforma paradigmadocs se encuentra bien definida inicialmente
; Dominio: Lista correspondiente a paradigmadocs
; Recorrido: Booleando verificador

(define paradigmadocs?
  (lambda (paradigmadocs)
    (if (list? paradigmadocs)
        (if (= (length paradigmadocs) 6)
            (if (string? (car paradigmadocs))
                (if (fecha? (car (cdr paradigmadocs)))
                    (if (and (procedure? (car (cdr (cdr paradigmadocs)))) (procedure? (car (cdr (cdr (cdr paradigmadocs))))))
                        #t
                        #f)
                    #f)
                #f)
            #f)
        #f)
    )
  )

; Selectores:
; Descripición: Capa que permite la obtención de determinado elemento en paradigmadocs
; Dominio: Lista
; Recorrido: Dato especifíco
; Recursión: Recursión de cola cuyo caso base contempla la entrega del primer elemento de la lista ingresada en caso de que el valor...
;            ... correspondiente a la posición buscada sea cero, caso contrario, se llama a la función empleando "cdr" de modo que...
;            ... se obtiene lo que puede considerarse como el resto de la lista restando un digito al n ingresado para "calibrar"...
;            ... la busqueda. El proceso se repite hasta conseguír dicho elemento.

(define get-dato
  (lambda (paradigmadocs n)
    (if (= n 0)
        (car paradigmadocs)
        (get-dato (cdr paradigmadocs) (- n 1))     )
    )
  )

; Modificador 
; Descripción: Permite la inserción de un elemento/lista en el final de la plataforma paradigmadocs
; Dominio: Lista
; Recorrido Lista "actualizada" (con el elemento/lista insertado)
; Recursión: Recursión natural/lineal cuyo caso base considera la no existencia de elementos en la lista entragada (null)...
;            ... de modo que de ser así se anexa el elemento a insertar mediante "cons". Caso contrario, anexado el...
;            ... primer elemento de la lista original se llama a la función recursivamente otorgando el "resto" de la lista...
;            ... y su correspondiente elemento

(define set-elemento
  (lambda (paradigmadocs elemento)
    (if (null? paradigmadocs)
        (cons elemento null)
        (cons (car paradigmadocs) (set-elemento (cdr paradigmadocs) elemento))
        )))

; Otras funciones:
; Descripción
; Dominio:
; Recorrido:

(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))


#|
--------------------------------------------------------------------------------------------------------------------------------------------
;EJEMPLOS CONSTRUCTOR:
(define emptyGDocs (paradigmadocs "gDocs" 16 10 2021 encryptFn encryptFn))
(define emptyGDocs-2 (paradigmadocs "gWord" 17 10 2021 encryptFn encryptFn))
(define emptyGDocs-3 (paradigmadocs "gTXT" 18 13 2021 encryptFn encryptFn))
; El tercer ejemplo representa una situación no valida

;EJEMPLOS PERTENENCIA:
(define ejemplo-paradigmadocs?-1 (paradigmadocs? emptyGDocs))
(define ejemplo-paradigmadocs?-2 (paradigmadocs? emptyGDocs-2))
(define ejemplo-paradigmadocs?-3 (paradigmadocs? emptyGDocs-3))
; El tercer ejemplo representa una situación no valida

;EJEMPLOS SELECTOR:
(define ejemplo-get-dato-1 (get-dato emptyGDocs 0))
(define ejemplo-get-dato-2 (get-dato emptyGDocs 1))
(define ejemplo-get-dato-3 (get-dato emptyGDocs-2 5))

;EJEMPLOS MODIFICADOR:
; Es importante comentar que el modificador de paradigmadocs no será utilizado pues no es necesario según la representación...
; ... cambiar los elementos inicialmente definidos en la plataforma
--------------------------------------------------------------------------------------------------------------------------------------------
|#

(provide (all-defined-out))  