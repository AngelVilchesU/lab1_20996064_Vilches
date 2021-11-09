#lang racket

(require "TDA_fecha.rkt")

; Implementación del TDA paradigmadocs

; Representación:
; (string X date X EncryptFunction X DecryptFunction X (<<<TDA-usuarios>>>) X (<<<TDA-documentos>>>))
; (list nombre-plataforma (dia mes año) EncryptFunction DecryptFunction '( ) '( ) )

; Contructor:
; Descripción: Permite crear la plataforma que contendrá datos referentes a distintos TDA a implementar. Algunos de estos son...
;              ... usuarios y su información, sesión activa, documentos, entre otras. 
; Dominio: (string X (integer X integer X integer) X EncryptFunction X DecryptFunction)
; Recorrido: (string X (integer X integer X integer) X EncryptFunction X DecryptFunction X '() '() )

(define paradigmadocs
  (lambda (nombre fecha EncryptFunction DecryptFunction)
    (if (paradigmadocs? (list nombre fecha EncryptFunction DecryptFunction '() '() ))
        (list nombre fecha EncryptFunction DecryptFunction '() '())
        null)
    )
  )

; Pertenencia:
; Descripción: Función que permite verificar si una plataforma paradigmadocs se encuentra bien definida inicialmente
; Dominio: Lista correspondiente a paradigmadocs
; Recorrido: Booleando verificador

(define paradigmadocs?
  (lambda (paradigmadocs)
    ; ¿Es lista?
    (if (list? paradigmadocs)
        ; ¿La longitud de la lista es de 6 elementos?
        (if (= (length paradigmadocs) 6)
            ; ¿El primer elemento es un string?
            (if (string? (car paradigmadocs))
                ; ¿El segundo elemento es del tipo TDA fecha?
                (if (fecha? (car (cdr paradigmadocs)))
                    ; ¿El tercer y cuarto elemento son operaciones/procedimientos?
                    (if (and (procedure? (car (cdr (cdr paradigmadocs)))) (procedure? (car (cdr (cdr (cdr paradigmadocs))))))
                        #t ; Si cumple con todo lo anterior...
                        #f)
                    #f)
                #f)
            #f)
        #f)
    )
  )

; Selectores:
; Descripición: Capa que permite la obtención de determinado elemento en paradigmadocs
; Dominio: paradigmadocs X integer
; Recorrido: Dato especifíco
; Recursión: Recursión de cola cuyo caso base contempla la entrega del primer elemento de la lista ingresada en caso de que el valor...
;            ... correspondiente a la posición buscada sea cero, caso contrario, se llama a la función empleando "cdr" de modo que...
;            ... se obtiene lo que puede considerarse como el resto de la lista restando un digito al n ingresado para "calibrar"...
;            ... la busqueda. El proceso se repite hasta conseguír dicho elemento.

(define get-dato
  (lambda (paradigmadocs n)
    ; ¿El elemento a buscar corresponde a la ubicación 0?
    (if (= n 0)
        ; Si se cumple lo anterior retorna el primer elemento de la lista
        (car paradigmadocs)
        ; Caso contrario y/o recursivo, se retorna el resto de la lista/paradigmadocs y se resta en uno el valor de la ubicación buscada
        (get-dato (cdr paradigmadocs) (- n 1)))
    )
  )

; Modificador 
; Descripción: Permite la inserción de un elemento cualquiera mediante la implementación de funciones que permitan realizar algún cambio...
;              ... en el sexto elemento (sin contar el cero) retornando todos los datos de la plataforma sin modificar a excepción de...
;              ... la lista en donde se ubica el TDA-documentos
; Dominio: paradigmadocs X lista actualizada de TDA-documentos
; Recorrido Lista "actualizada" (con la lista insertada)

(define set-act-list-doc-paradigmadocs
  (lambda (paradigmadocs act-docs)
    ; Se génera una lista que contiene los datos de la plataforma sin modificar a excepción de la ubicada en la sexta posición de la misma
    (list (get-dato paradigmadocs 0)
          (get-dato paradigmadocs 1)
          (get-dato paradigmadocs 2)
          (get-dato paradigmadocs 3)
          (get-dato paradigmadocs 4)
          act-docs)
    )
  )

; Descripción: Función que permite modificar la lista de usuarios creada en la plataforma diseñada en la quinta posición (sin contar el cero)...
;              ... recibiendo como parámetros de entrada la plataforma y la lista contenedora de la información de un nuevo usuario para, en paralelo...
;              ... con la función denominada "anexar-listas" insertarla dentro como lista obteniendo a nivel de plataforma en...
;              ... la posición número 5 una lista de listas correspondiente a la información de un usuario
; Dominio: paradigmadocs X lista
; Recorrido: Actualización de paradigmadocs

(define set-act-list-usuarios-paradigmadocs
  (lambda (plataforma lista-info-usuarios)
    (list (get-dato plataforma 0)
          (get-dato plataforma 1)
          (get-dato plataforma 2)
          (get-dato plataforma 3)
          (anexar-listas (get-dato plataforma 4)
                                 lista-info-usuarios)
          (get-dato plataforma 5))
    )
  )

; Otras funciones:

; Descripción: Función que permite unir dos listas mediante la función "append"
; Dominio: lista X nueva-lista
; Recorrido: Unión de listas como listas de listas

(define anexar-listas
  (lambda (lista nueva-lista)
    (append lista (list nueva-lista))
    )
  )

; Descripción
; Dominio:
; Recorrido:

(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))

;--------------------------------------------------------------------------------------------------------------------------------------------
;EJEMPLOS CONSTRUCTOR:
(define ejemplo-paradigmadocs (paradigmadocs "gDocs" (crear-fecha 16 10 2021) encryptFn encryptFn))
(define ejemplo-paradigmadocs-2 (paradigmadocs "gWord" (crear-fecha 17 10 2021) encryptFn encryptFn))
(define ejemplo-paradigmadocs-3 (paradigmadocs "gTXT" (crear-fecha 18 13 2021) encryptFn encryptFn))
; El tercer ejemplo representa una situación no valida

;EJEMPLOS PERTENENCIA:
(define ejemplo-paradigmadocs?-1 (paradigmadocs? ejemplo-paradigmadocs))
(define ejemplo-paradigmadocs?-2 (paradigmadocs? ejemplo-paradigmadocs-2))
(define ejemplo-paradigmadocs?-3 (paradigmadocs? ejemplo-paradigmadocs-3))
; El tercer ejemplo representa una situación no valida

;EJEMPLOS SELECTOR:
(define ejemplo-get-dato-1 (get-dato ejemplo-paradigmadocs 0))
(define ejemplo-get-dato-2 (get-dato ejemplo-paradigmadocs 1))
(define ejemplo-get-dato-3 (get-dato ejemplo-paradigmadocs-2 5))

;EJEMPLOS MODIFICADOR:
(define ejemplo-set-act-list-doc-paradigmadocs-1 (set-act-list-doc-paradigmadocs ejemplo-paradigmadocs (list "modificación del TDA documentos. EJ: creación de documento")))
(define ejemplo-set-act-list-doc-paradigmadocs-2 (set-act-list-doc-paradigmadocs ejemplo-paradigmadocs (list "modificación del TDA documentos. EJ: otra creación de documento")))
(define ejemplo-set-act-list-doc-paradigmadocs-3 (set-act-list-doc-paradigmadocs ejemplo-paradigmadocs-2 (list "modificación del TDA documentos. EJ: revocar accesos")))

(define ej-set-act-list-usuarios-paradigmadocs-1 (set-act-list-usuarios-paradigmadocs ejemplo-paradigmadocs (list "Entrada de un usuario de un usuario")))
(define ej-set-act-list-usuarios-paradigmadocs-2 (set-act-list-usuarios-paradigmadocs ejemplo-paradigmadocs (list "Salida de un usuario")))
(define ej-set-act-list-usuarios-paradigmadocs-3 (set-act-list-usuarios-paradigmadocs ejemplo-paradigmadocs-2 (list "Log de un usuario")))

(define ejemplo-anexar-listas-1 (anexar-listas (list (list "lista base")) (list "nueva lista")))
(define ejemplo-anexar-listas-2 (anexar-listas (list (list 0 1 2)) (list 3 4 5)))
(define ejemplo-anexar-listas-3 (anexar-listas (list (list "user1" "pass1" "activo/inactivo")) (list "user2" "pass2" "activo/inactivo")))
;--------------------------------------------------------------------------------------------------------------------------------------------


(provide (all-defined-out))  