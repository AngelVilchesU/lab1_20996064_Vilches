#lang racket

(require "TDA_fecha.rkt")

; Implementación del TDA paradigmadocs

; Representación:
; (string X date X EncryptFunction X DecryptFunction X (<<<TDA-usuarios>>>) X (<<<TDA-documentos>>>))
; (list nombre-plataforma dia mes año EncryptFunction DecryptFunction ((dia mes año) usuario/s contraseña/s activo-inactivo ID) () ) ;°°°°°°°°°°ACTUALIZAR

; Contructor:
; Descripción: Permite crear la plataforma que contendrá datos referentes a distintos TDA a implementar. Algunos de estos son...
;              ... usuarios y su información, sesión activa, documentos, entre otras. 
; Dominio: (string X integer X integer X integer X EncryptFunction X DecryptFunction)
; Recorrido: (string X integer X integer X integer X EncryptFunction X DecryptFunction X '() '() )

(define paradigmadocs
  (lambda (nombre dia mes año EncryptFunction DecryptFunction)
   (list nombre (crear-fecha dia mes año) EncryptFunction DecryptFunction '() '()
  )))

; Pertenencia:
; Descripción: Función que permite verificar si una plataforma paradigmadocs se encuentra bien definida inicialmente
; Dominio: Lista correspondiente a paradigmadocs
; Recorrido: Booleando verificador

(define paradigmadocs?
  (lambda (paradigmadocs)
    (if (string? (car paradigmadocs))
        (if (and (integer? (car (cdr paradigmadocs))) (integer? (car (cdr (cdr paradigmadocs)))) (integer? (car (cdr (cdr (cdr paradigmadocs))))))
            (if (and (procedure? (car (cdr (cdr (cdr (cdr paradigmadocs)))))) (procedure? (car (cdr (cdr (cdr (cdr (cdr paradigmadocs)))))))) ;REVISAR CUANDO LA FUNCIÓN encrypt esté REALIZADA
                #t
                #f)
            #f)
        #f)))

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
        (get-dato (cdr paradigmadocs) (- n 1))
        )))

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



#|
;EJEMPLOS CONSTRUCTOR:
(paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") ; Las funciones "encryptFn" deben ser llamadas sin comillas (procedimiento) una vez realizada la función enriptadora
(paradigmadocs "gWord" 17 10 2021 "encryptFn" "encryptFn") ;
(paradigmadocs "gTXT" 18 10 2021 "encryptFn" "encryptFn") ;

;EJEMPLOS PERTENENCIA:
PENDIENTE HASTA LA REALIZACIÓN DE LA FUNCIÓN ENCRIPTADORA, sin emabargo, es funcional

;EJEMPLOS SELECTOR:
(get-dato '("gDocs" (16 10 2021) "encryptFn" "encryptFn" () ()) 0)
(get-dato '("gWord" (17 10 2021) "encryptFn" "encryptFn" () ()) 1)
(get-dato '("gTXT" (18 10 2021) "encryptFn" "encryptFn" () ()) 5)

;EJEMPLOS MODIFICADOR:
(set-elemento (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") "algún string o entero")
(set-elemento (paradigmadocs "gWord" 17 10 2021 "encryptFn" "encryptFn") "alguna función (sin comillas)")
(set-elemento (paradigmadocs "gTXT" 18 10 2021 "encryptFn" "encryptFn") (list))
|#

(provide (all-defined-out))  