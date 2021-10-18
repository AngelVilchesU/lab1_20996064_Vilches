#lang racket

; Implementación del TDA paradigmadocs

; Representación:
; (string X date X EncryptFunction X DecryptFunction X (string X string X date) X (string X integer))
; (list nombre-plataforma dia mes año EncryptFunction DecryptFunction (usuario/s contraseña/s dia mes año) (usuario activo/inactivo)

; Contructor:
; Descripción: Permite crear la plataforma que contendrá datos referentes a distintos TDA a implementar. Algunos de estos son...
;              ... usuarios y su información, sesión activa, documentos, entre otras. 
; Dominio: (string X integer X integer X integer X EncryptFunction X DecryptFunction)
; Recorrido: (string X integer X integer X integer X EncryptFunction X DecryptFunction X (string X string X integer X integer X integer)...
;             ... X (string X integer))
; Adicional: Lo citado: "(string X string X integer X integer X integer)", corresponde a listas inicialmente nulas que contemplarán los usuarios,...
;            ... contraseñas y fechas de creación. Del mismo modo, "(string X integer)" corresponde al usuario y si se encuentra activo o no.

(define paradigmadocs
  (lambda (nombre dia mes año EncryptFunction DecryptFunction)
   (list nombre dia mes año EncryptFunction DecryptFunction '() '()
  )))

; Selectores:
; Descripición: Capa que permite la obtención de determinado elemento en paradigmadocs
; Dominio: Lista
; Recorrido: Dato especifíco
; Recursión: Recursión de cola cuyo caso base contempla la entrega del primer elemento de la lista ingresada en caso de que el valor...
;            ... correspondiente a la posición buscada sea cero, caso contrario, se llama a la función empleando "cdr" de modo que...
;            ... se obtiene lo que puede considerarse como el resto de la lista restando un digito al n ingresado para "calibrar"...
;            ... la busqueda. El proceso se repite hasta conseguír dicho elemento, no sin antes evalúar mediante un condicional si...
;            ... n es menor al largo de la lista ingresado, en cuyo caso no es posible conseguír un elemento (#f).

(define get-dato
  (lambda (paradigmadocs n)
    (if (< n (length paradigmadocs))
        (if (= n 0)
            (car paradigmadocs)
            (get-dato (cdr paradigmadocs) (- n 1)))
        #f
        )))

; Modificador 
; Descripción: Permite 

#|
(define set-usuario-contraseña-fecha
  (lambda (paradigmadocs usuario contraseña dia mes año)
    (if (null? (get-dato paradigmadocs 6))
        (list usuario contraseña dia mes año)
        (list (car paradigmadocs) (set-usuario-contraseña-fecha (cdr paradigmadocs) set-usuario-contraseña-fecha))
        )))
|#
















#|
EJEMPLOS CONSTRUCTOR:
(paradigmadocs "gDocs" 16 10 2021 encryptFn encryptFn)
(paradigmadocs "gWord" 17 10 2021 encryptFn encryptFn)
(paradigmadocs "gTXT" 18 10 2021 encryptFn encryptFn)


EJEMPLOS SELECTOR:
(get-dato (list "gDocs" 16 10 2021 encryptFn encryptFn '() '()) 0)
(get-dato (list "gWord" 16 10 2021 encryptFn encryptFn '() '()) 1)
(get-dato (list "gTXT" 16 10 2021 encryptFn encryptFn '("Laysa" "A" 17 10 2021) '()) 6) ; Luego de aplicar el tercer ejemplo del siguiente tópico

EJEMPLOS MODIFICADOR:
(set-usuario-contraseña-fecha (list "gDocs" 16 10 2021 encryptFn encryptFn '() '()) "Angel" "contraseña" 17 10 2021)
(set-usuario-contraseña-fecha (list "gWord" 17 10 2021 encryptFn encryptFn '() '()) "Milky" "guau" 17 10 2021)
(set-usuario-contraseña-fecha (list "gTXT" 18 10 2021 encryptFn encryptFn '() '()) "Laysa" "A" 17 10 2021)




|#

(provide (all-defined-out))  