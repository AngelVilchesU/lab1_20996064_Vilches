#lang racket

(require "TDA_fecha.rkt")
(require "TDA_paradigmadocs.rkt")

; Implementación del TDA usuarios

; Representación:
; (integer X integer X integer X string X string X integer X integer)
; ( (dia mes año) usuario contraseña activo-inactivo ID)

; Constructor:
; Descripción: Permite la creación de un usuario nuevo mediante la solicitud de los datos...
;              ...fecha, usuario, contraseña y sesión activa o inactiva (booleano)
; Dominio: (integer X integer X integer X string X string)
; Recorrido: Lista con los datos (Importante: Id es ingresado automáticamente)

(define crear-usuario
  (lambda (dia mes año usuario contraseña)
    (if (fecha? (crear-fecha dia mes año))
        (list (crear-fecha dia mes año) usuario contraseña 0)
        '()
        )
    )
  )

; Pertenencia:
; Descripción: Función que permite verificar si un usuario (información) se encuentra bien definido
; Dominio: Lista correspondiente a la información de un usuario
; Recorrido: Booleando verificador

(define usuario?
  (lambda (list-info-usuario)
    (if (fecha? (car list-info-usuario))
        (if (string? (car (cdr list-info-usuario)))
            (if (string? (car (cdr (cdr list-info-usuario))))
                #t
                #f)
            #f)
        #f)
    )
  )

; Selector:
; Descripción: Función que permite obtener la fecha registrada
; Dominio: Lista de información del usuario
; Recorrido: Fecha registrada

(define get-fecha
  (lambda (lista-info-usuario)
    (car lista-info-usuario)
    )
  )

; Descripción: Función que permite obtener el usuario registrado
; Dominio: Lista de información del usuario
; Recorrido: Usuario registrado

(define get-usuario
  (lambda (lista-info-usuario)
    (car (cdr lista-info-usuario))
    )
  )

; Descripción: Función que permite obtener la contraseña registrada
; Dominio: Lista de información del usuario
; Recorrido: Contraseña registrada

(define get-contraseña
  (lambda (lista-info-usuario)
    (car (cdr (cdr lista-info-usuario)))
    )
  )

; Descripción: Función que permite obtener el valor booleano indicador de sesión activa o inactiva
; Dominio: Lista de información del usuario
; Recorrido: Booleano

(define get-act-inc-sesión
  (lambda (lista-info-usuario)
    (car (cdr (cdr (cdr lista-info-usuario))))
    )
  )

; Modificador:
; Descripción: Función que permite modificar el usuario registrado mediante...
;              ...la creación de una nueva lista conservando los datos que no sean el usuario
; Dominio: Lista de información del usuario
; Recorrido: Lista correspondiente a la información del usuario modificada (usuario)

(define set-usuario
  (lambda (lista-info-usuario nuevo-usuario)
    (if (usuario? (list (get-fecha lista-info-usuario) nuevo-usuario (get-contraseña lista-info-usuario)))
        (list (get-fecha lista-info-usuario) nuevo-usuario (get-contraseña lista-info-usuario))
        #f)
    )
  )

; Descripción: Función que permite modificar la contraseña registrada mediante...
;              ...la creación de una nueva lista conservando los datos que no sean la contraseña
; Dominio: Lista de información del usuario
; Recorrido: Lista correspondiente a la información del usuario modificada (contraseña)

(define set-contraseña
  (lambda (lista-info-usuario nueva-contraseña)
    (if (usuario? (list (get-fecha lista-info-usuario) (get-usuario lista-info-usuario) nueva-contraseña))
        (list (get-fecha lista-info-usuario) (get-usuario lista-info-usuario) nueva-contraseña)
        #f)
    )
  )

; Descripción: Función que permite modificar la lista de usuarios creada en la plataforma diseñada (posición Nro. 4) recibiendo como...
;              ... parametros de entrada la plataforma y la lista contenedora de la información de un nuevo usuario para, en paralelo...
;              ... con la función denominada "set-lista-usuarios" insertarla dentro como lista obteniendo a nivel de plataforma en...
;              ... la posición número 4 una lista de listas correspondiente a la información de un usuario
; Dominio: paradigmadocs X list
; Recorrido: Actualización de paradigmadocs

(define modificar-lista-usuarios
  (lambda (plataforma lista-info-usuarios)
    (list (get-dato plataforma 0) (get-dato plataforma 1) (get-dato plataforma 2) (get-dato plataforma 3) (set-lista-usuarios (get-dato plataforma 4) lista-info-usuarios) (get-dato plataforma 5))
    )
  )

; Otras funciones:
; Descripción: Función que permite unir dos listas mediante la función "append"
; Dominio: list X nueva-lista
; Recorrido: Unión de listas como listas de listas

(define set-lista-usuarios
  (lambda (lista nueva-lista)
    (append lista (list nueva-lista))
    )
  )

; Descripción: Función que permite realizar una veríficación de la existencia (o no) de un usuario determinado mediante recursión natural/lineal
; Dominio: Lista X usuario
; Recorrido: Booleano
; Tipo de recursión: Recursión natural/lineal

(define usuario-repetido?
  (lambda (lista-info-usuarios usuario)
    (if (null? lista-info-usuarios) #f
        (if (equal? usuario (get-usuario (car lista-info-usuarios))) #t
            (if (null? (cdr lista-info-usuarios)) #f
                (usuario-repetido? (cdr lista-info-usuarios) usuario))
            )
        )
    )
  )

; Descripción: Función que permite modificar el valor booleano asignado de acuerdo con las operaciones...
;              ...que se realicen (activar)
; Dominio: Lista de información del usuario
; Recorrido: Lista correspondiente a la información del usuario modificada (sesión activa)

(define set-sesion-act
  (lambda (lista-info-usuario usuario paradigmadocs)
    (if (null? lista-info-usuario) #f
        (if (equal? usuario (get-usuario (car lista-info-usuario)))
            (modificar-lista-usuarios paradigmadocs (list (get-fecha (car lista-info-usuario)) (get-usuario (car lista-info-usuario)) (get-contraseña (car lista-info-usuario)) 1))
            (if (null? (cdr lista-info-usuario)) #f
                (set-sesion-act (cdr lista-info-usuario) usuario paradigmadocs)
                )
            )
        )
    )
  )

; ACTUALIZAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; Descripción: Función que permite modificar el valor booleano asignado de acuerdo con las operaciones...
;              ...que se realicen (desactivar)
; Dominio: Lista de información del usuario
; Recorrido: Lista correspondiente a la información del usuario modificada (sesión inactiva)

(define set-sesion-ina
  (lambda (lista-info-usuario)
    (list (get-fecha lista-info-usuario) (get-usuario lista-info-usuario) (get-contraseña lista-info-usuario) 0)
    ))

; Descripción: Función que permite realizar una veríficación de la existencia (o no) de una contraseña determinada mediante recursión natural/linea
; Dominio: Lista X contraseña
; Recorrido: Booleano
; Tipo de recursión: Recursión natural/lineal

(define contraseña-repetida?
  (lambda (lista-info-usuarios contraseña)
    (if (null? lista-info-usuarios) #f
        (if (equal? contraseña (get-contraseña (car lista-info-usuarios))) #t
            (if (null? (cdr lista-info-usuarios)) #f
                (contraseña-repetida? (cdr lista-info-usuarios) contraseña))
            )
        )
    )
  )

; Descripción: Función orientada a la autentificación del usuario (activo/inactivo), permite la eliminación de una lista con un usuario especifíco inactivo luego de,...
;              ... mediante una función complementaria, haber insertado dicha lista (mediante selectores y modificadores) como usuario activo (en caso de serlo)
; Dominio: Listabase X usuario X paradigmadocs
; Recorrido: Actualización de paradigmadocs
; Tipo de recursión: Recursión natural/lineal

(define remover-list-usuario-inactivo
  (lambda (lista usuario paradigmadocs)
    (if (null? lista)
         lista
        (if (and (equal? usuario (get-usuario (car lista))) (equal? 0 (get-act-inc-sesión (car lista)))) 
            (list (get-dato paradigmadocs 0) (get-dato paradigmadocs 1) (get-dato paradigmadocs 2) (get-dato paradigmadocs 3) (remove (car lista) (get-dato paradigmadocs 4)) (get-dato paradigmadocs 5))   
            (remover-list-usuario-inactivo (cdr lista) usuario paradigmadocs)
            )
        )
    )
  )

; Descripción: Función que aplica dos función definidas previamente de forma conjunta para la creación/copia (mediante getters) e inserción (mediante setters)
;              ... de los datos (lista) de un usuario variando el parametro inactivo a activo eliminando la primera lista y retornando las listas que existian
;              ... previamente a la operación
; Dominio: Listabase X usuario X paradigmadocs
; Recorrido: Actualización de paradigmadocs

(define agregar-y-remover
  (lambda (lista usuario paradigmadocs)
    (remover-list-usuario-inactivo (get-dato (set-sesion-act (get-dato paradigmadocs 4) usuario paradigmadocs) 4) usuario (set-sesion-act (get-dato paradigmadocs 4) usuario paradigmadocs))
    )
  )

; Descripción: Función que busca mediante recursión natural/lineal un usuario cuyo parámetro indicador de autentificador en su correspondiente lista de información...
;              ... refleje que este se encuentre activo (1)
; Dominio: Lista base de usuarios
; Recorrido: Usuario (caso verdadero, es decir, exista y esté activo) o el valor Booleano indicador de falso (caso contrario, es decir, no existe i no está activo)
; Tipo de recursión: Recursión natural/lineal

(define buscar-usuario-activo
  (lambda (lista-base)
    (if (equal? 1 (get-act-inc-sesión (car lista-base)))
        (get-usuario (car lista-base))
        (if (null? (cdr lista-base))
            #f
            (buscar-usuario-activo (cdr lista-base))
            )
        )
    )
  )

    

#|

;EJEMPLOS CONSTRUCTOR:
(crear-usuario 19 10 2021 "Angel" "contraseña")
(crear-usuario 20 10 2021 "Jaime" "pinturaceresita")
(crear-usuario 19 23 2021 "Fifi" "tostador") ;Este ejemplo expresa una situación no valida ya que no existe un mes 23

;EJEMPLOS PERTENENCIA:
(usuario? (crear-usuario 19 10 2021 "Angel" "contraseña"))
(usuario? (crear-usuario 20 10 2021 "Jaime" "pinturaceresita"))
(usuario? (crear-usuario 21 12 2021 1 "tostador")) ;Este ejemplo expresa una situación no valida ya que el usuario no es valido (integer)

;EJEMPLOS SELECTOR:
(get-fecha (crear-usuario 19 10 2021 "Angel" "contraseña"))
(get-fecha (crear-usuario 20 10 2021 "Jaime" "pinturaceresita"))
(get-fecha (crear-usuario 21 12 2021 "Fifi" "tostador"))
(get-usuario (crear-usuario 19 10 2021 "Angel" "contraseña"))
(get-usuario (crear-usuario 20 10 2021 "Jaime" "pinturaceresita"))
(get-usuario (crear-usuario 21 12 2021 "Fifi" "tostador"))
(get-contraseña (crear-usuario 19 10 2021 "Angel" "contraseña"))
(get-contraseña (crear-usuario 20 10 2021 "Jaime" "pinturaceresita"))
(get-contraseña (crear-usuario 21 12 2021 "Fifi" "tostador"))
(get-act-inc-sesión (crear-usuario 19 10 2021 "Angel" "contraseña"))

;EJEMPLOS MODIFICADORES:
Creadas pero no aplicadas (no son necesarias hasta el momento)

;EJEMPLOS OTRAS FUNCIONES:
(modificar-lista-usuarios (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") (crear-usuario 19 10 2021 "Angel" "contraseña"))
(modificar-lista-usuarios (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") (crear-usuario 20 10 2021 "Jaime" "pinturaceresita"))
(modificar-lista-usuarios (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") (crear-usuario 21 12 2021 "Benjamin" "aloy"))
(set-lista-usuarios (get-dato (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") 4) (crear-usuario 19 10 2021 "Angel" "contraseña"))
(set-lista-usuarios (get-dato (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") 4) (crear-usuario 20 10 2021 "Jaime" "pinturaceresita"))
(set-lista-usuarios (get-dato (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") 4) (crear-usuario 21 12 2021 "Benjamin" "aloy"))
(usuario-repetido? (get-dato (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") 4) "Angel")
(usuario-repetido? (get-dato (modificar-lista-usuarios (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") (crear-usuario 19 10 2021 "Angel" "contraseña")) 4) "Angel")
(usuario-repetido? (get-dato (modificar-lista-usuarios (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn") (crear-usuario 19 10 2021 "Angel" "contraseña")) 4) "Cale")
(set-sesion-act (crear-usuario 19 10 2021 "Angel" "contraseña"))
(set-sesion-ina (crear-usuario 19 10 2021 "Angel" "contraseña"))











> (define paradigma (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn"))
> (define user1 (crear-usuario 19 10 2021 "Angel" "contraseña"))
> (define user2 (crear-usuario 20 10 2021 "Jaime" "pinturaceresita"))
> (define paradigma2 (modificar-lista-usuarios paradigma user1))
> paradigma2
'("gDocs" (16 10 2021) "encryptFn" "encryptFn" (((19 10 2021) "Angel" "contraseña")) ())
> (define paradigma3 (modificar-lista-usuarios paradigma2 user2))
> paradigma3
'("gDocs" (16 10 2021) "encryptFn" "encryptFn" (((19 10 2021) "Angel" "contraseña") ((20 10 2021) "Jaime" "pinturaceresita")) ())
> (usuario-repetido? (get-dato paradigma3 4) "Angel")

buscar-usuario-activo (get-dato paradigma3 4)








-----------------> (define paradigma (paradigmadocs "gDocs" 16 10 2021 "encryptFn" "encryptFn"))
> (define user1 (crear-usuario 19 10 2021 "Angel" "contraseña"))
> (define user2 (crear-usuario 20 10 2021 "Jaime" "pinturaceresita"))
> (define paradigma2 (modificar-lista-usuarios paradigma user1))
> paradigma2
'("gDocs" (16 10 2021) "encryptFn" "encryptFn" (((19 10 2021) "Angel" "contraseña" 0)) ())
> (define paradigma3 (modificar-lista-usuarios paradigma2 user2))
> paradigma3
'("gDocs" (16 10 2021) "encryptFn" "encryptFn" (((19 10 2021) "Angel" "contraseña" 0) ((20 10 2021) "Jaime" "pinturaceresita" 0)) ())

> paradigma4
#f
> paradigma3
'("gDocs" (16 10 2021) "encryptFn" "encryptFn" (((19 10 2021) "Angel" "contraseña" 0) ((20 10 2021) "Jaime" "pinturaceresita" 0)) ())
> (define paradigma5 (agregar-y-remover (get-dato paradigma3 4) "Jaime" paradigma3))
> (define paradigma4 (buscar-usuario-activo (get-dato paradigma3 4)))
|#

(provide (all-defined-out))
