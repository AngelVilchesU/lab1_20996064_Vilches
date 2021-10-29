#lang racket

(require "TDA_fecha.rkt")
(require "TDA_paradigmadocs.rkt")

; Implementación del TDA usuarios

; Representación:
; (integer X integer X integer X string X string X integer)
; ( (dia mes año) usuario contraseña activo-inactivo )

; Constructor:
; Descripción: Permite la creación de un usuario nuevo mediante la solicitud de los datos...
;              ...fecha, usuario, contraseña y sesión activa o inactiva (booleano)
; Dominio: (integer X integer X integer X string X string)
; Recorrido: Lista con los datos (Importante: Id es ingresado automáticamente)

(define crear-usuario
  (lambda (dia mes año usuario contraseña)
    (if (usuario? (list (crear-fecha dia mes año) usuario contraseña 0))
        (list (crear-fecha dia mes año) usuario contraseña 0)
        null)
    )
  )

; Pertenencia:
; Descripción: Función que permite verificar si un usuario (información) se encuentra bien definido
; Dominio: Lista correspondiente a la información de un usuario
; Recorrido: Booleando verificador

(define usuario?
  (lambda (list-info-usuario)
    (if (list? list-info-usuario)
        (if (= (length list-info-usuario) 4)
            (if (fecha? (car list-info-usuario))
                (if (and (string? (car (cdr list-info-usuario))) (string? (car (cdr (cdr list-info-usuario)))))
                    #t
                    #f)
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
    (if (usuario? lista-info-usuario)
        (car lista-info-usuario)
        #f)
    )
  )

; Descripción: Función que permite obtener el usuario registrado
; Dominio: Lista de información del usuario
; Recorrido: Usuario registrado

(define get-usuario
  (lambda (lista-info-usuario)
    (if (usuario? lista-info-usuario)
        (car (cdr lista-info-usuario))
        #f) 
    )
  )

; Descripción: Función que permite obtener la contraseña registrada
; Dominio: Lista de información del usuario
; Recorrido: Contraseña registrada

(define get-contraseña
  (lambda (lista-info-usuario)
    (if (usuario? lista-info-usuario)
        (car (cdr (cdr lista-info-usuario)))
        #f)
    )
  )

; Descripción: Función que permite obtener el valor booleano indicador de sesión activa o inactiva
; Dominio: Lista de información del usuario
; Recorrido: Booleano

(define get-act-ina-sesión
  (lambda (lista-info-usuario)
    (if (usuario? lista-info-usuario)
        (car (cdr (cdr (cdr lista-info-usuario))))
        #f)
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
;              ... con la función denominada "anexar-lista-usuarios" insertarla dentro como lista obteniendo a nivel de plataforma en...
;              ... la posición número 4 una lista de listas correspondiente a la información de un usuario
; Dominio: paradigmadocs X list
; Recorrido: Actualización de paradigmadocs

(define modificar-lista-usuarios
  (lambda (plataforma lista-info-usuarios)
    (list (get-dato plataforma 0) (get-dato plataforma 1) (get-dato plataforma 2) (get-dato plataforma 3) (anexar-lista-usuarios (get-dato plataforma 4) lista-info-usuarios) (get-dato plataforma 5))
    )
  )

; Descripción: Función que permite unir dos listas mediante la función "append"
; Dominio: list X nueva-lista
; Recorrido: Unión de listas como listas de listas

(define anexar-lista-usuarios
  (lambda (lista nueva-lista)
    (append lista (list nueva-lista))
    )
  )

; Otras funciones:
; Descripción: Función que permite realizar una veríficación de la existencia (o no) de un usuario determinado mediante recursión natural/lineal
; Dominio: Lista X usuario
; Recorrido: Booleano
; Tipo de recursión: Recursión natural/lineal

(define usuario-repetido?
  (lambda (lista-info-usuarios usuario)
    (if (null? lista-info-usuarios)
        #f
        (if (equal? usuario (get-usuario (car lista-info-usuarios)))
            #t
            (if (null? (cdr lista-info-usuarios))
                #f
                (usuario-repetido? (cdr lista-info-usuarios) usuario))
            )
        )
    )
  )

; Descripción: Función que permite modificar el valor booleano asignado de acuerdo con las operaciones...
;              ...que se realicen (activar)
; Dominio: Lista de información del usuario
; Recorrido: Lista correspondiente a la información del usuario modificada (sesión activa)

(define activar-desactivar-usuario
  (lambda (lista-info-usuario usuario paradigmadocs)
    (if (null? lista-info-usuario) #f
        (if (and (equal? usuario (get-usuario (car lista-info-usuario))) (equal? 0 (get-act-ina-sesión (car lista-info-usuario)))) 
            (modificar-lista-usuarios paradigmadocs (list (get-fecha (car lista-info-usuario)) (get-usuario (car lista-info-usuario)) (get-contraseña (car lista-info-usuario)) 1))
            (if (and (equal? usuario (get-usuario (car lista-info-usuario))) (equal? 1 (get-act-ina-sesión (car lista-info-usuario))))
                (modificar-lista-usuarios paradigmadocs (list (get-fecha (car lista-info-usuario)) (get-usuario (car lista-info-usuario)) (get-contraseña (car lista-info-usuario)) 0))
                (if (null? (cdr lista-info-usuario))
                    #f
                    (activar-desactivar-usuario (cdr lista-info-usuario) usuario paradigmadocs))
                )
            )
        )
    )
  )

; Descripción: Función orientada a la autentificación del usuario (activo/inactivo), permite la eliminación de una lista con un usuario especifíco inactivo luego de,...
;              ... mediante una función complementaria, haber insertado dicha lista (mediante selectores y modificadores) como usuario activo (en caso de serlo)
; Dominio: Listabase X usuario X paradigmadocs
; Recorrido: Actualización de paradigmadocs
; Tipo de recursión: Recursión natural/lineal

(define remover-list-usuario-act-ina
  (lambda (lista-info-usuario usuario paradigmadocs)
    (if (null? lista-info-usuario)
        lista-info-usuario
        (if (and (equal? usuario (get-usuario (car lista-info-usuario))) (equal? 0 (get-act-ina-sesión (car lista-info-usuario)))) 
            (list (get-dato paradigmadocs 0) (get-dato paradigmadocs 1) (get-dato paradigmadocs 2) (get-dato paradigmadocs 3) (remove (car lista-info-usuario) (get-dato paradigmadocs 4)) (get-dato paradigmadocs 5))   
            (if (and (equal? usuario (get-usuario (car lista-info-usuario))) (equal? 1 (get-act-ina-sesión (car lista-info-usuario))))
                (list (get-dato paradigmadocs 0) (get-dato paradigmadocs 1) (get-dato paradigmadocs 2) (get-dato paradigmadocs 3) (remove (car lista-info-usuario) (get-dato paradigmadocs 4)) (get-dato paradigmadocs 5))
                (remover-list-usuario-act-ina (cdr lista-info-usuario) usuario paradigmadocs)
                )
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
  (lambda (usuario paradigmadocs)
    (remover-list-usuario-act-ina (get-dato (activar-desactivar-usuario (get-dato paradigmadocs 4) usuario paradigmadocs) 4) usuario (activar-desactivar-usuario (get-dato paradigmadocs 4) usuario paradigmadocs))
    )
  )

; Descripción: Función que permite realizar una veríficación de la existencia (o no) de una contraseña determinada mediante recursión natural/linea
; Dominio: Lista X contraseña
; Recorrido: Booleano
; Tipo de recursión: Recursión natural/lineal

(define esta-contraseña?
  (lambda (lista-info-usuarios contraseña)
    (if (null? lista-info-usuarios) #f
        (if (equal? contraseña (get-contraseña (car lista-info-usuarios))) #t
            (if (null? (cdr lista-info-usuarios)) #f
                (esta-contraseña? (cdr lista-info-usuarios) contraseña))
            )
        )
    )
  )

; Descripción: Función que busca mediante recursión natural/lineal un usuario cuyo parámetro indicador de autentificador en su correspondiente lista de información...
;              ... refleje que este se encuentre activo (1)
; Dominio: Lista base de usuarios
; Recorrido: Usuario (caso verdadero, es decir, exista y esté activo) o el valor Booleano indicador de falso (caso contrario, es decir, no existe i no está activo)
; Tipo de recursión: Recursión natural/lineal

(define buscar-usuario-activo
  (lambda (lista-base)
    (if (equal? 1 (get-act-ina-sesión (car lista-base)))
        (get-usuario (car lista-base))
        (if (null? (cdr lista-base))
            #f
            (buscar-usuario-activo (cdr lista-base))
            )
        )
    )
  )

#|
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;EJEMPLOS CONSTRUCTOR:
(define ejemplo-crear-usuario-1 (crear-usuario 19 10 2021 "Angel" "contraseña"))
(define ejemplo-crear-usuario-2 (crear-usuario 20 10 2021 "Jaime" "pinturaceresita"))
(define ejemplo-crear-usuario-3 (crear-usuario 19 12 2021 "Fifi" 6))
; El tercer ejemplo expresa una situación no valida

;EJEMPLOS PERTENENCIA:
(define ejemplo-usuario?-1 (usuario? ejemplo-crear-usuario-1))
(define ejemplo-usuario?-2 (usuario? ejemplo-crear-usuario-2))
(define ejemplo-usuario?-3 (usuario? ejemplo-crear-usuario-3))
; El tercer ejemplo expresa una situación no valida

;EJEMPLOS SELECTOR:
(define ejemplo-get-fecha-1 (get-fecha ejemplo-crear-usuario-1))
(define ejemplo-get-fecha-2 (get-fecha ejemplo-crear-usuario-2))
(define ejemplo-get-fecha-3 (get-fecha ejemplo-crear-usuario-3))
; El tercer ejemplo expresa una situación no valida

(define ejemplo-get-usuario-1 (get-usuario ejemplo-crear-usuario-1))
(define ejemplo-get-usuario-2 (get-usuario ejemplo-crear-usuario-2))
(define ejemplo-get-usuario-3 (get-usuario ejemplo-crear-usuario-3))
; El tercer ejemplo expresa una situación no valida

(define ejemplo-get-contraseña-1 (get-contraseña ejemplo-crear-usuario-1))
(define ejemplo-get-contraseña-2 (get-contraseña ejemplo-crear-usuario-2))
(define ejemplo-get-contraseña-3 (get-contraseña ejemplo-crear-usuario-3))
; El tercer ejemplo expresa una situación no valida

(define ejemplo-get-act-ina-sesión-1 (get-act-ina-sesión ejemplo-crear-usuario-1))
(define ejemplo-get-act-ina-sesión-2 (get-act-ina-sesión ejemplo-crear-usuario-2))
(define ejemplo-get-act-ina-sesión-3 (get-act-ina-sesión ejemplo-crear-usuario-3))
; El tercer ejemplo expresa una situación no valida

;EJEMPLOS MODIFICADORES:
; ...previamente definida la plataforma a emplear: "(define emptyGDocs (paradigmadocs "gDocs" 16 10 2021 encryptFn encryptFn))"
(define ejemplo-modificar-lista-usuarios-1 (modificar-lista-usuarios emptyGDocs ejemplo-crear-usuario-1))
(define ejemplo-modificar-lista-usuarios-2 (modificar-lista-usuarios emptyGDocs ejemplo-crear-usuario-2))
(define ejemplo-modificar-lista-usuarios-3 (modificar-lista-usuarios ejemplo-modificar-lista-usuarios-2 ejemplo-crear-usuario-1))

;EJEMPLOS OTRAS FUNCIONES:
; ...previamente definida la plataforma a emplear: "(define emptyGDocs (paradigmadocs "gDocs" 16 10 2021 encryptFn encryptFn))"
(define ejemplo-anexar-lista-usuarios-1 (anexar-lista-usuarios (get-dato emptyGDocs 4) ejemplo-crear-usuario-1))
(define ejemplo-anexar-lista-usuarios-2 (anexar-lista-usuarios (get-dato emptyGDocs 4) ejemplo-crear-usuario-2))
(define ejemplo-anexar-lista-usuarios-3 (anexar-lista-usuarios ejemplo-anexar-lista-usuarios-1 ejemplo-crear-usuario-2))

(define ejemplo-usuario-repetido?-1 (usuario-repetido? (get-dato emptyGDocs 4) "Angel"))
(define ejemplo-usuario-repetido?-2 (usuario-repetido? (get-dato emptyGDocs 4) "Jaime"))
(define ejemplo-usuario-repetido?-3 (usuario-repetido? (get-dato ejemplo-modificar-lista-usuarios-1 4) "Angel"))

; Las siguientes funciones "activar-desactivar-usuario" y "remover-list-usuario-act-ina" funcionan de forma complementaria
;; ACTIVA USUARIO "Jaime"
(define ejemplo-activar-desactivar-usuario-1 (activar-desactivar-usuario (get-dato ejemplo-modificar-lista-usuarios-3 4) "Jaime" ejemplo-modificar-lista-usuarios-3))
;; ELIMINA USUARIO INACTIVO "Jaime"
(define ejemplo-remover-list-usuario-act-ina-1 (remover-list-usuario-act-ina (get-dato ejemplo-activar-desactivar-usuario-1 4) "Jaime" ejemplo-activar-desactivar-usuario-1))
;; ACTIVA USUARIO "Angel"
(define ejemplo-activar-desactivar-usuario-2 (activar-desactivar-usuario (get-dato ejemplo-modificar-lista-usuarios-3 4) "Angel" ejemplo-modificar-lista-usuarios-3))
;; ELIMINA USUARIO INACTIVO "Angel"      
(define ejemplo-remover-list-usuario-act-ina-2 (remover-list-usuario-act-ina (get-dato ejemplo-activar-desactivar-usuario-2 4) "Angel" ejemplo-activar-desactivar-usuario-2))
;; DESACTIVA USUARIO "Angel" (proceso inverso a lo anterior)
(define ejemplo-activar-desactivar-usuario-3 (activar-desactivar-usuario (get-dato ejemplo-remover-list-usuario-act-ina-2 4) "Angel" ejemplo-remover-list-usuario-act-ina-2))
;; ELIMINA USUARIO ACTIVO "Angel"
(define ejemplo-remover-list-usuario-act-ina-3 (remover-list-usuario-act-ina (get-dato ejemplo-activar-desactivar-usuario-3 4) "Angel" ejemplo-activar-desactivar-usuario-3))

(define ejemplo-agregar-y-remover-1 (agregar-y-remover "Jaime" ejemplo-modificar-lista-usuarios-3))
(define ejemplo-agregar-y-remover-2 (agregar-y-remover "Angel" ejemplo-modificar-lista-usuarios-3))
(define ejemplo-agregar-y-remover-3 (agregar-y-remover "Angel" ejemplo-agregar-y-remover-2))

(define ejemplo-esta-contraseña?-1 (esta-contraseña? (get-dato ejemplo-modificar-lista-usuarios-3 4) "pinturaceresita"))
(define ejemplo-esta-contraseña?-2 (esta-contraseña? (get-dato ejemplo-modificar-lista-usuarios-3 4) "contraseña"))
(define ejemplo-esta-contraseña?-3 (esta-contraseña? (get-dato ejemplo-modificar-lista-usuarios-3 4) "imaginar usar esta FN igualando usuario y contraseña UwU"))

(define ejemplo-buscar-usuario-activo-1 (buscar-usuario-activo (get-dato ejemplo-modificar-lista-usuarios-3 4)))
(define ejemplo-buscar-usuario-activo-2 (buscar-usuario-activo (get-dato ejemplo-agregar-y-remover-2 4)))
(define ejemplo-buscar-usuario-activo-3 (buscar-usuario-activo (get-dato ejemplo-agregar-y-remover-3 4)))
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
|#

(provide (all-defined-out))
