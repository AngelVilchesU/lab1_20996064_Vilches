#lang racket

(require "TDAfecha_20996064_VilchesUrrutia.rkt")
(require "TDAparadigmadocs_20996064_VilchesUrrutia.rkt")

; Implementación del TDA usuarios

; Representación:
; ( (integer X integer X integer) X string X string X integer)
; ( (dia mes año) usuario contraseña activo-inactivo )

; Constructor:
; Descripción: Permite la creación de un usuario nuevo mediante la solicitud de los datos...
;              ...fecha, usuario y contraseña
; Dominio: ((integer X integer X integer) X string X string)
; Recorrido: Lista con los datos ingresados, de ser válidos (Importante: Id es ingresado automáticamente)

(define crear-usuario
  (lambda (fecha usuario contraseña)
    (if (usuario? (list fecha usuario contraseña 0))
        (list fecha usuario contraseña 0)
        null)
    )
  )

; Pertenencia:
; Descripción: Función que permite verificar si un usuario (información) se encuentra bien definido
; Dominio: Lista correspondiente a la información de un usuario
; Recorrido: Booleando verificador

(define usuario?
  (lambda (list-info-usuario)
    ; ¿Es lista?
    (if (list? list-info-usuario)
        ; ¿La longitud de la lista es de cuatro elementos?
        (if (= (length list-info-usuario) 4)
            ; ¿La fecha válida de acuerdo con el TDA-fecha?
            (if (fecha? (car list-info-usuario))
                ; ¿El segundo y tercer elemento corresponden a strings?
                (if (and (string? (car (cdr list-info-usuario))) (string? (car (cdr (cdr list-info-usuario)))))
                    #t ; Si se cumple todo lo anterior...
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
; Descripción: Función que permite modificar el valor booleano asignado de acuerdo con las operaciones...
;              ...que se realicen (activar)
; Dominio: Lista de información del usuario X usuario X paradigmadocs
; Recorrido: Lista correspondiente a la información del usuario modificada (sesión activa)
; Tipo de recursión: Recursión natural/lineal

(define set-act-ina-usuario
  (lambda (lista-info-usuario usuario paradigmadocs)
    ; ¿La lista de listas de usuarios es nula?
    (if (null? lista-info-usuario)
        #f
        ; ¿El usuario de primera lista de usuarios contenida coincide con el ingresado y su correspondiente valor booleano indica que está inactivo?
        (if (and (equal? usuario (get-usuario (car lista-info-usuario))) (equal? 0 (get-act-ina-sesión (car lista-info-usuario)))) 
            (set-act-list-usuarios-paradigmadocs paradigmadocs (list (get-fecha (car lista-info-usuario))
                                                          (get-usuario (car lista-info-usuario))
                                                          (get-contraseña (car lista-info-usuario))
                                                          1))
            ; ¿El usuario de primera lista de usuarios contenida coincide con el ingresado y su correspondiente valor booleano indica que está activo?
            (if (and (equal? usuario (get-usuario (car lista-info-usuario))) (equal? 1 (get-act-ina-sesión (car lista-info-usuario))))
                (set-act-list-usuarios-paradigmadocs paradigmadocs (list (get-fecha (car lista-info-usuario))
                                                                         (get-usuario (car lista-info-usuario))
                                                                         (get-contraseña (car lista-info-usuario))
                                                                         0))
                ; ¿El resto de listas de usuario es nulo?
                (if (null? (cdr lista-info-usuario))
                    #f
                    ; Llamado recursivo entregando el resto de listas de usuario en la lista "base", el usuario y la plataforma
                    (set-act-ina-usuario (cdr lista-info-usuario) usuario paradigmadocs))
                )
            )
        )
    )
  )

; Otras funciones:
; Descripción: Función que permite realizar una veríficación de la existencia (o no) de un usuario determinado mediante recursión natural/lineal
; Dominio: Lista X usuario
; Recorrido: Booleano
; Tipo de recursión: Recursión natural/lineal

(define esta-usuario?
  (lambda (lista-info-usuarios usuario)
    ; ¿La lista contenedora de listas de usuarios es nula?
    (if (null? lista-info-usuarios)
        #f
        ; ¿El usuario en la primera lista de usuarios contenida coincide con el ingresado?
        (if (equal? usuario (get-usuario (car lista-info-usuarios)))
            #t
            ; ¿El resto de listas de usuario es nulo?
            (if (null? (cdr lista-info-usuarios))
                #f
                ; Llamado recursivo entregando el resto de listas de usuario en la lista "base" y el usuario
                (esta-usuario? (cdr lista-info-usuarios) usuario))
            )
        )
    )
  )

; Descripción: Función orientada a la autentificación del usuario (activo/inactivo), permite la eliminación de una lista con un usuario especifíco inactivo luego de,...
;              ... mediante una función complementaria, haber insertado dicha lista (mediante selectores y modificadores) como usuario activo (en caso de serlo)
; Dominio: Listabase X usuario X paradigmadocs
; Recorrido: Actualización de paradigmadocs
; Tipo de recursión: Recursión natural/lineal
; Es importante comentar que la presente función no se encuetra en "TDA_paradigmadocs" puesto que se produciría un error en la ejecución...
; ... del código a causa de un "llamado entrelazado" entre los dos archivos

(define remover-list-usuario-act-ina
  (lambda (lista-info-usuario usuario paradigmadocs)
    ; ¿La lista contenedora de listas de usuarios es nula?
    (if (null? lista-info-usuario)
        lista-info-usuario
        ; ¿El usuario de primera lista de usuarios contenida coincide con el ingresado y su correspondiente valor booleano indica que está inactivo?
        (if (and (equal? usuario (get-usuario (car lista-info-usuario))) (equal? 0 (get-act-ina-sesión (car lista-info-usuario)))) 
            (list (get-dato paradigmadocs 0)
                  (get-dato paradigmadocs 1)
                  (get-dato paradigmadocs 2)
                  (get-dato paradigmadocs 3)
                  (remove (car lista-info-usuario)
                          (get-dato paradigmadocs 4))
                  (get-dato paradigmadocs 5))
            ; ¿El usuario de primera lista de usuarios contenida coincide con el ingresado y su correspondiente valor booleano indica que está activo?
            (if (and (equal? usuario (get-usuario (car lista-info-usuario))) (equal? 1 (get-act-ina-sesión (car lista-info-usuario))))
                (list (get-dato paradigmadocs 0)
                      (get-dato paradigmadocs 1)
                      (get-dato paradigmadocs 2)
                      (get-dato paradigmadocs 3)
                      (remove (car lista-info-usuario)
                              (get-dato paradigmadocs 4))
                      (get-dato paradigmadocs 5))
                ; Llamado recursivo entregando el resto de listas de usuario en la lista "base", el usuario y la plataforma
                (remover-list-usuario-act-ina (cdr lista-info-usuario) usuario paradigmadocs)
                )
            )
        )
    )
  )

; Descripción: Función que aplica las funciones "set-act-ina-usuario" y "remover-list-usuario-act-ina", pertenencientes al espectro...
;              ... de setters y "otras funciones" respectivamente, de forma conjunta para la creación/copia (mediante getters) e...
;              ... inserción (mediante setters) de los datos (lista) de un usuario variando el parametro inactivo a activo...
;              ... eliminando la primera lista y retornando las listas que existían previamente a la operación
; Dominio: usuario X paradigmadocs
; Recorrido: Actualización de paradigmadocs

(define set-n-remov
  (lambda (usuario paradigmadocs)
    (remover-list-usuario-act-ina (get-dato (set-act-ina-usuario (get-dato paradigmadocs 4) usuario paradigmadocs) 4)
                                  usuario
                                  (set-act-ina-usuario (get-dato paradigmadocs 4) usuario paradigmadocs))
    )
  )

; Descripción: Función que permite realizar una veríficación de la existencia (o no) de una contraseña determinada mediante recursión natural/linea
; Dominio: Lista X contraseña
; Recorrido: Booleano
; Tipo de recursión: Recursión natural/lineal

(define usuario-contraseña-valida?
  (lambda (lista-info-usuarios usuario contraseña)
    ; ¿La lista contenedora de listas de usuarios es nula?
    (if (null? lista-info-usuarios)
        #f
        ; ¿La contraseña en la primera lista de usuarios contenida coincide con la ingresada?
        (if (and (equal? usuario (get-usuario (car lista-info-usuarios))) (equal? contraseña (get-contraseña (car lista-info-usuarios))))
            #t
            ; ¿El resto de listas de usuario es nulo?
            (if (null? (cdr lista-info-usuarios))
                #f
                ; Llamado recursivo entregando el resto de listas de usuario en la lista "base" y la contraseña
                (usuario-contraseña-valida? (cdr lista-info-usuarios) usuario contraseña))
            )
        )
    )
  )
  

; Descripción: Función que busca, mediante recursión natural/lineal, un usuario cuyo parámetro indicador de autentificador en su correspondiente lista de información...
;              ... refleje que este se encuentre activo (1)
; Dominio: Lista base de usuarios
; Recorrido: Usuario (caso verdadero, es decir, exista y esté activo) o el valor Booleano indicador de falso (caso contrario, es decir, no existe i no está activo)
; Tipo de recursión: Recursión natural/lineal

(define buscar-usuario-activo
  (lambda (lista-base)
    ; ¿El usuario de primera lista de usuarios contenida posee su correspondiente valor booleano activo?
    (if (equal? 1 (get-act-ina-sesión (car lista-base)))
        (get-usuario (car lista-base))
        ; ¿El resto de listas de usuario es nulo?
        (if (null? (cdr lista-base))
            #f
            ; Llamado recursivo entregando el resto de listas de usuario en la lista "base"
            (buscar-usuario-activo (cdr lista-base))
            )
        )
    )
  )

; Descripción: Función que convierte y une la información de un usuario especifíco (Nombre y fecha de creación del usuario) a string
; Dominio: Lista de usuarios X usuario
; Recorrido: String
; Tipo de recursión: Recursión natural/lineal

(define usuario->string
  (lambda (lista-base-usuarios usuario)
    (if (null? lista-base-usuarios)
        #f
        (if (equal? usuario (get-usuario (car lista-base-usuarios)))
            (string-append "Nombre de usuario: " usuario "\n"
                           "Fecha de creación: " (fecha->string (get-fecha (car lista-base-usuarios))) "\n")
            (if (null? (cdr lista-base-usuarios))
                #f
                (usuario->string (cdr lista-base-usuarios) usuario))
            )
        )
    )
  )

; Descripción: Función que convierte y une la información todos los usuarios (Nombres y fechas de creación de los usuarios) a string en una lista...
;              ... (aplicar "string-join" al resultado para expresar el string)
; Dominio: Lista de usuarios
; Recorrido: lista
; Tipo de recursión: Recursión natural/lineal

(define usuarios->string
  (lambda (lista-base-usuarios)
    (if (null? lista-base-usuarios)
        null
        (cons (string-append "Nombre de usuario: " (get-usuario (car lista-base-usuarios)) "\n"
                             "Fecha de creación: " (fecha->string (get-fecha (car lista-base-usuarios))) "\n")
              (usuarios->string (cdr lista-base-usuarios)))
        )
    )
  )

;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;EJEMPLOS CONSTRUCTOR:
(define ejemplo-crear-usuario-1 (crear-usuario (date 19 10 2021) "Angel" "contraseña"))
(define ejemplo-crear-usuario-2 (crear-usuario (date 20 10 2021) "Jaime" "pinturaceresita"))
(define ejemplo-crear-usuario-3 (crear-usuario (date 19 12 2021) "Fifi" 6))
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
; ...previamente definida la plataforma a emplear: "(define ejemplo-paradigmadocs (paradigmadocs "gDocs" 16 10 2021 encryptFn encryptFn))"
(define ejemplo-set-act-list-usuarios-paradigmadocs-1 (set-act-list-usuarios-paradigmadocs ejemplo-paradigmadocs ejemplo-crear-usuario-1))
(define ejemplo-set-act-list-usuarios-paradigmadocs-2 (set-act-list-usuarios-paradigmadocs ejemplo-paradigmadocs ejemplo-crear-usuario-2))
(define ejemplo-set-act-list-usuarios-paradigmadocs-3 (set-act-list-usuarios-paradigmadocs ejemplo-set-act-list-usuarios-paradigmadocs-2 ejemplo-crear-usuario-1))

;EJEMPLOS OTRAS FUNCIONES:
; ...previamente definida la plataforma a emplear: "(define ejemplo-paradigmadocs (paradigmadocs "gDocs" 16 10 2021 encryptFn encryptFn))"


(define ejemplo-esta-usuario?-1 (esta-usuario? (get-dato ejemplo-paradigmadocs 4) "Angel"))
(define ejemplo-esta-usuario?-2 (esta-usuario? (get-dato ejemplo-paradigmadocs 4) "Jaime"))
(define ejemplo-esta-usuario?-3 (esta-usuario? (get-dato ejemplo-set-act-list-usuarios-paradigmadocs-1 4) "Angel"))

; Las siguientes funciones "set-act-ina-usuario" y "remover-list-usuario-act-ina" funcionan de forma complementaria
;; ACTIVA USUARIO "Jaime"
(define ejemplo-set-act-ina-usuario-1 (set-act-ina-usuario (get-dato ejemplo-set-act-list-usuarios-paradigmadocs-3 4) "Jaime" ejemplo-set-act-list-usuarios-paradigmadocs-3))
;; ELIMINA USUARIO INACTIVO "Jaime"
(define ejemplo-remover-list-usuario-act-ina-1 (remover-list-usuario-act-ina (get-dato ejemplo-set-act-ina-usuario-1 4) "Jaime" ejemplo-set-act-ina-usuario-1))
;; ACTIVA USUARIO "Angel"
(define ejemplo-set-act-ina-usuario-2 (set-act-ina-usuario (get-dato ejemplo-set-act-list-usuarios-paradigmadocs-3 4) "Angel" ejemplo-set-act-list-usuarios-paradigmadocs-3))
;; ELIMINA USUARIO INACTIVO "Angel"      
(define ejemplo-remover-list-usuario-act-ina-2 (remover-list-usuario-act-ina (get-dato ejemplo-set-act-ina-usuario-2 4) "Angel" ejemplo-set-act-ina-usuario-2))
;; DESACTIVA USUARIO "Angel" (proceso inverso a lo anterior)
(define ejemplo-set-act-ina-usuario-3 (set-act-ina-usuario (get-dato ejemplo-remover-list-usuario-act-ina-2 4) "Angel" ejemplo-remover-list-usuario-act-ina-2))
;; ELIMINA USUARIO ACTIVO "Angel"
(define ejemplo-remover-list-usuario-act-ina-3 (remover-list-usuario-act-ina (get-dato ejemplo-set-act-ina-usuario-3 4) "Angel" ejemplo-set-act-ina-usuario-3))

(define ejemplo-set-n-remov-1 (set-n-remov "Jaime" ejemplo-set-act-list-usuarios-paradigmadocs-3))
(define ejemplo-set-n-remov-2 (set-n-remov "Angel" ejemplo-set-act-list-usuarios-paradigmadocs-3))
(define ejemplo-set-n-remov-3 (set-n-remov "Angel" ejemplo-set-n-remov-2))

;(define ejemplo-esta-contraseña?-1 (esta-contraseña? (get-dato ejemplo-set-act-list-usuarios-paradigmadocs-3 4) "pinturaceresita"))
;(define ejemplo-esta-contraseña?-2 (esta-contraseña? (get-dato ejemplo-set-act-list-usuarios-paradigmadocs-3 4) "contraseña"))
;(define ejemplo-esta-contraseña?-3 (esta-contraseña? (get-dato ejemplo-set-act-list-usuarios-paradigmadocs-3 4) "imaginar usar esta FN igualando usuario y contraseña UwU"))

(define ejemplo-buscar-usuario-activo-1 (buscar-usuario-activo (get-dato ejemplo-set-act-list-usuarios-paradigmadocs-3 4)))
(define ejemplo-buscar-usuario-activo-2 (buscar-usuario-activo (get-dato ejemplo-set-n-remov-2 4)))
(define ejemplo-buscar-usuario-activo-3 (buscar-usuario-activo (get-dato ejemplo-set-n-remov-3 4)))

(define ejemplo-usuario->string-1 (usuario->string (get-dato ejemplo-set-n-remov-2 4) "Jaime"))
(define ejemplo-usuario->string-2 (usuario->string (get-dato ejemplo-set-act-list-usuarios-paradigmadocs-1 4) "Angel"))
(define ejemplo-usuario->string-3 (usuario->string (list (list (list 19 10 2021) "user1" "pass1" 0)
                                                         (list (list 20 10 2021) "user2" "pass2" 0)
                                                         (list (list 21 10 2021) "user3" "pass3" 0)
                                                         (list (list 22 10 2021) "user4" "pass4" 0))
                                                   "user3"))
(define ejemplo-usuarios->string-1 (usuarios->string (get-dato ejemplo-set-n-remov-2 4)))
(define ejemplo-usuarios->string-2 (usuarios->string (get-dato ejemplo-set-act-list-usuarios-paradigmadocs-1 4)))
(define ejemplo-usuarios->string-3 (usuarios->string (list (list (list 19 10 2021) "user1" "pass1" 0)
                                                         (list (list 20 10 2021) "user2" "pass2" 0)
                                                         (list (list 21 10 2021) "user3" "pass3" 0)
                                                         (list (list 22 10 2021) "user4" "pass4" 0))))
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


(provide (all-defined-out))
