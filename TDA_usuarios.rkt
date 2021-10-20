#lang racket

(require "TDA_fecha.rkt")

; Implementación del TDA usuarios

; Representación:
; (integer X integer X integer) (strings) (strings) (integers)
; (list dias meses años) (list usuarios) (list contraseñas) (list activo-inactivo)

; Constructor:
; Descripción: Permite la creación de un usuario nuevo mediante la solicitud de los datos...
;              ...fecha, usuario y contraseña
; Dominio: (integer X integer X integer X string X string)
; Recorrido: Lista con los datos

(define crear-usuario
  (lambda (dia mes año usuario contraseña)
    (if (fecha? (crear-fecha dia mes año))
        (list (crear-fecha dia mes año) usuario contraseña)
        '())))

; Pertenencia:
; Descripción: Función que permite verificar si una usuario (información) se encuentra bien definido
; Dominio: Lista correspondiente a la información de un usuario
; Recorrido: Booleando verificador

(define usuario?
  (lambda (list-info-usuario)
    (if (fecha? (car list-info-usuario))
        (if (string? (car (cdr list-info-usuario)))
            (if (string? (car (cdr (cdr list-info-usuario))))
                #t
                #f)
        #f)#f)))

; Selección:
; Descripción: Función que permite obtener la fecha registrada
; Dominio: Lista de información del usuario
; Recorrido: Fecha registrada

(define get-fecha
  (lambda (lista-info-usuario)
    (car lista-info-usuario)))

; Descripción: Función que permite obtener el usuario registrado
; Dominio: Lista de información del usuario
; Recorrido: Usuario registrado

(define get-usuario
  (lambda (lista-info-usuario)
    (car (cdr lista-info-usuario))))

; Descripción: Función que permite obtener la contraseña registrada
; Dominio: Lista de información del usuario
; Recorrido: Contraseña registrada

(define get-contraseña
  (lambda (lista-info-usuario)
    (car (cdr (cdr lista-info-usuario)))))

; Modificador:
; Descripción: Función que permite modificar el usuario registrado mediante...
;              ...la creación de una nueva lista conservando los datos que no sean el usuario
; Dominio: Lista de información del usuario
; Recorrido: Lista correspondiente a la información del usuario modificada (usuario)

(define set-usuario
  (lambda (lista-info-usuario nuevo-usuario)
    (if (usuario? (list (get-fecha lista-info-usuario) nuevo-usuario (get-contraseña lista-info-usuario)))
        (list (get-fecha lista-info-usuario) nuevo-usuario (get-contraseña lista-info-usuario))
        #f)))

; Descripción: Función que permite modificar la contraseña registrada mediante...
;              ...la creación de una nueva lista conservando los datos que no sean la contraseña
; Dominio: Lista de información del usuario
; Recorrido: Lista correspondiente a la información del usuario modificada (contraseña)

(define set-contraseña
  (lambda (lista-info-usuario nueva-contraseña)
    (if (usuario? (list (get-fecha lista-info-usuario) (get-usuario lista-info-usuario) nueva-contraseña))
        (list (get-fecha lista-info-usuario) (get-usuario lista-info-usuario) nueva-contraseña)
        #f)))





;EJEMPLOS CONSTRUCTOR:
(crear-usuario 19 10 2021 "Angel" "contraseña")
(crear-usuario 20 10 2021 "Jaime" "pinturaceresita")
(crear-usuario 19 23 2021 "Fifi" "tostador") ;Este ejemplo expresa una situación no valida ya que no existe un mes 23

;EJEMPLOS PERTENENCIA:
(usuario? (crear-usuario 19 10 2021 "Angel" "contraseña"))
(usuario? (crear-usuario 20 10 2021 "Jaime" "pinturaceresita"))
(usuario? (crear-usuario 21 12 2021 "Fifi" "tostador"))

;EJEMPLOS SELECTOR:
(get-fecha (crear-usuario 19 10 2021 "Angel" "contraseña"))
(get-fecha (crear-usuario 20 10 2021 "Jaime" "pinturaceresita"))
(get-fecha (crear-usuario 21 12 2021 "Fifi" "tostador"))



