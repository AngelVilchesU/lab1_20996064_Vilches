#lang racket

(require "TDA_paradigmadocs.rkt")

; Implementación del menú principal

; Representación:
; (string X date X string X string)

; Constructor:
; Descripción: Permite el registro de un usuario a la plataforma creada mediante la solicitud de el nombre de...
;              ... el nombre de la plataforma, fecha de creación, nombre de usuario y contraseña.
; Dominio: paradigmadocs X date X string X string
; Recorrido: Actualización de paradigmadocs
; Tipo de recursión: Natural pendiente

(define register
  (lambda (plataforma dia mes año usuario contraseña)
  (get-dato plataforma 6)))

