#lang racket

; Implementación del TDA fecha

; Representación
; (integer X integer X integer)
; (list dia mes año)

; Constructor: Función que permite la creación de una fecha en formato DD MM AA de acuerdo con...
;              ... tres valores enteros dados representando al dia, mes y año respectivamente
; Dominio: Lista de tres valores/números enteros
; Recorrido: Lista correspondiente a la fecha

(define crear-fecha
  (lambda (dia mes año)
    (if (fecha? (list dia mes año))
        (list dia mes año)
        null)
    )
  )

; Pertenencia:
; Descripción: Función que permite veríficar si los elementos ingresados corresponden a los...
;              ... considerados en el constructor de fechas
; Dominio: Lista fecha
; Recorrido: Valor booleano que confirma o no 

(define fecha?
  (lambda (fecha)
    (if (list? fecha)
        (if (= (length fecha) 3)
            (if (and (integer? (car fecha)) (integer? (car (cdr fecha))) (integer? (car (cdr (cdr fecha)))))
                (if (and (> (car fecha) 0) (<= (car fecha) (dias-del-mes (car (cdr fecha)) (car (cdr (cdr fecha))))))
                    (if (and (< (car (cdr fecha)) 13) (> (car (cdr fecha)) 0))
                        (if (> (car (cdr (cdr fecha))) 2020)
                            #t
                            #f)
                        #f)
                    #f)
                #f)
            #f)
        #f)
    )
  )

; Selectores (es importante destacar que no se emplean filtros verificadores de datos ya que se encuentran...
; ... en las funciones contructor y pertenencia las cuales deben ejecutarse previamente a los selectores):
; Descripción: Función que permite la obtención del día registrado
; Dominio: Lista correspondiente a la fecha en formato DD MM AA
; Recorrido: Entero correspondiente al día

(define get-dia
  (lambda (fecha)
    (if (fecha? fecha)
        (car fecha)
        #f)
    )
  )

; Descripción: Función que permite la obtención del mes registrado
; Dominio: Lista correspondiente a la fecha en formato DD MM AA
; Recorrido: Entero correspondiente al mes

(define get-mes
  (lambda (fecha)
    (if (fecha? fecha)
        (car (cdr fecha))
        #f)
    )
  )

; Descripción: Función que permite la obtención del año registrado
; Dominio: Lista correspondiente a la fecha en formato DD MM AA
; Recorrido: Entero correspondiente al año

(define get-año
  (lambda (fecha)
    (if (fecha? fecha)
        (car (cdr (cdr fecha)))
        #f)
    )
  )

; Modificadores:
; Descripción: Función que permite la modificación de una fecha mediante la creación de un fecha nueva...
;              ... cambiando el dato correspondiente, en este caso, el día
; Dominio: Lista correspondiente a la fecha a modificar y el día a registrar
; Recorrido: Lista correspondiente a la fecha ya modificada (dia)

(define set-dia
  (lambda (fecha nuevo-dia)
    (if (fecha? (list nuevo-dia (get-mes fecha) (get-año fecha)))
        (list nuevo-dia (get-mes fecha) (get-año fecha))
              #f)
    )
  )

; Descripción: Función que permite la modificación de una fecha mediante la creación de un fecha nueva...
;              ... cambiando el dato correspondiente, en este caso, el mes
; Dominio: Lista correspondiente a la fecha a modificar y el mes a registrar
; Recorrido: Lista correspondiente a la fecha ya modificada (mes)

(define set-mes
  (lambda (fecha nuevo-mes)
    (if (fecha? (list (get-dia fecha) nuevo-mes (get-año fecha)))
        (list (get-dia fecha) nuevo-mes (get-año fecha))
              #f)))

; Descripción: Función que permite la modificación de una fecha mediante la creación de un fecha nueva...
;              ... cambiando el dato correspondiente, en este caso, el año
; Dominio: Lista correspondiente a la fecha a modificar y el año a registrar
; Recorrido: Lista correspondiente a la fecha ya modificada (año)

(define set-año
  (lambda (fecha nuevo-año)
    (if (fecha? (list (get-dia fecha) (get-mes fecha) nuevo-año))
        (list (get-dia fecha) (get-mes fecha) nuevo-año)
              #f)))

; Otras funciones:

; Descripción: La siguiente función permite determinar si un año posee 365 días o 366 días (bisiesto cada cuatro años)...
;              ... de esta forma, los días del mes a excepción de febrero se mantienen constantes. Sin embargo, si el...
;              ... año "actual" es bisiesto, febrero tendrá 29 días, caso contrario serán 28 días.
; Dominio: Un valor entero correspondiente al año
; Recorrido: Un resultado booleando el cual indica si el año ingresado posee 365 días o 366 días (bisiesto)

(define (bisiesto? año)
  (if (exact-integer?(/ año 4)) #t #f))

; Descripción: Función que considera la obtención de los días de un mes en consideración con si el año ingresado...
;              ... es bisiesto o no
; Dominio: Dos valores enteros correspondientes al mes y al año respectivamente
; Recorrido: Un entero correspondiente a los días del mes ingresado considerando el año (bisiesto o no)

(define (dias-del-mes mes año)
  (if (or (= mes 1) (= mes 3) (= mes 5) (= mes 7) (= mes 8) (= mes 10) (= mes 12)) 31
      (if (= mes 2)
          (if (bisiesto? año) 29 28)
          30)))

#|
------------------------------------------------------------------------------------------------------
;EJEMPLOS CONSTRUCTOR:
(define ejemplo-crear-fecha-1 (crear-fecha 18 10 2021))
(define ejemplo-crear-fecha-2 (crear-fecha 22 11 2021))
(define ejemplo-crear-fecha-3 (crear-fecha 29 2 2021))
; El tercer ejemplo representa una situación no valida. Es importante comentar que el programa no seguirá...
; ... ejecutando funciones sobre una situación no valida, sin embargo, para ejemplificar se ejecutarán

;EJEMPLOS PERTENENCIA:
(define ejemplo-fecha?-1 (fecha? ejemplo-crear-fecha-1))
(define ejemplo-fecha?-2 (fecha? ejemplo-crear-fecha-2))
(define ejemplo-fecha?-3 (fecha? ejemplo-crear-fecha-3))
; El tercer ejemplo representa una situación no valida

;EJEMPLOS SELECTORES:
(define ejemplo-get-dia-1 (get-dia ejemplo-crear-fecha-1))
(define ejemplo-get-dia-2 (get-dia ejemplo-crear-fecha-2))
(define ejemplo-get-dia-3 (get-dia ejemplo-crear-fecha-3))
; El tercer ejemplo representa una situación no valida

(define ejemplo-get-mes-1 (get-mes ejemplo-crear-fecha-1))
(define ejemplo-get-mes-2 (get-mes ejemplo-crear-fecha-2))
(define ejemplo-get-mes-3 (get-mes ejemplo-crear-fecha-3))
; El tercer ejemplo representa una situación no valida

(define ejemplo-get-año-1 (get-año ejemplo-crear-fecha-1))
(define ejemplo-get-año-2 (get-año ejemplo-crear-fecha-2))
(define ejemplo-get-año-3 (get-año ejemplo-crear-fecha-3))
; El tercer ejemplo representa una situación no valida

;EJEMPLOS MODIFICADORES:
(define ejemplo-set-dia-1 (set-dia ejemplo-crear-fecha-1 20))
(define ejemplo-set-dia-2 (set-dia ejemplo-crear-fecha-2 30))
(define ejemplo-set-dia-3 (set-dia ejemplo-crear-fecha-3 0))
; El tercer ejemplo expresa una situación no valida

(define ejemplo-set-mes-1 (set-mes ejemplo-crear-fecha-1 2))
(define ejemplo-set-mes-2 (set-mes ejemplo-crear-fecha-2 12))
(define ejemplo-set-mes-3 (set-mes ejemplo-crear-fecha-3 13))
; El tercer ejemplo representa una situación no valida

(define ejemplo-set-año-1 (set-año ejemplo-crear-fecha-1 2022))
(define ejemplo-set-año-2 (set-año ejemplo-crear-fecha-2 2024))
(define ejemplo-set-año-3 (set-año ejemplo-crear-fecha-3 2020))
; El tercer ejemplo representa una situación no valida

;EJEMPLOS OTRAS FUNCIONES:
(define ejemplo-bisiesto?-1 (bisiesto? 2021))
(define ejemplo-bisiesto?-2 (bisiesto? 2024))
(define ejemplo-bisiesto?-3 (bisiesto? 2030))

(define ejemplo-dias-del-mes-1 (dias-del-mes 2 2021))
(define ejemplo-dias-del-mes-2 (dias-del-mes 2 2024))
(define ejemplo-dias-del-mes-3 (dias-del-mes 12 2030))
------------------------------------------------------------------------------------------------------
|#

(provide (all-defined-out))