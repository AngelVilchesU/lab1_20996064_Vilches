#lang racket

; Implementación del TDA fecha

; Representación
; (integer X integer X integer)
; (list dia mes año)

; Constructor: Función que permite la creación de una fecha en formato DD MM AA de acuerdo con...
;              ... tres valores enteros dados representando al dia, mes y año respectivamente
; Dominio: integer X integer X integer
; Recorrido: Lista correspondiente a la fecha

(define date
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
; Recorrido: Valor booleano que confirma, o no, la validez de la fecha

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
; Dominio: Lista de enteros correspondiente a la fecha en formato DD MM AA
; Recorrido: Entero correspondiente al día

(define get-dia
  (lambda (fecha)
    (if (fecha? fecha)
        (car fecha)
        #f)
    )
  )

; Descripción: Función que permite la obtención del mes registrado
; Dominio: Lista de enteros correspondiente a la fecha en formato DD MM AA
; Recorrido: Entero correspondiente al mes

(define get-mes
  (lambda (fecha)
    (if (fecha? fecha)
        (car (cdr fecha))
        #f)
    )
  )

; Descripción: Función que permite la obtención del año registrado
; Dominio: Lista de enteros correspondiente a la fecha en formato DD MM AA
; Recorrido: Entero correspondiente al año

(define get-año
  (lambda (fecha)
    (if (fecha? fecha)
        (car (cdr (cdr fecha)))
        #f)
    )
  )

; Modificadores:
; N/A

; Otras funciones:

; Descripción: La siguiente función permite determinar si un año posee 365 días o 366 días (bisiesto cada cuatro años)...
;              ... de esta forma, los días del mes a excepción de febrero se mantienen constantes. Sin embargo, si el...
;              ... año "actual" es bisiesto, febrero tendrá 29 días, caso contrario serán 28 días.
; Dominio: Un valor entero correspondiente al año
; Recorrido: Un resultado booleando el cual indica si el año ingresado posee 365 días o 366 días (bisiesto)

(define (bisiesto? año)
  (if (exact-integer? (/ año 4))
      #t
      #f)
  )

; Descripción: Función que considera la obtención de los días de un mes en consideración con si el año ingresado...
;              ... es bisiesto o no
; Dominio: Dos valores enteros correspondientes al mes y al año respectivamente
; Recorrido: Un entero correspondiente a los días del mes ingresado considerando el año (bisiesto o no)

(define (dias-del-mes mes año)
  (if (or (= mes 1) (= mes 3) (= mes 5) (= mes 7) (= mes 8) (= mes 10) (= mes 12))
      31
      (if (= mes 2)
          (if (bisiesto? año)
              29
              28)
          30)
      )
  )

; Descripción: Función que convierte una fecha (DD:MM:AA) como una lista de números enteros a un string
; Dominio: Lista correspondiente a la fecha
; Recorrido: String

(define fecha->string
  (lambda (fecha)
    (string-append (number->string (get-dia fecha)) "-" (number->string (get-mes fecha)) "-" (number->string (get-año fecha)))
    )
  )


;---------------------------------------------------------------------------------------------------------------------------------------------------------
;EJEMPLOS CONSTRUCTOR:
(define ejemplo-date-1 (date 18 10 2021))
(define ejemplo-date-2 (date 22 11 2021))
(define ejemplo-date-3 (date 29 2 2021))
; El tercer ejemplo representa una situación no valida. Es importante comentar que el programa no seguirá...
; ... ejecutando funciones sobre una situación no valida, sin embargo, para ejemplificar se ejecutarán

;EJEMPLOS PERTENENCIA:
(define ejemplo-fecha?-1 (fecha? ejemplo-date-1))
(define ejemplo-fecha?-2 (fecha? ejemplo-date-2))
(define ejemplo-fecha?-3 (fecha? ejemplo-date-3))
; El tercer ejemplo representa una situación no valida

;EJEMPLOS SELECTORES:
(define ejemplo-get-dia-1 (get-dia ejemplo-date-1))
(define ejemplo-get-dia-2 (get-dia ejemplo-date-2))
(define ejemplo-get-dia-3 (get-dia ejemplo-date-3))
; El tercer ejemplo representa una situación no valida

(define ejemplo-get-mes-1 (get-mes ejemplo-date-1))
(define ejemplo-get-mes-2 (get-mes ejemplo-date-2))
(define ejemplo-get-mes-3 (get-mes ejemplo-date-3))
; El tercer ejemplo representa una situación no valida

(define ejemplo-get-año-1 (get-año ejemplo-date-1))
(define ejemplo-get-año-2 (get-año ejemplo-date-2))
(define ejemplo-get-año-3 (get-año ejemplo-date-3))
; El tercer ejemplo representa una situación no valida

;EJEMPLOS MODIFICADORES:
; N/A

;EJEMPLOS OTRAS FUNCIONES:
(define ejemplo-bisiesto?-1 (bisiesto? 2021))
(define ejemplo-bisiesto?-2 (bisiesto? 2024))
(define ejemplo-bisiesto?-3 (bisiesto? 2030))

(define ejemplo-dias-del-mes-1 (dias-del-mes 2 2021))
(define ejemplo-dias-del-mes-2 (dias-del-mes 2 2024))
(define ejemplo-dias-del-mes-3 (dias-del-mes 12 2030))

(define ejemplo-fecha->string-1 (fecha->string ejemplo-date-1))
(define ejemplo-fecha->string-2 (fecha->string ejemplo-date-2))
(define ejemplo-fecha->string-3 (fecha->string (date 15 02 2022)))
;---------------------------------------------------------------------------------------------------------------------------------------------------------


(provide (all-defined-out))