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
    (if (and (integer? dia) (> dia 0))
        (if (and (integer? mes) (< mes 13) (> mes 0))
            (if (and (integer? año) (> año 2020))
                (list dia mes año)
                null)
            null)
        null)
    ))

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
                #t
                #f)
            #f)
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
EJEMPLOS CONSTRUCTOR:
(crear-fecha 18 10 2021)
(crear-fecha 22 11 2021)
(crear-fecha 0 13 2020) ;Este ejemplo expresa una situación no valida ya que no existe un día enumerado como 0, un mes 13 ni nos encontramos en el año 2020

EJEMPLOS PERTENENCIA:
(fecha? (list 18 10 2021))
(fecha? (list 22 11 2021))
(fecha? (list 18 10 2021 0)) ;Este ejemplo expresa una situación no valida ya que no se respeta el formato fecha DD MM AA como lista de enteros



|#