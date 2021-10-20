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
                (if (and (integer? (car fecha)) (> (car fecha) 0))
                    (if (and (integer? (car (cdr fecha))) (< (car (cdr fecha)) 13) (> (car (cdr fecha)) 0))
                        (if (and (integer? (car (cdr (cdr fecha)))) (> (car (cdr (cdr fecha))) 2020))
                            #t
                            #f)
                        #f)
                    #f)
                #f)
            #f)
        #f)
    ))

; Selectores (es importante destacar que no se emplean filtros verificadores de datos ya que se encuentran...
; ... en las funciones contructor y pertenencia las cuales deben ejecutarse previamente a los selectores):
; Descripción: Función que permite la obtención del día registrado
; Dominio: Lista correspondiente a la fecha en formato DD MM AA
; Recorrido: Entero correspondiente al día

(define get-dia
  (lambda (fecha)
    (car fecha)))

; Descripción: Función que permite la obtención del mes registrado
; Dominio: Lista correspondiente a la fecha en formato DD MM AA
; Recorrido: Entero correspondiente al mes

(define get-mes
  (lambda (fecha)
    (car (cdr fecha))))

; Descripción: Función que permite la obtención del año registrado
; Dominio: Lista correspondiente a la fecha en formato DD MM AA
; Recorrido: Entero correspondiente al año

(define get-año
  (lambda (fecha)
    (car (cdr (cdr fecha)))))

; Modificadores:
; Descripción: Función que permite la modificación de una fecha mediante la creación de un fecha nueva...
;              ... cambiando el dato correspondiente, en este caso, el día
; Dominio: Lista correspondiente a la fecha a modificar y el día a registrar
; Recorrido: Lista correspondiente a la fecha ya modificada (dia)

(define set-dia
  (lambda (fecha nuevo-dia)
    (if (fecha? (list nuevo-dia (get-mes fecha) (get-año fecha)))
        (list nuevo-dia (get-mes fecha) (get-año fecha))
              #f)))

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
(crear-fecha 18 10 2021)
(crear-fecha 22 11 2021)
(crear-fecha 0 13 2020) ;Este ejemplo expresa una situación no valida ya que no existe un día enumerado como 0, un mes 13 ni nos encontramos en el año 2020

;EJEMPLOS PERTENENCIA:
(fecha? (list 18 10 2021))
(fecha? (list 22 11 2021))
(fecha? (list 18 10 2021 0)) ;Este ejemplo expresa una situación no valida ya que no se respeta el formato fecha DD MM AA como lista de enteros

;EJEMPLOS SELECTORES:
(get-dia (list 18 10 2021))
(get-dia (list 22 11 2021))
(get-dia (list 24 12 2021))
(get-mes (list 18 10 2021))
(get-mes (list 22 11 2021))
(get-mes (list 24 12 2021))
(get-año (list 18 10 2022))
(get-año (list 18 10 2023))

;EJEMPLOS MODIFICADORES:
(set-dia (list 18 10 2021) 20)
(set-dia (list 18 10 2021) 30)
(set-dia (list 18 10 2021) 0) ;Este ejemplo expresa una situación no valida ya que no existe este día (tambien aplica al limite de dias de determinado mes sea bisiesto o no)
(set-mes (list 18 10 2021) 2)
(set-mes (list 18 10 2021) 12)
(set-mes (list 18 10 2021) 13) ;Este ejemplo expresa una situación no valida ya que no existe este mes
(set-año (list 18 10 2021) 2022)
(set-año (list 18 10 2021) 2024)
(set-año (list 18 10 2021) 2020) ;Este ejemplo expresa una situación no valida ya que el año ya no aplica (actualizable)

;EJEMPLOS OTRAS FUNCIONES:
(bisiesto? 2021)
(bisiesto? 2024)
(bisiesto? 2030)
(dias-del-mes 2 2021)
(dias-del-mes 2 2024)
(dias-del-mes 12 2030)
------------------------------------------------------------------------------------------------------
|#

(provide (all-defined-out))