#lang racket

;;Función que hace la supersuma de un número dado.
;;japones: number -> number
(define (supersuma x)
  (if  (< x 10) x
  [supersuma (suma-digitos x)]
  )
)
;;función auxiliar para supersuma-
(define (suma-digitos n)
  (if (zero? n) 0
  [+ (modulo n 10) (suma-digitos (truncate(/ n 10)))])
)

;; Función que regresa la representación japonesa de un número del 0-99.
;; Se puede mejorar, leer de izquierda a derecha, concatenar con lo correspondiente.
;; japones: number -> string
(define (japones n)
  (cond 
   [(= n 0) "rei "]
   [(= n 1) "ichi"]
   [(= n 2) "ni"]
   [(= n 3) "san"]
   [(= n 4) "yon"]
   [(= n 5) "go"]
   [(= n 6) "roku"]
   [(= n 7) "nana"]
   [(= n 8) "hachi"]
   [(= n 9) "kyu"]
   [(= n 10) "ju"]
   [(< n 20) (string-append  "ju " (japones (modulo n 10)))] 
   [(> n 19) (string-append (japones(truncate(/ n 10))) " ju "
                            (cond
                              [(= (modulo n 10) 0) ""]
                              [(japones (modulo n 10))])
                            )]
   ))

;; Función que regresa la representación japonesa en kanji de un número del 0-99.
;; japones-kanji: number -> string
(define (japones-kanji n)
  (cond 
   [(= n 0) "零"]
   [(= n 1) "一"]
   [(= n 2) "二"]
   [(= n 3) "三"]
   [(= n 4) "四"]
   [(= n 5) "五"]
   [(= n 6) "六"]
   [(= n 7) "七"]
   [(= n 8) "八"]
   [(= n 9) "九"]
   [(= n 10) "十"]
   [(< n 20) (string-append  "十" (japones-kanji (modulo n 10)))] 
   [(> n 19) (string-append (japones-kanji(truncate(/ n 10))) "十"
                            (cond
                              [(= (modulo n 10) 0) ""]
                              [(japones-kanji (modulo n 10))])
                            )]
   ))