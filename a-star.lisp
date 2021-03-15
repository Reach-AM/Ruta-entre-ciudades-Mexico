; *** Astar.lisp                              ***
; *** Código lisp para encontrar la ruta más  ***
; *** corta entre ciudades de México.         ***

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; **** CIUDADES Y DISTANCIAS ****
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((ciudades (make-hash-table :size 20)))              ; Tabla de ciudades
  (defun set-distancia (x y) (setf (gethash x ciudades) y))
  (defun get-distancia (x) (gethash x ciudades)))

(let ((caminos (make-hash-table :size 20)))               ; Tabla de caminos recorridas
  (defun set-ancestro (x y) (setf (gethash x caminos) y))
  (defun get-ancestro (x) (gethash x caminos)))

;Ciudades de Ciudad de Mexico
(set-distancia 'CDMX '((Iztapalapa . 14)(GustavoAMadero . 9)(MagdalenaContreras . 27)(MilpaAlta . 39)(Azcapotzalco . 13)(VenustianoCarranza . 7)(MiguelHidalgo . 9)(Coyoacan . 13)(AlvaroObregon . 16)))
(set-distancia 'Iztapalapa '((CDMX . 14)(GustavoAMadero . 20)(MagdalenaContreras . 25)(MilpaAlta . 32)(Azcapotzalco . 26)(VenustianoCarranza . 11)(MiguelHidalgo . 21)(Coyoacan . 11)(AlvaroObregon . 19)))
(set-distancia 'GustavoAMadero '((CDMX . 9)(Iztapalapa . 20)(MagdalenaContreras . 35)(MilpaAlta . 50)(Azcapotzalco . 15)(VenustianoCarranza . 11)(MiguelHidalgo . 15)(Coyoacan . 25)(AlvaroObregon . 24)))
(set-distancia 'MagdalenaContreras '((CDMX . 27)(Iztapalapa . 25)(GustavoAMadero . 35)(MilpaAlta . 38)(Azcapotzalco . 30)(VenustianoCarranza . 30)(MiguelHidalgo . 23)(Coyoacan . 15)(AlvaroObregon . 16)))
(set-distancia 'MilpaAlta '((CDMX . 39)(Iztapalapa . 32)(GustavoAMadero . 50)(MagdalenaContreras . 38)(Azcapotzalco . 51)(VenustianoCarranza . 42)(MiguelHidalgo . 49)(Coyoacan . 29)(AlvaroObregon . 44)))
(set-distancia 'Azcapotzalco '((CDMX . 13)(Iztapalapa . 26)(GustavoAMadero . 15)(MagdalenaContreras . 30)(MilpaAlta . 51)(VenustianoCarranza . 15)(MiguelHidalgo . 8)(Coyoacan . 23)(AlvaroObregon . 18)))
(set-distancia 'VenustianoCarranza '((CDMX . 7)(Iztapalapa . 11)(GustavoAMadero . 11)(MagdalenaContreras . 30)(MilpaAlta . 42)(Azcapotzalco . 15)(MiguelHidalgo . 15)(Coyoacan . 17)(AlvaroObregon . 20)))
(set-distancia 'MiguelHidalgo '((CDMX . 9)(Iztapalapa . 21)(GustavoAMadero . 15)(MagdalenaContreras . 23)(MilpaAlta . 49)(Azcapotzalco . 8)(VenustianoCarranza . 15)(Coyoacan . 17)(AlvaroObregon . 13)))
(set-distancia 'Coyoacan '((CDMX . 13)(Iztapalapa . 11)(GustavoAMadero . 25)(MagdalenaContreras . 15)(MilpaAlta . 29)(Azcapotzalco . 23)(VenustianoCarranza . 17)(MiguelHidalgo . 17)(AlvaroObregon . 12)))
(set-distancia 'AlvaroObregon '((CDMX . 16)(Iztapalapa . 19)(GustavoAMadero . 24)(MagdalenaContreras . 16)(MilpaAlta . 44)(Azcapotzalco . 18)(VenustianoCarranza . 20)(MiguelHidalgo . 13)(Coyoacan . 12)))


; *** Tablas de hash para recordar los valores           ***
; *** F y G que se vayan generando, según el             ***
; *** algoritmo de Geeks for Geeks:                      ***
; *** https://www.geeksforgeeks.org/a-search-algorithm/  ***
(let ((tabla-f (make-hash-table :size 20)))
  (defun set-f (x y) (setf (gethash x tabla-f) y))
  (defun get-f (x) (gethash x tabla-f))
)

(let ((tabla-g (make-hash-table :size 20)))
  (defun set-g (x y) (setf (gethash x tabla-g) y))
  (defun get-g (x) (gethash x tabla-g))
)

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
; **** TABLA DE COORDENADAS ****
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
(let ((cooredenadas (make-hash-table :size 20)))
  (defun set-coordenadas (x y) (setf (gethash x cooredenadas) y))
  (defun get-coordenadas (x) (gethash x cooredenadas)))

(set-coordenadas 'CDMX '(19.43 99.13))
(set-coordenadas 'Iztapalapa '(19.34 99.05))
(set-coordenadas 'GustavoAMadero '(19.48 99.12))
(set-coordenadas 'MagdalenaContreras '(19.31 99.24))
(set-coordenadas 'MilpaAlta '(19.19 99.02))
(set-coordenadas 'Azcapotzalco '(19.48 99.18))
(set-coordenadas 'VenustianoCarranza '(19.43 99.09))
(set-coordenadas 'MiguelHidalgo '(19.43 99.20))
(set-coordenadas 'Coyoacan '(19.34 99.15))
(set-coordenadas 'AlvaroObregon '(19.36 99.22))

(defun a-star (nodo-inicial nodo-final)                   ; algoritmo A*
  (set-meta nodo-final)                                   ; ¡OBJETO PARA GUARDAR EL NODO FINAL!
                                                          ; (PARA PODER ACCEDER A EL EN OTRAS FUNCIONES)

  (let ((abierto (list nodo-inicial))                       ; Inicializamos lista abierto y cerrado, el nodo x y sucesor
    (cerrado nil)
    nodo-actual
    sucesores)

    (set-ancestro nodo-inicial nil)                       ; Guardamos el nodo inicial en la tabla de caminos recorridos

    (set-g nodo-inicial 0)                                ; Inicializamos los valores g y f del primer nodo
    (set-f nodo-inicial (calcular-f nodo-inicial))        ; ¡FUNCIÓN PARA CALCULAR F y G!

    (loop                                                 ; Comienza propiamente el algoritmo
      (if (null abierto)(return 'imposible))              ; Si la lista abierta está vacía termina en fracaso

      (if (eql (first abierto) nodo-final)
          (setf nodo-actual (first abierto))
          (setf nodo-actual (buscar-mejor (first abierto) ; ¡FUNCIÓN RECURSIVA PARA BUSCAR UN NODO CON MENOR F!
                                          (rest abierto))))
      (format t "Procesando: ~a~%" nodo-actual)

      (setf abierto (remove nodo-actual abierto))         ; lo sacamos de abierto
      (push nodo-actual cerrado)                          ; y lo enviamos a cerrado

      (if (eql nodo-actual nodo-final)                    ; Si llegamos al nodo final terminamos
          (return (camino-final nodo-actual)))            ; ¡FUNCIÓN PARA REGRESAR UNA LISTA CON LOS NODOS DE LA RUTA!

      (setf sucesores                                     ; De otro modo, obtenemos los posibles sucesores de n
            (mapcar #'first (get-distancia nodo-actual))) ; Por medio de un mapcar de los primeros elementos de cada lista

      (dolist (sucesor sucesores)                         ; Para cada nodo sucesor revisa si aún no esta
        (if (not (or (member sucesor abierto)             ; ni en abierto
                     (member sucesor cerrado)))           ; ni en cerrado

          (progn                                          ; Si es TRUE :
           (incrementa-cuenta-nodos)                      ; aumenta un nodo a la cuenta
            (set-g sucesor
                (calcular-g sucesor nodo-actual))         ; Calculamos el valor g del nodo sucesor
            (set-f sucesor
                (calcular-f sucesor))                     ; Calculamos el valor f del nodo sucesor
            (format t "    Nuevo nodo: ~a - ~a~%" sucesor (get-f sucesor))
            (setf abierto (coloca sucesor abierto))       ; Metemos el nodo sucesor a los abiertos
            (set-ancestro sucesor nodo-actual))           ; Guardamos nodo-actual como el ancestro de sucesor

          (let*                                           ; Si es FALSE (Esta en abierto o cerrado):
            ((ans-anterior (get-ancestro sucesor))        ; Recupero el nodo que antes fue su ancestro

             (nuevo-f (if ans-anterior                    ; Reviso si es el nodo inicial (que no tiene ancestro)
                          (+ (- (get-f sucesor)
                                (get-g ans-anterior)
                                (or (rest (assoc ans-anterior (get-distancia sucesor))) 99999999))
                             (get-g nodo-actual)
                             (or (rest (assoc nodo-actual (get-distancia sucesor))) 99999999))
                          (get-f sucesor))))
            (format t "    Revisando nodo: ~a - ~a~%" sucesor (get-f sucesor))
            (format t "        Contra el nuevo: ~a~%" nuevo-f)
            (if (< nuevo-f (get-f sucesor))               ; En caso de que sea menor el nuevo-f que el anterior
                (progn
                  (set-g sucesor (+ (- (get-g sucesor)    ; Actualizo el valor de g al de la nueva conexión
                                       (get-f sucesor))
                                    nuevo-f))
                  (set-f sucesor nuevo-f)                 ; Actualizo el valor de f al del nuevo-f
                  (set-ancestro sucesor nodo-actual)      ; Cambio el nodo ancestro a este nodo

                  (if (member sucesor abierto)            ; Si estaba en abierto, lo saco y lo meto con su nuevo
                      (progn                              ; valor, en su nueva posición en la lista
                        (setf abierto
                              (remove sucesor abierto))
                        (setf abierto
                              (coloca sucesor abierto))))

                  (if (member sucesor cerrado)            ; Si estaba en cerrado, lo saco y lo regreso
                      (progn                              ; a abierto, para darle otra oportunidad
                        (setf abierto
                              (coloca sucesor abierto))
                        (setf cerrado
                              (remove sucesor cerrado))))

                ))                                        ; Aquí termina el if
          ))                                              ; Aquí termina el caso FALSE para abierto o cerrado
      )                                                   ; Aquí termina el dolist
    )                                                     ; Aquí termina el loop
))


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ************************************************ FUNCIONES EXTRA ************************************************
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ;;;;;;;;;;;;;;
; **** META ****
; ;;;;;;;;;;;;;;
;;; variable local de la meta.
(let (meta)
  (defun set-meta (x) (setf meta x))
  (defun get-meta () meta) )

; ;;;;;;;;;;;;;;;;;;;;
; **** CALCULAR-F ****
; ;;;;;;;;;;;;;;;;;;;;
(defun calcular-f (n)
  (+ (get-g n) (h n)))

; ;;;;;;;;;;;;;;;;;;;;
; **** CALCULAR-G ****
; ;;;;;;;;;;;;;;;;;;;;
(defun calcular-g (x y)
  (+ (get-g y)
     (or (rest (assoc y (get-distancia x))) 99999999)) )

; ;;;;;;;;;;;;;;;;;;;;
; **** CALCULAR-H ****
; ;;;;;;;;;;;;;;;;;;;;
(defun h (n)
  (sqrt (+ (expt (* (- (car (get-coordenadas n))
                       (car (get-coordenadas (get-meta)))) 110) 2)
              (expt (* (- (car (cdr (get-coordenadas n)))
                       (car (cdr (get-coordenadas (get-meta))))) 85) 2))))

; ;;;;;;;;;;;;;;;;;;;;;;
; **** BUSCAR-MEJOR **** Rodrigo
; ;;;;;;;;;;;;;;;;;;;;;;
; // Aquí, meta es equivalente al nodo final. Usar cualquiera que esté disponible
; (defun buscar-mejor (lst)	; Regresa el mejor nodo en "lst" para la expansión
;  (cond
;    ((null lst))
;    ((< (get-f (first lst)) (get-f (first (rest lst)))) (first lst))
;    ((eql (first lst) (get-meta)) (first lst))
;    (t (Buscar-mejor (rest lst)))))
(defun buscar-mejor (fst lst)
  (cond
    ((null lst) fst)
	  ((< (get-f fst) (get-f (first lst))) fst)
    ((eql (first lst) (get-meta)) (first lst))
    (t (buscar-mejor fst (rest lst)))))

; ;;;;;;;;;;;;;;;;
; **** COLOCA ****
; ;;;;;;;;;;;;;;;;
(defun coloca (nodo lst)
  (cond
    ((null lst)(list nodo))
    ((< (get-f nodo)
        (get-f (first lst)) )
     (cons nodo lst))
    (t (cons (first lst)
             (coloca nodo (rest lst))))))

; ;;;;;;;;;;;;;;;;;;;;;;
; **** CAMINO-FINAL **** Rodrigo
; ;;;;;;;;;;;;;;;;;;;;;;
; // Recibe el nodo actual (supuestamente el nodo final) y crea una lista n
; // añadiendo los nodos previos.
; Regresa una lista con el nodo inicial hasta la meta
(defun camino-final (n)
(cond
  ((null n) nil)
	(t (append (camino-final (get-ancestro n))
	           (list n)))))

; ;;;;;;;;;;;;;;;;;;;;;
; **** ACTUALIZA-F ****
; ;;;;;;;;;;;;;;;;;;;;;
;(defun actualiza-f (x y z)
;  (+ (- (get-f y);
;        (get-g z)
;        (or (rest (assoc z (get-distancia y))) 99999999))
;     (get-g x)
;     (or (rest (assoc x (get-distancia y))) 99999999)))

; ;;;;;;;;;;;;;;;;;;;;;;
; **** NUMERO-NODOS ****
; ;;;;;;;;;;;;;;;;;;;;;;
(let (NUMERO-NODOS)
  (defun iniciar-cuenta-nodos () (setf NUMERO-NODOS 0))
  (defun incrementa-cuenta-nodos () (incf NUMERO-NODOS))
  (defun get-numero-nodos () NUMERO-NODOS))


; ;;;;;;;;;;;;;;;;;;;;;;
; **** LLamada a la función ****
; ;;;;;;;;;;;;;;;;;;;;;;
(defun test ()
  (iniciar-cuenta-nodos)
  (format t "A-Estrella solution: ~s.~%"
    (a-star 'GustavoAMadero 'MilpaAlta))
  (format t "Path-length: ~s.~%"
    (get-f 'MilpaAlta))
  (format t "~s nodes expanded.~%"
    (get-numero-nodos)))

(test)
