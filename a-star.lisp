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

  ;Ciudades de Baja California
  (set-distancia 'Tijuana '((Playas-de-Rosarito . 21)(Ensenada . 100)(Tecate . 50)))
  (set-distancia 'Playas-de-Rosarito '((Tijuana . 21)(Ensenada . 84)))
  (set-distancia 'Ensenada '((Tijuana . 100)(Playas-de-Rosarito . 84)(Tecate . 108)(Mexicali . 238)(Ciudad-Progreso . 221)(Ciudad-Guadalupe-Victoria . 284)(Punta-Prieta . 480)(Bahia-de-los-Angeles . 520)))
  (set-distancia 'Tecate '((Tijuana . 50)(Ensenada . 108)(Ciudad-Progreso . 120)(Mexicali . 130)))
  (set-distancia 'Ciudad-Progreso '((Ensenada . 221)(Tecate . 120)(Mexicali . 15)(Ciudad-Guadalupe-Victoria . 75)(Bahia-de-los-Angeles . 537)(Punta-Prieta . 485)))
  (set-distancia 'Mexicali '((Ensenada . 238)(Tecate . 130)(Ciudad-Progreso . 15)(Ciudad-Guadalupe-Victoria . 62)))
  (set-distancia 'Ciudad-Guadalupe-Victoria '((Ensenada . 284)(Mexicali . 62)(Bahia-de-los-Angeles . 501)(Punta-Prieta . 459)))
  (set-distancia 'Punta-Prieta '((Ensenada . 480)(Ciudad-Progreso . 485)(Ciudad-Guadalupe-Victoria . 459)(Bahia-de-los-Angeles . 79)(Santo-Domingo . 85)))
  (set-distancia 'Bahia-de-los-Angeles '((Ensenada . 520)(Ciudad-Progreso . 537)(Ciudad-Guadalupe-Victoria . 501)(Punta-Prieta . 79)(El-Arco . 165)))
  (set-distancia 'Santo-Domingo '((Punta-Prieta . 85)(El-Arco . 108)(Benito-Juarez . 65)))
  (set-distancia 'El-Arco '((Bahia-de-los-Angeles . 165)(Santo-Domingo . 108)(Benito-Juarez . 49)))
  ;Ciudades de Baja California Sur
  (set-distancia 'Bahia-Tortugas '((Benito-Juarez . 226)(San-Ignacio . 245)))
  (set-distancia 'Benito-Juarez '((Bahia-Tortugas . 226)(San-Ignacio . 126)(El-Arco . 49)(Santo-Domingo . 65)))
  (set-distancia 'San-Ignacio '((Bahia-Tortugas . 245)(Benito-Juarez . 126)(Mulege . 138)(Comondu . 246)(Ciudad-Insurgentes . 310)))
  (set-distancia 'Mulege '((Comondu . 157)(Ciudad-Insurgentes . 250)(Loreto . 135)))
  (set-distancia 'Comondu '((San-Ignacio . 246)(Mulege . 157)(Ciudad-Insurgentes . 109)(Loreto . 75)))
  (set-distancia 'Loreto '((Mulege . 135)(Comondu . 75)(Ciudad-Insurgentes . 122)))
  (set-distancia 'Ciudad-Insurgentes '((San-Ignacio . 310)(Mulege . 250)(Comondu . 109)(Loreto . 122)(La-Paz . 236)))
  (set-distancia 'La-Paz '((Ciudad-Insurgentes . 236)(Todos-Santos . 82)(Santiago . 134)(La-Ribera . 136)))
  (set-distancia 'Todos-Santos '((La-Paz . 82)(Cabo-San-Lucas . 78)(Santiago . 155)(La-Ribera . 156)))
  (set-distancia 'Cabo-San-Lucas '((Santiago . 89)(Todos-Santos . 78)))
  (set-distancia 'Santiago '((Cabo-San-Lucas . 89)(La-Paz . 134)(Todos-Santos . 155)(La-Ribera . 23)))
  (set-distancia 'La-Ribera '((La-Paz . 136)(Santiago . 23)(Todos-Santos . 156)))

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

  ;Ciudades de Baja California
  (set-coordenadas 'Tijuana '(32.51 117.05))
  (set-coordenadas 'Playas-de-Rosarito '(32.36 117.06))
  (set-coordenadas 'Ensenada '(31.86 116.59))
  (set-coordenadas 'Tecate '(32.56 116.62))
  (set-coordenadas 'Ciudad-Progreso '(32.58 115.58))
  (set-coordenadas 'Mexicali '(32.60 115.46))
  (set-coordenadas 'Ciudad-Guadalupe-Victoria '(32.29 115.10))
  (set-coordenadas 'Punta-Prieta '(28.92 114.15))
  (set-coordenadas 'Bahia-de-los-Angeles '(28.95 113.56))
  (set-coordenadas 'Santo-Domingo '(25.49 111.92))
  (set-coordenadas 'El-Arco '(28.02 113.39))
  ;Ciudades de Baja California Sur
  (set-coordenadas 'Bahia-Tortugas '(27.69 114.89))
  (set-coordenadas 'Benito-Juarez '(27.88 113.78))
  (set-coordenadas 'San-Ignacio '(27.28 112.87))
  (set-coordenadas 'Mulege '(26.88 111.98))
  (set-coordenadas 'Comondu '(26.06 111.84))
  (set-coordenadas 'Loreto '(26.01 111.35))
  (set-coordenadas 'Ciudad-Insurgentes '(25.26 111.77))
  (set-coordenadas 'La-Paz '(24.14 110.31))
  (set-coordenadas 'Todos-Santos '(23.44 110.22))
  (set-coordenadas 'Cabo-San-Lucas '(22.89 109.91))
  (set-coordenadas 'Santiago '(23.47 109.71))
  (set-coordenadas 'La-Ribera '(23.59 109.58))

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
    (a-star 'Tijuana 'Cabo-San-Lucas))
  (format t "Path-length: ~s.~%"
    (get-f 'Cabo-San-Lucas))
  (format t "~s nodes expanded.~%"
    (get-numero-nodos)))

(test)
