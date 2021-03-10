; *** Astar.lisp                              ***
; *** Código lisp para encontrar la ruta más  ***
; *** corta entre ciudades de México.         ***

(let ((ciudades (make-hash-table :size 20)))              ; Tabla de ciudades
  (defun set-distancia (x y) (setf (gethash x ciudades) y))
  (defun get-distancia (x) (gethash x ciudades)))

(let ((caminos (make-hash-table :size 20)))               ; Tabla de caminos recorridas
  (defun set-ancestro (x y) (setf (gethash x caminos) y))
  (defun get-ancestro (x) (gethash x caminos)))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; **** CIUDADES Y DISTANCIAS ****
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; **** DISCUTIR HEURISTICO ****
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun a-star (nodo-inicial nodo-final)                   ; algoritmo A*

  (set-meta nodo-final)                                   ; ¡OBJETO PARA GUARDAR EL NODO FINAL!
                                                          ; (PARA PODER ACCEDER A EL EN OTRAS FUNCIONES)

  (let ((abierto (list start-node))                       ; Inicializamos lista abierto y cerrado, el nodo x y sucesor
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
          (setf nodo-actual
                (Buscar-mejor (first abierto)             ; ¡FUNCIÓN RECURSIVA PARA BUSCAR UN NODO CON MENOR F!
                              (rest abierto))))

      (setf abierto (remove nodo-actual abierto))         ; lo sacamos de abierto
      (push nodo-actual cerrado)                          ; y lo enviamos a cerrado

      (if (eql nodo-actual nodo-final)                    ; Si llegamos al nodo final terminamos
          (return (camino-final nodo-actual)))            ; ¡FUNCIÓN PARA REGRESAR UNA LISTA CON LOS NODOS DE LA RUTA!

      (setf sucesores                                     ; De otro modo, obtenemos los posibles sucesores de n
            (mapcar #'first (get-distancia nodo-actual))) ; Por medio de un mapcar de los primeros elementos de cada lista

      (dolist (sucesor sucesores)                         ; Para cada nodo sucesor revisa si aún no esta
        (if (not (or (member sucesor abierto)             ; ni en abierto
                     (member  cerrado)))                  ; ni en cerrado

          (progn                                          ; Si es TRUE :

           (incrementa-cuentaNodos)                       ; aumenta un nodo a la cuenta
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
                              (arc-dist ans-anterior sucesor) )
                           (get-g nodo-actual)
                           (arc-dist nodo-actual sucesor) )
                        (get-f sucesor) ) ) )

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

                ))                                        ; Aquí termina el primer progn
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
  (defun set-meta (la-meta) (setf meta la-meta))
  (defun get-meta () meta) )

; ;;;;;;;;;;;;;;;;;;;;
; **** CALCULAR-F ****
; ;;;;;;;;;;;;;;;;;;;;
(defun calcular-f (n)
  "Genera el valor de F para el nodo N. Es la suma de G y H"
  (+ (get-g n) (h n)) )
; ;;;;;;;;;;;;;;;;;;;;
; **** CALCULAR-G ****
; ;;;;;;;;;;;;;;;;;;;;
(defun calcular-g (nodo x)
  "Genera la distancia de los nodos desde el raiz hasta el X al añadirle la distancia x-nodo a la del x-raiz"
  (+ (get-g x) (arc-dist x nodo)) )


; ;;;;;;;;;;;;;;;;;;;;;;
; **** CALCULAR-H ****
; ;;;;;;;;;;;;;;;;;;;;;;
(defun h (n)
  "Genera un estimado de la distancia con la distancia euclidiana (toeréma de pitágoras)"
  (+ (* (-root_x n_x) (-root_x n_x) ) ( * (-root_y n_y) (-root_y n_y) ))

(defun arc-dist (n1 n2) 
  "Returns the distance along arc N1 N2. If no such arc
   exists, returns BIG-DISTANCE."
  (or (rest (assoc n1 (get-distances n2))) big-distance) )
; ;;;;;;;;;;;;;;;;;;;;;;
; **** BUSCAR-MEJOR **** Rodrigo
; ;;;;;;;;;;;;;;;;;;;;;;

; ;;;;;;;;;;;;;;;;
; **** COLOCA ****
; ;;;;;;;;;;;;;;;;
;;; Pone al nodo en abierto, manteniendola ordenada por valor f
(defun coloca (nodo lst)
  "Pone al nodo en abierto (lista lst), manteniendola ordenada por valor f"
  (cond ((null lst)(list nodo))
        ((< (get-f nodo)
            (get-f (first lst)) )
         (cons nodo lst) )
        (t (cons (first lst)
                 (coloca nodo (rest lst)) )) ) )

; ;;;;;;;;;;;;;;;;;;;;;;
; **** CAMINO-FINAL **** Rodrigo
; ;;;;;;;;;;;;;;;;;;;;;;

; ;;;;;;;;;;;;;;;;;;;;;
; **** ACTUALIZA-F ****
; ;;;;;;;;;;;;;;;;;;;;;

; ;;;;;;;;;;;;;;;;;;;;;;
; **** NUMERO-NODOS ****
; ;;;;;;;;;;;;;;;;;;;;;;

(let (NUMERO-NODOS)
  (defun iniciar-cuentaNodos () (setf NUMERO-NODOS 0))
  (defun incrementa-cuentaNodos () (incf NUMERO-NODOS))
  (defun get-numeroNodos () NUMERO-NODOS) )


;;; Distancia imposible
(defconstant distanciaImpoible 9999999)