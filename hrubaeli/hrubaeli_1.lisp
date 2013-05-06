;;; ================================================================
;;; Student's agent definition:

;;; Jmeno meho agenta
(defconstant hrubaeli-agent-name "EH")

(defstructure (hrubaeli   
                (:include db-agent 
                  (body (make-hrubaeli-body))
                  (program 'hrubaeli-agent-program)
                  (name "hrubaeli"))) 
  "Your agent for db-world.")

(defstructure (hrubaeli-body 
                (:include db-agent-body (name hrubaeli-agent-name)))
  ;(slot1 default1)  ; any specific extra slots your agent's body would need
  ;(slotn defaultn))
    ;
    )

;;; muj agent x nepohyblivi agenti
(defun hrubaeli-agent-program (percept)
  (let* ((me (car percept))    ; extracts agent body from percept
    (grid (cadr percept))
    (holding-ball (object-contents me))
    (my-loc (object-loc me))
    (ball-loc (find-ball-location grid))
    (enemy-lst (find-enemies grid))
    (enemy-to-hit (get-enemy-to-hit enemy-lst my-loc)))
	(cond
	    ((and (null ball-loc) (not holding-ball)) (nth (random 4) '(go-left go-right go-up go-down))) ;nekdo jiny drzi mic, uhni
		( (not holding-ball) ; nikdo nema mic, jdi pro nej
			(move enemy-lst ball-loc my-loc grid)) 
		( T (my-throw-ball enemy-to-hit my-loc)) ; mas mic, hod ho
	)))
    
;;; pokud je mic u tebe, zvedni ho, jinak jdi pro nej  
(defun move (enemy-lst ball-loc my-loc grid)
    (if (equal ball-loc my-loc) 'grab-ball (move-to-ball enemy-lst ball-loc my-loc grid))
)

;;; jdi pro mic
(defun move-to-ball (enemy-lst ball-loc my-loc grid)
  (let* ((up (count-value-of-field enemy-lst ball-loc (list (car my-loc) (+ 1 (cadr my-loc))) grid))
	     (right (count-value-of-field enemy-lst ball-loc (list (+ 1 (car my-loc)) (cadr my-loc)) grid))
	     (down (count-value-of-field enemy-lst ball-loc (list (car my-loc) (- (cadr my-loc) 1)) grid))
	     (left (count-value-of-field enemy-lst ball-loc (list (- (car my-loc) 1) (cadr my-loc)) grid))
	     (best-way (min up right down left))	
        )
        ;(format t "FIELD: ~A ~A ~A ~A" up right down left)
	    (cond 
	      ((= best-way up) 'go-up)
	      ((= best-way right) 'go-right)
	      ((= best-way down) 'go-down)
	      (T 'go-left)
	    ) ;konec cond
  ) ;konec let
) ; konec defun

;(defun move-to-ball (grid ball-loc my-loc)
	;(format t "Move to position ~A, from position ~A" ball-loc my-loc)
;	(cond ((> (car ball-loc) (car my-loc)) 'go-right)
;		  ((< (car ball-loc) (car my-loc)) 'go-left)
;          (T (cond  ((> (cadr ball-loc) (cadr my-loc)) 'go-up)
;          	        (T 'go-down)
;             ) 
;          )   
;    )
;)

;;; hod mic pred neratelskeho agenta
(defun throw-ball-next (enemy my-loc)
  (let* ((up (list (car enemy) (+ 1 (cadr enemy))))
	     (right (list (+ 1 (car enemy)) (cadr enemy)))
	     (down (list (car enemy) (- (cadr enemy) 1)))
	     (left (list (- (car enemy) 1) (cadr enemy)))
	     (length-up (points-dist up my-loc))
	     (length-right (points-dist right my-loc))
	     (length-down (points-dist down my-loc))
	     (length-left (points-dist left my-loc))
	     (best-way (min length-up length-right length-down length-left))	
        )
	    (cond 
	      ((= best-way length-up) up)
	      ((= best-way length-right) right)
	      ((= best-way length-down) down)
	      (T left)
	    ) ;konec cond
  ) ;konec let
)

;;; hod micem
(defun my-throw-ball (enemy my-loc)
;(format t "Vzdalenost je: ~A" (points-dist enemy my-loc))
	(if (= 1 (points-dist enemy my-loc)) `(throw-ball ,@enemy) ;vzdalenost 1, hod to na nepritele
	    `(throw-ball ,@(throw-ball-next enemy my-loc)) ;vzdalenost vetsi, hod k nepriteli
	)
)

;;; pocet nepratel kolem policka
(defun count-enemy-neighbour (enemy-num enemy-lst curr-loc)
 (cond 
   ((null enemy-lst) enemy-num)
   ((= 1 (points-dist (car enemy-lst) curr-loc)) (count-enemy-neighbour (+ 1 enemy-num) (cdr enemy-lst) curr-loc))
   (T (count-enemy-neighbour enemy-num (cdr enemy-lst) curr-loc))
 ))

;;; hodnota policka, na ktere se chystam stoupnout
(defun count-value-of-field (enemy-lst ball-loc curr-loc grid)
  (cond 
  	 ((equal curr-loc ball-loc) 0) ; je tam mic, jdi pro nej
     ((null (aref grid (car curr-loc) (cadr curr-loc))) 
     	(+ (count-enemy-neighbour 0 enemy-lst curr-loc) (points-dist ball-loc curr-loc)))
     (t 100) ; je tam zed nebo nepritel, sem ne!
  )	
)

;;; pozice nejblizsiho nepritele
(defun get-enemy-to-hit (enemy-loc my-loc)
 (if (null enemy-loc) nil
    (get-enemy-to-hit-iter (car enemy-loc) (cdr enemy-loc) my-loc))
)

(defun get-enemy-to-hit-iter (nearest enemy-loc my-loc)
  (cond ((null enemy-loc) nearest)
        ( (< (points-dist my-loc (car enemy-loc)) (points-dist my-loc nearest)) 
           (get-enemy-to-hit-iter (car enemy-loc) (cdr enemy-loc) my-loc))
        (T (get-enemy-to-hit-iter nearest (cdr enemy-loc) my-loc))
        ))


;;; najde vsechny nepratele, ulozi do enemy, vraci enemy  
(defun find-enemies (grid)
  (let (enemy)
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
        (when (identify-in-list #'enemy-p (aref grid numberx numbery))
          (pushnew (list numberx numbery) enemy)) nil ))
   enemy))
          
          

;;; vsechno krome me
(defmethod enemy-p ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) hrubaeli-agent-name)) 
           (not (equal (percept-object-name obj) "B")))
      obj nil))
    
