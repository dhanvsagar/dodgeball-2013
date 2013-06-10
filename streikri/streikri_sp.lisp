;;;;----------------------------------------------------------------------------------------------
;;;; Definice agenta pro mod 1
;;;;----------------------------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------------------------
;;; ## Definice struktury agenta ##
;;;-----------------------------------------------------------------------------------------------

(defconstant streikri-name "KS")

(defstructure (streikri-body (:include db-agent-body (name streikri-name))))

(defstructure (streikri                
               (:include db-agent 
                         (program 'streikri-program) 
                         (body (make-streikri-body))
                         (name streikri-name)))
  "An agent that will be attempting to hit the WT agents.")


;;;-----------------------------------------------------------------------------------------------
;;; ## Program agenta ##
;;;-----------------------------------------------------------------------------------------------

(defun streikri-program (percept)
;;-----------------------------------------------------
;; definice promennych
;;-----------------------------------------------------
  (let* ((me (car percept))
         (grid (cadr percept))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
         (holding-ball (object-contents me))
         (agents-loc (find-agents-location grid))
         (my-loc (object-loc me))
         (ball-loc (find-ball-location grid))         
         (is-ball-on-agents-loc (has-agent-ball-p agents-loc ball-loc))
        )
         
;;-----------------------------------------------------
;; reseni ruznych stavu
;;-----------------------------------------------------

    (cond 
    ; Pokud jiz nejsou zadni agenti, tak koncim
     ((not agents-loc) (return-from streikri-program 'stop))
   
   
     ; Balon je na mem policku -> vezmu jej
     (ball-on-my-loc (return-from streikri-program 'grab-ball))
          
          
     ; Balon mam v ruce -> hodim jej bud _na_ agenta ve vzdalenosti 1
     ;					-> nebo _k_ nejblizsimu agentovi
     (holding-ball (return-from streikri-program (react-to-holding-ball my-loc agents-loc)))
     
     
     ; Balon ma agent -> if je ve vzdalenosti 1 -> bump
	 ;					 else -> stuj
	 (is-ball-on-agents-loc (return-from streikri-program (react-to-ball-on-agents-loc my-loc ball-loc)))
	 
	 
	 ; Balon je volny -> jdu pro nej
     (t (return-from streikri-program (react-to-default my-loc ball-loc grid)))
    )
  )
)


;;;-----------------------------------------------------------------------------------------------
;; ## Moje funkce ##
;;;-----------------------------------------------------------------------------------------------

;;--------------------------------
;; Najde a vrati souradnice vsech agentu na gridu
(defun find-agents-location (grid)
  (let (agents-loc)
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
       	(when (identify-in-list #'my-agent-p (aref grid numberx numbery))	
         (push (@ numberx numbery) agents-loc) 								
        )
      )		
    )
    agents-loc
  )		     
)

;;--------------------------------
;; Metoda urcujici agenta
;; tzn. neni to zed ("#"), ja (streikri-name) ani balon ("B")
(defmethod my-agent-p ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) streikri-name)) 
           (not (equal (percept-object-name obj) "B"))
      )
      obj nil
  )
)
      
      
;;--------------------------------
;; Metoda urcujici zdali ma agent na svem poli balon
(defmethod has-agent-ball-p (agents-loc ball-loc)
	(mapc #'(lambda (agent-loc) (if (equal ball-loc agent-loc) 
									(return-from has-agent-ball-p t)
								)
			) agents-loc
	)
	nil
)


;;--------------------------------
;; Z listu list-of-locs vybere takove pole, ktere je nejblize poli loc
(defun find-closest-loc (loc list-of-locs)
  (let ((min-distance 10000)	; dostatecne vysoka hodnota
		(closest-loc nil))
		
	(mapc #'(lambda (current-loc) (when (< (xy-distance loc current-loc) min-distance) 
									(setf min-distance (xy-distance loc current-loc))
									(setf closest-loc current-loc)
						)
			) list-of-locs
	)     
    closest-loc
  )
)


;;--------------------------------
;; Vrati mista v "krizi" okolo souradnic danych parametry
(defun find-space-next-loc (loc)
  (let ((loc-cross nil))
  
    (push (@ (-(xy-x loc) 1) (xy-y loc)) loc-cross)    ; vlevo
    (push (@ (xy-x loc) (-   (xy-y loc) 1)) loc-cross) ; nahore
    (push (@ (+(xy-x loc) 1) (xy-y loc)) loc-cross)    ; vpravo
    (push (@ (xy-x loc) (+   (xy-y loc) 1)) loc-cross) ; dole
            
	loc-cross
  )
)


;;-----------------------------------------------------
;; Funkce obsluhujici reakce na ruzne stavy hlavni logiky programu
;;-----------------------------------------------------

;;--------------------------------
;; Funkce obsluhujici reakci na stav, kdy mam balon v ruce
;; Pokud mam balon v ruce, tak jej hodim bud _na_ agenta ve vzdalenosti 1
;; nebo _k_ nejblizsimu agentovi
(defun react-to-holding-ball (my-loc agents-loc)
  (let* ((closest-agent-loc (find-closest-loc my-loc agents-loc))
		 (closest-agent-dist (xy-distance my-loc closest-agent-loc)))
					
		; Pokud je nejblizsi agent u me, tak _na_ nej hodim balon
		(when (= closest-agent-dist 1) (return-from react-to-holding-ball `(throw-ball ,@closest-agent-loc))
		)
		
		; Pokud je nejblizsi agent nekde jinde, tak hodim balon _k_ nemu
		(when (> closest-agent-dist 1) 
			(let ((next-agent-loc (find-closest-loc my-loc (find-space-next-loc closest-agent-loc))))
			  (return-from react-to-holding-ball `(throw-ball ,@next-agent-loc)) 
			)
			
		)
  )
)


;;--------------------------------
;; Funkce obsluhujici reakci na stav, kdy ma agent balon na svem poli
;; if je ve vzdalenosti 1 -> bump
;; else -> stuj
(defun react-to-ball-on-agents-loc (my-loc ball-loc)
	(cond ((= (xy-distance my-loc ball-loc) 1.0) (return-from react-to-ball-on-agents-loc (select-move my-loc (list ball-loc)))))
	'stay
)


;;--------------------------------
;; Funkce obsluhujici reakci na stav, kdy je balon volny
;; Pokud je balon volny, tak se pro nej vydam
(defun react-to-default (my-loc ball-loc grid)
  (select-move my-loc (find-path my-loc ball-loc grid))
)


;;-----------------------------------------------------
;; Funkce zajistujici vyhledani cesty agenta
;; Vyuzijeme AIMA
;; When we define a new subtype of problem, we need to define a SUCCESSORS  method. 
;;-----------------------------------------------------

;;--------------------------------
;; Struktura subtypu problemu
(defstructure (find-path-problem (:include problem))
			  (grid)
)


;;--------------------------------
;; Najde cestu
(defun find-path (start finish grid)
	(solution-actions   (no-duplicates-breadth-first-search (make-find-path-problem :initial-state start 
																					:goal finish 
																					:grid grid
															)
						)
	)
)


;;--------------------------------
;; Urci, zda je dane pole volne
(defun free-loc (loc grid)
    (mapc #'(lambda (content) (if (or (my-agent-p content) 
									  (equal (percept-object-name content) "#")
									  (equal (percept-object-name content) "WT")
								  )
								  (return-from free-loc nil)
							  )
			) (aref grid (xy-x loc) (xy-y loc))
	)
  t
)


;;--------------------------------
;; Predefinujeme metodu successors
;; Ta urcuje kam muzeme z daneho mista jit (tzn. pouze vlevo, nahoru, dolu, vpravo
;; a zaroven tam musi byt volno)
;; "Return a list of (action . state) pairs, reachable from this state."
(defmethod successors ((problem find-path-problem) v1 )
  (let  ((loc-cross (find-space-next-loc v1))
		 (grid (find-path-problem-grid problem))
		 (result nil))
    
    (mapc #'(lambda (loc) (if (free-loc loc grid)
							(push (cons loc loc) result))
			) loc-cross
    )
    result 
  )
)


;;--------------------------------
;; Vybere pohyb pro dalsi krok
(defun select-move (current-loc path)
	(let ((next-loc (car path)))
		; pozor, osa "x" je ve sloupci, osa "y" v radku
		(cond	((= (1+ (xy-y current-loc)) (xy-y next-loc)) (return-from select-move 'go-up))
				((= (1+ (xy-x current-loc)) (xy-x next-loc)) (return-from select-move 'go-right))
				((= (1- (xy-y current-loc)) (xy-y next-loc)) (return-from select-move 'go-down))
				((= (1- (xy-x current-loc)) (xy-x next-loc)) (return-from select-move 'go-left))
		)
	)
)
