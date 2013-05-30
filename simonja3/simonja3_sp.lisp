;;; Dodgeball agent for singleplayer version

; Agent name
(defconstant simonj-name "JS")

; Agent definition
(defstructure (simonja3
               (:include db-agent 
                 (body (make-simonj-body))
                 (program 'simonj-program)
                 (name simonj-name)))
    "Your agent for db-world.")

; Agent body with name declaration
(defstructure (simonj-body
               (:include db-agent-body (name simonj-name)  (sname "JS")))
			   
			   )

; Main logic
(defun simonj-program (percept)  
  (let* ((me (car percept))
         (grid (cadr percept))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
         (holding-ball (object-contents me))
         (agent-loc (simonj-find-agent-location grid))
		 (ball-loc (find-ball-location grid))
		 (my-loc (simonj-find-my-location grid)))
    (cond 
	 
     ( ball-on-my-loc 'grab-ball )
	 ; pokud je agent 1 policko, hodin na nej, jinak hodit 1 policko pred nej
     ( holding-ball (simonj-throw-ball agent-loc my-loc grid))
	 
	 ((not ball-loc) 'stay)
	 ; presuneme se na mic
     ( t (progn 
	  ;(format t "~%agent-loc: ~S" agent-loc)
	  ;(format t "~%ball-loc: ~S" ball-loc)
	  ;(format t "~%my-loc: ~S" my-loc)
	  ;(format t "~%locality free 1 1:  ~S" (is-loc-free (list 1 1) grid))
	  ;(format t "~%locality free 0 0:  ~S" (is-loc-free (list 0 0) grid))
	  ;(format t "~%locality free my loc:  ~S" (is-loc-free my-loc grid))
	  ;(format t "~%locality free ball loc:  ~S" (is-loc-free ball-loc grid))
	  ;(format t "~%locality free agent loc:  ~S" (is-loc-free agent-loc grid))
	  ;('stop)
	  (simonj-naive-move-to-ball my-loc ball-loc grid)
	  ))
    )
   )
)

(defun simonj-throw-ball (agent-loc my-loc grid)
	; kdyz jsme na jedno policko, tak agenta vybijem
	(progn
		(format t "~%distance: ~S" (simonj-distance agent-loc my-loc))
		(if (equal 1 (simonj-distance agent-loc my-loc))
			`(throw-ball ,@agent-loc)
		; jinak to hodime o policko pred nej
			`(throw-ball ,@(simonj-agent-empty-loc my-loc agent-loc grid))
		)
	)
)

(defun simonj-agent-empty-loc (my-loc agent-loc grid)
	
	(cond
		;up
		( (and (> (cadr my-loc) (cadr agent-loc)) (is-loc-free (list (car agent-loc) (+ (cadr agent-loc) 1)) grid))
			(list (car agent-loc) (+ (cadr agent-loc) 1))
		)
		;down
		( (and (< (cadr my-loc) (cadr agent-loc)) (is-loc-free (list (car agent-loc) (- (cadr agent-loc) 1)) grid))
			(list (car agent-loc) (- (cadr agent-loc) 1))
		)
		;right
		( (and (> (car my-loc) (car agent-loc)) (is-loc-free (list (+ (car agent-loc) 1) (cadr agent-loc)) grid))
			(list (+ (car agent-loc) 1) (cadr agent-loc))
		)
		;left
		( (and (< (car my-loc) (car agent-loc)) (is-loc-free (list (- (car agent-loc) 1) (cadr agent-loc)) grid))
			(list (- (car agent-loc) 1) (cadr agent-loc))
		)
	)
) 



(defun simonj-find-agent-location (grid)
  (find-X-location #'simonj-agent-p grid)
)

(defun simonj-naive-move-to-ball (my-loc ball-loc grid) 
	(cond
		; kdyz je vzdalenost 1 k mici, tak jdu vzdy, bud ho seberu nebo bumpnu
		( (equal 1 (simonj-distance ball-loc my-loc)) 
			(cond 
				((> (car my-loc) (car ball-loc)) 'go-left)
				((< (car my-loc) (car ball-loc)) 'go-right)
				((> (cadr my-loc) (cadr ball-loc)) 'go-down)
				((< (cadr my-loc) (cadr ball-loc)) 'go-up)
			)
		)
		; obcas jdeme nahodne abych se nezaseknul:)
		( (eq (random 5) 0)
			(nth (random 4) '(go-left go-right go-up go-down))
		)
		
		( (if (eq (random 2) 0)
		; preference pohybu horizontalne	
			(cond
				( (and (> (car my-loc) (car ball-loc)) (is-loc-free (list (- (car my-loc) 1) (cadr my-loc)) grid)
						; jdem doleva dokud to jde
						'go-left
				  )
				)
				( (and (< (car my-loc) (car ball-loc)) (is-loc-free (list (+ (car my-loc) 1) (cadr my-loc)) grid)			
						; jdem doprava dokud to jde				
						'go-right
					)		
				)
				( (and (> (cadr my-loc) (cadr ball-loc)) (is-loc-free (list (car my-loc) (- (cadr my-loc) 1)) grid)
						; jdem dolu dokud to jde
						'go-down
					)
				)
				( (and (< (cadr my-loc) (cadr ball-loc)) (is-loc-free (list (car my-loc) (+ (cadr my-loc) 1)) grid)
						; jdem nahoru dokud to jde
						'go-up
					)
				)		
				; nejde se pohnout optimalnim smerem, tak pujdeme chvili nahodne	
				((nth (random 4) '(go-left go-right go-up go-down)))
			)
			; preference pohybu vertikalne
			(cond
				( (and (> (cadr my-loc) (cadr ball-loc)) (is-loc-free (list (car my-loc) (- (cadr my-loc) 1)) grid)
						; jdem dolu dokud to jde
						'go-down
					)
				)
				( (and (< (cadr my-loc) (cadr ball-loc)) (is-loc-free (list (car my-loc) (+ (cadr my-loc) 1)) grid)
						; jdem nahoru dokud to jde
						'go-up
					)
				)
				( (and (> (car my-loc) (car ball-loc)) (is-loc-free (list (- (car my-loc) 1) (cadr my-loc)) grid)
						; jdem doleva dokud to jde
						'go-left
				  )
				)
				( (and (< (car my-loc) (car ball-loc)) (is-loc-free (list (+ (car my-loc) 1) (cadr my-loc)) grid)			
						; jdem doprava dokud to jde				
						'go-right
					)		
				)				
				; nejde se pohnout optimalnim smerem, tak pujdeme chvili nahodne	
				((nth (random 4) '(go-left go-right go-up go-down)))
			)
			
			)
		)
	)
)


(defun is-loc-free (loc grid)
	(cond
		; prazny je ok, vracime true
		((null (aref grid (car loc) (cadr loc))) t)
		; samotny mic je ok, taky vracime true
		((my-ball-p (car (aref grid (car loc) (cadr loc)))) t)
		; jinak je plny (okraj nebo agent)
		(t nil)
	)
)


;manhattanska vzdalenost dvou uzlu
(defun simonj-distance (loc1 loc2) 
	( +
		(abs 
			(- 
				(car loc1)
				(car loc2)
			)
		)
		(abs 
			(- 
				(cadr loc1)
				(cadr loc2)
			)
		)
	)
)


  
(defmethod simonj-agent-p ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) simonj-name)) 
           (not (equal (percept-object-name obj) "B")))
      obj nil))

	  
(defun simonj-find-my-location (grid)
	(find-X-location #'simonj-me-p grid))

(defmethod simonj-me-p ((obj percept-object))
  (if (equal (percept-object-name obj) simonj-name)
      obj nil))

