
;;; ================================================================
;;; muj super agent
;;; ================================================================
;;; Student's agent definition:
;; This is to be defined when designing a new student agent 
(defconstant cincuada "AC")
(defconstant cincuada-go-for-ball 1)
(defconstant cincuada-run-out 2)
(defconstant cincuada-step-back 3)
(defconstant cincuada-top 4)
(defconstant cincuada-down 5)
(defconstant cincuada-right 6)
(defconstant cincuada-left 7)
(defconstant cincuada-first-move 8)


(defstructure (cincuada    ; replace "my-agent" by your unique name, as e.g. FIT username
                (:include db-agent 
                  (body (make-cincuada-body))
                  (program 'cincuada-program)
                  (name "cincuada")
                 ) 
               )
 )

(defstructure (cincuada-body 
                (:include db-agent-body
 		(name cincuada) (sname cincuada)))
                (mode cincuada-first-move)		
		(direction nil)
		(avoiding-rotations '0)
		(avoiding-direction nil)
)

(defun cincuada-program (percept)
  (let* ((me (car percept))
        (grid (cadr percept)) 
	(cincuada-my-position (object-loc me))
	(nearest (cincuada-nearest-agent (cincuada-find-evil-agents grid) cincuada-my-position))
	
   )	;(format t "mode: ~S~%" (cincuada-body-mode me))
		(cond
		((object-contents me ) 
			(progn
				(setf (cincuada-body-mode me) cincuada-first-move)			
				`(throw-ball ,@(cincuada-calculate-target nearest cincuada-my-position grid) )
			))
		( (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me)) ) 'grab-ball)
		( t (cincuada-next-move cincuada-my-position (cincuadcincuada-a-ball-position grid) grid me nearest))	
		)	
	
   	
 ;(nth (random 4) '(go-left go-right go-up go-down))
   ) 
)

(defun cincuada-next-move (position ball grid me nearest)
	(cond
		((cincuada-is-next-to-me ball position) (cincuada-choose-action ball position))
		((equal cincuada-first-move (cincuada-body-mode me)) (cincuada-first-move position ball grid me nearest))
		( t (cincuada-next-step ball position me grid))
	
	)
)

(defun cincuada-first-move (position ball grid me nearest)
	(if (null ball)(cincuada-choose-action (cincuada-next-target position (cincuada-body-direction me)) position)
	(let*((nearest-to-ball (cincuada-nearest-agent (cincuada-find-evil-agents grid) ball)))
	(if (<= (cincuada-manhattan-distance position ball)(cincuada-manhattan-distance ball nearest-to-ball))
	(progn (setf (cincuada-body-mode me) cincuada-go-for-ball)
	(setf (cincuada-body-direction me) (cincuada-calculate-next-direction cincuada-top position ball))
	(cincuada-next-step ball position me grid)	
	)
	(progn (setf (cincuada-body-mode me) cincuada-first-move)
	(setf (cincuada-body-direction me) (cincuada-cincuada-calculate-next-direction-out position ball))
	(cincuada-next-step ball position me grid))
	)))	
)

(defun cincuada-choose-action (target me)
	(cond ((>(car target)(car me)) 'go-right)
	      ((<(car target)(car me)) 'go-left)
	      ((>(cadr target)(cadr me)) 'go-up)
	      ((<(cadr target)(cadr me)) 'go-down)
	)
)

(defun cincuada-next-step (target cincuada-my-position me grid)
	(cond			
		((equal cincuada-first-move (cincuada-body-mode me)) ;uprk 
			(if (cincuada-is-position-free (cincuada-next-target cincuada-my-position (cincuada-body-direction me)) grid) 
			(cincuada-choose-action (cincuada-next-target cincuada-my-position (cincuada-body-direction me)) cincuada-my-position)
			(progn
				(setf (cincuada-body-direction me) (cincuada-cincuada-calculate-next-direction-out-second cincuada-my-position target))
				(cincuada-choose-action (cincuada-next-target cincuada-my-position (cincuada-body-direction me)) cincuada-my-position)
			)			
			)
		)
		((equal cincuada-go-for-ball (cincuada-body-mode me))
			(cond	;docel sjem do spravneho sloupce
				((and(equal (car target)(car cincuada-my-position))(or(equal cincuada-left (cincuada-body-direction me))(equal cincuada-right (cincuada-body-direction me)))) 
					(progn
						(setf (cincuada-body-direction me) (cincuada-calculate-next-direction (cincuada-body-direction me) cincuada-my-position target))	
						(cincuada-choose-action (cincuada-next-target cincuada-my-position (cincuada-body-direction me))cincuada-my-position)				
					)				
				)
				;dosel jsem do spravneho radku
				((and(equal (cadr target)(cadr cincuada-my-position))(or(equal cincuada-top (cincuada-body-direction me))(equal cincuada-down (cincuada-body-direction me)))) 
					(progn
						(setf (cincuada-body-direction me) (cincuada-calculate-next-direction (cincuada-body-direction me) cincuada-my-position target))	
						(cincuada-choose-action (cincuada-next-target cincuada-my-position (cincuada-body-direction me))cincuada-my-position)				
					)	
				)
				;pokracuju v puvodnim smeru
				((cincuada-is-position-free (cincuada-next-target cincuada-my-position (cincuada-body-direction me)) grid) 
					(cincuada-choose-action (cincuada-next-target cincuada-my-position (cincuada-body-direction me))cincuada-my-position)
				)
				(T (nth (random 4) '(go-left go-right go-up go-down)))
			)
		)
	)

)

;urci smer uteku
(defun cincuada-cincuada-calculate-next-direction-out ( cincuada-my-position ball)
	(if(>=(abs (- (car cincuada-my-position) (car ball)))(abs(-(cadr cincuada-my-position) (cadr ball))))
	(if(>(cadr cincuada-my-position)(cadr ball))cincuada-top  cincuada-down)
	(if(>(car cincuada-my-position)(car ball)) cincuada-left cincuada-right)
)
)


;urci smer druhorady uteku
(defun cincuada-cincuada-calculate-next-direction-out-second ( cincuada-my-position ball)
	(if(<(abs (- (car cincuada-my-position) (car ball)))(abs(-(cadr cincuada-my-position) (cadr ball))))
	(if(>(cadr cincuada-my-position)(cadr ball))cincuada-top  cincuada-down)
	(if(>(car cincuada-my-position)(car ball)) cincuada-left cincuada-right)
)
)

(defun cincuada-calculate-next-direction (prev-direction cincuada-my-position ball)
	(cond
		((or (equal cincuada-top prev-direction)(equal cincuada-down prev-direction)) (if (<(car cincuada-my-position) (car ball))cincuada-left cincuada-right))
		(t (if(>(cadr cincuada-my-position)(cadr ball))cincuada-down cincuada-top))	
	)	
)

;dalsi policko v danem smeru
(defun cincuada-next-target (cincuada-my-position direction)
	(cond
		((equal cincuada-top direction) (list (car cincuada-my-position) (+(cadr cincuada-my-position)'1)))
		((equal cincuada-down direction) (list (car cincuada-my-position) (-(cadr cincuada-my-position)'1)))
		((equal cincuada-right direction) (list (- (car cincuada-my-position) '1) (cadr cincuada-my-position)))
		((equal cincuada-left direction) (list (+ (car cincuada-my-position) '1) (cadr cincuada-my-position)))
	)
)


;je to jeden krok daleko?
(defun cincuada-is-next-to-me (target me)
	(cond	
		((and (equal (cadr me)(cadr target)) (equal (abs (- (car me)(car target))) '1)) t)
		((and (equal (car me) (car target))(equal (abs (- (cadr me)(cadr target))) '1) )t)
		(t nil)
	)
)

;kam hodim mic
(defun cincuada-calculate-target (agent me grid)
	(cond
		((cincuada-is-next-to-me agent me) agent)
		(t (cincuada-furthest-agent (cincuada-find-evil-agents grid) me))
	)
)

;je pozice kam chci jit volna
(defun cincuada-is-position-free (target-position grid)
	(cond   ((equal (aref grid (car target-position) (cadr target-position)) nil) t )
		((equal target-position (cincuadcincuada-a-ball-position grid)) t)
		((>(length (member target-position (cincuada-find-evil-agents grid))) '0) nil)				
		(t nil)
	)
)

;vrati nejblizsiho nepritele
(defun cincuada-nearest-agent (agents me)
	(cincuada-nearest-agent-helper (cdr agents) me (car agents))
)

;hledani nejblizsiho nepritele
(defun cincuada-nearest-agent-helper (agents me nearest)
	(cond ((null agents) nearest)
	      ((< (cincuada-manhattan-distance me (car agents)) (cincuada-manhattan-distance me nearest)) (cincuada-nearest-agent-helper (cdr agents) me (car agents)))
	      (t (cincuada-nearest-agent-helper (cdr agents) me nearest))	
	)
)

(defun cincuada-furthest-agent (agents me)
	(cincuada-furthest-agent-helper (cdr agents) me (car agents))
)

(defun cincuada-furthest-agent-helper (agents me nearest)
	(cond ((null agents) nearest)
	      ((> (cincuada-manhattan-distance me (car agents)) (cincuada-manhattan-distance me nearest)) (cincuada-furthest-agent-helper (cdr agents) me (car agents)))
	      (t (cincuada-furthest-agent-helper (cdr agents) me nearest))	
	)
)

;spocte manhattanskou vzdalenost
(defun cincuada-manhattan-distance (first second)
 (+ (abs (- (car first) (car second))) (abs (- (cadr first) (cadr second))))
)

;najde vsechnz nepratelske agenty
(defun cincuada-find-evil-agents (grid)
  (let ((agent)
	(result))	
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
       (when (setf agent (identify-in-list #'cincuada-evil-agent-p (aref grid numberx numbery)))
          (setf result (cons (list numberx numbery) result )))))
   result)
)

;najde moji pozici
(defun cincuada-my-position (grid)
  (let (me)
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
        (when (setf me (identify-in-list #'cincuada-me-p (aref grid numberx numbery)))
          (return-from cincuada-my-position (list numberx numbery)))))) nil 
)

;najde mic
(defun cincuadcincuada-a-ball-position (grid)
 (let (pos)
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
        (when (setf pos (identify-in-list #'cincuada-a-ball-p ( aref grid numberx numbery)))
          (return-from cincuadcincuada-a-ball-position (list numberx numbery)))))) nil 
	)

;identifikuje meho agenta
(defmethod cincuada-me-p ((obj percept-object))
	(if (or(equal (percept-object-name obj) cincuada)(equal (percept-object-name obj) "AC."))
	obj nil)
)

;identifikuje mic
(defmethod cincuada-a-ball-p ((obj percept-object))
	  (if (equal (percept-object-name obj) "B")
      obj nil)
)

;identifikuje nepratelske agenty
(defmethod cincuada-evil-agent-p ((obj percept-object))
	(if (and(not(equal (percept-object-name obj) "AC"))(not(equal (percept-object-name obj) "B"))(not (equal (percept-object-name obj) "#")))
        obj nil)	
)

