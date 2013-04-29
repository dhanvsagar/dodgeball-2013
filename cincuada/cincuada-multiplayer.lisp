
;;; ================================================================
;;; muj super agent
;;; ================================================================
;;; Student's agent definition:
;; This is to be defined when designing a new student agent 
(defconstant cincuada "AC")
(defconstant go-for-ball 1)
(defconstant run-out 2)
(defconstant step-back 3)
(defconstant top 4)
(defconstant down 5)
(defconstant right 6)
(defconstant left 7)
(defconstant first-move 8)


(defstructure (cincuada    ; replace "my-agent" by your unique name, as e.g. FIT username
                (:include db-agent 
                  (body (make-cincuada-body))
                  (program 'cincuada-program)
                  (name cincuada)
                 ) 
               )
 )

(defstructure (cincuada-body 
                (:include db-agent-body
 		(name cincuada)))
                (mode first-move)		
		(direction nil)
		(avoiding-rotations '0)
		(avoiding-direction nil)
)

(defun cincuada-program (percept)
  (let* ((me (car percept))
        (grid (cadr percept)) 
	(my-position (object-loc me))
	(nearest (nearest-agent (find-evil-agents grid) my-position))
	
   )	;(format t "mode: ~S~%" (cincuada-body-mode me))
		(cond
		((object-contents me ) 
			(progn
				(setf (cincuada-body-mode me) first-move)			
				`(throw-ball ,@(calculate-target nearest my-position grid) )
			))
		( (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me)) ) 'grab-ball)
		( t (next-move my-position (ball-position grid) grid me nearest))	
		)	
	
   	
 ;(nth (random 4) '(go-left go-right go-up go-down))
   ) 
)

(defun next-move (position ball grid me nearest)
	(cond
		((is-next-to-me ball position) (choose-action ball position))
		((equal first-move (cincuada-body-mode me)) (first-move position ball grid me nearest))
		( t (next-step ball position me grid))
	
	)
)

(defun first-move (position ball grid me nearest)
	(if (null ball)(choose-action (next-target position (cincuada-body-direction me)) position)
	(let*((nearest-to-ball (nearest-agent (find-evil-agents grid) ball)))
	(if (<= (manhattan-distance position ball)(manhattan-distance ball nearest-to-ball))
	(progn (setf (cincuada-body-mode me) go-for-ball)
	(setf (cincuada-body-direction me) (calculate-next-direction top position ball))
	(next-step ball position me grid)	
	)
	(progn (setf (cincuada-body-mode me) first-move)
	(setf (cincuada-body-direction me) (calculate-next-direction-out position ball))
	(next-step ball position me grid))
	)))	
)

(defun choose-action (target me)
	(cond ((>(car target)(car me)) 'go-right)
	      ((<(car target)(car me)) 'go-left)
	      ((>(cadr target)(cadr me)) 'go-up)
	      ((<(cadr target)(cadr me)) 'go-down)
	)
)

(defun next-step (target my-position me grid)
	(cond			
		((equal first-move (cincuada-body-mode me)) ;uprk 
			(if (is-position-free (next-target my-position (cincuada-body-direction me)) grid) 
			(choose-action (next-target my-position (cincuada-body-direction me)) my-position)
			(progn
				(setf (cincuada-body-direction me) (calculate-next-direction-out-second my-position target))
				(choose-action (next-target my-position (cincuada-body-direction me)) my-position)
			)			
			)
		)
		((equal go-for-ball (cincuada-body-mode me))
			(cond	;docel sjem do spravneho sloupce
				((and(equal (car target)(car my-position))(or(equal left (cincuada-body-direction me))(equal right (cincuada-body-direction me)))) 
					(progn
						(setf (cincuada-body-direction me) (calculate-next-direction (cincuada-body-direction me) my-position target))	
						(choose-action (next-target my-position (cincuada-body-direction me))my-position)				
					)				
				)
				;dosel jsem do spravneho radku
				((and(equal (cadr target)(cadr my-position))(or(equal top (cincuada-body-direction me))(equal down (cincuada-body-direction me)))) 
					(progn
						(setf (cincuada-body-direction me) (calculate-next-direction (cincuada-body-direction me) my-position target))	
						(choose-action (next-target my-position (cincuada-body-direction me))my-position)				
					)	
				)
				;pokracuju v puvodnim smeru
				((is-position-free (next-target my-position (cincuada-body-direction me)) grid) 
					(choose-action (next-target my-position (cincuada-body-direction me))my-position)
				)
				(nth (random 4) '(go-left go-right go-up go-down))				
			)
		)
	)

)

;urci smer uteku
(defun calculate-next-direction-out ( my-position ball)
	(if(>=(abs (- (car my-position) (car ball)))(abs(-(cadr my-position) (cadr ball))))
	(if(>(cadr my-position)(cadr ball))top  down)
	(if(>(car my-position)(car ball)) left right)
)
)


;urci smer druhorady uteku
(defun calculate-next-direction-out-second ( my-position ball)
	(if(<(abs (- (car my-position) (car ball)))(abs(-(cadr my-position) (cadr ball))))
	(if(>(cadr my-position)(cadr ball))top  down)
	(if(>(car my-position)(car ball)) left right)
)
)

(defun calculate-next-direction (prev-direction my-position ball)
	(cond
		((or (equal top prev-direction)(equal down prev-direction)) (if (<(car my-position) (car ball))left right))
		(t (if(>(cadr my-position)(cadr ball))down top))	
	)	
)

;dalsi policko v danem smeru
(defun next-target (my-position direction)
	(cond
		((equal top direction) (list (car my-position) (+(cadr my-position)'1)))
		((equal down direction) (list (car my-position) (-(cadr my-position)'1)))
		((equal right direction) (list (- (car my-position) '1) (cadr my-position)))
		((equal left direction) (list (+ (car my-position) '1) (cadr my-position)))
	)
)


;je to jeden krok daleko?
(defun is-next-to-me (target me)
	(cond	
		((and (equal (cadr me)(cadr target)) (equal (abs (- (car me)(car target))) '1)) t)
		((and (equal (car me) (car target))(equal (abs (- (cadr me)(cadr target))) '1) )t)
		(t nil)
	)
)

;kam hodim mic
(defun calculate-target (agent me grid)
	(cond
		((is-next-to-me agent me) agent)
		(t (furthest-agent (find-evil-agents grid) me))
	)
)

;je pozice kam chci jit volna
(defun is-position-free (target-position grid)
	(cond   ((equal (aref grid (car target-position) (cadr target-position)) nil) t )
		((equal target-position (ball-position grid)) t)
		((>(length (member target-position (find-evil-agents grid))) '0) nil)				
		(t nil)
	)
)

;vrati nejblizsiho nepritele
(defun nearest-agent (agents me)
	(nearest-agent-helper (cdr agents) me (car agents))
)

;hledani nejblizsiho nepritele
(defun nearest-agent-helper (agents me nearest)
	(cond ((null agents) nearest)
	      ((< (manhattan-distance me (car agents)) (manhattan-distance me nearest)) (nearest-agent-helper (cdr agents) me (car agents)))
	      (t (nearest-agent-helper (cdr agents) me nearest))	
	)
)

(defun furthest-agent (agents me)
	(furthest-agent-helper (cdr agents) me (car agents))
)

(defun furthest-agent-helper (agents me nearest)
	(cond ((null agents) nearest)
	      ((> (manhattan-distance me (car agents)) (manhattan-distance me nearest)) (furthest-agent-helper (cdr agents) me (car agents)))
	      (t (furthest-agent-helper (cdr agents) me nearest))	
	)
)

;spocte manhattanskou vzdalenost
(defun manhattan-distance (first second)
 (+ (abs (- (car first) (car second))) (abs (- (cadr first) (cadr second))))
)

;najde vsechnz nepratelske agenty
(defun find-evil-agents (grid)
  (let ((agent)
	(result))	
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
       (when (setf agent (identify-in-list #'evil-agent-p (aref grid numberx numbery)))
          (setf result (cons (list numberx numbery) result )))))
   result)
)

;najde moji pozici
(defun my-position (grid)
  (let (me)
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
        (when (setf me (identify-in-list #'me-p (aref grid numberx numbery)))
          (return-from my-position (list numberx numbery)))))) nil 
)

;najde mic
(defun ball-position (grid)
 (let (pos)
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
        (when (setf pos (identify-in-list #'a-ball-p ( aref grid numberx numbery)))
          (return-from ball-position (list numberx numbery)))))) nil 
	)

;identifikuje meho agenta
(defmethod me-p ((obj percept-object))
	(if (or(equal (percept-object-name obj) cincuada)(equal (percept-object-name obj) "AC."))
	obj nil)
)

;identifikuje mic
(defmethod a-ball-p ((obj percept-object))
	  (if (equal (percept-object-name obj) "B")
      obj nil)
)

;identifikuje nepratelske agenty
(defmethod evil-agent-p ((obj percept-object))
	(if (and(not(equal (percept-object-name obj) "AC"))(not(equal (percept-object-name obj) "B"))(not (equal (percept-object-name obj) "#")))
        obj nil)	
)

