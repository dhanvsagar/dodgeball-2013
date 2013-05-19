;;; ================================================================
;;; Student's agent definition:

;; This is to be defined when designing a new student agent 
;

(defconstant silhaja6-short-name "SIL")
(defstructure ( silhaja6    ; replace "my-agent" by your unique name, as e.g. FIT username
                (:include db-agent 
                  (body (make-silhaja6-body))
                  (program 'my-agent-program)
                  (name "silhaja6"))) 
  "Your agent for db-world.")

(defstructure (silhaja6-body 
                (:include db-agent-body (name silhaja6-short-name) (sname silhaja6-short-name)  ))
  ;;(slot1 default1)  ; any specific extra slots your agent's body would need
  ;...
  (slot1 234)
  ;
    )

(defun my_find-agent-location (grid)
  ;(print "find-agent-location")
  (find-X-location #'my_agent-p grid))

(defun my_find-my-location (grid)
  ;(print "find-agent-location")
  (find-X-location #'my_my-loc-p grid))

(defun getStep (me) 

	(let ( (X (car (silhaja6-body-slot1 me)))

				)	

	(setf (silhaja6-body-slot1 me)  (cdr (silhaja6-body-slot1 me)) )
	X

) ; //endLet
)


(defun simpleEq (loc1 loc2)
 	(if (equal loc1 loc2) T nil)
)

(defun nextToAgent? (loc agg)
  (if (equal agg (list (+ (car loc) 1) (cadr loc) ))
      (return-from nextToAgent? (list (+ (car loc) 1) (cadr loc) ) ))

	(if (equal agg (list (- (car loc) 1) (cadr loc) ))       
      (return-from nextToAgent? (list (- (car loc) 1) (cadr loc) ) ) )

	(if (equal agg (list (car loc) (+ (cadr loc) 1)  ))         
      (return-from nextToAgent? (list (car loc) (+ (cadr loc) 1) ) ) )

	(if (equal agg (list (car loc) (- (cadr loc) 1)   ))        
      (return-from nextToAgent? (list (car loc) (- (cadr loc) 1) ) ) )


    nil
)

(defun BFS (grid from to term)

	(print "BFS")

  ;(print (array-dimensions grid))   tohle dava (10 10)
	;(print (array-dimension grid 0))   a tohle dava 10

  (let* (  (x_d (car (array-dimensions grid)) )
					 (y_d (cadr (array-dimensions grid)) )
					 (my_grid  (make-array (list x_d y_d) ) )  ; mame nove pole NILU, budem postupne prepisovat na True
					 (qu  (make-empty-queue) )
						)
		
		
				(enqueue-at-end qu (list (list from (cons -2 nil))))     ; //dej do fronty prvni pozici, 3 pozice je serie kroku,  
				;(setf (aref my_grid (car from) (cadr from)) T )
				(setf (aref grid (car from) (cadr from)) T )
				(setf from (queue-front qu)) ;aby prosla podminka ve whilu			

				(loop while (not (funcall term (car from)  to)) do 
						(setf from (queue-front qu))
						(remove-front qu)

	;	(identify-in-list #'my-ball-p (aref grid (+ 1 (caar from)) (cadar from)) )

						(if (or (null (aref grid (+ 1 (caar from)) (cadar from)) ) (equal (list (+ 1 (caar from)) (cadar from)  ) to  )) 		;doprava - 1
								(progn   (enqueue-at-end qu (list (list (list (+ 1 (caar from)) (cadar from) ) (append (cadr from) '( 1 ) ) ) ) )       
												 (setf (aref grid (+ 1 (caar from)) (cadar from) ) T )			 ) )


						(if (or (null (aref grid (- (caar from) 1) (cadar from)) )  (equal (list (- (caar from) 1) (cadar from)  ) to  ))   ;doleva - 0
								(progn  ( enqueue-at-end qu (list (list (list (- (caar from) 1) (cadar from) ) (append (cadr from) '( 0 ) ) ) )  )      
												(setf (aref grid (- (caar from) 1) (cadar from)) T )			 ) )


						(if (or (null (aref grid  (caar from) (+ (cadar from) 1)) )  (equal (list (caar from) (+ (cadar from) 1) ) to  ))   ;nahoru - 2 
								(progn  ( enqueue-at-end qu (list (list (list (caar from) (+ (cadar from) 1) ) (append (cadr from) '( 2 ) ) ) )   )     
									      (setf (aref grid  (caar from) (+ (cadar from) 1)) T )			 ) )

						(if (or (null (aref grid  (caar from) ( - (cadar from) 1 )) ) (equal (list (caar from) (- (cadar from) 1) ) to  ))   ;dolu  - 3
								(progn  ( enqueue-at-end qu (list (list (list (caar from) (- (cadar from) 1) ) (append (cadr from) '( 3 ) ) ) )  ) 
										    (setf (aref grid  (caar from) ( - (cadar from) 1 )) T )				 ) )    

						(if   (empty-queue? qu)

										(return-from BFS '((0 0)(4 4)))						  )  ;stop - nema reseni


						;(print "------------------------")
						;(print (car from))
						;(print to)
 				)  ; //end loop

   ;(print (cdadr from))
         from   ; return 
 		)  ; konec let

)

(defun generatePath (me grid from to)

(let* (  

					(path (BFS grid from to #'simpleEq))
					(X (car   (cdadr path) ) )
)

	(setf (silhaja6-body-slot1 me)  (cdr (cdadr path)) )
	X

) ; //end let
 
)

(defun dist (loc1 loc2)

  (+ (* (- (car loc1) (car loc2)) (- (car loc1) (car loc2))) 
	   (* (- (cadr loc1) (cadr loc2)) (- (cadr loc1) (cadr loc2))) )
)


(defun closestAgent (my-loc grid)
(let (
 (maxdi  10000)     ;vetsi vzdalenost tam snad neni 
	(coord nil)     ;vetsi vzdalenost tam snad neni  
)

	(dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (identify-in-list #'my_agent-p (aref grid numberx numbery))
        (if (< (dist my-loc (list numberx numbery) ) maxdi)  
									(progn
                (setf maxdi (dist my-loc (list numberx numbery)  )) 
								(setf coord  (list numberx numbery)  ) 
									)
                            ) )))

coord
)

)

(defun generatePathToAgent (me grid from)  ; netreba tady to, ptze nevim kde je nejblizsi agent

(let* (
				
				(coord (closestAgent from grid))
				(path (BFS grid from coord #'nextToAgent? ))

)
		(setf (silhaja6-body-slot1 me)  (cdadr path))
		(car path)
)
	
)

(defun my-agent-program (percept)
 
 (let* ((me (car percept))
         (grid (cadr percept))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
         (holding-ball (object-contents me))
         (agent-loc (my_find-agent-location grid))
				 (ball-loc (find-ball-location grid) )
				 (my-loc (my_find-my-location grid) )
         (next-to (closestAgent my-loc grid)  )
												)


  ;(print "me") (print my-loc)
	;(print "wt") (print agent-loc) 
	;(print "ball") (print ball-loc)

  ;(print   ( car (aref grid (+ 1 (car my-loc)) (cadr my-loc) )  ))
    (cond 
     ( (not agent-loc) 'stop )
     ( ball-on-my-loc 'grab-ball )
     ( holding-ball        (if ( = (dist next-to my-loc) 1)
														 `(throw-ball ,@next-to) 
                            (progn  ( setf next-to-loc (generatePathToAgent me grid my-loc ) )
																				(print next-to-loc)
																				(print "next to")
                                     `(throw-ball ,@next-to-loc)    ) )  )
		 ( (not holding-ball)  (if (consp (silhaja6-body-slot1 me) )  
															 (nth ( getStep me) '(go-left go-right go-up go-down))
															 (nth ( generatePath me grid my-loc ball-loc) '(go-left go-right go-up go-down stop))
																										)  
																																										)


															
     ( t 'stop)))  

)

(defun my_agent-p ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) silhaja6-short-name)) 
           (not (equal (percept-object-name obj) "B")))
      obj nil))

(defun my_my-loc-p ((obj percept-object))
  (if        (equal (percept-object-name obj) silhaja6-short-name) 
      obj nil))

;(defmethod my_empty-p ((obj percept-object))
;  (if (and (not (equal (percept-object-name obj) "#")) 
;           (not (equal (percept-object-name obj) silhaja6-short-name)) 
;           (not (equal (percept-object-name obj) "B"))
;					 (not (equal (percept-object-name obj)  wait-and-throw-db-agent-name  )) )
;            
;      			    T nil ) )

;...
 ;...  here your program comes to calculate and return a proper action       
 ;...
 ;for example:


 ;(nth (random 1) '(go-left go-right go-up go-down))
 ;   ) )

;;; Any number of auxiliary functions can follow
;;; 
;;; To test run the game in single mode you perform
;;; (test-agent-mode-1 'your-agent-name)
;;; for example:
;;; (test-agent-mode-1 'ask-user-db-agent)
;;;
;;; To test run the game in competitive mode, we will perform
;;; (test-agent-mode-2 '(first-student-agent second-student-agent third-student-agent ...))
;;; for example:
;;; (test-agent-mode-2 '(ask-user-db-agent ask-user-db-agent))
;;; ==================================================================
