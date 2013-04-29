
(load (concatenate 'string prefix "/agents/sembejir-1.lisp"))

; hledání pozice za hráčem -> pokud ho netrefíme, tak nedostane míč
(defun make-suitable-for-overshoot-p (src-coord must-contain-this)
  (lambda (target grid)
    (and (identify-in-list (lambda (item) (equal item must-contain-this) ) (trajectory src-coord target) )
         (sembejir-coord-in-game-area target grid)
    ) 
  )
)

; je trajektorie volná?
(defun clear-view (src target grid)
  (not (member-if (lambda (elem) (not (null (apply #'aref grid elem)))) (butlast (cdr (trajectory src target)))))
)

; najde enemáka na hranici dostřelu, na kterého je čistý výhled
(defun sembejir-find-suitable-target (grid self-loc enemy-locs)
  (let ((result-loc (closest-target self-loc enemy-locs)))
    (dolist (enemy-loc enemy-locs)
      (when (and 
              (< (euclidean-distance self-loc result-loc) (euclidean-distance self-loc enemy-loc))
              (> *CAN-HIT-DIST* (euclidean-distance self-loc enemy-loc))
              (clear-view self-loc enemy-loc grid)
            )
        (setf result-loc enemy-loc) 
      )       
    )
    result-loc
  )
)

; název self-explanatory :) prostě vymyslí co s míčem :)
(defun sembejir-do-something-smart-with-the-ball (self grid near-ball has-ball self-loc)
  (if (list-surrounding-locations self-loc (sembejir-make-coord-enemy-p) grid )   ; we are near an enemy
    (values `(throw-ball ,@(car (list-surrounding-locations self-loc (sembejir-make-coord-enemy-p) grid ))) #'sembejir-hump-enemy)  ; throw the ball at him
    (let* ((all-enemies (list-all-locations (sembejir-make-enemy-p) grid)) (target (closest-target self-loc all-enemies)) )                                                 ; otherwise list all enemies
      (if (null (car all-enemies))
        (error "No enemies found!")
        (if (< *CAN-HIT-DIST* (euclidean-distance target self-loc))
          (progn  
             (print "enemy-too-far> ");                                                                                       ; enemy too far
             (let ((new-relative-target (combine-coordinates #'truncate (combine-coordinates #'- target self-loc) '(2 2))))
                (format t "relative offset ~S~%" new-relative-target)
                (format t "absolute-position ~S~%" (combine-coordinates #'+ new-relative-target self-loc))
                (values `(throw-ball ,@(combine-coordinates #'+ new-relative-target self-loc)) #'sembejir-fetch-ball)                                    ; throw it at him            
             )
          )
          (progn                            ; existuje enemák blíž než *CAN-HIT-DIST*
            (print "throwball> ");
            (setf target (sembejir-find-suitable-target grid self-loc all-enemies))
            (format t "Targeting player at: ~S ~%" target)            
            (let ((overshoot-target target) (found-loc nil) )
              (block loop-outer
                (loop
                  (setf found-loc (farthest-target self-loc (list-surrounding-locations overshoot-target (make-suitable-for-overshoot-p self-loc target) grid )))
                  (format t "FOUND-LOC: ~S ~%" found-loc)
                  (if found-loc
                    (if (or (equal found-loc overshoot-target) (< *CAN-HIT-DIST* (euclidean-distance found-loc self-loc) ) (< (euclidean-distance found-loc self-loc) (euclidean-distance overshoot-target self-loc) ) ) ; pokud najdeme stejnou lokaci, vzdálenější jak *CAN-HIT-DIST*, nebo bližší --> konec
                      (progn (setf overshoot-target found-loc) (return-from loop-outer) )
                      (setf overshoot-target found-loc)
                    )
                    (return-from loop-outer)                    
                  ) 
                )
              )
              (format t "targeting enemy at ~S~%" target)
              (format t "shooting at ~S~%" overshoot-target)                                
              (values `(throw-ball ,@overshoot-target) #'sembejir-fetch-ball)                                    ; throw it at him                         
            )
                        
          )
        )                    
      )  
    )
  )  
)

(defun sembejir-enemy-closer-p (target-loc my-loc enemy-loc-list)
  (dolist (enemy-loc enemy-loc-list)
    (format t "base: ~S --> ~S vs. ~S result ~S" target-loc my-loc enemy-loc (< (euclidean-distance target-loc enemy-loc) (euclidean-distance target-loc my-loc) ))
    (when (< (euclidean-distance target-loc enemy-loc) (euclidean-distance target-loc my-loc) ) (return-from sembejir-enemy-closer-p T) ) 
  )
  nil 
)

(defun sembejir-do-something-smart-without-the-ball (self grid near-ball has-ball self-loc) 
;  (print "enemy-closer")
;  (print (sembejir-enemy-closer-p (find-ball-location grid) self-loc (list-all-locations (sembejir-make-enemy-p) grid)))
  (cond
    ( (and (sembejir-enemy-has-balls grid) (equal 1 (euclidean-distance self-loc (sembejir-enemy-has-balls grid)))) (format t "~%>> Hump enemy") (sembejir-hump-enemy self grid near-ball has-ball self-loc) ) ; enemy is at the same spot as the ball and I'm next to him
    ( (null (find-ball-location grid) ) (format t "~%>> Grabbing ball") 'grab-ball)                          ; ball not found AND enemy not holding it -> my clone must be holding it already --> stay
    ( (sembejir-enemy-has-balls grid) (format t "~%>> Running away") (sembejir-run-away self grid near-ball has-ball self-loc) ) ; enemy is at the same spot as the ball
    ( (sembejir-enemy-closer-p (find-ball-location grid) self-loc (list-all-locations (sembejir-make-enemy-p) grid)) (format t "~%>> Enemy gets the ball faster") (sembejir-run-away self grid near-ball has-ball self-loc) ) ; enemy will get to the ball faster --> run away
    ( T (format t "~%>> Getting ball") (sembejir-fetch-ball self grid near-ball has-ball self-loc) )
  )
      
)