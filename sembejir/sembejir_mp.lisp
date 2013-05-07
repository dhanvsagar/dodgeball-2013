;; SEMBEJIR agent
;;
;;

; agent name
(defconstant sembejir-agent-name "SB")

;;;; MAIN BODY
(defun sembejir-prgm (percept)            
  (let ((agent-body (first percept)))    ; extracts agent body from percept
    (let* (                                                             ; copy-pasted from WT agent
         (me (car percept))
         (grid (cadr percept))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
         (holding-ball (object-contents me))
         (student-loc (find-student-location grid))
         (my-loc (find-X-location (sembejir-percept-object-name-p-factory sembejir-agent-name) grid))
         (me-percept (first (apply #'aref grid (object-loc me))))
         )              
         
         (format t "~%~%==== DEBUG DUMP AND STUFF ====~%~%")
         
         (format t "name: ~S ~%" sembejir-agent-name)
         (when (null my-loc)
            (format t "I'm dead! ~%")
            ;(error "dead")
            (return-from sembejir-prgm 'stay)          
         )                
         
         (format t "ball-on-my-loc: ~S ~%" ball-on-my-loc)
         (format t "holding-ball: ~S ~%" holding-ball)
         (format t "student-loc: ~S ~%" student-loc)
         (format t "my-loc: ~S ~%" my-loc)
         (format t "(sembejir-enemy-armed grid): ~S ~%" (sembejir-enemy-armed grid))
         (format t "(sembejir-enemy-has-balls grid): ~S ~%" (sembejir-enemy-has-balls grid))
         
         (format t "~%CURRENT-WPTS: ~S~%" (sembejir-agent-body-waypoints agent-body))                           
         (format t "CURRENT-TACTICS: ~S~%~%" (sembejir-agent-body-tactics me))
                  
         ;(multiple-value-bind (action new-tactics)                                                ; original idea with a state-machine                              
         ;   (funcall (sembejir-agent-body-tactics me) me grid ball-on-my-loc holding-ball my-loc) ; execute tactics!
         ;   (setf (sembejir-agent-body-tactics me) new-tactics)                                   ; set next tactics
         ;   action                                                                                ; return action
         ;)
         
         (sembejir-do-something-smart me grid ball-on-my-loc holding-ball my-loc)   
    )
   )
)

(defstructure (sembejir-agent-body                 
                (:include db-agent-body
                (name sembejir-agent-name)
                (sname sembejir-agent-name)
                )                
              )
    (tactics #'sembejir-fetch-ball)                             ; tactics
    (waypoints nil)                                    ; pre-planned waypoints
    (waypoint-target nil)                              ; the waypoints' target
)
  
(defstructure (sembejir
    (:include db-agent
      (body (make-sembejir-agent-body))
      (program 'sembejir-prgm)
      (name sembejir-agent-name)
    )
  )
  "The sembejir agent" 
)


;;;; UTILITIES --------------------- 

; factory for comparing a single string to others
(defun sembejir-string-match-p-factory (stringToMatch) 
  (lambda (otherString) (equal stringToMatch otherString)) 
)

; factory for comparing percept-object names to a single string
(defun sembejir-percept-object-name-p-factory (object-name)
  (let ((name-matches (sembejir-string-match-p-factory object-name))) 
    (lambda (object) 
      (if (null object) 
        nil
        (if (funcall name-matches (percept-object-name object) ) object nil)
      ) 
    )  
  ) 
)

;; MISC

(defun sembejir-sembejir-euclidean-distance-helper (list1 list2 result)
  (if (or (null list1) (null list2))
    result
    (sembejir-sembejir-euclidean-distance-helper (cdr list1) (cdr list2) (+ result (* (- (car list1) (car list2)) (- (car list1) (car list2))) ))
  )   
)
(defun sembejir-euclidean-distance (list1 list2) 
  (sqrt (sembejir-sembejir-euclidean-distance-helper list1 list2 0))
)

;; PREDICATE FACTORIES
(defun make-sembejir-p ()
  (sembejir-percept-object-name-p-factory sembejir-agent-name)  
)

; does object at given coordinates represent MYSELF?
(defun make-sembejir-coord-p ()
  (let ((my-p (make-my-p)))
    (lambda (coord grid)
      (identify-in-list my-p (apply #'aref grid coord) )            
    )
  )  
)

; does object at given coordinates represent an ENEMY?
(defun sembejir-make-enemy-p ()
  (let ((me-p (make-sembejir-p)))
    (lambda (object)
      (if (null object) nil
        (if (and (not (equal (percept-object-name object) "#")) 
             (not (funcall me-p object)) 
             (not (equal (percept-object-name object) "B")))
          object nil)
      ) 
    )
  )
)

(defun sembejir-make-coord-enemy-p ()
  (let ((enemy-p (sembejir-make-enemy-p)))
    (lambda (coord grid)
      (identify-in-list enemy-p (apply #'aref grid coord) )            
    )
  )
)

(defun sembejir-in-game-area (object)
;  (format t "in-game-area dbg: ~S ~S ~S" object (percept-object-name object)  (equal (percept-object-name object) "#"))
  (not (equal (percept-object-name object) "#")) 
)

(defun sembejir-coord-in-game-area (coord grid)
  (if (null (apply #'aref grid coord))
    T
    (identify-in-list #'sembejir-in-game-area (apply #'aref grid coord) )
  )
)

;; OBSERVERS
(defun sembejir-enemy-has-balls (grid)
  (if (find-ball-location grid)
    (if (member-if (sembejir-make-enemy-p) (apply #'aref grid (find-ball-location grid)))
      (find-ball-location grid)
      nil
    )
    (sembejir-enemy-armed grid)    
  )
)

(defun sembejir-enemy-armed (grid) 
  (car (member-if 
    (lambda (coord)
      (member-if
        (lambda (object)                              ; check if an object is enemy and armed 
          (if (funcall (sembejir-make-enemy-p) object)
            (percept-object-agent-has-ball object)
            nil        
          )
        )
        (apply #'aref grid coord)                     ; get objects on coord
      )          
    )
    (sembejir-list-all-locations (sembejir-make-enemy-p) grid)          ; coords of all enemies
  ))
) 

; coordinates modification utils
(defun sembejir-modify-nth (list n &optional (func #'1+)) 
  (if (= n 0) (cons (funcall func (car list)) (cdr list))
    (cons (car list) (sembejir-modify-nth (cdr list) (1- n) func))
  )
)
(defun sembejir-inc-x (coord) 
  (sembejir-modify-nth coord 0)
)
(defun sembejir-dec-x (coord) 
  (sembejir-modify-nth coord 0 #'1-)
)
(defun sembejir-inc-y (coord) 
  (sembejir-modify-nth coord 1)
)
(defun sembejir-dec-y (coord) 
  (sembejir-modify-nth coord 1 #'1-)
)

; coordinates combinator
(defun sembejir-combine-coordinates (operation list1 list2)
  (if (null (car list1))
    nil
    (cons (funcall operation (car list1) (car list2)) (sembejir-combine-coordinates operation (cdr list1) (cdr list2)) )
  )  
)

; navigation utilities
(defun sembejir-can-go-there (target grid) 
  (let ((field (apply #'aref grid target)))
    (or 
      (null field)
      (and (percept-object-ball-p (car field)) (null (cdr field)))  
    )
  ) 
)

(defun sembejir-can-throw-there (target grid) 
  (let ((field (apply #'aref grid target)))
    (or 
      (null field)
      (and (member-if #'percept-object-agent-p field) 
        (not (member-if (make-sembejir-p) field ))
      )
    )
  ) 
)   

(defun make-sembejir-can-throw-there-p (grid)
  (lambda (target) (sembejir-can-throw-there target grid) ) 
)

(defun sembejir-null-there (target grid) 
  (let ((field (apply #'aref grid target)))
      (null field)
  ) 
)

; navigates from given source coordinates to target coordinates using the shortest path, X-Y directioning       
(defun sembejir-navigate (src target grid &optional (check #'sembejir-can-go-there) ) 
  (let ((result nil))
    (when
      (and      
        (= (car src) (car target)) 
        (= (cadr src) (cadr target))
        ) (push 'stay result))                        ; . 
    (when
      (and      
        (< (car src) (car target)) 
        (funcall check (sembejir-inc-x src ) grid)
     ) (push 'go-right result) )                        ; ->
    (when
      (and 
        (> (car src) (car target)) 
        (funcall check (sembejir-dec-x src ) grid) ; <-
     ) (push 'go-left result) )  
    (when
      (and 
        (< (cadr src) (cadr target))
        (funcall check (sembejir-inc-y src ) grid)
     ) (push 'go-up result) )                          ; ^
    (when
      (and 
        (> (cadr src) (cadr target))
        (funcall check (sembejir-dec-y src ) grid)
     ) (push 'go-down result) )                         ; v 
            
     (if (null result) 
        'stay
        (nth (random (length result)) result )
     )         
     
  )

)

; navigates from given source coordinates to target coordinates using the shortest path, randomized directioning       
;(defun sembejir-navigate (src target grid &optional (check #'sembejira-can-go-there) ) 
;  (cond
;    ((and      
;        (= (car src) (car target)) 
;        (= (cadr src) (cadr target))
;     ) 'stay )                        ; .  
;    ((and      
;        (< (car src) (car target)) 
;        (funcall check (sembejir-inc-x src ) grid)
;     ) 'go-right )                        ; ->
;     
;     ((and 
;        (> (car src) (car target)) 
;        (funcall check (sembejir-dec-x src ) grid)
;     ) 'go-left )                         ; <-
;     
;     ((and 
;        (< (cadr src) (cadr target))
;        (funcall check (sembejir-inc-y src ) grid)
;     ) 'go-up )                           ; ^ 
;     
;     ((and 
;        (> (cadr src) (cadr target))
;        (funcall check (sembejir-dec-y src ) grid)
;     ) 'go-down )                         ; v
;
;    (T nil)                             ; we cannot take the shortest route 
;  )
;)

(defun sembejir-navigate-away (src target grid &optional (check #'sembejir-can-go-there) )
  (if (< *CAN-HIT-DIST* (sembejir-euclidean-distance src target))
    'stay                                                                                 ; wont run farther than *CAN-HIT-DIST*
    (let
      ((new-target (sembejir-combine-coordinates #'+ (sembejir-combine-coordinates #'- src target) src )))  ; opačný vektor
      (if (sembejir-navigate src new-target grid check)
        (sembejir-navigate src new-target grid check)
        'stay
      )
    )
  )
)

; closest target pick
(defun sembejir-sembejir-closest-target-helper (src target-list best-solution best-solution-distance comparison-fun) 
  (if (null (car target-list))                                                ; no more targets
     best-solution                                                        ; return best solution
    (if (funcall comparison-fun (sembejir-euclidean-distance src (car target-list)) best-solution-distance) ; current is beter
      (sembejir-sembejir-closest-target-helper src (cdr target-list) (car target-list) (sembejir-euclidean-distance src (car target-list)) comparison-fun)  ; recursive call with new best
      (sembejir-sembejir-closest-target-helper src (cdr target-list) best-solution best-solution-distance comparison-fun)   ; recursive call with current best
    )
  )
)
   
(defun sembejir-closest-target (src targets)
  (if (null targets)
    nil
    (sembejir-sembejir-closest-target-helper src targets nil 65535 #'<)  
  )
)

(defun sembejir-farthest-target (src targets)
  (if (null targets)
    nil
    (sembejir-sembejir-closest-target-helper src targets nil 0 #'>)  
  )
)
; new waypoint generator
(defmacro sembejir-cond-add-waypoint (wpt existing-wpts new-wpts grid)
  `(if (or (not(sembejir-can-go-there ,wpt ,grid)) (member-if (lambda (obj) (equal ,wpt obj) ) ,existing-wpts))
    nil
    (push ,wpt ,new-wpts)
  ) 
)
(defun sembejir-make-new-waypoint (src target grid agent-body)
  (let ((waypoints (sembejir-agent-body-waypoints agent-body)) (new-wpts nil))
    (sembejir-cond-add-waypoint (sembejir-inc-x target) waypoints new-wpts grid)    
    (sembejir-cond-add-waypoint (sembejir-inc-y target) waypoints new-wpts grid)
    (sembejir-cond-add-waypoint (sembejir-dec-x target) waypoints new-wpts grid)
    (sembejir-cond-add-waypoint (sembejir-dec-y target) waypoints new-wpts grid)
    
    (sembejir-closest-target src new-wpts)        
  )
)
; smarter navigation
(defun sembejir-navigate-smarter (src target grid agent-body)
  (let ((current-target target))
    (format t "Navigating to ~S... ~%" target)
    (if (equal src target)
      'stay
      (progn
        (when (not (equal target (sembejir-agent-body-waypoint-target agent-body) ) )  ; navigating elsewhere -> remove waypoints 
          (setf (sembejir-agent-body-waypoint-target agent-body) target)
          (setf (sembejir-agent-body-waypoints agent-body) nil)
          (format t "  wpts reset... ~%") 
        )
        (when (equal (car (sembejir-agent-body-waypoints agent-body)) src)                 ; if we are on a waypoint -> remove it
          (pop (sembejir-agent-body-waypoints agent-body))
          (format t "  Standing at current waypoint -> remove... ~%")
        )
        (when (sembejir-agent-body-waypoints agent-body)                  ; if we have a waypoint to use, do so
          (setf current-target (car (sembejir-agent-body-waypoints agent-body)))
          (format t "  Going at waypoint ~S~%" current-target)
        )
        (if (sembejir-navigate src current-target grid)               ; try to sembejir-navigate to waypoint
          (sembejir-navigate src current-target grid)                 ; if succesful, return the direction
          (if (not (sembejir-make-new-waypoint src current-target grid agent-body))  ; else try to make up a new waypoint
            'stop                                    ; failed --> end simulation, we are unable get to the target
            (progn
              (format t "  Navigation failed, looking for suitable waypoints near ~S~%" current-target)
              (push (sembejir-make-new-waypoint src current-target grid agent-body) (sembejir-agent-body-waypoints agent-body)) ; push new waypoint
              (format t "  New waypoints ~S~%" (sembejir-agent-body-waypoints agent-body))                                     
              (sembejir-navigate-smarter src target grid agent-body)     ; try to sembejir-navigate again
            )
          )
        )
      )
    )     
  )  
)

(defun sembejir-pre-plan-waypoints (src target grid)
  (progn
    (format t "  Waypoint preplanning started...~%")
    (let ((tmparr (make-array (array-dimensions grid) :initial-element nil))
         (queue nil) (result (list src)))
      (push src queue)                          ; init BFS queue

      (setf (apply #'aref tmparr src) '(nil nil))   ; the origin was discovered by nobody

      (block bfs
        (loop
          (when (null queue) (return-from bfs))         ; queue empty
;          (format t "Queue: ~S~%" queue)
          
          (let ((current-coord (pop queue)))            ; pick new coordinate from queue
            (dolist (new-location (sembejir-list-surrounding-locations current-coord    ; list all unvisited surrounding locations
                                       (lambda (target grid) (and (sembejir-can-go-there target grid) (sembejir-null-there target tmparr) ) ) 
                                       grid))
              (setf queue (append queue (list new-location) ))           ; add location
              (setf (apply #'aref tmparr new-location) current-coord )   ; add location parent
            ) 
          )          
        )
      )
      
      (when (apply #'aref tmparr target)    ; backtrack path (if found)
        (let ((current-coord target))
          (loop
            (when (equal current-coord src ) (return))
            (push current-coord result)
            (setf current-coord (apply #'aref tmparr current-coord) )            
          )        
        )      
      )
      
      (when (null (apply #'aref tmparr target))
        (format t "  No path found! ~S~%" tmparr)        
      )
      
      result
    )
  ) 
)
(defun sembejir-navigate-with-preplanning (src target grid agent-body &optional (flush-preplanned nil))
  (let ((current-target target))
    (if (equal src target)
      'stay
      (progn
        (when (not (null flush-preplanned))
          (setf (sembejir-agent-body-waypoints agent-body) nil)
          (format t "  wpts reset~%")           
        )
        (when (not (equal target (sembejir-agent-body-waypoint-target agent-body) ) )  ; navigating elsewhere -> remove waypoints 
          (setf (sembejir-agent-body-waypoint-target agent-body) target)
          (setf (sembejir-agent-body-waypoints agent-body) nil)
          (format t "  wpts reset, navigating to a new location... ~%") 
        )
        (when (equal (car (sembejir-agent-body-waypoints agent-body)) src)                 ; if we are on a waypoint -> remove it
          (pop (sembejir-agent-body-waypoints agent-body))
          (format t "  Standing at current waypoint -> remove... ~%")
        )
        (when (not (sembejir-agent-body-waypoints agent-body))                 ; if we are out of waypoints
          (setf (sembejir-agent-body-waypoint-target agent-body) target)
          (format t "  Preplanned waypoints: ~S~%" (sembejir-pre-plan-waypoints src target grid))
          (setf (sembejir-agent-body-waypoints agent-body) (sembejir-pre-plan-waypoints src target grid))                    
        )
        (when (sembejir-agent-body-waypoints agent-body)                  ; if we have a waypoint to use, do so
          (setf current-target (car (sembejir-agent-body-waypoints agent-body)))
          (format t "  Going at waypoint ~S~%" current-target)
        )
        (if (sembejir-navigate src current-target grid)               ; try to sembejir-navigate to waypoint
          (sembejir-navigate src current-target grid)                 ; if succesful, return the direction
          (error "Waypoint error") 
        )                          ; 
      )
    )
  )
)

(defun sembejir-navigate-without-preplanning (src target grid agent-body)
  (sembejir-navigate-with-preplanning src target grid agent-body T)
)

; ball flight calculation
(defun sembejir-trajectory (point-from point-to)
  (let ((traj (go-through-dist-list point-to point-from)) (result nil)) ; calculate in reverse order
    (push point-to result)
    (dolist (square-info traj)
      (push (car square-info) result)       ; reverse
    )    
    result
  ) 
)

(defun sembejir-trajectory-coords-only-helper (lst)
  (if (null lst)
    nil
    (cons (caar lst) (sembejir-trajectory-coords-only-helper (cdr lst)) )
  )
)

(defun sembejir-trajectory-coords-only (point-from point-to)
  (sembejir-trajectory-coords-only-helper (sembejir-trajectory point-from point-to))
)

; lists surrounding location coordinates based on predicate - object works with coordinates
(defun sembejir-list-surrounding-locations (coord predicate grid)
  (let ((result nil))    
    (when (not (null (funcall predicate (sembejir-inc-x coord) grid ))) (push (sembejir-inc-x coord) result))
    (when (not (null (funcall predicate (sembejir-dec-x coord) grid ))) (push (sembejir-dec-x coord) result))
    (when (not (null (funcall predicate (sembejir-inc-y coord) grid ))) (push (sembejir-inc-y coord) result))
    (when (not (null (funcall predicate (sembejir-dec-y coord) grid ))) (push (sembejir-dec-y coord) result))
    result   
  )
)

; lists all location coordinates based on predicate  - predicate identifies object only
(defun sembejir-list-all-locations (predicate grid)
  (let ((result nil))
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
        (when (identify-in-list predicate (aref grid numberx numbery))
          (push (list numberx numbery) result)))) 
    result
  )
)
                                               
;;;; TACTICS-MODE1 ---------------------

(defun sembejir-do-something-smart (self grid near-ball has-ball self-loc)
                                                    
  (if has-ball (sembejir-do-something-smart-with-the-ball self grid near-ball has-ball self-loc)
    (sembejir-do-something-smart-without-the-ball self grid near-ball has-ball self-loc)
  )
)

(defun sembejir-do-something-smart-without-the-ball (self grid near-ball has-ball self-loc)
  ;(if (not has-ball) 
  ;(values 'stop #'sefmbejir-do-something-smart-with-the-ball)
  
  (cond
    ( (and (sembejir-enemy-has-balls grid) (equal 1 (sembejir-euclidean-distance self-loc (sembejir-enemy-has-balls grid)))) (sembejir-hump-enemy self grid near-ball has-ball self-loc) ) ; enemy is at the same spot as the ball and I'm next to him
    ( (sembejir-enemy-has-balls grid) (sembejir-run-away self grid near-ball has-ball self-loc) ) ; enemy is at the same spot as the ball
    ( (null (find-ball-location grid) ) 'grab-ball)                          ; ball not found AND enemy not holding it -> my clone must be holding it already --> stay
    ( T (sembejir-fetch-ball self grid near-ball has-ball self-loc) )
  )
      
)

(defun sembejir-do-something-smart-with-the-ball (self grid near-ball has-ball self-loc)
  (if (not has-ball)  
    (sembejir-do-something-smart-without-the-ball self grid near-ball has-ball self-loc)
    (if (sembejir-list-surrounding-locations self-loc (sembejir-make-coord-enemy-p) grid )   ; we are near an enemy
      (values `(throw-ball ,@(car (sembejir-list-surrounding-locations self-loc (sembejir-make-coord-enemy-p) grid ))) #'sembejir-hump-enemy)  ; throw the ball at him
      (let ((all-enemies (sembejir-list-all-locations (sembejir-make-enemy-p) grid)))                                                 ; otherwise list all enemies
        (if (null (car all-enemies))
          (error "No enemies found!")
          (let ((enemy-surroundings (sembejir-list-surrounding-locations (sembejir-closest-target self-loc all-enemies) #'sembejir-can-throw-there grid ) )) ; list surrounding locations of the closest enemy
            (print "throwball> ");
            (format t "targeting ~S~%" (car all-enemies))
            (format t "~%enemy-surroundings ~S~%" enemy-surroundings)            
            (format t "nearest ~S to ~S~%" (sembejir-closest-target self-loc enemy-surroundings) self-loc)                                 
            (values `(throw-ball ,@(sembejir-closest-target self-loc enemy-surroundings)) #'sembejir-fetch-ball)                                    ; throw it near him
          )                    
        )  
      )
    )  
  )  
)

;;;; SUB-TACTICS

(defun sembejir-fetch-ball (self grid near-ball has-ball self-loc)    ; když ball neex (sebral ho hráč)
  (if near-ball 
     (values 'grab-ball #'sembejir-do-something-smart-with-the-ball)
     ;(values (sembejir-navigate self-loc (find-ball-location grid) grid) #'sembejir-fetch-ball)  ; simple navigation
     ;(values (sembejir-navigate-smarter self-loc (find-ball-location grid) grid self) #'sembejir-fetch-ball)  ; advanced navigation (for static environment)
     (values (sembejir-navigate-without-preplanning self-loc (find-ball-location grid) grid self) #'sembejir-fetch-ball)  ; pre-planning navigation
  ) 
)

(defun sembejir-run-away (self grid near-ball has-ball self-loc)    ; run away from ball
  (if (sembejir-enemy-armed grid)
    'stay
    (if (sembejir-enemy-has-balls grid)
      (values (sembejir-navigate-away self-loc (sembejir-enemy-has-balls grid) grid) #'sembejir-fetch-ball)  ; sembejir-navigate from player with balls
      (values (sembejir-navigate-away self-loc (find-ball-location grid) grid) #'sembejir-fetch-ball)  ; from ball
    )    
  )
)

(defun sembejir-hump-enemy (self grid near-ball has-ball self-loc) 
   (if (sembejir-enemy-has-balls grid)
      (values (sembejir-navigate self-loc (sembejir-enemy-has-balls grid) grid (lambda (a b) T)) #'sembejir-fetch-ball)      
      (sembejir-do-something-smart-without-the-ball self grid near-ball has-ball self-loc)
   )
)


; hledání pozice za hráčem -> pokud ho netrefíme, tak nedostane míč
(defun sembejir-make-suitable-for-overshoot-p (src-coord must-contain-this)
  (lambda (target grid)
    (and (identify-in-list (lambda (item) (equal item must-contain-this) ) (sembejir-trajectory src-coord target) )
         (sembejir-coord-in-game-area target grid)
    ) 
  )
)

; je trajektorie volná?
(defun sembejir-clear-view (src target grid)
  (not (member-if (lambda (elem) (not (null (apply #'aref grid elem)))) (butlast (cdr (sembejir-trajectory src target)))))
)

; najde enemáka na hranici dostřelu, na kterého je čistý výhled
(defun sembejir-find-suitable-target (grid self-loc enemy-locs)
  (let ((result-loc (sembejir-closest-target self-loc enemy-locs)))
    (dolist (enemy-loc enemy-locs)
      (when (and 
              (< (sembejir-euclidean-distance self-loc result-loc) (sembejir-euclidean-distance self-loc enemy-loc))
              (> *CAN-HIT-DIST* (sembejir-euclidean-distance self-loc enemy-loc))
              (sembejir-clear-view self-loc enemy-loc grid)
            )
        (setf result-loc enemy-loc) 
      )       
    )
    result-loc
  )
)

; název self-explanatory :) prostě vymyslí co s míčem :)
(defun sembejir-do-something-smart-with-the-ball (self grid near-ball has-ball self-loc)
  (if (sembejir-list-surrounding-locations self-loc (sembejir-make-coord-enemy-p) grid )   ; we are near an enemy
    (values `(throw-ball ,@(car (sembejir-list-surrounding-locations self-loc (sembejir-make-coord-enemy-p) grid ))) #'sembejir-hump-enemy)  ; throw the ball at him
    (let* ((all-enemies (sembejir-list-all-locations (sembejir-make-enemy-p) grid)) (target (sembejir-closest-target self-loc all-enemies)) )                                                 ; otherwise list all enemies
      (if (null (car all-enemies))
        (error "No enemies found!")
        (if (< *CAN-HIT-DIST* (sembejir-euclidean-distance target self-loc))
          (progn  
             (print "enemy-too-far> ");                                                                                       ; enemy too far
             (let ((new-relative-target (sembejir-combine-coordinates #'truncate (sembejir-combine-coordinates #'- target self-loc) '(2 2))))
                (format t "relative offset ~S~%" new-relative-target)
                (format t "absolute-position ~S~%" (sembejir-combine-coordinates #'+ new-relative-target self-loc))
                (values `(throw-ball ,@(sembejir-combine-coordinates #'+ new-relative-target self-loc)) #'sembejir-fetch-ball)                                    ; throw it at him            
             )
          )
          (progn                            ; existuje enemák blíž než *CAN-HIT-DIST*
            (print "throwball> ");
            (setf target (sembejir-find-suitable-target grid self-loc all-enemies))
            (format t "Targeting player at: ~S ~%" target)            
            (let ((overshoot-target target) (found-loc nil) )
              (block loop-outer
                (loop
                  (setf found-loc (sembejir-farthest-target self-loc (sembejir-list-surrounding-locations overshoot-target (sembejir-make-suitable-for-overshoot-p self-loc target) grid )))
                  (format t "FOUND-LOC: ~S ~%" found-loc)
                  (if found-loc
                    (if (or (equal found-loc overshoot-target) (< *CAN-HIT-DIST* (sembejir-euclidean-distance found-loc self-loc) ) (< (sembejir-euclidean-distance found-loc self-loc) (sembejir-euclidean-distance overshoot-target self-loc) ) ) ; pokud najdeme stejnou lokaci, vzdálenější jak *CAN-HIT-DIST*, nebo bližší --> konec
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
    (format t "base: ~S --> ~S vs. ~S result ~S" target-loc my-loc enemy-loc (< (sembejir-euclidean-distance target-loc enemy-loc) (sembejir-euclidean-distance target-loc my-loc) ))
    (when (< (sembejir-euclidean-distance target-loc enemy-loc) (sembejir-euclidean-distance target-loc my-loc) ) (return-from sembejir-enemy-closer-p T) ) 
  )
  nil 
)

(defun sembejir-do-something-smart-without-the-ball (self grid near-ball has-ball self-loc) 
;  (print "enemy-closer")
;  (print (sembejir-enemy-closer-p (find-ball-location grid) self-loc (list-all-locations (sembejir-make-enemy-p) grid)))
  (cond
    ( (and (sembejir-enemy-has-balls grid) (equal 1 (sembejir-euclidean-distance self-loc (sembejir-enemy-has-balls grid)))) (format t "~%>> Hump enemy") (sembejir-hump-enemy self grid near-ball has-ball self-loc) ) ; enemy is at the same spot as the ball and I'm next to him
    ( (null (find-ball-location grid) ) (format t "~%>> Grabbing ball") 'grab-ball)                          ; ball not found AND enemy not holding it -> my clone must be holding it already --> stay
    ( (sembejir-enemy-has-balls grid) (format t "~%>> Running away") (sembejir-run-away self grid near-ball has-ball self-loc) ) ; enemy is at the same spot as the ball
    ( (sembejir-enemy-closer-p (find-ball-location grid) self-loc (sembejir-list-all-locations (sembejir-make-enemy-p) grid)) (format t "~%>> Enemy gets the ball faster") (sembejir-run-away self grid near-ball has-ball self-loc) ) ; enemy will get to the ball faster --> run away
    ( T (format t "~%>> Getting ball") (sembejir-fetch-ball self grid near-ball has-ball self-loc) )
  )
      
)
