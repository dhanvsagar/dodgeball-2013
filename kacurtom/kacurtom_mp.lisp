;;; ================================================================
;;; Student's agent definition:

(defparameter *track-to-ball-stack* nil)
(defparameter *closed-nodes* nil)
(defvar *last-ball-loc* ())

 (defstructure (kacurtom    ; replace "my-agent" by your unique name, as e.g. FIT username
                (:include db-agent 
                  (body (make-kacurtom-body))
                  (program 'kacurtom-program)
                  (name "kacurtom"))) 
  "Your agent for db-world.")

(defstructure (kacurtom-body 
                (:include db-agent-body (name "ttk")))
;  (slot1 default1)  ; any specific extra slots your agent's body would need
;  ...
;  (slotn defaultn))
    )

(defun kacurtom-program (percept)
  (let* (  
         (agent-body (first percept))    ; extracts agent body from percept
         (me (car percept))
         (grid (cadr percept))
         (me-loc (object-loc me))
         (agents (kacurtom-extract-agents grid me-loc ))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))) )        
         
         (holding-ball (object-contents me))
         (ball-on-agent-loc (kacurtom-agent-ball-loc grid agents))         
         (ball-loc (kacurtom-find-ball grid))     
         (tmp nil)
         )
    (setf percept (second percept))      ; extracts proper percept 
    
         
    (when ball-on-agent-loc
            (setf *last-ball-loc* ball-loc)
      ; (return-from kacurtom-program (kacurtom-dodge-agent grid ball-on-agent-loc me-loc))
      (return-from kacurtom-program 'stay)

      )     
    
    (when ball-on-my-loc       
      (kacurtom-clean-ball-track)
      (return-from kacurtom-program 'grab-ball))
    
    (when holding-ball
      (print "holding-bal")     
      (kacurtom-clean-ball-track) 
      (setf tmp (kacurtom-shoot-this-mother-fucker grid (first agents) me-loc))
      (return-from kacurtom-program `(throw-ball ,@tmp )))
    
    (when ball-loc
      (if (not (equal *last-ball-loc* ball-loc))
          (kacurtom-clean-ball-track) 
          )
      ;ball is free to chase for
      (return-from kacurtom-program (kacurtom-best-path grid me-loc ball-loc))
      )
    
    
    ;AGENT IS ABOUT TO THROW
    (setf ball-loc *last-ball-loc*)
    (kacurtom-dodge-agent grid ball-loc me-loc)   
   
  ;  (nth (random 4) '(go-left go-right go-up go-down))    
    ) )


;;;
;;; SHOOT AGENT
;;;
(defun kacurtom-shoot-this-mother-fucker (grid agent me-loc)
  (let*( (agent-x (car agent) )
         (agent-y (second agent))
         
         (goup (list agent-x (kacurtom-inc agent-y) ))
         (godown (list agent-x (kacurtom-dec agent-y) ))
         (goleft (list (kacurtom-dec agent-x) agent-y )) 
         (goright (list (kacurtom-inc agent-x) agent-y )) 
        (freecells (list goup godown  goleft goright ))
        (result agent)

        )
    
    ;sort position to choose the furthes step to the agent location
    (setf freecells (sort freecells #'< :key #'(lambda (p1) (kacurtom-xydelta  p1 me-loc)                                
                                           )))
    ;remove invalid locations
    (setf freecells (remove-if #'(lambda (pos) (kacurtom-contains-object grid 'percept-object-wall pos )                                                                                                
                                                )
                              freecells ))
    
    ;if im too far from the closesst agent i just throw the ball to the closest free cell to the agent cell
    (if (> (kacurtom-xydelta me-loc agent) 4)
      (setf result (first freecells))
      ()
      )
    
  (return-from kacurtom-shoot-this-mother-fucker result)
  ))



;;;
;;;DODGE AGENT
;;;
(defun kacurtom-dodge-agent (grid agent-loc me-loc)
  (let* (
         (me-x (car me-loc) )
         (me-y (second me-loc))
         
         (goup (list me-x (kacurtom-inc me-y) ))
         (godown (list me-x (kacurtom-dec me-y) ))
         (goleft (list (kacurtom-dec me-x) me-y )) 
         (goright (list (kacurtom-inc me-x) me-y )) 
         (allpaths (list (list goup 'go-up) (list godown 'go-down) (list goleft 'go-left ) (list goright 'go-right) ))
         
         )
    
    ;sort position to choose the furthes step to the agent location
    (setf allpaths (sort allpaths #'> :key #'(lambda (p1) (kacurtom-xydelta  (first p1) agent-loc)                                
                                           )))
    ;remove steps that bumps
    (setf allpaths (remove-if #'(lambda (pos) (or (kacurtom-contains-object grid 'percept-object-agent (first pos) )
                                                (kacurtom-contains-object grid 'percept-object-wall (first pos) )                                                
                                                ))
                              allpaths ))
    ;;return the best action to take
    (second (first allpaths)  )
  ))


;;;
;;;kacurtom-clean-ball-track
;;;
(defun kacurtom-clean-ball-track ()
   
    (setf *track-to-ball-stack* ())
    (setf *closed-nodes* ()) 
    )

;;;
;;; AGENT BALL LOC - returns location of an agent holding the ball
(defun kacurtom-agent-ball-loc (grid agents)
  (loop for agent in agents do
        (if  (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid agent))        
            (return-from kacurtom-agent-ball-loc agent)
          )
        ) 
  )



;;;
;;;DECREMENT
(defun kacurtom-dec (num)
  (- num 1)
  )
;;;
;;;kacurtom-incREMENT
(defun kacurtom-inc (num)
  (+ num 1)
  )

;;;
;;;kacurtom-xydelta computes sum of coordinates difference of two objects
(defun kacurtom-xydelta (me object)
  ( let (
         (xdelta (- (car me) (car object)))
         (ydelta (- (second me) (second object)))                               

         )
    ( + (abs xdelta) (abs ydelta))   
   ))

  
  
;;;
;;;
;;;BEST PATH - select the step to take in order to get to the ball
;;;
(defun kacurtom-best-path (grid me-loc ball-loc)
  (let* ( (xdelta (- (car me-loc) (car ball-loc)))
         (ydelta (- (second me-loc) (second ball-loc)))                               
         (me-x (car me-loc) )
         (me-y (second me-loc))
         
         (goup (list me-x (kacurtom-inc me-y) ))
         (godown (list me-x (kacurtom-dec me-y) ))
         (goleft (list (kacurtom-dec me-x) me-y )) 
         (goright (list (kacurtom-inc me-x) me-y )) 
         ;list consists of first:xy and second:command items
         (allpaths (list (list goup 'go-up) (list godown 'go-down) (list goleft 'go-left ) (list goright 'go-right) ))
         (result (copy-tree allpaths))
         (next nil)
         (stack-element nil)
         (last-pos nil)        
         )

   
    (setf *last-ball-loc* ball-loc)

    
    ;if i did step back then i just pop last result
    (setf stack-element (first *track-to-ball-stack*))   
    (setf last-pos (first stack-element))
    (if  (equal me-loc last-pos)             
          (setf result (second (pop *track-to-ball-stack*)))        
        
        )


    
    ;sort position to choose the closest step to the ball location
    (setf result (sort result #'< :key #'(lambda (p1) (kacurtom-xydelta  (first p1) ball-loc)                                
                              )))
    
    
    
    ;remove positions that are forbiden to step in ie that bumps or are closed
    (setf result (remove-if #'(lambda (pos) (or (kacurtom-contains-object grid 'percept-object-agent (first pos) )
                                                (kacurtom-contains-object grid 'percept-object-wall (first pos) )
                                                (kacurtom-is-node-closed  (first pos))
                                                (equal (first pos) last-pos )
                                                ))
                            result ))
    
    ;prefer to choose to step back at last
    (if (kacurtom-find-action-to-loc allpaths last-pos)
        (setf result  (append result  (list (kacurtom-find-action-to-loc allpaths last-pos)))  )
      )
       
    ;NEXT STEP TO TAKE  
    (setf next (first result)) 
   

    ;creates new stack element which consist of current position and alternative steps
    (setf stack-element (list me-loc (rest result) ))

    
    ;if there are no other alternative steps or im stepping back then close this (cross)road
    ;else push alternatives on the stack
    (if (or (equal nil  (rest result))
            (equal last-pos (first next))
            )                      
         (push  me-loc *closed-nodes*  )           
            ;ELSE        
         (push stack-element *track-to-ball-stack*)         
      )   
    
    ;(print result)
    ;(print *track-to-ball-stack*)
    
    ;choose an action 
    (second next)   
   
  ))


;;;
;;;kacurtom-find-action-to-locACTION
;;;
(defun kacurtom-find-action-to-loc (paths loc )
  (loop for action in paths do
        (if (equal (first action) loc)
            (return-from kacurtom-find-action-to-loc action)
         
         )
        ) 
 )



;;;
;;;IS NODE CLOSED
;;;
(defun kacurtom-is-node-closed (tmpnode)
     (loop for node  in *closed-nodes* do
        (if (equal node tmpnode)
            (return-from kacurtom-is-node-closed node)         
         )
        ) 

    
    )


;;
;;EXTRACT AGENTS and sort from closest to the furthest one
;;
(defun kacurtom-extract-agents(grid me-loc)
  ; (kacurtom-find-object (lambda (a) (typep a 'percept-object-agent)) grid)
  (let (
        (result nil)
        )
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (kacurtom-identify-in-list #'agent-pp (aref grid numberx numbery))
        (push  (list numberx numbery) result) )))
    
    (setf result (sort result #'< :key #'(lambda (p1) (kacurtom-xydelta  p1 me-loc)                                
                              )))
   ;(result)
  ))



;;
;;kacurtom-contains-object
;;
(defun kacurtom-contains-object (grid object-type cords)
  (let (
        (cell (aref grid (first cords) (second cords) ))
        )
    
    (kacurtom-identify-in-list  #'(lambda (a) (typep a object-type)) cell)
    
  ))

;;
;;kacurtom-find-ball
;;
(defun kacurtom-find-ball (grid)  
  (kacurtom-find-object  (lambda (a) (typep a 'percept-object-ball)) grid  )
  )

;;
;;kacurtom-find-agent
;;
(defun kacurtom-find-agent(grid)
  ; (kacurtom-find-object (lambda (a) (typep a 'percept-object-agent)) grid)
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (kacurtom-identify-in-list #'agent-pp (aref grid numberx numbery))
        (return-from kacurtom-find-agent (list numberx numbery))))) nil

  )


;;
;;kacurtom-find-object
;;
(defun kacurtom-find-object (pred grid)
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (kacurtom-identify-in-list pred (aref grid numberx numbery))
        (return-from kacurtom-find-object (list numberx numbery))))) nil
  )

(defun kacurtom-identify-in-list (pred list)
  (dolist (item list)
    (when (funcall pred item)
      (return-from kacurtom-identify-in-list item))) nil)

(defmethod agent-pp ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) "ttk")) 
           (not (equal (percept-object-name obj) "B")))
      obj nil))




;;;
;(defun kacurtom-find-ball (grid)
 ; (dotimes (numberx (car (array-dimensions grid)))
  ;  (dotimes (numbery (cadr (array-dimensions grid)))
   ;   (when (kacurtom-identify-in-list #'(lambda (a) (typep a 'percept-object-ball)) (aref grid numberx numbery))
    ;    (return-from kacurtom-find-ball (list numberx numbery))))) nil )

;(defun find-aagent (grid)
;  (dotimes (numberx (car (array-dimensions grid)))
;    (dotimes (numbery (cadr (array-dimensions grid)))
;      (when (kacurtom-identify-in-list #'agent-pp (aref grid numberx numbery))
;        (return-from kacurtom-find-agent (list numberx numbery))))) nil )
;
(defun kacurtom-print-my-list (my-list)
  (cond ((equal nil my-list) nil)
        ( (atom (car my-list)) (format t "~A" (car my-list) ) (kacurtom-print-my-list (cdr my-list)) )
        (t (kacurtom-print-my-list (cdr my-list)) )
        
        )
  )

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
