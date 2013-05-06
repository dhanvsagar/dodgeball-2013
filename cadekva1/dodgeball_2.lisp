(defconstant cadekva1-agent-name "VC")
 
(defstructure 
  (cadekva1
    (:include db-agent 
                  (body (make-cadekva1-body))
                  (program 'cadekva1-program)
                  (name cadekva1-agent-name)
      )
    )
  )
 
(defstructure (cadekva1-body (:include db-agent-body (name cadekva1-agent-name))))
 
(defun cadekva1-program (percept)
    (let* ((me (first percept))
          (grid (second percept))
          (my-loc (object-loc me))
          (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
          (holding-ball (object-contents me))
          (agent-loc (closest-agent my-loc grid))
          (ball-loc (find-ball-or-agent-with-ball grid))          
          (agent-next-to-me (standing-next-to-agent my-loc agent-loc))
          (agent-neighbor-loc (closest-location-around-agent my-loc agent-loc grid))
          (shortest-path (bfs my-loc ball-loc grid))
          (next-step (first shortest-path)))
 
     (cond 
     ;; no more agents -> stop
     ((not agent-loc) 'stop)
 
     ;; ball on my location -> grab it
     (ball-on-my-loc 'grab-ball)     
 
     ;; holding ball and standing next to agent -> hit him
     ((and holding-ball agent-next-to-me) `(throw-ball ,@agent-loc))
 
     ;; holding ball -> throw it closer to agent
     (holding-ball `(throw-ball ,@agent-neighbor-loc))
    
     ;; not holding ball -> go for it
     ((not holding-ball) (go-on-position my-loc next-step)))))
 
(defun standing-next-to-agent (my-loc agent-loc)
  "Whether the agent is next to me."
  (if (= (manhattan-distance my-loc agent-loc) 1) t nil ))
 
(defun manhattan-distance (a b)
  "Get Manhattan distance between two points."
  (let ((ax (first a))
        (ay (second a))
        (bx (first b))
        (by (second b)))
 
  (+ (abs (- ax bx)) (abs (- ay by)))
))

(defun closest-agent(my-loc grid) 
  "Finds location of the closest agent."
  (let* ((agents (find-agents grid))
         (closest (first agents)))

    (dolist
      (loc (rest agents))
      (when (< (manhattan-distance my-loc loc) (manhattan-distance my-loc closest)) 
        (setf closest loc))) closest ))

(defun find-agents (grid)
  "Finds location of all agents."
  (let ((agent) (agents))
    (dotimes (i (first (array-dimensions grid)))
      (dotimes (j (second (array-dimensions grid)))
        (when (setf agent (identify-in-list #'agent-predicate (aref grid i j)))
          (push (list i j) agents)))) agents ))
 
(defun closest-location-around-agent (my-loc agent-loc grid)
  "Determine closest location so agent can't catch the ball when throwing it behind him." 
(let* ((neighbors (get-4-neighborhood agent-loc grid))
       (empty-neighbors (remove-if-not #'(lambda (x) (is-position-free x grid)) neighbors)))

  (setf closest (first empty-neighbors))
  (dolist 
    (loc (rest empty-neighbors))
    (when (< (manhattan-distance my-loc loc) (manhattan-distance my-loc closest)) 
     (setf closest loc)
    )
  )
  closest
))
 
(defun is-ball-on-location (loc grid)
  "Whether a ball is on the location."
(let* ((x (first loc))
      (y (second loc))
      (cell (aref grid x y))
      (percepted-object (identify-in-list #'my-ball-p cell)))
  (when (equal nil percepted-object) (return-from is-ball-on-location nil))
  (if (equal (percept-object-name percepted-object) "B") t nil)))
 
(defun find-ball-or-agent-with-ball (grid)
  "This implicitly include the situation of bumping the agent with ball."
  (find-X-location #'ball-or-agent-with-ball-predicate grid))
 
(defun get-4-neighborhood (position grid)
 "Return 4-neighborhood of a position."
 (let*  ((x (first position))
        (y (second position)) 
        (north (list x (+ y 1)))
        (east (list (+ x 1) y))
        (south (list x (- y 1)))
        (west (list (- x 1) y)))
 
(list north east south west)))
 
(defun expand-neighbors(position grid map) 
  "Expand empty un-visited neighbor cells."
  (let* ((4-connected-neighbors (get-4-neighborhood position grid))
         (free-positions (remove-if-not #'(lambda (x) (is-position-free x grid)) 4-connected-neighbors))
         (free-unvisited-positions (remove-if-not #'(lambda (x) (is-unvisited x map)) free-positions)))
 
    free-unvisited-positions
))
 
(defun go-on-position (my-loc target-loc)
 "Generate proper action to go towards target position."
 (let* ((x (first my-loc))
        (y (second my-loc))
        (target-x (first target-loc))
        (target-y (second target-loc)))
(cond
 ((> x target-x) 'go-left)
 ((< x target-x) 'go-right)
 ((> y target-y) 'go-down)
 ((< y target-y) 'go-up))))
 
(defun is-position-free(position grid)
 "Whether position is free."
(let* ((x (first position))
      (y (second position))
      (cell (aref grid x y)))
  
  (if (or (equal cell nil) (is-ball-on-location position grid)) t nil)))
 
(defun find-agent-location (grid)
  "Return location of first agent found."
  (find-X-location #'agent-predicate grid))
 
(defun bfs (start target grid)
  "Find a shortest-path using simple BFS search."
    (let ((map-of-visited (make-array (array-dimensions grid) :initial-element nil))
         (queue nil)
         (shortest-path nil))
      
        ; init BFS queue, start and target node
        (push start queue)
        (setf (apply #'aref map-of-visited start) "S")
 
        ; while queue is not empty or target is found
        (loop                     
         (setf current (pop queue))                 
         (setf neighborhood (expand-neighbors current grid map-of-visited))
         
         ; expand neighbors and set their parent
         (dolist 
             (neighbor neighborhood)
 
              ; add location to end          
              (setf queue (append queue (list neighbor)))  
  
              ; add location parent              
              (setf (apply #'aref map-of-visited neighbor) current)          
         )            
         (when (or (equal current target) (eq nil queue)) (return))
         )        
        
        ; backtrack the shortest path
        (loop
         (let ((current target)))
         (when (equal current start) (return))
         (push current shortest-path)
         (setf current (apply #'aref map-of-visited current))         
         )
 
        shortest-path
))
 
(defun is-unvisited (location map)
  "Finds out whether a location is visited."
  (let* ((x (first location))
         (y (second location))
         (cell (aref map x y))
         (is-start (equal (aref map x y) "S")))
    
    (if (and (equal cell nil) (not is-start)) t nil)))
 
(defmethod agent-predicate ((obj percept-object))
  "Predicate that identifies agent."
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) cadekva1-agent-name)) 
           (not (equal (percept-object-name obj) "B")))
      obj nil))
 
(defmethod ball-or-agent-with-ball-predicate ((obj percept-object))
  "Predicate that identifies ball or agent with ball."
  (cond ((equal (percept-object-name obj) "B") obj)
        ((equal (percept-object-name obj) "BWT") obj)
        (t nil)))
