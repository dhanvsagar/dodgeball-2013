(defconstant kersnmar-agent-name "KER")

(defstructure (kersnmar
              (:include db-agent 
                (body (make-body-kersnmar))
                (program 'kersnmar-agent)
                (name kersnmar-agent-name))) 
                "Your agent for db-world."
)

(defstructure (body-kersnmar (:include db-agent-body (name kersnmar-agent-name))))

;;;
(defun kersnmar-agent (percept)
  (let* ( (me (car percept))
          (grid (cadr percept))
          (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
          (holding-ball (object-contents me))
          (kersnmar-loc (object-loc me))
          (agent-loc (find_agent_loc grid))
          (ball-loc (find_ball_loc grid)) 
          (free-space-agent (find_closest_free_agent_cell kersnmar-loc grid))
          (closest-agent (find_closest_agent kersnmar-loc grid)))

  (cond 
        ; ball is on the same position as me, i am gonna take it
        (ball-on-my-loc 'grab-ball)

        ; i am holding ball, some agent is next to me, now i am throwing ball to agent next to me
        ((and holding-ball (= (get_distance kersnmar-loc closest-agent) 1))
          `(throw-ball ,@closest-agent))

        ; i am holding ball, no agent is next to me, now i am gonna throw it one cell next to closes agent
        ((and holding-ball (/= (get_distance kersnmar-loc closest-agent) 1))
          `(throw-ball ,@free-space-agent))

        ; i am not holding ball, even ball's and my position are dismissed, now i am gonna to go ball position
        ((and (not (equal kersnmar-loc ball-loc)) (not (equal ball-loc closest-agent)))
          (dijkstra kersnmar-loc ball-loc grid)
         )

        ; i threw a ball to agent next to me and hit him, now i am gonna bump him
        (t (bump kersnmar-loc closest-agent)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bumps agent
;;; me      : my position
;;; agent   : agents
;;; 
;;; return  : allowed movement
(defun bump (me agent)
  (let* ( (mex (car me))
          (mey (cadr me))
          (agentx (car agent))
          (agenty (cadr agent)))

  (cond ((< mey agenty) 'go-up)
        ((< mex agentx) 'go-right)
        ((> mey agenty) 'go-down)
        ((> mex agentx) 'go-left))
  ) 
)

;;; Looking for objects in grid
;;; X-predicate : predicate to make decision of object type
;;; grid        : 2d array containing all game field
;;;
;;; return      : position of cell
(defun find_location (X-predicate grid)
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (identify_in_list X-predicate (aref grid numberx numbery))
        (return-from find_location (list numberx numbery))))) nil )

;;; Gets a list of positions all agents
;;; X-predicate : predicate to make decision of object type
;;; grid        : 2d array containing all game field
;;;
;;; return      : agent's location
(defun find_agent_locations (grid)
 (let ((agents nil))
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (identify_in_list #'agent_p (aref grid numberx numbery))
        (setf agents (append `((,numberx ,numbery)) agents))
    )))

    agents ;return
 )
)

;;; Find closest agent to my agent
;;; me    : my position
;;; grid  : 2d array containing all game field
;;; 
;;; return : closest agent
(defun find_closest_agent (me grid)
  (let* ((agents (find_agent_locations grid))
        (closest (car agents)))

  (mapc #'(lambda (z) (if (> (get_distance closest me) (get_distance z me)) (setf closest z))) (cdr agents))    

  closest ; return
  )
)

;;; Find closest free cell next to agent.
;;; me      : my position
;;; grid    : 2d array containing all game field
;;;
;;; return  : closest free cell next to agent
(defun find_closest_free_agent_cell (me grid)
  (let* ( (agentPos (find_closest_agent me grid))
          (freePos (getNeighbors (check_free agentPos grid) agentPos))
          (closest (car freePos)) )

  (mapc #'(lambda (z) (if (not (null z)) (if (> (get_distance closest me) (get_distance z me)) (setf closest z)))) (cdr freePos))    

  closest ; return
  )   
)

;;; Get manhattan distance of two points
;;; A       : first point
;;; B       : second point
;;;
;;; return  : distance between point A and B
(defun get_distance (A B)
  (let ((Ax (car A))
        (Ay (cadr A))
        (Bx (car B))
        (By (cadr B)))

  (+ (abs (- Ax Bx)) (abs (- Ay By)))
  )
)

(defun find_agent_loc (grid)
  (find_location #'agent_p grid))

(defun find_ball_loc (grid)
  (find_location #'ball_p grid))

(defun identify_in_list (pred list)
  (dolist (item list)
    (when (funcall pred item)
      (return-from identify_in_list item))) nil)

;;; Checks if object is an agent
;;; obj             : type of object
;;; percept-object  :
;;;
;;; return          : T
;;;                   nil
(defmethod agent_p ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) kersnmar-agent-name)) 
           (not (equal (percept-object-name obj) "B")))
      obj nil))

;;; Checks if object is a ball
;;; obj             : type of object
;;; percept-object  :
;;;
;;; return          : T
;;;                   nil
(defmethod ball_p ((obj percept-object))
  (if (equal (percept-object-name obj) "B")
      obj nil))

;;; Finding a path to go for ball.
;;; me-pos    : my position
;;; ball-pos  : position of ball
;;;
;;; return    : allowed movement
(defun go_for_ball (me-pos ball-pos)
  (cond 
    ((> (car me-pos) (car ball-pos)) 'go-left)
    ((< (car me-pos) (car ball-pos)) 'go-right)
    ((> (cadr me-pos) (cadr ball-pos)) 'go-down)
    ((< (cadr me-pos) (cadr ball-pos)) 'go-up)
    (t 'stay)
  )
)

;;; Checks free cells around clockwise.
;;; pos     : my position
;;; grid    : 2d array containing all game field
;;;
;;; return  : list of four checked free cells
(defun check_free (pos grid)
 (let*  ( (numberx (car pos))
          (numbery (cadr pos))
          (dimensionx (1- (car (array-dimensions grid))))
          (dimensiony (1- (cadr (array-dimensions grid)))) )

  (list
    (check_free_cell (list numberx (try-plus numbery dimensiony)) grid)
    (check_free_cell (list (try-plus numberx dimensionx) numbery) grid)
    (check_free_cell (list numberx (try-minus numbery)) grid)
    (check_free_cell (list (try-minus numberx) numbery) grid)
    )
  )
)

;;; Checks if cell is free. 
;;; pos     : controlled cell
;;; grid    : 2d array containing all game field
;;;
;;; return  : T
;;;           nil
(defun check_free_cell (pos grid)
  (let* ( (numberx (car pos))
          (numbery (cadr pos))
          (dimensionx (1- (car (array-dimensions grid))))
          (dimensiony (1- (cadr (array-dimensions grid)))) )

  (cond ((null numberx) nil)
        ((null numbery) nil)
        ((identify_in_list #'agent_p (aref grid numberx numbery)) nil)
        ((>= numberx dimensionx) nil)
        ((>= numbery dimensiony) nil)
        ((<= numberx 0) nil)
        ((<= numbery 0) nil)
        (t t) )
  ) 
)

;;; Checking higher borders of game field. Prevention to out of range errors.
;;; value   : value for check
;;; maximum : dimension of game field
;;;
;;; return  : nil ; out of range
;;;         : (1+ value)
(defun try-plus (value maximum)
 (if  (>= (1+ value) maximum)
      nil
      (1+ value))
)

;;; Checking lower borders of game field. Prevention to out of range errors.
;;; value   : value for check
;;;
;;; return  : nil ; out of range
;;;         : (1- value)
(defun try-minus (value)
 (if  (<= (1- value) 0)
      nil
      (1- value))
)

;;; Dijkstra algorithm for finding shortest path.
;;; me      : position of my agent
;;; target  : position of desired place
;;; grid    : 2d array containing all game field
;;;
;;; return  : allowed movement
(defun dijkstra (me target grid)
  (let* ((unvisited (getUnvisited me grid))
         (visited nil)
         (inf 100) ; 100 like infinity value
         (dimensionx (1- (car (array-dimensions grid))))
         (dimensiony (1- (cadr (array-dimensions grid))))
         (lengthArray (make-array `(,dimensionx ,dimensiony) :initial-element inf))
         (moveArray (make-array `(,dimensionx ,dimensiony) :initial-element nil))
         (actualNode me)
         (actualMoveNode target)
         )

        (setf (aref lengthArray (car me) (cadr me)) 0) ; set the start position to 0 length

        (loop while (not (equal actualNode target)) do ;looping till target node is unvisited
           (setf testingNodes (getNeighbors (check_free actualNode grid) actualNode)) ; get neighbors of expanded node
           (setf actualLength (aref lengthArray (car actualNode) (cadr actualNode)))
           (updateArrays actualLength actualNode testingNodes lengthArray moveArray); set new lengths and moves of neighbors
           (setf unvisited (remove actualNode unvisited :test #'equal))
           (setf actualNode (getActualNode unvisited lengthArray)) ; choose and set new actual mode
        ) 

        ;;; going backward from target to start position
        (loop while (not (equal me actualMoveNode)) do
          (multiple-value-bind (dir pos) 
           (getNewPosition (aref moveArray (car actualMoveNode) (cadr actualMoveNode)) actualMoveNode)
           (setf actualMoveNode pos)
           (setf direction dir))
        )
        
        direction ; return
  ) 
)

;;; Helps to move from target to my agent.
;;; move      : tested movement
;;; node      : actual node
;;;
;;; return    : chosen movement 
;;; return2   : new actual position
(defun getNewPosition (move node)
  (let* ( (x (car node))
          (y (cadr node)) )

  (cond ((eq move 1) (values 'go-down (list x (1+ y))))
        ((eq move 2) (values 'go-left (list (1+ x) y)))
        ((eq move 3) (values 'go-up (list x (1- y))))
        ((eq move 4) (values 'go-right (list (1- x) y))) )
  )
)

;;; Get node (for expanding) with shortest path from my agent to temporary target.
;;; unvisited   : unvisited nodes
;;; lengthArray : array o lengths from my agent to target
;;;
;;; return      : position of actual node
(defun getActualNode (unvisited lengthArray)
  (cond ((not (null (car unvisited)))
  (let* ( (unvisitedNode (car unvisited))
         (x (car unvisitedNode))
         (y (cadr unvisitedNode))
         (tmpMin (aref lengthArray x y)) )

    (loop for i in unvisited do
     (when (> tmpMin (aref lengthArray (car i) (cadr i))) 
      (setf tmpMin (aref lengthArray (car i) (cadr i)))
      (setf x (car i))
      (setf y (cadr i)) ))     

    (list x y)
  ) 
  )
  )
)

;;; Updates arrays during dijkstra algoritm.
;;; len           : actual length
;;; expandedNode  :
;;; nodes         : 
;;; lengthArray   : array o lengths from my agent to target
;;; moveArray     : array of movements from target to my agent
(defun updateArrays (len expandedNode nodes lengthArray moveArray)
  (cond ((not (null (car nodes)))
          (let* ( (node (car nodes))
                  (actualLength (aref lengthArray (car node) (cadr node)))
                  (newLength (1+ len))
                  (x (car node))
                  (y (cadr node)) )
  
          (if (< newLength actualLength)
            (progn 
              (setf (aref lengthArray x y) newLength)
              (setf (aref moveArray x y) (getMove expandedNode node))
          ))
  
          (updateArrays len expandedNode (cdr nodes) lengthArray moveArray)
        ))

        ((> (length nodes) 0)
          (updateArrays len expandedNode (cdr nodes) lengthArray moveArray))
  )
)

;;; Choose a direction of next agent step.
;;; expandedNode  : 
;;; neighborNode  :
;;;
;;; return        : allowed movement
(defun getMove (expandedNode neighborNode)
 (let* ( (expandedx (car expandedNode))
         (expandedy (cadr expandedNode))
         (neighborx (car neighborNode))
         (neighbory (cadr neighborNode)) )
  
    (cond 
     ((> expandedy neighbory) 1) ;up
     ((> expandedx neighborx) 2) ;right
     ((< expandedy neighbory) 3) ;down
     ((< expandedx neighborx) 4) ;left
     (t nil))
  )
)

;;; Fills a list with all coordinates from game filled without agent's positions.
;;; me      : position of my agent
;;; grid    : 2d array containing all game field
;;;
;;; return  : unvisited cells
(defun getUnvisited (me grid)
 (let ((locations nil))
  (dotimes (numberx (- (car (array-dimensions grid)) 2))
    (dotimes (numbery (- (cadr (array-dimensions grid)) 2))
      (if (not (identify_in_list #'agent_p (aref grid (1+ numberx) (1+ numbery))))
        (setf locations (append `((,(1+ numberx) ,(1+ numbery))) locations))
    )))

    (remove me locations :test #'equal)
 )
)

;;;  Get positions of free cells around my agent.
;;;  predicate  : information about free cells
;;;  pos        : position of my agent
;;;
;;;  return     : list of four positons 
(defun getNeighbors (predicate pos)
  (let* ((x (car pos))
         (y (cadr pos)))

  (list
   ( if (car    predicate) (list x (1+ y)) ) ;up
   ( if (cadr   predicate) (list (1+ x) y) ) ;right
   ( if (caddr  predicate) (list x (1- y)) ) ;down
   ( if (cadddr predicate) (list (1- x) y) ) ;left
  ) 
  )
)
