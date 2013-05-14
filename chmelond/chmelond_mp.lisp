;;; Ondrej Chmelar - chmelond@fit.cvut.cz
;;; evil student definition against students


(defconstant seek-hide-or-throw-db-student-name "CHM")

(defstructure (seek-hide-or-throw-db-student-body (:include db-agent-body (name seek-hide-or-throw-db-student-name))))

(defstructure (chmelond                
               (:include db-agent 
                         (program 'seek-hide-or-throw) 
                         (body (make-seek-hide-or-throw-db-student-body))
                         (name seek-hide-or-throw-db-student-name)))
    "A student that will be attempting to hit students in the second part of the semestral project.")

(defun seek-hide-or-throw (percept)
  "throw the ball at an agent if the ball is in my posession.
   grab the ball if it is on my square
   otherwise go to the ball"
  (let* ((me (car percept))	 
         (grid (cadr percept))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
         (holding-ball (object-contents me))
         (agent-locs (chmelond-find-agent-locations grid))
         (ball-loc (find-ball-location grid))
         (my-loc (chmelond-find-my-location grid)))
    (cond 
     ( (not agent-locs) 'stop )
     ( ball-on-my-loc 'grab-ball )
     ( holding-ball (chmelond-throw-ball-closest my-loc agent-locs grid))
     ( ball-loc (chmelond-my-go ball-loc my-loc agent-locs grid))
     ( t (chmelond-go-random))))) ;se stejne nestane
     
     
(defun chmelond-find-closest-point (point points)
	; (format t "finding closest point ~A ~A~%" point points)
	(if (null points) nil)
	(setf mindist 9999)
	(setf minpoint nil)
	(dolist (p points)
		(if (< (points-dist point p) mindist)
			(and (setf mindist (points-dist point p)) (setf minpoint p)))) 
	;(format t "point=~A~%" minpoint)
   minpoint)
			
     
(defun chmelond-throw-ball-closest (my-loc agent-locs grid)
	`(throw-ball ,@(chmelond-find-closest-point my-loc agent-locs)))		
		
(defun chmelond-backtrack (m target my-loc)
	`(throw-ball ,@(chmelond-step-back-to-one m target my-loc)))		
	
(defun chmelond-step-back-to-one (m target loc)
	(setf step (aref m (car loc) (elt loc 1)))
	(setf up-loc (chmelond-go-up-move loc))
	(setf down-loc (chmelond-go-down-move loc))
	(setf right-loc (chmelond-go-right-move loc))
	(setf left-loc (chmelond-go-left-move loc))
	(if (equal step target) (return-from chmelond-step-back-to-one loc)
		(if (equal (- step 1) (aref m (car up-loc) (elt up-loc 1))) (chmelond-step-back-to-one m target (list (car up-loc) (elt up-loc 1)))
			(if (equal (- step 1) (aref m (car down-loc) (elt down-loc 1))) (chmelond-step-back-to-one m target (list (car down-loc) (elt down-loc 1)))
				(if (equal (- step 1) (aref m (car right-loc) (elt right-loc 1))) (chmelond-step-back-to-one m target (list (car right-loc) (elt right-loc 1)))
					(if (equal (- step 1) (aref m (car left-loc) (elt left-loc 1))) (chmelond-step-back-to-one m target (list (car left-loc) (elt left-loc 1)))))))))
     
(defun chmelond-go-random ()
	(nth (random 4) '(go-left go-right go-up go-down)))

(defun chmelond-my-go (ball-loc my-loc agent-locs grid)
	(if (equal my-loc (chmelond-find-closest-point ball-loc (cons my-loc agent-locs)))
		(chmelond-go-to-ball ball-loc my-loc grid)
		(chmelond-go-away-from-ball ball-loc my-loc grid)))
		
(defun chmelond-go-to-ball (ball-loc my-loc grid)
	; (format t "going to ball ~A ~A~%" ball-loc my-loc)
	(case (chmelond-bfs (list ball-loc) my-loc grid)		;; pujdu od mice k sobe
		(chmelond-go-up-move 'go-down)						;; chmelond-bfs zjisti smer, odkud bych od mice prisel k sobe
		(chmelond-go-down-move 'go-up)						;; vydam se v protismeru
		(chmelond-go-left-move 'go-right)
		(chmelond-go-right-move 'go-left)))		
			
(defun chmelond-go-away-from-ball (ball-loc my-loc grid)
	; (format t "going away from ball ~A ~A~%" ball-loc my-loc)
	(case (chmelond-bfs (list ball-loc) my-loc grid)		;; pujdu od mice k sobe
		(chmelond-go-up-move 'go-up)						;; chmelond-bfs zjisti smer, odkud bych od mice prisel k sobe
		(chmelond-go-down-move 'go-down)						
		(chmelond-go-left-move 'go-left)
		(chmelond-go-right-move 'go-right)))		
	
(defun chmelond-bfs (start goal grid)	
  (declare (special *open*)
           (special *closed*)
           (special *map*)
           (special *path*)
           (special *grid*)
           (special *goal*))
  (setq *open* start)
  (setq *closed* nil)
  (setq *goal* goal)
  (setq *grid* grid)
  (setq *map* (make-array '(10 10) :initial-element 99))
  (setq *path* (make-array '(10 10) :initial-element nil))
  (dolist (item start)
	(setf (aref *map* (car item) (elt item 1)) 0))
  (setq *moves* '(chmelond-go-up-move chmelond-go-down-move chmelond-go-right-move chmelond-go-left-move))
  (chmelond-breadth-first))
  
(defun chmelond-breadth-first ()		
  (declare (special *open*)
		(special *closed*)
		(special *goal*)
		(special *grid*)
		(special *path*)
		(special *moves*))
		(cond ((null *open*) nil)
			(t (let ((state (car *open*)))
				(cond ((equal state *goal*) (aref *path* (car *goal*) (elt *goal* 1)))
                   (t (setq *closed* (cons state *closed*))
                      (setq *open* 
                            (append (cdr *open*)
								(chmelond-generate-steps state *moves*)))
                     (chmelond-breadth-first)))))))
                      
(defun chmelond-go-up-move (point)
	(return-from chmelond-go-up-move (list (car point) (+ (cadr point) 1))))
    
(defun chmelond-go-down-move (point)
	(return-from chmelond-go-down-move (list (car point) (- (cadr point) 1))))
    
(defun chmelond-go-right-move (point)
	(return-from chmelond-go-right-move (list (+ (car point) 1) (cadr point))))
        
(defun chmelond-go-left-move (point)
	(return-from chmelond-go-left-move (list (- (car point) 1) (cadr point))))

(defun chmelond-generate-steps (state moves)
  (declare (special *open*)
           (special *grid*)
           (special *map*)
           (special *path*)
           (special *closed*))
  (cond ((null moves) nil)
        (t (let ((child (funcall (car moves) state))
                 (rest (chmelond-generate-steps state (cdr moves))))
             (cond ((null child) rest)
				   ((member-if (lambda (a) (typep a 'percept-object-wall)) (apply #'aref *grid* child)) rest)
				   ((member-if (lambda (a) (and (typep a 'percept-object-agent) (not (equal (percept-object-name a) seek-hide-or-throw-db-student-name)))) (apply #'aref *grid* child)) rest)
                   ((member child rest :test #'equal) rest)
                   ((member child *open* :test #'equal) rest)
                   ((member child *closed* :test #'equal) rest)
                   (t 	(chmelond-set-distance-if-less *map* *path* state child (car moves)) 
						(cons child rest)))))))

(defun chmelond-set-distance-if-less (m p state child move)
	(if (> (aref m (car child) (elt child 1)) (+ (aref m (car state) (elt state 1)) 1))
		(cond 
			(t
				(setf (aref m (car child) (elt child 1)) (+ (aref m (car state) (elt state 1)) 1))
				(setf (aref p (car child) (elt child 1)) move)))))

(defun chmelond-find-agent-locations (grid)		
  (chmelond-find-X-locations #'chmelond-agent-pr grid))

(defun chmelond-find-X-locations (X-predicate grid)
  (setq locations nil)
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (identify-in-list X-predicate (aref grid numberx numbery))
        (setq locations (cons (list numberx numbery) locations))))) locations)


(defun chmelond-find-my-location (grid)
  (find-X-location #'chmelond-me-pr grid))

(defmethod chmelond-agent-pr ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) seek-hide-or-throw-db-student-name)) 
           (not (equal (percept-object-name obj) "B")))
      obj nil))
      
(defmethod chmelond-me-pr ((obj percept-object))
  (if (equal (percept-object-name obj) seek-hide-or-throw-db-student-name)
      obj nil))




