;;; Ondrej Chmelar - chmelond@fit.cvut.cz
;;; evil student definition against agents


(defconstant kill-em-all-db-student-name "chmelond")

(defstructure (kill-em-all-db-student-body (:include db-agent-body (name kill-em-all-db-student-name))))

(defstructure (chmelond                
               (:include db-agent 
                         (program 'kill-em-all-agent) 
                         (body (make-kill-em-all-db-student-body))
                         (name kill-em-all-db-student-name)))
    "A student that will be attempting to hit agents in the first part of the semestral project.")

(defun kill-em-all-agent (percept)
  "throw the ball at an agent if the ball is in my posession.
   grab the ball if it is on my square
   otherwise go to the ball"
  (let* ((me (car percept))					
         (grid (cadr percept))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
         (holding-ball (object-contents me))
         (agent-locs (find-agent-locations grid))
         (ball-loc (find-ball-location grid))
         (my-loc (find-my-location grid)))
    (cond 
     ( (not agent-locs) 'stop )
     ( ball-on-my-loc 'grab-ball )
     ( holding-ball (throw-ball-intelligently my-loc agent-locs grid))
     ( ball-loc (go-to-ball ball-loc my-loc grid))
     ( t (go-random))))) 
     
     
(defun throw-ball-intelligently (my-loc agent-locs grid)
	(declare (special *map*))
	(bfs agent-locs my-loc grid)
	(case (aref *map* (car my-loc) (elt my-loc 1))
		(1 (backtrack *map* 0 my-loc))
		(otherwise (backtrack *map* 1 my-loc))))
		
		
(defun backtrack (m target my-loc)
	`(throw-ball ,@(step-back-to-one m target my-loc)))		
	
(defun step-back-to-one (m target loc)
	(setf step (aref m (car loc) (elt loc 1)))
	(setf up-loc (go-up-move loc))
	(setf down-loc (go-down-move loc))
	(setf right-loc (go-right-move loc))
	(setf left-loc (go-left-move loc))(format t "~A ~A~%" step loc)
	(if (equal step target) (return-from step-back-to-one loc)
		(if (equal (- step 1) (aref m (car up-loc) (elt up-loc 1))) (step-back-to-one m target (list (car up-loc) (elt up-loc 1)))
			(if (equal (- step 1) (aref m (car down-loc) (elt down-loc 1))) (step-back-to-one m target (list (car down-loc) (elt down-loc 1)))
				(if (equal (- step 1) (aref m (car right-loc) (elt right-loc 1))) (step-back-to-one m target (list (car right-loc) (elt right-loc 1)))
					(if (equal (- step 1) (aref m (car left-loc) (elt left-loc 1))) (step-back-to-one m target (list (car left-loc) (elt left-loc 1)))))))))
     
(defun go-random ()
	(nth (random 4) '(go-left go-right go-up go-down)))

(defun go-to-ball (ball-loc my-loc grid)
	(case (bfs (list ball-loc) my-loc grid)		;; pujdu od mice k sobe
		(go-up-move 'go-down)						;; bfs zjisti smer, odkud bych od mice prisel k sobe
		(go-down-move 'go-up)						;; vydam se v protismeru
		(go-left-move 'go-right)
		(go-right-move 'go-left)))
	
(defun bfs (start goal grid)	
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
  (setq *moves* '(go-up-move go-down-move go-right-move go-left-move))
  (breadth-first))
  
(defun breadth-first ()		
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
								(generate-steps state *moves*)))
                     (breadth-first)))))))
                      
(defun go-up-move (point)
	(return-from go-up-move (list (car point) (+ (cadr point) 1))))
    
(defun go-down-move (point)
	(return-from go-down-move (list (car point) (- (cadr point) 1))))
    
(defun go-right-move (point)
	(return-from go-right-move (list (+ (car point) 1) (cadr point))))
        
(defun go-left-move (point)
	(return-from go-left-move (list (- (car point) 1) (cadr point))))

(defun generate-steps (state moves)
  (declare (special *open*)
           (special *grid*)
           (special *map*)
           (special *path*)
           (special *closed*))
  (cond ((null moves) nil)
        (t (let ((child (funcall (car moves) state))
                 (rest (generate-steps state (cdr moves))))
             (cond ((null child) rest)
				   ((member-if (lambda (a) (typep a 'percept-object-wall)) (apply #'aref *grid* child)) rest)
				   ((member-if (lambda (a) (and (typep a 'percept-object-agent) (not (equal (percept-object-name a) kill-em-all-db-student-name)))) (apply #'aref *grid* child)) rest)
                   ((member child rest :test #'equal) rest)
                   ((member child *open* :test #'equal) rest)
                   ((member child *closed* :test #'equal) rest)
                   (t 	(set-distance-if-less *map* *path* state child (car moves)) 
						(cons child rest)))))))

(defun set-distance-if-less (m p state child move)
	(if (> (aref m (car child) (elt child 1)) (+ (aref m (car state) (elt state 1)) 1))
		(cond 
			(t
				(setf (aref m (car child) (elt child 1)) (+ (aref m (car state) (elt state 1)) 1))
				(setf (aref p (car child) (elt child 1)) move)))))

(defun find-agent-locations (grid)		
  (find-X-locations #'agent-pr grid))

(defun find-X-locations (X-predicate grid)
  (setq locations nil)
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (identify-in-list X-predicate (aref grid numberx numbery))
        (setq locations (cons (list numberx numbery) locations))))) locations)


(defun find-my-location (grid)
  (find-X-location #'me-pr grid))

(defmethod agent-pr ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) kill-em-all-db-student-name)) 
           (not (equal (percept-object-name obj) "B")))
      obj nil))
      
(defmethod me-pr ((obj percept-object))
  (if (equal (percept-object-name obj) kill-em-all-db-student-name)
      obj nil))
