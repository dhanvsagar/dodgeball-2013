;;; ================================================================
;;; Student's agent definition:
;;; Petr Vales valespe3

(defconstant valespe-db-agent-name "PV")

(defstructure (valespe-db-agent-body (:include db-agent-body (name valespe-db-agent-name))))

(defstructure (valespe3                
               (:include db-agent 
                         (program 'valespe) 
                         (body (make-valespe-db-agent-body))
                         (name valespe-db-agent-name))))

(defun valespe (percept)
  (let* ((me (car percept))
          (grid (cadr percept))
          (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
          (holding-ball (object-contents me))
          (my-loc (object-loc me))
          (nearest-agent-loc (valespe-find-nearest-agent-location grid my-loc))
          (ball-loc (find-ball-location grid)))
    (cond 
      (holding-ball (valespe-throw-held-ball my-loc nearest-agent-loc)) 
      (ball-on-my-loc (valespe-grab-ball-on-my-location))
      (T (valespe-go-to-ball-loc my-loc ball-loc grid)))))

(defun valespe-find-nearest-agent-location (grid my-loc)
  (let* ((agents (valespe-find-agents-locations grid)))
    (valespe-get-nearest-agent-loc my-loc (cdr agents) (car agents))))

(defun valespe-find-agents-locations (grid)
  (let (agents)
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
        (dolist (item (aref grid numberx numbery))
          (when (and (percept-object-agent-p (eval item)) 
                      (not (equal (percept-object-name item) valespe-db-agent-name)))
              (setf agents (push (@ numberx numbery) agents)))))
    )
    (return-from valespe-find-agents-locations agents)))

(defun valespe-get-nearest-agent-loc (my-loc agents nearest-agent)
  (if (null agents) 
    nearest-agent
    (let* ((nearest-distance (valespe-get-loc-distance my-loc nearest-agent))
            (next-distance (valespe-get-loc-distance my-loc (car agents))))
      (cond 
        ((< next-distance nearest-distance) (valespe-get-nearest-agent-loc my-loc (cdr agents) (car agents)))
        (T (valespe-get-nearest-agent-loc my-loc (cdr agents) nearest-agent))))))

(defun valespe-throw-held-ball (my-loc nearest-agent-loc)
  (cond
    ((valespe-do-i-stand-next-to-him my-loc nearest-agent-loc) `(throw-ball ,@nearest-agent-loc))
    (T (valespe-throw-ball-next-to-agent my-loc nearest-agent-loc))))

(defun valespe-do-i-stand-next-to-him (my-loc nearest-agent-loc)
  (let* ((x (valespe-get-x-of-loc my-loc))
          (y (valespe-get-y-of-loc my-loc)))
    (or (valespe-loc-equal (list (+ x 1) y) nearest-agent-loc)
      (valespe-loc-equal (list (- x 1) y) nearest-agent-loc)
      (valespe-loc-equal (list x (+ y 1)) nearest-agent-loc)
      (valespe-loc-equal (list x (- y 1)) nearest-agent-loc))))

(defun valespe-throw-ball-next-to-agent (my-loc nearest-agent-loc) `(throw-ball ,@(valespe-get-loc-next-to-agent my-loc nearest-agent-loc)))

(defun valespe-get-loc-next-to-agent (my-loc nearest-agent-loc)
  (let* ((my-x (valespe-get-x-of-loc my-loc))
          (my-y (valespe-get-y-of-loc my-loc))
          (agent-x (valespe-get-x-of-loc nearest-agent-loc))
          (agent-y (valespe-get-y-of-loc nearest-agent-loc)))
    (cond ((> my-x agent-x) (list (+ agent-x 1) agent-y))
          ((< my-x agent-x) (list (- agent-x 1) agent-y))
          ((> my-y agent-y) (list agent-x (+ agent-y 1)))
          ((< my-y agent-y) (list agent-x (- agent-y 1))))))

(defun valespe-grab-ball-on-my-location () 'grab-ball)

(defun valespe-go-to-ball-loc (my-loc ball-loc grid)
  (valespe-get-action-from-path my-loc (valespe-find-path my-loc ball-loc grid)))

(defun valespe-find-path (from to grid)
    (solution-actions (A*-search (make-path-finding-problem :initial-state from :goal to :grid grid))))

(defun valespe-get-action-from-path (my-loc path)
  (if (null path)
    'stay
  (let ((next-loc (car path)))
    (cond ((> (valespe-get-x-of-loc my-loc) (valespe-get-x-of-loc next-loc)) 'go-left)
          ((< (valespe-get-x-of-loc my-loc) (valespe-get-x-of-loc next-loc)) 'go-right)
          ((> (valespe-get-y-of-loc my-loc) (valespe-get-y-of-loc next-loc)) 'go-down)
          ((< (valespe-get-y-of-loc my-loc) (valespe-get-y-of-loc next-loc)) 'go-up)
          (T 'stay)))))

(defun valespe-is-free-to-go (go-loc ball-loc grid) 
  (cond ((valespe-loc-equal go-loc ball-loc) T)
        (T (valespe-is-loc-empty go-loc grid))))

(defun valespe-is-loc-empty (loc grid) (let* ((loc-ref (aref grid (valespe-get-x-of-loc loc) (valespe-get-y-of-loc loc))))
  (cond ((eq (valespe-get-loc-content loc-ref grid) NIL) T)
        (T NIL))))

(defun valespe-get-loc-content (loc grid)
  (dolist (item loc)
    (when (or (percept-object-wall-p (eval item)) (percept-object-agent-p (eval item))) 
      (return-from valespe-get-loc-content item)))
  NIL)

(defun valespe-loc-equal (loc1 loc2)
  (and (equal (valespe-get-x-of-loc loc1) (valespe-get-x-of-loc loc2))
    (equal (valespe-get-y-of-loc loc1) (valespe-get-y-of-loc loc2))))

(defun valespe-get-x-of-loc (loc) (car loc))
(defun valespe-get-y-of-loc (loc) (cadr loc))
(defun valespe-get-loc-distance (loc1 loc2)
  (let* ((loc1-x (valespe-get-x-of-loc loc1))
          (loc1-y (valespe-get-y-of-loc loc1))
          (loc2-x (valespe-get-x-of-loc loc2))
          (loc2-y (valespe-get-y-of-loc loc2))
          (x (abs (- loc1-x loc2-x)))
          (y (abs (- loc1-y loc2-y))))
    (+ x y)))

(defstructure (path-finding-problem (:include problem)) (grid)) 

(defmethod successors ((problem path-finding-problem) state)
  (let* ((successors NIL)
        (grid (path-finding-problem-grid problem)))
    (dolist (neighbour (valespe-get-successors grid state))
      (setf successors (push (cons neighbour neighbour) successors)))
    successors))

(defun valespe-get-successors (grid where)
  (let* ((result)
         (x (xy-x where))
         (y (xy-y where)))
    (when (valespe-is-square-free (aref grid (- x 1) y))
      (setf result (push (make-xy :x (- x 1) :y y) result)))
    (when (valespe-is-square-free (aref grid (+ x 1) y))
      (setf result (push (make-xy :x (+ x 1) :y y) result)))
    (when (valespe-is-square-free (aref grid x (- y 1)))
      (setf result (push (make-xy :x x :y (- y 1)) result)))
    (when (valespe-is-square-free (aref grid x (+ y 1)))
      (setf result (push (make-xy :x x :y (+ y 1)) result)))
    result))

(defun valespe-is-square-free (square)
  (dolist (item square)
    (when (percept-object-ball-p (eval item)) (return-from valespe-is-square-free T)))
  (dolist (item square)
    (when (or (percept-object-wall-p (eval item)) (percept-object-agent-p (eval item)) ) 
      (return-from valespe-is-square-free nil)))
  T)

(defmethod valespe-goal-test ((problem path-finding-problem) state)
  (declare-ignore state)
  (equalp state (problem-goal problem)))

(defmethod valespe-edge-cost ((problem path-finding-problem) node action state)  
  (declare-ignore node action state) 1)

(defmethod valespe-h-cost ((problem path-finding-problem) state)
  (xy-distance (problem-goal problem) state))

;;; ==================================================================
