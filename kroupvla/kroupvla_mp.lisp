;;; File: dodgeball.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;; ================================================================
;;; Student's agent definition:

;; This is to be defined when designing a new student agent 

(defconstant kroupvla-agent-name "KR")

(defstructure (kroupvla-agent-body (:include db-agent-body (name kroupvla-agent-name)))
  (state 0)
  (command-queue '()))

(defstructure (kroupvla                
               (:include db-agent 
                         (program 'kroupvla-agent-program) 
                         (body (make-kroupvla-agent-body))
                         (name kroupvla-agent-name)))
    "Cheating agent.")

(defun kroupvla-agent-program (percept)
  (let* ((me (car percept))
         (grid (cadr percept)))
    (print (kroupvla-agent-body-command-queue me))
    (print (kroupvla-agent-body-state me))
    (next-action grid me)))

;; curry function from http://cl-cookbook.sourceforge.net/functions.html
(defun curry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))

;; core agent definition

(defun next-action (grid me)
    (when (= 0 (length (kroupvla-agent-body-command-queue me)))
      (setf (kroupvla-agent-body-state me) (next-state (kroupvla-agent-body-state me)))
      (setf (kroupvla-agent-body-command-queue me) (issue-commands (kroupvla-agent-body-state me) grid))
      (print 'issuing-commands)
      (print (kroupvla-agent-body-command-queue me)) 
      (print (kroupvla-agent-body-state me))
      )
    (pop (kroupvla-agent-body-command-queue me)))

;; agent state machine

(defun next-state (state)
  (cdr (assoc state '((0 . 1) (1 . 2) (2 . 3) (3 . 1))))) 

(defun issue-commands (state grid)
  (funcall (command-functions state) grid))
  
(defun command-functions (state)
  (cdr (assoc state '((1 . state-1-commands) (2 . state-2-commands) (3 . state-3-commands)))))

(defun state-1-commands (grid)
  (go-grab-ball grid))

(defun state-2-commands (grid)
  (append (list (throw-ball-front-of-agent grid)) (go-harass-agent grid)))
 
(defun state-3-commands (grid)
  (append '(grab-ball) (list (throw-ball-neighbor-agent grid)) '(stay) (list (bump-neighbor-agent grid)) (list (bump-neighbor-agent grid)) (list (bump-neighbor-agent grid)) '(stay) ))
  
;; functions to realize agent STM

(defun go-grab-ball (grid)
  (append (walk-to-ball grid (find-student-location grid)) '(grab-ball)))

(defun go-harass-agent (grid)
  (walk-to-agent grid (find-student-location grid)))

(defun throw-ball-front-of-agent (grid)
  `(throw-ball ,@(next-to-closest-agent grid)))

(defun throw-ball-neighbor-agent (grid)
  `(throw-ball ,@(neighbor-agent-tile grid)))

(defun bump-neighbor-agent (grid)
  (direction (find-student-location grid) (neighbor-agent-tile grid)))

(defun neighbor-agent-tile (grid)
  (dolist (tile (neighbor-tiles (find-student-location grid)))
    (when (identify-in-list #'evil-agent-p (aref grid (car tile) (cadr tile)))
      (return-from neighbor-agent-tile tile))))

(defun next-to-closest-agent (grid)
  (car (find-nearest (curry #'evil-agent-in-neighborhood-pred grid) (cost-map-player grid))))

(defun evil-agent-pred (grid til)
  (identify-in-list #'evil-agent-p (aref grid (car tile) (cadr tile))))

(defun evil-agent-in-neighborhood-pred (grid til)
  (dolist (tile (neighbor-tiles til))
    (when (identify-in-list #'evil-agent-p (aref grid (car tile) (cadr tile)))
      (return-from evil-agent-in-neighborhood-pred t))))

(defun find-nearest (predicate cost-map)
  (nearest (find-matching predicate cost-map)))

(defun nearest (alist)
  (reduce #'(lambda (entry-1 entry-2) (if (< (cdr entry-1) (cdr entry-2)) entry-1 entry-2)) alist))
  
(defun find-matching (predicate cost-map)
  (remove-if-not (lambda (entry) (funcall predicate (car entry))) cost-map))

(defmethod evil-agent-p ((obj percept-object))
  (if (equal (percept-object-name obj) "WB")
      obj nil))
  
(defun walk-to-agent (grid self-tile)
  (path-commands (create-path self-tile (next-to-closest-agent grid) (cost-map grid self-tile))))

(defun walk-to-ball (grid self-tile)
  (path-commands (create-path self-tile (find-ball-location grid) (cost-map grid self-tile))))

(defun path-commands (path)
  (loop for (from to) on path while to 
        collect (direction from to)))

(defun direction (tile-from tile-to)
  (delta-to-direction (delta tile-from tile-to)))

(defun delta (tile-from tile-to)
  (list (- (car tile-to) (car tile-from)) (- (cadr tile-to) (cadr tile-from))))

(defun delta-to-direction (tile-delta)
  (cdr (assoc tile-delta '(((1 0) . go-right) ((-1 0) . go-left) ((0 1) . go-up) ((0 -1) . go-down)) :test #'equal)))

(defun create-path (start-tile goal-tile cost-map)
  "if goal-tile is not in cost-map, then it is unreachable"
  (if (alist-get goal-tile cost-map) (reverse (reconstruct-path (list goal-tile) goal-tile cost-map)) nil))

(defun reconstruct-path (path actual-tile cost-map)
  (if (eq (cost-of-tile actual-tile cost-map) 0) (return-from reconstruct-path path)
      (let ((tiles-back (steps-back (1- (cost-of-tile actual-tile cost-map)) cost-map)))
        (dolist (candidate tiles-back)
          (when (neighbors candidate actual-tile) (return-from reconstruct-path (reconstruct-path (append path (list candidate)) candidate cost-map)))
          )
        )
      )
  )

(defun steps-back (cost cost-map)
  (mapcar #'car (remove-if-not (lambda (entry) (equal (cdr entry) cost)) cost-map)))

(defun neighbors (tile-1 tile-2)
  (if (member tile-1 (neighbor-tiles tile-2) :test #'equal) t nil))

(defun cost-map-player (grid)
  "computes cost map for student player"
  (cost-map grid (find-student-location grid)))

(defun cost-map (grid start-tile)
  "computes cost map from start-tile to every reachable tile"
  (setq tile-cost (list))
  (setq queue (list))
  (setq queue (enqueue queue start-tile))
  (setq tile-cost (add-tile-cost tile-cost start-tile 0))
    ;(print (unvisited-neighbor-tiles grid (list 4 5) tile-cost))
  (loop while (not (= 0 (length queue))) do
      (let ((tile (pop queue)))
        (let ((unvisited (unvisited-neighbor-tiles grid tile tile-cost)))
          (dolist (new-tile unvisited)
            (setq tile-cost (add-tile-cost tile-cost new-tile (1+ (cost-of-tile tile tile-cost))))
            (setq queue (enqueue queue new-tile))
          )
        )
      )
    )
  (return-from cost-map tile-cost)
)

(defun cost-of-tile (tile tile-cost)
  "returns cost of tile from tile-cost alist"
  (cdr (alist-get tile tile-cost)))
  
(defun alist-get (key alist)
  "returns entry from alist using equal for comparison"
  (assoc key alist :test #'equal))
      
(defun add-tile-cost (alist tile cost)
  "returns alist with new entry"
  (acons tile cost alist))
  ;(cons (cons tile cost) alist))

(defun enqueue (queue elem)
  "adds item to the back of queue"
  (append queue (list elem)))

;; tile operations

(defun unvisited-neighbor-tiles (grid tile tile-cost)
  (remove-if #'(lambda (til) (alist-get til tile-cost)) (unoccupied-neighbor-tiles grid tile)))
  ; debug (dolist (x (unoccupied-neighbor-tiles grid tile)) (print x) (print (assoc x tile-cost))))

(defun empty-tile (grid tile)
  (not (or (contains-wall grid tile)
            (contains-evil-agent grid tile)
       )
   ))

(defun wall-p ((obj percept-object))
  (if (equal (percept-object-name obj) "#") 
      obj nil))

(defun evil-agent-p ((obj percept-object))
  (if (equal (percept-object-name obj) "WT")
      obj nil))

(defun contains-wall (grid tile)
  (if (identify-in-list #'wall-p (apply #'aref grid tile)) t nil))

(defun contains-evil-agent (grid tile)
  (if (identify-in-list #'evil-agent-p (apply #'aref grid tile)) t nil))

(defun unoccupied-neighbor-tiles (grid tile)
  (remove-if-not #'(lambda (til) (empty-tile grid til)) (neighbor-tiles tile)))

(defun neighbor-tiles (tile)
  (list (top-of tile) (right-of tile) (bottom-of tile) (left-of tile)))
  
(defun bottom-of (tile)
  (cons (1- (car tile)) (cdr tile)))

(defun top-of (tile)
  (cons (1+ (car tile)) (cdr tile)))  

(defun left-of (tile)
  (cons (car tile) (list (1- (cadr tile)))))

(defun right-of (tile)
  (cons (car tile) (list (1+ (cadr tile)))))

