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
    ;(print (kroupvla-agent-body-command-queue me))
    ;(print (kroupvla-agent-body-state me))
    (kroupvla-next-action grid me)))

;; kroupvla-curry function from http://cl-cookbook.sourceforge.net/functions.html
(defun kroupvla-curry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))

;; core agent definition

(defun kroupvla-next-action (grid me)
    (when (= 0 (length (kroupvla-agent-body-command-queue me)))
      (setf (kroupvla-agent-body-state me) (kroupvlanext-state (kroupvla-agent-body-state me)))
      (setf (kroupvla-agent-body-command-queue me) (kroupvla-issue-commands (kroupvla-agent-body-state me) grid))
      ;(print 'issuing-commands)
      ;(print (kroupvla-agent-body-command-queue me)) 
      ;(print (kroupvla-agent-body-state me))
      )
    (pop (kroupvla-agent-body-command-queue me)))

;; agent state machine

(defun kroupvlanext-state (state)
  (cdr (assoc state '((0 . 1) (1 . 2) (2 . 3) (3 . 1))))) 

(defun kroupvla-issue-commands (state grid)
  (funcall (kroupvla-command-functions state) grid))
  
(defun kroupvla-command-functions (state)
  (cdr (assoc state '((1 . kroupvla-state-1-commands) (2 . kroupvla-state-2-commands) (3 . kroupvla-state-3-commands)))))

(defun kroupvla-state-1-commands (grid)
  (kroupvla-go-grab-ball grid))

(defun kroupvla-state-2-commands (grid)
  (append (list (kroupvla-throw-ball-front-of-agent grid)) (kroupvla-go-harass-agent grid)))
 
(defun kroupvla-state-3-commands (grid)
  (append '(grab-ball) (list (kroupvla-throw-ball-neighbor-agent grid)) '(stay) (list (kroupvla-bump-neighbor-agent grid)) (list (kroupvla-bump-neighbor-agent grid)) (list (kroupvla-bump-neighbor-agent grid)) '(stay) ))
  
;; functions to realize agent STM

(defun kroupvla-go-grab-ball (grid)
  (append (kroupvla-walk-to-ball grid (find-student-location grid)) '(grab-ball)))

(defun kroupvla-go-harass-agent (grid)
  (kroupvla-walk-to-agent grid (find-student-location grid)))

(defun kroupvla-throw-ball-front-of-agent (grid)
  `(throw-ball ,@(kroupvla-next-to-closest-agent grid)))

(defun kroupvla-throw-ball-neighbor-agent (grid)
  `(throw-ball ,@(kroupvla-neighbor-agent-tile grid)))

(defun kroupvla-bump-neighbor-agent (grid)
  (kroupvla-direction (find-student-location grid) (kroupvla-neighbor-agent-tile grid)))

(defun kroupvla-neighbor-agent-tile (grid)
  (dolist (tile (kroupvla-neighbor-tiles (find-student-location grid)))
    (when (identify-in-list #'kroupvla-evil-agent-p (aref grid (car tile) (cadr tile)))
      (return-from kroupvla-neighbor-agent-tile tile))))

(defun kroupvla-next-to-closest-agent (grid)
  (car (kroupvla-find-nearest (kroupvla-curry #'kroupvla-evil-agent-in-neighborhood-pred grid) (kroupvla-cost-map-player grid))))

(defun kroupvla-evil-agent-pred (grid til)
  (identify-in-list #'kroupvla-evil-agent-p (aref grid (car tile) (cadr tile))))

(defun kroupvla-evil-agent-in-neighborhood-pred (grid til)
  (dolist (tile (kroupvla-neighbor-tiles til))
    (when (identify-in-list #'kroupvla-evil-agent-p (aref grid (car tile) (cadr tile)))
      (return-from kroupvla-evil-agent-in-neighborhood-pred t))))

(defun kroupvla-find-nearest (predicate cost-map)
  (kroupvla-nearest (kroupvla-find-matching predicate cost-map)))

(defun kroupvla-nearest (alist)
  (reduce #'(lambda (entry-1 entry-2) (if (< (cdr entry-1) (cdr entry-2)) entry-1 entry-2)) alist))
  
(defun kroupvla-find-matching (predicate kroupvla-cost-map)
  (remove-if-not (lambda (entry) (funcall predicate (car entry))) kroupvla-cost-map))

(defmethod kroupvla-evil-agent-p ((obj percept-object))
  (if (equal (percept-object-name obj) "WB")
      obj nil))
  
(defun kroupvla-walk-to-agent (grid self-tile)
  (kroupvla-path-commands (kroupvla-create-path self-tile (kroupvla-next-to-closest-agent grid) (kroupvla-cost-map grid self-tile))))

(defun kroupvla-walk-to-ball (grid self-tile)
  (kroupvla-path-commands (kroupvla-create-path self-tile (find-ball-location grid) (kroupvla-cost-map grid self-tile))))

(defun kroupvla-path-commands (path)
  (loop for (from to) on path while to 
        collect (kroupvla-direction from to)))

(defun kroupvla-direction (tile-from tile-to)
  (kroupvla-delta-to-direction (kroupvla-delta tile-from tile-to)))

(defun kroupvla-delta (tile-from tile-to)
  (list (- (car tile-to) (car tile-from)) (- (cadr tile-to) (cadr tile-from))))

(defun kroupvla-delta-to-direction (tile-delta)
  (cdr (assoc tile-delta '(((1 0) . go-right) ((-1 0) . go-left) ((0 1) . go-up) ((0 -1) . go-down)) :test #'equal)))

(defun kroupvla-create-path (start-tile goal-tile kroupvla-cost-map)
  "if goal-tile is not in kroupvla-cost-map, then it is unreachable"
  (if (kroupvla-alist-get goal-tile kroupvla-cost-map) (reverse (kroupvla-reconstruct-path (list goal-tile) goal-tile kroupvla-cost-map)) nil))

(defun kroupvla-reconstruct-path (path actual-tile kroupvla-cost-map)
  (if (eq (kroupvla-cost-of-tile actual-tile kroupvla-cost-map) 0) (return-from kroupvla-reconstruct-path path)
      (let ((tiles-back (kroupvla-steps-back (1- (kroupvla-cost-of-tile actual-tile kroupvla-cost-map)) kroupvla-cost-map)))
        (dolist (candidate tiles-back)
          (when (kroupvla-neighbors candidate actual-tile) (return-from kroupvla-reconstruct-path (kroupvla-reconstruct-path (append path (list candidate)) candidate kroupvla-cost-map)))
          )
        )
      )
  )

(defun kroupvla-steps-back (cost kroupvla-cost-map)
  (mapcar #'car (remove-if-not (lambda (entry) (equal (cdr entry) cost)) kroupvla-cost-map)))

(defun kroupvla-neighbors (tile-1 tile-2)
  (if (member tile-1 (kroupvla-neighbor-tiles tile-2) :test #'equal) t nil))

(defun kroupvla-cost-map-player (grid)
  "computes cost map for student player"
  (kroupvla-cost-map grid (find-student-location grid)))

(defun kroupvla-cost-map (grid start-tile)
  "computes cost map from start-tile to every reachable tile"
  (setq tile-cost (list))
  (setq queue (list))
  (setq queue (kroupvla-enqueue queue start-tile))
  (setq tile-cost (kroupvla-add-tile-cost tile-cost start-tile 0))
    ;(print (kroupvla-unvisited-neighbor-tiles grid (list 4 5) tile-cost))
  (loop while (not (= 0 (length queue))) do
      (let ((tile (pop queue)))
        (let ((unvisited (kroupvla-unvisited-neighbor-tiles grid tile tile-cost)))
          (dolist (new-tile unvisited)
            (setq tile-cost (kroupvla-add-tile-cost tile-cost new-tile (1+ (kroupvla-cost-of-tile tile tile-cost))))
            (setq queue (kroupvla-enqueue queue new-tile))
          )
        )
      )
    )
  (return-from kroupvla-cost-map tile-cost)
)

(defun kroupvla-cost-of-tile (tile tile-cost)
  "returns cost of tile from tile-cost alist"
  (cdr (kroupvla-alist-get tile tile-cost)))
  
(defun kroupvla-alist-get (key alist)
  "returns entry from alist using equal for comparison"
  (assoc key alist :test #'equal))
      
(defun kroupvla-add-tile-cost (alist tile cost)
  "returns alist with new entry"
  (acons tile cost alist))
  ;(cons (cons tile cost) alist))

(defun kroupvla-enqueue (queue elem)
  "adds item to the back of queue"
  (append queue (list elem)))

;; tile operations

(defun kroupvla-unvisited-neighbor-tiles (grid tile tile-cost)
  (remove-if #'(lambda (til) (kroupvla-alist-get til tile-cost)) (kroupvla-unoccupied-neighbor-tiles grid tile)))
  ; debug (dolist (x (kroupvla-unoccupied-neighbor-tiles grid tile)) (print x) (print (assoc x tile-cost))))

(defun kroupvla-empty-tile (grid tile)
  (not (or (kroupvla-contains-wall grid tile)
            (kroupvla-contains-evil-agent grid tile)
       )
   ))

(defmethod kroupvla-wall-p ((obj percept-object))
  (if (equal (percept-object-name obj) "#") 
      obj nil))

(defmethod kroupvla-evil-agent-p ((obj percept-object))
  (if (equal (percept-object-name obj) "WT")
      obj nil))

(defun kroupvla-contains-wall (grid tile)
  (if (identify-in-list 'kroupvla-wall-p (apply #'aref grid tile)) t nil))

(defun kroupvla-contains-evil-agent (grid tile)
  (if (identify-in-list #'kroupvla-evil-agent-p (apply #'aref grid tile)) t nil))

(defun kroupvla-unoccupied-neighbor-tiles (grid tile)
  (remove-if-not #'(lambda (til) (kroupvla-empty-tile grid til)) (kroupvla-neighbor-tiles tile)))

(defun kroupvla-neighbor-tiles (tile)
  (list (kroupvla-top-of tile) (kroupvla-right-of tile) (kroupvla-bottom-of tile) (kroupvla-left-of tile)))
  
(defun kroupvla-bottom-of (tile)
  (cons (1- (car tile)) (cdr tile)))

(defun kroupvla-top-of (tile)
  (cons (1+ (car tile)) (cdr tile)))  

(defun kroupvla-left-of (tile)
  (cons (car tile) (list (1- (cadr tile)))))

(defun kroupvla-right-of (tile)
  (cons (car tile) (list (1+ (cadr tile)))))

