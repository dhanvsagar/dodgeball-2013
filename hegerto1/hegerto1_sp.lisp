;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   File: hegerto1.lisp
;;;   Author: Tomas Heger (hegerto1@fit.cvut.cz)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My agent definition:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant hegerto1-name "TH")

(defstructure (hegerto1-body (:include db-agent-body (name hegerto1-name))))

(defstructure (hegerto1
               (:include db-agent
                         (program 'hegerto1-program)
                         (body (make-hegerto1-body))
                         (name hegerto1-name)))
  "Hegerto1 agent.") 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My agent program:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hegerto1-program (percept)
  (let* ((me (car percept))
         (grid (cadr percept))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
         (holding-ball (object-contents me))
         (enemies-loc (find-enemies-loc grid))
         (my-loc (object-loc me))
         (ball-loc (find-ball-location grid))
        )
    (cond
      (ball-on-my-loc (return-from hegerto1-program 'grab-ball))
      (holding-ball (return-from hegerto1-program `(throw-ball ,@(find-target-loc my-loc enemies-loc))))
      ((adjacent my-loc ball-loc) (choose-move my-loc (list ball-loc)))
      (t (return-from hegerto1-program (choose-move my-loc (find-path my-loc ball-loc grid))))
    )
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auxiliary functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; predicate true if the given object is an enemy agent
(defmethod enemy-p ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) hegerto1-name)) 
           (not (equal (percept-object-name obj) "B")))
      obj nil
  )
)


;;; returns list of enemy agents' locations as a list of "(x y)" lists
(defun find-enemies-loc (grid)
  (let ((enemies-loc nil))
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
        (if (identify-in-list 'enemy-p (aref grid numberx numbery))
          (push (@ numberx numbery) enemies-loc)
        )
      )
    ) 
    (return-from find-enemies-loc enemies-loc)
  )
)


;;; returns the nearest (to loc) location from the given list of locations
;;; loc-list must not be empty
(defun nearest-loc (loc loc-list)
  (cond ((null (cdr loc-list)) (car loc-list))
        ((< (x+y-distance loc (car loc-list)) (x+y-distance loc (nearest-loc loc (cdr loc-list)))) (car loc-list))
        (t (nearest-loc loc (cdr loc-list)))
  )
)

    
;;; determines location the ball is to be thrown at (when I'm in possesion)
(defun find-target-loc (my-loc enemies-loc)
  (let ((nearest-enemy (nearest-loc my-loc enemies-loc)))
    (if (adjacent my-loc nearest-enemy) (return-from find-target-loc nearest-enemy))
    (let ((possible-targets nil))
      (push (@ (xy-x nearest-enemy) (+ (xy-y nearest-enemy) 1)) possible-targets)
      (push (@ (xy-x nearest-enemy) (- (xy-y nearest-enemy) 1)) possible-targets)
      (push (@ (+ (xy-x nearest-enemy) 1) (xy-y nearest-enemy)) possible-targets)
      (push (@ (- (xy-x nearest-enemy) 1) (xy-y nearest-enemy)) possible-targets)
      (nearest-loc my-loc possible-targets)
    )
  )
)


;;; are the two points (coordinates) adjacent?
(defun adjacent (x y)
  (= (x+y-distance x y) 1)
)


;;; chooses the next move according to path (from current-loc)
(defun choose-move (current-loc path)
  (if (= (- (car current-loc) (car (car path))) 1)
    (return-from choose-move 'go-left))
  (if (= (- (car current-loc) (car (car path))) -1)
    (return-from choose-move 'go-right))
  (if (= (- (cadr current-loc) (cadr (car path))) 1)
    (return-from choose-move 'go-down))
  (if (= (- (cadr current-loc) (cadr (car path))) -1)
    (return-from choose-move 'go-up))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Path finding functions (AIMA used here)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; structures
(defstructure (path-finding-problem (:include problem)) (grid)) 


;;; use A* search from AIMA (additional needed functions follow)
(defun find-path (start target grid)
  (let ((problem (make-path-finding-problem :initial-state start :goal target :grid grid)))
    (solution-actions (A*-search problem))
  )
)


;;; returns a list of possible successors of current location
;;; (needed for AIMA search)
(defmethod successors ((problem path-finding-problem) loc)
  (let ((neighbors nil) (successors-list nil))
    (push (@ (xy-x loc) (+ (xy-y loc) 1)) neighbors)
    (push (@ (xy-x loc) (- (xy-y loc) 1)) neighbors)
    (push (@ (+ (xy-x loc) 1) (xy-y loc)) neighbors)
    (push (@ (- (xy-x loc) 1) (xy-y loc)) neighbors)
    (dolist (n neighbors)
      (if (is-free-to-go n (path-finding-problem-grid problem)) (push (cons n n) successors-list))
    )
    (return-from successors successors-list)
  )  
)


;;; true if loc is free to step on ;;;;;;;;;;;;;;; - nevim jestli funguje, spoleha na to, ze volne pole neobsahuje zed ani nepritele
(defun is-free-to-go (loc grid)
  (let ((place (aref grid (xy-x loc) (xy-y loc))))
    (dolist (content place)
      (if (or (equal (percept-object-name content) "#") (enemy-p content))
        (return-from is-free-to-go nil)
      )
    )
  )
  t
)


