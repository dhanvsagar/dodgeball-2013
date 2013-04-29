;;; ================================================================
;;; Student's agent definition:
;;; Petr Vales valespe3

(defconstant valespe-db-agent-name "PV")

(defstructure (valespe-db-agent-body (:include db-agent-body (name valespe-db-agent-name))))

(defstructure (valespe-db-agent                
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
          (nearest-agent-loc (find-nearest-agent-location grid my-loc))
          (ball-loc (find-ball-location grid)))
    (cond 
      (holding-ball (throw-held-ball my-loc nearest-agent-loc)) 
      (ball-on-my-loc (grab-ball-on-my-location))
      (T (go-to-ball-loc my-loc ball-loc grid)))))

(defun find-nearest-agent-location (grid my-loc)
  (let* ((agents (find-agents-locations grid)))
    (get-neares-agent-loc my-loc (cdr agents) (car agents))))

(defun find-agents-locations (grid)
  (let (agents)
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
        (dolist (item (aref grid numberx numbery))
          (when (and (percept-object-agent-p (eval item)) 
                      (not (equal (percept-object-name item) valespe-db-agent-name)))
              (setf agents (push (@ numberx numbery) agents)))))
    )
    (return-from find-agents-locations agents)))

(defun get-neares-agent-loc (my-loc agents nearest-agent)
  (if (null agents) 
    nearest-agent
    (let* ((nearest-distance (get-loc-distance my-loc nearest-agent))
            (next-distance (get-loc-distance my-loc (car agents))))
      (cond 
        ((< next-distance nearest-distance) (get-neares-agent-loc my-loc (cdr agents) (car agents)))
        (T (get-neares-agent-loc my-loc (cdr agents) nearest-agent))))))

(defun throw-held-ball (my-loc nearest-agent-loc)
  (cond
    ((do-i-stand-next-to-him my-loc nearest-agent-loc) `(throw-ball ,@nearest-agent-loc))
    (T (throw-ball-next-to-agent my-loc nearest-agent-loc))))

(defun do-i-stand-next-to-him (my-loc nearest-agent-loc)
  (let* ((x (get-x-of-loc my-loc))
          (y (get-y-of-loc my-loc)))
    (or (loc-equal (list (+ x 1) y) nearest-agent-loc)
      (loc-equal (list (- x 1) y) nearest-agent-loc)
      (loc-equal (list x (+ y 1)) nearest-agent-loc)
      (loc-equal (list x (- y 1)) nearest-agent-loc))))

(defun throw-ball-next-to-agent (my-loc nearest-agent-loc) `(throw-ball ,@(get-loc-next-to-agent my-loc nearest-agent-loc)))

(defun get-loc-next-to-agent (my-loc nearest-agent-loc)
  (let* ((my-x (get-x-of-loc my-loc))
          (my-y (get-y-of-loc my-loc))
          (agent-x (get-x-of-loc nearest-agent-loc))
          (agent-y (get-y-of-loc nearest-agent-loc)))
    (cond ((> my-x agent-x) (list (+ agent-x 1) agent-y))
          ((< my-x agent-x) (list (- agent-x 1) agent-y))
          ((> my-y agent-y) (list agent-x (+ agent-y 1)))
          ((< my-y agent-y) (list agent-x (- agent-y 1))))))

(defun grab-ball-on-my-location () 'grab-ball)

(defun go-to-ball-loc (my-loc ball-loc grid)
  (get-action-from-path my-loc (find-path my-loc ball-loc grid)))

(defun find-path (from to grid)
    (solution-actions (A*-search (make-path-finding-problem :initial-state from :goal to :grid grid))))

(defun get-action-from-path (my-loc path)
  (if (null path)
    'stay
  (let ((next-loc (car path)))
    (cond ((> (get-x-of-loc my-loc) (get-x-of-loc next-loc)) 'go-left)
          ((< (get-x-of-loc my-loc) (get-x-of-loc next-loc)) 'go-right)
          ((> (get-y-of-loc my-loc) (get-y-of-loc next-loc)) 'go-down)
          ((< (get-y-of-loc my-loc) (get-y-of-loc next-loc)) 'go-up)
          (T 'stay)))))

(defun is-free-to-go (go-loc ball-loc grid) 
  (cond ((loc-equal go-loc ball-loc) T)
        (T (is-loc-empty go-loc grid))))

(defun is-loc-empty (loc grid) (let* ((loc-ref (aref grid (get-x-of-loc loc) (get-y-of-loc loc))))
  (cond ((eq (get-loc-content loc-ref grid) NIL) T)
        (T NIL))))

(defun get-loc-content (loc grid)
  (dolist (item loc)
    (when (or (percept-object-wall-p (eval item)) (percept-object-agent-p (eval item))) 
      (return-from get-loc-content item)))
  NIL)

(defun loc-equal (loc1 loc2)
  (and (equal (get-x-of-loc loc1) (get-x-of-loc loc2))
    (equal (get-y-of-loc loc1) (get-y-of-loc loc2))))

(defun get-x-of-loc (loc) (car loc))
(defun get-y-of-loc (loc) (cadr loc))
(defun get-loc-distance (loc1 loc2)
  (let* ((loc1-x (get-x-of-loc loc1))
          (loc1-y (get-y-of-loc loc1))
          (loc2-x (get-x-of-loc loc2))
          (loc2-y (get-y-of-loc loc2))
          (x (abs (- loc1-x loc2-x)))
          (y (abs (- loc1-y loc2-y))))
    (+ x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATICKE VYHODNOCOVANI XY SMEROVANI                      ;;
;; NEUMI SE VYHNOUT AGENTUM PO CESTE                        ;;
;; Obchazeni by bylo otrava programovat - je potreba        ;;
;; pamatovat si stav                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (defun go-to-ball-loc (my-loc ball-loc grid)
;   (cadr (path-search0 my-loc ball-loc grid (+ (car (array-dimensions grid)) (cadr (array-dimensions grid))))))

; (defun go-to-ball-loc (my-loc ball-loc grid)
;   (let* ((my-x (get-x-of-loc my-loc))
;           (my-y (get-y-of-loc my-loc))
;           (ball-x (get-x-of-loc ball-loc))
;           (ball-y (get-y-of-loc ball-loc)))
;     (cond ((and (> my-x ball-x) (is-free-to-go (list (- my-x 1) my-y) ball-loc grid) 'go-left))
;           ((and (< my-x ball-x) (is-free-to-go (list (+ my-x 1) my-y) ball-loc grid) 'go-right))
;           ((and (> my-y ball-y) (is-free-to-go (list my-x (- my-y 1)) ball-loc grid) 'go-down))
;           ((and (< my-y ball-y) (is-free-to-go (list my-x (+ my-y 1)) ball-loc grid) 'go-up))
;           (T (go-to-ball-loc my-loc (list (random (car (array-dimensions grid))) (random (cadr (array-dimensions grid)))) grid)))))

; (defun select-next-move (my-loc next-loc)
;   (let* ((my-x (get-x-of-loc my-loc))
;           (my-y (get-y-of-loc my-loc))
;           (next-x (get-x-of-loc next-loc))
;           (next-y (get-y-of-loc next-loc)))
;       (cond ((> my-x next-x) 'go-left)
;           ((< my-x next-x) 'go-right)
;           ((> my-y next-y) 'go-down)
;           ((< my-y next-y) 'go-up))))

; (defun is-free-to-go (go-loc ball-loc grid) 
;   (cond ((loc-equal go-loc ball-loc) T)
;           (T (is-loc-empty go-loc grid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HLOUPE PROHLEDAVANI VEDE KE STACK OVERFLOW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (defun path-search0 (from to grid limit)
;        (let* ((x (get-x-of-loc from))
;               (y (get-y-of-loc from))
;               (left (list (- x 1) y))
;               (right (list (+ x 1) y))
;               (up (list x (+ y 1)))
;               (down (list x (- y 1)))
;               (left-length (car (path-search left to grid (- limit 1))))
;               (right-length (car (path-search right to grid (- limit 1))))
;               (up-length (car (path-search up to grid (- limit 1))))
;               (down-length (car (path-search down to grid (- limit 1))))
;               (best-length (max left-length right-length up-length down-length)))
;         (cond ((loc-equal from to) `(limit 'stay))
;               ((equal limit 0) `(-1 'stay))
;               ((= best-length left-length) `(best-length 'go-left))
;               ((= best-length right-length) `(best-length 'go-right))
;               ((= best-length up-length) `(best-length 'go-down))
;               ((= best-length down-length) `(best-length 'go-up))
;               (T 'stay))))

; (defun path-search (from to grid limit)
;   (if (is-free-to-go from to grid) 
;       (let* ((x (get-x-of-loc from))
;               (y (get-y-of-loc from))
;               (left (list (- x 1) y))
;               (right (list (+ x 1) y))
;               (up (list x (+ y 1)))
;               (down (list x (- y 1)))
;               (left-length (car (path-search left to grid (- limit 1))))
;               (right-length (car (path-search right to grid (- limit 1))))
;               (up-length (car (path-search up to grid (- limit 1))))
;               (down-length (car (path-search down to grid (- limit 1))))
;               (best-length (max left-length right-length up-length down-length)))
;         (cond ((loc-equal from to) `(limit 'stay))
;               ((equal limit 0) `(-1 'stay))
;               ((= best-length left-length) `(best-length 'go-left))
;               ((= best-length right-length) `(best-length 'go-right))
;               ((= best-length up-length) `(best-length 'go-down))
;               ((= best-length down-length) `(best-length 'go-up))
;               (T 'stay)))
;       `(-1 'stay)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROHLEDAVANI POMOCI A* SEARCH Z AIMA SEARCH API ;;;
;;; Dekuji Pavel Pokorny za ukazkove pouziti tohoto ;;;
;;; api v lonskem reseni semestralni prace          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstructure (path-finding-problem (:include problem)) (grid)) 

(defmethod successors ((problem path-finding-problem) state)
  (let* ((successors NIL)
        (grid (path-finding-problem-grid problem)))
    (dolist (neighbour (get-successors grid state))
      (setf successors (push (cons neighbour neighbour) successors)))
    successors))

(defun get-successors (grid where)
  (let* ((result)
         (x (xy-x where))
         (y (xy-y where)))
    (when (is-square-free (aref grid (- x 1) y))
      (setf result (push (make-xy :x (- x 1) :y y) result)))
    (when (is-square-free (aref grid (+ x 1) y))
      (setf result (push (make-xy :x (+ x 1) :y y) result)))
    (when (is-square-free (aref grid x (- y 1)))
      (setf result (push (make-xy :x x :y (- y 1)) result)))
    (when (is-square-free (aref grid x (+ y 1)))
      (setf result (push (make-xy :x x :y (+ y 1)) result)))
    result))

(defun is-square-free (square)
  (dolist (item square)
    (when (percept-object-ball-p (eval item)) (return-from is-square-free T)))
  (dolist (item square)
    (when (or (percept-object-wall-p (eval item)) (percept-object-agent-p (eval item)) ) 
      (return-from is-square-free nil)))
  T)

(defmethod goal-test ((problem path-finding-problem) state)
  (declare-ignore state)
  (equalp state (problem-goal problem)))

(defmethod edge-cost ((problem path-finding-problem) node action state)  
  (declare-ignore node action state) 1)

(defmethod h-cost ((problem path-finding-problem) state)
  (xy-distance (problem-goal problem) state))

;;; ==================================================================