;; Algorithm description:
;; 1/ Get ball
;; 2/ Agent at distance 1?
;;    a/ Yes - throw at him
;;    b/ No  - pass ball next to agent
;; 3/ While enemy agent grabs ball - bump him (always at distance 1 while hitting agent)
;; 4/ Repeat until all enemy defeated

;; Agent's name
(defconstant nohavja1-db-agent-name "NOH")

(defstructure (nohavja1-agent
                (:include db-agent 
                 (body (make-nohavja1-body))
                 (program 'nohavja1-program)
                 (name nohavja1-db-agent-name))) 
                  "Agent for db-world.")

(defstructure (nohavja1-body (:include db-agent-body (name nohavja1-db-agent-name)  (sname nohavja1-db-agent-name))) (position nil))

(defun nohavja1-program (percept)
  (let ((agent-body (first percept)))    ; Get agent body
    (setf percept (second percept))      ; Get percept
    
    (setf my-position (nohavja1-find-myself percept))
    (if (caddr my-position)                                                                                                                     ; Do i have a ball?
        (progn 
          (setf agent-position (nohavja1-find-agent percept my-position 1))                                                                     ; Find closest agent to me - start from distance 1
          (setf target-field-position (nohavja1-pass-or-attack agent-position my-position))                                                     ; Find way to enemy
          `(throw-ball ,@target-field-position))                                                                                                ; Throw ball at chosen place
        (progn
          (setf ball-position (nohavja1-find-ball percept))                                                                                     ; Find ball in grid
          (nohavja1-find-path percept ball-position my-position agent-body (nohavja1-find-preferred-move percept ball-position my-position) 0)) ; Go for ball
        )))

;;; Find myself in grid

(defun nohavja1-find-myself (grid)
  (dotimes (x (car (array-dimensions grid)))
    (dotimes (y (cadr (array-dimensions grid)))
      (dolist (object (aref grid x y))
        (if (and (percept-object-agent-p object) (equal (percept-object-agent-name object) nohavja1-db-agent-name))
            (return-from nohavja1-find-myself (list x y (percept-object-agent-has-ball object)))))))) ; Return list (X, Y, HasBall?)

;;; Find closest target (agent)

(defun nohavja1-find-agent (grid me dist) 
 (let ((x (car me))
       (y (cadr me)))                                    ;   Check local fields
   (dotimes (i (+ dist 1))                               ;    in this pattern
        (let* ((dir1 (list (+ x i) (+ y (- dist i))))    ;           2
               (dir2 (list (- x i) (+ y (- dist i))))    ;         2 1 2   
               (dir3 (list (+ x i) (- y (- dist i))))    ;       2 1 0 1 2   
               (dir4 (list (- x i) (- y (- dist i))))    ;         2 1 2 
               (directions (list dir1 dir2 dir3 dir4)))  ;           2
        (dolist (dir directions) 
          (if (nohavja1-check-grid-cell grid dir #'percept-object-agent-p) (return-from nohavja1-find-agent (list (car dir) (cadr dir) dist))))))) ; If agent found return list (X, Y, dist)
 (nohavja1-find-agent grid me (+ dist 1))) ; Not found - extend search (distance + 1)

;;; Check cell in grid with function in parameter

(defun nohavja1-check-grid-cell (grid cell function)
  (let ((x (car cell))
        (y (cadr cell))
        (xDim (car (array-dimensions grid)))
        (yDim (cadr (array-dimensions grid))))
    (if (and (>= x 0) (< x xDim) (>= y 0) (< y yDim)) ; Is cell in grid?
        (dolist (object (aref grid x y))
          (if (funcall function object) (return-from nohavja1-check-grid-cell object)))
      nil)))

;;; Choose pass or attack while ball in hand
;;; Agent = X, Y, Distance

(defun nohavja1-pass-or-attack (agent me)
  (let ((x-a (car agent))
        (y-a (cadr agent))
        (x-me (car me))
        (y-me (cadr me)))
  (cond ((= (caddr agent) 1) (subseq agent 0 2))     ; Agent in distance 1 - throw at him
        ((= x-a x-me)                                ; Myself and Agent have same X  
         (cond ((< y-a y-me) (list x-a (+ y-a 1)))   ; Pass ball at agent south
               ((> y-a y-me) (list x-a (- y-a 1))))) ; Pass ball at agent north
        ((< x-a x-me) (list (+ x-a 1) y-a))          ; Pass ball at agent east
        ((> x-a x-me) (list (- x-a 1) y-a)))))       ; Pass ball at agent west

;;; Find ball in grid
;;; Return list(X, Y, Has anyone else ball?)

(defun nohavja1-find-ball (grid)
  (dotimes (x (car (array-dimensions grid)))
    (dotimes (y (cadr (array-dimensions grid)))
      (dolist (object (aref grid x y))
        (if (percept-object-ball-p object) (return-from nohavja1-find-ball (list x y nil)))
        (if (and (percept-object-agent-p object) (percept-object-agent-has-ball object)) (return-from nohavja1-find-ball (list x y T)))))))

;;; Find list of possible moves based on my and ball positions

(defun nohavja1-find-preferred-move (grid ball me)
  (let ((x-me (nth 0 me))
        (y-me (nth 1 me))
        (x-b (nth 0 ball))
        (y-b (nth 1 ball)))
  (cond ((null ball) '(go-right go-up go-left go-down))           ; No ball - go anywhere
        ((and (= x-me x-b) (= y-me y-b)) '(grab-ball))            ; I am at ball location - grab ball
        ((= x-b x-me)                                             ; Same X
         (cond ((< y-b y-me) '(go-down go-left go-right go-up))   ; Ball at south - preferably go down
               ((> y-b y-me) '(go-up go-right go-left go-down)))) ; Ball at north - preferably go up
        ((< x-b x-me)                                             ; Ball at west
         (if (< y-b y-me) '(go-left go-down go-up go-right)       ; Ball at south-west
           '(go-left go-up go-down go-right)))                    ; Ball at north-west
        ((> x-b x-me)                                             ; Ball at east
         (if (< y-b y-me) '(go-right go-down go-up go-left)       ; Ball at south-east
           '(go-right go-up go-down go-left))))))                 ; Ball at north-east

;;; Find path to ball, remove unusable moves based on obstacles

(defun nohavja1-find-path (grid ball me my-instance moves depth)
  (if (= depth 0)
      (progn 
        (setf last-position (nohavja1-body-position my-instance))
        (setf (nohavja1-body-position my-instance) (subseq me 0 2))))
  (cond ((null moves) 'stay)                                                                      ; No possible move - stay
        ((equal (car moves) 'grab-ball) 'grab-ball)                                               ; On ball position - grab it
        (T (cond ((equal (car moves) 'go-left) (setf position (list (- (car me) 1) (cadr me))))   ; Set new position based on the most preferable move
                 ((equal (car moves) 'go-right) (setf position (list (+ (car me) 1) (cadr me))))
                 ((equal (car moves) 'go-up) (setf position (list (car me) (+ (cadr me) 1))))
                 ((equal (car moves) 'go-down) (setf position (list (car me) (- (cadr me) 1)))))
           (setf cell (aref grid (car position) (cadr position)))                                 ; Get preferable position from grid and save it to cell
           (if (> (length cell) 1) 
               (car moves)                                                                        ; Return preferable position
             (cond ((or (percept-object-wall-p (car cell))                                        ; There is already wall
                        (percept-object-agent-p (car cell)))                                      ; or agent
                    (nohavja1-find-path grid ball me my-instance (cdr moves) 1))                  ; Try to use next move                 
                   (T (car moves)))))))                                                           ; Otherwise return preferable positition