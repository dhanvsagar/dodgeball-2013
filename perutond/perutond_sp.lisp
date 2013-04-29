(defconstant perutond-agent-name "perutond")

(defstructure (perutond
                (:include db-agent
                  (body (make-perutond-1-body))
                  (program 'perutond-agent-1)
                  (name perutond-agent-name)))
              "Ondrej Perutka")

(defstructure (perutond-1-body (:include db-agent-body (name perutond-agent-name))))

(defun perutond-agent-1 (percept)
  (let* ((me (first percept))                                 ; extracts agent body from percept
         (grid (second percept))                              ; extracts grid from percept
         (holding-ball (object-contents me))                  ; check whether I have the ball
         (my-loc (object-loc me))                             ; get my location
         (ball-loc (nearest-ball-loc my-loc grid))            ; get loaction of the nearest ball
         (agent-loc (nearest-agent-loc my-loc grid)))         ; get loaction of the nearest agent
        
        (cond (holding-ball                                   ; if the ball is in my possesion:
                (if (staying-next-to-agent my-loc agent-loc)
                    `(throw-ball ,@agent-loc)
                    `(throw-ball ,@(nearest-neighbouring-loc my-loc agent-loc))
                ))
              ((equal ball-loc my-loc) 'grab-ball)            ; grab the ball if it is on my location
              ((null ball-loc) 'stay)                         ; stay if there is no ball to take
              (t (go-get-ball my-loc ball-loc grid)))         ; otherwise, go get the ball
              
  ))

; find nearest object (generic)
(defun nearest-X-loc (X-predicate my-loc grid)
  (let ((nearest nil))
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (identify-in-list X-predicate (aref grid numberx numbery))
        (if (null nearest) 
            (setf nearest (list numberx numbery))
            (when (< (my-distance (list numberx numbery) my-loc) (my-distance nearest my-loc))
                  (setf nearest (list numberx numbery)))
        ))
      )) nearest
  ))

(defun nearest-ball-loc (my-loc grid)
  (nearest-X-loc #'my-ball-p my-loc grid))

(defun nearest-agent-loc (my-loc grid)
  (nearest-X-loc #'my-agent-p my-loc grid))

; is it another agent?
(defmethod my-agent-p ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) perutond-agent-name)) 
           (not (equal (percept-object-name obj) "B")))
      obj nil))

; is it agent?
; O(1)
(defmethod my-agent-p-2 ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#"))
           (not (equal (percept-object-name obj) "B")))
      obj nil))

; am I next to the agent?
(defun staying-next-to-agent (my-loc agent-loc)
  (let ((x1 (first my-loc))
        (y1 (second my-loc))
        (x2 (first agent-loc))
        (y2 (second agent-loc)))
       
       (cond ((and (equal x2 (- x1 1)) (equal y2 y1)) t)
             ((and (equal x2 (+ x1 1)) (equal y2 y1)) t)
             ((and (equal x2 x1) (equal y2 (- y1 1))) t)
             ((and (equal x2 x1) (equal y2 (+ y1 1))) t)
             (t nil))))

; get nearest location next to the agent
; O(1)
(defun nearest-neighbouring-loc (my-loc agent-loc)
  (let* ((vecx (- (first my-loc) (first agent-loc)))
         (vecy (- (second my-loc) (second agent-loc)))
         (svx (signum vecx))
         (svy (signum vecy)))
        
        (when (equal (abs svx) (abs svy))
           (if (< (abs vecx) (abs vecy))
               (setf svx 0)
               (setf svy 0)))
        (list (+ (first agent-loc) svx) (+ (second agent-loc) svy))))

; get Manhattan distance
(defun my-distance (loc-1 loc-2)
  (let ((vecx (- (first loc-1) (first loc-2)))
        (vecy (- (second loc-1) (second loc-2))))
       (+ (abs vecx) (abs vecy))))

; go get the ball
(defun go-get-ball (my-loc ball-loc grid &optional (next-loc (find-path my-loc ball-loc grid)))
  (if (null next-loc)
    'stay
    (let ((vecx (- (first my-loc) (first next-loc)))
          (vecy (- (second my-loc) (second next-loc))))

          (cond ((> vecx 0) 'go-left)
                ((< vecx 0) 'go-right)
                ((> vecy 0) 'go-down)
                ((< vecy 0) 'go-up)
                (t 'stay)))))

; find the best path to a ball and return first step (BFS)
(defun find-path (my-loc dest-loc grid &optional (get-step #'find-path-first-step))
  (if (null dest-loc) nil
    (let ((visited (make-array (array-dimensions grid) :initial-element nil))
          (place-queue (make-empty-queue))
          (my-x (first my-loc))
          (my-y (second my-loc)))

         ;(print (format nil "finding path (dest-loc: ~a)" dest-loc))
         (setf (aref visited my-x my-y) 0)
         (enqueue-at-end place-queue (list (list (first my-loc) (second my-loc) 0)))
         (find-path-aux dest-loc grid visited place-queue get-step))))

; auxiliary function for the find-path
(defun find-path-aux (dest-loc grid visited q get-step)
  (if (empty-queue? q) nil                         ; (progn (print "funkce find-path-aux vraci nil - tohle by se nemelo stat") (break) nil)
      (let* ((front (remove-front q))
             (loc-x (car front))
             (loc-y (cadr front))
             (st (+ 1 (caddr front))))
           
           ;(print (format nil "  finding path (front: ~a)" front))
           (find-path-open (+ loc-x 1) loc-y dest-loc grid visited q st)
           (find-path-open (- loc-x 1) loc-y dest-loc grid visited q st)
           (find-path-open loc-x (+ loc-y 1) dest-loc grid visited q st)
           (find-path-open loc-x (- loc-y 1) dest-loc grid visited q st)
           
           (if (equal (list loc-x loc-y) dest-loc)
             (funcall get-step (first dest-loc) (second dest-loc) visited)
             (find-path-aux dest-loc grid visited q get-step))
      )))

; auxiliary function for the find-path
(defun find-path-open (x y dest-loc grid visited q st)
  (when (valid-loc x y grid)
    (when (or (equal (list x y) dest-loc) (not (is-obstacle x y grid)))
      (when (null (aref visited x y))
        (progn (enqueue-at-end q (list (list x y st)))
               (setf (aref visited x y) st))
      ))))

; check whether there is an obstacle (it is not considered as obstacle if there is a ball)
(defun is-obstacle (x y grid)
  (if (valid-loc x y grid)
    (if (and (not (identify-in-list #'my-ball-p (aref grid x y)))
             (identify-in-list #'is-obstacle-aux (aref grid x y)))
        t nil) t))

; check whether there is an obstacle
(defun is-obstacle-2 (x y grid)
  (if (valid-loc x y grid)
    (if (identify-in-list #'is-obstacle-aux (aref grid x y))
        t nil) t))

; check whether the object is an obstacle
(defun is-obstacle-aux (obj)
  (if (equal (percept-object-name obj) "B") nil obj))

; return the first step from the path
(defun find-path-first-step (x y visited)
  (let ((en (- (aref visited x y) 1)))
       (cond ((= en 0) (list x y))
             ((eql en (loc-value (- x 1) y visited)) (find-path-first-step (- x 1) y visited))
             ((eql en (loc-value (+ x 1) y visited)) (find-path-first-step (+ x 1) y visited))
             ((eql en (loc-value x (- y 1) visited)) (find-path-first-step x (- y 1) visited))
             ((eql en (loc-value x (+ y 1) visited)) (find-path-first-step x (+ y 1) visited))
             (t nil))))   ;(progn (print "funkce find-path-first-step vraci nil - tohle by se nemelo stat") (break) nil)))))

; check whether it is a valid location
(defun valid-loc (x y a)
  (cond ((< x 0) nil)
        ((< y 0) nil)
        ((>= x (first (array-dimensions a))) nil)
        ((>= y (second (array-dimensions a))) nil)
        (t t)))

; get value from the specified location (returns null if the location is invalid)
(defun loc-value (x y a)
  (if (valid-loc x y a)
      (aref a x y)
      nil))