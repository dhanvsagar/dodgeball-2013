
(defconstant perutond-agent-name "PER")

(defstructure (perutond-1
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
         (ball-loc (perutond-nearest-ball-loc my-loc grid))            ; get loaction of the nearest ball
         (agent-loc (perutond-nearest-agent-loc my-loc grid)))         ; get loaction of the nearest agent
        
        (cond (holding-ball                                   ; if the ball is in my possesion:
                (if (perutond-staying-next-to-agent my-loc agent-loc)
                    `(throw-ball ,@agent-loc)
                    `(throw-ball ,@(perutond-nearest-neighbouring-loc my-loc agent-loc))
                ))
              ((equal ball-loc my-loc) 'grab-ball)                     ; grab the ball if it is on my location
              ((null ball-loc) 'stay)                                  ; stay if there is no ball to take
              (t (perutond-go-get-ball my-loc ball-loc grid)))         ; otherwise, go get the ball
              
  ))

; find nearest object (generic)
(defun perutond-nearest-X-loc (X-predicate my-loc grid)
  (let ((nearest nil))
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (identify-in-list X-predicate (aref grid numberx numbery))
        (if (null nearest) 
            (setf nearest (list numberx numbery))
            (when (< (perutond-my-distance (list numberx numbery) my-loc) (perutond-my-distance nearest my-loc))
                  (setf nearest (list numberx numbery)))
        ))
      )) nearest
  ))

(defun perutond-nearest-ball-loc (my-loc grid)
  (perutond-nearest-X-loc #'my-ball-p my-loc grid))

(defun perutond-nearest-agent-loc (my-loc grid)
  (perutond-nearest-X-loc #'perutond-my-agent-p my-loc grid))

; is it another agent?
(defmethod perutond-my-agent-p ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) perutond-agent-name)) 
           (not (equal (percept-object-name obj) "B")))
      obj nil))

; is it agent?
(defmethod perutond-my-agent-p-2 ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#"))
           (not (equal (percept-object-name obj) "B")))
      obj nil))

; am I next to the agent?
(defun perutond-staying-next-to-agent (my-loc agent-loc)
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
(defun perutond-nearest-neighbouring-loc (my-loc agent-loc)
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
(defun perutond-my-distance (loc-1 loc-2)
  (let ((vecx (- (first loc-1) (first loc-2)))
        (vecy (- (second loc-1) (second loc-2))))
       (+ (abs vecx) (abs vecy))))

; go get the ball
(defun perutond-go-get-ball (my-loc ball-loc grid &optional (next-loc (perutond-find-path my-loc ball-loc grid)))
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
(defun perutond-find-path (my-loc dest-loc grid &optional (get-step #'perutond-find-path-first-step))
  (if (null dest-loc) nil
    (let ((visited (make-array (array-dimensions grid) :initial-element nil))
          (place-queue (make-empty-queue))
          (my-x (first my-loc))
          (my-y (second my-loc)))

         ;(print (format nil "finding path (dest-loc: ~a)" dest-loc))
         (setf (aref visited my-x my-y) 0)
         (enqueue-at-end place-queue (list (list (first my-loc) (second my-loc) 0)))
         (perutond-find-path-aux dest-loc grid visited place-queue get-step))))

; auxiliary function for the find-path
(defun perutond-find-path-aux (dest-loc grid visited q get-step)
  (if (empty-queue? q) nil                         ; (progn (print "funkce find-path-aux vraci nil - tohle by se nemelo stat") (break) nil)
      (let* ((front (remove-front q))
             (loc-x (car front))
             (loc-y (cadr front))
             (st (+ 1 (caddr front))))
           
           ;(print (format nil "  finding path (front: ~a)" front))
           (perutond-find-path-open (+ loc-x 1) loc-y dest-loc grid visited q st)
           (perutond-find-path-open (- loc-x 1) loc-y dest-loc grid visited q st)
           (perutond-find-path-open loc-x (+ loc-y 1) dest-loc grid visited q st)
           (perutond-find-path-open loc-x (- loc-y 1) dest-loc grid visited q st)
           
           (if (equal (list loc-x loc-y) dest-loc)
             (funcall get-step (first dest-loc) (second dest-loc) visited)
             (perutond-find-path-aux dest-loc grid visited q get-step))
      )))

; auxiliary function for the find-path
(defun perutond-find-path-open (x y dest-loc grid visited q st)
  (when (perutond-valid-loc x y grid)
    (when (or (equal (list x y) dest-loc) (not (perutond-is-obstacle x y grid)))
      (when (null (aref visited x y))
        (progn (enqueue-at-end q (list (list x y st)))
               (setf (aref visited x y) st))
      ))))

; check whether there is an obstacle (it is not considered as obstacle if there is a ball)
(defun perutond-is-obstacle (x y grid)
  (if (perutond-valid-loc x y grid)
    (if (and (not (identify-in-list #'my-ball-p (aref grid x y)))
             (identify-in-list #'perutond-is-obstacle-aux (aref grid x y)))
        t nil) t))

; check whether there is an obstacle
(defun perutond-is-obstacle-2 (x y grid)
  (if (perutond-valid-loc x y grid)
    (if (identify-in-list #'perutond-is-obstacle-aux (aref grid x y))
        t nil) t))

; check whether the object is an obstacle
(defun perutond-is-obstacle-aux (obj)
  (if (equal (percept-object-name obj) "B") nil obj))

; return the first step from the path
(defun perutond-find-path-first-step (x y visited)
  (let ((en (- (aref visited x y) 1)))
       (cond ((= en 0) (list x y))
             ((eql en (perutond-loc-value (- x 1) y visited)) (perutond-find-path-first-step (- x 1) y visited))
             ((eql en (perutond-loc-value (+ x 1) y visited)) (perutond-find-path-first-step (+ x 1) y visited))
             ((eql en (perutond-loc-value x (- y 1) visited)) (perutond-find-path-first-step x (- y 1) visited))
             ((eql en (perutond-loc-value x (+ y 1) visited)) (perutond-find-path-first-step x (+ y 1) visited))
             (t nil))))   ;(progn (print "funkce find-path-first-step vraci nil - tohle by se nemelo stat") (break) nil)))))

; check whether it is a valid location
(defun perutond-valid-loc (x y a)
  (cond ((< x 0) nil)
        ((< y 0) nil)
        ((>= x (first (array-dimensions a))) nil)
        ((>= y (second (array-dimensions a))) nil)
        (t t)))

; get value from the specified location (returns null if the location is invalid)
(defun perutond-loc-value (x y a)
  (if (perutond-valid-loc x y a)
      (aref a x y)
      nil))

;;; ==================================================================
;;; My contest agent (also requires functions from agent perutond-1)


(defconstant perutond-agent-name-2 "PER")

(defstructure (perutond
                (:include db-agent
                  (body (make-perutond-2-body))
                  (program 'perutond-agent-2)
                  (name "perutond")))
              "Ondrej Perutka")

(defstructure (perutond-2-body 
                (:include db-agent-body
                  (name perutond-agent-name-2)
                  (sname perutond-agent-name-2)))
              (puss-dists (make-hash-table))
              (puss-clock (make-hash-table)))

(defun perutond-agent-2 (percept)
  (let* ((me (first percept))                                   ; extracts agent body from percept
         (grid (second percept))                                ; extracts grid from percept
         (my-loc (object-loc me))                               ; get my location
         (agents (perutond-get-agent-locations my-loc grid))    ; get locations of all agents (except me)
         (holding-ball (object-contents me))                    ; check whether I have the ball
         (ball-loc (perutond-nearest-ball-loc my-loc grid))     ; get loaction of the nearest ball
         (agent-loc (car agents))                               ; get loaction of the nearest agent
         (next-loc (perutond-find-path my-loc ball-loc grid))   ; get next loaction to get a ball
         (puss-dists (perutond-2-body-puss-dists me))
         (puss-clock (perutond-2-body-puss-clock me)))
        
        (perutond-update-pussy-clocks puss-dists puss-clock agents grid ball-loc)
        ;(print "pussy-dists content:")
        ;(maphash #'(lambda (key value) (format t "~S: ~S~%" key value)) puss-dists)
        ;(print "pussy-clock content:")
        ;(maphash #'(lambda (key value) (format t "~S: ~S~%" key value)) puss-clock)
        (cond (holding-ball `(throw-ball ,@(perutond-get-throw-dest my-loc agent-loc grid)))                                       ; throw the ball if it is in my possesion
              ((equal ball-loc my-loc) 'grab-ball)                                                                                 ; grab the ball if it is on my location
              ((perutond-can-get-ball my-loc ball-loc agents puss-clock grid next-loc) 
                  (perutond-go-get-ball my-loc ball-loc grid next-loc))                                                            ; go get a ball if it is possible
              (t (perutond-go-to-safe me my-loc ball-loc agents grid)))                                                            ; otherwise, go to a safe location
              
  ))

; get hit probability for the given location
(defun perutond-get-danger (loc agents &optional (md 0))
  (if (null agents) md
    (perutond-get-danger loc (cdr agents) (max (perutond-get-danger-aux loc (car agents)) md))
  ))

; get hit probability for the given location and agent
(defun perutond-get-danger-aux (loc agent-loc)
  (let ((dist (points-dist loc agent-loc)))
       (cond ((< dist *SURE-HIT-DIST*) 100)
             ((< dist *CAN-HIT-DIST*) (- 100 (* dist 10)))
             (t 0))))

(defun perutond-get-ball-danger (loc ball-loc)
  (if (null ball-loc) 0
    (* 2 (perutond-get-danger-aux loc ball-loc))))

(defun perutond-left-loc (loc)
  (list (- (car loc) 1) (cadr loc)))

(defun perutond-right-loc (loc)
  (list (+ (car loc) 1) (cadr loc)))

(defun perutond-up-loc (loc)
  (list (car loc) (+ (cadr loc) 1)))

(defun perutond-down-loc (loc)
  (list (car loc) (- (cadr loc) 1)))

; go to some safer place
(defun perutond-go-to-safe (me my-loc ball-loc agents grid)
  (let* ((ll (perutond-left-loc my-loc))
         (rl (perutond-right-loc my-loc))
         (ul (perutond-up-loc my-loc))
         (dl (perutond-down-loc my-loc))
         (psblts (sort `((nil ,(+ (/ (perutond-get-danger my-loc agents) 10) (perutond-get-ball-danger my-loc ball-loc)) stay)
                  (,(perutond-is-obstacle (car ll) (cadr ll) grid) ,(+ (/ (perutond-get-danger ll agents) 10) (perutond-get-ball-danger ll ball-loc)) go-left)
                  (,(perutond-is-obstacle (car rl) (cadr rl) grid) ,(+ (/ (perutond-get-danger rl agents) 10) (perutond-get-ball-danger rl ball-loc)) go-right)
                  (,(perutond-is-obstacle (car ul) (cadr ul) grid) ,(+ (/ (perutond-get-danger ul agents) 10) (perutond-get-ball-danger ul ball-loc)) go-up)
                  (,(perutond-is-obstacle (car dl) (cadr dl) grid) ,(+ (/ (perutond-get-danger dl agents) 10) (perutond-get-ball-danger dl ball-loc)) go-down))
                  #'(lambda (ps1 ps2) (< (cadr ps1) (cadr ps2))))
                  ))
                  
        (perutond-go-to-safe-aux me psblts)))

; take first possible step
(defun perutond-go-to-safe-aux (me psblts)
  (if (null psblts)
    'stay
    (if (caar psblts)
      (perutond-go-to-safe-aux me (cdr psblts))
      (progn ;(print (format nil "~a going ~a : ~a" me (caddr (car psblts)) (cadr (car psblts)))) 
             (caddr (car psblts)))
    )))

; check if it is possible to get a ball without putting myself in danger
(defun perutond-can-get-ball (my-loc ball-loc agents puss-clock grid &optional (next-loc (perutond-find-path my-loc ball-loc grid)))
  (cond ((null ball-loc) nil)
        ((= (perutond-my-distance my-loc ball-loc) 1) t)
        ((null next-loc) nil)
        ((perutond-can-get-ball-aux my-loc ball-loc agents puss-clock grid) t)
        ((= (perutond-get-danger next-loc agents) 0) t)
        (t nil)))

(defun perutond-can-get-ball-aux (my-loc ball-loc agents puss-clock grid)
  (if (null agents) t
    (if (or (< (perutond-my-distance my-loc ball-loc) (perutond-my-distance (car agents) ball-loc))
            (perutond-agent-is-pussy (identify-in-list #'perutond-my-agent-p-2 (aref grid (first (car agents)) (second (car agents)))) puss-clock))
      (perutond-can-get-ball-aux my-loc ball-loc (cdr agents) puss-clock grid)
      nil)))

; get throw destination
(defun perutond-get-throw-dest (my-loc agent-loc grid)
  (if (> (perutond-get-danger my-loc (list agent-loc)) 0)
      agent-loc
      (perutond-find-path my-loc agent-loc grid #'perutond-find-path-mid-step)))

(defun perutond-find-path-mid-step (x y visited &optional (dn (- (floor (aref visited x y) 2) 1)))
  (let ((en (aref visited x y)))
       (cond ((<= dn 0) (list x y))
             ((= en dn) (list x y))
             ((eql (- en 1) (perutond-loc-value (- x 1) y visited)) (perutond-find-path-mid-step (- x 1) y visited dn))
             ((eql (- en 1) (perutond-loc-value (+ x 1) y visited)) (perutond-find-path-mid-step (+ x 1) y visited dn))
             ((eql (- en 1) (perutond-loc-value x (- y 1) visited)) (perutond-find-path-mid-step x (- y 1) visited dn))
             ((eql (- en 1) (perutond-loc-value x (+ y 1) visited)) (perutond-find-path-mid-step x (+ y 1) visited dn))
             (t (progn (print "funkce find-path-mid-step vraci nil - tohle by se nemelo stat") (break) nil)))))

; return sorted list of agent locations
(defun perutond-get-agent-locations (my-loc grid)
  (let ((result nil))
       (dotimes (numberx (car (array-dimensions grid)))
         (dotimes (numbery (cadr (array-dimensions grid)))
           (when (and (not (equal (list numberx numbery) my-loc))
                      (identify-in-list #'perutond-my-agent-p-2 (aref grid numberx numbery)))
             (setf result (cons (list numberx numbery) result)))
         ))
       (sort result #'(lambda (loc1 loc2) (< (points-dist my-loc loc1) (points-dist my-loc loc2))))))

; check whether the given agent was getting close to the ball in the last step
(defun perutond-agent-is-pussy (agent pussy-clock)
  (let ((pc (gethash (percept-object-name agent) pussy-clock)))
       (cond ((null pc) nil)
             ((> pc 0) t)
             (t nil))))

; increment clock for each agent which was getting far from the ball during the last step
; or zero clock for the agents which were getting close during the last step
(defun perutond-update-pussy-clocks (puss-dists puss-clock agents grid ball-loc)
  (perutond-update-pussy-clocks-aux puss-dists puss-clock (perutond-remove-same-agents grid agents ball-loc) grid ball-loc))

(defun perutond-update-pussy-clocks-aux (puss-dists puss-clock agents grid ball-loc)
  (when (and (not (null agents)) (not (null ball-loc)))
    (let* ((aloc (car agents))
           (agent (identify-in-list #'perutond-my-agent-p-2 (aref grid (first aloc) (second aloc))))
           (agent-name (percept-object-name agent))
           (dist (perutond-my-distance aloc ball-loc))
           (ldist (gethash agent-name puss-dists))
           (lclock (gethash agent-name puss-clock)))
          
          (setf (gethash agent-name puss-dists) dist)
          (if (null ldist)
            (setf (gethash agent-name puss-clock) 0)
            (if (< dist ldist)
              (setf (gethash agent-name puss-clock) 0)
              (setf (gethash agent-name puss-clock) (+ lclock 1))))
          
          (perutond-update-pussy-clocks-aux puss-dists puss-clock (cdr agents) grid ball-loc))))

(defun perutond-remove-same-agents (grid agents ball-loc)
  (perutond-remove-same-agents-aux grid agents ball-loc (make-hash-table)))

(defun perutond-remove-same-agents-aux (grid agents ball-loc agent-table)
  (if (null ball-loc)
    nil
    (if (null agents)
      
      (let ((result nil))
           (maphash #'(lambda (key value)
             (setf result (cons value result))) agent-table)
           result)
           
      (let* ((aloc (car agents))
             (agent (identify-in-list #'perutond-my-agent-p-2 (aref grid (first aloc) (second aloc))))
             (agent-name (percept-object-name agent))
             (dist (perutond-my-distance aloc ball-loc))
             (sloc (gethash agent-name agent-table)))
            
            (if (null sloc)
              (setf (gethash agent-name agent-table) aloc)
              (when (< dist (perutond-my-distance sloc ball-loc))
                (setf (gethash agent-name agent-table) aloc)))
            
            (perutond-remove-same-agents-aux grid (cdr agents) ball-loc agent-table)
      
      ))))

