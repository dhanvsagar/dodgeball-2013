(defconstant loblpave-agent-name "PL")
(defstructure (loblpave (:include db-agent
                  (body (make-loblpave-body))
                  (program 'loblpave-program)
                  (name "loblpave")))
  "Your agent for db-world.")

(defstructure (loblpave-body (:include db-agent-body (name loblpave-agent-name))))

(defun loblpave-program (percept)
  (let* (
    (me (first percept))    ; extracts agent body from percept
    (grid (second percept))      ; extracts proper percept
    (pos (object-loc me))
    (ball-pos (loblpave-ball-pos grid))
    (enemy-ret #'(lambda (bfs-item) `(throw-ball ,@(loblpave-tile-in-dir pos (car (third bfs-item))))))
    (ball-ret #'(lambda (bfs-item) (car (third bfs-item))))
    (enemy #'(lambda (tl) (loblpave-is-object tl wait-and-throw-db-agent-name)))
    (ball #'(lambda (tl) (loblpave-is-object tl "B"))))
    (cond ((object-contents me) (loblpave-bfs pos grid enemy enemy-ret)) ; got ball - throw
          ((loblpave-tile-equal pos ball-pos) 'grab-ball)   ; at ball - grab
          (t (loblpave-bfs pos grid ball ball-ret)) ))) ; nothing - go for ball

; returns coordinates in given direction
(defun loblpave-tile-in-dir (pos dir)
    (let ((x (first pos)) (y (second pos)))
        (cond ((equal 'go-up dir) (list x (+ y 1)))
              ((equal 'go-down dir) (list x (- y 1)))
              ((equal 'go-right dir) (list (+ x 1) y))
              ((equal 'go-left dir) (list (- x 1) y)) )))

; returns location of the ball in the grid
(defun loblpave-ball-pos (grid)
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
        (dolist (item (aref grid numberx numbery))
            (when (equal (percept-object-name item) "B")
                (return-from loblpave-ball-pos (list numberx numbery)))))) nil)

; check if the tile contains object with given name
(defun loblpave-is-object (tile name)
    (dolist (item tile)
        (when (equal (percept-object-name item) name)
            (return-from loblpave-is-object t))) nil)

; used by bfs search to prevent infinite loop
(defun loblpave-bfs-not-visited (tile visited)
    (dolist (vis visited)
        (when (loblpave-tile-equal tile vis)
            (return-from loblpave-bfs-not-visited nil))) t)

; compare coordinates
(defun loblpave-tile-equal (t1 t2)
    (if (and (equal (first t1) (first t2))
             (equal (second t1) (second t2))) t nil))

; append neighbouring tiles to the bfs queue
(defun loblpave-tile-expand (tile grid queue visited)
    (let ((x (first tile)) (y (second tile)) (path (third tile)))
    (dolist (tl (list
        (list (- x 1) y (append path (list 'go-left)))
        (list (+ x 1) y (append path (list 'go-right)))
        (list x (- y 1) (append path (list 'go-down)))
        (list x (+ y 1) (append path (list 'go-up))) ) )
        (setf neigh (aref grid (first tl) (second tl)))
        (when (and (not (null (loblpave-bfs-not-visited tl visited)))
                 (or (null neigh) (not (loblpave-is-object neigh "#"))))
            (nconc queue (list tl))
            (setf visited (nconc visited (list tl)))
        ))) queue)

; bfs wrapper
(defun loblpave-bfs (start grid identify-target return-target)
    (loblpave-bfs-tail grid (list (list (first start) (second start) ())) () identify-target return-target))

; bfs search
(defun loblpave-bfs-tail (grid queue visited identify-target return-target)
    (let ((tile (car queue)))
        (if (funcall identify-target (aref grid (first tile) (second tile)))
            (return-from loblpave-bfs-tail (funcall return-target tile)))
        (setf visited (nconc visited (list tile)))
        (if (not (loblpave-is-object (aref grid (first tile) (second tile)) wait-and-throw-db-agent-name))
            (loblpave-tile-expand tile grid queue visited))
        (loblpave-bfs-tail grid (cdr queue) visited identify-target return-target)))
