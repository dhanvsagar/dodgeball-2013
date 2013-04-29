;;; ================================================================
;;; Student's agent definition:

;; This is to be defined when designing a new student agent 
;
(defconstant hanny-agent-name "h4")
(defconstant hanny-agent-fixpoint-detection 2)

(defstructure (strnaj11-agent   ; replace "my-agent" by your unique name, as e.g. FIT username
                (:include db-agent 
                  (body (make-hanny-agent-body))
                  (program 'hanny-agent-program)
                  (name hanny-agent-name)))
  "Jan Strnad")
;
(defstructure (hanny-agent-body 
                (:include db-agent-body
                (name hanny-agent-name)))
  ; expected path
  (expected-path)
  ; last ball location
  (last-ball-location nil)
  ; n. of turns while ball location has not changed
  (turns-since-last-ball-change 0)
  ; was ball i my possesion in the last round?
  ; (ball-was-mine nil)
)

(defun hanny-agent-program (per)
	(let* ((agent-body (first per))
				 (percept (second per))
				 (my-location (object-loc agent-body))
         (ball-location (hanny-agent-find-ball percept))
         (me-holding-ball (object-contents agent-body))
         (expected-path (hanny-agent-body-expected-path agent-body))
         (expected-path-validated (hanny-agent-program-expected-validate expected-path my-location agent-body))
         (enemies-locations (hanny-agent-find-enemies percept))
         ; (enemies-ids (mapcar
         ;                (lambda (x) (hanny-agent-get-at percept x))
         ;                enemies-locations
         ;              )
         ; )
         ; (enemies-lives (mapcar
         ;                  (lambda (x) (percept-object-agent-lives (identify-in-list #'enemy-p x)))
         ;                  enemies-ids
         ;                )
         ; )
         (last-ball-location (hanny-agent-body-last-ball-location agent-body))
         (turns-since-last-ball-change 
          (cond ((equal ball-location last-ball-location)
                 (+ 1 (hanny-agent-body-turns-since-last-ball-change agent-body))
                )
                (T 0)
          )
         )
        )
    (setf (hanny-agent-body-turns-since-last-ball-change agent-body) turns-since-last-ball-change)
    (setf (hanny-agent-body-last-ball-location agent-body) ball-location)
    
    (cond (me-holding-ball 
            (hanny-agent-program-decide-what-with-ball agent-body my-location percept)
          )
          ((equal my-location ball-location) 
            'grab-ball
          )
          ; decide next move
          (ball-location (let ((move (hanny-agent-program-decide-move agent-body 
                                                       my-location 
                                                       ball-location 
                                                       percept
                                                       :forbidden (if (not expected-path-validated) 
                                                           (list (first expected-path)) 
                                                           nil
                                                       )
                                                       :enemies enemies-locations
                                                       :force-get-ball (>= turns-since-last-ball-change hanny-agent-fixpoint-detection)
                                      )
                              ))
                          (if move move 'stay)
                         )
          )
          (T 'stay)
    )
	)
)

(defun hanny-agent-program-decide-what-with-ball (agent my-location percept)
  (let* ((enemies (hanny-agent-find-enemies percept))
         ; direct-hits - list of enemies i can possibly hit, NO distance is taken into account
         (direct-hits (hanny-agent-get-reachable-targets my-location enemies percept))
         (enemies-neighbors 
          (mapcar
            (lambda (x) (first (second (reverse (go-through-dist-list my-location (first x))))))
            direct-hits
          )
         )
         ; reachable-hits - list of those enemies i can actually hit (distace taken into account)
         (reachable-hits
          (remove-if
            (lambda (x) (>= (second x) *CAN-HIT-DIST*))
            direct-hits
          )
         )
         ;sure hit - with 100% probability
         (sure-hits
          (remove-if
            (lambda (x) (>= (second x) *SURE-HIT-DIST*))
            reachable-hits
          )
         )
         ;safe hit - I can hit him with probability 100% and he will also die
         (safe-hits
          (remove-if
            (lambda (x) (> 
                          (percept-object-agent-lives (hanny-agent-get-agent-at percept (first x)))
                          1
                        )
            )
            sure-hits
          )
         )
         ;defensible hits
         (defensible-hits
          (remove-if
            (lambda (x) (> (hanny-agent-manhattan-distance (first x) my-location) 2))
            reachable-hits
          )
         )
         (safe-hits-ordered
          (sort
            safe-hits
            (lambda (x y) (< (second x) (secodn y)))
          )
         )
         (improving-location (hanny-agent-improving-location agent 
                                                             percept 
                                                             my-location 
                                                             (mapcar #'first direct-hits)
                             )
         )
         (improving-location-confirmed (hanny-agent-confirm-improving agent percept my-location improving-location))
         (defensive-improving-location (hanny-agent-improving-location2 agent
                                                                           percept
                                                                           my-location
                                                                           (mapcar #'first direct-hits)
                                          )
         )
        )
    (cond (defensible-hits `(throw-ball ,@(caar defensible-hits)))
          (enemies-neighbors `(throw-ball ,@(first enemies-neighbors)))
          (defensive-improving-location `(throw-ball ,@ defensive-improving-location))
          (safe-hits `(throw-ball ,@(caar safe-hits)))
          (sure-hits `(throw-ball ,@(caar sure-hits)))
          (improving-location-confirmed `(throw-ball ,@(first improving-location)))
          ; (reachable-hits `(throw-ball ,@(caar reachable-hits)))
          ;(targets `(throw-ball ,@(first targets)))
          ; (T `(throw-ball ,@(first (hanny-agent-sort-targets enemies percept my-location))))
          (T `(throw-ball ,@(first (hanny-agent-neighbors-locations-reachable my-location percept))))
    )
  )
)

(defun hanny-agent-program-decide-move (agent my-location ball-location percept &key (forbidden nil) (enemies nil) (force-get-ball nil))
  (multiple-value-bind (nodes path) (hanny-agent-path my-location (list ball-location) percept forbidden ball-location)
    (let* ((my-distance (length path))
           (closer-enemies
            (remove-if
              ; FIX ME
              (complement (lambda (x) (<  (+ 2 (hanny-agent-manhattan-distance x ball-location))
                                          my-distance
                                      )
                          )
              )
              enemies
            )
           )
           (closer-enemies-distance
            (mapcar
              (lambda (x) (hanny-agent-manhattan-distance x ball-location))
              closer-enemies
            )
           )
           (the-closest-enemy-distance (sort closer-enemies-distance #'<))
           (enemies-to-be-caught
            (remove-if
              ; FIX ME
              (complement (lambda (x) (and (<=  my-distance
                                                (+ (hanny-agent-manhattan-distance x ball-location) 2)
                                           )
                                           (>=  (+ 2 my-distance)
                                                (hanny-agent-manhattan-distance x ball-location)
                                           )
                                      )
                          )
              )
              enemies
            )
           )
           (enemies-to-be-caught-distance
            (mapcar
              (lambda (x) (hanny-agent-manhattan-distance x ball-location))
              enemies-to-be-caught
            )
           )
          )
      (cond (force-get-ball
              (setf (hanny-agent-body-expected-path agent) (list (first nodes)))
                    (first path)
            )
            (closer-enemies
              (let* ((my-escape-locations (hanny-agent-find-position
                                            percept
                                            (lambda (i j) (<= (hanny-agent-manhattan-distance 
                                                                my-location
                                                                (list i j)
                                                              )
                                                              (+ (first the-closest-enemy-distance) 1)
                                                          )
                                            )
                                          )
                     )
                    (my-escape-locations-distance
                     (mapcar
                      (lambda (x)
                        (list x 
                              (if (equal ball-location x)
                                  0
                                  (cadar (last (go-through-dist-list ball-location x)))
                              )
                        )
                      )
                      my-escape-locations
                     )
                    )
                    (my-escape-locations-distance-sorted-by-distance-from-center
                      (sort
                        (copy-seq my-escape-locations-distance) 
                        #'< 
                        :key (lambda (x) (hanny-agent-manhattan-distance-to-center (first x)))
                      )
                    )
                    (my-escape-locations-distance-sorted-by-distance-from-ball
                      (stable-sort 
                        my-escape-locations-distance-sorted-by-distance-from-center
                        #'> 
                        :key #'second
                      )
                    )
                    (optimal-escape-distance
                      (hanny-agent-find-optimal-escape my-escape-locations-distance-sorted-by-distance-from-ball 
                                                      (cadar my-escape-locations-distance-sorted-by-distance-from-ball)
                      )
                    )
                    (optimal-escape-locations
                      (remove-if
                        (lambda (x) (> (second x) optimal-escape-distance))
                        my-escape-locations-distance-sorted-by-distance-from-ball
                      )
                    )
                   )

                (multiple-value-bind (nodes path) (hanny-agent-path my-location (list (caar optimal-escape-locations)) percept forbidden)
                  (setf (hanny-agent-body-expected-path agent) (list (first nodes)))
                  (first path)
                )
              )
            )
            ((and enemies-to-be-caught
                  ; (< the-closest-enemy-distance my-distance) 
                  ; (< (apply #'min enemies-to-be-caught-distance) my-distance)
                  (not (hanny-agent-ball-in-possesion percept (first ball-location) (second ball-location)))
                  (equal (first nodes) ball-location)
             )
             'stay
            )
            (T
              (setf (hanny-agent-body-expected-path agent) (list (first nodes)))
              (first path)
            )
      )
    )
  )
)

(defun hanny-agent-get-rid-of-ball (agent my-location enemies grid)
  (let ((possible (sort
                    (set-difference (hanny-agent-neighbors-locations my-location grid)
                                    enemies
                                    :test #'equal
                    )
                    (lambda (x y) (< (hanny-agent-manhattan-distance x my-location)
                                     (hanny-agent-manhattan-distance y my-location) 
                                  )
                    )
                  )
        )
       )
    `(throw-ball ,@(first possible))
  )
)

(defun hanny-agent-get-at (grid location)
  (aref grid (first location) (second location))
)

(defun hanny-agent-get-agent-at (grid location)
  (first (hanny-agent-get-at grid location))
)

(defun hanny-agent-get-reachable-targets (location enemies percept)
  (let* ((go-through (mapcar
                      (lambda (x) (go-through-dist-list location x))
                      enemies
                     )
         )
         (go-through-removed (remove-if
                              (lambda (x) 
                                  (member-if
                                    (lambda (y) (find (first y) enemies :test #'equal))
                                    (butlast x)
                                  )
                              )
                              go-through
                             )
         )
        )
    (mapcar
      (lambda (x) (first (last x)))
      go-through-removed
    )
  )
)

(defun hanny-agent-find-optimal-escape (list current)
  (cond ((null list) current)
        (T 
          (let ((first (cadar list))
               )
            (cond ((> first *CAN-HIT-DIST*)
                    (hanny-agent-find-optimal-escape (rest list) 
                      (if (< first current) first current)
                    )
                  )
                  (T 
                    current
                  )
            )
          )
        )
  )
)

(defun hanny-agent-improving-location (agent grid my-location enemies)
  (let* ((shortest-paths-lens
          (mapcar
            (lambda (x) (hanny-agent-manhattan-distance my-location x))
            enemies
          )
         )
         (can-move-by
          (mapcar
            (lambda (x) (max 0
                             (- (floor (- x 1)
                                       2
                                )
                                2
                             )
                        )
            )
            shortest-paths-lens
          )
         )
         (min-move-by-index (hanny-agent-find-min-index can-move-by))
         (min-move-by (cond (min-move-by-index (nth min-move-by-index can-move-by))
                            (T nil)
                      )
         )
         (closest-enemy (cond (min-move-by-index (nth min-move-by-index enemies))
                              (T nil)
                        )
         )

         ; find all location whre can I be within min-move-by steps
         (bfs-locations (cond (min-move-by
                                (hanny-agent-find-position
                                  grid
                                  (lambda (i j) (<= (hanny-agent-manhattan-distance my-location (list i j))
                                                    min-move-by
                                                )
                                  )
                                )
                              )
                              (T nil)
                        )
         )
         (bfs-locations-distance-to-targets
          (mapcar
            (lambda (location) 
              ; (cadar (last (go-through-dist-list location (nth min-move-by-index enemies))))
              (cons
                location
                (let* ((distances (hanny-agent-get-reachable-targets location enemies grid))
                       (minidx (hanny-agent-find-min-index distances -1 0 0 #'second))
                      )
                  (nth minidx distances )
                )
              )
            )
            bfs-locations
          )
         )
         (bfs-distance-to-target-min-index
          (hanny-agent-find-min-index bfs-locations-distance-to-targets -1 0 0 #'third)
         )
        )
    (cond (bfs-distance-to-target-min-index 
             (let ((location (nth bfs-distance-to-target-min-index bfs-locations-distance-to-targets))
                  )
               (if
                 (equal (first location) my-location)
                 nil
                 (list (first location) closest-enemy)
               )
             )
           )
          (T nil)
    )
  )
)

(defun hanny-agent-confirm-improving (agent grid my-location improving)
  (cond ((null improving) nil)
        (T
          (let* ((improving-location (first improving))
                 (improving-enemy (second improving))
                 (enemy-escape-positions (hanny-agent-find-position
                                           grid
                                           (lambda (i j) (<= (hanny-agent-manhattan-distance 
                                                               improving-enemy
                                                               (list i j)
                                                             )
                                                           (+ 1 (hanny-agent-manhattan-distance my-location improving-location))
                                                         )
                                           )
                                         )
                )
                (enemy-escape-positions-distance
                 (mapcar
                  (lambda (x)
                    (cadar (last (go-through-dist-list improving-location x)))
                  )
                  enemy-escape-positions
                 )
                )
                (current-distance (cadar (last (go-through-dist-list my-location improving-enemy))))
                (maximal-escape-positions-distance (apply #'max enemy-escape-positions-distance))
               )
            (cond ((>= current-distance *CAN-HIT-DIST*) T)
                  (T (< maximal-escape-positions-distance current-distance))
            )
          )
        )
  )
)

(defun hanny-agent-improving-location2 (agent grid my-location enemies)
  (let* ((shortest-paths-lens
          (mapcar
            (lambda (x) (hanny-agent-manhattan-distance my-location x))
            enemies
          )
         )
         (can-move-by
          (mapcar
            (lambda (x) (max 0
                             (- (floor (- x 1)
                                       2
                                )
                                2
                             )
                        )
            )
            shortest-paths-lens
          )
         )
         (min-move-by-index (hanny-agent-find-min-index can-move-by))
         (min-move-by (cond (min-move-by-index (nth min-move-by-index can-move-by))
                            (T nil)
                      )
         )
         (closest-enemy (cond (min-move-by-index (nth min-move-by-index enemies))
                              (T nil)
                        )
         )

         ; find all location whre can I be within min-move-by steps
         (bfs-locations (cond (min-move-by
                                (hanny-agent-find-position
                                  grid
                                  (lambda (i j) (<= (hanny-agent-manhattan-distance my-location (list i j))
                                                    min-move-by
                                                )
                                  )
                                )
                              )
                              (T nil)
                        )
         )
         (bfs-locations-distance-to-targets
          (mapcar
            (lambda (location) 
              ; (cadar (last (go-through-dist-list location (nth min-move-by-index enemies))))
              (cons
                location
                (let* ((distances (hanny-agent-get-reachable-targets location enemies grid))
                       (minidx (hanny-agent-find-min-index distances -1 0 0 #'second))
                      )
                  (nth minidx distances )
                )
              )
            )
            bfs-locations
          )
         )
         (bfs-locations-distance-to-targets-safe
          (remove-if
            (lambda (x) (or (>=  (third x) *CAN-HIT-DIST*)
                            (< (third x) (- *CAN-HIT-DIST* 1))
                        )
            )
            bfs-locations-distance-to-targets
          )
         )
         (bfs-distance-to-target-min-index
          (hanny-agent-find-min-index bfs-locations-distance-to-targets-safe -1 0 0 #'third)
         )
         (am-i-good-enough (find my-location bfs-locations-distance-to-targets-safe :test #'equal :key #'first))
        )
    (cond (am-i-good-enough
            (second am-i-good-enough)
          )
          (bfs-distance-to-target-min-index 
             (let ((location (nth bfs-distance-to-target-min-index bfs-locations-distance-to-targets-safe))
                  )
              (first location)
             )
          )
          (T nil)
    )
  )
)


(defun hanny-agent-find-min-index (list &optional (sofar -1) (minindex 0) (current 0)  (selector #'identity))
  (cond ((and (null list) (= sofar -1)) nil)
        ((null list) minindex)
        ((= sofar -1) (hanny-agent-find-min-index (rest list) (funcall selector (first list)) 0 1 selector))
        ((< (funcall selector (first list)) sofar) (hanny-agent-find-min-index (rest list) (funcall selector (first list)) current (+ 1 current) selector))
        (T (hanny-agent-find-min-index (rest list) sofar minindex (+ 1 current) selector))
  )
)

(defun hanny-agent-program-expected-validate (path location agent)
  (cond ((and path (equal location (first path)))
          (setf (hanny-agent-body-expected-path agent) (rest path))
          T
         )
        ((null path) T)
        (T nil)
  )
)

(defun hanny-agent-sort-targets (enemies percept from)
  (sort
    (reduce #'append
      (mapcar
        (lambda (x) (hanny-agent-neighbors-locations-reachable x percept))
        enemies
      )
      :initial-value nil
    )
    (lambda (x y) 
      (< (hanny-agent-manhattan-distance x from) 
         (hanny-agent-manhattan-distance y from)
      )
    )
  )
)

(defun hanny-agent-neighbors-locations (location percept)
  (let ((width (first (array-dimensions percept)))
        (height (second (array-dimensions percept)))
        (x (first location))
        (y (second location)))
    (remove-if 
      (lambda (tmp) (or (equal (first tmp) 0) (equal (second tmp) 0) 
                      (equal (second tmp) (- width 1))
                      (equal (second tmp) (- height 1))))
      (list (list (- x 1) (- y 1)) (list (- x 1) y) (list (- x 1) (+ y 1))
            (list x (- y 1)) (list x (+ y 1))
            (list (+ x 1) (- y 1)) (list (+ x 1) y) (list (+ x 1) (+ y 1))
      )
    )
  )
)

(defun hanny-agent-neighbors-locations-reachable (location percept)
  (remove-if
    (lambda (x) (> (hanny-agent-manhattan-distance x location) 1))
    (hanny-agent-neighbors-locations location percept)
  )
)

(defun hanny-agent-loc-p (locations percept p)
  (remove-if
    (lambda (x) (funcall p (aref percept (first x) (second x))))
    locations
  )
)

(defun hanny-agent-loc-empty (locations percept)
  (hanny-agent-loc-p locations percept (lambda (x) (not (equal x nil))))
)

(defun hanny-agent-loc-empty-or-ball (locations percept)
  (hanny-agent-loc-p locations percept (lambda (x) (not (or (equal x nil) (hanny-agent-my-ball-p x)))))
)

(defun hanny-agent-loc-nonempty (locations percept)
  (hanny-agent-loc-p locations percept (lambda (x) (equal x nil)))
)

(defun hanny-agent-manhattan-distance (a b)
  (+ (abs (- (first a) (first b))) (abs (- (second a) (second b))))
)

(defun hanny-agent-manhattan-distance-to-center (x)
  (/ 
    (+ (hanny-agent-manhattan-distance x '(4 5))
       (hanny-agent-manhattan-distance x '(4 4))
       (hanny-agent-manhattan-distance x '(5 4))
       (hanny-agent-manhattan-distance x '(5 5))
    )
    4.0
  )
)

(defun hanny-agent-moves (location grid)
  (let ((neighbors (hanny-agent-loc-empty-or-ball (hanny-agent-neighbors-locations location grid) grid)))
    (remove-if
      (lambda (x) (> (hanny-agent-manhattan-distance location x) 1))
      neighbors
    )
  )
)

(defun hanny-agent-move-action (location move)
  (let ((x0 (first location))
        (y0 (second location))
        (x1 (first move))
        (y1 (second move)))
    (cond ((< x1 x0) 'go-left)
          ((> x1 x0) 'go-right)
          ((< y1 y0) 'go-down)
          (T 'go-up)
    )
  )
)

(defun hanny-agent-moves-actions (location moves)
  (mapcar
    (lambda (move) (cons (hanny-agent-move-action location move) move))
    moves
  )
)

(defstructure (hanny-path-problem (:include problem))
  (grid)
  (forbidden nil)
  (allowed nil)
)

(defun hanny-agent-path (from to grid &optional (forbidden nil) (allowed nil))
  (let* ((problem (make-hanny-path-problem :initial-state from :goal to :grid grid :forbidden forbidden :allowed allowed))
         (solution (A*-search problem))
        )
    (values 
      (rest (mapcar #'node-state (solution-nodes solution)))
      (solution-actions solution)
    )
  )
)


(defmethod successors ((problem hanny-path-problem) state)
  (let ((grid (hanny-path-problem-grid problem))
        (forbidden (hanny-path-problem-forbidden problem))
        (allowed (hanny-path-problem-allowed problem))
       )
    (hanny-agent-moves-actions state 
                               (remove-if
                                (lambda (x) (and (member x forbidden :test #'equal)
                                                 (not (equal x allowed))
                                            )
                                )
                                (hanny-agent-moves state grid)
                               )
    )
  )
)

(defmethod goal-test ((problem hanny-path-problem) state)
  (find state (problem-goal problem) :test  #'equal)
)

(defmethod h-cost ((problem hanny-path-problem) state)
  (apply #'min 
    (mapcar
      (lambda (x) (hanny-agent-manhattan-distance x state))
      (problem-goal problem)
    )
  )
)

(defmethod hanny-agent-enemy-p ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) hanny-agent-name)) 
           (not (equal (percept-object-name obj) "B"))
      )
      obj nil
  )
)

; (defmethod hanny-agent-enemy-p (object) nil)

(defun hanny-agent-find-enemies (grid)
  (hanny-agent-find-position
    grid
    (lambda (i j) (identify-in-list #'hanny-agent-enemy-p (aref grid i j)))
  )
)

(defun hanny-agent-my-ball-p (value)
  (or (identify-in-list #'percept-object-ball-p value)
      (if (percept-object-agent-p (first value))
          (percept-object-agent-has-ball (first value))
          nil
      )
  ) 
)

(defun hanny-agent-ball-in-possesion (grid i j)
  (let ((value (aref grid i j)))
    (if (percept-object-agent-p (first value))
        (percept-object-agent-has-ball (first value))
          nil
    )
  )
)

(defun hanny-agent-find-ball (grid)
  (first
    (hanny-agent-find-position
      grid
      (lambda (i j) (hanny-agent-my-ball-p (aref grid i j)))
    )
  )
)

(defun hanny-agent-find-position (grid predicate)
  (let* ((xdim (first (array-dimensions grid)))
         (ydim (second (array-dimensions grid))))
    (labels ((inner (i j acc)
              (cond ((equal (+ 1 j) ydim) acc)
                    ((funcall predicate i j) (cons (list i j) (inner i (+ j 1) acc)))
                    (T (inner i (+ j 1) acc))
              )
             )
             (outer (i acc)
              (cond ((equal (+ 1 i) xdim) acc)
                    (T (outer (+ i 1) (inner i 1 acc)))
              )
             )
            )
      (outer 1 nil)
    )
  )
)
