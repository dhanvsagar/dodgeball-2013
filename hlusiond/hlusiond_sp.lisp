;;; Dodgeball agents
;;; Author: Ondrej Hlusicka (hlusiond@fit.cvut.cz)
;;; Date: 2013

(defconstant hlusiond-name "HO")

;;; Mode 1 Agent
;;;;;;;;;;;;;;;;
(defstructure (hlusiond (:include db-agent
                                             (program 'ho-db-agent-1)
                                             (body (make-hlusiond-body))
                                             (name hlusiond-name)))
              "hlusiond mode 1 agent.")

(defstructure (hlusiond-body (:include db-agent-body 
                                                  (name hlusiond-name))))

;;; Mode 2 Agent
;;;;;;;;;;;;;;;;
(defstructure (hlusiond-db-agent-2 (:include db-agent
                                             (program 'ho-db-agent-2)
                                             (body (make-hlusiond-bd-agent-2-body))
                                             (name hlusiond-name)))
              "hlusiond mode 2 agent.")

(defstructure (hlusiond-bd-agent-2-body (:include db-agent-body 
                                                  (name hlusiond-name))))


(defun ho-db-agent-1 (percept)
       (let* ((me (first percept))
              (loc-me (object-loc me))
              (grid (second percept))
              (holding-ball (object-contents me)))
             (cond (holding-ball (ho-throw-1 me grid)) ; holding the ball, throw it somewhere!
                   ((ho-all-at-loc loc-me grid #'ho-ball?) 'grab-ball) ; just grab the ball...
                   ((ho-someone-has-ball? grid) 'stay) ; an hero
                   ((ho-ball-under-agent? grid) (ho-go-to loc-me (ho-bump-or-stay-loc loc-me grid))) ; bump the agent or stay
                   (T (ho-fetch-ball me grid))))) ; fetch the ball!

(defun ho-db-agent-2 (percept)
       (let* ((me (first percept))
	          (loc-me (object-loc me))
              (grid (second percept))
              (ball-on-my-loc (ho-all-at-loc loc-me grid #'ho-ball?))
              (holding-ball (object-contents me)))
             (setf percept (second percept))
             (cond (holding-ball (ho-do-throw (ho-throw-2 me grid))) ; holding the ball, throw it somewhere!
                   (ball-on-my-loc (ho-ball-on-me me grid)) ; are we threatened?
                   ((ho-someone-has-ball? grid) (ho-try-bump me grid)) ; try bump enemy...
                   ((and (ho-ball-under-agent? grid) (ho-in-wider-neighborhood? me (ho-loc (ho-find-first #'ho-ball? grid)) grid)) (ho-kill me loc-me grid)) ; kill enemy
                   (T (ho-fight-or-flight me grid))))) ; flee, bump, or go fetch the ball!

(defun ho-throw-2 (me grid)
       "Cunningly choose throw destination."
      ; (print "throwing")
       (let* ((agent (ho-closest-enemy me grid))
              (loc-me (object-loc me))
              (loc-agent (ho-loc agent))
              (path (ho-find-path loc-me
                                  (lambda (l)
                                          (equal l loc-agent))
                                  grid
                                  (lambda (l)
                                          (not (ho-all-at-loc l grid #'ho-wall?)))))
              (dist (list-length path))
              (throw-dist (+ 2 (floor (- dist 1) 2)))
              (loc (nth (- throw-dist 1)  path)))
            ; (print (list "throw! path:" path "throw-dist:" throw-dist "loc:" loc "agent" agent "loc-agent:" loc-agent))
             loc))

(defun ho-kill (me loc-me grid)
       "Poke agent until it dies."
       (let* ((loc-agent (ho-loc (ho-find-first #'ho-ball? grid)))
              (path (ho-find-path loc-me
                                  (ho-lemon-curry #'equal loc-agent)
                                  grid
                                  (lambda (l)
                                          (or (equal l loc-agent)
                                              (ho-loc-passable? me l grid)))))
              (dist (list-length path)))
             (if (< dist 3)
                 'stay
                 (ho-go-to loc-me (cadr path)))))
                                          

(defun ho-ball-on-me (me grid)
       "Determine action when ball is on our location."
       (let* ((loc-me (object-loc me)))
             (if (ho-threatened-but-free? me grid)
                 (ho-go-to loc-me (find-if (lambda (loc)
                                                   (ho-loc-passable? me loc grid))
                                           (ho-neighbours-of loc-me))); flee
                 'grab-ball)))

(defun ho-threatened-but-free? (me grid)
       "Are we threatened and if so, can we esacpe?"
       (let* ((loc-me (object-loc me))
              (neighbours (ho-get-wide-neighbours loc-me grid)))
             (if (every #'not
                        (mapcar (lambda (loc)
                                        (ho-all-at-loc loc grid #'ho-agent?))
                                neighbours))
                 nil ; not threatened
                 (some (lambda (loc) ; threatened
                               (ho-loc-passable? me loc grid))
                       (ho-neighbours-of loc-me))))) ; T if there is some passable loc

(defun ho-get-wide-neighbours (loc grid)
       "Gets all locations of dist 2."
       (let ((x (car (array-dimensions grid)))
             (y (cadr (array-dimensions grid))))
            (labels ((in-range (r)
                            (let ((a (first r))
                                  (b (second r)))
                               (and (>= a 0)
                                    (< a x)
                                    (>= b 0)
                                    (< b y)))))
                    (remove-if-not (lambda (l)
                                           (in-range l))
                                   (mapcar (ho-lemon-curry #'ho-offset-loc loc)
                                           '((0 1) (1 1) (1 0) (1 -1) (0 -1) (-1 -1) (-1 0) (-1 1) (0 2) (0 -2) (2 0) (-2 0)))))))

(defun ho-in-wider-neighborhood? (me loc grid)
       "Are we in wider neighborhood?"
       (let* ((loc-me (object-loc me))
              (path (ho-find-path loc-me
                                  (lambda (l)
                                          (ho-all-at-loc l grid #'ho-ball?))
                                  grid
                                  (lambda (l)
                                          (not (ho-all-at-loc l grid #'ho-wall?)))))
              (dist (list-length path)))
             (<= dist 3)))


(defun ho-fight-or-flight (me grid)
       "Determines whether to fetch the ball, bump agent, or flee."
       (let* ((loc-me (object-loc me))
              (loc-ball (ho-loc (ho-find-first #'ho-ball? grid))))
             (if (ho-first-at? me loc-me loc-ball grid)
                 (ho-fetch-ball me grid)
                 (if (ho-first-at-neighborhood? me loc-me loc-ball grid)
                     (ho-fetch-ball me grid)
                     (ho-go-to loc-me (ho-flee-1 me grid loc-ball))))))

(defun ho-first-at? (me loc-me loc grid)
       "Can we be first at loc?"
       (let* ((agents (ho-find-all (lambda (obj)
                                           (ho-enemy? me obj))
                                   grid))
              (my-dist (list-length (ho-find-path loc-me
                                                  (ho-lemon-curry #'equal loc)
                                                  grid
                                                  (lambda (l) (ho-loc-passable? me l grid)))))
              (dists (mapcar (lambda (agent) (list-length (ho-find-path (ho-loc agent)
                                                                        (ho-lemon-curry #'equal loc)
                                                                        grid
                                                                        (lambda (l) (ho-loc-passable? me l grid)))))
                             agents)))
              ; (print (list "first-at? my dist:" my-dist "distst:" dists))
              (every (lambda (dist) (> dist (+ 2 my-dist)))
                     dists)))

(defun ho-first-at-neighborhood? (me loc-me loc grid)
       "Can we cause trouble at ball pickup?"
       (let* ((agents (ho-find-all (lambda (obj)
                                           (ho-enemy? me obj))
                                   grid))
              (my-dist (list-length (ho-find-path loc-me
                                                  (ho-lemon-curry #'equal loc)
                                                  grid
                                                  (lambda (l) (ho-loc-passable? me l grid)))))
              (dists (mapcar (lambda (agent) (list-length (ho-find-path (ho-loc agent)
                                                                        (ho-lemon-curry #'equal loc)
                                                                        grid
                                                                        (lambda (l) (ho-loc-passable? me l grid)))))
                             agents)))
               ;(print (list "first-at-neigh? my dist:" my-dist "distst:" dists))
              (and (every (lambda (dist) (>= dist (- my-dist 2)))
                          dists)
                   (some (lambda (dist) (or (= dist (- my-dist 2))
                                            (= dist (- my-dist 1))))
                         dists))))

(defun ho-closest-enemy (me grid)
       "Finds closest enemy."
       (let* ((loc-me (object-loc me))
              (path (ho-find-path loc-me
                                  (lambda (l)
                                          (ho-all-at-loc l grid (ho-lemon-curry #'ho-enemy? me)))
                                  grid
                                  (lambda (l) (not (ho-all-at-loc l grid #'ho-wall?))))))
         ; (print (list "Closest enemy:" path))
             (first (ho-all-at-loc (car (last  path)) grid #'ho-agent?))))

(defun ho-try-bump (me grid)
       "Tries to bump agent with ball."
       (let* ((loc-me (object-loc me))
              (neighbour (find-if (lambda (loc)
                                          (let* ((objs (mapcar #'ho-obj (ho-all-at-loc loc grid)))
                                                 (agent (find-if #'ho-agent? objs)))
                                                (if agent
                                                    (percept-object-agent-has-ball agent)
                                                    nil)))
                                  (ho-neighbours-of loc-me))))
          ; (print (list "try to bump agent" neighbour))
             (if neighbour
                 (ho-go-to loc-me neighbour)
                 'stay)))

(defun ho-flee-1 (me grid from)
       "Flees from specified location. Returns (x y) of destination"
       (let ((loc-me (object-loc me))) ; my location
            (second (reduce (lambda (loc1 loc2) (if (> (first loc1) (first loc2)) ; reduce: (dist (x y))
                                                    (if (ho-loc-passable? me (second loc1) grid)
                                                        loc1
                                                        loc2)
                                                    (if (ho-loc-passable? me (second loc2) grid)
                                                        loc2 
                                                        loc1))) ; choose only empty and farthest location of the two
                            (mapcar (lambda (loc)
                                            (list (points-dist from loc)
                                                  loc)) ; add distance to loc: (dist (x y))
                                    (cons loc-me (ho-neighbours-of loc-me))))))) ; all 5 locations: (x y)

(defun ho-bump-or-stay-loc (loc grid)
       "Get location to move to if we want to bump agent with ball or stay."
       (let ((agent-loc (find-if (lambda (l)
                                         (let ((objs (mapcar #'ho-obj (ho-all-at-loc l grid))))
 ;  (print (list "objs:" objs "result:" (and (find-if #'ho-ball? objs) (find-if #'ho-agent? objs))))
                                              (and (find-if #'ho-ball? objs)
                                                   (find-if #'ho-agent? objs))))
                                 (ho-neighbours-of loc))))
 ;   (print (list "bump or stay from" loc "agent-loc:" agent-loc))
            (if (null agent-loc) 
                loc
                agent-loc)))

(defun ho-fetch-ball (me grid)
       "Go get the ball."
       (let* ((loc-me (object-loc me))
              (loc-ball (ho-loc (ho-find-first #'ho-ball? grid)))
              (path (ho-find-path loc-me 
                                  (lambda (loc) (equal loc loc-ball))
                                  grid
                                  (lambda (loc) (ho-loc-passable? me loc grid)))))
; (print (list "Path from" loc-me "Path to" loc-ball "MY PATH:" path "Action" (ho-go-to loc-me (car path))))
             (if (null path)
                 'stay
                 (ho-go-to loc-me (cadr path)))))

(defun ho-throw-1 (me grid)
       "If we are ner enemy, hrow the ball. Otherwise throw to an optimal location."
       (let* ((loc-me (object-loc me))
              (agent-neighbour (find-if (lambda (l)
                                                (ho-all-at-loc l grid (ho-lemon-curry #'ho-enemy? me)))
                                        (ho-neighbours-of loc-me))))
 ; (print (list "agent-neighbour:" agent-neighbour))
            (if agent-neighbour
                (ho-do-throw agent-neighbour) ; throw at first neighbouring agent
                (let ((path (ho-find-path loc-me
                                          (lambda (loc)
                                                  (find-if (lambda (l)
 ; (print (list "throw loc check:" loc l (ho-all-at-loc l grid (ho-lemon-curry #'ho-enemy? me))))
                                                                   (ho-all-at-loc l grid (ho-lemon-curry #'ho-enemy? me)))
                                                           (ho-neighbours-of loc))) ; destination is a neighbour of enemy
                                          grid
                                          (lambda (loc) (ho-loc-passable? me loc grid))))) ; find an optimal location to throw at
 ; (print (list "throw path:" path))
                     (if path
                         (ho-do-throw (find-if (ho-lemon-curry #'ho-los? grid loc-me) (reverse path))) ; throw at most distant loc of path that we have los to
                         (ho-do-throw loc-me)))))); no viable target...

(defun ho-do-throw (loc)
       "Produce throw action."
  ; (print (list "throwing at:" loc))
       `(throw-ball ,@loc))

(defun ho-hiding-location? (me agent loc grid)
       "Is this loc a good hiding place?"
       (let ((loc-me (object-loc me))
             (neighbours (ho-neighbours-of loc)))
            (and (not (ho-los? grid loc agent)) ; not in los of agent
                 (ho-loc-passable? me loc grid) ; we can stand there
                 (some (lambda (neighbour)
                               (and (ho-loc-passable? me neighbour grid)
                                    (ho-los? grid neighbour agent)))
                       (ho-neighbours-of loc))))) ; there is neighbour from we can throw


(defun ho-safe-throwing-loc? (me loc grid)
       "Is this loc a good place to throw from?"
       (let ((loc-me (object-loc me))
             (agents (ho-find-all (ho-lemon-curry #'ho-enemy? me) grid))
             (neighbours (ho-neighbours-of loc)))
            (and (ho-los? grid loc-me loc)
                 (some (lambda (enemy-loc)
                               (let ((dist (points-dist loc enemy-loc)))
                                    (and (< dist *CAN-HIT-DIST*)
                                         (some (lambda (neighbour)
                                                       (and (ho-loc-passable? me neighbour grid)
                                                            (> (points-dist neighbour enemy-loc) *CAN-HIT-DIST*)))
                                               neighbours)
                                         (ho-los? grid loc enemy-loc)
                       (mapcar #'ho-loc agents))))))))

(defun ho-go-to (from to)
       "Produce go-to command."
       (second (reduce (lambda (loc1 loc2) (if (< (first loc1) (first loc2))
                                               loc1
                                               loc2))
                       (mapcar (lambda (com) (list (points-dist to (ho-offset-loc from (second com))) (first com))) ; (dist com)
                               '((stay (0 0)) (go-up (0 1)) (go-down (0 -1)) (go-right (1 0)) (go-left (-1 0))))))) ; (com (x y))

(defun ho-los? (grid from to)
       "Is there a line-of-sight between from and to locations?"
       (let ((path (butlast (rest (go-through-dist-list from to)))))
           ; (print (list "Los path:" path)) ; DEBUG
            (every (lambda (loc) 
                           (every (lambda (o) 
                                          (and (not (ho-agent? (ho-obj o)))
                                               (not (ho-wall? (ho-obj o)))))
                                  (ho-all-at-loc (first loc) grid)))
                   path)))

(defun ho-ball-under-agent? (grid)
       "Is ball under some agent?"
       (ho-all-at-loc (ho-loc (ho-find-first #'ho-ball? grid)) grid #'ho-agent?))

(defun ho-neighbours-of (loc)
       "Returns list of 4 neighbours of location."
       (mapcar (ho-lemon-curry #'ho-offset-loc loc) '((0 1) (0 -1) (1 0) (-1 0))))

(defun ho-loc-passable? (me loc grid)
       "Can we stand on this location?"
       (every (lambda (l)
                      (and (not (ho-enemy? me (ho-obj l)))
                           (not (ho-wall? (ho-obj l))))) ; every object must not be enemy or wall
              (ho-all-at-loc loc grid)))
       
(defun ho-enemy? (me other)
       "Is other object my enemy?"
       (if (and (ho-agent? other)
                (not (equal hlusiond-name (percept-object-name other)))) ; better identification???
           T
           nil))

(defun ho-wall? (obj)
       "Is obj a wall?"
       (percept-object-wall-p obj))
 
(defun ho-ball? (obj)
       "Is obj a ball?"
       (percept-object-ball-p obj))

(defun ho-agent? (obj)
       "Is other object an agent?"
       (percept-object-agent-p obj))

(defun ho-agent-has-ball? (agent)
       "Does the agent have the ball?"
       (percept-object-agent-has-ball agent))

(defun ho-all-at-loc (loc grid &optional (pred (lambda (x) T)))
       "Gets list of all objects on specified location."
       (mapcar (lambda (obj) (ho-mkloc loc obj))
               (remove-if-not pred (aref grid (first loc) (second loc)))))
 
(defun ho-find-all (pred grid)
       "Finds all objects satisfying pred."
       (let ((res nil))
            (dotimes (x (car (array-dimensions grid)))
                     (dotimes (y (cadr (array-dimensions grid)))
                              (setf res (append res 
                                                (mapcar (ho-lemon-curry #'ho-mkloc (list x y))
                                                     (remove-if-not pred (aref grid x y)))))))
            res)) ; ((x y) obj)

(defun ho-find-first (pred grid)
       "Finds first occurence matched by predicate."
        (dotimes (x (car (array-dimensions grid)))
                 (dotimes (y (cadr (array-dimensions grid)))
                          (let ((item (find-if pred (aref grid x y))))
                               (when item
                                     (return-from ho-find-first (ho-mkloc (list x y) item)))))) nil) ; ((x y) obj)

(defun ho-someone-has-ball? (grid)
       "Checks if someone holds the ball."
; (print (list "finding ball"))
       (let ((ball (ho-find-first #'ho-ball? grid)))
            (not ball)))

(defun ho-find-ball-posseser (grid)
       "Returns agent holding the ball."       
       (ho-find-first #'ho-agent-has-ball? grid))


(defun ho-offset-loc (loc offset)
       "Offsets loc by (xd, yd)."
       (list (+ (car loc) (car offset))
             (+ (cadr loc) (cadr offset))))

(defun ho-loc (loc)
       (first loc))

(defun ho-obj (loc)
       (second loc))

(defun ho-mkloc (loc obj)
       (list loc obj))

(defun ho-find-path (from to? grid no-obstacle?)
       "Finds shortest path from from (x y) to any point satisfying to? (x y) predicate, avoiding obstacles."
       (let* ((to nil)
              (queue (list from)) ; locations queue, elements are (x y)
              (states (make-array (array-dimensions grid)))) ; graph representation, nil = open; (x y) = closed, with marked redecesor
            ;  (print "begin path search")
             (loop (if (null queue)
                       (return-from ho-find-path (ho-reconstruct-shortest-path from to states (list to))) ; nothing more to do, try to get our path
                       (let ((cur (pop queue))) ; pop first loc
                            (dolist (new (ho-neighbours-of cur)) ; examine each successor
                                    (when (and (null (aref states (first new) (second new))) ; not yet visited
                                               (funcall no-obstacle? new)) ; not an obstacle
                                          (setf (aref states (first new) (second new)) cur) ; set our predecessor
                                          (if (funcall to? new) ; are we there yet?
                                              (progn (setf to new) ; save this position
                                                     (setf queue nil) ; empty the queue
                                                     (return)) ; we are done!
                                              (setf queue (append queue (list new))))))))))) ; add new to queue

(defun ho-reconstruct-shortest-path (from to states &optional (path nil))
       "Return path from graph in states."
       (cond ((null to) nil) ; no path exists
             ((equal from to) path) ; end of path
             (T (let ((new-to (aref states (first to) (second to)))) ; move one step
                     (ho-reconstruct-shortest-path from new-to states (cons new-to path))))))

(defun ho-lemon-curry (f &rest args1)
       "It's..." 
       (lambda (&rest args2) 
               (apply f 
                      (append args1 args2)))) ; lemon curry?!?!


