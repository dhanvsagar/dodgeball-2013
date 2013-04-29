
; constants

(defconstant macalkar-agent-name "MAC")


(defstructure
	(macalkar-body
		(:include db-agent-body
			(name macalkar-agent-name)
			(sname macalkar-agent-name))))

; agents

(defstructure
	(macalkar
		(:include db-agent
			(program 'macalkar)
			(body (make-macalkar-body))
			(name "macalkar"))))

; main

(defun macalkar (percept)
	(if (find-my-position (cadr percept) (car percept)); if is alive
		(let* ((me (car percept))
					(my-name (db-agent-body-name me))
					(grid (cadr percept))
					(my-position (object-loc me))
				
					(ball-on-my-position (ball-on-my-position grid me))
					(me-holding-ball (holding-ball grid me))
					(opponent-closer-to-ball (opponent-closer-to-ball grid me))
				
					; controller
					(action
							(cond
								(me-holding-ball (action-throw-ball grid me))
								(ball-on-my-position (action-grab-ball))
								;(opponent-holding-ball (action-run-or-bounce grid me)) TODO
								(opponent-closer-to-ball (action-run-away-from-ball grid me))
								(t (action-go-to-ball grid me)))))
		
			;(print action)
			;(read-line)
			;(sleep 0.2)
			action)))


; actions

(defun action-stay()
	'stay)

(defun action-throw-ball(grid me)
	(let* ((my-position (object-loc me))
				(opp-position (find-closest-opponent-position grid me (object-loc me)))
				(opp-dist (count-distance opp-position my-position)))
		(cond
			((< opp-dist *CAN-HIT-DIST*)
				(action-throw-ball-at opp-position))
			(t
				(action-throw-ball-at
						(vector-move my-position (count-direction my-position opp-position) (/ opp-dist 3))))
		
		)))

(defun action-throw-ball-at(position)
		`(throw-ball ,@position))
	
(defun action-go-left()
	'go-left)
	
(defun action-go-left-safe(grid my-position)
	(cond
		((not (empty-or-ball grid (- (first my-position) 1) (second my-position))); position not empty
			(action-go-random-safe grid my-position))
		(t (action-go-left))))
	
(defun action-go-right()
	'go-right)
	
(defun action-go-right-safe(grid my-position)
	(cond
		((not (empty-or-ball grid (+ (first my-position) 1) (second my-position))); position not empty
			(action-go-random-safe grid my-position))
		(t (action-go-right))))
	
(defun action-go-up()
	'go-up)
	
(defun action-go-up-safe(grid my-position)
	(cond
		((not (empty-or-ball grid (first my-position) (+ (second my-position) 1))); position not empty
			(action-go-random-safe grid my-position))
		(t (action-go-up))))
	
(defun action-go-down()
	'go-down)
	
(defun action-go-down-safe(grid my-position)
	(cond
		((not (empty-or-ball grid (first my-position) (- (second my-position) 1))); position not empty
			(action-go-random-safe grid my-position))
		(t (action-go-down))))

(defun action-go-random-safe(grid my-position)
	(funcall (nth (random 4) (list
					#'action-go-left-safe
					#'action-go-right-safe
					#'action-go-up-safe
					#'action-go-down-safe))
		grid my-position))

(defun action-go-to-ball(grid me)
	(let ((ball (find-ball-position grid me))
				(my-position (object-loc me))
				(moves (list)))
		(when (< (first ball) (first my-position))
				(push (action-go-left-safe grid my-position) moves))
		(when (> (first ball) (first my-position))
				(push (action-go-right-safe grid my-position) moves))
		(when (< (second ball) (second my-position))
				(push (action-go-down-safe grid my-position) moves))
		(when (> (second ball) (second my-position))
				(push (action-go-up-safe grid my-position) moves))
	(return-from action-go-to-ball (nth (random (length moves)) moves))))

(defun action-run-away-from-ball(grid me)
	(let ((ball (find-ball-position grid me))
				(my-position (object-loc me))
				(moves (list)))
		(when (< (first ball) (first my-position))
				(push (action-go-right-safe grid my-position) moves))
		(when (> (first ball) (first my-position))
				(push (action-go-left-safe grid my-position) moves))
		(when (< (second ball) (second my-position))
				(push (action-go-up-safe grid my-position) moves))
		(when (> (second ball) (second my-position))
				(push (action-go-down-safe grid my-position) moves))
	(return-from action-run-away-from-ball (nth (random (length moves)) moves))))

(defun action-grab-ball()
	'grab-ball)


; misc

(defun get-empty-positions-around(grid p)
	(let ((positions (list
									 (list (first p) (+ (second p) 1)); up
									 (list (+ (first p) 1) (second p)); right
									 (list (first p) (- (second p) 1)); down
									 (list (- (first p) 1) (second p)); left
							)))
			(remove-if (lambda (position)
					(aref grid (first position) (second position)))
				positions)))

(defun filter-closest-position(positions p)
	(let ((closest nil))
		(dolist (position positions)
			(when (or (not closest) (position-is-closer p closest position))
				(setf closest position)))
		closest))

(defun agent-next-to-position(grid me position)
	(or
		(is-opponent grid me (first position) (+ (second position) 1)) ; up
		(is-opponent grid me (+ (first position) 1) (second position)) ; right
		(is-opponent grid me (first position) (- (second position) 1)) ; down
		(is-opponent grid me (- (first position) 1) (second position)) ; left
	))

(defun ball-on-my-position(grid me)
	(member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))

(defun holding-ball(grid me)
	(object-contents me))

(defun find-closest-opponent-position(grid me toposition)
	(let ((opp-position nil))
		(dotimes (numberx (car (array-dimensions grid)))
	    (dotimes (numbery (cadr (array-dimensions grid)))
	    	(dolist (item (aref grid numberx numbery))
	    		(when (funcall #'my-opponent-predicate item me)
	    			(when (or (not opp-position) (position-is-closer toposition opp-position (list numberx numbery)))
	    				(setf opp-position (list numberx numbery)))))))
	  opp-position))

(defun count-direction(p1 p2)
	(list (- (first p2) (first p1)) (- (second p2) (second p1))))

(defun vector-len(v)
	(sqrt (vector-squared-length v)))

(defun vector-squared-length(v)
	(+ (* (first v) (first v)) (* (second v) (second v))))

(defun count-distance(p1 p2)
	(sqrt (count-squared-distance p1 p2)))

(defun count-squared-distance(p1 p2)
	(vector-squared-length (count-direction p1 p2)))

(defun position-is-closer(my-position prev-position new-position)
	(< (count-squared-distance my-position new-position) (count-squared-distance my-position prev-position)))

(defun position-is-closer-or-equal(my-position prev-position new-position)
	(let ((d1 (count-squared-distance my-position new-position))
				(d2 (count-squared-distance my-position prev-position)))
		(or (< d1 d2) (= d1 d2))))

(defun find-X-position (X-predicate grid me)
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (identify-in-list X-predicate (aref grid numberx numbery) me)
        (return-from find-X-position (list numberx numbery))))) nil )

(defun find-ball-position (grid me)
  (find-X-position #'my-ball-predicate grid me))

(defun find-my-position (grid me)
  (find-X-position #'my-myself-predicate grid me))

(defun identify-in-list (predicate list me)
  (dolist (item list)
    (when (funcall predicate item me)
      (return-from identify-in-list item))) nil)

(defun my-myself-predicate (obj me)
  (if (equal (percept-object-name obj) (db-agent-body-name me))
      obj nil))

(defun my-ball-predicate (obj me)
  (if (or
  			(equal (percept-object-name obj) "B")
  			(if (typep obj 'percept-object-agent) (percept-object-agent-has-ball obj) nil))
      obj nil))

(defun my-wall-predicate (obj me)
  (if (equal (percept-object-name obj) "#")
      obj nil))

(defun my-opponent-predicate (obj me)
  (if (and
  			(not (equal (percept-object-name obj) "B"))
  			(not (my-wall-predicate obj me))
  			(not (my-myself-predicate obj me))
  			(percept-object-name obj)); name not nil
  		obj nil))

(defun empty-or-ball(grid x y)
	(let ((items (aref grid x y)))
		(cond
			((not items) t)
			((identify-in-list #'my-ball-predicate items nil) t))))

(defun is-opponent(grid me x y)
	(identify-in-list #'my-opponent-predicate (aref grid x y) me))

(defun opponent-closer-to-ball(grid me)
	(let* ((ball-position (find-ball-position grid me))
				(closest-opponent-position (find-closest-opponent-position grid me ball-position)))
		(position-is-closer ball-position (object-loc me) closest-opponent-position)))

(defun vectors-add(v1 v2)
	(list (+ (first v1) (first v2)) (+ (second v1) (second v2))))

(defun vector-mul(v c)
	(list (* (first v) c) (* (second v) c)))

(defun vector-div(v c)
	(list (/ (first v) c) (/ (second v) c)))

(defun vector-normalize(v)
	(vector-div v (vector-len v)))

(defun floor-number(n)
	(if (< n 0) (ceiling n) (floor n)))

(defun vector-floor(v)
	(list (floor-number (first v)) (floor-number (second v))))

(defun vector-move(pos dir dist)
	(vectors-add pos (vector-floor (vector-mul (vector-normalize dir) dist))))














