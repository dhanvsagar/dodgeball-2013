
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
	(if (macalkar-find-my-position (cadr percept) (car percept)); if is alive
		(let* ((me (car percept))
					(my-name (db-agent-body-name me))
					(grid (cadr percept))
					(my-position (object-loc me))
				
					(macalkar-ball-on-my-position (macalkar-ball-on-my-position grid me))
					(me-macalkar-holding-ball (macalkar-holding-ball grid me))
					(macalkar-opponent-closer-to-ball (macalkar-opponent-closer-to-ball grid me))
				
					; controller
					(action
							(cond
								(me-macalkar-holding-ball (macalkar-action-throw-ball grid me))
								(macalkar-ball-on-my-position (macalkar-action-grab-ball))
								;(opponent-macalkar-holding-ball (action-run-or-bounce grid me)) TODO
								(macalkar-opponent-closer-to-ball (macalkar-action-run-away-from-ball grid me))
								(t (macalkar-action-go-to-ball grid me)))))
		
			;(print action)
			;(read-line)
			;(sleep 0.2)
			action)))


; actions

(defun macalkar-action-stay()
	'stay)

(defun macalkar-action-throw-ball(grid me)
	(let* ((my-position (object-loc me))
				(opp-position (macalkar-find-closes-opponent-position grid me (object-loc me)))
				(opp-dist (macalkar-count-distance opp-position my-position)))
		(cond
			((< opp-dist *CAN-HIT-DIST*)
				(macalkar-action-throw-ball-at opp-position))
			(t
				(macalkar-action-throw-ball-at
						(macalkar-vector-move my-position (macalkar-count-direction my-position opp-position) (/ opp-dist 3))))
		
		)))

(defun macalkar-action-throw-ball-at(position)
		`(throw-ball ,@position))
	
(defun macalkar-action-go-left()
	'go-left)
	
(defun macalkar-action-go-left-safe(grid my-position)
	(cond
		((not (macalkar-empty-or-ball grid (- (first my-position) 1) (second my-position))); position not empty
			(macalkar-go-random-safe grid my-position))
		(t (macalkar-action-go-left))))
	
(defun macalkar-action-go-right()
	'go-right)
	
(defun macalkar-action-go-right-safe(grid my-position)
	(cond
		((not (macalkar-empty-or-ball grid (+ (first my-position) 1) (second my-position))); position not empty
			(macalkar-go-random-safe grid my-position))
		(t (macalkar-action-go-right))))
	
(defun macalkar-action-go-up()
	'go-up)
	
(defun macalkar-action-go-up-safe(grid my-position)
	(cond
		((not (macalkar-empty-or-ball grid (first my-position) (+ (second my-position) 1))); position not empty
			(macalkar-go-random-safe grid my-position))
		(t (macalkar-action-go-up))))
	
(defun macalkar-action-go-down()
	'go-down)
	
(defun macalkar-action-go-down-safe(grid my-position)
	(cond
		((not (macalkar-empty-or-ball grid (first my-position) (- (second my-position) 1))); position not empty
			(macalkar-go-random-safe grid my-position))
		(t (macalkar-action-go-down))))

(defun macalkar-go-random-safe(grid my-position)
	(funcall (nth (random 4) (list
					#'macalkar-action-go-left-safe
					#'macalkar-action-go-right-safe
					#'macalkar-action-go-up-safe
					#'macalkar-action-go-down-safe))
		grid my-position))

(defun macalkar-action-go-to-ball(grid me)
	(let ((ball (macalkar-find-ball-position grid me))
				(my-position (object-loc me))
				(moves (list)))
		(when (< (first ball) (first my-position))
				(push (macalkar-action-go-left-safe grid my-position) moves))
		(when (> (first ball) (first my-position))
				(push (macalkar-action-go-right-safe grid my-position) moves))
		(when (< (second ball) (second my-position))
				(push (macalkar-action-go-down-safe grid my-position) moves))
		(when (> (second ball) (second my-position))
				(push (macalkar-action-go-up-safe grid my-position) moves))
	(return-from macalkar-action-go-to-ball (nth (random (length moves)) moves))))

(defun macalkar-action-run-away-from-ball(grid me)
	(let ((ball (macalkar-find-ball-position grid me))
				(my-position (object-loc me))
				(moves (list)))
		(when (< (first ball) (first my-position))
				(push (macalkar-action-go-right-safe grid my-position) moves))
		(when (> (first ball) (first my-position))
				(push (macalkar-action-go-left-safe grid my-position) moves))
		(when (< (second ball) (second my-position))
				(push (macalkar-action-go-up-safe grid my-position) moves))
		(when (> (second ball) (second my-position))
				(push (macalkar-action-go-down-safe grid my-position) moves))
	(return-from macalkar-action-run-away-from-ball (nth (random (length moves)) moves))))

(defun macalkar-action-grab-ball()
	'grab-ball)


; misc

(defun macalkar-get-empty-positions-around(grid p)
	(let ((positions (list
									 (list (first p) (+ (second p) 1)); up
									 (list (+ (first p) 1) (second p)); right
									 (list (first p) (- (second p) 1)); down
									 (list (- (first p) 1) (second p)); left
							)))
			(remove-if (lambda (position)
					(aref grid (first position) (second position)))
				positions)))

(defun macalkar-filter-closest-position(positions p)
	(let ((closest nil))
		(dolist (position positions)
			(when (or (not closest) (macalkar-position-is-closer p closest position))
				(setf closest position)))
		closest))

(defun macalkar-agent-next-to-position(grid me position)
	(or
		(macalkar-is-opponent grid me (first position) (+ (second position) 1)) ; up
		(macalkar-is-opponent grid me (+ (first position) 1) (second position)) ; right
		(macalkar-is-opponent grid me (first position) (- (second position) 1)) ; down
		(macalkar-is-opponent grid me (- (first position) 1) (second position)) ; left
	))

(defun macalkar-ball-on-my-position(grid me)
	(member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))

(defun macalkar-holding-ball(grid me)
	(object-contents me))

(defun macalkar-find-closes-opponent-position(grid me toposition)
	(let ((opp-position nil))
		(dotimes (numberx (car (array-dimensions grid)))
	    (dotimes (numbery (cadr (array-dimensions grid)))
	    	(dolist (item (aref grid numberx numbery))
	    		(when (funcall #'macalkar-my-opponent-predicate item me)
	    			(when (or (not opp-position) (macalkar-position-is-closer toposition opp-position (list numberx numbery)))
	    				(setf opp-position (list numberx numbery)))))))
	  opp-position))

(defun macalkar-count-direction(p1 p2)
	(list (- (first p2) (first p1)) (- (second p2) (second p1))))

(defun macalkar-vector-len(v)
	(sqrt (macalkar-vector-squared-length v)))

(defun macalkar-vector-squared-length(v)
	(+ (* (first v) (first v)) (* (second v) (second v))))

(defun macalkar-count-distance(p1 p2)
	(sqrt (macalkar-count-squared-distance p1 p2)))

(defun macalkar-count-squared-distance(p1 p2)
	(macalkar-vector-squared-length (macalkar-count-direction p1 p2)))

(defun macalkar-position-is-closer(my-position prev-position new-position)
	(< (macalkar-count-squared-distance my-position new-position) (macalkar-count-squared-distance my-position prev-position)))

(defun macalkar-position-is-closer-or-equal(my-position prev-position new-position)
	(let ((d1 (macalkar-count-squared-distance my-position new-position))
				(d2 (macalkar-count-squared-distance my-position prev-position)))
		(or (< d1 d2) (= d1 d2))))

(defun macalkar-find-x-position (X-predicate grid me)
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (macalkar-identify-in-list X-predicate (aref grid numberx numbery) me)
        (return-from macalkar-find-x-position (list numberx numbery))))) nil )

(defun macalkar-find-ball-position (grid me)
  (macalkar-find-x-position #'macalkar-my-ball-predicate grid me))

(defun macalkar-find-my-position (grid me)
  (macalkar-find-x-position #'macalkar-my-myself-predicate grid me))

(defun macalkar-identify-in-list (predicate list me)
  (dolist (item list)
    (when (funcall predicate item me)
      (return-from macalkar-identify-in-list item))) nil)

(defun macalkar-my-myself-predicate (obj me)
  (if (equal (percept-object-name obj) (db-agent-body-name me))
      obj nil))

(defun macalkar-my-ball-predicate (obj me)
  (if (or
  			(equal (percept-object-name obj) "B")
  			(if (typep obj 'percept-object-agent) (percept-object-agent-has-ball obj) nil))
      obj nil))

(defun macalkar-my-wall-predicate (obj me)
  (if (equal (percept-object-name obj) "#")
      obj nil))

(defun macalkar-my-opponent-predicate (obj me)
  (if (and
  			(not (equal (percept-object-name obj) "B"))
  			(not (macalkar-my-wall-predicate obj me))
  			(not (macalkar-my-myself-predicate obj me))
  			(percept-object-name obj)); name not nil
  		obj nil))

(defun macalkar-empty-or-ball(grid x y)
	(let ((items (aref grid x y)))
		(cond
			((not items) t)
			((macalkar-identify-in-list #'macalkar-my-ball-predicate items nil) t))))

(defun macalkar-is-opponent(grid me x y)
	(macalkar-identify-in-list #'macalkar-my-opponent-predicate (aref grid x y) me))

(defun macalkar-opponent-closer-to-ball(grid me)
	(let* ((ball-position (macalkar-find-ball-position grid me))
				(closest-opponent-position (macalkar-find-closes-opponent-position grid me ball-position)))
		(macalkar-position-is-closer ball-position (object-loc me) closest-opponent-position)))

(defun macalkar-vectors-add(v1 v2)
	(list (+ (first v1) (first v2)) (+ (second v1) (second v2))))

(defun macalkar-vector-mul(v c)
	(list (* (first v) c) (* (second v) c)))

(defun macalkar-vector-div(v c)
	(list (/ (first v) c) (/ (second v) c)))

(defun macalkar-vector-normalize(v)
	(macalkar-vector-div v (macalkar-vector-len v)))

(defun macalkar-floor-number(n)
	(if (< n 0) (ceiling n) (floor n)))

(defun macalkar-vector-floor(v)
	(list (macalkar-floor-number (first v)) (macalkar-floor-number (second v))))

(defun macalkar-vector-move(pos dir dist)
	(macalkar-vectors-add pos (macalkar-vector-floor (macalkar-vector-mul (macalkar-vector-normalize dir) dist))))














