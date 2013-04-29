(defstructure
	(macalkar-body 
		(:include db-agent-body
			(name "P"))) ;macalkar
  (slot1 '(state 'start))  ; any specific extra slots your agent's body would need
;  ...
;  (slotn defaultn))
;    ;
    )

(defstructure
	(macalkar-agent
		(:include db-agent
			(program 'macalkar)
			(body (make-macalkar-body)))))


(defun parse-line (string)
  (if (or (null string) (equal "" string)) nil
    (let ((read (multiple-value-list (read-from-string string))))
      (if (car read) (cons (car read) (parse-line (subseq string (cadr read)))) nil ))))

(defun macalkar (percept)
	(let* ((me (car percept))
				(grid (cadr percept))
				(my-position (find-my-position grid))
				
				(agent-next-to-opponent (agent-next-to-position grid my-position))
				(ball-on-my-position (ball-on-my-position grid me))
				(holding-ball (holding-ball grid me))
		
				(action
						(cond
							(holding-ball (action-throw-ball grid my-position agent-next-to-opponent))
							(ball-on-my-position (action-grab-ball))
							(t (action-go-to-ball grid))))); if ball away or on the opponent position
		
		;(print action)
		;(read-line)
		;(sleep 0.2)
		action))


; returning action

(defun action-throw-ball(grid my-position agent-next-to-opponent)
	(cond
		(agent-next-to-opponent (action-throw-ball-at-opponent grid))
		(t (action-throw-ball-next-to-opponent grid my-position))))

(defun action-throw-ball-at-opponent(grid)
	(let ((position (find-closest-opponent-position grid)))
		`(throw-ball ,@position)))

(defun action-throw-ball-next-to-opponent(grid my-position)
		(let ((position (filter-closest-position (get-empty-positions-around grid (find-closest-opponent-position grid)) my-position)))
			`(throw-ball ,@position)))
	
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

(defun action-go-to-ball(grid)
	(let ((ball (find-ball-position grid))
				(my-position (find-my-position grid))
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
			(when (position-is-closer p closest position)
				(setf closest position)))
		closest))

(defun agent-next-to-position(grid position)
	(or
		(is-opponent grid (first position) (+ (second position) 1)) ; up
		(is-opponent grid (+ (first position) 1) (second position)) ; right
		(is-opponent grid (first position) (- (second position) 1)) ; down
		(is-opponent grid (- (first position) 1) (second position)) ; left
	))

(defun ball-on-my-position(grid me)
	(member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))

(defun holding-ball(grid me)
	(object-contents me))

(defun find-closest-opponent-position(grid)
	(let ((my-position (find-my-position grid))
				(opp-position nil))
		(dotimes (numberx (car (array-dimensions grid)))
	    (dotimes (numbery (cadr (array-dimensions grid)))
	    	(dolist (item (aref grid numberx numbery))
	    		(when (funcall #'my-opponent-p item)
	    			(when (position-is-closer my-position opp-position (list numberx numbery))
	    				(setf opp-position (list numberx numbery)))))))
	   
	  opp-position))

(defun count-direction(p1 p2)
	(list (- (first p2) (first p1)) (- (second p2) (second p1))))

(defun count-squared-distance(p1 p2)
	(let ((dir (count-direction p1 p2)))
		(+ (* (first dir) (first dir)) (* (second dir) (second dir)))))

(defun position-is-closer(my-position prev-position new-position)
	(cond
		((not prev-position)
			t)
		((< (count-squared-distance my-position new-position) (count-squared-distance my-position prev-position))
			t)))

(defun find-X-position (X-predicate grid)
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (identify-in-list X-predicate (aref grid numberx numbery))
        (return-from find-X-position (list numberx numbery))))) nil )

(defun find-ball-position (grid)
  (find-X-position #'my-ball-p grid))

(defun find-my-position (grid)
  (find-X-position #'my-myself-p grid))

(defun identify-in-list (pred list)
  (dolist (item list)
    (when (funcall pred item)
      (return-from identify-in-list item))) nil)

(defmethod my-myself-p ((obj percept-object))
  (if (equal (percept-object-name obj) "P")
      obj nil))

(defmethod my-ball-p ((obj percept-object))
  (if (equal (percept-object-name obj) "B")
      obj nil))

(defmethod my-opponent-p ((obj percept-object))
  (if (equal (percept-object-name obj) wait-and-throw-db-agent-name)
      obj nil))

(defun empty-or-ball(grid x y)
	(let ((items (aref grid x y)))
		(cond
			((not items) t)
			((identify-in-list #'my-ball-p items) t))))

(defun is-opponent(grid x y)
	(identify-in-list #'my-opponent-p (aref grid x y)))







