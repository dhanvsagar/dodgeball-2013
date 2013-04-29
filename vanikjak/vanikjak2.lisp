(defconstant vanikjak-agent-name "JV")

(defstructure (vanikjak-agent-body (:include db-agent-body (name vanikjak-agent-name) (sname vanikjak-agent-name))))

(defstructure (vanikjak-agent (:include db-agent (program 'vanikjak-program) (body (make-vanikjak-agent-body)) (name vanikjak-agent-name))))

(defun vanikjak-program (percept)
	(let*
		(
			(body (car percept))
			(field (cadr percept))
			(self (vanikjak-find-self field))
			(ball (vanikjak-find-ball field))
		)
		(if
			self
			(if
				(object-contents body)
				(vanikjak-program-i-have-ball body field self ball)
				(if
					ball
					(vanikjak-program-ball-in-field body field self ball)
					(vanikjak-program-he-has-ball body field self ball)
				)
			)
			'stay
		)
	)
)

(defun vanikjak-program-i-have-ball (body field self ball)
	(let*
		(
			(players (vanikjak-find-players field))
			(nearest (vanikjak-select-nearest players self nil 100))
			(minimum (vanikjak-get-distance nearest self))
		)
		(if
			(equal minimum 1)
			(list 'throw-ball (car nearest) (cdr nearest))
			(let
				(
					(target (vanikjak-select-target players self field))
				)
				(list 'throw-ball (car target) (cdr target))
			)
		)
	)
)

(defun vanikjak-program-ball-in-field (body field self ball)
	(if
		(equal self ball)
		'grab-ball
		(let*
			(
				(distance (vanikjak-get-distance self ball))
				(players (vanikjak-find-players field))
				(nearest (vanikjak-select-nearest players ball nil 100))
				(minimum (vanikjak-get-distance nearest ball))
			)
			(if
				(or
					(equal distance 1)
					(<= distance minimum)
				)
				(vanikjak-bfs-route self ball field)
				(vanikjak-run-away self ball field)
			)
		)
	)
)

(defun vanikjak-program-he-has-ball (body field self ball)
	'grab-ball
)

(defun vanikjak-find-self (field)
	(let
		(
			(selfs (vanikjak-find-by-name field vanikjak-agent-name))
		)
		(if
			selfs
			(car selfs)
			nil
		)
	)
)

(defun vanikjak-find-ball (field)
	(let
		(
			(balls (vanikjak-find-by-name field "B"))
		)
		(if
			balls
			(car balls)
			nil
		)
	)
)

(defun vanikjak-find-by-name (field name)
	(let
		(
			(cols (car (array-dimensions field)))
			(rows (cadr (array-dimensions field)))
			(places nil)
		)
		(dotimes
			(y rows)
			(dotimes
				(x cols)
				(dolist
					(item (aref field x y))
					(if 
						(equal (percept-object-name item) name)
						(setf places (cons (cons x y) places))
					)
				)
			)
		)
		places
	)
)

(defun vanikjak-find-players (field)
	(let
		(
			(cols (car (array-dimensions field)))
			(rows (cadr (array-dimensions field)))
			(places nil)
		)
		(dotimes
			(y rows)
			(dotimes
				(x cols)
				(dolist
					(item (aref field x y))
					(if
						item
						(if
							(and
								(not (equal (percept-object-name item) "#"))
								(not (equal (percept-object-name item) "B"))
								(not (equal (percept-object-name item) vanikjak-agent-name))
							)
							(setf places (cons (cons x y) places))
						)
					)
				)
			)
		)
		places
	)
)

(defun vanikjak-select-nearest (places center nearest minimum)
	(if
		(null places)
		nearest
		(let*
			(
				(place (car places))
				(distance (+ (abs (- (car place) (car center))) (abs (- (cdr place) (cdr center)))))
			)
			(if
				(<= distance minimum)
				(vanikjak-select-nearest (cdr places) center place distance)
				(vanikjak-select-nearest (cdr places) center nearest minimum)
			)
		)
	)
)

(defun vanikjak-get-distance (point center)
	(let
		(
			(x1 (car point))
			(y1 (cdr point))
			(x2 (car center))
			(y2 (cdr center))
		)
		(+ (abs (- x1 x2)) (abs (- y1 y2)))
	)
)

(defun vanikjak-select-target (players self field)
	(dolist
		(player players)
		(if
			(vanikjak-optimal-distance player self)
			(let
				(
					(direction (vanikjak-select-direction player self field))
				)
				(if
					direction
					(return-from vanikjak-select-target direction)
				)
			)
		)
	)
	(vanikjak-select-farthest self players field)
)

(defun vanikjak-select-direction (player self field)
	(let
		(
			(cols (car (array-dimensions field)))
			(rows (cadr (array-dimensions field)))
			(farthest nil)
			(maximum 0)
		)
		(dotimes
			(y rows)
			(dotimes
				(x cols)
				(if
					(and
						(>= x 1)
						(<= x (- cols 2))
						(>= y 0)
						(<= y (- rows 2))
					)
					(dolist 
						(place (go-through-dist-list (list (car self) (cdr self)) (list x y)))
						(if
							(and
								(equal (car (car place)) (car player))
								(equal (cadr (car place)) (cdr player))
							)
							(let*
								(
									(target (cons x y))
									(distance (vanikjak-get-distance target self))
								)
								(if
									(> distance maximum)
									(progn
										(setf farthest target)
										(setf maximum distance)
										(return)
									)
								)
							)
							(if
								(aref field x y)
								(return)
							)
						)
					)
				)
			)
		)
		farthest
	)
)

(defun vanikjak-select-farthest (self players field)
	(let
		(
			(cols (car (array-dimensions field)))
			(rows (cadr (array-dimensions field)))
			(farthest-1 nil)
			(maximum-1 0)
			(farthest-2 nil)
			(maximum-2 0)
		)
		(dotimes
			(y rows)
			(dotimes
				(x cols)
				(if
					(and
						(>= x 1)
						(<= x (- cols 2))
						(>= y 1)
						(<= y (- rows 2))
					)
					(dolist 
						(place (go-through-dist-list (list (car self) (cdr self)) (list x y)))
						(let*
							(
								(target (cons x y))
								(distance (vanikjak-get-distance self target))
								(nearest (vanikjak-select-nearest players target nil 100))
								(minimum (vanikjak-get-distance nearest target))
							)
							(if
								(< distance minimum)
								(if
									(> distance maximum-1)
									(progn
										(setf farthest-1 target)
										(setf maximum-1 distance)
									)
								)
							)
							(if
								(<= distance minimum)
								(if
									(> distance maximum-2)
									(progn
										(setf farthest-2 target)
										(setf maximum-2 distance)
									)
								)
							)
						)
					)
				)
			)
		)
		(if
			farthest-1
			farthest-1
			(if
				farthest-2
				farthest-2
				self
			)
		)
	)
)

(defun vanikjak-run-away (self ball field)
	(let
		(
			(best 'stay)
			(maximum 0)
		)
		(dolist
			(move (vanikjak-gen-moves self))
			(let
				(
					(place (car move))
					(action (cdr move))
				)
				(if
					(and
						(vanikjak-is-place-free place field)
						(not (vanikjak-optimal-distance place ball))
					)
					(let
						(
							(distance (expt (+ (expt (- (car place) (car ball)) 2) (expt (- (cdr place) (cdr ball)) 2)) 0.5))
						)
						(if
							(>= distance 5)
							(progn
								(setf best action)
								(setf maximum 100)
							)
							(if
								(>= distance maximum)
								(progn
									(setf best action)
									(setf maximum distance)
								)
							)
						)
					)
				)
			)
		)
		best
	)
)

(defun vanikjak-gen-moves (self)
	(let
		(
			(x (car self))
			(y (cdr self))
		)
		(list
			(cons (cons (- x 1) y) 'go-left)
			(cons (cons (+ x 1) y) 'go-right)
			(cons (cons x (- y 1)) 'go-down)
			(cons (cons x (+ y 1)) 'go-up)
			(cons (cons x y) 'stay)
		)
	)
)

(defun vanikjak-optimal-distance (place center)
	(and
		(< (expt (+ (expt (- (car place) (car center)) 2) (expt (- (cdr place) (cdr center)) 2)) 0.5) 5)
		(or
			(>= (expt (+ (expt (+ (abs (- (car place) (car center))) 1) 2) (expt (- (cdr place) (cdr center)) 2)) 0.5) 5)
			(>= (expt (+ (expt (- (car place) (car center)) 2) (expt (+ (abs (- (cdr place) (cdr center))) 1) 2)) 0.5) 5)
		)
	)
)

(defun vanikjak-bfs-route (start target field)
	(let*
		(
			(paths (cons nil (cons (cons start nil) nil)))
			(route (vanikjak-bfs-step paths target field))
			(action (vanikjak-first-action route))
		)
		action
	)
)

(defun vanikjak-bfs-step (paths target field)
	(let
		(
			(route (vanikjak-finished-route (cdr paths) target))
			(paths (vanikjak-extend-paths paths (cons (car paths) nil) field))
		)
		(if 
			route
			route
			(vanikjak-bfs-step paths target field)
		)
	)
)

(defun vanikjak-finished-route (routes target)
	(if
		(null routes)
		nil
		(if
			(equal (car (car routes)) target)
			(car routes)
			(vanikjak-finished-route (cdr routes) target)
		)
	)
)

(defun vanikjak-extend-paths (prev-paths next-paths field)
	(if
		(null (cdr prev-paths))
		next-paths
		(let
			(
				(prev-paths (cons (car prev-paths) (cdr (cdr prev-paths))))
				(next-paths (vanikjak-extend-path (car (cdr prev-paths)) next-paths field))
			)
			(vanikjak-extend-paths prev-paths next-paths field)
		)
	)
)

(defun vanikjak-extend-path (path paths field)
	(if
		(vanikjak-is-place-fresh (car path) (car paths))
		(let*
			(
				(place (car path))
				(route (cdr path))
				(x (car place))
				(y (cdr place))
			)
			(cons
				(cons place (car paths))
				(if 
					(vanikjak-is-place-free place field)
					(cons
						(cons (cons (- x 1) y) (cons 'go-left route))
						(cons
							(cons (cons (+ x 1) y) (cons 'go-right route))
							(cons
								(cons (cons x (- y 1)) (cons 'go-down route))
								(cons
									(cons (cons x (+ y 1)) (cons 'go-up route))
									(cdr paths)
								)
							)
						)
					)
					(cdr paths)
				)
			)
		)
		paths
	)
)

(defun vanikjak-is-place-fresh (place places)
	(if
		(null places)
		t
		(if
			(equal place (car places))
			nil
			(vanikjak-is-place-fresh place (cdr places))
		)
	)
)

(defun vanikjak-is-place-free (place field)
	(let
		(
			(x (car place))
			(y (cdr place))
		)
		(if
			(null (aref field x y))
			(return-from vanikjak-is-place-free t)
			(dolist
				(item (aref field x y))
				(if 
					(or
						(equal (percept-object-name item) vanikjak-agent-name)
						(equal (percept-object-name item) "B")
					)
					(return-from vanikjak-is-place-free t)
				)
			)
		)
		nil
	)
)

(defun vanikjak-first-action (path)
	(if
		(null (cdr path))
		(car path)
		(vanikjak-first-action (cdr path))
	)
)
