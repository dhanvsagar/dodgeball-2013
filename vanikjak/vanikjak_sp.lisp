(defconstant vanikjak-agent-name "JV")

(defstructure (vanikjak-agent-body (:include db-agent-body (name vanikjak-agent-name) (sname vanikjak-agent-name))))

(defstructure (vanikjak (:include db-agent (program 'vanikjak-program) (body (make-vanikjak-agent-body)) (name vanikjak-agent-name))))

(defun vanikjak-program (percept)
	(let*
		(
			(body (car percept))
			(field (cadr percept))
			(self (vanikjak-find-self field))
			(ball (vanikjak-find-ball field))
		)
		(if 
			(object-contents body)
			(let*
				(
					(agent (vanikjak-select-target self field))
					(x (car agent))
					(y (cdr agent))
				)
				(list 'throw-ball x y)
			)
			(if
				(equal self ball)
				'grab-ball
				(vanikjak-bfs-route self ball field)
			)
		)
	)
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

(defun vanikjak-select-target (self field)
	(let*
		(
			(agents (vanikjak-find-by-name field "WT"))
			(agent (vanikjak-select-nearest agents self nil 1))
		)
		(if
			agent
			agent
			(let*
				(
					(places (vanikjak-expand-places agents nil))
					(place (vanikjak-select-nearest places self nil 100))
				)
				place
			)
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

(defun vanikjak-expand-places (prev-places next-places)
	(if
		(null prev-places)
		next-places
		(vanikjak-expand-places (cdr prev-places) (vanikjak-expand-place (car prev-places) next-places))
	)
)

(defun vanikjak-expand-place (place places)
	(let
		(
			(x (car place))
			(y (cdr place))
		)
		(cons
			(cons (- x 1) y)
			(cons
				(cons (+ x 1) y)
				(cons
					(cons x (- y 1))
					(cons
						(cons x (+ y 1))
						places
					)
				)
			)
		)
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
