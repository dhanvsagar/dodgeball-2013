(defstructure (temnymar-agent
	(:include db-agent
		(program 'temnymar-agent-program)
		(body (make-temnymar-agent-body))
		(name "temnymar"))))
	
(defstructure (temnymar-agent-body
	(:include db-agent-body
		(name "TA")
		(sname "temnymar"))))
		
(defun temnymar-agent-program (percept)
	(let ((grid (get-grid percept)) (ball) (myself) (enemies))
		(setf ball (get-ball grid))
		(setf myself (get-myself grid (get-name percept)))
		(setf enemies (get-enemies grid (get-name percept)))
		(cond
			((null myself) 'stay) ; if I am dead
			((has-ball? myself) ; if I have got ball
				(let ((enemy (choose-target myself enemies))) ; find closest enemy
					(if (> (distance-el myself enemy) *SURE-HIT-DIST*) ; if closest enemy is farther than 5 fields
						`(throw-ball ,@(close-ball myself enemy)) ; get ball closer to enemy
						`(throw-ball ,@(get-coord enemy))))) ; throw ball at enemy
			((null ball) (run-from myself (get-ball-enemy enemies) grid)) ; if enemy has ball then move opposite from ball (this should not happen)
			((coord-equal? (get-coord myself) (get-coord ball)) 'grab-ball) ; if I am on same place as ball then grab it
			((= (distance-el myself ball) 1) (approach-ball myself ball grid T)) ; if ball is only one field away then always try to approach it
			((follow-ball? myself ball enemies) (approach-ball myself ball grid)) ; if I am closest to ball then approach it
			(T (run-from myself ball grid))))) ; else run away from ball


(defun get-grid (percept)
	(cadr percept))

(defun get-name (percept)
	(db-agent-body-name (car percept)))

; returns ball with location
(defun get-ball (grid)
	(traverse-2d-array
		grid
		(lambda (el) (is-ball? el))
		(lambda (el x y) (cons (car el) (cons x (cons y nil))))))
	
; returns myself with location	
(defun get-myself (grid name)
	(traverse-2d-array
		grid
		(lambda (el) (is-myself? el name))
		(lambda (el x y) (cons (car el) (cons x (cons y nil))))))

; returns all agents with locations except myself
(defun get-enemies (grid name)
	(traverse-2d-array
		grid
		(defun con (el) (and (is-enemy? el) (not (is-myself? el name))))
		(defun prod (el x y) (cons (cons (car el) (cons x (cons y nil))) (traverse-2d-array grid #'con #'prod (1+ x) y)))))

; traverses elements in 2d array checks condition con and if it is true then applies prod
(defun traverse-2d-array (arr con prod &optional (x 0) (y 0))
	(cond
		((>= y (array-dimension arr 1)) nil) ; if y gets over size of array - end recursion
		((>= x (array-dimension arr 0)) (traverse-2d-array arr con prod 0 (1+ y))) ; if x gets over size of array - get to next row
		(T
			(cond
				((funcall con (aref arr x y)) (funcall prod (aref arr x y) x y)) ; try condition on first element in array[x,y] 
				((and (not (null (aref arr x y))) (funcall con (cdr (aref arr x y)))) (funcall prod (cdr (aref arr x y)) x y)) ; try condition on second element in array[x,y]
			(T (traverse-2d-array arr con prod (1+ x) y)))))) ; get to next column

; returns enemy with ball
(defun get-ball-enemy (enemies)
	(cond
		((null enemies) nil)
		((percept-object-agent-has-ball (caar enemies)) (car enemies))
		(T (get-ball-enemy (cdr enemies)))))

			
; chooses closest target
(defun choose-target (myself enemies &optional (best nil))
	(if (null enemies)
		best
		(if (null best)
			(choose-target myself (cdr enemies) (car enemies))
			(if (< (distance-el myself (car enemies)) (distance-el myself best)) 
				(choose-target myself (cdr enemies) (car enemies))
				(choose-target myself (cdr enemies) best)))))

; finds out if I am closest to ball and therefore should follow it
(defun follow-ball? (myself ball enemies)
	(cond
		((null enemies) T)
		((> (distance-el myself ball) (distance-el (car enemies) ball))	nil)
		(T (follow-ball? myself ball (cdr enemies)))))

; run away from element
(defun run-from (myself el grid)
	(let ((moves '()))
		(if (and (>= (get-x myself) (get-x el)) (possible-move? grid (1+ (get-x myself)) (get-y myself)))
			(push 'go-right moves))
		(if (and (<= (get-x myself) (get-x el)) (possible-move? grid (1- (get-x myself)) (get-y myself)))
			(push 'go-left moves))

		(if (and (>= (get-y myself) (get-y el)) (possible-move? grid (get-x myself) (1+ (get-y myself))))
			(push 'go-up moves))
		(if (and (<= (get-y myself) (get-y el)) (possible-move? grid (get-x myself) (1- (get-y myself))))
			(push 'go-down moves))
		(random-move moves myself)))

; approach ball
(defun approach-ball (myself ball grid &optional (ignore-enemy nil))
	(let ((moves (list)))
		(if (and (> (get-x myself) (get-x ball)) (possible-move? grid (1- (get-x myself)) (get-y myself) ignore-enemy))
			(push 'go-left moves))
		(if (and (< (get-x myself) (get-x ball)) (possible-move? grid (1+ (get-x myself)) (get-y myself) ignore-enemy ))
			(push 'go-right moves))

		(if (and (> (get-y myself) (get-y ball)) (possible-move? grid (get-x myself) (1- (get-y myself)) ignore-enemy))
			(push 'go-down moves))
		(if (and (< (get-y myself) (get-y ball)) (possible-move? grid (get-x myself) (1+ (get-y myself)) ignore-enemy))
			(push 'go-up moves))
		(random-move moves myself)))

; finds out if move is possible
(defun possible-move? (grid x y &optional (ignore-enemy nil))
	(or
		(null (aref grid x y))
		(is-ball? (aref grid x y))
		(and
			ignore-enemy
			(is-enemy? (aref grid x y)))))

; move ball closer to enemy
(defun close-ball (myself enemy)
	(add-coord (get-coord myself) (div-coord (sub-coord (get-coord enemy) (get-coord myself)) 3)))
	
; chooses random move from list
(defun random-move (moves myself)
	(if (= (length moves) 0)
		'stay
		(elt moves (random (length moves)))))
		
; distance of 2 elements
(defun distance-el (el-a el-b)
	(distance (get-coord el-a) (get-coord el-b)))

; distance of 2 sets of coordinates
(defun distance (a b)
	(+ (abs (- (car b) (car a))) (abs (- (cadr b) (cadr a)))))

; agent has ball?
(defun has-ball? (el)
	(percept-object-agent-has-ball (car el)))

; gets coordinates of element
(defun get-coord (el)
	(cdr el))

(defun get-x (el)
	(cadr el))

(defun get-y (el)
	(caddr el))

(defun coord-equal? (a b)
	(equal a b))

(defun add-coord (a b)
	(cons (+ (car a) (car b)) (cons (+ (cadr a) (cadr b)) nil)))

(defun sub-coord (a b)
	(cons (- (car a) (car b)) (cons (- (cadr a) (cadr b)) nil)))

; divides coordinates by d
(defun div-coord (coord d)
	(cons (floor (/ (car coord) d)) (cons (floor (/ (cadr coord) d)) nil)))

(defun is-empty? (el)
	(null el))

(defun is-wall? (el)
	(and (not (is-empty? el)) (typep (car el) 'percept-object-wall)))

(defun is-ball? (el)
	(and (not (is-empty? el)) (typep (car el) 'percept-object-ball)))

(defun is-enemy? (el)
	(and (not (is-empty? el)) (typep (car el) 'percept-object-agent)))

(defun is-myself? (el name)
	(and (is-enemy? el) (string-equal (percept-object-agent-name (car el)) name)))
