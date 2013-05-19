(defstructure (temnymar
	(:include db-agent
		(program 'temnymar-agent-program)
		(body (make-temnymar-agent-body))
		(name "temnymar"))))
	
(defstructure (temnymar-agent-body
	(:include db-agent-body
		(name "TA")
		(sname "TA"))))
		
(defun temnymar-agent-program (percept)
	(let ((grid (temnymar-get-grid percept)) (ball) (myself) (enemies))
		(setf ball (temnymar-get-ball grid))
		(setf myself (temnymar-get-myself grid (temnymar-get-name percept)))
		(setf enemies (temnymar-get-enemies grid (temnymar-get-name percept)))
		(cond
			((null myself) 'stay) ; if I am dead
			((temnymar-has-ball? myself) ; if I have got ball
				(let ((enemy (temnymar-choose-target myself enemies))) ; find closest enemy
					(if (> (temnymar-distance-el myself enemy) *SURE-HIT-DIST*) ; if closest enemy is farther than 5 fields
						`(throw-ball ,@(temnymar-close-ball myself enemy)) ; get ball closer to enemy
						`(throw-ball ,@(temnymar-get-coord enemy))))) ; throw ball at enemy
			((null ball) (temnymar-run-from myself (temnymar-get-ball-enemy enemies) grid)) ; if enemy has ball then move opposite from ball (this should not happen)
			((temnymar-coord-equal? (temnymar-get-coord myself) (temnymar-get-coord ball)) 'grab-ball) ; if I am on same place as ball then grab it
			((= (temnymar-distance-el myself ball) 1) (temnymar-approach-ball myself ball grid T)) ; if ball is only one field away then always try to approach it
			((temnymar-follow-ball? myself ball enemies) (temnymar-approach-ball myself ball grid)) ; if I am closest to ball then approach it
			(T (temnymar-run-from myself ball grid))))) ; else run away from ball


(defun temnymar-get-grid (percept)
	(cadr percept))

(defun temnymar-get-name (percept)
	(db-agent-body-name (car percept)))

; returns ball with location
(defun temnymar-get-ball (grid)
	(temnymar-traverse-2d-array
		grid
		(lambda (el) (temnymar-is-ball? el))
		(lambda (el x y) (cons (car el) (cons x (cons y nil))))))
	
; returns myself with location	
(defun temnymar-get-myself (grid name)
	(temnymar-traverse-2d-array
		grid
		(lambda (el) (temnymar-is-myself? el name))
		(lambda (el x y) (cons (car el) (cons x (cons y nil))))))

; returns all agents with locations except myself
(defun temnymar-get-enemies (grid name)
	(temnymar-traverse-2d-array
		grid
		(defun con (el) (and (temnymar-is-enemy? el) (not (temnymar-is-myself? el name))))
		(defun prod (el x y) (cons (cons (car el) (cons x (cons y nil))) (temnymar-traverse-2d-array grid #'con #'prod (1+ x) y)))))

; traverses elements in 2d array checks condition con and if it is true then applies prod
(defun temnymar-traverse-2d-array (arr con prod &optional (x 0) (y 0))
	(cond
		((>= y (array-dimension arr 1)) nil) ; if y gets over size of array - end recursion
		((>= x (array-dimension arr 0)) (temnymar-traverse-2d-array arr con prod 0 (1+ y))) ; if x gets over size of array - get to next row
		(T
			(cond
				((funcall con (aref arr x y)) (funcall prod (aref arr x y) x y)) ; try condition on first element in array[x,y] 
				((and (not (null (aref arr x y))) (funcall con (cdr (aref arr x y)))) (funcall prod (cdr (aref arr x y)) x y)) ; try condition on second element in array[x,y]
			(T (temnymar-traverse-2d-array arr con prod (1+ x) y)))))) ; get to next column

; returns enemy with ball
(defun temnymar-get-ball-enemy (enemies)
	(cond
		((null enemies) nil)
		((percept-object-agent-has-ball (caar enemies)) (car enemies))
		(T (temnymar-get-ball-enemy (cdr enemies)))))

			
; chooses closest target
(defun temnymar-choose-target (myself enemies &optional (best nil))
	(if (null enemies)
		best
		(if (null best)
			(temnymar-choose-target myself (cdr enemies) (car enemies))
			(if (< (temnymar-distance-el myself (car enemies)) (temnymar-distance-el myself best)) 
				(temnymar-choose-target myself (cdr enemies) (car enemies))
				(temnymar-choose-target myself (cdr enemies) best)))))

; finds out if I am closest to ball and therefore should follow it
(defun temnymar-follow-ball? (myself ball enemies)
	(cond
		((null enemies) T)
		((> (temnymar-distance-el myself ball) (temnymar-distance-el (car enemies) ball))	nil)
		(T (temnymar-follow-ball? myself ball (cdr enemies)))))

; run away from element
(defun temnymar-run-from (myself el grid)
	(let ((moves '()))
		(if (and (>= (temnymar-get-x myself) (temnymar-get-x el)) (temnymar-possible-move? grid (1+ (temnymar-get-x myself)) (temnymar-get-y myself)))
			(push 'go-right moves))
		(if (and (<= (temnymar-get-x myself) (temnymar-get-x el)) (temnymar-possible-move? grid (1- (temnymar-get-x myself)) (temnymar-get-y myself)))
			(push 'go-left moves))

		(if (and (>= (temnymar-get-y myself) (temnymar-get-y el)) (temnymar-possible-move? grid (temnymar-get-x myself) (1+ (temnymar-get-y myself))))
			(push 'go-up moves))
		(if (and (<= (temnymar-get-y myself) (temnymar-get-y el)) (temnymar-possible-move? grid (temnymar-get-x myself) (1- (temnymar-get-y myself))))
			(push 'go-down moves))
		(temnymar-random-move moves myself)))

; approach ball
(defun temnymar-approach-ball (myself ball grid &optional (ignore-enemy nil))
	(let ((moves (list)))
		(if (and (> (temnymar-get-x myself) (temnymar-get-x ball)) (temnymar-possible-move? grid (1- (temnymar-get-x myself)) (temnymar-get-y myself) ignore-enemy))
			(push 'go-left moves))
		(if (and (< (temnymar-get-x myself) (temnymar-get-x ball)) (temnymar-possible-move? grid (1+ (temnymar-get-x myself)) (temnymar-get-y myself) ignore-enemy ))
			(push 'go-right moves))

		(if (and (> (temnymar-get-y myself) (temnymar-get-y ball)) (temnymar-possible-move? grid (temnymar-get-x myself) (1- (temnymar-get-y myself)) ignore-enemy))
			(push 'go-down moves))
		(if (and (< (temnymar-get-y myself) (temnymar-get-y ball)) (temnymar-possible-move? grid (temnymar-get-x myself) (1+ (temnymar-get-y myself)) ignore-enemy))
			(push 'go-up moves))
		(temnymar-random-move moves myself)))

; finds out if move is possible
(defun temnymar-possible-move? (grid x y &optional (ignore-enemy nil))
	(or
		(null (aref grid x y))
		(temnymar-is-ball? (aref grid x y))
		(and
			ignore-enemy
			(temnymar-is-enemy? (aref grid x y)))))

; move ball closer to enemy
(defun temnymar-close-ball (myself enemy)
	(temnymar-add-coord (temnymar-get-coord myself) (temnymar-div-coord (temnymar-sub-coord (temnymar-get-coord enemy) (temnymar-get-coord myself)) 3)))
	
; chooses random move from list
(defun temnymar-random-move (moves myself)
	(if (= (length moves) 0)
		'stay
		(elt moves (random (length moves)))))
		
; distance of 2 elements
(defun temnymar-distance-el (el-a el-b)
	(temnymar-distance (temnymar-get-coord el-a) (temnymar-get-coord el-b)))

; distance of 2 sets of coordinates
(defun temnymar-distance (a b)
	(+ (abs (- (car b) (car a))) (abs (- (cadr b) (cadr a)))))

; agent has ball?
(defun temnymar-has-ball? (el)
	(percept-object-agent-has-ball (car el)))

; gets coordinates of element
(defun temnymar-get-coord (el)
	(cdr el))

(defun temnymar-get-x (el)
	(cadr el))

(defun temnymar-get-y (el)
	(caddr el))

(defun temnymar-coord-equal? (a b)
	(equal a b))

(defun temnymar-add-coord (a b)
	(cons (+ (car a) (car b)) (cons (+ (cadr a) (cadr b)) nil)))

(defun temnymar-sub-coord (a b)
	(cons (- (car a) (car b)) (cons (- (cadr a) (cadr b)) nil)))

; divides coordinates by d
(defun temnymar-div-coord (coord d)
	(cons (floor (/ (car coord) d)) (cons (floor (/ (cadr coord) d)) nil)))

(defun temnymar-is-empty? (el)
	(null el))

(defun temnymar-is-wall? (el)
	(and (not (temnymar-is-empty? el)) (typep (car el) 'percept-object-wall)))

(defun temnymar-is-ball? (el)
	(and (not (temnymar-is-empty? el)) (typep (car el) 'percept-object-ball)))

(defun temnymar-is-enemy? (el)
	(and (not (temnymar-is-empty? el)) (typep (car el) 'percept-object-agent)))

(defun temnymar-is-myself? (el name)
	(and (temnymar-is-enemy? el) (string-equal (percept-object-agent-name (car el)) name)))
