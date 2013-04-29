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
			((has-ball? myself) ; if I have got ball
				(let ((enemy (choose-target myself enemies))) ; find closest enemy
					(if (= (distance-el myself enemy) 1) 
						`(throw-ball ,@(get-coord enemy)) ; if enemy is only 1 field away then throw ball at him
						(throw-close myself enemy grid)))) ; if enemy is farther then throw ball close to him 
			((coord-equal? (get-coord myself) (get-coord ball)) 'grab-ball) ; if I am on same place as ball then grab it
			((= (distance-el myself ball) 1) (approach-ball myself ball grid T)) ; if ball is one field away then always approach it
			(T (approach-ball myself ball grid))))) ; else approach ball

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
				((and (not (null (aref arr x y))) (funcall con (cdr (aref arr x y)))) (funcall prod (cdr (aref arr x y)) x y)) ; try condition of second element in array[x,y]
				(T (traverse-2d-array arr con prod (1+ x) y)))))) ; get to next column
			
; chooses closest target
(defun choose-target (myself enemies &optional (best nil))
	(if (null enemies)
		best
		(if (null best)
			(choose-target myself (cdr enemies) (car enemies))
			(if (< (distance-el myself (car enemies)) (distance-el myself best)) 
				(choose-target myself (cdr enemies) (car enemies))
				(choose-target myself (cdr enemies) best)))))

; move ball next to enemy
(defun throw-close (myself enemy grid)
	(cond
		((> (get-x myself) (get-x enemy)) `(throw-ball ,@(cons (1+ (get-x enemy)) (cons (get-y enemy) nil))))
		((< (get-x myself) (get-x enemy)) `(throw-ball ,@(cons (1- (get-x enemy)) (cons (get-y enemy) nil))))
		((> (get-y myself) (get-y enemy)) `(throw-ball ,@(cons (get-x enemy) (cons (1+ (get-y enemy)) nil))))
		((< (get-y myself) (get-y enemy)) `(throw-ball ,@(cons (get-x enemy) (cons (1- (get-y enemy)) nil))))))

; approach ball
(defun approach-ball (myself ball grid &optional (always nil))
	(cond	
		((and (> (get-x myself) (get-x ball)) (or always (is-empty? (aref grid (1- (get-x myself)) (get-y myself))))) 'go-left)
		((and (< (get-x myself) (get-x ball)) (or always (is-empty? (aref grid (1+ (get-x myself)) (get-y myself))))) 'go-right)
		((and (> (get-y myself) (get-y ball)) (or always (is-empty? (aref grid (get-x myself) (1- (get-y myself)))))) 'go-down)
		((and (< (get-y myself) (get-y ball)) (or always (is-empty? (aref grid (get-x myself) (1+ (get-y myself)))))) 'go-up)
		(T (random-move))))


(defun random-move ()
	(let ((move (random 4)))
		(cond
			((= move 0) 'go-right)
			((= move 1) 'go-left)
			((= move 2) 'go-up)
			((= move 3) 'go-down))))

; distance of 2 elements
(defun distance-el (el-a el-b)
	(distance (get-coord el-a) (get-coord el-b)))

; distance of 2 sets of coordinates
(defun distance (a b)
;	(sqrt (+ (expt (- (car b) (car a)) 2) (expt (- (cadr b) (cadr a)) 2))))
	(+ (abs (- (car b) (car a))) (abs (- (cadr b) (cadr a)))))

; element has ball?
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
