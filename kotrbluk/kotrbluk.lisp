(defconstant kotrbluk-name "LK")

(defstructure
	(kotrbluk-body
		(:include db-agent-body (name kotrbluk-name))
	)
)

(defstructure 
	(kotrbluk
		(:include db-agent
			(body (make-kotrbluk-body))
			(program 'kotrbluk-main)
			(name kotrbluk-name)
		)
	)
)

(defun kotrbluk-main (percept)
	(let (
			(ball-pos (kotrbluk-find-ball (cadr percept)))
			(my-pos (kotrbluk-find-me (cadr percept)))
			(enemy-pos (kotrbluk-find-enemy (cadr percept)))
		)
		(cond
			((null ball-pos)
				;(print (kotrbluk-find-target (cadr percept) my-pos))
				(return-from kotrbluk-main `(throw-ball ,@(kotrbluk-find-target (cadr percept) my-pos)))
			)
			((equal ball-pos my-pos) 'grab-ball)
			(t
				(let 
					(
						(next-pos (kotrbluk-get-next-position (cadr percept) my-pos ball-pos))
					)
					(return-from kotrbluk-main
						(cond 
							((< (car next-pos) (car my-pos)) `go-left)
							((> (car next-pos) (car my-pos)) `go-right)
							((< (cadr next-pos) (cadr my-pos)) `go-down)
							((> (cadr next-pos) (cadr my-pos)) `go-up)
							(t `stay)
						)
					)
				)
			)
		)
	)
	;(read)
)

(defun kotrbluk-backtrack (state)
	(loop
		(if (null (caddr state)) (return-from kotrbluk-backtrack (car state)))
		(setf state (cdr state))
	)
)

(defun kotrbluk-equal-node (left right)
	(equal (car left) (car right))
)

(defun kotrbluk-find-ball (grid)
	(dotimes (x (car (array-dimensions grid)))
		(dotimes (y (cadr (array-dimensions grid)))
			(if (kotrbluk-is-ball (aref grid x y))
				(return-from kotrbluk-find-ball (list x y))
			)
		)
	)
)

(defun kotrbluk-find-enemy (grid)
	(dotimes (x (car (array-dimensions grid)))
		(dotimes (y (cadr (array-dimensions grid)))
			(if (kotrbluk-is-enemy (aref grid x y))
				(return-from kotrbluk-find-enemy (list x y))
			)
		)
	)
)
(defun kotrbluk-find-me (grid)
	(dotimes (x (car (array-dimensions grid)))
		(dotimes (y (cadr (array-dimensions grid)))
			(if (kotrbluk-is-me (aref grid x y))
				(return-from kotrbluk-find-me (list x y))
			)
		)
	)
)

(defun kotrbluk-find-target (grid my-pos)
	(let ((enemy-pos (kotrbluk-get-closest-enemy grid my-pos)))
		;(print (car enemy-pos))
		;(print (cadr enemy-pos))
		;(print (car my-pos))
		;(print (cadr my-pos))
		(print (kotrbluk-get-distance enemy-pos my-pos))
		(cond
			((equal (kotrbluk-get-distance enemy-pos my-pos) 1) enemy-pos)
			((< (car enemy-pos) (car my-pos)) (list (+ (car enemy-pos) 1) (cadr enemy-pos)))
			((> (car enemy-pos) (car my-pos)) (list (- (car enemy-pos) 1) (cadr enemy-pos)))
			((< (cadr enemy-pos) (cadr my-pos)) (list (car enemy-pos) (+ (cadr enemy-pos) 1)))
			((> (cadr enemy-pos) (cadr my-pos)) (list (car enemy-pos) (- (cadr enemy-pos) 1)))
		)
	)
)

(defun kotrbluk-get-closest-enemy (grid my-pos)
	(let (
			(opened nil)
			(closed nil)
			(visit nil)
			(successors nil)
		)
		(setf my-pos (list (list my-pos nil)))
		(setf opened (append opened my-pos))
		(loop
			(if (null opened) (return-from kotrbluk-get-next-position my-pos))
			(setf visit (pop opened))
			(setf closed (append closed (list visit)))
			(if (kotrbluk-is-enemy (aref grid (caar visit) (cadar visit))) (return-from kotrbluk-get-closest-enemy (car visit)))
			(setf successors (kotrbluk-get-successors grid visit))
			(dotimes (i (length successors))
				(let ((suc (nth i successors)))
					(if 
						(and
							(not
								(or
									(position suc opened :test #'kotrbluk-equal-node)
									(position suc closed :test #'kotrbluk-equal-node)
								)
							)
							(or (null (aref grid (caar suc) (cadar suc))) (kotrbluk-is-enemy (aref grid (caar suc) (cadar suc))))
						)
						(setf opened (append opened (list suc)))
					)
				)
			)
		)
	)
)

(defun kotrbluk-get-distance (pos1 pos2) (+ (abs (- (car pos1) (car pos2))) (abs (- (cadr pos1) (cadr pos2)))))

(defun kotrbluk-get-next-position (grid start finish)
	(let (
			(opened nil)
			(closed nil)
			(visit nil)
			(successors nil)
		)
		(setf start (list (list start nil)))
		(setf opened (append opened start))
		(loop
			(if (null opened) (return-from kotrbluk-get-next-position start))
			(setf visit (pop opened))
			(setf closed (append closed (list visit)))
			(if (equal (car visit) finish) (return-from kotrbluk-get-next-position (kotrbluk-backtrack visit)))
			(setf successors (kotrbluk-get-successors grid visit))
			(dotimes (i (length successors))
				(let ((suc (nth i successors)))
					(if 
						(and
							(not
								(or
									(position suc opened :test #'kotrbluk-equal-node)
									(position suc closed :test #'kotrbluk-equal-node)
								)
							)
							(or (null (aref grid (caar suc) (cadar suc))) (kotrbluk-is-ball (aref grid (caar suc) (cadar suc))))
						)
						(setf opened (append opened (list suc)))
					)
				)
			)
		)
	)
)

(defun kotrbluk-get-successors (grid visit)
	(let 
		(
			(result nil)
			(left (list (- (caar visit) 1) (cadar visit)))
			(right (list (+ (caar visit) 1) (cadar visit)))
			(down (list (caar visit) (- (cadar visit) 1)))
			(top (list (caar visit) (+ (cadar visit) 1)))
		)
		(setf result (append result (list (cons left visit))))
		(setf result (append result (list (cons right visit))))
		(setf result (append result (list (cons down visit))))
		(setf result (append result (list (cons top visit))))
		(return-from kotrbluk-get-successors result)
	)
)

(defun kotrbluk-is-ball (x)
	(or (equal (subseq (write-to-string (car x)) 0 1) "B") (equal (subseq (write-to-string (cadr x)) 0 1) "B"))
)

(defun kotrbluk-is-enemy (x)
	(not 
		(or (kotrbluk-is-ball x)
			(kotrbluk-is-me x)
			(kotrbluk-is-wall x)
			(null x)
		)
	)
)

(defun kotrbluk-is-me (x)
	(cond
		((>= (length (write-to-string (car x))) (length kotrbluk-name))
			(equal (subseq (write-to-string (car x)) 0 (length kotrbluk-name)) kotrbluk-name)
		)
		((>= (length (write-to-string (cadr x))) (length kotrbluk-name))
			(equal (subseq (write-to-string (cadr x)) 0 (length kotrbluk-name)) kotrbluk-name)
		)
	)
)

(defun kotrbluk-is-wall (x) (equal (write-to-string (car x)) "#"))
