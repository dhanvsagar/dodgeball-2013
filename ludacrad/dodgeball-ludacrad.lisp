(defconstant ludacrad-name "lr")

(defstructure (ludacrad    ; replace "my-agent" by your unique name, as e.g. FIT username
                (:include db-agent 
                  (body (make-ludacrad-body))
                  (program 'my-ludacrad-program)
                  (name ludacrad-name))))

(defstructure (ludacrad-body
               (:include db-agent-body
                         (name ludacrad-name)  (sname "LUDACRAD")))
    (spot nil))


(defun my-ludacrad-program (percept)
	(let*    
		(
			(me (car percept))
			(grid (cadr percept))
        	(my-loc (object-loc me))
	    	(ball-on-my-loc (member-if 
					(lambda (a) (typep a 'percept-object-ball)) 
					(apply #'aref grid (object-loc me))))
        	(holding-ball (object-contents me))	
		)
		(cond 
			(ball-on-my-loc 'grab-ball)
			( (and holding-ball (is-next-to-evil my-loc grid))
				(setf nearest (get-next-to-evil my-loc grid))
				`(throw-ball ,@nearest)
			)	
			( 	holding-ball 
				(setf evils (find-evils grid))
				(setf nearest (find-nearest-evil my-loc evils))
				(setf nearest (compute-next-to my-loc nearest grid))
     			`(throw-ball ,@nearest)	
			)
			( t 
				(when t
					(setf ball-pos (find-ball grid))
					(car (a-star my-loc ball-pos grid))
				)
			)
		)
	)
)

(defun is-next-to-evil (target grid) 
	(let*
		(
			(x (car target))
			(y (cadr target))
			(ret nil)
		)
	 	(when (exist-evil (+ x 1) y grid) (print 't1) (setf ret T))
		(when (exist-evil (- x 1) y grid) (print 't2) (setf ret T))
		(when (exist-evil x (+ y 1) grid) (print 't3) (setf ret T))
		(when (exist-evil x (- y 1) grid) (print 't4) (setf ret T))
		ret
	)
)


(defun get-next-to-evil (target grid) 
	(let*
		(
			(x (car target))
			(y (cadr target))
			(ret nil)
		)
	 	(when (exist-evil (+ x 1) y grid) (setf ret (list (+ x 1) y)))
		(when (exist-evil (- x 1) y grid) (setf ret (list (- x 1) y)))
		(when (exist-evil x (+ y 1) grid) (setf ret (list x (+ y 1))))
		(when (exist-evil x (- y 1) grid) (setf ret (list x (- y 1))))
		ret
	)
)

(defun compute-next-to (my-loc target grid)
	(let*
		(
			(x (car target))
			(y (cadr target))
			(mx (car my-loc))
			(my (cadr my-loc))
			(ret nil)
		)
		(if (< my y)
			(when (not (is-wall-or-evil x (- y 1) grid)) (setf ret (list x (- y 1))))
			(if (= my y)
				(if (< mx x)
					(when (not (is-wall-or-evil (- x 1) y grid)) (setf ret (list (- x 1) y)))
					(when (not (is-wall-or-evil (+ x 1) y grid)) (setf ret (list (+ x 1) y)))
				)
				(when (not (is-wall-or-evil x (+ y 1) grid)) (setf ret (list x (+ y 1))))			
			)
		)  
		ret
	)
)

(defun is-wall-or-evil (i j grid)
	(when (or (exist-evil i j grid) (is-wall i j grid))
		't
	)
)


(defun is-wall (i j grid)
	(let ((item (aref grid i j)))
		(setf ret (percept-object-wall-p item))
		ret
	)
)

(defvar *WIDTH* 8)
(defvar *HEIGHT* 8)


(defstruct node
  (coord '(-1 -1))
  (parent '(-1 -1))
  (successors '()) 
  (been-seen '*)
  (f 0)
  (g 0)
  (h 0)
)

(defun init-array(w grid)
  (dotimes (i (array-dimension w 0))
    (dotimes (j (array-dimension w 0))
      (setf (aref w i j) (make-node :coord (LIST i j)))
	  (setf ri (recompute-axis (+ i 1)))
	  (setf v (exist-evil (+ j 1) ri grid))
	  (when v
	 		(SETF (node-been-seen (aref w i j)) 'a)
	  )
    )
  )
)

(defun recompute-axis (x)
	(setf r (+ (- (- x 4)) 5))
)

; zdroj: http://www.edenwaith.com/products/pige/src/a-star.l

(defun a-star (s g grid)
  (let*
    (
	  (sx (- (recompute-axis (cadr s)) 1))
	  (sy (- (car s) 1))
	  (gx (- (recompute-axis (cadr g)) 1))
	  (gy (- (car g) 1))
	  (g1 (list gx gy))
      (start (make-node :been-seen 's))
      (w (make-array '(8 8)))
      (OPEN '())
      (CLOSED '())
      (steplist '())
    )

    (init-array w grid)

    (setf (node-been-seen (aref w sx sy)) 's)
    (setf (node-parent (aref w sx sy)) (list sx sy))
    (setf (node-been-seen (aref w gx gy)) 'g)
	
    (SETF OPEN (CONS (aref w sx sy) OPEN))
	
    (do
      (
        (ret_val 0)
        (n '())
        (i -1)
        (j -1)
      )
      ( (/= ret_val 0) )
      
        (COND
          ( (ENDP OPEN) 
            (setf ret_val -1)
          )  
        )

        (SETF n (FIRST OPEN))
        (SETF OPEN (REST OPEN))
        (SETF CLOSED (CONS n CLOSED))
        (SETF i (FIRST (node-coord n)))
        (SETF j (SECOND (node-coord n)))

        (COND
          ( (EQUAL (node-coord n) g1)
            (SETF ret_val 1)
          )
          (T
            (COND 
              ((is-successor w (- i 1) j) 
                (add-successor n (- i 1) j)
                (set-node n w (- i 1) j g1)
                (SETF OPEN (CONS (AREF w (- i 1) j) OPEN))
              )
            )
            (COND 
              ((is-successor w i (- j 1)) 
                (add-successor n i (- j 1))
                (set-node n w i (- j 1) g1)
                (SETF OPEN (CONS (AREF w i (- j 1)) OPEN))
              )
            )
            (COND 
              ((is-successor w i (+ j 1)) 
                (add-successor n i (+ j 1))
                (set-node n w i (+ j 1) g1)
                (SETF OPEN (CONS (AREF w i (+ j 1)) OPEN))
              )
            )
            (COND 
              ((is-successor w (+ i 1) j) 
                (add-successor n (+ i 1) j)
                (set-node n w (+ i 1) j g1)
                (SETF OPEN (CONS (AREF w (+ i 1) j) OPEN))
              )
            )
            (SORT OPEN #'f-compare)
          )
        )
      )
      
      (SETF (node-been-seen (AREF w (FIRST g1) (SECOND g1))) 'g)
      (DO
        ( (trail-coord g1))
        ( (EQUAL trail-coord (list sx sy)) )
        (SETF (node-been-seen (AREF w (FIRST trail-coord) (SECOND trail-coord))) 't)
		(SETF p-parent (node-parent (AREF w (FIRST trail-coord) (SECOND trail-coord))))
		(SETF steplist (append steplist (recompute-axis-path trail-coord p-parent)))
        (SETF trail-coord (node-parent (AREF w (FIRST trail-coord) (SECOND trail-coord))))
      )
	  (setf steplist (reverse steplist))
	  (print steplist)
  )
)

(defun recompute-axis-path (s g)
	(let (
			(sy (car s))
			(sx (cadr s))
			(gy (car g))
			(gx (cadr g))
		 ) 
		(cond 
				((< sx gx) '(go-left))
			  	((> sx gx) '(go-right))
		  	  	((< sy gy) '(go-up))
		  	  	((> sy gy) '(go-down))
		)
	)
)

(defun find-nearest-evil (start evils)
	(let* 	( 		
				(evils1 (car evils))
				(long (length evils1))
				(long2 (/ long 2))
				(min-coord (list 100 100))
			)
			(do
				((i 0 (+ i 1)))
				((= i long2) min-coord)
				(setf x (nth (* i 2) evils1))
				(setf y (nth (+ (* i 2) 1) evils1))
				(when ( < (distance start (list x y)) (distance start min-coord))
					(setf min-coord (list x y))
				)
			)
	)
)

(defun distance (start target)
   (+ (abs (- (xy-x start) (xy-x target))) (abs (- (xy-y start) (xy-y target))))
) 

(defun find-evils (grid)
	(let (
			(dimen (array-dimensions grid))
			(evils nil)
		)
		(do
				((x 1 (+ x 1)))
				((=  x (first dimen)) x)				
				(do
					((y 1 (+ y 1)))
					((=  y (second dimen)) y)
					(let ((objects (aref grid x y)))
						(when (not (null objects))
            				(dolist (object objects)	
	  							(setf v (exist-evil x y grid))	
              					(when v
									(setf evils (append evils  (list x y)))
								)
							)
						)	
					)
				)
		)
		(list evils)
	)	
)

(defun is-successor(w i j)
  (COND
    ( (OR (< i 0) (>= i 8)) NIL )
    ( (OR (< j 0) (>= j 8)) NIL )
    ( (EQUAL (node-been-seen (aref w i j)) 'g) T)
    ( (NOT (EQUAL (node-been-seen (aref w i j)) '* )) NIL)
    (T T)
  )
)

(defun add-successor(n i j)
  (SETF (node-successors n) (CONS (LIST i j) (node-successors n)))
)

(defun set-node (n w i j goal)
  (SETF (node-parent (aref w i j)) (node-coord n)) ;; set the parent
  (SETF (node-g (aref w i j)) ( + (node-g n) 1)) ;; get the real distance to node
  (SETF (node-h (aref w i j)) (+ (ABS (- i (FIRST goal)) ) (ABS (- j (SECOND goal)) ) ) )
  (SETF (node-f (aref w i j)) (+ (node-g (aref w i j)) (node-h (aref w i j)) ) )
  (SETF (node-been-seen (aref w i j)) 'x)
)


(defun f-compare (item1 item2)
    (< (node-f item1) (node-f item2) )
)

(defmethod is-evil-f ((obj percept-object))
  (equal (percept-object-name obj) wait-and-throw-db-agent-name))

(defun exist-evil(i j grid)
	(let ((item (aref grid i j)))
    	(dolist (object item)
        	(when (identify-in-list #'is-evil-f (aref grid i j))
				(return 't)
    )))
)


(defun find-ball (grid) 
	(let ((dimen (array-dimensions grid)))
		(setf rx 1)
		(setf ry 1)
		(do
				((x 1 (+ x 1)))
				((=  x (first dimen)) x)
				(do
					((y 1 (+ y 1)))
					((=  y (second dimen)) y)
					(let ((objects (aref grid x y)))
						(when (not (null objects))
            				(dolist (object objects)
              					(when (percept-object-ball-p object) 
									'(object) 
									(setf rx x)
									(setf ry y)
								)))	
					)
				)
		)
		(list rx ry)
	)
)

