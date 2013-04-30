(defconstant milikjan-agent-name "milikjan")


;; This is to be defined when designing a new student agent 
;
(defstructure (milikjan    ; replace "my-agent" by your unique name, as e.g. FIT username
                (:include db-agent 
                  (body (make-milikjan-agent-body))
                  (program 'milikjan-agent-program)
                  (name milikjan-agent-name)))
  "Your agent for db-world.")

(defstructure (milikjan-agent-body
			 (:include db-agent-body (name milikjan-agent-name)))
		    (history (make-hash-table))
		    )

(defun milikjan-agent-program (percept)
  (let* ((me (car percept))
	    (grid (cadr percept))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
         (holding-ball (object-contents me))
	    (ball-location (find-ball-location grid))
	    (location (find-X-location #'milikjan-is-myself grid))
	    (enemy-loc (milikjan-closest-enemy grid location))
	    (enemy-dist (milikjan-distance-to location enemy-loc))
	    (enemy-move (milikjan-move grid location enemy-loc))
	    (enemy-throw (mapcar #'+ location enemy-move))
	    (enemy-with-ball (milikjan-enemy-with-ball grid location))
	    ;(enemy (find-X-location #'milikjan-is-enemy grid))
	    )
    (progn
	 ;(format t "~%enemy-loc: ~S" enemy-loc)
	 ;(format t "~%objects: ~S" (milikjan-find-objects grid))
	 ;(format t "~%enemies ~S" (milikjan-find-enemies grid location))
	 (cond ( holding-ball (if (<= enemy-dist 1) ;*SURE-HIT-DIST*)
					    `(throw-ball ,@enemy-loc)
					    `(throw-ball ,@enemy-throw)) )
		  ( ball-on-my-loc 'grab-ball )
		  ( enemy-with-ball (milikjan-go enemy-with-ball) )
		  ( (not ball-location) (milikjan-evade grid location) )
		  ( t (or (milikjan-go (milikjan-move grid location ball-location)) 'stay) )
		)
	 )

;    (cond ( (not ball-location) 'stay )
;		( ball-on-my-loc 'grab-ball )
;		( holding-ball 'stop )
;		( t (or (milikjan-go (milikjan-move grid location ball-location)) 'stay) )
;		)
    )
  )


(defun milikjan-go (delta)
  (if (> (abs (first delta)) (abs (second delta)))
    (cond ( (= (first delta) 0) nil )
		( (> (first delta) 0) 'go-right )
		( t 'go-left )
		)
    (if (> (second delta) 0) 'go-up 'go-down))
  )


(defun milikjan-move (grid location target)
  (reduce (lambda (old delta)
		  (cond ( (not old) (if (milikjan-is-free grid (mapcar #'+ location delta))
						  delta old) )
			   ( (not (milikjan-is-free grid (mapcar #'+ location delta))) old )
			   ( (< (points-dist target (mapcar #'+ location delta))
				   (points-dist target (mapcar #'+ location old))) delta )
			;   ( (< (milikjan-distance-to target (mapcar #'+ location delta))
			;	   (milikjan-distance-to target (mapcar #'+ location old))
			;	   ) delta )
			   ( t old )
			   ))
		'((1 0) (-1 0) (0 1) (0 -1))
		:initial-value nil
		)
  )


(defun milikjan-evade (grid location)
  (or (milikjan-go '(1 0))
	 (milikjan-go '(0 1))
	 (milikjan-go '(-1 0))
	 (milikjan-go '(0 -1))
	 'stay)
  )


(defun milikjan-distance (delta)
  (+ (abs (first delta)) (abs (second delta)))
  )


(defun milikjan-distance-to (from to)
  (milikjan-distance (mapcar #'- to from))
  )


(defun milikjan-get-object (grid loc)
  (let ((value (aref grid (first loc) (second loc)))
	   )
    (if value (list loc (car value)) nil)
    )
  )


; Code from http://www.lispforum.com/viewtopic.php?f=2&t=976
(defun milikjan-cartesian-product (list1 list2)
  "Return a list of the Cartesian product of two lists."
  (mapcan (lambda (x) (mapcar (lambda (y) (list x y)) list2)) list1)
  )


(defun milikjan-grid-locations (grid)
  (milikjan-cartesian-product (loop for i below (first (array-dimensions grid)) collect i)
						(loop for j below (second (array-dimensions grid)) collect j))
  )


(defun milikjan-find-objects (grid)
  (reduce (lambda (found loc) (let ((item (milikjan-get-object grid loc))
							 )
						  (if item (cons item found) found)))
		(milikjan-grid-locations grid)
		:initial-value nil)
  )


(defun milikjan-find-enemies (grid location)
  (sort (reduce (lambda (found object)
			   (if (milikjan-is-enemy (second object))
				(cons (first object) found)
				found))
			 (milikjan-find-objects grid)
			 :initial-value nil)
	   (lambda (a b) (< (milikjan-distance-to location a)
					(milikjan-distance-to location b))))
  )


(defun milikjan-closest-enemy (grid location)
  (let ((enemies (milikjan-find-enemies grid location))
	   )
    (if enemies (car enemies) nil)
    )
  )


(defun milikjan-is-free (grid location)
  (let ((x (aref grid (first location) (second location)))
	   )
    (cond ( (not x) t )
		( (and (not (cdr x)) (my-ball-p (first x))) t )
		( t nil )
		)
    )
  )


(defmethod milikjan-is-myself ((obj percept-object))
  (if (equal (percept-object-name obj) milikjan-agent-name) obj nil)
  )


(defmethod milikjan-is-enemy ((obj percept-object))
  (let ((x (percept-object-name obj))
	   )
  	(if (and (not (equal x "#"))
		    (not (equal x "B"))
		    (not (equal x milikjan-agent-name)))
	  obj nil)
	)
  )


(defun milikjan-is-enemy-with-ball (objects)
  (and (identify-in-list #'milikjan-is-enemy objects)
	  (or (identify-in-list #'my-ball-p objects)
		 nil))
		 ;(object-contents (first objects))))
  )


(defun milikjan-enemy-with-ball (grid location)
  (dolist (neighbour '((1 0) (-1 0) (0 1) (0 -1)))
    (let ((loc (mapcar #'+ location neighbour)))
	 (when (milikjan-is-enemy-with-ball (aref grid (first loc) (second loc)))
	   (return-from milikjan-enemy-with-ball neighbour))))
  )


