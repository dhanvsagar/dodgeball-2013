(defconstant musilon4-agent-name "musilon4")

(defstructure (musilon4 (:include db-agent 
	(body (make-musilon4-agent-body))
	(program 'musilon4-program)
	(name musilon4-agent-name)
)))

(defstructure (musilon4-agent-body (:include db-agent-body
	(name musilon4-agent-name))))

(defun musilon4-program (percept)
	(let* (
			(me (first percept))
			(grid (second percept))
			(my-pos (musilon4-find-my-position grid))
			(ball-pos (musilon4-find-ball-position grid))
			(enemies (musilon4-annotate-enemies my-pos (musilon4-find-enemies-position grid) grid))
			(holding-ball (object-contents me))
		)

		; debug output
		;(print my-pos)
		;(print ball-pos)
		;(print enemies)
		;(print (musilon4-sort-enemies-by-distance enemies))
		;(print (musilon4-sort-enemies-by-lives enemies))
		;(print (musilon4-sort-enemies-by-ball enemies))
		;(print (musilon4-accessible-fields '(1 1) grid))
		;(print (musilon4-empty-field (list 1 1) grid))
		;(print *CAN-HIT-DIST*)
		;(print (musilon4-fields-dists my-pos grid))

		;(print (remove-if (lambda (x) (< (car x) *CAN-HIT-DIST*)) (musilon4-fields-dists ball-pos grid)))
		;(print (musilon4-go (list 3 3) (list 4 3) grid))

		;(print (musilon4-go-safe my-pos ball-pos grid))

		(cond 
			(holding-ball (musilon4-holding-ball-action enemies my-pos grid)) ; nejak chytre hodit
			((equal my-pos ball-pos) 'grab-ball) ; zvedneme mic
			((musilon4-accessible-fields my-pos grid) (musilon4-empty-hands-action enemies my-pos ball-pos grid)) ; jdeme pro balon
			(T 'stay)
		)
	))

;;; pohyb bo hernim planu

(defun musilon4-empty-hands-action (enemies my-pos ball-pos grid)
	(cond 
		;( (musilon4-protect enemies my-pos ball-pos grid)) ; bran se co to jde
		((every (lambda (x) (musilon4-static-player x grid)) (mapcar #'musilon4-get-enemy-position enemies)) (musilon4-static-enemy-empty-hands-action enemies my-pos ball-pos grid))
		(T (musilon4-non-static-enemy-empty-hands-action enemies my-pos ball-pos grid))))

(defun musilon4-enemy-and-ball (enemies ball-pos grid)
	(some (lambda (x) ) ))

(defun musilon4-static-enemy-empty-hands-action (enemies my-pos ball-pos grid)
	(if (= (musilon4-manhattan my-pos ball-pos) 1)
		(musilon4-go-all my-pos ball-pos grid)
		(musilon4-go my-pos ball-pos grid)))

(defun musilon4-non-static-enemy-empty-hands-action (enemies my-pos ball-pos grid)
	(let ((enemies-dists (mapcar (lambda (x) (musilon4-manhattan ball-pos (musilon4-get-enemy-position x))) enemies))
		(my-dist (musilon4-manhattan ball-pos my-pos)))
		(cond 
			((= my-dist 1) (musilon4-go-all my-pos ball-pos grid))
			((every (lambda (x) (> x my-dist)) enemies-dists) (musilon4-go my-pos ball-pos grid))
			(T (musilon4-protect enemies my-pos ball-pos grid)))))

(defun musilon4-protect (enemies my-pos ball-pos grid)
	(let* (
		(dists (remove-if (lambda (x) (< (car x) *CAN-HIT-DIST*)) (musilon4-fields-dists-pst ball-pos grid)))
		(closest (sort (mapcar (lambda (x) (cons (musilon4-manhattan my-pos (cadr x)) (cdr x))) dists) #'< :key #'first))
		)
		(musilon4-go my-pos (cadar closest) grid)
	))

;;;

;;; akce kdyz mam mic --------------------------------------------------------------------------------------------------------
(defun musilon4-holding-ball-action-dummy (enemies my-pos grid)
	(let ((target (musilon4-get-enemy-position (car enemies))))
		(list 'throw-ball (car target) (cadr target))))

(defun musilon4-holding-ball-action (enemies my-pos grid)
	(if (every (lambda (x) (musilon4-static-player x grid)) (mapcar #'musilon4-get-enemy-position enemies))
		(musilon4-static-enemy-holding-ball-action enemies my-pos grid)
		(musilon4-non-static-enemy-holding-action enemies my-pos grid)))

(defun musilon4-static-enemy-holding-ball-action (enemies my-pos grid)
	(let ((closest (musilon4-sort-enemies-by-distance enemies)))
		(if (= 1 (musilon4-get-enemy-distance (car closest)))
			(musilon4-throw-to (musilon4-get-enemy-position (car closest)))
			(musilon4-pass-close (musilon4-get-enemy-position (car closest)) my-pos grid))))

(defun musilon4-pass-close (position my-pos grid)
	(let ((sorted (sort (musilon4-annotate-with-distance my-pos (musilon4-empty-fields position grid)) #'< :key #'first)))
		(musilon4-throw-to (cadar sorted))))

(defun musilon4-non-static-enemy-holding-action (enemies my-pos grid)
	(let ((sorted (musilon4-sort-enemies-by-distance enemies)))
		(musilon4-throw-to (musilon4-get-enemy-position (car (last enemies))))))

(defun musilon4-throw-to (position)
	(list 'throw-ball (car position) (cadr position)))

;;; --------------------------------------------------------------------------------------------------------

(defun musilon4-can-grab-ball-action ()
	'grab-ball)

(defun musilon4-find-my-position (grid)
	(find-X-location #'musilon4-me-position-p grid))

(defun musilon4-find-enemies-position (grid)
	(musilon4-find-X-location-all #'musilon4-enemy-position-p grid))

(defun musilon4-find-ball-position (grid)
	(find-X-location #'musilon4-ball-p grid))

(defmethod musilon4-ball-p ((obj percept-object))
	(if (or (and (not (equal (percept-object-name obj) "#")) 
			(not (equal (percept-object-name obj) "B"))
			(percept-object-agent-has-ball obj))
		(equal (percept-object-name obj) "B"))
	obj nil))

(defmethod musilon4-me-position-p ((obj percept-object)) 
	(if (equal (percept-object-name obj) musilon4-agent-name) 
		obj
		nil))

(defmethod musilon4-enemy-position-p ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#")) 
		   (not (equal (percept-object-name obj) musilon4-agent-name)) 
		   (not (equal (percept-object-name obj) "B")))
	  obj
	  nil))

(defun musilon4-accessible-fields (from grid)
	(let* ((dirs '((1 0) (-1 0) (0 1) (0 -1)))
			(adj (mapcar (lambda (x) (mapcar #'+ from x)) dirs)))
		(remove-if (lambda (x) (or (every (lambda (y) (< y 0)) x) (musilon4-occupied-field x grid))) adj)
	))

(defun musilon4-empty-fields (from grid)
	(let* ((dirs '((1 0) (-1 0) (0 1) (0 -1)))
			(adj (mapcar (lambda (x) (mapcar #'+ from x)) dirs)))
		(remove-if-not (lambda (x) (and (every (lambda (y) (>= y 0)) x) (musilon4-empty-field x grid))) adj)
	))

(defun musilon4-adjacent-fields (from)
	(let* ((dirs '((1 0) (-1 0) (0 1) (0 -1)))
			(adj (mapcar (lambda (x) (mapcar #'+ from x)) dirs)))
	adj))

(defun musilon4-direction (coord)
	(cond ((equal coord '(1 0)) 'go-right)
		((equal coord '(-1 0)) 'go-left)
		((equal coord '(0 1)) 'go-up)
		((equal coord '(0 -1)) 'go-down)
		(T 'stay)
	))

(defun musilon4-navigate (from to grid)
	(if (or (null from) (null to))
		'(0 0)
		(let ((adj (sort (musilon4-annotate-with-distance-euklid to (musilon4-accessible-fields from grid)) #'< :key #'first)))
			(if (null adj)
				'(0 0)
				(mapcar #'- (cadar adj) from)))
		))

(defun musilon4-go (from to grid)
	(musilon4-direction (musilon4-navigate from to grid)))

(defun musilon4-go-all (from to grid)
	(if (or (null from) (null to))
		'(0 0)
		(let ((adj (sort (musilon4-annotate-with-distance-euklid to (musilon4-adjacent-fields from)) #'< :key #'first)))
			(if (null adj)
				'(0 0)
				(musilon4-direction (mapcar #'- (cadar adj) from))))
		))

(defun musilon4-go-safe (my-pos ball-pos grid)
	(let* (
		(dists (remove-if (lambda (x) (< (car x) *CAN-HIT-DIST*)) (musilon4-fields-dists-pst ball-pos grid))) ; vememe si pole ktera jslou uz za hrani trefitelnosti
		(closest (sort (mapcar (lambda (x) (cons (musilon4-manhattan my-pos (cadr x)) (cdr x))) dists) #'< :key #'first))
		)
		(musilon4-go my-pos (cadar closest) grid)
	))

(defun musilon4-fields-dists-pst (from grid)
	(setf result '())
	(dotimes (numberx (car (array-dimensions grid)))
		(dotimes (numbery (cadr (array-dimensions grid)))
			(setf result (cons (cons (musilon4-euklid from (list numberx numbery)) (list (list numberx numbery))) result))))
	result)

(defun musilon4-manhattan (from to)
	(sum (list (abs (- (car to) (car from))) (abs (- (cadr from) (cadr to))))))

(defun musilon4-euklid (from to) 
	(sqrt (apply #'+ (mapcar (lambda (x) (* x x)) (mapcar #'- from to)))))

(defun musilon4-find-X-location-all (X-predicate grid)
	(setf result '())
	(dotimes (numberx (car (array-dimensions grid)))
		(dotimes (numbery (cadr (array-dimensions grid)))
			(when (identify-in-list X-predicate (aref grid numberx numbery))
				(setf result (cons (list numberx numbery) result))
			)))
	result)

(defun musilon4-annotate-enemies (frompoint enemies grid &optional (annotated '()))
	(if (null enemies) 
		annotated
		(let ((pos (car enemies)))
		(musilon4-annotate-enemies frompoint (cdr enemies) grid (cons (append (list (musilon4-manhattan frompoint pos)) (list (musilon4-enemy-lives pos grid)) (list (musilon4-enemy-holds-ball pos grid)) (list pos)) annotated) ))))

(defun musilon4-annotate-with-distance (frompoint points)
	(mapcar (lambda (x) (cons (musilon4-manhattan frompoint x) (list x))) points))

(defun musilon4-annotate-with-distance-euklid (frompoint points)
	(mapcar (lambda (x) (cons (musilon4-euklid frompoint x) (list x))) points))

(defun musilon4-sort-enemies-by-distance (enemies &key (test #'<))
	(sort enemies test :key #'first))

(defun musilon4-sort-enemies-by-lives (enemies &key (test #'<))
	(sort enemies test :key #'second))

(defun musilon4-sort-enemies-by-ball (enemies &key (test #'equal))
	(sort enemies test :key #'third))

(defun musilon4-enemy-holds-ball (position grid)
	(let ((agent (identify-in-list #'musilon4-enemy-position-p (aref grid (car position) (cadr position)))))
		(if (not (equal agent nil))
			(percept-object-agent-has-ball agent)
			nil
		)))

(defun musilon4-enemy-lives (position grid)
	(let ((agent (identify-in-list #'musilon4-enemy-position-p (aref grid (car position) (cadr position)))))
		(if (not (equal agent nil))
			(percept-object-agent-lives agent)
			nil
		)))

(defun musilon4-empty-field (position grid)
	(equal nil (aref grid (car position) (cadr position))))

(defun musilon4-occupied-field (position grid)
	(identify-in-list #'musilon4-ocuppied-field-p (aref grid (car position) (cadr position))))

(defmethod musilon4-ocuppied-field-p ((obj percept-object))
  (if (or (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) musilon4-agent-name)) 
           (not (equal (percept-object-name obj) "B")))
      (equal (percept-object-name obj) "#"))
  obj nil))

(defun musilon4-static-player (position grid)
	(identify-in-list (lambda ((obj percept-object)) (if (equal (percept-object-name obj) wait-and-throw-db-agent-name) obj nil)) (aref grid (car position) (cadr position))))

(defun musilon4-get-enemy-distance (entry)
	(first entry))

(defun musilon4-get-enemy-lives (entry)
	(second entry))

(defun musilon4-get-enemy-has-ball (entry)
	(third entry))

(defun musilon4-get-enemy-position (entry)
	(fourth entry))
