(defconstant kajtmlud-db-agent-name "LK")

(defstructure (kajtmlud-db-agent-body (:include db-agent-body (name kajtmlud-db-agent-name))))

(defstructure (kajtmlud
               (:include db-agent 
                         (program 'kajtmlud-program) 
                         (body (make-kajtmlud-db-agent-body))
                         (name kajtmlud-db-agent-name))))

(defun kajtmlud-program (percept)
  (let* ((me (car percept))
         (grid (cadr percept))
		(my-loc (find-my-loc grid))
		(evil-loc (find-evil-loc grid))
         	(ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
         	(holding-ball (object-contents me))
		(ball-loc (find-ball-loc grid)))

	(cond ((not evil-loc) 'stop)
		(ball-on-my-loc 'grab-ball)
    		(holding-ball `(throw-ball ,@(find-closest-evil-robot my-loc evil-loc)))
    		(t (cond ((and (or (watch-my-steps grid (car my-loc) (+ (cadr my-loc) 1))
				(watch-my-steps grid (+ (car my-loc) 1) (cadr my-loc)))
                    		(< (cadr my-loc) (cadr ball-loc))
				(< (car my-loc) (car ball-loc)))
				(nth (random 2) '(go-up go-right)))
                	((and (or (watch-my-steps grid (+ (car my-loc) 1) (cadr my-loc))
				(watch-my-steps grid (car my-loc) (- (cadr my-loc) 1)))
				(> (cadr my-loc) (cadr ball-loc))
                     		(< (car my-loc) (car ball-loc)))
				(nth (random 2) '(go-down go-right)))
                	((and (or (watch-my-steps grid (- (car my-loc) 1) (cadr my-loc))
				(watch-my-steps grid (car my-loc) (+ (cadr my-loc) 1)))
				(< (cadr my-loc) (cadr ball-loc))
                     		(> (car my-loc) (car ball-loc)))
				(nth (random 2) '(go-up go-left)))
               		((and (or (watch-my-steps grid (car my-loc) (- (cadr my-loc) 1))
				(watch-my-steps grid (- (car my-loc) 1) (cadr my-loc)))   
                  		(> (cadr my-loc) (cadr ball-loc))
				(> (car my-loc) (car ball-loc)))
				(nth (random 2) '(go-down go-left)))

			; nasledujici moznosti mohou vyuzivat funkci watch-my-steps
			; misto watch-my-steps2, coz podstatne snizi pocet kol
			; nutnych k vitezstvi, ale existuji pak (ac velmi male mnozstvi)
			; takova rozvrzeni zlych robotu, ze se chytry agent zasekne
 
            		((and (watch-my-steps2 grid (car my-loc) (+ (cadr my-loc) 1))
                    		(< (cadr my-loc) (cadr ball-loc))
				(equal (car my-loc) (car ball-loc)))
                     		'go-up)
                	((and (watch-my-steps2 grid (+ (car my-loc) 1) (cadr my-loc))
				(equal (cadr my-loc) (cadr ball-loc))
                     		(< (car my-loc) (car ball-loc)))
                     		'go-right)
                	((and (watch-my-steps2 grid (- (car my-loc) 1) (cadr my-loc))
				(equal (cadr my-loc) (cadr ball-loc))
                     		(> (car my-loc) (car ball-loc)))
                     		'go-left)
               		((and (watch-my-steps2 grid (car my-loc) (- (cadr my-loc) 1))
                     		(> (cadr my-loc) (cadr ball-loc))
				(equal (car my-loc) (car ball-loc)))
				'go-down)
			(t (nth (random 4) '(go-down go-right go-left go-up))))))))

(defun find-my-loc (grid)
    (dotimes (numberx (car (array-dimensions grid)))
        (dotimes (numbery (cadr (array-dimensions grid)))
            (when (identify-in-list #'id-my-robot (aref grid numberx numbery))
                (return-from find-my-loc (list numberx numbery))))) nil)

(defun find-ball-loc (grid)
    (dotimes (numberx (car (array-dimensions grid)))
        (dotimes (numbery (cadr (array-dimensions grid)))
            (when (identify-in-list #'percept-object-ball-p (aref grid numberx numbery))
                (return-from find-ball-loc (list numberx numbery))))) nil)

(defun find-evil-loc (grid)
    (let ((evil-list nil))
        (dotimes (numberx (car (array-dimensions grid)))
            (dotimes (numbery (cadr (array-dimensions grid)))
                (when (identify-in-list #'id-evil-robot (aref grid numberx numbery))
                    (setf evil-list (cons (cons numberx numbery) evil-list))))) evil-list))

(defmethod id-evil-robot ((robot percept-object))
    (if (and (percept-object-agent-p robot) (not (equal (percept-object-name robot) kajtmlud-db-agent-name))) robot nil))

(defmethod id-my-robot ((robot percept-object))
    (if (equal (percept-object-name robot) kajtmlud-db-agent-name) robot nil))

(defun find-closest-evil-robot (my-loc evil-loc)
    (let ((min-distance 50) (closest-evil-robot nil))
        (dolist (evil-robot evil-loc)
            (let ((distance (count-distance (car my-loc) (cadr my-loc) (car evil-robot) (cdr evil-robot))))
                (if (< distance min-distance)
                    (progn
                        (setf min-distance distance)
			(if (> (+ (abs (- (car my-loc) (car evil-robot))) (abs (- (cadr my-loc) (cdr evil-robot)))) 1)
				(cond ((> (car my-loc) (car evil-robot))
						(setf closest-evil-robot (list (+ (car evil-robot) 1) (cdr evil-robot))))
					((< (car my-loc) (car evil-robot))
						(setf closest-evil-robot (list (- (car evil-robot) 1) (cdr evil-robot))))
					(t (cond ((> (cadr my-loc) (cdr evil-robot))
							(setf closest-evil-robot (list (car evil-robot) (+ (cdr evil-robot) 1))))
						((< (cadr my-loc) (cdr evil-robot))
							(setf closest-evil-robot (list (car evil-robot) (- (cdr evil-robot) 1))))
						(t (setf closest-evil-robot (list (car evil-robot) (cdr evil-robot))))))) 
							(setf closest-evil-robot (list (car evil-robot) (cdr evil-robot)))))))) closest-evil-robot))
	
(defun count-distance (my-x-coords my-y-coords wt-x-coords wt-y-coords)
	(+ (abs (- my-x-coords wt-x-coords)) (abs (- my-y-coords wt-y-coords))))

(defun watch-my-steps (grid x-coords y-coords)
	(let ((step (aref grid x-coords y-coords)))
       		(if (or (null step) (identify-in-list #'percept-object-ball-p step)) t nil)))

(defun watch-my-steps2 (grid x-coords y-coords)
	(let ((step (aref grid x-coords y-coords)))
       		(if (identify-in-list #'percept-object-ball-p step) t nil)))
