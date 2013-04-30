;; 22/04/2013
;; MI-FLP - semestralni projekt
;; Soutezni agent pro dodgeball
;; Autor : wancavil (Vilibald Wanca)


(defconstant wanvi-agent-name "wv")

(defun square (x) 
  (* x x)
  )

(defun !sign (n)
  (* n -1))

(defun wanvi-between (x y z)
  "iff number x is between numbers y and z."
  (or (<= y x z) (>= y x z)))

(defstruct (pt (:type list)) "point." x y)

(defun pt-p (arg) 
  (and (consp arg) (= (length arg) 2) (every #'numberp arg))
  )

(defun mk-pt (x y) 
  (make-pt :x x :y y)
  )

(defun pt-equal (p q)
  (and (= (pt-x p) (pt-x q)) (= (pt-y p) (pt-y q)))
  )

(defun pt-add (p q)
  "add two points"
  (mk-pt (+ (pt-x p) (pt-x q)) (+ (pt-y p) (pt-y q)))
  )

(defun pt-subtract (p q)
  "subtract two pointse"
  (mk-pt (- (pt-x p) (pt-x q)) (- (pt-y p) (pt-y q)))
  )

(defun pt-distance (p q)
  "Euclid distance"
  (sqrt (+ (square (- (pt-x p) (pt-x q)))
	   (square (- (pt-y p) (pt-y q)))))
  )

(defun pt-m-distance (p q)
  "Manthan distance"
  (+ (abs (- (pt-x p) (pt-x q)))
     (abs (- (pt-y p) (pt-y q))))
  )

(defun pt-make-in (p d)
  "Make point to fit in grid given by dimension 1-dx 1-dy"
  (let ((x (pt-x p))
	(y (pt-y p)))
    (if (> (pt-x p) (pt-x d))
	(setf x (pt-x d)))
    (if (> (pt-y p) (pt-y d))
	(setf y (pt-y d)))
    (if (< (pt-x p) 1)
	(setf x 1))
    (if (< (pt-y p) 1)
	(setf y 1))
    (mk-pt x y))
  )

(defun wanvi-is-in-grid (p d)
  "Is point in grid given by dimension 1-dx 1-dy"
  (and (between (pt-x p) 1 (pt-x d))
       (between (pt-y p) 1 (pt-y d)))
  )

(defun wanvi-< (a1 a2)
  (if (< (car a1) (car a2))
      a1 
      a2)
  )

(defun wanvi-> (a1 a2)
  (if (> (car a1) (car a2))
      a1 
      a2)
  )

(defmacro wanvi-step-up (loc)
  `(pt-add ,loc (mk-pt 0 1)))

(defmacro wanvi-step-right (loc)
  `(pt-add ,loc (mk-pt 1 0)))

(defmacro wanvi-step-down (loc)
  `(pt-add ,loc (mk-pt 0 -1)))

(defmacro wanvi-step-left (loc)
  `(pt-add ,loc (mk-pt -1 0)))

(defmacro wanvi-pt+offset (loc x y)
  `(pt-add ,loc (mk-pt ,x ,y)))

(defmacro wanvi-pt-offset (loc x y)
  `(pt-subtract ,loc (mk-pt ,x ,y)))

(defmacro wanvi-grid-bounds (grid)
  `(mk-pt (- (pt-x (array-dimensions ,grid)) 2) (- (pt-y (array-dimensions ,grid)) 2)))


(defstructure (wanvi  
	       (:include db-agent 
			 (program 'wanvi-program)
			 (body (make-wanvi-body))
			 (name wanvi-agent-name)))
    "Your agent for db-world.")

(defstructure (wanvi-body (:include db-agent-body (name wanvi-agent-name))))

(defun wanvi-program (percept)
  (let* ((me (car percept))
         (grid (cadr percept))
	 (my-loc (object-loc me))
	 (ball-loc (wanvi-find-in-grid grid))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid my-loc)))
         (holding-ball (object-contents me)))
    (cond 
      ((not (wanvi-find-in-grid grid :test #'wanvi-opponent-p)) 'stop)
      (ball-on-my-loc 'grab-ball)
      (holding-ball `(throw-ball ,@(wanvi-where-to-throw grid my-loc)))
      (t (wanvi-where-to-go grid my-loc ball-loc))))
  )

(defun wanvi-where-to-go (grid my-loc ball-loc)
  "Am I close enough to the ball to go for it or better back off?"
  (if (null ball-loc)
      (let ((ag-ball (wanvi-find-in-grid grid :test #'wanvi-has-ball-p)))
	(if (< (pt-distance my-loc ag-ball) (1+ *CAN-HIT-DIST*))
	    (return-from wanvi-where-to-go (wanvi-back-off grid my-loc ag-ball))
	    (return-from wanvi-where-to-go 'stay))))

  (let* ((opponent (reduce #'wanvi-< (wanvi-find-list-in-grid grid ball-loc)))
	 (my-m-distance (pt-m-distance my-loc ball-loc))
	 (my-distance (pt-distance my-loc ball-loc)))
    (cond 
      ((< my-m-distance (+ (car opponent) 2)) (wanvi-go-for-ball grid my-loc ball-loc opponent))
      ((< my-distance (1+ *CAN-HIT-DIST*)) (wanvi-back-off grid my-loc ball-loc))
      (t (wanvi-come-closer grid my-loc ball-loc))))
  )

(defun wanvi-come-closer (grid my-loc ball-loc)
  "It's safe to get closer to the ball"
  (let ((path (remove-if #'endp (list 
				 (wanvi-step-it grid (wanvi-step-up my-loc) ball-loc) 
				 (wanvi-step-it grid (wanvi-step-right my-loc) ball-loc) 
				 (wanvi-step-it grid (wanvi-step-down my-loc) ball-loc) 
				 (wanvi-step-it grid (wanvi-step-left my-loc) ball-loc)))))
    (when (null path)
      (return-from wanvi-come-closer 'stay)) 
    (setf path  (cdr (reduce #'wanvi-< path)))
    (if (< (pt-distance path ball-loc) (1+ *CAN-HIT-DIST*))
	'stay
	(wanvi-where my-loc path)))
  )

(defun wanvi-go-for-ball (grid my-loc ball-loc opponent)
  "Gwaan bwoy!"
  (let ((my-path (wanvi-next-step grid my-loc ball-loc (list my-loc)))
	(op-path (wanvi-next-step grid (cdr opponent) ball-loc (list (cdr opponent)))))
    (cond
      ((null my-path) 'stay)
      ((null op-path) (wanvi-where my-loc (car my-path)))
      ((< (length my-path) (+ (length op-path) 2)) (wanvi-where my-loc (car my-path)))
      (t (wanvi-back-off grid my-loc ball-loc))))
  )

(defun wanvi-back-off (grid my-loc ball-loc)
  "Run away it not safe here"
  (let* ((safe-place (wanvi-step-back grid my-loc ball-loc))
	 (path (wanvi-next-step grid my-loc safe-place (list my-loc))))
    (if (null path)
	'stay
	(wanvi-where my-loc (car path))))
  )

(defun wanvi-step-back (grid my-loc ball-loc)
  "Where is it safer to go?"
  (let ((options (list 
		  (wanvi-step-it grid (wanvi-step-up my-loc) ball-loc) 
		  (wanvi-step-it grid (wanvi-step-right my-loc) ball-loc) 
		  (wanvi-step-it grid (wanvi-step-down my-loc) ball-loc) 
		  (wanvi-step-it grid (wanvi-step-left my-loc) ball-loc)
		  (cons (pt-distance my-loc ball-loc) my-loc))))
    (cdr (reduce #'wanvi-> (remove-if #'endp options))))
  )

(defun wanvi-where (my-loc new-loc)
  "Just transform coordinates to move directions."
  (let ((u (pt-subtract my-loc new-loc)))
    (cond
      ((= (pt-x u) -1) 'go-right)
      ((= (pt-x u) 1) 'go-left)
      ((= (pt-y u) -1) 'go-up)
      ((= (pt-y u) 1) 'go-down)
      (t 'stay)))
  )

(defun wanvi-next-step (grid my-loc dst-loc prev-loc)
  "Find the way to your destiny."
  (let ((options (list 
		  (wanvi-step-it grid (wanvi-step-up my-loc) dst-loc) 
		  (wanvi-step-it grid (wanvi-step-right my-loc) dst-loc) 
		  (wanvi-step-it grid (wanvi-step-down my-loc) dst-loc) 
		  (wanvi-step-it grid (wanvi-step-left my-loc) dst-loc))))
    (setf options (remove-if #'endp options))
    (if (null options)
	(return-from wanvi-next-step nil))
    (if (pt-equal my-loc dst-loc)
	(return-from wanvi-next-step nil))
    (if (> (length prev-loc) (+ (pt-m-distance (car (last prev-loc)) dst-loc) 3))
	(return-from wanvi-next-step nil))
    (sort options #'< :key #'car)
    (dolist (n options)
      (block iter
	(cond 
	  ((= 0 (car n)) (return-from wanvi-next-step (list (cdr n))))
	  ((find (cdr n) prev-loc :test #'pt-equal) (return-from iter nil))
	  (t (block deeper (let ((path (wanvi-next-step grid (cdr n) dst-loc (cons my-loc prev-loc))))
			     (cond
			       ((null path) (return-from deeper nil))
			       ((pt-equal dst-loc (car path)) (return-from wanvi-next-step (cons (cdr n) path)))
			       ((pt-equal dst-loc (car (last path))) (return-from wanvi-next-step (cons (cdr n) path)))
			       (t  (return-from deeper nil)))))))))
    nil)
  )

(defun wanvi-step-it (grid loc1 loc2 &key (dst #'pt-distance))
  "How many steps to go?"
  (if (wanvi-field-p grid loc1)
      (cons (funcall dst loc1 loc2) loc1)
      nil)
  )

(defun wanvi-field-p (grid loc)
  "Watch your steps mate."
  (if (wanvi-is-in-grid loc (wanvi-grid-bounds grid))
      (let ((obj (aref grid (pt-x loc) (pt-y loc))))
	(if (or
	     (null obj)
	     (identify-in-list #'percept-object-ball-p obj))
	    T
	    nil))
      nil)
  )

(defun wanvi-danger-zone (grid ball-loc hit-dist)
  "Nobody is safe within these bounds."
  (let ((places nil)
	(danger-zone nil))
    (dotimes (x hit-dist)
      (setf places (cons (wanvi-pt+offset ball-loc x hit-dist) places))
      (setf places (cons (wanvi-pt+offset ball-loc x (!sign hit-dist)) places))
      (setf places (cons (wanvi-pt-offset ball-loc x hit-dist) places))
      (setf places (cons (wanvi-pt-offset ball-loc x (!sign hit-dist)) places))
      (setf places (cons (wanvi-pt+offset ball-loc  hit-dist x) places))
      (setf places (cons (wanvi-pt+offset ball-loc (!sign hit-dist) x) places))
      (setf places (cons (wanvi-pt-offset ball-loc hit-dist x)  places))
      (setf places (cons (wanvi-pt-offset ball-loc (!sign hit-dist) x) places)))

    (dolist (n places)
      (if (wanvi-is-in-grid n (wanvi-grid-bounds grid))
	  (setf danger-zone (cons n danger-zone))
 	  (setf danger-zone(cons (pt-make-in  n (wanvi-grid-bounds grid)) danger-zone))))
    (remove-duplicates danger-zone :test #'pt-equal))
  )

(defun wanvi-where-to-throw (grid my-loc)
  "Be clever and throw wisely in the right direction."
  (let* ((agents (wanvi-find-list-in-grid grid my-loc :dst #'pt-distance)) 
	 (p-targets (remove-if #'endp (mapcar #'(lambda (a)(if (< (car a) *CAN-HIT-DIST*) a)) agents))) 
	 (sure-zone (wanvi-danger-zone grid my-loc 2))
    	 (danger-zone (wanvi-danger-zone grid my-loc 5))
	 (targets nil)
	 (got-it nil))

    (when (null (car p-targets))
      (let* ((closest (cdr (reduce #'wanvi-< agents)))
	     (options (list 
		       (wanvi-step-it grid (wanvi-step-up my-loc) closest) 
		       (wanvi-step-it grid (wanvi-step-right my-loc) closest)
		       (wanvi-step-it grid (wanvi-step-down my-loc) closest) 
		       (wanvi-step-it grid (wanvi-step-left my-loc) closest))))
	(return-from wanvi-where-to-throw (cdr (reduce #'wanvi-< (remove-if #'endp options))))))

    (sort agents #'< :key #'car)
    (dolist (n sure-zone)
      (let ((hit (intersection (mapcar #'cdr agents) (go-through-dist-list my-loc n) :test #'(lambda (p q) (pt-equal p (car q))))))
	(unless (null hit)
	  (return-from wanvi-where-to-throw (car hit)))))

    (dolist (n danger-zone)
      (let ((hit (intersection (mapcar #'cdr agents) (go-through-dist-list my-loc n) :test #'(lambda (p q) (pt-equal p (car q))))))
	(unless (null hit)
	  (setf targets (cons (cons (length hit) (cons n hit)) targets)))))

    (unless (null targets)
      (progn
	(setf got-it (reduce #'wanvi-> targets))
	(if (= 1 (car got-it))
	    (return-from wanvi-where-to-throw (cdar agents)))
	(return-from wanvi-where-to-throw (cadr got-it))))

    (let* ((closest (cdr (reduce #'wanvi-< agents)))
	   (options (list 
		     (wanvi-step-it grid (wanvi-step-up my-loc) closest) 
		     (wanvi-step-it grid (wanvi-step-right my-loc) closest)
		     (wanvi-step-it grid (wanvi-step-down my-loc) closest) 
		     (wanvi-step-it grid (wanvi-step-left my-loc) closest))))
      (cdr (reduce #'wanvi-< (remove-if #'endp options)))))
  )

(defun wanvi-find-in-grid (grid &key (test #'percept-object-ball-p))
  "Find stuff in grid."
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (identify-in-list test (aref grid numberx numbery))
	(return-from wanvi-find-in-grid (list numberx numbery))
	)))
  nil
  )

(defun wanvi-find-list-in-grid (grid loc &key (dst #'pt-m-distance) (test #'wanvi-opponent-p))
  "Find stuff in grid and how far is it."
  (let ((agents nil))
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
	(if (identify-in-list test (aref grid numberx numbery))
	    (setf agents (cons (cons 
				(funcall dst loc (mk-pt numberx numbery))
				(mk-pt numberx numbery)) 
			       agents))
	    )))
    agents)
  )

(defun wanvi-opponent-p ((obj percept-object))
  "Know thy enemy."
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) wanvi-agent-name)) 
           (not (equal (percept-object-name obj) "B")))
      obj nil)
  )

(defun wanvi-has-ball-p ((obj percept-object))
  "Has the player ball?"
  (and (wanvi-opponent-p obj) (percept-object-agent-has-ball obj)))

