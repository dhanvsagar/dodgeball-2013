;; 22/04/2013
;; MI-FLP - semestralni projekt
;; Soutezni agent pro dodgeball
;; Autor : wancavil (Vilibald Wanca)


(defconstant wvi-agent-name "wvi")

(defun wvi!sign (n)
  (* n -1))

(defun wvi-between (x y z)
  "iff number x is between numbers y and z."
  (or (<= y x z) (>= y x z)))

(defstruct (wpt (:type list)) "point." x y)

(defun wpt-p (arg) 
  (and (consp arg) (= (length arg) 2) (every #'numberp arg))
  )

(defun mk-wpt (x y) 
  (make-wpt :x x :y y)
  )

(defun wpt-equal (p q)
  (and (= (wpt-x p) (wpt-x q)) (= (wpt-y p) (wpt-y q)))
  )

(defun wpt-add (p q)
  "add two points"
  (mk-wpt (+ (wpt-x p) (wpt-x q)) (+ (wpt-y p) (wpt-y q)))
  )

(defun wpt-subtract (p q)
  "subtract two pointse"
  (mk-wpt (- (wpt-x p) (wpt-x q)) (- (wpt-y p) (wpt-y q)))
  )

(defun wpt-distance (p q)
  "Euclid distance"
  (sqrt (+ (square (- (wpt-x p) (wpt-x q)))
	   (square (- (wpt-y p) (wpt-y q)))))
  )

(defun wpt-m-distance (p q)
  "Manthan distance"
  (+ (abs (- (wpt-x p) (wpt-x q)))
     (abs (- (wpt-y p) (wpt-y q))))
  )

(defun wpt-make-in (p d)
  "Make point to fit in grid given by dimension 1-dx 1-dy"
  (let ((x (wpt-x p))
	(y (wpt-y p)))
    (if (> (wpt-x p) (wpt-x d))
	(setf x (wpt-x d)))
    (if (> (wpt-y p) (wpt-y d))
	(setf y (wpt-y d)))
    (if (< (wpt-x p) 1)
	(setf x 1))
    (if (< (wpt-y p) 1)
	(setf y 1))
    (mk-wpt x y))
  )

(defun wvi-is-in-grid (p d)
  "Is point in grid given by dimension 1-dx 1-dy"
  (and (wvi-between (wpt-x p) 1 (wpt-x d))
       (wvi-between (wpt-y p) 1 (wpt-y d)))
  )

(defun wvi-< (a1 a2)
  (if (< (car a1) (car a2))
      a1 
      a2)
  )

(defun wvi-> (a1 a2)
  (if (> (car a1) (car a2))
      a1 
      a2)
  )

(defun wvi-same-op (a1 a2)
  (and (= (car a1) (car a2))
       (= (cadr a1) (cadr a2))
       (= (caddr a1) (caddr a2)))
  )

(defun wvi-make-circular (l)
  (setf (cdr (last l)) l))

(defun wvi-permute (l)
  (when l
    (loop
       :with len = (length l)
       :with choices = (wvi-make-circular (copy-list l))
       :collect (pop (cdr (nthcdr (random len) choices)))
       :while (plusp (decf len))))
)

(defun wvi-opponent-p ((obj percept-object))
  "Know thy enemy."
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) wvi-agent-name)) 
           (not (equal (percept-object-name obj) "B")))
      obj nil)
  )

(defun wvi-has-ball-p ((obj percept-object))
  "Has the player ball?"
  (and (wvi-opponent-p obj) (percept-object-agent-has-ball obj)))

(defun wvi-identify-in-list (pred list)
  (dolist (item list)
    (when (funcall pred item)
      (return-from wvi-identify-in-list item))) nil)


(defmacro wvi-step-up (loc)
  `(wpt-add ,loc (mk-wpt 0 1)))

(defmacro wvi-step-right (loc)
  `(wpt-add ,loc (mk-wpt 1 0)))

(defmacro wvi-step-down (loc)
  `(wpt-add ,loc (mk-wpt 0 -1)))

(defmacro wvi-step-left (loc)
  `(wpt-add ,loc (mk-wpt -1 0)))

(defmacro wvi-pt+offset (loc x y)
  `(wpt-add ,loc (mk-wpt ,x ,y)))

(defmacro wvi-pt-offset (loc x y)
  `(wpt-subtract ,loc (mk-wpt ,x ,y)))

(defmacro wvi-grid-bounds (grid)
  `(mk-wpt (- (wpt-x (array-dimensions ,grid)) 2) (- (wpt-y (array-dimensions ,grid)) 2)))


(defstructure (wancavil
	       (:include db-agent 
			 (program 'wvi-program)
			 (body (make-wvi-body))
			 (name wvi-agent-name)))
    "Your agent for db-world.")

(defstructure (wvi-body 
	       (:include db-agent-body (name wvi-agent-name)))
    (last-loc (mk-wpt 0 0))
    (last-grid nil)
)

(defun wvi-program (percept)
  (let* ((me (car percept))
         (grid (cadr percept))
	 (my-loc (object-loc me))
	 (ball-loc (wvi-find-in-grid grid))
	 (last-pos (wvi-body-last-loc me))
	 (last-grid (wvi-body-last-grid me))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid my-loc)))
         (holding-ball (object-contents me)))
    
    (setf (wvi-body-last-loc me) my-loc)
    (setf (wvi-body-last-grid me) grid)
    (cond 
      ((not (wvi-find-in-grid grid :test #'wvi-opponent-p)) 'stop)
      (ball-on-my-loc 'grab-ball)
      (holding-ball `(throw-ball ,@(wvi-where-to-throw grid my-loc)))
      ((wvi-has-anybody-moved grid last-grid my-loc ball-loc) (wvi-go-for-ball grid my-loc ball-loc nil)) 
      (t (wvi-where-to-go grid my-loc ball-loc))))
  )

(defun wvi-has-anybody-moved (grid last-grid loc ball-loc)
  "Are we playing or we're stuck?"
  (if (null ball-loc)
      (return-from wvi-has-anybody-moved nil))
  (if (null last-grid)
      nil
      (let* ((new-ops (wvi-find-list-in-grid grid loc))
	     (old-ops (wvi-find-list-in-grid last-grid loc))
	     (dif-ops (set-difference new-ops old-ops :test #'wvi-same-op)))
	(if (null dif-ops)
	    t
	    nil))
      )
)

(defun wvi-where-to-go (grid my-loc ball-loc)
  "Am I close enough to the ball to go for it or better back off?"
  (if (null ball-loc)
      (let ((ag-ball (wvi-find-in-grid grid :test #'wvi-has-ball-p)))
	(if (< (wpt-distance my-loc ag-ball) (1+ *CAN-HIT-DIST*))
	    (return-from wvi-where-to-go (wvi-back-off grid my-loc ag-ball))
	    (return-from wvi-where-to-go 'stay))))

  (let* ((opponent (reduce #'wvi-< (wvi-find-list-in-grid grid ball-loc)))
	 (my-m-distance (wpt-m-distance my-loc ball-loc))
	 (my-distance (wpt-distance my-loc ball-loc)))
    (cond 
      ((< my-m-distance (+ (car opponent) 2)) (wvi-go-for-ball grid my-loc ball-loc opponent))
      ((< my-distance (1+ *CAN-HIT-DIST*)) (wvi-back-off grid my-loc ball-loc))
      (t (wvi-come-closer grid my-loc ball-loc))))
  )

(defun wvi-come-closer (grid my-loc ball-loc)
  "It's safe to get closer to the ball"
  (let ((path (remove-if #'endp (list 
				 (wvi-step-it grid (wvi-step-up my-loc) ball-loc) 
				 (wvi-step-it grid (wvi-step-right my-loc) ball-loc) 
				 (wvi-step-it grid (wvi-step-down my-loc) ball-loc) 
				 (wvi-step-it grid (wvi-step-left my-loc) ball-loc)))))
    (when (null path)
      (return-from wvi-come-closer 'stay)) 
    (setf path  (cdr (reduce #'wvi-< path)))
    (if (< (wpt-distance path ball-loc) (1+ *CAN-HIT-DIST*))
	'stay
	(wvi-where my-loc path)))
  )

(defun wvi-go-for-ball (grid my-loc ball-loc opponent)
  "Gwaan bwoy!"
  (let ((my-path (wvi-next-step grid my-loc ball-loc (list my-loc)))
	(op-path (wvi-next-step grid (cdr opponent) ball-loc (list (cdr opponent)))))
    (cond
      ((null my-path) 'stay)
      ((null op-path) (wvi-where my-loc (car my-path)))
      ((< (length my-path) (+ (length op-path) 2)) (wvi-where my-loc (car my-path)))
      ((= 1 (wpt-distance my-loc (cdr opponent))) (wvi-where my-loc
							     (wvi-one-step grid my-loc ball-loc t :cmp #'wvi-<)))
      (t (wvi-back-off grid my-loc ball-loc))))
  )

(defun wvi-back-off (grid my-loc ball-loc)
  "Run away it not safe here"
  (let* ((safe-place (wvi-one-step grid my-loc ball-loc))
	 (path (wvi-next-step grid my-loc safe-place (list my-loc))))
    (if (null path)
	'stay
	(wvi-where my-loc (car path))))
  )

(defun wvi-one-step (grid my-loc ball-loc &optional (ignore nil) &key (cmp #'wvi->))
  "Where is it safer to go?"
  (let ((options (list 
		  (wvi-step-it grid (wvi-step-up my-loc) ball-loc ignore) 
		  (wvi-step-it grid (wvi-step-right my-loc) ball-loc ignore) 
		  (wvi-step-it grid (wvi-step-down my-loc) ball-loc ignore) 
		  (wvi-step-it grid (wvi-step-left my-loc) ball-loc ignore)
		  (cons (wpt-distance my-loc ball-loc) my-loc))))
    (cdr (reduce cmp (wvi-permute (remove-if #'endp options)))))
  )

(defun wvi-where (my-loc new-loc)
  "Just transform coordinates to move directions."
  (let ((u (wpt-subtract my-loc new-loc)))
    (cond
      ((= (wpt-x u) -1) 'go-right)
      ((= (wpt-x u) 1) 'go-left)
      ((= (wpt-y u) -1) 'go-up)
      ((= (wpt-y u) 1) 'go-down)
      (t 'stay)))
  )

(defun wvi-next-step (grid my-loc dst-loc prev-loc)
  "Find the way to your destiny."
  (if (null my-loc)
      (return-from wvi-next-step nil))

  (let ((options (list 
		  (wvi-step-it grid (wvi-step-up my-loc) dst-loc) 
		  (wvi-step-it grid (wvi-step-right my-loc) dst-loc) 
		  (wvi-step-it grid (wvi-step-down my-loc) dst-loc) 
		  (wvi-step-it grid (wvi-step-left my-loc) dst-loc))))
    (setf options (wvi-permute (remove-if #'endp options)))
    (if (null options)
	(return-from wvi-next-step nil))
    (if (wpt-equal my-loc dst-loc)
	(return-from wvi-next-step nil))
    (if (> (length prev-loc) (+ (wpt-m-distance (car (last prev-loc)) dst-loc) 3))
	(return-from wvi-next-step nil))
    (sort options #'< :key #'car)
    (dolist (n options)
      (block iter
	(cond 
	  ((= 0 (car n)) (return-from wvi-next-step (list (cdr n))))
	  ((find (cdr n) prev-loc :test #'wpt-equal) (return-from iter nil))
	  (t (block deeper (let ((path (wvi-next-step grid (cdr n) dst-loc (cons my-loc prev-loc))))
			     (cond
			       ((null path) (return-from deeper nil))
			       ((wpt-equal dst-loc (car path)) (return-from wvi-next-step (cons (cdr n) path)))
			       ((wpt-equal dst-loc (car (last path))) (return-from wvi-next-step (cons (cdr n) path)))
			       (t  (return-from deeper nil)))))))))
    nil)
  )

(defun wvi-step-it (grid loc1 loc2 &optional (ignore nil) &key (dst #'wpt-distance))
  "How many steps to go?"
  (if (wvi-field-p grid loc1 ignore)
      (cons (funcall dst loc1 loc2) loc1)
      nil)
  )

(defun wvi-field-p (grid loc &optional (ignore nil))
  "Watch your steps mate."
  (if (wvi-is-in-grid loc (wvi-grid-bounds grid))
      (if ignore
	  t
      (let ((obj (aref grid (wpt-x loc) (wpt-y loc))))
	(if (or
	     (null obj)
	     (wvi-identify-in-list #'percept-object-ball-p obj))
	    t
	    nil)))
      nil)
  )

(defun wvi-danger-zone (grid ball-loc hit-dist)
  "Nobody is safe within these bounds."
  (let ((places nil)
	(danger-zone nil))
    (dotimes (x hit-dist)
      (setf places (cons (wvi-pt+offset ball-loc x hit-dist) places))
      (setf places (cons (wvi-pt+offset ball-loc x (wvi!sign hit-dist)) places))
      (setf places (cons (wvi-pt-offset ball-loc x hit-dist) places))
      (setf places (cons (wvi-pt-offset ball-loc x (wvi!sign hit-dist)) places))
      (setf places (cons (wvi-pt+offset ball-loc  hit-dist x) places))
      (setf places (cons (wvi-pt+offset ball-loc (wvi!sign hit-dist) x) places))
      (setf places (cons (wvi-pt-offset ball-loc hit-dist x)  places))
      (setf places (cons (wvi-pt-offset ball-loc (wvi!sign hit-dist) x) places)))

    (dolist (n places)
      (if (wvi-is-in-grid n (wvi-grid-bounds grid))
	  (setf danger-zone (cons n danger-zone))
 	  (setf danger-zone(cons (wpt-make-in  n (wvi-grid-bounds grid)) danger-zone))))
    (remove-duplicates danger-zone :test #'wpt-equal))
  )

(defun wvi-where-to-throw (grid my-loc)
  "Be clever and throw wisely in the right direction."
  (let* ((agents (wvi-find-list-in-grid grid my-loc :dst #'wpt-distance)) 
	 (p-targets (remove-if #'endp (mapcar #'(lambda (a)(if (< (car a) *CAN-HIT-DIST*) a)) agents))) 
	 (sure-zone (wvi-danger-zone grid my-loc 2))
    	 (danger-zone (wvi-danger-zone grid my-loc 5))
	 (targets nil)
	 (got-it nil))

    (when (null (car p-targets))
      (let* ((closest (cdr (reduce #'wvi-< agents)))
	     (options (list 
		       (wvi-step-it grid (wvi-step-up my-loc) closest) 
		       (wvi-step-it grid (wvi-step-right my-loc) closest)
		       (wvi-step-it grid (wvi-step-down my-loc) closest) 
		       (wvi-step-it grid (wvi-step-left my-loc) closest))))
	(return-from wvi-where-to-throw (cdr (reduce #'wvi-< (remove-if #'endp options))))))

    (sort agents #'< :key #'car)
    (dolist (n sure-zone)
      (let ((hit (intersection (mapcar #'cdr agents) (go-through-dist-list my-loc n) :test #'(lambda (p q) (wpt-equal p (car q))))))
	(unless (null hit)
	  (return-from wvi-where-to-throw (car hit)))))

    (dolist (n danger-zone)
      (let ((hit (intersection (mapcar #'cdr agents) (go-through-dist-list my-loc n) :test #'(lambda (p q) (wpt-equal p (car q))))))
	(unless (null hit)
	  (setf targets (cons (cons (length hit) (cons n hit)) targets)))))

    (unless (null targets)
      (progn
	(setf got-it (reduce #'wvi-> targets))
	(if (= 1 (car got-it))
	    (return-from wvi-where-to-throw (cdar agents)))
	(return-from wvi-where-to-throw (cadr got-it))))

    (let* ((closest (cdr (reduce #'wvi-< agents)))
	   (options (list 
		     (wvi-step-it grid (wvi-step-up my-loc) closest) 
		     (wvi-step-it grid (wvi-step-right my-loc) closest)
		     (wvi-step-it grid (wvi-step-down my-loc) closest) 
		     (wvi-step-it grid (wvi-step-left my-loc) closest))))
      (cdr (reduce #'wvi-< (remove-if #'endp options)))))
  )

(defun wvi-find-in-grid (grid &key (test #'percept-object-ball-p))
  "Find stuff in grid."
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (wvi-identify-in-list test (aref grid numberx numbery))
	(return-from wvi-find-in-grid (list numberx numbery))
	)))
  nil
  )

(defun wvi-find-list-in-grid (grid loc &key (dst #'wpt-m-distance) (test #'wvi-opponent-p))
  "Find stuff in grid and how far is it."
  (let ((agents nil))
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
	(if (wvi-identify-in-list test (aref grid numberx numbery))
	    (setf agents (cons (cons 
				(funcall dst loc (mk-wpt numberx numbery))
				(mk-wpt numberx numbery)) 
			       agents))
	    )))
    agents)
  )

