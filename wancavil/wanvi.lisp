;; 22/04/2013
;; MI-FLP - semestralni projekt
;; Zapoctovy (nesoutezni) agent
;; Autor : wancavil (Vilibald Wanca)

(defun square (x) 
  (* x x)
  )

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
  (equal p q)
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

(defun wanvi-is-in-grid (p d)
  "Is point in grid given by dimension 1-dx 1-dy"
  (and (between (pt-x p) 1 (pt-x d))
       (between (pt-y p) 1 (pt-y d)))
  )

(defun wanvi-loc-closer (a1 a2)
  (if (< (car a1) (car a2))
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

(defmacro wanvi-grid-bounds (grid)
  `(mk-pt (- (pt-x (array-dimensions ,grid)) 2) (- (pt-y (array-dimensions ,grid)) 2)))

(defstructure (wanvi  
	       (:include db-agent 
			 (program 'wanvi-program)
			 (body (make-wanvi-body))
			 (name "wanvi")))
    "Your agent for db-world.")

(defstructure (wanvi-body (:include db-agent-body (name "wanvi"))))

(defun wanvi-program (percept)
  (let* ((me (car percept))
         (grid (cadr percept))
	 (my-loc (object-loc me))
	 (ball-loc (wanvi-find-ball grid))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid my-loc)))
         (holding-ball (object-contents me))
	 (evil-agents (wanvi-find-evil-agents grid my-loc)))
    (cond 
      ((not evil-agents) 'stop)
      (ball-on-my-loc 'grab-ball)
      (holding-ball `(throw-ball ,@(wanvi-where-to-throw grid my-loc evil-agents)))
      (t (wanvi-where-to-go grid my-loc ball-loc))))
  ) 

(defun wanvi-where-to-go (grid my-loc ball)
  "Decide where to go"
  (if (null ball)
      'stay
      (let ((path (wanvi-next-step grid my-loc ball)))
	(if (null path)
	    'stay
	    (wanvi-where my-loc path))))
  )

(defun wanvi-where (my-loc new-loc)
  "Transform coordinates to move command"
  (let ((u (pt-subtract my-loc new-loc)))
    (cond
      ((= (pt-x u) -1) 'go-right)
      ((= (pt-x u) 1) 'go-left)
      ((= (pt-y u) -1) 'go-up)
      ((= (pt-y u) 1) 'go-down)
      (t 'stay)))
  )


(defun wanvi-next-step (grid my-loc ball)
  "Find way to ball"
  (let ((options (list 
		  (wanvi-step-it grid (wanvi-step-up my-loc) ball) 
		  (wanvi-step-it grid (wanvi-step-right my-loc) ball) 
		  (wanvi-step-it grid (wanvi-step-down my-loc) ball) 
		  (wanvi-step-it grid (wanvi-step-left my-loc) ball))))
    (setf options (remove-if #'endp options))
    (when options
	(cdr (reduce #'wanvi-loc-closer options))))
  )

(defun wanvi-step-it (grid loc1 loc2 &key (dst #'pt-distance))
  "Weight next step"
  (if (wanvi-field-p grid loc1)
      (cons (funcall dst loc1 loc2) loc1)
      nil)
  )

(defun wanvi-throw-it (grid loc1 loc2 &key (dst #'pt-distance))
  "Weight the throw"
  (if (wanvi-field-p grid loc1)
      (cons (funcall dst loc1 loc2) loc1)
      (cons most-positive-fixnum loc1))
  )

(defun wanvi-field-p (grid loc)
  "Is the filed free to go or has a ball"
  (if (wanvi-is-in-grid loc (wanvi-grid-bounds grid))
      (let ((obj (aref grid (pt-x loc) (pt-y loc))))
	(if (or
	     (null obj)
	     (identify-in-list #'percept-object-ball-p obj))
	    T
	    nil))
      nil)
  )

(defun wanvi-where-to-throw (grid my-loc agents)
  "Decide where to throw, one before WT or at WT?"
  (let* ((loc (cdr (reduce #'wanvi-loc-closer agents)))
	 (loc2 (cdr (reduce #'wanvi-loc-closer (list 
						(wanvi-throw-it grid (wanvi-step-up loc) my-loc) 
						(wanvi-throw-it grid (wanvi-step-right loc) my-loc) 
						(wanvi-throw-it grid (wanvi-step-down loc) my-loc) 
						(wanvi-throw-it grid (wanvi-step-left loc) my-loc))))))
    (if (= 1 (pt-distance my-loc loc))
        loc
        loc2))
  )

(defun wanvi-find-ball (grid)
  "Find ball in grid"
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (identify-in-list #'percept-object-ball-p (aref grid numberx numbery))
	(return-from wanvi-find-ball (list numberx numbery))
	)))
  nil
  )

(defun wanvi-find-evil-agents (grid loc)
  "Find WTs in grid"
  (let ((agents nil))
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
	(if (identify-in-list #'wanvi-evil-agent-p (aref grid numberx numbery))
	    (setf agents (cons (cons (pt-distance loc (mk-pt numberx numbery)) (mk-pt numberx numbery)) agents))
	    )))
    agents)
  )

(defun wanvi-evil-agent-p ((obj percept-object))
  "Is object percepted a WT?"
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) "wanvi")) 
           (not (equal (percept-object-name obj) "B")))
      obj nil)
  )