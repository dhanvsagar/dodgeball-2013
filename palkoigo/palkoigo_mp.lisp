;;; ================================================================
;;; Student's agent definition:

; This is to be defined when designing a new student agent 

(defconstant palkoigo-name "palkoigo")

(defstructure (palkoigo
               (:include db-agent 
                 (body (make-palkoigo-body))
                 (program 'palkoigo-program)
                 (name palkoigo-name)                 
                 ))
 "Your agent for db-world.")

(defstructure (palkoigo-body (:include db-agent-body (name palkoigo-name))))

(defstruct search-node
      			coord
      			prev
      			g-score
      			f-score)

(defun heuristic-cost (start goal)
  (if (and start goal)
    (+ (abs (- (car start) (car goal))) (abs (- (cadr start) (cadr goal))))))

(defun search-node-contains (coord set &optional condition)
  (cond ((null set) nil)
        ((and condition (equal coord (search-node-coord (car set))) (funcall condition (car set))) T)
        ((equal coord (search-node-coord (car set))) T)         
        (T (search-node-contains coord (cdr set)))))

(defun find-in-set (coord set property &optional condition)
  (cond ((null set) nil)
        ((and condition (equal coord (funcall property (car set))) (funcall condition (car set))) (car set))
        ((equal coord (funcall property (car set))) (car set))
        (T (find-in-set coord (cdr set) property))))

(defun neighbour-f-score (prev-node cell-coord goal)
  (+ (1+ (search-node-g-score prev-node)) (heuristic-cost cell-coord goal)))

(defun get-cell-object (cell-coord grid)
  (aref grid (car cell-coord) (cadr cell-coord)))

(defun contains-node-with-less-f-score (set cell-coord prev-node goal) 
  (search-node-contains cell-coord set
                        #'(lambda (node) 
                                  (< (search-node-f-score node) 
                                     (neighbour-f-score prev-node cell-coord goal)))))

(defun process-neighbouring-cell (X-predicate prev-node cell-coord goal open-set closed-set grid my-name)
  (let ((cell (get-cell-object cell-coord grid)))
    (if (or (eq cell nil) 
            (alternative-identify-in-list X-predicate cell my-name))
  	        (cond ((contains-node-with-less-f-score closed-set cell-coord prev-node goal) nil)
              	  ((contains-node-with-less-f-score open-set cell-coord prev-node goal) nil)
              	  (T (make-search-node :coord cell-coord 
                                       :prev (search-node-coord prev-node) :g-score (1+ (search-node-g-score prev-node)) 
                                       :f-score (neighbour-f-score prev-node cell-coord goal)))))))	

(defun next-cell-index (add-coords current)
  (if (and add-coords current)
    (list (+ (car add-coords) (car (search-node-coord current)))
          (+ (cadr add-coords) (cadr (search-node-coord current))))))

(defun construct-path (closed-set last-item)
  (if last-item
      (cons (search-node-coord last-item)
            (construct-path closed-set 
                            (find-in-set (search-node-prev last-item) 
                                                           closed-set 
                                                           #'search-node-coord)))))

(defun my-A*-search (X-predicate start goal grid my-name)
  (let (current tmp closed-set (open-set (list 
                               (make-search-node :coord start 
                                                 :prev nil :g-score 0 
                                                 :f-score (heuristic-cost start goal)))));
  		(do () 
        	((or (and current (equal (search-node-coord current) goal)) (null open-set)) nil)
        	(sort open-set #'< :key #'search-node-f-score)
        	(setf current (car open-set))
        	(setf open-set (cdr open-set))
  				(dolist (i '((0 1) (0 -1) (1 0) (-1 0)))
         					(setf tmp (process-neighbouring-cell X-predicate current 
                                                       (next-cell-index i current) 
                                                       goal open-set closed-set grid my-name))
         					(if tmp (setf open-set (cons tmp open-set))))
          (setf closed-set (cons current closed-set)))
  		(reverse (construct-path closed-set (find-in-set goal closed-set #'search-node-coord)))))

(defun generate-next-step (curr-coord next-coord)
  (cond ((or (not curr-coord) (not next-coord)) 'stay)
        ((> (car next-coord) (car curr-coord)) 'go-right)
        ((< (car next-coord) (car curr-coord)) 'go-left)
        ((> (cadr next-coord) (cadr curr-coord)) 'go-up)
        ((< (cadr next-coord) (cadr curr-coord)) 'go-down)))

(defun find-nearest-X-location (X-predicate grid my-name source)
  (let (result)
    (find-nearest-X-location-internal X-predicate grid my-name source '(0 0) result)))
  
(defun find-nearest-X-location-internal (X-predicate grid my-name source current result)
  (cond ((>= (car current) (array-dimension grid 0)) result)
        ((>= (cadr current) (array-dimension grid 1)) 
         (find-nearest-X-location-internal X-predicate grid my-name source 
                                           (list (1+ (car current)) 0) result))
        ((and (alternative-identify-in-list X-predicate (get-cell-object current grid) my-name)
              (or (not result) (< (heuristic-cost source current) (heuristic-cost source result))))
         (find-nearest-X-location-internal X-predicate grid my-name source 
                                           (list (car current) (1+ (cadr current))) current))
        (T
         (find-nearest-X-location-internal X-predicate grid my-name source 
                                           (list (car current) (1+ (cadr current))) result))))
  
(defun find-nearest-adversary (source grid my-name)
  (find-nearest-X-location #'adversary-p grid my-name source))

(defmethod adversary-p ((obj percept-object) (my-name string))
  (if (and (not (equal (percept-object-name obj) "#"))
           (not (equal (percept-object-name obj) my-name))
           (not (equal (percept-object-name obj) "B")))
      obj nil))

(defmethod alternative-my-ball-p ((obj percept-object) (my-name string))
  (if (equal (percept-object-name obj) "B")
      obj nil))

(defun find-adversary-with-ball (grid my-name)
  (dotimes (x (array-dimension grid 0))
    (dotimes (y (array-dimension grid 1))
      (when (and (alternative-identify-in-list #'adversary-p (aref grid x y) my-name)
                 (alternative-identify-in-list #'(lambda (a n) (typep a 'percept-object-ball)) (aref grid x y) my-name))
            (return-from find-adversary-with-ball (list x y))))) nil)

(defun find-adversary-holding-ball (grid my-name)
  (alternative-find-X-location #'(lambda (e n) (and (funcall #'adversary-p e n) 
                                                    (percept-object-agent-has-ball e)))
                               grid my-name))

(defun alternative-find-X-location (X-predicate grid my-name)
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (alternative-identify-in-list X-predicate (aref grid numberx numbery) my-name)
        (return-from alternative-find-X-location (list numberx numbery))))) nil)

(defun alternative-identify-in-list (pred list my-name)
  (dolist (item list)
    (when (funcall pred item my-name)
      (return-from alternative-identify-in-list item))) nil)

(defun find-my-location (grid my-name)
  (alternative-find-X-location #'palkoigo-me-p grid my-name))

(defmethod palkoigo-me-p ((obj percept-object) (my-name string))
  (if (equal (percept-object-name obj) my-name) obj nil))

(defun palkoigo-program (percept)
  (universal-program percept))
  
(defun universal-program (percept)  
  (let* ((me (car percept))
         (grid (cadr percept))
         (my-name (db-agent-body-name me))
         (my-location (find-my-location grid my-name))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
         (holding-ball (object-contents me)))
    
       (cond (ball-on-my-loc 'grab-ball)
             (holding-ball 
               (process-holding-ball-state (find-nearest-adversary my-location grid my-name) my-location grid my-name))
             ((find-adversary-holding-ball grid my-name) 
              (process-adversary-holding-ball-state (my-A*-search #'adversary-p my-location (find-adversary-holding-ball grid my-name) grid my-name) grid my-name my-location))
             ((find-adversary-with-ball grid my-name) 
              (process-adversary-with-ball-state (my-A*-search #'adversary-p my-location (find-adversary-with-ball grid my-name) grid my-name) 
                                                 grid my-name my-location))
             (T (process-ball-in-space-state my-location 
                                       (my-A*-search #'alternative-my-ball-p my-location (find-ball-location grid) grid my-name))))))

(defun process-holding-ball-state (adversary my-location grid my-name)  
  (let ((path (my-A*-search #'adversary-p my-location adversary grid my-name)))
    (cond ((null path) 'stay)
          ((> (length path) 3) (append '(throw-ball) (nth (- (length path) 3) path)))
          (T (append '(throw-ball) adversary)))))

(defun process-adversary-holding-ball-state (path grid my-name my-location)
  (if (or (null path) (> (length path) 2)) 'stay
      (generate-next-step my-location (nth 1 path))))

(defun process-adversary-with-ball-state (path grid my-name my-location)
  (if (and (not (null path)) (> (length path) 2))
      (generate-next-step my-location (nth 1 path)) 
      'stay))

(defun process-ball-in-space-state (my-location path)
  (if (null path) 'stay (generate-next-step my-location (nth 1 path))))
