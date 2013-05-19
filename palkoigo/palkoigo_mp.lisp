;;; ================================================================
;;; Student's agent definition:

(defconstant palkoigo-name "pal")

(defstructure (palkoigo
               (:include db-agent 
                 (body (make-palkoigo-body))
                 (program 'palkoigo-program)
                 (name "palkoigo")))
 "Igor Palkoci")

(defstructure (palkoigo-body (:include db-agent-body (name palkoigo-name) (sname palkoigo-name))))

(defstruct search-node
      			coord
      			prev
      			g-score
      			f-score)

(defun palkoigo-heuristic-cost (start goal)
  (if (and start goal)
    (+ (abs (- (car start) (car goal))) (abs (- (cadr start) (cadr goal))))))

(defun palkoigo-search-node-contains (coord set &optional condition)
  (cond ((null set) nil)
        ((and condition (equal coord (search-node-coord (car set))) (funcall condition (car set))) T)
        ((equal coord (search-node-coord (car set))) T)         
        (T (palkoigo-search-node-contains coord (cdr set)))))

(defun palkoigo-find-in-set (coord set property &optional condition)
  (cond ((null set) nil)
        ((and condition (equal coord (funcall property (car set))) (funcall condition (car set))) (car set))
        ((equal coord (funcall property (car set))) (car set))
        (T (palkoigo-find-in-set coord (cdr set) property))))

(defun palkoigo-neighbour-f-score (prev-node cell-coord goal)
  (+ (1+ (search-node-g-score prev-node)) (palkoigo-heuristic-cost cell-coord goal)))

(defun palkoigo-get-cell-object (cell-coord grid)
  (aref grid (car cell-coord) (cadr cell-coord)))

(defun palkoigo-contains-node-with-less-f-score (set cell-coord prev-node goal) 
  (palkoigo-search-node-contains cell-coord set
                                 #'(lambda (node) 
                                           (< (search-node-f-score node) 
                                              (palkoigo-neighbour-f-score prev-node cell-coord goal)))))

(defun palkoigo-process-neighbouring-cell (X-predicate prev-node cell-coord goal open-set closed-set grid palkoigo-my-name)
  (let ((cell (palkoigo-get-cell-object cell-coord grid)))
    (if (or (eq cell nil) 
            (palkoigo-identify-in-list X-predicate cell palkoigo-my-name))
  	        (cond ((palkoigo-contains-node-with-less-f-score closed-set cell-coord prev-node goal) nil)
              	  ((palkoigo-contains-node-with-less-f-score open-set cell-coord prev-node goal) nil)
              	  (T (make-search-node :coord cell-coord
                                       :prev (search-node-coord prev-node) 
                                       :g-score (1+ (search-node-g-score prev-node)) 
                                       :f-score (palkoigo-neighbour-f-score prev-node cell-coord goal)))))))	

(defun palkoigo-next-cell-index (add-coords current)
  (if (and add-coords current)
    (list (+ (car add-coords) (car (search-node-coord current)))
          (+ (cadr add-coords) (cadr (search-node-coord current))))))

(defun palkoigo-construct-path (closed-set last-item)
  (palkoigo-construct-path-internal closed-set nil last-item))

(defun palkoigo-construct-path-internal (closed-set acc last-item)
  (if last-item
      (palkoigo-construct-path-internal closed-set 
                                        (cons (search-node-coord last-item) acc) 
                                        (palkoigo-find-in-set (search-node-prev last-item) 
                                                              closed-set 
                                                              #'search-node-coord))
      acc))

(defun palkoigo-push-with-priority (set elem X-predicate)
  (cond ((not elem) nil)
        ((not set) (list elem))
        ((null set) elem)
        ((<= (funcall X-predicate elem) (funcall X-predicate (car set))) (cons elem set))
        (T (cons (car set) (palkoigo-push-with-priority (cdr set) elem X-predicate)))))

(defun palkoigo-A*-search (X-predicate start goal grid palkoigo-my-name)
  (let (current tmp closed-set (open-set (list 
                               (make-search-node :coord start 
                                                 :prev nil :g-score 0 
                                                 :f-score (palkoigo-heuristic-cost start goal)))));
  		(do () 
        	((or (and current (equal (search-node-coord current) goal)) (null open-set)) nil)
        	(setf current (car open-set))
        	(setf open-set (cdr open-set))
  				(dolist (i '((0 1) (0 -1) (1 0) (-1 0)))
         					(setf tmp (palkoigo-process-neighbouring-cell X-predicate current 
                                                       (palkoigo-next-cell-index i current) 
                                                       goal open-set closed-set grid palkoigo-my-name))
         					(if tmp (setf open-set (palkoigo-push-with-priority open-set tmp #'search-node-f-score))))
          (setf closed-set (cons current closed-set)))
  		(palkoigo-construct-path closed-set (palkoigo-find-in-set goal closed-set #'search-node-coord))))

(defun palkoigo-generate-next-step (curr-coord next-coord)
  (cond ((or (not curr-coord) (not next-coord)) 'stay)
        ((> (car next-coord) (car curr-coord)) 'go-right)
        ((< (car next-coord) (car curr-coord)) 'go-left)
        ((> (cadr next-coord) (cadr curr-coord)) 'go-up)
        ((< (cadr next-coord) (cadr curr-coord)) 'go-down)))

(defun palkoigo-find-nearest-X-location (X-predicate grid palkoigo-my-name source)
  (let (result)
    (palkoigo-find-nearest-X-location-internal X-predicate grid palkoigo-my-name source '(0 0) result)))
  
(defun palkoigo-find-nearest-X-location-internal (X-predicate grid palkoigo-my-name source current result)
  (cond ((>= (car current) (car (array-dimensions grid))) result)
        ((>= (cadr current) (cadr (array-dimensions grid))) 
         (palkoigo-find-nearest-X-location-internal X-predicate grid palkoigo-my-name source 
                                           (list (1+ (car current)) 0) result))
        ((and (palkoigo-identify-in-list X-predicate (palkoigo-get-cell-object current grid) palkoigo-my-name)
              (or (not result) (< (palkoigo-heuristic-cost source current) (palkoigo-heuristic-cost source result))))
         (palkoigo-find-nearest-X-location-internal X-predicate grid palkoigo-my-name source 
                                           (list (car current) (1+ (cadr current))) current))
        (T
         (palkoigo-find-nearest-X-location-internal X-predicate grid palkoigo-my-name source 
                                           (list (car current) (1+ (cadr current))) result))))
  
(defun palkoigo-find-nearest-adversary (source grid palkoigo-my-name)
  (palkoigo-find-nearest-X-location #'palkoigo-adversary-p grid palkoigo-my-name source))

(defmethod palkoigo-adversary-p ((obj percept-object) (palkoigo-my-name string))
  (if (and (not (equal (percept-object-name obj) "#"))
           (not (equal (percept-object-name obj) palkoigo-my-name))
           (not (equal (percept-object-name obj) "B")))
      obj nil))

(defmethod palkoigo-ball-p ((obj percept-object) (palkoigo-my-name string))
  (if (equal (percept-object-name obj) "B")
      obj nil))

(defun palkoigo-find-adversary-with-ball (grid palkoigo-my-name)
  (dotimes (x (car (array-dimensions grid)))
    (dotimes (y (cadr (array-dimensions grid)))
      (when (and (palkoigo-identify-in-list #'palkoigo-adversary-p (aref grid x y) palkoigo-my-name)
                 (palkoigo-identify-in-list #'(lambda (a n) (typep a 'percept-object-ball)) (aref grid x y) palkoigo-my-name))
            (return-from palkoigo-find-adversary-with-ball (list x y))))) nil)

(defun palkoigo-find-adversary-holding-ball (grid palkoigo-my-name)
  (palkoigo-find-X-location #'(lambda (e n) (and (funcall #'palkoigo-adversary-p e n) 
                                                    (percept-object-agent-has-ball e)))
                               grid palkoigo-my-name))

(defun palkoigo-find-X-location (X-predicate grid palkoigo-my-name)
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (palkoigo-identify-in-list X-predicate (aref grid numberx numbery) palkoigo-my-name)
        (return-from palkoigo-find-X-location (list numberx numbery))))) nil)

(defun palkoigo-identify-in-list (pred list palkoigo-my-name)
  (dolist (item list)
    (when (funcall pred item palkoigo-my-name)
      (return-from palkoigo-identify-in-list item))) nil)

(defun palkoigo-find-my-location (grid palkoigo-my-name)
  (palkoigo-find-X-location #'palkoigo-me-p grid palkoigo-my-name))

(defmethod palkoigo-me-p ((obj percept-object) (palkoigo-my-name string))
  (if (equal (percept-object-name obj) palkoigo-my-name) obj nil))

(defun palkoigo-program (percept)
  (let* ((me (first percept))
         (grid (second percept))
         (palkoigo-my-name (db-agent-body-name me))
         (my-location (palkoigo-find-my-location grid palkoigo-my-name))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
         (holding-ball (object-contents me)))
    (if my-location
       (cond (ball-on-my-loc 'grab-ball)
             (holding-ball 
               (palkoigo-process-holding-ball-state (palkoigo-find-nearest-adversary my-location grid palkoigo-my-name) 
                                                    my-location 
                                                    grid 
                                                    palkoigo-my-name))
             ((palkoigo-find-adversary-holding-ball grid palkoigo-my-name)
               (palkoigo-process-adversary-holding-ball-state (palkoigo-A*-search #'palkoigo-adversary-p 
                                                                                  my-location 
                                                                                  (palkoigo-find-adversary-holding-ball grid palkoigo-my-name) 
                                                                                  grid
                                                                                  palkoigo-my-name) 
                                                              grid 
                                                              palkoigo-my-name 
                                                              my-location))
             ((palkoigo-find-adversary-with-ball grid palkoigo-my-name) 
               (palkoigo-process-adversary-with-ball-state (palkoigo-A*-search #'palkoigo-adversary-p 
                                                                               my-location 
                                                                               (palkoigo-find-adversary-with-ball grid palkoigo-my-name) 
                                                                               grid 
                                                                               palkoigo-my-name) 
                                                            grid 
                                                            palkoigo-my-name
                                                            my-location))
             (T (palkoigo-process-ball-in-space-state my-location 
                                                      (palkoigo-A*-search #'palkoigo-ball-p 
                                                                          my-location 
                                                                          (find-ball-location grid) 
                                                                          grid palkoigo-my-name)))))))

(defun palkoigo-process-holding-ball-state (adversary my-location grid palkoigo-my-name)  
  (let ((path (palkoigo-A*-search #'palkoigo-adversary-p my-location adversary grid palkoigo-my-name)))
    (cond ((null path) 'stay)
          ((> (length path) 3) (append '(throw-ball) (nth (- (length path) 3) path)))
          (T (append '(throw-ball) adversary)))))

(defun palkoigo-process-adversary-holding-ball-state (path grid palkoigo-my-name my-location)
  (if (or (null path) (> (length path) 2)) 'stay
      (palkoigo-generate-next-step my-location (nth 1 path))))

(defun palkoigo-process-adversary-with-ball-state (path grid palkoigo-my-name my-location)
  (if (and (not (null path)) (> (length path) 2))
      (palkoigo-generate-next-step my-location (nth 1 path)) 
      'stay))

(defun palkoigo-process-ball-in-space-state (my-location path)
  (if (null path) 'stay (palkoigo-generate-next-step my-location (nth 1 path))))
