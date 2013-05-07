(defstructure (brazdma1
               (:include db-agent 
                         (body (make-brazdma1-body))
                         (program 'brazdma1-program)
                         (name "brazdma1"))) 
    "Your agent for db-world.")

(defstructure (brazdma1-body (:include db-agent-body (name "^.^")))
    (history NIL))

(defun brazdma1-A*-pathfinding (grid from to &optional &key (dist 'brazdma1-manhattan-distance) (objective NIL))
  (when (or (null grid) (null from) (null to)) (return-from brazdma1-A*-pathfinding NIL))
  (labels ((move (from delta) (let ((move (mapcar #'+ from delta))) (when (validate-move move) move)))
           (check (x max) (and (< x (- max 1)) (>= x 1)))
           (validate-move (move) (and (every #'identity (mapcar #'check move (array-dimensions grid)))
                                      (or (not (aref grid (first move) (second move)))
                                          (equal move to))))
           (child (parent delta) (let* ((coord (move (first parent) delta))) (when coord (list coord (+ (second parent) 1) (funcall dist coord to) (first parent)))))
           (children (parent) (delete NIL (mapcar #'child (make-list 4 :initial-element parent) (list '(0 1) '(1 0) '(0 -1) '(-1 0)))))
           (f-value (node) (+ (second node) (third node))))
    (let* ((start (list from 0 (funcall dist from to) NIL))
           (opened (list start))
           (closed NIL))
      (loop do
            (unless opened (return-from brazdma1-A*-pathfinding NIL)) 
            (let ((node (find (reduce #'min opened :key #'f-value) opened :key #'f-value)))
              (when (equal (first node) to)
                (labels ((path (node) (if node (append (path (find (fourth node) (stable-sort (sort (copy-list closed) (brazdma1-hazard-comparator agents) :key #'first) #'< :key #'f-value) :test #'equal :key #'first)) (list (first node))) NIL)))
                  (return-from brazdma1-A*-pathfinding (path node))))
              (setf opened (remove node opened :test #'equal))
              (push node closed)
              (loop for child in (children node)
                  do (let* ((inOpened (find (first child) opened :test #'equal :key #'first))
                            (inClosed (find (first child) closed :test #'equal :key #'first)))
                       (cond ((and inClosed (< (second child) (second inClosed)))
                              (substitute child inClosed closed :test #'equal))
                             ((and inOpened (< (second child) (second inOpened)))
                              (substitute child inOpened opened :test #'equal))
                             ((and (null inClosed) (null inOpened))
                              (push child opened))))))))))

(defun brazdma1-print-map (array)
  (write-char #\linefeed)
  (loop for i from 0 below (array-dimension array 0)
        do (loop for j from 0 below (array-dimension array 1)
                 do (princ (aref array i j))
                    (if (= j (1- (array-dimension array 1)))
                        (terpri)
                      (princ #\Space)))))

(defun brazdma1-probe (grid dX dY objective &optional (x 1) (y 1) (ret NIL))
  (when (equal (type-of (car (aref grid x y))) objective)
    (setf ret (append ret (list (list x y)))))
  (when (not (equal (cdr (aref grid x y)) NIL))
    (when (equal (type-of (car (cdr (aref grid x y)))) objective)
      (setf ret (append ret (list (list x y))))))
  (cond ((< y (- dY 2)) (return-from brazdma1-probe (brazdma1-probe grid dX dY objective x (+ y 1) ret)))
        ((< x (- dX 2)) (return-from brazdma1-probe (brazdma1-probe grid dX dY objective (+ x 1) 1 ret))))
  ret)

(defun brazdma1-euclidean-distance (from to)
  (let ((a (- (first from) (first to)))
        (b (- (second from) (second to))))
    (sqrt (+ (* a a) (* b b)))))

(defun brazdma1-aim-nearest (attacker agents &optional (target (car agents)) &key (measure 'brazdma1-euclidean-distance))
  (if (null (car agents))
      target
    (progn
      (when (and (not (equal attacker (car agents)))
                 (< (funcall measure attacker (car agents))
                    (funcall measure attacker target)))
        (setf target (car agents)))
      (brazdma1-aim-nearest attacker (cdr agents) target))))

;;;(defmacro distance-from (from)
;;;  '(lambda (x y) (if (< (brazdma1-euclidean-distance x from) (brazdma1-euclidean-distance y from)) T NIL)))

(defun brazdma1-k-nearest-neighbours (from agents &optional (k 1)  &key (measure 'brazdma1-euclidean-distance))
  (subseq (sort (copy-list agents) (lambda (x y) (if (< (funcall measure x from) (funcall measure y from)) T NIL))) 0 k))

(defun brazdma1-sum-distances (from targets &optional (sum 0) &key (measure 'brazdma1-euclidean-distance))
  (if (null (car targets))
      sum
    (brazdma1-sum-distances from (cdr targets) (+ sum (funcall measure from (car targets))))))

(defun brazdma1-weigthing (from targets)
  (let ((sum (brazdma1-sum-distances from targets)))
    (if (not (= sum 0))
        (+ (list-length targets) (/ 1 (/ sum (list-length targets))))
      0)))

(defun brazdma1-enemies-of (target agents &optional (res NIL) (all-agents agents))
  (if (null (car agents))
      res
    (progn
      (when (equal target (brazdma1-aim-nearest (car agents) (append (list target) all-agents)))
        (setf res (append res (list (car agents)))))
      (brazdma1-enemies-of target (cdr agents) res all-agents))))

(defun brazdma1-hazard (target agents)
  (brazdma1-weigthing target (brazdma1-enemies-of target agents)))

(defun brazdma1-path-hazard (path agents &optional (step-counter 1))
  (if path
      (let ((hazard (/ (brazdma1-hazard (car path) agents) step-counter)))
        (+ hazard (brazdma1-path-hazard (cdr path) agents (1+ step-counter))))
    0))

(defun brazdma1-direction->position (from direction)
  (cond ((eq direction 'UP) (list (- (first from) 1) (second from)))
        ((eq direction 'DOWN) (list (+ (first from) 1) (second from)))
        ((eq direction 'RIGHT) (list (first from) (+ (second from) 1))) #polluting
        ((eq direction 'LEFT) (list (first from) (- (second from) 1)))))

(defun brazdma1-check (grid moves &optional (objective NIL) (move NIL))
  (when (null (car moves))
    (return-from brazdma1-check NIL))
  (let ((x (first (car moves)))
        (y (second (car moves))))
    (when (or (equal (type-of (car (aref grid x y))) 'NULL)
              (equal (type-of (car (aref grid x y))) objective))
      (return-from brazdma1-check (car moves))))
  (brazdma1-check grid (cdr moves) objective move))

(defun brazdma1-sneak (grid from to agents &optional (objective NIL))
  (when (or (equal from NIL) (equal to NIL) (equal agents NIL))
    (return-from brazdma1-sneak NIL))
  (labels ((move (from delta) (let ((move (mapcar #'+ from delta))) (when (validate-move move) move)))
           (check (x max) (and (< x (- max 1)) (>= x 1)))
           (validate-move (move) (and (every #'identity (mapcar #'check move (array-dimensions grid)))
                                      (or (not (aref grid (first move) (second move)))
                                          (equal move to))))
           (move-hazard (xpath) (brazdma1-path-hazard xpath agents))
           (xpath (from) (brazdma1-A*-pathfinding grid from to))
           (next-move (delta) (let* ((coord (move from delta)) (xpath (xpath coord))) (when (and coord xpath) (list coord (list-length xpath) (move-hazard xpath)))))
           (all-moves () (delete NIL (mapcar #'next-move (list '(0 1) '(1 0) '(0 -1) '(-1 0))))))
    (let* ((possible-moves (all-moves))
          (next-move (first (first (stable-sort (sort possible-moves #'< :key #'third) #'< :key #'second)))))
      (brazdma1-position->command from (brazdma1-check grid (list next-move) objective)))))
  
(defmacro brazdma1-hazard-comparator (agents)
  '(lambda (choice1 choice2) (if (< (brazdma1-hazard choice1 agents) (brazdma1-hazard choice2 agents)) T NIL)))

(defun brazdma1-stealth (grid from agents &optional (objective NIL))
  (let ((ch1 from)
        (ch2 (brazdma1-direction->position from 'UP))
        (ch3 (brazdma1-direction->position from 'DOWN))
        (ch4 (brazdma1-direction->position from 'LEFT))
        (ch5 (brazdma1-direction->position from 'RIGHT)))
    (setf move (brazdma1-check grid (sort (list ch1 ch2 ch3 ch4 ch5) (brazdma1-hazard-comparator agents)) objective))
    (brazdma1-position->command from move)))

(defun brazdma1-accuracy (attacker target)
  (- 100 (* (brazdma1-euclidean-distance attacker target) 10)))
 
(defun brazdma1-lives (grid agent)
  (let ((lives (percept-object-agent-lives (car (aref grid (first agent) (second agent))))))
    (if (equal lives NIL)
        NIL
      lives)))

(defun brazdma1-find-agents (grid dX dY my-loc)
  (let ((agents-including-me (brazdma1-probe grid dX dY 'PERCEPT-OBJECT-AGENT)))
    (set-difference agents-including-me (list my-loc) :test #'equal)))

(defun brazdma1-ball-on-square (square)
  (if (equal (percept-object-name (car square)) "B")
      (return-from brazdma1-ball-on-square T)
    (when (not (equal (percept-object-agent-has-ball (car square)) NIL))
      (return-from brazdma1-ball-on-square T)))
  (if (not (equal (cdr square) NIL))
      (brazdma1-ball-on-square (cdr square))
    NIL))

(defun brazdma1-find-ball (grid dX dY &optional (x 1) (y 1) (ret NIL))
  (let ((square (aref grid x y)))
    (when (and (not (equal square NIL))
               (or (equal (type-of (car square)) 'PERCEPT-OBJECT-AGENT)
                   (equal (type-of (car square)) 'PERCEPT-OBJECT-BALL)))
      (unless (equal (brazdma1-ball-on-square square) NIL)
        (return-from brazdma1-find-ball (list x y ))))
    (cond ((< y (- dY 2)) (return-from brazdma1-find-ball (brazdma1-find-ball grid dX dY x (+ y 1) ret)))
          ((< x (- dX 2)) (return-from brazdma1-find-ball (brazdma1-find-ball grid dX dY (+ x 1) 1 ret))))))

(defun brazdma1-position->command (my-loc position)
  (if (equal position NIL)
      'WAIT
    (cond ((equal position (list (- (first my-loc) 1) (second my-loc))) 'GO-LEFT)
          ((equal position (list (+ (first my-loc) 1) (second my-loc))) 'GO-RIGHT)
          ((equal position (list (first my-loc) (+ (second my-loc) 1))) 'GO-UP)
          ((equal position (list (first my-loc) (- (second my-loc) 1))) 'GO-DOWN))))

(defun brazdma1-get-ball (grid dX dY my-loc agents)
  (let* ((ball (brazdma1-find-ball grid dX dY))
         (move (brazdma1-sneak grid my-loc ball agents 'PERCEPT-OBJECT-BALL)))
    (if (equal ball my-loc)
        'GRAB-BALL
      move)))

(defun brazdma1-threat (grid dX dY agents my-loc target)
  (if (equal target NIL)
      0
    (let* ((distance (brazdma1-euclidean-distance my-loc target))
           (hazard (brazdma1-hazard target agents))
           (agents-including-me (brazdma1-probe grid dX dY 'PERCEPT-OBJECT-AGENT))
           (agent-count (list-length agents-including-me))
           (k-nearest (brazdma1-k-nearest-neighbours target agents-including-me 3)))
      (if (equal NIL (intersection k-nearest (list my-loc) :test #'equal))
          hazard
        0))))

(defun brazdma1-at-sight (from to)
  (let ((fireline (go-through-dist-list from to)))
    (when (not (equal fireline NIL))
      (if (equal (caar (last fireline)) to)
          T
        NIL))))

(defun brazdma1-detonator (grid dX dY agents my-loc)
  (setf ret '(0 0))
  (loop for x from 1 below (- dX 1)
      do (loop for y from 1 below (- dY 1)
             do (when (and (> (brazdma1-threat grid dX dY agents my-loc (list x y))
                              (brazdma1-threat grid dX dY agents my-loc ret))
                           (brazdma1-at-sight my-loc ret))
                  (setf ret (list x y)))))
  ret)

(defun brazdma1-deflect (grid dX dY agents my-loc)
  (let ((target (brazdma1-detonator grid dX dY agents my-loc)))
    (if (equal target '(0 0))
        (return-from brazdma1-deflect NIL)
      `(THROW-BALL ,(first target) ,(second target)))))

(defun brazdma1-opportunity (grid dX dY my-loc)
  (let* ((ball (brazdma1-find-ball grid dX dY))
         (agents-including-me (brazdma1-probe grid dX dY 'PERCEPT-OBJECT-AGENT))
         (two-nearest (brazdma1-k-nearest-neighbours ball agents-including-me 2 :measure 'brazdma1-manhattan-distance)))
    (when (equal my-loc ball)
      (return-from brazdma1-opportunity T))
    (if (and (equal my-loc (first two-nearest))
             (<= (brazdma1-manhattan-distance (first two-nearest) ball)
                 (brazdma1-manhattan-distance (second two-nearest) ball)))
        T
      NIL)))

(defun brazdma1-direct-threat (grid dX dY agents my-loc)
  (let* ((ball (brazdma1-find-ball grid dX dY))
         (agents-including-me (brazdma1-probe grid dX dY 'PERCEPT-OBJECT-AGENT))
         (potential-attacker (first (brazdma1-k-nearest-neighbours ball agents-including-me 1 :measure 'brazdma1-manhattan-distance))))
    (if (equal potential-attacker my-loc)
        NIL
      (let* ((agents-without-attacker (set-difference agents-including-me (list potential-attacker) :test #'equal))
            (prey (brazdma1-aim-nearest potential-attacker agents-without-attacker)))
        (if (or (equal prey my-loc)
                (< (brazdma1-euclidean-distance my-loc potential-attacker) 6))
            T
          NIL)))))

(defun brazdma1-retreat (grid from agents attacker &optional (objective NIL))
  (let ((ch1 from)
        (ch2 (brazdma1-direction->position from 'UP))
        (ch3 (brazdma1-direction->position from 'DOWN))
        (ch4 (brazdma1-direction->position from 'LEFT))
        (ch5 (brazdma1-direction->position from 'RIGHT)))
    (brazdma1-position->command from (brazdma1-check grid (sort (list ch1 ch2 ch3 ch4 ch5) (lambda (x y) (if (> (brazdma1-euclidean-distance x attacker) (brazdma1-euclidean-distance y attacker)) T NIL))) objective))))

(defun brazdma1-logistics (grid dX dY my-loc)
  (let ((x (first my-loc))
        (y (second my-loc))
        (maxX (- dX (/ dX 4)))
        (minX (/ dX 4))
        (maxY (- dY (/ dY 4)))
        (minY (/ dY 4)))
    (if (or (< x minX) (> x maxX) (< y minY) (> y maxY))
        T
      NIL)))

(defun brazdma1-relocation (grid dX dY agents my-loc)
  (brazdma1-sneak grid my-loc (list (round (/ dX 2)) (round (/ dY 2))) agents))

(defun brazdma1-safe-zone (grid dX dY agents my-loc &optional (x 1) (y 1) (ret NIL))
  (let* ((agents-including-me (brazdma1-probe grid dX dY 'PERCEPT-OBJECT-AGENT))
         (two-nearest (brazdma1-k-nearest-neighbours (list x y) agents-including-me 2 :measure 'brazdma1-manhattan-distance)))
    (when (and (equal (first two-nearest) my-loc)
               (< (+ 2 (brazdma1-manhattan-distance (first two-nearest) (list x y)))
                  (brazdma1-manhattan-distance (second two-nearest) (list x y))))
      (setf ret (append ret (list (list x y)))))
    (cond ((< y (- dY 2)) (return-from brazdma1-safe-zone (brazdma1-safe-zone grid dX dY agents my-loc x (+ y 1) ret)))
          ((< x (- dX 2)) (return-from brazdma1-safe-zone (brazdma1-safe-zone grid dX dY agents my-loc (+ x 1) 1 ret))))
    ret))

(defun brazdma1-manhattan-distance (from to)
  (+ (abs (- (first from) (first to)))
     (abs (- (second from) (second to)))))

(defun brazdma1-retribution (grid dX dY avenger my-loc)
  (let* ((agents-including-me (brazdma1-probe grid dX dY 'PERCEPT-OBJECT-AGENT))
         (enemy0 (brazdma1-aim-nearest avenger agents-including-me))
         (enemy1 (brazdma1-aim-nearest (brazdma1-direction->position avenger 'UP) agents-including-me))
         (enemy2 (brazdma1-aim-nearest (brazdma1-direction->position avenger 'DOWN) agents-including-me))
         (enemy3 (brazdma1-aim-nearest (brazdma1-direction->position avenger 'LEFT) agents-including-me))
         (enemy4 (brazdma1-aim-nearest (brazdma1-direction->position avenger 'RIGHT) agents-including-me)))
    (if (or (equal enemy0 my-loc)
            (equal enemy1 my-loc)
            (equal enemy2 my-loc)
            (equal enemy3 my-loc)
            (equal enemy4 my-loc))
        T
      NIL)))

(defun brazdma1-all-paralyzed (history targets)
  (if (equal (list-length targets) (list-length (member-if #'= targets :key #'(lambda (x) (aref history (first x) (second x))))))
      T
    NIL))
    

(defun brazdma1-offensive (grid dX dY agents my-loc safe-zone)
  (let ((target (brazdma1-aim-nearest my-loc agents)))
    (when (<= (brazdma1-euclidean-distance my-loc target) 5)
      (when (brazdma1-at-sight my-loc target)
        (when (and (> (list-length agents) 2)
                   (not (brazdma1-retribution grid dX dY target my-loc)))
          (return-from brazdma1-offensive `(THROW-BALL ,(first target) ,(second target))))
        (when (and (<= (list-length agents) 2)
                   (<= (brazdma1-euclidean-distance my-loc target) 4))
          (return-from brazdma1-offensive `(THROW-BALL ,(first target) ,(second target))))))
    (when (> (list-length agents) 2)
        (labels ((find-prey (agent) (brazdma1-retribution grid dX dY agent my-loc)))
          (let ((prey (nth (position NIL (mapcar #'identity (mapcar #'find-prey agents))) agents)))
            (setf maneuver (first (sort safe-zone (lambda (x y) (if (< (brazdma1-euclidean-distance x prey) (brazdma1-euclidean-distance y prey)) T NIL)))))
            (if maneuver
                (return-from brazdma1-offensive `(THROW-BALL ,(first maneuver) ,(second maneuver))))
              (brazdma1-deflect grid dX dY agents my-loc))))
    (setf maneuver (first (sort safe-zone (lambda (x y) (if (< (brazdma1-euclidean-distance x target) (brazdma1-euclidean-distance y target)) T NIL)))))
    (return-from brazdma1-offensive `(THROW-BALL ,(first maneuver) ,(second maneuver)))))

(defun brazdma1-ball-state (square)
  (if (equal (percept-object-name (car square)) "B")
      (return-from brazdma1-ball-state 'ON-GROUND)
    (when (not (equal (percept-object-agent-has-ball (car square)) NIL))
      (return-from brazdma1-ball-state 'IN-POSSESION)))
  (if (not (equal (cdr square) NIL))
      (brazdma1-ball-state (cdr square))
    NIL))

(defun brazdma1-attack-countdown (grid ball attacker)
  (let ((ball-state (brazdma1-ball-state (aref grid (first ball) (second ball)))))
    (cond ((equal ball-state 'ON-GROUND) (+ 1 (brazdma1-manhattan-distance attacker ball)))
          ((equal ball-state 'IN-POSSESION) 0))))

(defun brazdma1-counter (grid dX dY agents my-loc)
  (let* ((ball (brazdma1-find-ball grid dX dY))
         (attacker (first (brazdma1-k-nearest-neighbours ball agents 1 :measure 'brazdma1-manhattan-distance)))
         (time-left (brazdma1-attack-countdown grid ball attacker))
         (distance (brazdma1-manhattan-distance my-loc ball)))
    (if (and (>= time-left distance)
             (not (equal my-loc attacker)))
        T
      NIL)))

(defun brazdma1-strike (grid from to agents)
  (when (or (equal from NIL)
            (equal to NIL)
            (equal agents NIL))
    (return-from brazdma1-strike 'WAIT))
  (let ((choice1 (list (- (first from) (signum (- (first from) (first to)))) (second from)))
        (choice2 (list (first from) (- (second from) (signum (- (second from) (second to)))))))
    (if (not (or (equal choice1 from) (equal choice2 from)))
        (if (random 2)
            (brazdma1-position->command from choice2)
          (brazdma1-position->command from choice1))
      (if (equal choice1 from)
          (brazdma1-position->command from choice2)
        (brazdma1-position->command from choice1)))))

(defun brazdma1-tactics (grid dX dY agents my-loc my-ball)
  ; (print "TACTICS")
  (when (not (equal my-ball NIL))
    (let ((safe-zone (remove my-loc (brazdma1-safe-zone grid dX dY agents my-loc) :test #'equal)))
      (progn
        ; (print "OFFENSIVE")
        (return-from brazdma1-tactics (brazdma1-offensive grid dX dY agents my-loc safe-zone)))))  
  (when (brazdma1-direct-threat grid dX dY agents my-loc)
    (let* ((agents-including-me (brazdma1-probe grid dX dY 'PERCEPT-OBJECT-AGENT))
           (ball (brazdma1-find-ball grid dX dY))
           (potential-attacker (first (brazdma1-k-nearest-neighbours ball agents 1 :measure 'brazdma1-manhattan-distance))))
      (if (brazdma1-counter grid dX dY agents my-loc)
          (progn
            ; (print "STRIKE")
            (return-from brazdma1-tactics (brazdma1-strike grid my-loc ball agents)))
        (progn
          ; (print "RETREAT")
          (return-from brazdma1-tactics (brazdma1-retreat grid my-loc agents potential-attacker))))))
  (when (brazdma1-opportunity grid dX dY my-loc)
    ; (print "GET-BALL")
    (return-from brazdma1-tactics (brazdma1-get-ball grid dX dY my-loc agents)))
  ; (print "STEALTH")
  (return-from brazdma1-tactics (brazdma1-stealth grid my-loc agents)))

(defun brazdma1-training-safe-zone (grid target)
  (let ((sz1 (brazdma1-direction->position target 'UP))
        (sz2 (brazdma1-direction->position target 'DOWN))
        (sz3 (brazdma1-direction->position target 'LEFT))
        (sz4 (brazdma1-direction->position target 'RIGHT)))
    (list sz1 sz2 sz3 sz4)))

(defun brazdma1-training-offense (grid dX dY agents my-loc)
  (let* ((target (brazdma1-aim-nearest my-loc agents)))
    (if (equal (brazdma1-manhattan-distance my-loc target) 1)
        `(THROW-BALL ,(first target) ,(second target))
      (let* ((safezone (brazdma1-training-safe-zone grid target))
             (pass (first (sort (copy-list safezone) (lambda (x y) (if (< (brazdma1-euclidean-distance x my-loc) (brazdma1-euclidean-distance y my-loc)) T NIL))))))
        `(THROW-BALL ,(first pass) ,(second pass))))))

(defun brazdma1-training (grid dX dY agents my-loc my-ball)
  (print "TRAINING")
  (when (not (equal my-ball NIL))
    (return-from brazdma1-training (brazdma1-training-offense grid dX dY agents my-loc)))
  (let ((target (brazdma1-aim-nearest my-loc agents))
        (ball (brazdma1-find-ball grid dX dY)))
    (when (equal target ball)
      (return-from brazdma1-training (brazdma1-strike grid my-loc ball agents))))
  (return-from brazdma1-training (brazdma1-get-ball grid dX dY my-loc agents)))

(defun brazdma1-init-array (array dX dY)
  (loop for x from 1 below (- dX 1)
      do (loop for y from 1 below (- dY 1)
             do (setf (aref array x y) 0))))
    
(defun brazdma1-archive (history dX dY agents)
  (loop for x from 1 below (- dX 1)
      do (loop for y from 1 below (- dY 1)
             do (if (member (list x y) agents :test #'equal)
                    (setf (aref history x y) (+ (aref history x y) 1))
                  (setf (aref history x y) 0)))))

(defun brazdma1-program (percept)
  (let* ((me (car percept))
         (my-loc (object-loc me))
         (my-ball (object-contents me))
         (grid (cadr percept)))
    (setf dX (array-dimension grid 0))                 
    (setf dY (array-dimension grid 1))
    (when (equal (get 'history 'me) NIL)
      (setf (get 'history 'me) (make-array (list dX dY)))
      (brazdma1-init-array (get 'history 'me) dX dY))
    (brazdma1-archive (get 'history 'me) dX dY (brazdma1-probe grid dX dY 'PERCEPT-OBJECT-AGENT))
    (setf agents (brazdma1-find-agents grid dX dY my-loc))
    (setf ball (brazdma1-find-ball grid dX dY))
    (brazdma1-tactics grid dX dY agents my-loc my-ball)))
