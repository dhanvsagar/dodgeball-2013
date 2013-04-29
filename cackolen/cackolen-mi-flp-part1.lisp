;;; MI-FLP: CAST 1
;;; Lenka Cackova 2013

;;; cackolen's agent definition:

(defconstant cackolen-name "LC")

(defstructure (cackolen
                (:include db-agent 
                  (body (make-cackolen-body))
                  (program 'cackolen-go-and-throw-at-statue)
                  (name cackolen-name)))
  "Your agent for db-world.")

(defstructure (cackolen-body (:include db-agent-body (name cackolen-name) (sname "LCAC"))))


(defun cackolen-go-and-throw-at-statue (percept)
  "goes to get ball, throw it in front of nearest agent or at him if standing next to him"
  (let* ((me (car percept))
         (grid (cadr percept))
         (my-loc (cackolen-my-location grid))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
         (ball-loc (find-ball-location grid))
         (holding-ball (object-contents me))
         (opponents-loc (cackolen-list-all-opponents grid))
         ;'(format t "~&my location: ~A, ball location: ~A, opponents: ~A" my-loc ball-loc opponents-loc)
         (nearest-opponent (cackolen-find-nearest-opponent my-loc opponents-loc))
         ;'(format t "~&nearest: ~A" nearest-opponent)
        )
    (cond 
     ( (not opponents-loc) 'stop )
     ( ball-on-my-loc 'grab-ball )
     ( holding-ball (cackolen-where-to-throw my-loc nearest-opponent) )
     ;; not holding ball, go and get it, also takes care of bumps
     ( t (cackolen-take-step my-loc (cackolen-calculate-next-step grid my-loc ball-loc)) ))))

(defun cackolen-where-to-throw (my-loc nearest-opponent)
  "evaluates if to throw at agent or in front of him"
  (setf dist (cadr nearest-opponent))
  (cond 
        ( (= dist 1) (return-from cackolen-where-to-throw `(throw-ball ,@(car nearest-opponent))) ) 
        ( t (return-from cackolen-where-to-throw (cackolen-throw-in-front my-loc (car nearest-opponent))) )
  ))

(defun cackolen-throw-in-front (my-loc nearest-opponent)
  "evaluates nearest field next to target"
  (cond ( (> (car my-loc) (car nearest-opponent)) 
            (return-from cackolen-throw-in-front `(throw-ball ,(+ (car nearest-opponent) 1) ,(cadr nearest-opponent))) ) 
        ( (< (car my-loc) (car nearest-opponent)) 
            (return-from cackolen-throw-in-front `(throw-ball ,(- (car nearest-opponent) 1) ,(cadr nearest-opponent))) )
        ( (> (cadr my-loc) (cadr nearest-opponent)) 
            (return-from cackolen-throw-in-front `(throw-ball ,(car nearest-opponent) ,(+ (cadr nearest-opponent) 1))) )
        ( (< (cadr my-loc) (cadr nearest-opponent)) 
          (return-from cackolen-throw-in-front `(throw-ball ,(car nearest-opponent) ,(- (cadr nearest-opponent) 1))) )
    )
)

(defun cackolen-find-nearest-opponent (my-loc opponents-loc)
  "returns position of nearest opponent from opponent list"
  (let* ( (mindist (cackolen-manhattan-dist my-loc (car opponents-loc)))
          (nearest-loc (car opponents-loc))
        ;'(format t "~&min distance: ~A, first opponent in list: ~A" mindist nearest-loc) 
        )
        (dolist (tmp opponents-loc)
          (setf tmpdist (cackolen-manhattan-dist my-loc tmp)) 
          ;(format t "~&item checked ~A, distance to it ~A" tmp tmpdist)
          (when (< tmpdist mindist) (setf mindist tmpdist) (setf nearest-loc tmp) 
            ;'(format t "~&is less, MIN: ~A, NEAREST ~A" mindist nearest-loc)
          ) 
        ) (return-from cackolen-find-nearest-opponent (list nearest-loc mindist))))     

(defun cackolen-manhattan-dist (point-from point-to)
  "returns manhattan distance between two points"
   (+ (abs (- (car point-from) (car point-to))) (abs (- (cadr point-from) (cadr point-to))) ))

(defun cackolen-take-step (my-loc next-step)
  (cond 
     ( (> (car my-loc) (car next-step)) (return-from cackolen-take-step 'go-left) )
     ( (< (car my-loc) (car next-step)) (return-from cackolen-take-step 'go-right) )
     ( (< (cadr my-loc) (cadr next-step)) (return-from cackolen-take-step 'go-up) )
     ( (> (cadr my-loc) (cadr next-step)) (return-from cackolen-take-step 'go-down) )
     (t 'stay)
  )
)

(defun cackolen-calculate-next-step (grid start end )
  "return next step to destination using BFS search"
  ;(format t "~&from: ~A, to: ~A" start end)
  (let ( (open (list start)) (closed) (path (make-array (array-dimensions grid))) )
    ;(format t "~&open: ~A, closed: ~A, path: ~A" open closed path)
    (loop
      (when (null open) (return-from cackolen-calculate-next-step (cackolen-backtrack-path-array path start end)))
      (let* ( (node (car open)) 
              (neighbours (cackolen-empty-neighbours grid node)) )
        ;(format t "~&node: ~A, neighbours: ~A" node neighbours)
        (when (equal node end) (return-from cackolen-calculate-next-step (cackolen-backtrack-path-array path start end)))
        (setf open (cdr open)) ;(format t "~&reset open: ~A" open)
        (setf closed (append closed (list node))) ;(format t "~&reset closed: ~A" closed)
        (dolist (tmp neighbours)
          (when (and (not (member tmp closed :test #'equal)) (not (member tmp open :test #'equal)))
            (setf open (append open (list tmp)))
            (setf (aref path (car tmp) (cadr tmp)) node)))
            ;(format t "~&ADDING: open: ~A, path: ~A" open path)
        ))))

(defun cackolen-backtrack-path-array (path start end)
  "backrack path of BFS search"
  (let ((node end) (next (aref path (car end) (cadr end))))
       ;(format t "~&Start backracking: from: ~A, to: ~A, NEXT: ~A" end start next)
    (loop
      (when (equal next start) (return-from cackolen-backtrack-path-array node))
      (when (null next) (return-from cackolen-backtrack-path-array nil))
      (setf node next)
      (setf next (aref path (car node) (cadr node)))
      )))

(defun cackolen-empty-neighbours (grid position)
  "returns list of ok-to-step-on neighbouring positions"
  (setf p-x (car position)) (setf p-y (cadr position))
  (let ((all-neighbours nil)
        (maxx (- (car (array-dimensions grid)) 1 )) 
        (maxy (- (cadr (array-dimensions grid)) 1 )))
        (when (and (< p-x maxx) (cackolen-is-free-to-step grid (list (+ p-x 1) p-y)))
          (setf all-neighbours (append all-neighbours (list (list (+ p-x 1) p-y)))))
        (when (and (> p-x 0) (cackolen-is-free-to-step grid (list (- p-x 1) p-y)))
          (setf all-neighbours (append all-neighbours (list (list (- p-x 1) p-y)))))
        (when (and (< p-y maxy) (cackolen-is-free-to-step grid (list p-x (+ p-y 1)))) 
          (setf all-neighbours (append all-neighbours (list (list p-x (+ p-y 1))))))
        (when (and (> p-y 0) (cackolen-is-free-to-step grid (list p-x (- p-y 1))))
          (setf all-neighbours (append all-neighbours (list (list p-x (- p-y 1))))))
        ;(format t "~&position: ~A, all-neighbours: ~A" position all-neighbours)
        (return-from cackolen-empty-neighbours all-neighbours)))


(defun cackolen-is-free-to-step (grid position)
  "returns true if ball or empty"
  (setf mex (car position)) (setf mey (cadr position))
  (or (null (aref grid mex mey)) (identify-in-list #'percept-object-ball-p (aref grid mex mey)))
)

(defun cackolen-list-all-opponents (grid)
  "returns list of all opponents alive"
    (let (opponents)
        (dotimes (numberx (car (array-dimensions grid)))
            (dotimes (numbery (cadr (array-dimensions grid)))
                (when (identify-in-list #'cackolen-opponent-p (aref grid numberx numbery))
                    (setf opponents (append opponents (list(list numberx numbery))))
                    ;(setf opponents (cons (cons numberx numbery) opponents))
                ))) nil opponents))

(defmethod cackolen-opponent-p ((obj percept-object))
  "my opponent predicate"
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) cackolen-name)) 
           (not (equal (percept-object-name obj) "B")))
      obj nil))

(defun cackolen-my-location (grid)
  "returns my location"
    (dotimes (numberx (car (array-dimensions grid)))
        (dotimes (numbery (cadr (array-dimensions grid)))
            (when (identify-in-list #'cackolen-me-predicate (aref grid numberx numbery))
                (return-from cackolen-my-location (list numberx numbery))
            )
        )) nil )

(defmethod cackolen-me-predicate ((obj percept-object))
  "me predicate"
    (if (equal (percept-object-name obj) cackolen-name)
      obj nil))

;;------------THE END--------------