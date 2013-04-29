;;; ================================================================
;;; Student's agent definition:

(defstructure (staryvac
               (:include db-agent 
                         ; Simple agent body
                         (body (make-staryvac-body))
                         ; Use multi-mode proram
                         (program 'staryvac-agent-program)
                         ; Name is a constant
                         (name "staryvac"))))



;; Simple agent body structure with one data field for mode 1
(defstructure (staryvac-body
               (:include db-agent-body
                         ; Name is constant
                         (name "vst")  (sname "waz")))
    ; Variable for pre-calculated data
    (target nil)
  (have-ball nil)
  (pth nil))





(defun staryvac-agent-program (percept)
  (let* (  
         (agent-body (first percept))    ; extracts agent body from percept
         (me (car percept))
         (grid (cadr percept))
         (step nil)
         (next nil)
         (place nil)
         (myloc (object-loc me))         
         (ball-loc (staryvac-find #'percept-object-ball-p grid))
         )
    (setf percept (second percept))      ; extracts proper percept
    
    (print "my location:")
    (print myloc)
    (print "ball location:")
    (print (staryvac-find #'percept-object-ball-p grid))
    (print "info")
    (print (staryvac-body-target agent-body))
    (print (staryvac-body-have-ball agent-body))
    (print (staryvac-body-pth agent-body))
      
    (when (not(equal nil (staryvac-body-target agent-body)))
      (setf step (staryvac-body-target agent-body))
      (print step)
      (setf (staryvac-body-target agent-body) nil)
      (return-from staryvac-agent-program 
        (staryvac-coo-to-fun myloc step))
      )
       
    (when (staryvac-body-have-ball agent-body)
      (setf next (staryvac-get-next-enemy grid myloc))
      (when (not (equal next nil))
        (setf (staryvac-body-target agent-body) next)
              (setf (staryvac-body-have-ball agent-body) nil) 
           (return-from staryvac-agent-program (list 'throw-ball (car next) (cadr next))))
      
      (setf place (staryvac-find-place grid myloc))
      (setf (staryvac-body-have-ball agent-body) nil)      
      (return-from staryvac-agent-program (list 'throw-ball (car place) (cadr place))))
    
    (when (equal myloc ball-loc) 
      (when (not (staryvac-body-have-ball agent-body)) 
        (setf (staryvac-body-have-ball agent-body) T)
        (return-from staryvac-agent-program 'grab-ball))
      )
    

    (when (eql (staryvac-body-pth agent-body) nil)
      (print "not nil, value: ")
      (print (staryvac-body-pth agent-body))
      (setf (staryvac-body-pth agent-body) (staryvac-find-path grid myloc ball-loc))
      )
    
    (print (staryvac-body-pth agent-body) )
    (when (not (eql (staryvac-body-pth agent-body) nil))
           
           (setf step (car (staryvac-body-pth agent-body)))
           (setf (staryvac-body-pth agent-body) (cdr (staryvac-body-pth agent-body)))
           
           (return-from staryvac-agent-program step)
    )
               
    (when (equal myloc ball-loc) stop)  
   
    (return-from staryvac-agent-program 'stay)
    
    ) 
  )


(defun staryvac-find (item array)
  (dotimes (numberx (car (array-dimensions array)))
    (dotimes (numbery (cadr (array-dimensions array)))
      (when (listp (aref array numberx numbery))
        (dolist (x (aref array numberx numbery))
          (when (funcall item x)
            (return-from staryvac-find (list numberx numbery)))))))) 

(defun staryvac-get-next-enemy (grid position)
  (let (next)
      (print "x")
    (setf next (staryvac-get-neigh grid position))
      (dolist (item next)
        (when (staryvac-is-enemy (aref grid (car item) (cadr item)))
          (return-from staryvac-get-next-enemy item)))
    (return-from staryvac-get-next-enemy nil)
    )
  
  )

(defun staryvac-empty (node)
  (progn
    ;(print node)
    (when (percept-object-ball-p (car node)) (return-from staryvac-empty T))
    (when (eql node nil) (return-from staryvac-empty T))
    (return-from staryvac-empty nil)
    )
  )

(defun staryvac-is-enemy (node)  
  (when (eql node nil) (return-from staryvac-is-enemy nil))
  ;(print node)
  (when (and (percept-object-agent-p (car node)) (not (equal (percept-object-agent-name (car node)) "vst")))
      (return-from staryvac-is-enemy T))
  (return-from staryvac-is-enemy nil)
)

(defun staryvac-get-neigh (grid position)
  ( let   ((x (car position))
           (y (cadr position))
           (cx 0)
           (cy 0)
           (result nil))
    ;(print "let * is ok")
    (setf cx (+ x 1)) (setf cy y)
    (setf result (append result (list (list cx cy))))
    (setf cx (- x 1)) (setf cy y)
    (setf result (append result (list (list cx cy))))
    (setf cx x) (setf cy (+ y 1))
    (setf result (append result (list (list cx cy))))
    (setf cx x) (setf cy (- y 1))
    (setf result (append result (list (list cx cy))))
    ;(print result)
    (return-from staryvac-get-neigh result)
    )
  )

(defun staryvac-get-free-neigh (grid position)
  ( let   ((x (car position))
           (y (cadr position))
           (cx 0)
           (cy 0)
           (result nil))
    ;(print "let * is ok")
    (setf cx (+ x 1)) (setf cy y)
    (when (staryvac-empty (aref grid cx cy)) (setf result (append result (list (list cx cy)))))
    (setf cx (- x 1)) (setf cy y)
    (when (staryvac-empty (aref grid cx cy)) (setf result (append result (list (list cx cy)))))
    (setf cx x) (setf cy (+ y 1))
    (when (staryvac-empty (aref grid cx cy)) (setf result (append result (list (list cx cy)))))
    (setf cx x) (setf cy (- y 1))
    (when (staryvac-empty (aref grid cx cy)) (setf result (append result (list (list cx cy)))))
    ;(print result)
    (return-from staryvac-get-free-neigh result)
    )
  )

(defun staryvac-coo-to-fun (cooX cooY)
  (let ((xX (car  cooX))
        (xY (cadr cooX))
        (yX (car  cooY))
        (yY (cadr cooY)))
    
  (when (< xX yX) (return-from staryvac-coo-to-fun 'go-right))
  (when (> xX yX) (return-from staryvac-coo-to-fun 'go-left))
  
  (when (< xY yY) (return-from staryvac-coo-to-fun 'go-up))
  (when (> xY yY) (return-from staryvac-coo-to-fun 'go-down))
  )
  )

(defun staryvac-find-place (grid start)
  (let ((open nil)
        (close (make-hash-table :test #'equal))
        (temp nil)
        (nei nil)
        (ancestor nil))
    
    (setf open (append open (list start))) ;; add start into open
      
    (loop 
      (when (eql (length open) 0) (return nil))
      (setf ancestor (car open))

      (setf nei (staryvac-get-neigh grid ancestor))
      ;(print nei)
      (dolist (item nei)
        (when (staryvac-is-enemy (aref grid (car item) (cadr item)))
               (return-from staryvac-find-place ancestor)))
      
      (setf temp (staryvac-get-free-neigh grid ancestor)) 
      (setf open (cdr open))
            
      (dolist (item temp)  
        (when (not (cadr (multiple-value-bind (x y) (gethash item close) (list x y))))
          (progn
            (setf (gethash item close) ancestor)
            (setf open (append open (list item)))
            )
          )
        )
      )
    )
  )
  



(defun staryvac-find-path (grid start goal)
  (let ((open nil)
        (close (make-hash-table :test #'equal))
        (temp nil)
        (ancestor nil)
        (posA nil)
        (posB nil)
        (result nil))
    
    (setf open (append open (list start))) ;; add start into open
    (print "before w")
    (loop 
      (when (eql (length open) 0) (return nil))
      (setf ancestor (car open))
      (when (equal goal ancestor) (return T))
      
      (setf temp (staryvac-get-free-neigh grid ancestor)) 
      (setf open (cdr open))
            
      (dolist (item temp)  
        (when (not (cadr (multiple-value-bind (x y) (gethash item close) (list x y))))
          (progn
            (setf (gethash item close) ancestor)
            (setf open (append open (list item)))
            )
          )
        )
       ;(print open)
      )
    
    ;;;;--------------------------------------;;;;
    ;(print "Hledam cestu")
    (setf posA goal)
    (loop
        (when (equal posA start) (return T))
        (setf posB (gethash posA close))
        (setf result (append (list (staryvac-coo-to-fun posB posA)) result))
        (setf posA posB)
     )
    (print result)
    )
)