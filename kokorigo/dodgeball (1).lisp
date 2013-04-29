;;; File: dodgeball.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;;; The Dodgeball World Environment


;;;================================================================
;;; The world definition


(defconstant single-mode 1)
(defconstant competetive-mode 2)
(defparameter *SURE-HIT-DIST* 2)
(defparameter *CAN-HIT-DIST* 5)

(defstructure (db-world 
               (:include grid-environment
                         (size (@ 10 10))                ; 10 x 10 grid environment
                         (aspec nil)                     ; dodging agents
                         (bspec '((at edge wall)         ; grid edges are wall
                                  ball))                 ; 1 ball at a random location
                         ))
    "the dodgeball world"
  (game-mode single-mode)
  (agents-at-ball nil) ; (cons (the agent that is standing where the ball is) (no of turns he stands there)) : 
  ;; used for keeping record of agents who hold the ball excessively
  (agents-holding nil)
  (agents-lives nil); ; alist of agents and their lives
  (agents-hits nil) ; alist of agents and hits they made
  (agents-outlived nil) ; alist of agents and number of other agents they survived
  )          ; cons holding (car) an agent that holds the ball and number of turns he held it (cdr)

(defmethod agent-lives ((env db-world) (ag agent-body))
  (cdr (assoc ag (db-world-agents-lives env))))

(defmethod agent-hits ((env db-world) (ag agent-body))
  (cdr (assoc ag (db-world-agents-hits env))))

(defmethod agent-outlived ((env db-world) (ag agent-body))
  (cdr (assoc ag (db-world-agents-outlived env))))

(defmethod agent-has-ball ((env db-world) (ag agent-body))
  (cdr (assoc ag (db-world-agents-holding env))))

(defmethod agent-turns-with-ball ((env db-world) (ag agent-body))
  "returns a number of turns the agent is holding the ball or nil if he is not holding the ball."
  (cdr (assoc ag (db-world-agents-at-ball env))))

(defmethod agent-lost-life ((env db-world) (ag agent-body))
  ;;(format t "~A lost life and now has ~A lives~%" ag (- (agent-lives env ag) 1))
  (when (agent-alive env ag)
    (setf (cdr (assoc ag (db-world-agents-lives env))) (- (agent-lives env ag) 1))
    (when (= 0 (cdr (assoc ag (db-world-agents-lives env))))
      (format t "~&Agent ~A lost life and will be removed.~%" ag)
      (remove-agent env ag))))

(defmethod remove-agent ((env db-world) (ag agent-body))
  (format t "~A is removed from the env.~%" ag)
  ; release the ball if he has it
  (when (agent-has-ball env ag)
    ; make sure the agent releases the ball:
    (setf (cdr (assoc ag (db-world-agents-lives env))) 1)
    (throw-ball env ag (car (object-loc ag)) (cadr (object-loc ag)))
    (setf (cdr (assoc ag (db-world-agents-lives env))) 0))
  (setf (cdr (assoc ag (db-world-agents-at-ball env))) nil)
  (setf (cdr (assoc ag (db-world-agents-holding env))) nil)
  (remove-object ag env)
  (dolist (ag-alive (db-world-agents-outlived env))
      (when (agent-alive env (car ag-alive)) (agent-outlived-another env (car ag-alive)))))

(defmethod agent-alive ((env db-world) (ag agent-body))
  (> (agent-lives env ag) 0))

(defmethod agent-scored-hit ((env db-world) (ag agent-body))
  ;;(format t "~A scored a hit and now has ~A hits.~%" ag (+ 1 (agent-hits env ag)))
  (setf (cdr (assoc ag (db-world-agents-hits env))) (+ 1 (agent-hits env ag))))

(defmethod agent-outlived-another ((env db-world) (ag agent-body))
  ;;(format  t "~A outlived another.~%" ag)
  (setf (cdr (assoc ag (db-world-agents-outlived env))) (+ 1 (agent-outlived env ag))))

(defmethod agent-threw-ball ((env db-world) (ag agent-body))
  ;;(format t "~A threw ball~%" ag)
  (setf (cdr (assoc ag (db-world-agents-holding env))) nil)
  (let* ((loc (object-loc ag)) 
         (ball (ball-at-p env loc)))
    (when (not ball) ; when not standing where the ball is, then perform next line:
      (setf (cdr (assoc ag (db-world-agents-at-ball env))) nil))))

(defmethod agent-got-ball ((env db-world) (ag agent-body))
  ;;(format t "~A got ball~%" ag)
  (setf (cdr (assoc ag (db-world-agents-holding env))) t)
  (when (not (agent-turns-with-ball env ag))
    (setf (cdr (assoc ag (db-world-agents-at-ball env))) 0)))

(defmethod agent-at-ball ((env db-world) (ag agent-body))
  ;;(format t "~a is standing where the ball is.~%" ag)
  (let ((tmp (assoc ag (db-world-agents-at-ball env))))
    (when (null (cdr tmp))
      (setf (cdr tmp) 0))))

(defmethod agent-not-at-ball ((env db-world) (ag agent-body))
  ;;(format t "~a is NOT standing where the ball is.~%" ag)
  (let ((tmp (assoc ag (db-world-agents-at-ball env))))
    (when (cdr tmp)
      (setf (cdr tmp) nil))))

(defmethod increase-time-with-ball ((env db-world))
  (dolist (agent-time (db-world-agents-at-ball env))
    (when (cdr agent-time)
      (incf (cdr agent-time))
      ;;(format t "~A is standing at the ball for one more time, ~A in total~%" (car agent-time) (cdr agent-time) )
      (when (> (cdr agent-time) 3)
        (agent-lost-life env (car agent-time))))))


;;; ==========================================================
;;; percept objects definition


(defstructure (percept-object)
    "objects that are percieved by the agent in the environment"
  (name ""))

(defstructure (percept-object-wall (:include percept-object)))

(defstructure (percept-object-ball (:include percept-object)))

(defstructure (percept-object-agent (:include percept-object))
  (lives 0)
  (has-ball nil))


;;; ==================================================================
;;;; percept redefinition


(defun get-percept-grid (env)
  (transform-percept (my-copy-array (db-world-grid env)) env))

(defmethod get-percept ((env db-world) agent)
  (let ((ag-body (agent-body agent)))
    (list ag-body (get-percept-grid env))))

(defun transform-percept (grid env)
    (dotimes (x (car (array-dimensions grid)))
      (dotimes (y (cadr (array-dimensions grid)))
        (let ((tmp nil)(elems (aref grid x y)))
          (dolist (elem elems)
            (cond ((typep elem 'agent-body) 
                   (push (make-percept-object-agent :name (object-name elem) :lives (agent-lives env elem) :has-ball (agent-has-ball env elem)) tmp))
                  ((typep elem 'obstacle) 
                   (push (make-percept-object-wall :name (object-name elem)) tmp))
                  ((typep elem 'ball) 
                   (push (make-percept-object-ball :name (object-name elem)) tmp))
                  (t (print "unknown element in grid"))))
          (setf (aref grid x y) tmp)))) grid )


;;; ==========================================================
;;; agent definition


(defstructure (db-agent 
                (:include agent (body (make-db-agent-body))))
    "An agent for db world."
  )

(defstructure (db-agent-body (:include agent-body)) ; it is an object holding nil
    (grid nil)
  (sname nil))


;;; ==========================================================
;;; interactive agent definition


(defstructure (ask-user-db-agent (:include db-agent (program 'ask-user-db))) 
    "An agent that asks the user to type in an action.")

(defun parse-line (string)
  (if (or (null string) (equal "" string)) nil
    (let ((read (multiple-value-list (read-from-string string))))
      (if (car read) (cons (car read) (parse-line (subseq string (cadr read)))) nil ))))

(defun ask-user-db (percept)
  "Ask the user what action to take."
  (format t 
     "~&action (go-right go-up go-left go-down grab-ball throw-ball stop stay)? ")
  (parse-line (read-line)))


;;; ==========================================================
;;; evil, back-throwing agent definition


(defconstant wait-and-throw-db-agent-name "WT")

(defstructure (wait-and-throw-db-agent-body (:include db-agent-body (name wait-and-throw-db-agent-name))))

(defstructure (wait-and-throw-db-agent                
               (:include db-agent 
                         (program 'wait-and-throw-at-student) 
                         (body (make-wait-and-throw-db-agent-body))
                         (name wait-and-throw-db-agent-name)))
    "An agent that will be attempting to hit the student in the first part of the semestral project.")

(defun wait-and-throw-at-student (percept)
  "throw the ball at the student if the ball is in my posession.
   grab the ball if it is on my square
   otherwise stay"
  (let* ((me (car percept))
         (grid (cadr percept))
         (ball-on-my-loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
         (holding-ball (object-contents me))
         (student-loc (find-student-location grid)))
    (cond 
     ( (not student-loc) 'stop )
     ( ball-on-my-loc 'grab-ball )
     ( holding-ball `(throw-ball ,@student-loc))
     ( t 'stay))))

(defun find-student (grid)
  (let (student)
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
        (when (setf student (identify-in-list #'student-p (aref grid numberx numbery)))
          (return-from find-student student))))) nil )

(defun find-X-location (X-predicate grid)
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (identify-in-list X-predicate (aref grid numberx numbery))
        (return-from find-X-location (list numberx numbery))))) nil )

(defun find-student-location (grid)
  (find-X-location #'student-p grid))

(defun find-ball-location (grid)
  (find-X-location #'my-ball-p grid))

(defun identify-in-list (pred list)
  (dolist (item list)
    (when (funcall pred item)
      (return-from identify-in-list item))) nil)

(defmethod student-p ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) wait-and-throw-db-agent-name)) 
           (not (equal (percept-object-name obj) "B")))
      obj nil))

(defmethod my-ball-p ((obj percept-object))
  (if (equal (percept-object-name obj) "B")
      obj nil))


;;; ================================================================
;;; Student's agent definition:

;; This is to be defined when designing a new student agent 
;
(defconstant my-name "kokorigo-master")
(defconstant my-short-name "kk")

(defstructure (kokorigo-agent    ; replace "my-agent" by your unique name, as e.g. FIT username
                (:include db-agent 
                  (body (make-my-agent-body))
                  (program 'my-agent-program)
                  (name my-name))) 
  "Your agent for db-world.")

(defstructure (my-agent-body 
                (:include db-agent-body (name my-short-name)))


)
;  (slot1 default1)  ; any specific extra slots your agent's body would need
;  ...
;  (slotn defaultn))
;    ;
;    )
;
(defun my-agent-program (percept)
  (let ((agent-body (first percept))
         
         (nearestenemy nil)
         (wheretogonext nil)
         (weakestnear nil)
        (nearesttoball nil)
        (movpriorities nil)
)    ; extracts agent body from percept
    (setf percept (second percept))      ; extracts proper percept
 ;...
 ;...  here your program comes to calculate and return a proper action       
 ;...
;(dotimes (numberx (car (array-dimensions percept)))
;    (dotimes (numbery (cadr (array-dimensions percept)))
;      (dolist (item (aref percept numberx numbery))
;          (format t "~&na pozici ~D, ~D, jmeno ~A, " numberx numbery (percept-object-name item))
;
;          (cond 
;                 ( (typep item 'percept-object-wall) (format t "item je stena") )
;
;                 (  (typep item 'percept-object-agent) (format t "item je agent"))
;                 ( (typep item 'percept-object-ball) (format t "item je mic") )
;                 ( t (format t "item je neznamy"))
;          ) 
;          (if 
;                 (and (typep item 'percept-object-agent) (null (percept-object-agent-has-ball item)))
;                 (format t ", ma ~D zivotu a nema mic" (percept-object-agent-lives item))
;          )
;          (if 
;                 (and 
;                       (typep item 'percept-object-agent) 
;                       (not (null (percept-object-agent-has-ball item)))
;                 )
;                 (format t ", ma ~D zivotu a ma mic" (percept-object-agent-lives item))
;          )    
;      ))
;)

;returns either the agent with least lives or nil of he's too far
(setf weakestnear (findleastlives (object-loc agent-body) (findnextenemy 0 0 percept) percept))


(setf nearestenemy (findnearestenemy (object-loc agent-body) (findnextenemy 0 0 percept)))


;(format t "~&debug1")

;(if (null (car nearestenemy))
;(format t 
;     "~&action (go-right go-up go-left go-down grab-ball throw-ball stop stay)? ")
;)  

;(when (null (car nearestenemy))
;(return-from my-agent-program (parse-line (read-line)))
;)


;(format t "~&debug2")




(when (and  (not (null (find-ball-location percept)))
            (= 
                  (car (object-loc agent-body)) 
                  (car (find-ball-location percept))
            )
            (=
                  (cadr (object-loc agent-body)) 
                  (cadr (find-ball-location percept))
            )
      )
(return-from my-agent-program 'grab-ball)
)
;(format t "~&debug2.5")


(when (and (not (object-contents agent-body)) (not (find-ball-location percept)))
(return-from my-agent-program 'stay)
)

;only works if the ball is on the field!!!!
( if (find-ball-location percept)
(dolist (item (findnextenemy 0 0 percept))
(if (< 
           (+ 
                 (abs (- (car(find-ball-location percept)) (car item)))
                 (abs (- (cadr(find-ball-location percept)) (cadr item)))
                 1
           )
           (+ 
                 (abs (- (car(find-ball-location percept)) (car (object-loc agent-body))))
                 (abs (- (cadr(find-ball-location percept)) (cadr (object-loc agent-body))))
           )
      ) 

(setf nearesttoball t)
)
)
)

(when (and      (find-ball-location percept)
               (aminext (find-ball-location percept) (object-loc agent-body))
               (=  (- (car (find-ball-location percept)) (car (object-loc agent-body))) -1)
)
(return-from my-agent-program 'go-left)
)

(when (and        (find-ball-location percept)
               (aminext (find-ball-location percept) (object-loc agent-body))
               (=  (- (car (find-ball-location percept)) (car (object-loc agent-body))) 1)
)
(return-from my-agent-program 'go-right)
)

(when (and       (find-ball-location percept)
               (aminext (find-ball-location percept) (object-loc agent-body))
               (=  (- (cadr (find-ball-location percept)) (cadr (object-loc agent-body))) 1)
)
(return-from my-agent-program 'go-up)
)

(when (and        (find-ball-location percept)
                (aminext (find-ball-location percept) (object-loc agent-body))
               (=  (- (cadr (find-ball-location percept)) (cadr (object-loc agent-body))) -1)
)
(return-from my-agent-program 'go-down)
)



(if (and (find-ball-location percept)

(not (object-contents agent-body)))
(setf movpriorities
(transformpriorities (object-loc agent-body)
           (gopriority (object-loc agent-body) (find-ball-location percept)))
))


(setf wheretogonext nil)


(if(and (not (object-contents agent-body))
        nearesttoball
        (null (aref percept 
                 (+  (car (object-loc agent-body)) (- (car (object-loc agent-body)) (caar movpriorities)))
                 (+  (cadr (object-loc agent-body)) (- (cadr (object-loc agent-body)) (cadar movpriorities)))
              )  
        )
        (<=
         (cadar(last(go-through-dist-list (object-loc agent-body) (find-ball-location percept))))  
          (cadar(last(go-through-dist-list 
(list  
 (+  
              (car (object-loc agent-body)) 
           (- 
                   (car (object-loc agent-body)) 
                   (caar movpriorities)
           )
 )
 (+  (cadr (object-loc agent-body)) (- (cadr (object-loc agent-body)) (cadar movpriorities)))
)
 (find-ball-location percept))))
        )
)
(setf wheretogonext(  list 
 (+  (car (object-loc agent-body)) (- (car (object-loc agent-body)) (caar movpriorities)))
 (+  (cadr (object-loc agent-body)) (- (cadr (object-loc agent-body)) (cadar movpriorities)))
)
)
)

(if(and  (not (object-contents agent-body))
          (null wheretogonext)
        nearesttoball
        (null (aref percept 
                 (+  (car (object-loc agent-body)) (- (car (object-loc agent-body)) (caadr movpriorities)))
                 (+  (cadr (object-loc agent-body)) (- (cadr (object-loc agent-body)) (cadadr movpriorities)))
              )  
        )
        (<=
         (cadar(last(go-through-dist-list (object-loc agent-body) (find-ball-location percept))))  
          (cadar(last(go-through-dist-list 
(list  
 (+  (car (object-loc agent-body)) (- (car (object-loc agent-body)) (caadr movpriorities)))
 (+  (cadr (object-loc agent-body)) (- (cadr (object-loc agent-body)) (cadadr movpriorities)))
)
 (find-ball-location percept))))
        )
)
(setf wheretogonext (  list
 (+  (car (object-loc agent-body)) (- (car (object-loc agent-body)) (caadr movpriorities)))
 (+  (cadr (object-loc agent-body)) (- (cadr (object-loc agent-body)) (cadadr movpriorities)))
)
)
)

(if(and (not (object-contents agent-body))
        (null wheretogonext)
         nearesttoball
        (null (aref percept 
                 (+  (car (object-loc agent-body)) (- (car (object-loc agent-body)) (caadr (cdr movpriorities))))
                 (+  (cadr (object-loc agent-body)) (- (cadr (object-loc agent-body)) (cadadr (cdr movpriorities))))
              )  
        )
        (<=
         (cadar(last(go-through-dist-list (object-loc agent-body) (find-ball-location percept))))  
          (cadar(last(go-through-dist-list 
(list  
 (+  (car (object-loc agent-body)) (- (car (object-loc agent-body)) (caadr (cdr  movpriorities))))
 (+  (cadr (object-loc agent-body)) (- (cadr (object-loc agent-body)) (cadadr (cdr movpriorities))))
)
 (find-ball-location percept))))
        )
)
(setf wheretogonext (list  
 (+  (car (object-loc agent-body)) (- (car (object-loc agent-body)) (caadr (cdr movpriorities))))
 (+  (cadr (object-loc agent-body)) (- (cadr (object-loc agent-body)) (cadadr (cdr movpriorities))))
)
)
)

(if(and (not (object-contents agent-body))
      (null wheretogonext)
        nearesttoball
)
(setf wheretogonext (list (car (object-loc agent-body))  (cadr (object-loc agent-body))  ))
)

(if(and
           (not (object-contents agent-body))
            (null wheretogonext)
             (null nearesttoball)
        (null (aref percept 
                   (caar movpriorities) 
                (cadar movpriorities) 
              )  
        )

)
(setf wheretogonext (list (caar movpriorities) (cadar movpriorities)))
)
(if(and
           (not (object-contents agent-body))
            (null wheretogonext)
             (null nearesttoball)
(null (aref percept 
                   (caadr movpriorities) 
                (cadadr movpriorities) 
              )  
        )
)
(setf wheretogonext (list (caadr movpriorities) (cadadr movpriorities)))
)
(if(and    (find-ball-location percept)
           (not (object-contents agent-body))
            (null wheretogonext)
             (null nearesttoball)
(null (aref percept 
                   (caadr (cdr movpriorities) )
                (cadadr ( cdr movpriorities) )
              )  
        )
(=  (cadar(last(go-through-dist-list (caddr movpriorities) (find-ball-location percept))))
 (cadar(last(go-through-dist-list (cadr movpriorities) (find-ball-location percept))))

)


)
(setf wheretogonext (list (caadr (cdr movpriorities)) (cadadr (cdr movpriorities))))
)
(if(and
           (not (object-contents agent-body))
            (null wheretogonext)
             (null nearesttoball)
)
(setf wheretogonext (list (car (object-loc agent-body))  (cadr (object-loc agent-body))  ))
)



(when(and
        (object-contents agent-body)  
        
           weakestnear
)
(return-from my-agent-program `(throw-ball ,@weakestnear))
)



(setf movpriorities
(transformpriorities (object-loc agent-body)
           (gopriority (object-loc agent-body) nearestenemy))
)

(when(and(object-contents agent-body)  
        
           (null weakestnear)
)
(return-from my-agent-program `(throw-ball ,@(car movpriorities)))
)


(when (and (not (object-contents agent-body))
(= (cadr (object-loc agent-body)) (cadr wheretogonext))
(= (car (object-loc agent-body)) (car wheretogonext))
)
(return-from my-agent-program 'stay)
)
(when (and (not (object-contents agent-body))
(= (cadr (object-loc agent-body)) (- (cadr wheretogonext) 1))
(= (car (object-loc agent-body)) (car wheretogonext))
)
(return-from my-agent-program 'go-up)
)
(when (and (not (object-contents agent-body))
(= (cadr (object-loc agent-body)) (+ (cadr wheretogonext) 1))
(= (car (object-loc agent-body)) (car wheretogonext))
)
(return-from my-agent-program 'go-down)
)
(when (and (not (object-contents agent-body))
(= (cadr (object-loc agent-body)) (cadr wheretogonext))
(= (car (object-loc agent-body)) (- (car wheretogonext) 1))
)
(return-from my-agent-program 'go-right)
)
(when (and (not (object-contents agent-body))
(= (cadr (object-loc agent-body)) (cadr wheretogonext))
(= (car (object-loc agent-body)) (+ (car wheretogonext) 1))
)
(return-from my-agent-program 'go-left)
)

'stay

;(if(object-contents agent-body)(format t "~&drzim balon")(format t "~&nedrzim balon"))





;(setf nearestenemy (findnearestenemy (object-loc agent-body) (findnextenemy 0 0 percept)))
;(if (null nearestenemy) (format t "~& no enemies on the field~&")
;(format t "~&nearest enemy is: ~D, ~D~&" (car nearestenemy)(cadr nearestenemy)))

;(format t "~&throw at: ~D, ~D" (car(wheretothrow (object-loc agent-body) nearestenemy))
;(cadr(wheretothrow (object-loc agent-body) nearestenemy)))

;(if (find-ball-location percept) (format t "~&mic je na pozici ~D, ~D~&" 
;(car (find-ball-location percept))
;(cadr (find-ball-location percept))
;)
;(format t "~& mic neni nikde~&"))

;(format t "~&go priority: ~A, ~A, ~A, ~A"
;(car(gopriority (object-loc agent-body) (find-ball-location percept)))
;(cadr(gopriority (object-loc agent-body) (find-ball-location percept)))
;(caddr(gopriority (object-loc agent-body) (find-ball-location percept)))
;(cadddr(gopriority (object-loc agent-body) (find-ball-location percept)))
;)

;(if (find-ball-location percept)
;(dolist (item (pathway (object-loc agent-body) (find-ball-location percept) percept "nowhere"))
;(format t "~A " item)
;
;)
;)

;is true (if(typep agent-body 'my-agent-body)(format t "~&ano, je to spravnej typ~&")) 
;is true (if(typep agent-body 'object)(format t "~&ano, je to spravnej typ2~&"))


;(if (and (= (car (object-loc agent-body)) 2) (= (cadr (object-loc agent-body)) 5)) (setf (my-agent-body-cesta agent-body) 2))


;(format t "~&global variable je: ~D~&" (my-agent-body-cesta agent-body))





) )
; for example:
; (nth (random 4) '(go-left go-right go-up go-down))
;    ) )


(defun aminext (mylocation enemylocation)
;(format t "~&debug77")
(when (or
            (and 
                   (= (- (car mylocation) 1) (car enemylocation)) 
                   (= (cadr mylocation) (cadr enemylocation))
            )
            (and 
                   (= (+ (car mylocation) 1) (car enemylocation)) 
                   (= (cadr mylocation) (cadr enemylocation))
            )
            (and 
                   (= (car mylocation) (car enemylocation)) 
                   (= (- (cadr mylocation) 1) (cadr enemylocation)))
            (and 
                   (= (car mylocation) (car enemylocation))
                   (= (+ (cadr mylocation) 1) (cadr enemylocation)))
      )
(return-from aminext t)
)
nil
)

(defun alreadyvisited (priorities previouslyvisited)
"takes priorities (a list of 4 lists (x y)) and the previously visited coordinates
and returns modofied priorities"
(let ((resultlist (list (list -1 -1) (list -1 -1) (list -1 -1) (list -1 -1))))

(when (null (car previouslyvisited)) 
(return-from alreadyvisited (list (list -1 -1) (list -1 -1) (list -1 -1) (list -1 -1)))
)

(dolist (item previouslyvisited)

(when (and (= (car item) (caar priorities)) (= (cadr item) (cadar priorities))
)
(setf (caar resultlist) (car item))
(setf (cadar resultlist) (cadr item))
)

(when (and (= (car item) (caadr priorities)) (= (cadr item) (cadadr priorities))
)
(setf (caadr resultlist) (car item))
(setf (cadadr resultlist) (cadr item))
)

(when (and (= (car item) (caadr (cdr priorities))) (= (cadr item) (cadadr (cdr priorities)))
)
(setf (caadr (cdr resultlist)) (car item))
(setf (cadadr (cdr resultlist)) (cadr item))
)

(when (and (= (car item) (caadr (cddr priorities))) (= (cadr item) (cadadr (cddr priorities)))
)
(setf (caadr (cddr resultlist)) (car item))
(setf (cadadr (cddr resultlist)) (cadr item))
)

)

(when t
(return-from alreadyvisited
(list
(list (caar resultlist) (cadar resultlist))
(list (caadr resultlist) (cadadr resultlist))
(list (caadr (cdr resultlist)) (cadadr (cdr resultlist)))
(list (caadr (cddr resultlist)) (cadadr (cddr resultlist)))
)
)
)


)
)



(defun pathway (mylocation wheretogo grid previousway)
"returns a list of waypoints, last waypoint is stay 
or returns a list with car equal to nil and cdr equal to nil, however it is a 1 element list"
(let*  (                
             (priorities (transformpriorities mylocation (gopriority mylocation wheretogo)))
             (locationoccupied nil)
             (prioritiesnames (gopriority mylocation wheretogo))
             (wherenottogo (alreadyvisited priorities previousway))

      )
(when (and (= (car mylocation) (car wheretogo)) (= (cadr mylocation) (cadr wheretogo)))
      (return-from pathway (list "stay"))
)
(dolist (item (aref grid (car mylocation) (cadr mylocation)))
   (if    (or  (equal (percept-object-name item) "#")
               (equal (percept-object-name item) wait-and-throw-db-agent-name)
               (typep item 'percept-object-wall)
               (and (typep item 'percept-object-agent) (not (equal (percept-object-name item) my-short-name)))
          )
         (setf locationoccupied t)
   )

)
(when (null (not locationoccupied)) (return-from pathway (list nil)))


(if (not (and 
                (= (caar priorities) (caar wherenottogo)) 
                (= (cadar priorities) (cadar wherenottogo) )
        )
    )
(when (not (null 
               (car  
                      (last (pathway (car priorities) wheretogo grid (cons mylocation previousway)))
               )
           )
      )               
      (return-from pathway (cons (car prioritiesnames) (pathway (car priorities) wheretogo grid (cons mylocation previousway))))         
))
(if (not (and 
                (= (caadr priorities) (caadr wherenottogo)) 
                (= (cadadr priorities) (cadadr wherenottogo) )
        ))
(when (not (null 
               (car  
                      (last (pathway (cadr priorities) wheretogo grid (cons mylocation previousway)))
               )
           )
      )               
      (return-from pathway (cons (cadr prioritiesnames) (pathway (cadr priorities) wheretogo grid (cons mylocation previousway))))         
))
(if (not (and 
                (= (caadr (cdr priorities)) (caadr (cdr wherenottogo))) 
                (= (cadadr (cdr priorities)) (cadadr (cdr wherenottogo)))
        ))
(when (not (null 
               (car  
                      (last (pathway (caddr priorities) wheretogo grid (cons mylocation previousway)))
               )
           )
      )               
      (return-from pathway (cons (caddr prioritiesnames) (pathway (caddr priorities) wheretogo grid (cons mylocation previousway))))         
))
(if (not (and 
                (= (caadr (cddr priorities)) (caadr (cddr wherenottogo))) 
                (= (cadadr (cddr priorities)) (cadadr (cddr wherenottogo)))
        ))
(when (not (null 
               (car  
                      (last (pathway (cadddr priorities) wheretogo grid (cons mylocation previousway)))
               )
           )
      )               
      (return-from pathway (cons (cadddr prioritiesnames) (pathway (cadddr priorities) wheretogo grid (cons mylocation previousway))))         
))


)
(list nil)
)

(defun transformpriorities (mylocation priorities) 
"takes my location (x y) and go priorities (way way way way) and transforms them to coordinates"
(let
      (
         (resultoutput 
             (list nil nil nil nil)
         )
      )
(if (equal (car priorities) "go-left") (setf (car resultoutput) (list (- (car mylocation) 1) (cadr mylocation))))
(if (equal (car priorities) "go-right") (setf (car resultoutput) (list (+ (car mylocation) 1) (cadr mylocation))))
(if (equal (car priorities) "go-up") (setf (car resultoutput) (list (car mylocation) (+ (cadr mylocation) 1))))
(if (equal (car priorities) "go-down") (setf (car resultoutput) (list (car mylocation) (- (cadr mylocation) 1)))) 

(if (equal (cadr priorities) "go-left") (setf (cadr resultoutput) (list (- (car mylocation) 1) (cadr mylocation))))
(if (equal (cadr priorities) "go-right") (setf (cadr resultoutput) (list (+ (car mylocation) 1) (cadr mylocation))))
(if (equal (cadr priorities) "go-up") (setf (cadr resultoutput) (list (car mylocation) (+ (cadr mylocation) 1))))
(if (equal (cadr priorities) "go-down") (setf (cadr resultoutput) (list (car mylocation) (- (cadr mylocation) 1)))) 

(if (equal (caddr priorities) "go-left") (setf (caddr resultoutput) (list (- (car mylocation) 1) (cadr mylocation))))
(if (equal (caddr priorities) "go-right") (setf (caddr resultoutput) (list (+ (car mylocation) 1) (cadr mylocation))))
(if (equal (caddr priorities) "go-up") (setf (caddr resultoutput) (list (car mylocation) (+ (cadr mylocation) 1))))
(if (equal (caddr priorities) "go-down") (setf (caddr resultoutput) (list (car mylocation) (- (cadr mylocation) 1)))) 

(if (equal (cadddr priorities) "go-left") (setf (cadddr resultoutput) (list (- (car mylocation) 1) (cadr mylocation))))
(if (equal (cadddr priorities) "go-right") (setf (cadddr resultoutput) (list (+ (car mylocation) 1) (cadr mylocation))))
(if (equal (cadddr priorities) "go-up") (setf (cadddr resultoutput) (list (car mylocation) (+ (cadr mylocation) 1))))
(if (equal (cadddr priorities) "go-down") (setf (cadddr resultoutput) (list (car mylocation) (- (cadr mylocation) 1)))) 


(list
(list (caar resultoutput) (cadar resultoutput))
(list (caadr resultoutput) (cadadr resultoutput))
(list (caaddr resultoutput) (cadadr (cdr resultoutput)))
(list (caaddr (cdr resultoutput)) (cadadr (cddr resultoutput)))
)
)
)



(defun gopriority (mylocation wheretogo)
"mylocation and wheretogo MUST NOT be the same space!! otherwise the returned list will have duplicate directions"
(let ((seznamsmeru
                (list
                    (cadar(last(go-through-dist-list (list (- (car mylocation) 1) (cadr mylocation)) wheretogo)))
                    (cadar(last(go-through-dist-list (list (+ (car mylocation) 1) (cadr mylocation)) wheretogo)))
                    (cadar(last(go-through-dist-list (list (car mylocation) (- (cadr mylocation) 1)) wheretogo)))
                    (cadar(last(go-through-dist-list (list (car mylocation) (+ (cadr mylocation) 1)) wheretogo)))
                )
      )
      (kopieseznamu nil)
      (serazenyseznam nil)
      (vysledek (list 1 2 3 4))
     )

(if (null (car seznamsmeru)) (setf (car seznamsmeru) 0))
(if (null (cadr seznamsmeru)) (setf (cadr seznamsmeru) 0))
(if (null (caddr seznamsmeru)) (setf (caddr seznamsmeru) 0))
(if (null (cadddr seznamsmeru)) (setf (cadddr seznamsmeru) 0))

(setf kopieseznamu (list (car seznamsmeru) (cadr seznamsmeru) (caddr seznamsmeru) (cadddr seznamsmeru)))
(setf serazenyseznam (sort seznamsmeru #'<))

(if (= (car kopieseznamu) (car serazenyseznam)) (setf (car vysledek) "go-left"))
(if (= (cadr kopieseznamu) (car serazenyseznam)) (setf (car vysledek) "go-right"))
(if (= (caddr kopieseznamu) (car serazenyseznam)) (setf (car vysledek) "go-down"))
(if (= (cadddr kopieseznamu) (car serazenyseznam)) (setf (car vysledek) "go-up"))

(if (and (not (equal (car vysledek) "go-left")) (= (car kopieseznamu) (cadr serazenyseznam))) (setf (cadr vysledek) "go-left"))
(if (and (not (equal (car vysledek) "go-right")) (= (cadr kopieseznamu) (cadr serazenyseznam))) (setf (cadr vysledek) "go-right"))
(if (and (not (equal (car vysledek) "go-down")) (= (caddr kopieseznamu) (cadr serazenyseznam))) (setf (cadr vysledek) "go-down"))
(if (and (not (equal (car vysledek) "go-up")) (= (cadddr kopieseznamu) (cadr serazenyseznam))) (setf (cadr vysledek) "go-up"))

(if (and (not (equal (cadr vysledek) "go-left")) (= (car kopieseznamu) (caddr serazenyseznam))) (setf (caddr vysledek) "go-left"))
(if (and (not (equal (cadr vysledek) "go-right")) (= (cadr kopieseznamu) (caddr serazenyseznam))) (setf (caddr vysledek) "go-right"))
(if (and (not (equal (cadr vysledek) "go-down")) (= (caddr kopieseznamu) (caddr serazenyseznam))) (setf (caddr vysledek) "go-down"))
(if (and (not (equal (cadr vysledek) "go-up")) (= (cadddr kopieseznamu) (caddr serazenyseznam))) (setf (caddr vysledek) "go-up"))

(if (and (not (equal (caddr vysledek) "go-left")) (= (car kopieseznamu) (cadddr serazenyseznam))) (setf (cadddr vysledek) "go-left"))
(if (and (not (equal (caddr vysledek) "go-right")) (= (cadr kopieseznamu) (cadddr serazenyseznam))) (setf (cadddr vysledek) "go-right"))
(if (and (not (equal (caddr vysledek) "go-down")) (= (caddr kopieseznamu) (cadddr serazenyseznam))) (setf (cadddr vysledek) "go-down"))
(if (and (not (equal (caddr vysledek) "go-up")) (= (cadddr kopieseznamu) (cadddr serazenyseznam))) (setf (cadddr vysledek) "go-up"))


(when t 
     (return-from gopriority (list (car vysledek) (cadr vysledek) (caddr vysledek) (cadddr vysledek)))
)
)
)

(defun wheretothrow(mylocation enemylocation)
"might return mylocation since we are next to enemy already"
(let((vzdaldown 0)(vzdalup 0)(vzdalleft 0)(vzdalright 0))
(when (or
            (and
                   (= (car mylocation) (car enemylocation))
                   (or  
                         (= (cadr mylocation) (- (cadr enemylocation) 1))
                         (= (cadr mylocation) (+ (cadr enemylocation) 1))
                   )
            )
            (and
                   (= (cadr mylocation) (cadr enemylocation))
                   (or  
                         (= (car mylocation) (- (car enemylocation) 1))
                         (= (car mylocation) (+ (car enemylocation) 1))
                   )
            )
      ) 
(return-from wheretothrow (list (car mylocation) (cadr mylocation)))
)
(setf vzdaldown (cadar(
                         last(
                             go-through-dist-list 
                                mylocation 
                                (list 
                                    (car enemylocation) 
                                    (- (cadr enemylocation) 1)
                                )
                             )
                      )
                )
)
(setf vzdalup (cadar(
                         last(
                             go-through-dist-list 
                                mylocation 
                                (list 
                                    (car enemylocation) 
                                    (+ (cadr enemylocation) 1)
                                )
                             )
                      )
                )
)
(setf vzdalleft (cadar(
                         last(
                             go-through-dist-list 
                                mylocation 
                                (list 
                                    (- (car enemylocation) 1)
                                    (cadr enemylocation) 
                                )
                             )
                      )
                )
)
(setf vzdalright (cadar(
                         last(
                             go-through-dist-list 
                                mylocation 
                                (list 
                                    (+ (car enemylocation) 1) 
                                    (cadr enemylocation)
                                )
                             )
                      )
                )
)

(when (and
             (<= vzdaldown vzdalup)
             (<= vzdaldown vzdalleft)
             (<= vzdaldown vzdalright)
      )
(return-from wheretothrow (list (car enemylocation) (- (cadr enemylocation) 1) ))                                  
)

(when (and
             (<= vzdalup vzdalleft)
             (<= vzdalup vzdalright)
      )
(return-from wheretothrow (list (car enemylocation) (+ (cadr enemylocation) 1) ))                                  
)

(when  (<= vzdalleft vzdalright) 
(return-from wheretothrow (list (- (car enemylocation) 1)  (cadr enemylocation) ))                                  
)

(when t
(return-from wheretothrow (list (+ (car enemylocation) 1)  (cadr enemylocation)  ))                                  
)

)
)

(defun findleastlives(mylocation enemylocations grid)
"returns nil (there are no enemies on the field) or the location of nearest enemy with least lives"
(let((weakestenemy nil)(leastlivescount 30)(distanceofcandidate nil)(livesofcandidate 0))
(dolist (item enemylocations)
(setf distanceofcandidate (cdar(last(go-through-dist-list mylocation item))))

    (dolist (vecnapolicku (aref grid (car item) (cadr item)))
       (if (typep vecnapolicku 'percept-object-agent)
           (setf livesofcandidate (percept-object-agent-lives vecnapolicku))
       )
    )


(when
(
             and (> 5 (car distanceofcandidate)) (> leastlivescount livesofcandidate)
)
(setf leastlivescount livesofcandidate)
(setf weakestenemy (list (car item) (cadr item)))
)



)

(
when (not (null weakestenemy))
(return-from findleastlives (list (car weakestenemy) (cadr weakestenemy)))
)

)

nil
)



(defun findnearestenemy(mylocation enemylocations)
"returns nil (there are no enemies on the field) or the location of nearest enemy"
(let((nearestone nil)(nearestdistance 0)(distancecandidate nil))
(dolist (item enemylocations)
(setf distancecandidate (cdar(last(go-through-dist-list mylocation item))))
(when
(
             or (> nearestdistance (car distancecandidate)) (= nearestdistance 0)
)
(setf nearestdistance (car distancecandidate))
(setf nearestone (list (car item) (cadr item)))
)



)

(
when (not (null nearestone))
(return-from findnearestenemy (list (car nearestone) (cadr nearestone)))
)

)

nil
)


(defun findnextenemy(fromx fromy grid)
"returns nil (when there are no enemies on field) or a list of enemy locations 
(each enemy location is a list containing 2 things: x coordinate and y coordinate)"
(let (
      (sizex (car (array-dimensions grid)))
      (sizey (cadr (array-dimensions grid)))
     )
(loop for i from 
           (+ (* fromy sizex) fromx)
           to
           (- (* sizex sizey) 1) do
(dolist (item 
             (aref grid 
                        (mod i sizex) 
                        (/ (- i (mod i sizex)) sizex)
             )
        )
(when (and (not (equal (percept-object-name item) "#")) 
           (not (equal (percept-object-name item) my-short-name)) 
           (not (equal (percept-object-name item) "B"))
           (not  (typep item 'percept-object-wall))
           (not  (typep item 'percept-object-ball))
           (typep item 'percept-object-agent)
      )
      (return-from findnextenemy
                  (cons (list (mod i sizex) (/ (- i (mod i sizex)) sizex))
                        (findnextenemy
                            (mod (+ (mod i sizex) 1) sizex) 
                            (/ (- (+ i 1) (mod (+ i 1) sizex)) sizex)
                            grid       
                         )
                  )
      )
)


))
)


nil
)

;;; Any number of auxiliary functions can follow
;;; 
;;; To test run the game in single mode you perform
;;; (test-agent-mode-1 'your-agent-name)
;;; for example:
;;; (test-agent-mode-1 'ask-user-db-agent)
;;;
;;; To test run the game in competitive mode, we will perform
;;; (test-agent-mode-2 '(first-student-agent second-student-agent third-student-agent ...))
;;; for example:
;;; (test-agent-mode-2 '(ask-user-db-agent ask-user-db-agent))
;;; ==================================================================

;;; ==================================================================
;;; Functions that are used for testing your agent


(defun test-agent-mode-1 (student-agent &rest other-agents)
  "returns t if the agent survives until the end and max-steps was not reached. Otherwise returns nil"
  (let* ((world (make-db-world :max-steps 500)) (SHD *SURE-HIT-DIST*))
 
   (add-agent world 'wait-and-throw-db-agent)
    (add-agent world 'wait-and-throw-db-agent)
    (add-agent world 'wait-and-throw-db-agent)
(add-agent world 'wait-and-throw-db-agent)
(add-agent world 'wait-and-throw-db-agent)
(add-agent world 'wait-and-throw-db-agent)
    (add-agent world 'wait-and-throw-db-agent)
    (add-agent world 'wait-and-throw-db-agent)
(add-agent world 'wait-and-throw-db-agent)
(add-agent world 'wait-and-throw-db-agent)
(add-agent world 'wait-and-throw-db-agent)
    (add-agent world 'wait-and-throw-db-agent)
  






   







    (add-agent world student-agent)
    (dolist (agent other-agents)
      (add-agent world agent))
    (setf *SURE-HIT-DIST* 20) ; The ball will take life when hitting anywhere in the db-world
    (initialize world)
    (run-environment world)
    (setf *SURE-HIT-DIST* SHD)
    (if (and (= 1 (agents-alive (db-world-agents world) world)) (find-student (get-percept-grid world))) t nil)))

(defun test-agent-mode-2 (agent-list)
  (let* ((world (make-db-world :game-mode competetive-mode :max-steps (* 100 (list-length agent-list)))))
    (dolist (agent agent-list)
      (add-agent world agent))
    (initialize world)
    (run-environment world)
    ;; print a table of results
    (let ((agents (db-world-agents world)))
      (sort agents #'(lambda (ag1 ag2) (> (agent-score ag1) (agent-score ag2))))
      (print-results agents))))

(defun print-results (agent-list)
  (format t "~&Results:~%")
  (dolist (agent agent-list)
    (format t "~A: ~A~%" (db-agent-body-sname (db-agent-body agent)) agent)))


;;; ==================================================================
;;;; The ball definition


(defstructure (ball (:include object
    (alive? nil)
    (name "B") 
    (size 0.01)))
  "A ball used for hitting other agents")


;;; ==================================================================
;;;; Defining the generic functions


(defmethod termination? ((env db-world))
  "End when everyone dead except one or when"
  (or (and (eq (db-world-game-mode env) single-mode) 
           (or (> (environment-step env) (environment-max-steps env))
               (= 1 (agents-alive (db-world-agents env) env))
               (not (find-student (get-percept-grid env)))))
      (and (eq (db-world-game-mode env) competetive-mode) 
           (or (> (environment-step env) (environment-max-steps env))
               (= 1 (agents-alive (db-world-agents env) env))))))

(defmethod agents-alive (ag-list (env db-world))
  (if (not ag-list) 0
    (+ (if (agent-alive env (agent-body (car ag-list))) 1 0) 
       (agents-alive (cdr ag-list) env))))
   
(defmethod performance-measure ((env db-world) agent)
  "Score 10 for hitting a person and score 20 for each person out while still living."
  (+ (* (agent-hits env (agent-body agent)) 10) 
     (* (agent-outlived env (agent-body agent)) 20)))

(defmethod legal-actions ((env db-world))
  "In the dodgeball world, agents can move around, grab a ball and throw the ball."
  '(go-right go-left go-up go-down grab-ball throw-ball stay stop))


;;; ==================================================================
;;;; Actions


;;; ==================================================================
;;;; grab-ball


(defmethod grab-ball ((env db-world) agent-body &optional args)
  "Agent grabs the ball."
  (declare-ignore args) ;; They are used in other environments
  (let* ((loc (object-loc agent-body)) 
         (ball (ball-at-p env loc)))
    (when (and (agent-alive env agent-body) ball)
      (place-in-container ball agent-body env)
      (agent-got-ball env agent-body))))

(defun ball-at-p (env loc)
  (let* ((x (car loc)) 
         (y (cadr loc)) 
         (objs (objects-at env x y))
         (ball (contains-pred objs "B" #'(lambda (obj name) (equal (object-name obj) name)))))
    ball))

(defmethod objects-at ((env db-world) x y)
  (aref (db-world-grid env) x y))

(defun contains-pred (l o pred &optional (true nil))
  "returns the element if the list contains it. useless for nil element unless the optional true arg is given"
  (if (not l) 
      nil 
    (let ((c (car l))) 
      (if (funcall pred c o) 
          (if true t c)
        (contains-pred (cdr l) o pred true)))))


;;; ==================================================================
;;; The code that does throw-ball
;;; Look at function go-through-dist-list -- you will find it useful in your code
;;; The code checks which squares the ball flies through. It hits any agents in the path with a certain probability
;;; The only function interesting for the student is 'go-through-dist-list. The others can be regarded as private.

(defun points-dist (point-from point-to)
  "return distance between two points
   call e.g. (points-dist '(0 0) '(1 1))"
  (expt (+ 
      (expt (- (car point-from) (car point-to)) 2) 
      (expt (- (cadr point-from) (cadr point-to)) 2))
        0.5))

(defun signum (x) (if (zerop x) x (if (> x 0) 1 -1)))

(defun intersect-list (point-from point-to)
  "private function"
  (let* ((xf (car point-from))
         (yf (cadr point-from))
         (xt (car point-to))
         (yt (cadr point-to))
         (xlist (cross-list xf xt))
         (ylist (cross-list yf yt))
         (resl nil)
         (firstop (if (< xf xt) #'< #'>))
         (secop (if (< yf yt) #'< #'>)))
    (setq resl (sort  
                      (append (cross-where xlist t point-from point-to) (cross-where ylist nil point-from point-to))
                     (lambda (p1 p2) (if (funcall firstop (car p1) (car p2)) 
                                         t (if (eql (car p1) (car p2)) 
                                               (funcall secop (cadr p1) (cadr p2) ) nil )))))
    (setq resl (remove-dups resl))))

(defun go-through-list (il pf)
  "private function
   parameters (intersect-list point-from)
   returns an ordered list of squares that a ball thrown from a specific square will fly through, 
   including the target square."
  (if (not il) nil
    (let ((nextl (car il))
          (restl (cdr il))
          newx newy)
          (if (wholep (car nextl))
              (if (wholep (cadr nextl))
                  (progn
                    (if (= (car nextl) (car pf)) 
                        (setf newx (floor (- (car nextl) 1)))
                      (setf newx (floor (car nextl))))
                    (if (= (cadr nextl) (cadr pf))
                        (setf newy (floor (- (cadr nextl) 1)))
                      (setf newy (floor (cadr nextl))))
                    (return-from go-through-list (cons `(,newx ,newy) (go-through-list (cdr il) `(,newx ,newy))))
                    )
                (progn
                  (setf newy (floor (cadr pf)))
                  (if (= (floor (car nextl)) (car pf))
                      (setf newx (floor (- (car nextl) 1)))
                    (setf newx (floor (car nextl))))
                  (return-from go-through-list (cons `(,newx ,newy) (go-through-list (cdr il) `(,newx ,newy))))
                  ))
            (progn ;whole-number intersection with a y-line
              (setf newx (floor (car pf)))
              (if (= (floor (cadr nextl)) (cadr pf)) ; floor function not needed here, probably
                  (setf newy (floor (- (cadr nextl) 1)))
                (setf newy (floor (cadr nextl))))
              (return-from go-through-list (cons `(,newx ,newy) (go-through-list (cdr il) `(,newx ,newy)))))))))

(defun wholep (num)
  "private function"
  (= (floor num) num))

(defun go-through-dist-list (pf pt &optional (gtl nil gtl-supplied-p))
  "parameters point-from, point-to. 
   Returns a list of squares the ball flies over together with their distance from the source square.
   Use this function when deciding where to throw the ball if unsure of the trajectory."
  (when (null gtl-supplied-p) (setf gtl (go-through-list (intersect-list pf pt) pf)))
  (let ((resl nil) (currcons nil) tmp)
    (if (null gtl)
        nil
      (cons (list (first gtl) (points-dist pf (first gtl))) (go-through-dist-list pf pt (rest gtl))))))
    

(defun remove-dups (sortedl)
  "private function
   remove duplicities in the sorted list"
  (let ((newl nil) tmp)
    (dolist (x sortedl)
      (when (not (equalp x (car newl))) (push x newl)))
    (reverse newl)))

(defun ltoints (l)
  "private function"
  (mapcar #'toints l))

(defun toints (l)
  "private function"
    (list (floor (car l)) (floor (cadr l))))

(defun cross-list (fx tx)
  "private function
   point-from and point-to are squares in the grid. Imagine a line connecting centers of the squares"
  (let* ((res-list nil)
         (dif (- tx fx))
         (sigdif (signum dif))
         (timesdif (* dif sigdif))
         temp (rev t))
    (when (< tx fx) (setf temp fx) (setf fx tx) (setf tx temp) (setf rev nil)) ; reverse the output when done
    (dotimes (num timesdif nil) (push (+ fx 1 num) res-list))
    (if rev (reverse res-list) res-list)))

(defun cross-where (cross-list x? point-from point-to)
  "private function
   return coordinates of intersections with lines in cross-list. First add 0.5 to the point coords"
  (let ((mylist nil)
        (xf (+ 0.5 (car point-from)))
        (yf (+ 0.5 (cadr point-from)))
        (xt (+ 0.5 (car point-to)))
        (yt (+ 0.5 (cadr point-to))))
    (if x?
        (dolist (x cross-list) (setf mylist (cons `(,x 0 ,x 1 ,xf ,yf ,xt ,yt) mylist))) 
      (dolist (x cross-list) (setf mylist (cons `(0 ,x 1 ,x ,xf ,yf ,xt ,yt) mylist))))
    (reverse (mapcar #'(lambda (y) (apply #'get-intersection y)) mylist))))

(defun get-intersection (x1 y1 x2 y2 x3 y3 x4 y4)
  "private function
   return intersection of the two lines"
  (let ((denom (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4))))
        (ua-num (- (* (- (* x1 y2) (* y1 x2)) (- x3 x4)) (* (- x1 x2) (- (* x3 y4) (* y3 x4)))))
        (ub-num (- (* (- (* x1 y2) (* y1 x2)) (- y3 y4)) (* (- y1 y2) (- (* x3 y4) (* y3 x4))))))
    (cond 
     ((zerop denom) (if (zerop ua-num) 'all nil))
     (t (list (/ ua-num denom) (/ ub-num denom))))))

(defmethod fly-the-ball ((env db-world) ball point-from point-to)
  "Make the ball fly towards the point-to. Any agent standing between point-from and point-to can be hit by the ball.
If the agent is hit, the ball drops on his square. The ball will hit the agent with probability depending on his distance
from the agent that threw the ball. 
Dist < SURE-HIT-DIST = 100% hit. 
Dist => SURE-HIT-DIST, Dist < CAN-HIT-DIST = 50% + (CAN-HIT-DIST - dist)/(CAN-HIT-DIST)/2 % hit. 
Dist >= CAN-HIT-DIST = 0% hit."
  (let ((gtl (go-through-dist-list point-from point-to))
        (agents (db-world-agents env)))
    (dolist (point-dist gtl)
      (let* ((ag-point (car point-dist))
             (ag-dist (cadr point-dist))
             (ag (contains-pred agents 
                                ag-point
                                #'(lambda (ag1 point) (equal (object-loc (agent-body ag1)) point)) ))
             (agb (if (eq ag nil) nil (agent-body ag))))
        (when (and agb (agent-alive env agb)
                   (or (< ag-dist *SURE-HIT-DIST*) 
                       (and (< ag-dist *CAN-HIT-DIST*) (>= (random (* *CAN-HIT-DIST* 2)) (floor ag-dist)))))
          ;;(format t "an agent was hit~%")
          (let* ((ag-source (contains-pred agents 
                                           point-from
                                           #'(lambda (ag1 point) (equal (object-loc (agent-body ag1)) point)) )))
            ;(print ag-source)
            (agent-scored-hit env (agent-body ag-source)))
          (agent-lost-life env agb )
          ;;(format  t "Putting ball at ~A~%." ag-point)
          (place-object ball ag-point env)
          (return-from fly-the-ball t)))))
  ;;(format  t "Putting ball at ~A~%." point-to)
  (place-object ball point-to env) 
  nil)

(defmethod throw-ball ((env db-world) agent-body x y &optional args)
  "Agent throws the ball and possibly hits another agent with a ball."
  (declare-ignore args) ;; They are used in other environments
  (let* ((loc (object-loc agent-body))
         (point-to (@ x y))
         (dims (array-dimensions (db-world-grid env)))
         (dimx (car dims))
         (dimy (cadr dims)))
    ;;(format t "x: ~A y: ~A~%" x y)
    (if (and (agent-alive env agent-body) (agent-has-ball env agent-body) (< x (- dimx 1)) (> x 0) (< y (- dimy 1)) (> y 0))
        ;; if the agent holds the ball and is not throwing into a wall, then:
      (let ((ball (car (object-contents agent-body))))
        ;;(format t "throwing ball~%")
        (setf (object-contents agent-body) nil)
        (setf (object-container ball) nil)
        
        (fly-the-ball env ball loc point-to)
        (agent-threw-ball env agent-body))
        ;; If point-to contains an agent, then hit the agent with some probability... 
        ;; Also try to hit the agents on the fly.
      (format t "Cannot throw the ball~%"))))


;;; ==================================================================
;;;; go-left,right,up,down, stay


(defmethod go-left ((env db-world) agent-body &optional (args 1))
  "Go args steps to the left."
  (and (agent-alive env agent-body) (not (agent-has-ball env agent-body )) (move-all-by env agent-body '(-1 0))))
         
(defmethod go-right ((env db-world) agent-body &optional (args 1))
  "Go args steps to the right."
  (and (agent-alive env agent-body) (not (agent-has-ball env agent-body )) (move-all-by env agent-body '(1 0))))

(defmethod go-up ((env db-world) agent-body &optional (args 1))
  "Go args steps up."
   (and (agent-alive env agent-body) (not (agent-has-ball env agent-body )) (move-all-by env agent-body '(0 1))))

(defmethod go-down ((env db-world) agent-body &optional (args 1))
  "Go args steps down"
   (and (agent-alive env agent-body) (not (agent-has-ball env agent-body )) (move-all-by env agent-body '(0 -1))))

(defmethod stay ((env db-world) agent-body &optional (args 1))
  "do not move."
  t)

(defmethod move-all-by ((env db-world) agent-body dir)
  "Move the agent and the ball if he has it"
  (move-object-by agent-body dir env)
  (when (and (> (length (object-contents agent-body)) 0) 
             (eq (car (object-contents agent-body)) (db-world-ball env)))
    (move-object-by (db-world-ball env) dir env))) 


;;; ==================================================================
;;;; Stop world evaluation


(defmethod stop ((env db-world) agent-body &optional args)
  "Stop db world execution. Use for debugging only."
  (setf (environment-max-steps env) 0))


;;; ==================================================================
;;;; Other code, replacing older methods and taking care of bumping agents


(defmethod print-structure ((object db-agent-body) stream)
  "Overload from grid-env: Show an object's name"
  (let ((name (or (object-name object) (type-of object)))
        (cont (object-contents object)))
    (if cont 
        (format stream "~A." name )
      (format stream "~A" name ))))

(defmethod print-structure ((object percept-object) stream)
  "Show an object's name"
    (format stream "~A" (percept-object-name object)))

(defmethod print-structure ((object percept-object-agent) stream)
  "Show an agent's name"
    (format stream "~A-~A" (percept-object-name object) (percept-object-agent-lives object))
    (if (percept-object-agent-has-ball object) (format stream ".")))

(defmethod initialize ((env db-world))
  "Build a new environment with all the agents and objects in place.
  This gets passed an environment which may need to have the objects placed.
  See PARSE-SPECS below in this file for more on initialization."
  (unless (environment-initialized env)
    (call-next-method)
    ;; put all agents on random spaces
    ;;(format t "moving agents randomly")
    (loop for ag in (db-world-agents env) do (move-object-to (agent-body ag) (random-loc env :if 'free-loc?) env))
    ;; initialise the hits, outlived, lives and ball-holding alists
    (loop for ag in (db-world-agents env) do 
    	    (push (cons (agent-body ag) 3) (db-world-agents-lives env))
    	    (push (cons (agent-body ag) 0) (db-world-agents-hits env))
    	    (push (cons (agent-body ag) 0) (db-world-agents-outlived env))
    	    (push (cons (agent-body ag) nil) (db-world-agents-at-ball env))
    	    (push (cons (agent-body ag) nil) (db-world-agents-holding env)))))

(defmethod add-agent ((env db-world) ag)
  "add an agent to the world, on an arbitrary position"
  (push ag (db-world-aspec env)))

(defun my-copy-array (array)
  "http://lemonodor.com/archives/000100.html"
  (let (lin-array (dims (array-dimensions array)) lin-copied-array final-array)
    (setf lin-array (make-array (apply #'* dims) :displaced-to array))
    (setf lin-copied-array (copy-seq lin-array))
    (setf final-array (make-array dims :displaced-to lin-copied-array))))

(defun update-holding (env)
  "Update the counter that says how many turns some agent is holding the ball or standing on the ball's square. 
   If a limit of 3 is exceeded, kill the agent.
   Find agents that stand on the same square as is the ball"
  (dolist (agent (db-world-agents env))
    (if (or (agent-has-ball env (agent-body agent)) (ball-at-p env (object-loc (agent-body agent))))
      (agent-at-ball env (agent-body agent)) (agent-not-at-ball env (agent-body agent))))
  (increase-time-with-ball env))

(defmethod update-fn ((env db-world))
  "Modify the environment, based on agents actions. Also inhibit actions that would result in an agent bump."
  (update-holding env)
  (deactivate-dead env)
  (forbid-crashes env) ; this function is the reason why grab-ball is executed only after movement
  ;; put the ball-throwing agents first:
  (sort-agents env) 
  (execute-agent-actions env))

(defun deactivate-dead (env)
  "Change the actions of dead agents to 'stay."
  (dolist (agent (db-world-agents env))
    (when (not (agent-alive env (agent-body agent))) (setf (agent-action agent) 'stay))))

(defun is-action-throw-ball (agent)
  "true if the action is throw-ball"
  (and (listp (agent-action agent)) (eq (car (agent-action agent)) 'throw-ball)))

(defun is-action-grab-ball (agent)
  "true if the action is grab-ball"
  (eq (agent-action agent) 'grab-ball))


(defun sort-agents (env)
  "Put the agent that throws the ball on the first position in the list of agents."
  (setf (db-world-agents env) 
    (sort (db-world-agents env) 
          #'(lambda (agx agy) 
               (is-action-throw-ball agx)))))
  
  
  
(defun forbid-crashes (env)
  "Make sure that agents do not bump into each other."
  (let* ((grid (db-world-grid env))
         (agents (db-world-agents env))
         (want-to-grid (make-array (array-dimensions grid) :initial-element nil))
         (taken '(nil nil))
         (agents-want-to nil))
    
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
        (when (find-object-if #'obstacle-p (list numberx numbery) env)
          (setf (aref want-to-grid numberx numbery) taken))))
    ; The algorithm decides whether an agent may move on a selected square
    ; he may not if this square contains an obstacle, another agent that does not move
    ; or is a target of another agent
    ; if this forbidden square contains a ball, the ball is displaced on a random location
    
    ; mark squares as interested
    ; if no conflict, the agents may be moved
    ; otherwise the agents that wanted to move there cannot do so and stay where they are (taken=1)
    ; in such case the whole process must be redone for the remaining agents until there is no conflict.
    ; all agents shall place a pointer to themselves on the square that interests them
    
    (dolist (agent agents)
      (let* ((loc (object-loc (agent-body agent)))
             (target (list (car loc) (cadr loc))))
        (case (op (agent-action agent))
          (go-up (setf (cadr target) (+ (cadr target) 1)))
          (go-down (setf (cadr target) (- (cadr target) 1)))
          (go-right (setf (car target) (+ (car target) 1)))
          (go-left (setf (car target) (- (car target) 1)))
          (otherwise t))
        (when (agent-alive env (agent-body agent))
          (push agent (aref want-to-grid (car target) (cadr target)))
          (push (cons agent target) agents-want-to))))
    
    ;; check the agent actions; the agents are in conses together with their target positions
    (let ((change t))
      (loop while change do
            (setf change nil)
            (dolist (agent-where agents-want-to)
              (let* ((tox (car (cdr agent-where)))
                     (toy (cadr (cdr agent-where)))
                     (loc (object-loc (agent-body (car agent-where)))))
                (when (and (not (member (agent-action (car agent-where)) '(bump stay throw-ball grab-ball stop)))
                           (> (list-length (aref want-to-grid tox toy)) 1))
                  (setf (aref want-to-grid (car loc) (cadr loc)) taken)
                  (setf change t)
                  (setf (agent-action (car agent-where)) 'bump)
                  (let ((ball (ball-at-p env (list tox toy))))
                    (when ball
                      (move-object-to ball (random-loc env :if 'free-loc?) env)))))))))) ;move ball somewhere else


#|
;;; ==================================================================
;;; Testing code
;;; follows a sample code that will initialize the world with two agents and one asking agent

;(setf world (make-db-world))
;(add-agent world 'ask-user-db-agent)
;(add-agent world 'wait-and-throw-db-agent)
;(add-agent world 'wait-and-throw-db-agent)
;(initialize world)
;(setf (db-world-max-steps world) 100)
;(run-environment world)
;(test-agent-mode-1 'ask-user-db-agent)

;;; display the environment:
;(display-environment-snapshot world)

|#
