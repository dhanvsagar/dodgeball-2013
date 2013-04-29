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

(cesta nil)
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

(setf nearestenemy (findnearestenemy (object-loc agent-body) (findnextenemy 0 0 percept)))
;(format t "~&debug1")

;(if (<= (length (findnextenemy 0 0 percept)) 1)
;(format t 
;     "~&action (go-right go-up go-left go-down grab-ball throw-ball stop stay)? ")
;)  

;(when (<= (length (findnextenemy 0 0 percept)) 1)
;(return-from my-agent-program (parse-line (read-line)))
;)

(when (null (car nearestenemy))
(return-from my-agent-program 'stop)
)
;(format t "~&debug2")

(if (and    (not (null (find-ball-location percept)))
            (= 
                  (car (object-loc agent-body)) 
                  (car (find-ball-location percept))
            )
            (=
                  (cadr (object-loc agent-body)) 
                  (cadr (find-ball-location percept))
            )
    )
(setf (my-agent-body-cesta agent-body) nil)
)


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
(if (and (not (object-contents agent-body)) (not (find-ball-location percept)))
(setf (my-agent-body-cesta agent-body) nil)
)
(when (and (not (object-contents agent-body)) (not (find-ball-location percept)))
(return-from my-agent-program 'stay)
)
;(format t "~&debug3")
(when (and (object-contents agent-body) (aminext (object-loc agent-body) nearestenemy))
(return-from my-agent-program `(throw-ball ,@nearestenemy))
)
(when (object-contents agent-body)
(return-from my-agent-program `(throw-ball ,@(wheretothrow (object-loc agent-body) nearestenemy)))
)

(if (null (car (my-agent-body-cesta agent-body)))
(setf (my-agent-body-cesta agent-body) 
      (pathway (object-loc agent-body) (find-ball-location percept) percept "nowhere"))
)
(when (null (car (my-agent-body-cesta agent-body)))
(return-from my-agent-program 'stop)
)

(setf wheretogonext (car (my-agent-body-cesta agent-body)))
(setf (my-agent-body-cesta agent-body) (cdr (my-agent-body-cesta agent-body)))

(when (equal wheretogonext "go-right")
(return-from my-agent-program 'go-right)
)
(when (equal wheretogonext "go-left")
(return-from my-agent-program 'go-left)
)
(when (equal wheretogonext "go-up")
(return-from my-agent-program 'go-up)
)
(when (equal wheretogonext "go-down")
(return-from my-agent-program 'go-down)
)
(when (equal wheretogonext "stay")
(return-from my-agent-program 'stay)
)

'stop

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



(defun pathway (mylocation wheretogo grid wherenottogo)
"returns a list of waypoints, last waypoint is stay 
or returns a list with car equal to nil and cdr equal to nil, however it is a 1 element list"
(let  (                
             (priorities (transformpriorities mylocation (gopriority mylocation wheretogo)))
             (locationoccupied nil)
             (prioritiesnames (gopriority mylocation wheretogo))
             (opposites (list nil nil nil nil))

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

(if (equal (car prioritiesnames) "go-left") (setf (car opposites) "go-right"))
(if (equal (car prioritiesnames) "go-right") (setf (car opposites) "go-left"))
(if (equal (car prioritiesnames) "go-down") (setf (car opposites) "go-up"))
(if (equal (car prioritiesnames) "go-up") (setf (car opposites) "go-down"))

(if (equal (cadr prioritiesnames) "go-left") (setf (cadr opposites) "go-right"))
(if (equal (cadr prioritiesnames) "go-right") (setf (cadr opposites) "go-left"))
(if (equal (cadr prioritiesnames) "go-down") (setf (cadr opposites) "go-up"))
(if (equal (cadr prioritiesnames) "go-up") (setf (cadr opposites) "go-down"))

(if (equal (caddr prioritiesnames) "go-left") (setf (caddr opposites) "go-right"))
(if (equal (caddr prioritiesnames) "go-right") (setf (caddr opposites) "go-left"))
(if (equal (caddr prioritiesnames) "go-down") (setf (caddr opposites) "go-up"))
(if (equal (caddr prioritiesnames) "go-up") (setf (caddr opposites) "go-down"))

(if (equal (cadddr prioritiesnames) "go-left") (setf (cadddr opposites) "go-right"))
(if (equal (cadddr prioritiesnames) "go-right") (setf (cadddr opposites) "go-left"))
(if (equal (cadddr prioritiesnames) "go-down") (setf (cadddr opposites) "go-up"))
(if (equal (cadddr prioritiesnames) "go-up") (setf (cadddr opposites) "go-down"))


(if (not (equal (car prioritiesnames) wherenottogo))
(when (not (null 
               (car  
                      (last (pathway (car priorities) wheretogo grid (car opposites)))
               )
           )
      )               
      (return-from pathway (cons (car prioritiesnames) (pathway (car priorities) wheretogo grid (car opposites))))         
))
(if (not (equal (cadr prioritiesnames) wherenottogo))
(when (not (null 
               (car  
                      (last (pathway (cadr priorities) wheretogo grid (cadr opposites)))
               )
           )
      )               
      (return-from pathway (cons (cadr prioritiesnames) (pathway (cadr priorities) wheretogo grid (cadr opposites))))         
))
(if (not (equal (caddr prioritiesnames) wherenottogo))
(when (not (null 
               (car  
                      (last (pathway (caddr priorities) wheretogo grid (caddr opposites)))
               )
           )
      )               
      (return-from pathway (cons (caddr prioritiesnames) (pathway (caddr priorities) wheretogo grid (caddr opposites))))         
))
(if (not (equal (cadddr prioritiesnames) wherenottogo))
(when (not (null 
               (car  
                      (last (pathway (cadddr priorities) wheretogo grid (cadddr opposites)))
               )
           )
      )               
      (return-from pathway (cons (cadddr prioritiesnames) (pathway (cadddr priorities) wheretogo grid (cadddr opposites))))         
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

