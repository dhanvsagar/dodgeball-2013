;;; ================================================================
;;; Student's agent definition:

;; This is to be defined when designing a new student agent 
;
(defconstant kokorigo-my-name "kokorigo")
(defconstant kokorigo-my-short-name "kk")

(defstructure (kokorigo    ; replace "my-agent" by your unique name, as e.g. FIT username
                (:include db-agent 
                          (body (make-kokorigo-my-agent-body))
                          (program 'kokorigo-my-agent-program)
                          (name kokorigo-my-name))) 
              "Your agent for db-world.")

(defstructure (kokorigo-my-agent-body 
                (:include db-agent-body (name kokorigo-my-short-name) (sname kokorigo-my-short-name)))

              (kolbezpriblizeni 0)
              (poslednivzdalenost -1)
              )
;  (slot1 default1)  ; any specific extra slots your agent's body would need
;  ...
;  (slotn defaultn))
;    ;
;    )
;
(defun kokorigo-my-agent-program (percept)
  (let ((agent-body (first percept))

        (nearestenemy nil)
        (soucasnavzdalenost -1)
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
    (setf weakestnear (kokorigo-findleastlives (object-loc agent-body) (kokorigo-findnextenemy 0 0 percept) percept))


    (setf nearestenemy (kokorigo-findnearestenemy (object-loc agent-body) (kokorigo-findnextenemy 0 0 percept)))


    ;(format t "~&debug1")

    ;(if (null (car nearestenemy))
    ;(format t 
    ;     "~&action (go-right go-up go-left go-down grab-ball throw-ball stop stay)? ")
    ;)  

    ;(when (null (car nearestenemy))
    ;(return-from kokorigo-my-agent-program (parse-line (read-line)))
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
      (return-from kokorigo-my-agent-program 'grab-ball)
      )
    ;(format t "~&debug2.5")


    (when (and (not (object-contents agent-body)) (not (find-ball-location percept)))
      (return-from kokorigo-my-agent-program 'stay)
      )

    ;only works if the ball is on the field!!!!
    ( if (find-ball-location percept)
         (dolist (item (kokorigo-findnextenemy 0 0 percept))

           (if  
             (and (cadar(last(go-through-dist-list item (find-ball-location percept)))) 
                  (or (kokorigo-< soucasnavzdalenost 0)
                      (kokorigo-> soucasnavzdalenost 
                         (cadar(last(go-through-dist-list item (find-ball-location percept)))) 
                         )
                      ))
             (setf soucasnavzdalenost (cadar(last(go-through-dist-list item (find-ball-location percept)))))
             )

           (if(null (cadar(last(go-through-dist-list item (find-ball-location percept)))))

             (setf soucasnavzdalenost 0)
             )

           (if (kokorigo-< 
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

    (if(and 
         (kokorigo-> soucasnavzdalenost -1)  
         (>= soucasnavzdalenost (kokorigo-my-agent-body-poslednivzdalenost agent-body)) 
         (kokorigo-> (kokorigo-my-agent-body-poslednivzdalenost agent-body) -1) 
         )


      (setf (kokorigo-my-agent-body-kolbezpriblizeni agent-body) 
            (+ (kokorigo-my-agent-body-kolbezpriblizeni agent-body) 1)   )

      (setf (kokorigo-my-agent-body-kolbezpriblizeni agent-body) 0)

      )

    (setf (kokorigo-my-agent-body-poslednivzdalenost agent-body) soucasnavzdalenost)


    (when (and      (find-ball-location percept)
                    (kokorigo-aminext (find-ball-location percept) (object-loc agent-body))
                    (=  (- (car (find-ball-location percept)) (car (object-loc agent-body))) -1)
                    )
      (return-from kokorigo-my-agent-program 'go-left)
      )

    (when (and        (find-ball-location percept)
                      (kokorigo-aminext (find-ball-location percept) (object-loc agent-body))
                      (=  (- (car (find-ball-location percept)) (car (object-loc agent-body))) 1)
                      )
      (return-from kokorigo-my-agent-program 'go-right)
      )

    (when (and       (find-ball-location percept)
                     (kokorigo-aminext (find-ball-location percept) (object-loc agent-body))
                     (=  (- (cadr (find-ball-location percept)) (cadr (object-loc agent-body))) 1)
                     )
      (return-from kokorigo-my-agent-program 'go-up)
      )

    (when (and        (find-ball-location percept)
                      (kokorigo-aminext (find-ball-location percept) (object-loc agent-body))
                      (=  (- (cadr (find-ball-location percept)) (cadr (object-loc agent-body))) -1)
                      )
      (return-from kokorigo-my-agent-program 'go-down)
      )



    (if (and (find-ball-location percept)

             (not (object-contents agent-body)))
      (setf movpriorities
            (kokorigo-transformpriorities (object-loc agent-body)
                                 (kokorigo-gopriority (object-loc agent-body) (find-ball-location percept)))
            ))


    (setf wheretogonext nil)


    (if(and (not (object-contents agent-body))
            nearesttoball
            (kokorigo-<
              (kokorigo-my-agent-body-kolbezpriblizeni agent-body) 
              5
              )
            (null (aref percept 
                        (+  (car (object-loc agent-body)) (- (car (object-loc agent-body)) (caar movpriorities)))
                        (+  (cadr (object-loc agent-body)) (- (cadr (object-loc agent-body)) (cadar movpriorities)))
                        )  
                  )
            (kokorigo-<
              (cadar(last(go-through-dist-list (object-loc agent-body) (find-ball-location percept)))) 
              5
              )
            (kokorigo-<=
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
             (kokorigo-<
               (kokorigo-my-agent-body-kolbezpriblizeni agent-body) 
               5
               )
             (null (aref percept 
                         (+  (car (object-loc agent-body)) (- (car (object-loc agent-body)) (caadr movpriorities)))
                         (+  (cadr (object-loc agent-body)) (- (cadr (object-loc agent-body)) (cadadr movpriorities)))
                         )  
                   )
             (kokorigo-<
               (cadar(last(go-through-dist-list (object-loc agent-body) (find-ball-location percept)))) 
               5
               )
             (kokorigo-<=
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
            (kokorigo-<
              (kokorigo-my-agent-body-kolbezpriblizeni agent-body) 
              5
              )
            (null (aref percept 
                        (+  (car (object-loc agent-body)) (- (car (object-loc agent-body)) (caadr (cdr movpriorities))))
                        (+  (cadr (object-loc agent-body)) (- (cadr (object-loc agent-body)) (cadadr (cdr movpriorities))))
                        )  
                  )
            (kokorigo-<
              (cadar(last(go-through-dist-list (object-loc agent-body) (find-ball-location percept)))) 
              5
              )
            (kokorigo-<=
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
            (kokorigo-<
              (kokorigo-my-agent-body-kolbezpriblizeni agent-body) 
              5
              )
            )
      (setf wheretogonext (list (car (object-loc agent-body))  (cadr (object-loc agent-body))  ))
      )

    (if(and
         (not (object-contents agent-body))
         (null wheretogonext)
         (or(null nearesttoball)
           (>=
             (kokorigo-my-agent-body-kolbezpriblizeni agent-body) 
             5
             )

           )
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
         (or(null nearesttoball)
           (>=
             (kokorigo-my-agent-body-kolbezpriblizeni agent-body) 
             5
             )

           )
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
               (or(null nearesttoball)
                 (>=
                   (kokorigo-my-agent-body-kolbezpriblizeni agent-body) 
                   5
                   )

                 )
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
      (return-from kokorigo-my-agent-program `(throw-ball ,@weakestnear))
      )



    (setf movpriorities
          (kokorigo-transformpriorities (object-loc agent-body)
                               (kokorigo-gopriority (object-loc agent-body) nearestenemy))
          )

    (when(and(object-contents agent-body)  

           (null weakestnear)
           )
      (return-from kokorigo-my-agent-program `(throw-ball ,@(car movpriorities)))
      )


    (when (and wheretogonext
               (not (object-contents agent-body))
               (= (cadr (object-loc agent-body)) (cadr wheretogonext))
               (= (car (object-loc agent-body)) (car wheretogonext))
               )
      (return-from kokorigo-my-agent-program 'stay)
      )
    (when (and wheretogonext
               (not (object-contents agent-body))
               (= (cadr (object-loc agent-body)) (- (cadr wheretogonext) 1))
               (= (car (object-loc agent-body)) (car wheretogonext))
               )
      (return-from kokorigo-my-agent-program 'go-up)
      )
    (when (and wheretogonext
               (not (object-contents agent-body))
               (= (cadr (object-loc agent-body)) (+ (cadr wheretogonext) 1))
               (= (car (object-loc agent-body)) (car wheretogonext))
               )
      (return-from kokorigo-my-agent-program 'go-down)
      )
    (when (and wheretogonext
               (not (object-contents agent-body))
               (= (cadr (object-loc agent-body)) (cadr wheretogonext))
               (= (car (object-loc agent-body)) (- (car wheretogonext) 1))
               )
      (return-from kokorigo-my-agent-program 'go-right)
      )
    (when (and wheretogonext
               (not (object-contents agent-body))
               (= (cadr (object-loc agent-body)) (cadr wheretogonext))
               (= (car (object-loc agent-body)) (+ (car wheretogonext) 1))
               )
      (return-from kokorigo-my-agent-program 'go-left)
      )

    'stay

    ;(if(object-contents agent-body)(format t "~&drzim balon")(format t "~&nedrzim balon"))





    ;(setf nearestenemy (kokorigo-findnearestenemy (object-loc agent-body) (kokorigo-findnextenemy 0 0 percept)))
    ;(if (null nearestenemy) (format t "~& no enemies on the field~&")
    ;(format t "~&nearest enemy is: ~D, ~D~&" (car nearestenemy)(cadr nearestenemy)))

    ;(format t "~&throw at: ~D, ~D" (car(kokorigo-wheretothrow (object-loc agent-body) nearestenemy))
    ;(cadr(kokorigo-wheretothrow (object-loc agent-body) nearestenemy)))

    ;(if (find-ball-location percept) (format t "~&mic je na pozici ~D, ~D~&" 
    ;(car (find-ball-location percept))
    ;(cadr (find-ball-location percept))
    ;)
    ;(format t "~& mic neni nikde~&"))

    ;(format t "~&go priority: ~A, ~A, ~A, ~A"
    ;(car(kokorigo-gopriority (object-loc agent-body) (find-ball-location percept)))
    ;(cadr(kokorigo-gopriority (object-loc agent-body) (find-ball-location percept)))
    ;(caddr(kokorigo-gopriority (object-loc agent-body) (find-ball-location percept)))
    ;(cadddr(kokorigo-gopriority (object-loc agent-body) (find-ball-location percept)))
    ;)

    ;(if (find-ball-location percept)
    ;(dolist (item (pathway (object-loc agent-body) (find-ball-location percept) percept "nowhere"))
    ;(format t "~A " item)
    ;
    ;)
    ;)

    ;is true (if(typep agent-body 'kokorigo-my-agent-body)(format t "~&ano, je to spravnej typ~&")) 
    ;is true (if(typep agent-body 'object)(format t "~&ano, je to spravnej typ2~&"))


    ;(if (and (= (car (object-loc agent-body)) 2) (= (cadr (object-loc agent-body)) 5)) (setf (kokorigo-my-agent-body-cesta agent-body) 2))


    ;(format t "~&global variable je: ~D~&" (kokorigo-my-agent-body-cesta agent-body))





    ) )
; for example:
; (nth (random 4) '(go-left go-right go-up go-down))
;    ) )


(defun kokorigo-aminext (mylocation enemylocation)
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
    (return-from kokorigo-aminext t)
    )
  nil
  )

(defun kokorigo-alreadyvisited (priorities previouslyvisited)
  "takes priorities (a list of 4 lists (x y)) and the previously visited coordinates
  and returns modofied priorities"
  (let ((resultlist (list (list -1 -1) (list -1 -1) (list -1 -1) (list -1 -1))))

    (when (null (car previouslyvisited)) 
      (return-from kokorigo-alreadyvisited (list (list -1 -1) (list -1 -1) (list -1 -1) (list -1 -1)))
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
      (return-from kokorigo-alreadyvisited
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



(defun kokorigo-pathway (mylocation wheretogo grid previousway)
  "returns a list of waypoints, last waypoint is stay 
  or returns a list with car equal to nil and cdr equal to nil, however it is a 1 element list"
  (let*  (                
                          (priorities (kokorigo-transformpriorities mylocation (kokorigo-gopriority mylocation wheretogo)))
                          (locationoccupied nil)
                          (prioritiesnames (kokorigo-gopriority mylocation wheretogo))
                          (wherenottogo (kokorigo-alreadyvisited priorities previousway))

                          )
    (when (and (= (car mylocation) (car wheretogo)) (= (cadr mylocation) (cadr wheretogo)))
      (return-from kokorigo-pathway (list "stay"))
      )
    (dolist (item (aref grid (car mylocation) (cadr mylocation)))
      (if    (or  (equal (percept-object-name item) "#")
                  (equal (percept-object-name item) wait-and-throw-db-agent-name)
                  (typep item 'percept-object-wall)
                  (and (typep item 'percept-object-agent) (not (equal (percept-object-name item) kokorigo-my-short-name)))
                  )
        (setf locationoccupied t)
        )

      )
    (when (null (not locationoccupied)) (return-from kokorigo-pathway (list nil)))


    (if (not (and 
               (= (caar priorities) (caar wherenottogo)) 
               (= (cadar priorities) (cadar wherenottogo) )
               )
             )
      (when (not (null 
                   (car  
                     (last (kokorigo-pathway (car priorities) wheretogo grid (cons mylocation previousway)))
                     )
                   )
                 )               
        (return-from kokorigo-pathway (cons (car prioritiesnames) (kokorigo-pathway (car priorities) wheretogo grid (cons mylocation previousway))))         
        ))
    (if (not (and 
               (= (caadr priorities) (caadr wherenottogo)) 
               (= (cadadr priorities) (cadadr wherenottogo) )
               ))
      (when (not (null 
                   (car  
                     (last (kokorigo-pathway (cadr priorities) wheretogo grid (cons mylocation previousway)))
                     )
                   )
                 )               
        (return-from kokorigo-pathway (cons (cadr prioritiesnames) (kokorigo-pathway (cadr priorities) wheretogo grid (cons mylocation previousway))))         
        ))
    (if (not (and 
               (= (caadr (cdr priorities)) (caadr (cdr wherenottogo))) 
               (= (cadadr (cdr priorities)) (cadadr (cdr wherenottogo)))
               ))
      (when (not (null 
                   (car  
                     (last (kokorigo-pathway (caddr priorities) wheretogo grid (cons mylocation previousway)))
                     )
                   )
                 )               
        (return-from kokorigo-pathway (cons (caddr prioritiesnames) (kokorigo-pathway (caddr priorities) wheretogo grid (cons mylocation previousway))))         
        ))
    (if (not (and 
               (= (caadr (cddr priorities)) (caadr (cddr wherenottogo))) 
               (= (cadadr (cddr priorities)) (cadadr (cddr wherenottogo)))
               ))
      (when (not (null 
                   (car  
                     (last (kokorigo-pathway (cadddr priorities) wheretogo grid (cons mylocation previousway)))
                     )
                   )
                 )               
        (return-from kokorigo-pathway (cons (cadddr prioritiesnames) (kokorigo-pathway (cadddr priorities) wheretogo grid (cons mylocation previousway))))         
        ))


    )
  (list nil)
  )

(defun kokorigo-transformpriorities (mylocation priorities) 
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



(defun kokorigo-gopriority (mylocation wheretogo)
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
    (setf serazenyseznam (sort seznamsmeru #'kokorigo-<))

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
      (return-from kokorigo-gopriority (list (car vysledek) (cadr vysledek) (caddr vysledek) (cadddr vysledek)))
      )
    )
  )

(defun kokorigo-wheretothrow(mylocation enemylocation)
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
      (return-from kokorigo-wheretothrow (list (car mylocation) (cadr mylocation)))
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
            (kokorigo-<= vzdaldown vzdalup)
            (kokorigo-<= vzdaldown vzdalleft)
            (kokorigo-<= vzdaldown vzdalright)
            )
      (return-from kokorigo-wheretothrow (list (car enemylocation) (- (cadr enemylocation) 1) ))                                  
      )

    (when (and
            (kokorigo-<= vzdalup vzdalleft)
            (kokorigo-<= vzdalup vzdalright)
            )
      (return-from kokorigo-wheretothrow (list (car enemylocation) (+ (cadr enemylocation) 1) ))                                  
      )

    (when  (kokorigo-<= vzdalleft vzdalright) 
      (return-from kokorigo-wheretothrow (list (- (car enemylocation) 1)  (cadr enemylocation) ))                                  
      )

    (when t
      (return-from kokorigo-wheretothrow (list (+ (car enemylocation) 1)  (cadr enemylocation)  ))                                  
      )

    )
  )

(defun kokorigo-findleastlives(mylocation enemylocations grid)
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
         and (not (null (car distanceofcandidate))) (kokorigo-> 5 (car distanceofcandidate)) (kokorigo-> leastlivescount livesofcandidate)
         )
        (setf leastlivescount livesofcandidate)
        (setf weakestenemy (list (car item) (cadr item)))
        )



      )

    (
     when (not (null weakestenemy))
     (return-from kokorigo-findleastlives (list (car weakestenemy) (cadr weakestenemy)))
     )

    )

  nil
)

(defun kokorigo-> (x y)
  (and (not (null x)) (not (null y)) (> x y)))

(defun kokorigo-< (x y)
  (and (not (null x)) (not (null y)) (< x y)))

(defun kokorigo->= (x y)
  (and (not (null x)) (not (null y)) (>= x y)))

(defun kokorigo-<= (x y)
  (and (not (null x)) (not (null y)) (<= x y)))

(defun kokorigo-findnearestenemy(mylocation enemylocations)
  "returns nil (there are no enemies on the field) or the location of nearest enemy"
  (let((nearestone nil)(nearestdistance 0)(distancecandidate nil))
    (dolist (item enemylocations)
      (setf distancecandidate (cdar(last(go-through-dist-list mylocation item))))
      (when
        (
         or (kokorigo-> nearestdistance (car distancecandidate)) (= nearestdistance 0)
         )
        (setf nearestdistance (car distancecandidate))
        (setf nearestone (list (car item) (cadr item)))
        )



      )

    (
     when (not (null nearestone))
     (return-from kokorigo-findnearestenemy (list (car nearestone) (cadr nearestone)))
     )

    )

  nil
  )


(defun kokorigo-findnextenemy(fromx fromy grid)
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
                       (not (equal (percept-object-name item) kokorigo-my-short-name)) 
                       (not (equal (percept-object-name item) "B"))
                       (not  (typep item 'percept-object-wall))
                       (not  (typep item 'percept-object-ball))
                       (typep item 'percept-object-agent)
                       )
              (return-from kokorigo-findnextenemy
                           (cons (list (mod i sizex) (/ (- i (mod i sizex)) sizex))
                                 (kokorigo-findnextenemy
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

