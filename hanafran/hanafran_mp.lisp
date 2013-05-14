;hanafran - Hána František FIT 2012/2013

(defconstant hanafran-agent-name "FH")

(defstructure (hanafran
                (:include db-agent 
                          (body (make-ME))
                          (program 'hanafran-my-agent-program)
                          (name hanafran-agent-name))) 
              "Your agent for db-world.")

(defstructure (ME (:include db-agent-body (name hanafran-agent-name))) )

(defun hanafran-my-agent-program (percept)

  (let* ((me (car percept))
         (grid (cadr percept))
         (ball_on_my_loc (member-if (lambda (a) (typep a 'percept-object-ball)) (apply #'aref grid (object-loc me))))
         (holding_ball (object-contents me))
         (my_loc (object-loc me))
         (ball_loc (hanafran-find_ball grid))
         (agent_loc (hanafran-find_clothes_agent my_loc grid))     
         )

    (progn
      ;        (print agent_loc)
      ;        (print (hanafran-get_free_place_near_agent my_loc agent_loc grid))
      ;        `(print ,@(hanafran-get_free_place_near_agent agent_loc grid ))
      (cond 
        ( (not agent_loc) 'stop )
        ( (and holding_ball (= 1 (hanafran-get_distance my_loc agent_loc)) ) `(throw-ball ,@agent_loc ))
        ( holding_ball `(throw-ball ,@(hanafran-get_free_place_near_agent my_loc agent_loc grid )))
        ( ball_on_my_loc 'grab-ball )
        ( (= 1 (hanafran-get_distance ball_loc my_loc)) (hanafran-do_bump my_loc ball_loc) )
        ( (hanafran-do_next_step my_loc ball_loc grid) )

        )
      );end of progn
    ))

(defun hanafran-find_location (X-predicate grid)
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (hanafran-identify_in_list X-predicate (aref grid numberx numbery))
        (return-from hanafran-find_location (list numberx numbery))))) nil )

(defun hanafran-find_all_agents_loc (grid)
  (progn
    (setf res nil)
    (dotimes (numberx (car (array-dimensions grid)))
      (dotimes (numbery (cadr (array-dimensions grid)))
        (when (hanafran-identify_in_list #'hanafran-my_agent (aref grid numberx numbery))
          (setf res (append `((,numberx ,numbery)) res))
          )))
    res
    )
  )

(defun hanafran-find_ball (grid)
  (hanafran-find_location #'hanafran-my_ball grid))

(defun hanafran-find_clothes_agent (my_loc grid)
  (hanafran-get_place_with_min_distance my_loc (hanafran-find_all_agents_loc grid))
  )

(defun hanafran-identify_in_list (pred list)
  (dolist (item list)
    (when (funcall pred item)
      (return-from hanafran-identify_in_list item))) nil)

(defmethod hanafran-my_player ((obj percept-object))
  (if (equal (percept-object-name obj) hanafran-agent-name)
    obj nil))

(defmethod hanafran-my_agent ((obj percept-object))
  (if (and (not (equal (percept-object-name obj) "#")) 
           (not (equal (percept-object-name obj) hanafran-agent-name)) 
           (not (equal (percept-object-name obj) "B")))
    obj nil))

(defmethod hanafran-my_ball ((obj percept-object))
  (if (equal (percept-object-name obj) "B")
    obj nil))

(defun hanafran-do_bump (start_loc end_loc)
  (cond   ((< (car start_loc) (car end_loc)) 'go-right)
          ((> (car start_loc) (car end_loc)) 'go-left)
          ((< (cadr start_loc) (cadr end_loc)) 'go-up)
          ((> (cadr start_loc) (cadr end_loc)) 'go-down)
          (t 'stay)
          )
  )

(defun hanafran-sec_cond (loc ball_loc grid)
  ;(print "SEC COND")
  ;(print loc)
  ;(print (hanafran-is_free loc grid) )
  (or (hanafran-is_free loc grid) 
      )
  )


(defun hanafran-get_distance (start_loc end_loc)
  ( +
    (- (max (car start_loc) (car end_loc)) (min (car start_loc) (car end_loc)))
    (- (max (cadr start_loc) (cadr end_loc)) (min (cadr start_loc) (cadr end_loc)))
    )
  )

(defun hanafran-find_location (X-predicate grid)
  (dotimes (numberx (car (array-dimensions grid)))
    (dotimes (numbery (cadr (array-dimensions grid)))
      (when (hanafran-identify_in_list X-predicate (aref grid numberx numbery))
        (return-from hanafran-find_location (list numberx numbery))))) nil )

;;; return bool
(defun hanafran-is_free(loc grid)
  (let* ( (pos_x (car loc))
         (pos_y (cadr loc))
         (dim_x (1- (car (array-dimensions grid))))
         (dim_y (1- (cadr (array-dimensions grid)))) )
    (cond
      ((>= pos_x dim_x ) nil)
      ((>= pos_y dim_y ) nil)
      ((<= pos_x 0 ) nil)
      ((<= pos_y 0 ) nil)
      ((hanafran-identify_in_list #'hanafran-my_agent (aref grid pos_x pos_y)) nil)
      (t t)
      )
    )
  )
;;; vrací seznam volnych pozic ve ctyr okoli
(defun hanafran-get_ctyrokoli(loc grid)
  (hanafran-if_free_append (list (- (car loc) 1) (cadr loc)) 
                  (hanafran-if_free_append (list (car loc) (- (cadr loc) 1))
                                  (hanafran-if_free_append (list (+ (car loc) 1) (cadr loc))
                                                  (hanafran-if_free_append (list (car loc) (+ 1 (cadr loc))) (list ) grid)
                                                  grid)
                                  grid)
                  grid)
  )

(defun hanafran-if_free_append (loc lst grid)
  (if (hanafran-is_free loc grid) (cons loc lst) lst)
  )

(defun hanafran-get_free_place_near_agent (my_loc agent_loc grid)
  (hanafran-get_place_with_min_distance my_loc (hanafran-get_ctyrokoli agent_loc grid))
  )

(defun hanafran-get_place_with_min_distance (my_loc lst)
  (let* ( (res (car lst)) )
    (mapc #'(lambda (a)
              (if (< (hanafran-get_distance my_loc a) (hanafran-get_distance my_loc res) ) (setf res a))
              )
          (cdr lst)
          )
    res
    )
  )

(defun hanafran-do_next_step (my_loc ball_loc grid)
  (hanafran-my_dijkstra my_loc ball_loc grid)
  )

(defun hanafran-my_dijkstra (start_loc end_loc grid)
  (let* (
         (visited nil)
         (unvisited (hanafran-get_unvisited_places start_loc grid))
         (dim_x  (car (array-dimensions grid)))
         (dim_y  (cadr (array-dimensions grid)))
         (maximum 99)
         (distance_array (make-array `(,dim_x ,dim_y) :initial-element maximum))
         )

    (setf (aref distance_array (car start_loc) (cadr start_loc)) 0)
    ;(print "po init fazi") 
    ;(print dim_y) 

    (setf actual_loc nil)
    ;(print "z  do")
    ;(print start_loc)
    ;(print end_loc)
    (loop while (not (equal actual_loc end_loc)) do 
          (setf actual_loc (hanafran-remove_minimal_unvisited_location unvisited distance_array))
          (setf unvisited (remove actual_loc unvisited :test #'equal))
          ;(print "expanduji") 
          ;(print actual_loc)
          (hanafran-expand_loc actual_loc unvisited distance_array grid) ; ohodnotit sousedy
          )
    ;(print "nasel jsem cestu") 
    ;(print distance_array)

    (setf predecessor_loc end_loc)
    (loop while (not (= 0 (hanafran-get_distance predecessor_loc start_loc))) do 
          (setf predecessor_loc (hanafran-get_predecessor actual_loc distance_array grid))
          ;(print "testuju")
          ;(print predecessor_loc)
          (if (= 0 (hanafran-get_distance predecessor_loc start_loc))
            (setf result-move (hanafran-get_direction predecessor_loc actual_loc))
            (setf actual_loc predecessor_loc)
            )
          )
    result-move
    ) 
  )

(defun hanafran-get_unvisited_places (loc grid)
  (let* ((res nil))
    (dotimes (numberx (- (car (array-dimensions grid)) 2))
      (dotimes (numbery (- (cadr (array-dimensions grid)) 2))
        (when (not (hanafran-identify_in_list #'hanafran-my_agent (aref grid (+ 1 numberx) (+ 1 numbery))))
          (setf res (append `((,(1+ numberx) ,(1+ numbery))) res))
          )))
    res
    ))

;;; vrací minimální polohy ze zadaného pole
(defun hanafran-remove_minimal_unvisited_location (unvisited d_array)
  (setf res (car unvisited))
  (mapc #'(lambda (a)
            (if (< (aref d_array (car a) (cadr a)) (aref d_array (car res) (cadr res)) ) (setf res a))
            )
        (cdr unvisited)
        )
  res
  )

(defun hanafran-expand_loc (loc unvisited d_array grid)
  (mapc #'(lambda (a)
            (if (< (1+ (aref d_array (car loc) (cadr loc))) (aref d_array (car a) (cadr a)) )
              (setf (aref d_array     (car a)  (cadr a))  (1+ (aref d_array (car loc) (cadr loc)))) 
              )
            ) (hanafran-get_ctyrokoli loc grid) )
  d_array
  )

(defun hanafran-get_predecessor (loc d_array grid)
  (let* ( (res nil) )
    (mapc #'(lambda (a)
              ;(print "predecessor")
              ;(print a)
              ;(print (1+ (aref d_array (car a) (cadr a))))
              ;(print "-------")
              (if (= (1+ (aref d_array (car a) (cadr a))) (aref d_array (car loc) (cadr loc)) ) (setf res a))
              ) (hanafran-get_ctyrokoli loc grid) )
    ;(print "zkoncil mapc")
    ;(print res)
    res ))

(defun hanafran-get_direction (start_loc end_loc)
  ;(print "hanafran-get_direction")
  (cond   ((< (car start_loc) (car end_loc)) 'go-right)
          ((> (car start_loc) (car end_loc)) 'go-left)
          ((< (cadr start_loc) (cadr end_loc)) 'go-up)
          ((> (cadr start_loc) (cadr end_loc)) 'go-down)
          (t 'stay)
          )
  )


