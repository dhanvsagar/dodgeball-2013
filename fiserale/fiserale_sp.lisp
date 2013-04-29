(defstructure
	(fiserale 
		(:include db-agent 
			(program 'fiserale-program) 
			(body (make-fiserale-agent-body)) 
			(name "fiserale")) ) 
    "An agent good enough to defeat EVIL computer agents")

(defstructure (fiserale-agent-body 
	(:include db-agent-body 
		(name "fiserale")
		(sname "F9A"))))

(defun fiserale-program (percept)
	(block action
		(let ((myAgentBody (first percept)) (mapArray (second percept)) myAgentPosition ballPosition opponents agentWithBall)
			(dotimes (y (first (array-dimensions mapArray)))
				(dotimes (x (second (array-dimensions mapArray)))
					(dolist (object (aref mapArray x y))
							
						(cond 
							((percept-object-agent-p object)
								(if (equal (percept-object-name object) (percept-object-name myAgentBody))
									(setf myAgentPosition (list x y))
									(setf opponents (cons (list x y) opponents))
								)
								(if (percept-object-agent-has-ball object)
									(setf agentWithBall (list x y))
								)
							)							
							((percept-object-ball-p object)
								(setf ballPosition (list x y))
							)
						)
					)
				)
			)
			
			;;if I am on ball position
			(if (equal myAgentPosition ballPosition)
				(return-from action 'GRAB-BALL)
			)

			;;someone has ball
			(if (null ballPosition)				
				(let* ((target (fiserale-findCloseOpponent myAgentPosition opponents)) (xdiff (- (first myAgentPosition) (first target))) 
												 (ydiff (- (second myAgentPosition) (second target))))
					;;I have ball					
					(if (equal agentWithBall myAgentPosition)
						(progn							
							(cond
								;;if i am enough close to my target
								((and (eq xdiff 0)
										(or (eq ydiff -1) (eq ydiff 1)))
								)
								((and (eq ydiff 0)
										(or (eq xdiff -1) (eq xdiff 1)))
								)
								;;if i am NOT close to my target
								(t 
									(setf target (fiserale-adjustTargetToGoCloser xdiff ydiff target))
								)						
							)
							(return-from action (list 'throw-ball (first target) (second target)))
						)
						;;enemy has ball - impossible in first gametype
						(return-from action (fiserale-findActionToBumpAgent xdiff ydiff))					
					)
				)
			)

			;;ball is on enemy position
			(if (fiserale-is-ball-on-opponent-position ballPosition opponents)
				(let ((xdiff (- (first myAgentPosition) (first ballPosition))) (ydiff (- (second myAgentPosition) (second ballPosition))) )
					(return-from action (fiserale-findActionToBumpAgent xdiff ydiff))					
				)
			)

			;;go for ball
			(return-from action (fiserale-findPerfectTrajectory myAgentPosition ballPosition opponents mapArray))
		)	
	)
)

(defun fiserale-is-position-free-to-move (mapArray coordinates)
	(let (myAgentPosition opponents)
		(dotimes (y (first (array-dimensions mapArray)))
			(dotimes (x (second (array-dimensions mapArray)))
				(dolist (object (aref mapArray x y))
					(cond 
						((percept-object-agent-p object)
							(setf opponents (cons (list x y) opponents))
						)							
					)
				)
			)
		)

		(dolist (opp opponents)
			(if (and (eq (first opp) (first coordinates)) (eq (second opp) (second coordinates)))
				(return-from fiserale-is-position-free-to-move nil)
			)
		)
	)
	(fiserale-is-position-without-wall coordinates)
)

(defun fiserale-is-position-without-wall (coordinates)
	(let ((x (first coordinates)) (y (second coordinates)))
		(cond 
			((eq x 0) nil)
			((eq x 9) nil)
			((eq y 0) nil)
			((eq y 9) nil)
			(t t)
		)
	)
)

(defun fiserale-adjustTargetToGoCloser (xdiff ydiff target)
	(if (> xdiff 0)
		(if (fiserale-is-position-without-wall (list (+ (first target) 1) (second target)))
			(return-from fiserale-adjustTargetToGoCloser (list (+ (first target) 1) (second target)))
		)		
	)
	(if (< xdiff 0)
		(if (fiserale-is-position-without-wall (list (- (first target) 1) (second target)))
			(return-from fiserale-adjustTargetToGoCloser (list (- (first target) 1) (second target)))
		)
	)
	(if (> ydiff 0)
		(if (fiserale-is-position-without-wall (list (first target) (+ (second target) 1)))
			(return-from fiserale-adjustTargetToGoCloser (list (first target) (+ (second target) 1)))
		)		
	)
	(if (< ydiff 0)
		(if (fiserale-is-position-without-wall (list (first target) (- (second target) 1)))
			(return-from fiserale-adjustTargetToGoCloser (list (first target) (- (second target) 1)))
		)
	)
)

(defun fiserale-findActionToBumpAgent (xdiff ydiff)
	(cond
		((< xdiff 0)
			'GO-RIGHT
		)
		((> xdiff 0)
			'GO-LEFT
		)
		((< ydiff 0)
			'GO-UP
		)
		((> ydiff 0)
			'GO-DOWN
		)
	)
)

(defun fiserale-findCloseOpponent (myAgentPosition opponents)
	(let ((closeOpponent (first opponents)) (closeOpponentDist (fiseraleCalculateDist myAgentPosition (first opponents))))
		(dolist (pos (cdr opponents))
			(if (< (fiseraleCalculateDist pos myAgentPosition) closeOpponentDist)
				(progn
					(setf closeOpponent pos)
					(setf closeOpponentDist (fiseraleCalculateDist pos myAgentPosition))
				)
			)
		)
		closeOpponent
	)
)

(defun fiseraleCalculateDist (positionA positionB)
	(let ((diff1 (- (first positionA) (first positionB))) (diff2 (- (second positionA) (second positionB))) )
		(+ (* diff1 diff1) (* diff2 diff2))
	)
)

(defun fiserale-is-ball-on-opponent-position (ballPosition opponents)
	(dolist (opp opponents)
		(if (equal opp ballPosition)
			(return-from fiserale-is-ball-on-opponent-position t)
		)
	)
	nil
)

;;;;;;;;;;;;;;;;;;;functions to find trajectory;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fiserale-findPerfectTrajectory (myAgentPosition ballPosition opponents mapArray)
	(let ((mapTmp (make-array (array-dimensions mapArray) :initial-element 998)))
		;;opponents
		(dolist (opp opponents)
			(setf (aref mapTmp (first opp) (second opp)) 996)			
		)
		;;walls
		(dotimes (y (first (array-dimensions mapTmp)))
			(dotimes (x (second (array-dimensions mapTmp)))
				(if (not (fiserale-is-position-without-wall (list x y)))
					(setf (aref mapTmp x y) 997)
				)		
			)
		)
		;;
		(setf (aref mapTmp (first ballPosition) (second ballPosition)) 0)

		(let ((nm (fiserale-identifyNextMove mapTmp myAgentPosition 0)))
			(setf (aref mapTmp (first myAgentPosition) (second myAgentPosition)) 'A)
			(print mapTmp)
			(print nm)
			nm
		)		
	)
)

(defun fiserale-identifyNextMove (mapTmp myAgentPosition step)
	(let ((agentX (first myAgentPosition)) (agentY (second myAgentPosition)))
		(if (/= (aref mapTmp agentX agentY) 998)
			(fiseralr-findSmlestNumber mapTmp agentX agentY)
			(progn
				(dotimes (y (first (array-dimensions mapTmp)))
					(dotimes (x (second (array-dimensions mapTmp)))
						(if (eq step (aref mapTmp x y))
							(fiseralr-expand mapTmp x y step)
						)		
					)
				)
				(fiserale-identifyNextMove mapTmp myAgentPosition (+ step 1))
			)
		)
	)
)

(defun fiseralr-findSmlestNumber (mapTmp x y)
	(let ((small 999) move (xplus (aref mapTmp (+ x 1) y)) (xminus (aref mapTmp (- x 1) y)) (yplus (aref mapTmp x (+ y 1))) (yminus (aref mapTmp x (- y 1))))
		(if (< xplus small)
			(progn
				(setf small xplus)
				(setf move 'GO-RIGHT)
			)
		)
		(if (< xminus small)
			(progn
				(setf small xminus)
				(setf move 'GO-LEFT)
			)
		)
		(if (< yplus small)
			(progn
				(setf small yplus)
				(setf move 'GO-UP)
			)
		)
		(if (< yminus small)
			(progn
				(setf small yminus)
				(setf move 'GO-DOWN)
			)
		)
		move
	)
)

(defun fiseralr-expand (mapTmp x y step)
	(fiseralr-expandTestAndSet mapTmp (+ x 1) y (+ step 1))
	(fiseralr-expandTestAndSet mapTmp (- x 1) y (+ step 1))
	(fiseralr-expandTestAndSet mapTmp x (+ y 1) (+ step 1))
	(fiseralr-expandTestAndSet mapTmp x (- y 1) (+ step 1))
)

(defun fiseralr-expandTestAndSet(mapTmp x y nextStep)
	(if (eq 998 (aref mapTmp x y))
		(setf (aref mapTmp x y) nextStep)
	)
)
