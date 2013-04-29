
(defstructure (steklmar		; replace "my-agent" by your unique name, as e.g. FIT username
		(:include db-agent
			(body (make-steklmar-body))
			(program 'steklmar-program)
			(name "steklmar")
		)
	)
	"steklmar's agent"
)

(defstructure (steklmar-body
	(:include db-agent-body (name "steklmar"))
)
;  (slot1 default1)  ; any specific extra slots your agent's body would need
;  ...
;  (slotn defaultn))
;    ;
)

(defun steklmar-program (percept)
	(let* (
		(me (car percept))
		(grid (cadr percept))
		(enemy-loc (find-enemy-location grid))
		(ball-on-my-loc
			(member-if
				(lambda (a) (typep a 'percept-object-ball))
				(apply #'aref grid (object-loc me))
			)
		)
		(holding-ball (object-contents me))
		(throw-loc (find-location-for-throw me grid))
	)
	(cond
		;;; Nemam nepritele -> koncim
		((not enemy-loc) 'stop)
		;;; Stojim na mici -> tak ho seberu
		(ball-on-my-loc 'grab-ball)
		;;; Mam mic -> hodim mic na nalezenou pozici
		(holding-ball `(throw-ball ,@throw-loc))
		;;; Nejsem u mice -> jdu k nemu
		(t (find-next-step me grid))
	)
	)
)

(defun find-next-step (me grid)
	(let* (
		(distances (make-array (array-dimensions grid) :initial-element 10000))
		(predecessors (make-array (array-dimensions grid) :initial-element nil))
		(not-visited nil)
		(my-loc (object-loc me))
		(ball-loc (find-ball-location grid))
	)
		;; Inicializuj nenavstivene pozice 
		(dotimes (x (car (array-dimensions grid)))
			(dotimes (y (cadr (array-dimensions grid)))
				(push (list x y) not-visited)
			)
		)
		;; Nastav vzdalenost moji pozice na 0
		(setf (aref distances (car my-loc) (cadr my-loc)) 0)
		;; Dokud existuje alespon jeden nenavstiveny uzel
		(loop while (not (null not-visited)) do
			;; Najdi minimum
			(setf current-node (find-minimal not-visited distances))
			;; Odeber nalezeny minimalni uzel z nenavstivenych
			(setf not-visited (remove current-node not-visited :test #'equal))
			;; Pokud jsem dorazil k mici
			(cond ((equal current-node ball-loc)
				(return-from find-next-step (get-next-step current-node my-loc predecessors))
			))
			;; Vezmi sousedy soucasneho uzlu
			(setf neighbors (get-neighbors current-node))
			;; Spocitej vzdalenosti sousedu
			(loop for n-loc in neighbors do
				(cond ((and
					(< 0 (car n-loc))
					(< 0 (cadr n-loc))
					(> (car (array-dimensions grid)) (car n-loc))
					(> (cadr (array-dimensions grid)) (cadr n-loc))
					(null (enemy-at? n-loc grid))
				)
					(setf tmp (+ (aref distances (car current-node) (cadr current-node)) 1))
					(cond (
						(< tmp (aref distances (car n-loc) (cadr n-loc)))
							(setf (aref distances (car n-loc) (cadr n-loc)) tmp)
							(setf (aref predecessors (car n-loc) (cadr n-loc)) current-node)
					))
				))
			)
		)
		'stay ;; Pokud neumim, tak zustanu stat
	)
)

(defun get-neighbors (location)
	(list
		(list (- (car location) 1) (cadr location))
		(list (+ (car location) 1) (cadr location))
		(list (car location) (- (cadr location) 1))
		(list (car location) (+ (cadr location) 1))
	)
)

(defun get-next-step (current-node my-loc predecessors)
	(setf back-node current-node)
	(loop while (and
		(not (null (aref predecessors (car back-node) (cadr back-node))))
		(not (equal my-loc (aref predecessors (car back-node) (cadr back-node))))
	) do
		(setf back-node (aref predecessors (car back-node) (cadr back-node)))
	)
	(cond
		((> (xy-x my-loc) (car back-node)) 'go-left)
		((< (xy-x my-loc) (car back-node)) 'go-right)
		((< (xy-y my-loc) (cadr back-node)) 'go-up)
		((> (xy-y my-loc) (cadr back-node)) 'go-down)
		(t 'stay)
	)
)

(defun find-minimal (not-visited distances)
	(setf min-distance-loc nil)
	(loop for loc in not-visited do
		(cond ((or
			(null min-distance-loc)
			(<
				(aref distances (car loc) (cadr loc))
				(aref distances (car min-distance-loc) (cadr min-distance-loc))
			)
		)
			(setf min-distance-loc loc)
		))
	)
	min-distance-loc
)

(defun find-location-for-throw (me grid)
	(setf my-loc (object-loc me))
	(setf closest-enemy-loc (get-closest-enemy my-loc grid))
	(cond ((near? my-loc closest-enemy-loc)
			closest-enemy-loc
		)
		(t
			(get-closest-loc my-loc (get-neighbors closest-enemy-loc))
		)
	)
)

(defun get-closest-loc (main-loc locations)
	(setf closest-loc nil)
	(loop for loc in locations do
		(cond ((or
				(null closest-loc)
				(or
					(<
						(abs (- (xy-x main-loc) (xy-x loc)))
						(abs (- (xy-x main-loc) (xy-x closest-loc)))
					)
					(<
						(abs (- (xy-y main-loc) (xy-y loc)))
						(abs (- (xy-y main-loc) (xy-y closest-loc)))
					)
				)
			)
				(setf closest-loc loc)
			)
		)
	)
	closest-loc
)

(defun get-closest-enemy (my-loc grid)
	(get-closest-loc my-loc (find-all-enemies grid))
)

(defun find-all-enemies (grid)
	(let* (
		(enemies nil)
	)
		(dotimes (numberx (car (array-dimensions grid)))
			(dotimes (numbery (cadr (array-dimensions grid)))
				(setf enemy (identify-in-list #'enemy-p (aref grid numberx numbery)))
				(if (not (null enemy))
					(push (list numberx numbery) enemies)
				)
			)
		)
		enemies
	)
)

(defun enemy-at? (location grid)
	(identify-in-list #'enemy-p (aref grid (car location) (cadr location)))
)

(defun find-enemy-location (grid)
	(find-X-location #'enemy-p grid)
)

(defmethod enemy-p ((obj percept-object))
	(if	(and
		(not (equal (percept-object-name obj) "#"))
		(not (equal (percept-object-name obj) "steklmar"))
		(not (equal (percept-object-name obj) "B"))
	) obj nil)
)
