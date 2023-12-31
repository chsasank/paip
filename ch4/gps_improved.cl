(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
    "Find all elements that match according to test"
    (if test-not
        (apply #'remove item sequence
            :test-not (complement test-not) keyword-args)
        (apply #'remove item sequence
            :test (complement test) keyword-args)))

(defun mappend (fn the-list)
    (apply #'append (mapcar fn the-list)))

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun set-debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun unset-debug (&rest ids)
 "Stop dbg on the ids. With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
            (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ " " *debug-io*))
    (apply #'format *debug-io* format-string args)))

(defvar *ops* nil "List of available ops")

(defstruct op "An operation"
    (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun executing-p (x)
    "Is x of form (executing ..)"
    (starts-with x 'executing))

(defun starts-with (list x)
    "Is this a list with first element x"
    (and (consp list) (eql (first list) x)))

(defun convert-op (op)
    "Make op confirm to (executing op) convention"
    (unless (some #'executing-p (op-add-list op))
        (push (list 'executing (op-action op)) (op-add-list op)))
    op)

(defun op (action &key preconds add-list del-list)
    "make a new operator that obeys (executing op) convention"
    (convert-op
        (make-op :action action :preconds preconds
            :add-list add-list :del-list del-list)))

(defparameter *school-ops*
    (list
        (make-op :action 'drive-son-to-school
            :preconds '(son-at-home car-works)
            :add-list '(son-at-school)
            :del-list '(son-at-home))
        (make-op :action 'shop-installs-battery
            :preconds '(car-needs-battery shop-knows-problem shop-has-money)
            :add-list '(car-works))
        (make-op :action 'tell-shop-problem
            :preconds '(in-communication-with-shop)
            :add-list '(shop-knows-problem))
        (make-op :action 'telephone-shop
            :preconds '(know-phone-number)
            :add-list '(in-communication-with-shop))
        (make-op :action 'look-up-number
            :preconds '(have-phone-book)
            :add-list '(know-phone-number))
        (make-op :action 'give-shop-money
            :preconds '(have-money)
            :add-list '(shop-has-money)
            :del-list '(have-money))))
(mapc #'convert-op *school-ops*)


(defun achieve-all (state goals goal-stack)
    "Achieve each goal and make sure they still hold at the end"
    (let ((current-state state))
        (if (and 
                (every #'(lambda (g)
                    (setf current-state (achieve current-state g goal-stack)))
                    goals)
                (subsetp goals current-state :test #'equal)
            )
            current-state)))


(defun achieve (state goal goal-stack)
    "A goal is achieved if it already holds or 
    if there is an appropriate op for it"
    (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
    (cond 
        ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                (find-all goal *ops* :test #'appropriate-p)))))

(defun member-equal (item list)
    (member item list :test #'equal))


(defun apply-op (state goal op goal-stack)
    "Return a new transformed state if op is applicable"
    (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
    (let ((state2 (achieve-all state (op-preconds op)
                   (cons goal goal-stack))))
        (unless (null state2)
            ; return updated state
            (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
            (append 
                (remove-if
                    #'(lambda (x) (member-equal x (op-del-list op)))
                    state2)
                (op-add-list op)))))

(defun appropriate-p (goal op)
    "Op is appropriate if goal is op's add-list"
    (member-equal goal (op-add-list op)))

(defun use (oplist)
    "Use oplist as default list of operators"
    (length (setf *ops* oplist)))


(defun GPS (state goals &optional (*ops* *ops*))
    "General purpose problem solver: from state, achieve goals using *ops*"
    (find-all-if #'action-p (achieve-all (cons '(start) state) goals nil)))

(defun action-p (x)
  "Is x something that is (start) or (executing ...)?"
  (or (equal x '(start)) (executing-p x)))

(use *school-ops*)
(set-debug :gps)
(print (gps '(son-at-home car-needs-battery have-money have-phone-book)
      '(son-at-school)))

; GPS works for different set of ops too
(defparameter *banana-ops*
  (list
    (op
      'climb-on-chair
      :preconds '(chair-at-middle-room at-middle-room on-floor)
      :add-list '(at-bananas on-chair)
      :del-list '(at-middle-room on-floor))
    (op
      'push-chair-from-door-to-middle-room
      :preconds '(chair-at-door at-door)
      :add-list '(chair-at-middle-room at-middle-room)
      :del-list '(chair-at-door at-door))
    (op
      'walk-from-door-to-middle-room
      :preconds '(at-door on-floor)
      :add-list '(at-middle-room)
      :del-list '(at-door))
    (op
      'grasp-bananas
      :preconds '(at-bananas empty-handed)
      :add-list '(has-bananas)
      :del-list '(empty-handed))
    (op
      'drop-ball
      :preconds '(has-ball)
      :add-list '(empty-handed)
      :del-list '(has-ball))
    (op
      'eat-bananas
      :preconds '(has-bananas)
      :add-list '(empty-handed not-hungry)
      :del-list '(has-bananas hungry))))

(unset-debug)
(use *banana-ops*)
(print (GPS '(at-door on-floor has-ball hungry chair-at-door)
      '(not-hungry)))


(defun make-maze-ops (pair)
  "Make maze ops in both directions"
  (list (make-maze-op (first pair) (second pair))
      (make-maze-op (second pair) (first pair))))

(defun make-maze-op (here there)
  "Make an operator to move between two places"
  (op
    `(move from ,here to ,there)
    :preconds `((at ,here))
    :add-list `((at ,there))
    :del-list `((at ,here))))

(defparameter *maze-ops*
  (mappend #'make-maze-ops
    '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
      (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
      (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))

(use *maze-ops*)
(gps '((at 1)) '((at 25)))

