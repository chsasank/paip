(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
    "Find all elements that match according to test"
    (if test-not
        (apply #'remove item sequence
            :test-not (complement test-not) keyword-args)
        (apply #'remove item sequence
            :test (complement test) keyword-args)))


(defvar *state* nil "Current state")

(defvar *ops* nil "List of available ops")

(defstruct op "An operation"
    (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun GPS (*state* goals *ops*)
    "General purpse solver"
    (if (every #'achieve goals) 'solved))

(defun achieve (goal)
    "If goal is not already achieved, apply appropriate op"
    (or (member goal *state*)
        (some #'apply-op 
            (find-all goal *ops* :test #'appropriate-p))))

(defun appropriate-p (goal op)
    "check if op is appropriate for a goal"
    (member goal (op-add-list op)))

(defun apply-op (op)
    "print a message and update *state* if op is applicable"
    (when (every #'achieve (op-preconds op))
        (print (list 'executing (op-action op)))
        (setf *state* (set-difference *state* (op-del-list op)))
        (setf *state* (union *state* (op-add-list op)))    
        t))



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


(gps '(son-at-home car-needs-battery have-money have-phone-book)
    '(son-at-school)
    *school-ops*)
