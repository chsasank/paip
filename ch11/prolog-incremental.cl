(load "unification.cl")

(defun format-flush (&rest args)
    (apply #'format args)
    (finish-output))

;; Clauses are represented as (head . body) cons cells
(defun clause-head (clause) (first clause))
(defun clause-body (clause) (rest clause))

;; clauses are stored on the predicate's plist
(defun get-clauses (pred) (get pred 'clauses))
(defun predicate (relation) (first relation))

(defvar *db-predicates* nil 
    "A list of all predicates stored in the database")

;; adding a clause
(defmacro <- (&rest clause)
    "Add a clause to the data base"
    `(add-clause ',(replace-?-vars clause)))

(defun add-clause (clause)
    "Add a clause to the database, indexed by head's predicate"
    (let ((pred (predicate (clause-head clause))))
        (assert (and (symbolp pred) (not (variable-p pred))))
        (pushnew pred *db-predicates*)
        (setf (get pred 'clauses)
            (nconc (get-clauses pred) (list clause)))
        pred))

;; removing a clause

(defun clear-db ()
    "Remove all clauses (for all predicates) from the database"
    (mapc #'clear-predicate *db-predicates*))

(defun clear-predicate (predicate)
    "Remove the clauses for a single predicate"
    (setf (get predicate 'clauses) nil))

;; prove a goal
(defun prove-all (goals bindings)
    "Find a solution to the conjunction of goals"
    (cond 
        ((eq bindings fail) fail)
        ((null goals) bindings)
        (t (prove (first goals) bindings (rest goals)))))

(defun prove (goal bindings other-goals)
    "Return a list of possible solutions to goal"
    (let ((clauses (get-clauses (predicate goal))))
        (if (listp clauses)
            (some 
                #'(lambda (clause)
                    (let ((new-clause (rename-variables clause)))
                        (prove-all
                            (append (clause-body new-clause) other-goals)
                            (unify goal (clause-head new-clause) bindings))))
                clauses)
            ;; the predicate's "clauses" can be an atom
            ;; a primitive function to call
            (funcall clauses (rest goal) bindings other-goals))))


;; rename variables so that variables can be treated 'local'
(defun rename-variables (x)
    "Replace all variables in x with new ones"
    (sublis 
        (mapcar
            #'(lambda (var) (cons var (gensym (string var))))
            (variables-in x))
        x))

(defun variables-in (exp)
    "Return a list of all variables in EXP."
    (unique-find-anywhere-if #'variable-p exp))

(defun unique-find-anywhere-if (predicate tree
                                &optional found-so-far)
    "Return a list of leaves of tree satisying predicate,
    with duplicates removed"
    (if (atom tree)
        (if (funcall predicate tree)
            (adjoin tree found-so-far)
            found-so-far)
        (unique-find-anywhere-if
            predicate
            (first tree)
            (unique-find-anywhere-if
                predicate (rest tree) found-so-far))))

(defun replace-?-vars (exp)
    "Replace any ? withing exp with a var of the form ?123"
    (cond 
        ((eq exp '?) (gensym "?"))
        ((atom exp) exp)
        (t (reuse-cons 
                (replace-?-vars (first exp))
                (replace-?-vars (rest exp))
                exp))))

(defmacro ?- (&rest goals) `(top-level-prove ',(replace-?-vars goals)))

;; clean up and show nice output
(defun top-level-prove (goals)
    (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
        no-bindings)
    (format-flush t "~&No.") 
    (values))

(defun show-prolog-solutions (vars solutions)
    "Print the variables in each of the solutions"
    (if (null solutions)
        (format-flush t "~&No.")
        (mapc #'(lambda (solution) (show-prolog-vars vars solution))
            solutions))
    (values))

(defun show-prolog-vars (vars bindings other-goals)
    "Print each variable with its binding
    Then ask the user if more solutions are desired"
    (if (null vars)
        (format-flush t "~&Yes")
        (dolist (var vars)
            (format-flush t "~&~a = ~a" var
                (subst-bindings bindings var))))
    (if (continue-p)
        fail
        (prove-all other-goals bindings)))

; set show-prolog-vars as primitive
(setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)

(defun continue-p ()
    "Ask user if we should continue looking for solutions"
    (case (read-char)
        (#\; t)
        (#\. nil)
        (#\newline (continue-p))
        (otherwise
            (format-flush t " Type ; to see more or . to stop")
            (continue-p))))

;; example
(<- (likes Kim Robin))
(<- (likes Sandy Lee))
(<- (likes Sandy Kim))
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
(<- (likes ?x ?x))

;; member relation
(<- (member ?item (?item . ?)))
(<- (member ?item (? . ?rest)) (member ?item ?rest))

;; length
(<- (length () 0))
(<- (length (?x . ?y) (1 + ?n)) (length ?y ?n))