(load "unification.cl")

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
    `(add-clause ',clause))

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
(defun prove (goal bindings)
    "Return a list of possible solutions to goal"
    (mapcan
        #'(lambda (clause)
            (let ((new-clause (rename-variables clause)))
                (prove-all
                    (clause-body new-clause)
                    (unify goal (clause-head new-clause) bindings))))            
        (get-clauses (predicate goal))))


(defun prove-all (goals bindings)
    "Return a list of solutions to the conjuction of goals"
    (cond 
        ((eq bindings fail) fail)
        ((null goals) (list bindings))
        (t (mapcan 
                #'(lambda (goal1-solution)
                    (prove-all (rest goals) goal1-solution))
                (prove (first goals) bindings)))))


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

(defmacro ?- (&rest goals) `(top-level-prove ',goals))

;; clean up and show nice output
(defun top-level-prove (goals)
    "prove the goals and print variables readably"
    (show-prolog-solutions
        (variables-in goals)
        (prove-all goals no-bindings)))

(defun show-prolog-solutions (vars solutions)
    "Print the variables in each of the solutions"
    (if (null solutions)
        (format t "~&No.")
        (mapc #'(lambda (solution) (show-prolog-vars vars solution))
            solutions))
    (values))

(defun show-prolog-vars (vars bindings)
    "Print each variable with its binding"
    (if (null vars)
        (format t "~&Yes")
        (dolist (var vars)
            (format t "~&~a = ~a" var
                (subst-bindings bindings var))))
    (princ ";"))


;; example
(<- (likes Kim Robin))
(<- (likes Sandy Lee))
(<- (likes Sandy Kim))
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
(<- (likes ?x ?x))
