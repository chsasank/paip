(defconstant fail nil "Indicates pat-match failure")
(defconstant no-bindings '((t . t))
    "Indicates pat-match success with no variables")

(defun variable-p (x)
    "Is x a variable?"
    (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
    "Find a (variable . value) pair in a binding list"
    (assoc var bindings))

(defun binding-val (binding)
    "Get the value part of a single binding"
    (cdr binding))

(defun lookup (var bindings)
    "Get the value part (for var) from a binding list"
    (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
    "Add a (var . value) pair to a binding list"
    (cons (cons var val)
        ;; get rid of dummy binding if present
        (if (and (eq bindings no-bindings))
            nil
            bindings
        )))

(defun match-variable (var input bindings)
    "Does var match input? Use/updates and returns bindings"
    (let ((binding (get-binding var bindings)))
    (cond
        ((not binding) (extend-bindings var input bindings))
        ((equal input (binding-val binding)) bindings)
        (t fail))))


;; end of pat-match common stuff
;; below from chapter on efficiency
(proclaim '(inline reuse-cons))
(defun reuse-cons (x y x-y)
 "Return (cons x y), or just x-y if it is equal to (cons x y)."
 (if (and (eql x (car x-y)) (eql y (cdr x-y)))
   x-y
   (cons x y)))

(defparameter *occurs-check* t "Should we do the occurs check")

(defun unify (x y &optional (bindings no-bindings))
    "See if x and y match with given bindings"
    (cond 
        ((eq bindings fail) fail)
        ((eql x y) bindings)
        ((variable-p x) (unify-variable x y bindings))
        ((variable-p y) (unify-variable y x bindings))
        ((and (consp x) (consp y))
            (unify (rest x) (rest y)
                (unify (first x) (first y) bindings)))        
        (t fail)))

(defun unify-variable (var x bindings)
    "Unify var with x, using (and may be extending) bindings"
    (cond
        ((get-binding var bindings)
            (unify (lookup var bindings) x bindings))
        ((and (variable-p x) (get-binding x bindings))
            (unify var (lookup x bindings) bindings))
        ((and *occurs-check* (occurs-check var x bindings)) fail)
        (t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)
    "Does var occur anywhere inside x"
    (cond 
        ((eq var x) t)
        ((and (variable-p x) (get-binding x bindings))
            (occurs-check var (lookup x bindings) bindings))
        ((consp x) 
            (or (occurs-check var (first x) bindings)
                (occurs-check var (rest x) bindings)))
        (t nil)))

(defun subst-bindings (bindings x)
    "Substitute the value of variables in bindings into x
    taking recursively bound variables into account"
    (cond
        ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((and (variable-p x) (get-binding x bindings))
            (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                (subst-bindings bindings (cdr x))
                x))))

(defun unifier (x y)
 "Return something that unifies with both x and y (or fail)."
 (subst-bindings (unify x y) x))