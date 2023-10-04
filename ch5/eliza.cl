(defconstant fail nil "Indicates pat-match failure")

(defconstant no-bindings '((t . t)) 
    "Indicates pat-match success but with no variables")

; functions for assoc
(defun get-binding (var bindings)
    (assoc var bindings))

(defun binding-val (binding)
    (cdr binding))

(defun lookup (var bindings)
    "Look up value for var"
    (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
    "Add (var . val) to binding list"
    (cons (cons var val) 
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun pat-match (pattern input &optional (bindings no-bindings))
    "Match pattern against input in the context of bindings"
    (cond
        ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
            (pat-match (first pattern) (first input) bindings)))
        (t fail)    
    )
)

(defun match-variable (var input bindings)
    "Does var match input? Updates and return bindings"
    (let ((binding (get-binding var bindings)))
        (cond
            ((not binding) (extend-bindings var input bindings))
            ((equal input (binding-val binding)) bindings)
            (t fail)
        )))

(defun variable-p (x)
    "Is X a symbol starting with ?"
    (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

