(defconstant fail nil "Indicates pat-match failure")

(defconstant no-bindings '((t . t))
    "Indicates pat-match success with no variables")

(defun variable-p (x)
    "Is x a variable (a symbol beginning with ?)"
    (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
    "Find a (var . value) pair in a bindings list"
    (assoc var bindings))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun make-binding (var val) (cons var val))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (make-binding var val)
    ;; Once we add a "real" binding,
    ;; we can get rid of the dummy no-bindings
    (if (eq bindings no-bindings)
      nil
      bindings)))

(defun match-variable (var input bindings)
    "Does var match input? Uses (or updates) and returns bindings"
    (let ((binding (get-binding var binding)))
        (cond 
            ((not binding) (extend-bindings var input bindings))
            ((equal input (binding-val binding)) bindings)
            (t fail))))

(defun pat-match (pattern input &optional (bindings no-bindings))
    "Match pattern against input in the context of bindings"
    (cond
        ((eq bindings fail) fail)
        ((variable-p pattern)
            (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
            (segment-matcher pattern input bindings))
        ((single-pattern-p pattern)
            (single-matcher pattern input bindings))
        ((and (consp pattern) (consp input))
            (pat-match (rest pattern) (rest input)
                (pat-match (first pattern) (first input) bindings)))
    
        (t fail)))


; tables for single and segment matching
(setf (get '?is 'single-match) 'match-is)
(setf (get '?or 'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?* 'segment-match) 'segment-match)
(setf (get '?+ 'segment-match) 'segment-match+)
(setf (get '?? 'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun segment-pattern-p (pattern)
    "Is this a segment matching pattern like ((?* var) . pat)"
    (and (consp pattern) (consp (first pattern))
        (symbolp (first (first pattern)))
        (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  (and (consp pattern)
      (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
    "Call the right function for this kind of segment pattern"
    (funcall (segment-match-fn (first (first (pattern))))
        pattern input bindings))

(defun single-matcher (pattern input bindings)
    "Call the right function for this kind of single pattern"
    (funcall (single-match-fn (first pattern))
        (rest pattern) input bindings))

(defun segment-match-fn (x)
    "Get the segment-match function for x
    if it is a symbol that has one"
    (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
    "Get the single-match function for x,
    if it is a symbol that has one."
    (when (symbolp x) (get x 'single-match)))


; define functions for single pattern matching

(defun match-is (var-and-pred input bindings)
    "Succeed and bind var if the input satisifies pred
    where var-and-pred is the list (var pred)"
    (let* ((var (first var-and-pred))
           (pred (second var-and-pred))
           (new-bindings (pat-match var input bindings)))
        (if (or (eq new-bindings fail)
                (not (funcall pred input)))
            fail
            new-bindings)))

(defun match-and (patterns input bindings)
    "Succeed if all the patterns match the input"
    (cond
        ((eq bindings fail) fail)
        ((null patterns) bindings)
        (t (match-and (rest patterns) input
                (pat-match (first patterns) input bindings)))))

(defun match-or (patterns input bindings)
    "Succeed if any of the patterns match the input"
    (if (null patterns)
        fail
        (let ((new-bindings (pat-match (first patterns) input bindings)))
            (if (eq new-bindings fail)
                (match-or (rest patterns) input bindings)
                new-bindings))))

(defun match-not (patterns input bindings)
    "Succeed if none of the patterns match the input
    This will never bind any variable"
    (if (match-or patterns input bindings)
        fail
        bindings))

(defun segment-match (pattern input bindings &optional (start 0))
    "Match the segment pattern ((?* var) . pat) against input"
)