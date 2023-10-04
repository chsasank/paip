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
        ((segment-pattern-p pattern)
         (segment-match pattern input bindings))
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


(defun starts-with (list x)
    "Is this a list with first element x"
    (and (consp list) (eql (first list) x)))


(defun segment-pattern-p (pattern)
    "Is this a segment pattern ((?* var) . pat)"
    (and (consp pattern)
         (starts-with (first pattern) '?*)))

(defun segment-match (pattern input bindings &optional (start 0))
    "match segment pattern ((?* var) . pat) against input"
    (let ((var (second (first pattern)))
          (pat (rest pattern)))
        (if (null pat)
            (match-variable var input bindings)
            ; assume that pattern starts with a constant
            ; i.e two consecutive vars not allowed
            (let ((pos (position (first pat) input
                        :start start :test #'equal)))
                (if (null pos)
                    fail
                    (let ((b2 (pat-match
                                pat (subseq input pos)
                                (match-variable var (subseq input 0 pos)
                                                bindings))))
                        ; if this match failed try another longer one
                        ; if it worked, check that variables match
                        (if (eq b2 fail)
                            (segment-match pattern input bindings (+ pos 1))
                            (match-variable var (subseq input 0 pos) b2))))))))


(defun rule-pattern (rule) (first rule))

(defun rule-responses (rule) (rest rule))


(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y))
     (How do you do. Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y) (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y) (Do you wish that ?y)
     (What do you think about ?y) (Really-- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not?) (You are being a bit negative)
     (Are you saying "NO" just to be negative?))
    (((?* ?x) I was (?* ?y))
     (Were you really?) (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))))


(defun eliza ()
    "respond to user input using mattern matching rules"
    (loop
        (print 'eliza>)
        (write (flatten (use-eliza-rules (read))) :pretty t)))

(defun use-eliza-rules (input)
    "Find some rule to transform the input"
    (some #'(lambda (rule)
                (let ((result (pat-match (rule-pattern rule) input)))
                    (if (not (eq result fail))
                        (sublis (switch-viewpoint result)
                                (random-elt (rule-responses rule))))))
            *eliza-rules*))

(defun switch-viewpoint (words)
    "change I to you and viceversa and so on"
    (sublis '((I . you) (you . I) (me . you) (am . are))
          words))

; helper functions
(defun flatten (the-list)
  "Append together elements (or lists) in the list."
  (mappend #'mklist the-list))

(defun mklist (x)
  "Return x if it is a list, otherwise (x)."
  (if (listp x)
      x
    (list x)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the result."
  (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(eliza)