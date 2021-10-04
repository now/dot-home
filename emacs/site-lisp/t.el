;; t.el --- Transducers and sequences  -*- lexical-binding: t -*-

;;; Commentary:

;; Transducers and sequences.

;;; Code:

(eval-when-compile
  (require 'cl-generic)
  (require 'pcase))

(cl-defmacro t-loop (bindings &rest body)
  "TODO"
  (let ((recur (gensym "t-loop-recur-"))
        (result (gensym "t-loop-result-")))
    `(let (,@bindings
           (,recur t)
           (,result nil))
      (cl-macrolet ((recur
                     (&rest rebindings)
                     (let ((temporaries (mapcar (lambda (_) (gensym))
                                                rebindings)))
                       `(let (,@(cl-mapcar (lambda (temporaries rebinding)
                                             `(,temporaries ,rebinding))
                                           temporaries rebindings))
                          (setq ,',recur t
                                ,@(cl-mapcan (lambda (binding temporary)
                                               `(,binding ,temporary))
                                             ',(mapcar #'car bindings)
                                             temporaries))))))
        (while ,recur
          (setq ,recur nil
                ,result (progn ,@body)))
        ,result))))

;;; TODO t-negate.

;;; t-funcall

(cl-defgeneric t-funcall (function &rest arguments)
  "Return result of applying FUNCTION to ARGUMENTS."
  "Return result of ‘apply’ on FUNCTION with ARGUMENTS."
  (apply function arguments))

;;; t-apply

(cl-defgeneric t-apply (function &rest arguments)
  "Return result of applying FUNCTION to ARGUMENTS."
  "Return result of ‘t-funcall’ on FUNCTION with ARGUMENTS."
  (apply #'t-funcall function arguments))

;;; t-iterations

(cl-defgeneric t-iterations (iterative)
  "Return something that will perform iterations over ITERATIVE.
Apply ‘t-current’ to the returned value to get the value of the
current iteration.  Apply ‘t-advance’ to the returned value to
advance the iteration process.

If nil is returned, there are no more iterations."
  "Return ITERATIVE."
  iterative)

;;; t-current

(cl-defgeneric t-current (iterations)
  "Return the value of the current iteration among ITERATIONS.")

;;; t-advance

(cl-defgeneric t-advance (iterations)
  "Return a producer of the next iteration among ITERATIONS.
Applying ‘t-iterations’ to the returned value gives the
iterations to apply ‘t-current’ to, or nil if there are no more
iterations.")

;;; t--each

(cl-defgeneric t--each (iterative function)
  "Iterate over ITERATIVE, applying FUNCTION to each iteration.
Iteration is interrupted if FUNCTION returns nil.
Doesn’t return a meaningful value."
  "First apply ‘t-iterations’ to ITERATIVE.  Then, if the result
is non-nil, apply FUNCTION to the result of applying ‘t-current’
to it.  If FUNCTION returns non-nil, repeat on the result of
applying ‘t-iterations’ to the result of applying ‘t-advance’ to
it."
  (t-loop (iterations (t-iterations iterative))
          (when iterations
            (t-funcall function (t-current iterations))
            (recur (t-advance iterations))))
  (let ((iterations (t-iterations iterative)))
    (while (and iterations (t-funcall function (t-current iterations)))
      (setq iterations (t-iterations (t-advance iterations))))))

;;; t-iterate

(defmacro t-iterate (spec &rest body)
  "Iterate over ITERATIVE.
Evaluate BODY with VARIABLE bound to each value in ITERATIVE via
‘t--each’, then return the evaluation of RESULT, which defaults
to ACCUMULATOR.

If an ACCUMULATOR isn’t provided, VARIABLE mustn’t appear inside
a list.

If an ACCUMULATOR is provided, it’s first initialized to
INITIAL-VALUE, then set to the result of the last form of body
during each iteration.  ACCUMULATOR is bound within the
evaluation of BODY.

ACCUMULATOR is bound within the evaluation of RESULT.

Each TEST is a test to perform before each iteration begins to
see if iteration should continue or not.  Each TEST consists of a
keyword followed by an expression.

  :until EXPR
    Stop iteration if EXPR evaluates to t
  :while EXPR
    Stop iteration unless EXPR evaluates to t

Both VARIABLE and ACCUMULATOR are bound within the evaluation of
EXPR.

\(fn ((VARIABLE [(ACCUMULATOR INITIAL-VALUE)]) ITERATIVE [RESULT] [TEST]...) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (<= 2 (length spec))
    (signal 'wrong-number-of-arguments (list '(2 . 3) (length spec))))
  (let* ((bindings (car spec))
         (iterative (cadr spec))
         (maybe-result (caddr spec))
         (result (unless (keywordp maybe-result) maybe-result))
         (test nil))
    (when (consp bindings)
      (unless (symbolp (car bindings))
        (signal 'wrong-type-argument (list 'symbolp (car bindings))))
      (unless (consp (cadr bindings))
        (if (cadr bindings)
            (signal 'wrong-type-argument (list 'consp (cadr bindings)))
          (signal 'wrong-type-argument (list 'symbolp bindings)))))
    (let ((aux (if result (cdddr spec) (cddr spec))))
      (while (consp aux)
        (unless (cadr aux)
          (signal 'wrong-number-of-arguments
                  (list (1+ (length spec)) (length spec))))
        (pcase (car aux)
          (:until (setq test (cons `(not ,(cadr aux)) test)))
          (:while (setq test (cons (cadr aux) test)))
          (keyword (error "Unknown keyword %s" keyword)))
        (setq aux (cddr aux))))
    (let* ((element (if (consp bindings) (car bindings) bindings))
           (accumulator (when (consp bindings) (caadr bindings)))
           (body (if accumulator `((setq ,accumulator (progn ,@body))) body))
           (body `((t--each ,iterative
                            (lambda (,element)
                              ,@(if test
                                    `((when ,(if (cdr test)
                                                 `(and ,@ (nreverse test))
                                               (car test))
                                        ,@body))
                                  body)))
                    ,(if result result accumulator))))
      (if (consp bindings)
          `(let (,@(cdr bindings))
             ,@body)
        `(progn
           ,@body)))))

;;; t-reduction

(defsubst t-reduction (result)
  "Return RESULT as a ‘t-reduction?’.
This means that ‘t-reduce’ will return RESULT."
  `(t-reduction . ,result))

(defsubst t-reduction? (result)
  "Return t if RESULT is a reduction."
  (eq (car-safe result) 't-reduction))

(defsubst t--reduced (result)
  "Return the RESULT previously marked as a ‘t-reduction?’."
  (if (t-reduction? result) (cdr result) result))

(defsubst t--ensure-reduction (result)
  "Return RESULT as a ‘t-reduction?’, unless that’s already the case."
  (if (t-reduction? result) result (t-reduction result)))

(defun t--preserving-reduction (operation)
  "Return an anonymous function that preserves reductions of OPERATION.
The returned function takes two arguments, the result of the
‘t-reduce’ so far and the current input."
  (lambda (result input)
    (let ((result (t-funcall operation result input)))
      (if (t-reduction? result) (t-reduction result) result))))

;;; t-reduce

(cl-defun t-reduce (iterative monoid-operation
                              &optional (initial-value nil initial?))
  "Return a reduction of ITERATIVE via MONOID-OPERATION.

If INITIAL-VALUE is given and ITERATIVE yields no iterations,
then INITIAL-VALUE is the reduction.

If INITIAL-VALUE is given and ITERATIVE yields one or more
iterations, MONOID-OPERATION is first applied to INITIAL-VALUE
and the result of the first iteration.  Then MONOID-OPERATION is
applied to its previous result and the result of the second
iteration, and so on, resulting in a final reduction.

If INITIAL-VALUE isn’t given and ITERATIVE yields no iterations,
then MONOID-OPERATION is called without arguments to get its
identity element, and then that’s the reduction.

If INITIAL-VALUE isn’t given and ITERATIVE yields one or more
iterations, then the result of the first iteration is taken as
the INITIAL-VALUE and MONOID-OPERATION is applied to
INITIAL-VALUE and the result of the second iteration.  Then
MONOID-OPERATION is applied to its previous result and the result
of the third iteration, and so on, resulting in a final
reduction.

Iteration is interrupted if INITIAL-VALUE is or MONOID-OPERATION
returns a ‘t-reduction?’."
  (if (not initial?)
      (let* ((iterations (t-iterations iterative))
             (first-iteration (t-current iterations)))
        (if first-iteration
            (t-reduce (t-advance iterations) monoid-operation first-iteration)
          (t-funcall monoid-operation)))
    (t--reduced
     (t-iterate ((iteration (result initial-value)) iterative
                 :until (t-reduction? result))
       (t-funcall monoid-operation result iteration)))))

;;; t-transduce

(cl-defun t-transduce (iterative transformation monoid-operation
                                 &optional (initial-value
                                            (t-funcall monoid-operation)))
  "Return a reduction of ITERATIVE via a TRANSFORMATION of MONOID-OPERATION.

First, TRANSFORMATION is applied to MONOID-OPERATION, resulting
in a transformed operation.  Then ‘t-reduce’ is applied to
ITERATIVE, the transformed operation, and INITIAL-VALUE.
Finally, the transformed operation is applied to the reduction.

If INITIAL-VALUE isn’t given, then MONOID-OPERATION is called
without arguments to get its identity element and that’s used as
the INITIAL-VALUE.

\(fn ITERATIVE TRANSFORMATION MONOID-OPERATION &optional INITIAL-VALUE)"
  (let ((transformed-operation (t-funcall transformation monoid-operation)))
    (t-funcall transformed-operation
               (t-reduce iterative transformed-operation initial-value))))

;;; t-constant

(defun t-constant (value)
  "Return an anonymous function that will always return VALUE.
The returned function takes any number of arguments."
  (lambda (&rest _) value))

;;; t-negative?

(defsubst t-negative? (n)
  "Return t if N is less than 0."
  (< n 0))

;;; t-zero?

(defsubst t-zero? (N)
  "Return t if N is 0."
  (zerop n))

;;; t-positive?

(defsubst t-positive? (n)
  "Return t if N is greater than 0."
  (> n 0))

;;; t-count
;;; TODO Rename to t-cardinality?

(let ((one (t-constant 1)))
  (cl-defgeneric t-count (iterative)
    "Return the number of iterations that ITERATIVE yields."
    "Apply ‘t-transduce’ to ITERATIVE, mapping values to 1 and
‘+’ together them."
    (t-transduce iterative (t-map one) #'+)))

;;; t-empty?

(cl-defgeneric t-empty? (iterative)
  "Return t if ITERATIVE doesn’t yield any iterations."
  "Return t if ‘t-iterate’ on ITERATIVE yields an iteration."
  (t-iterate ((_ (empty t)) iterative empty :while empty)))

;;; t-non-empty?

(cl-defgeneric t-non-empty? (iterative)
  "Return t if ITERATIVE yields at least one iteration."
  "Apply ‘not’ to the result of ‘t-empty?’ on ITERATIVE.
Thus, you only need to define a method for ‘t-empty?’ and you get
‘t-non-empty?’ for free."
  (not (t-empty? iterative)))

;;; nil

(cl-defmethod t-current ((_ (eql nil)))
  "Return nil."
  nil)

(cl-defmethod t-advance ((_ (eql nil)))
  "Return nil."
  nil)

(cl-defmethod t--each ((_ (eql nil)) _2)
  "Do nothing.")

(cl-defmethod t-count ((_ (eql nil)))
  "Return 0."
  0)

(cl-defmethod t-empty? ((_ (eql nil)))
  "Return t."
  t)

(cl-defmethod t-non-empty? ((_ (eql nil)))
  "Return nil."
  nil)

;;; sequence

;; TODO Update tests.
(cl-defmethod t-apply ((sequence sequence) (n integer))
  "Apply ‘elt’ to SEQUENCE and N.
Return nil if that integer is greater than or equal to the
‘length’ of SEQUENCE.  Signal ‘wrong-type-argument’ unless the
sub-sequence of SEQUENCE up to that integer is a ‘listp’."
  (elt sequence n))

(cl-defmethod t-current ((sequence sequence))
  "Apply ‘elt’ to SEQUENCE with 0."
  (elt sequence 0))

(cl-defmethod t--each ((sequence sequence) function)
  "Apply ‘mapc’ to FUNCTION and SEQUENCE."
  (mapc function sequence))

(cl-defmethod t-count ((sequence sequence))
  "Apply ‘length’ to SEQUENCE."
  (length sequence))

;;; list

;; TODO Add tests.
(cl-defmethod t-apply ((list list) (key-or-property symbol))
  "TODO"
  (if (consp (car list))
      (assoc key-or-property list)
    (plist-get list key-or-property)))

(cl-defmethod t-advance ((list list))
  "Apply ‘cdr’ to LIST."
  (cdr list))

(cl-defmethod t-empty? ((_ list))
  "Return nil."
  nil)

(cl-defmethod t-non-empty? ((_ list))
  "Return t."
  t)

(cl-defmethod t--conj ((list list) element)
  "Apply ‘cons‘ to ELEMENT and LIST."
  (cons element list))

;;; array

(cl-defmethod t-empty? ((array array))
  "Return t if ‘length’ of ARRAY is 0."
  (t-zero? (length array)))

(cl-defmethod t-non-empty? ((array array))
  "Return t if ‘length’ of ARRAY isn’t 0."
  (not (t-zero? (length array))))

;;; string

(cl-defmethod t-advance ((string string))
  "Apply ‘substring’ to STRING with 1.
Return nil if STRING is ‘t-empty?’."
  (unless (t-zero? (length string))
    (substring string 1)))

;;; t-cons

(defun t-cons (object iterative)
  "Return a t-cons of OBJECT and ITERATIVE."
  `(t--cons ,object . ,iterative))

(cl-defmethod t-current (((_ object . -) (head t--cons)))
  "Return the OBJECT."
  object)

(cl-defmethod t-advance (((_ _2 . iterative) (head t--cons)))
  "Return the ITERATIVE."
  iterative)

(cl-defmethod t--each (((_ object . iterative) (head t--cons)) function)
  "Apply FUNCTION to OBJECT and ‘t--each’ to ITERATIVE and FUNCTION.
FUNCTION is applied to OBJECT via ‘t-funcall’."
  (t-funcall function object)
  (t--each iterative function))

(cl-defmethod t-count (((_ _2 . iterative) (head t--cons)))
  "Return 1 + ‘t-count’ of ITERATIVE"
  (1+ (t-count iterative)))

(cl-defmethod t-empty? ((_ (head t--cons)))
  "Return nil."
  nil)

(cl-defmethod t-non-empty? ((_ (head t--cons)))
  "Return t."
  t)

;;; t-conj

(cl-defgeneric t--conj (collection element)
  "TODO"
  "Apply ‘t-cons’ to ELEMENT and COLLECTION."
  (t-cons element collection))

(defun t-conj (&rest args)
  "TODO"
  (pcase args
    (`() nil)
    (`(,collection) collection)
    (`(,collection ,element) (t--conj collection element))
    (`(,collection . ,elements)
     (t-iterate ((element (result collection)) collection)
       (t--conj result element)))))

;;; t-repeat

(cl-defun t-repeat (element &optional (n 0 n?))
  "Return an iterative that always yields ELEMENT.
If N is given and is ‘t-positive?’, return an iterative that
yields ELEMENT N times."
  (if n?
      (when (t-positive? n)
        `(t--bounded-repetitions ,element . ,n))
    `(t--unbounded-repetitions . ,element)))

(cl-defmethod t-current (((_ . element) (head t--unbounded-repetitions)))
  "Return ELEMENT."
  element)

(cl-defmethod t-advance ((repetitions (head t--unbounded-repetitions)))
  "Return REPETITIONS."
  repetitions)

(cl-defmethod t--each (((_ . element) (head t--unbounded-repetitions))
                       function)
  "TODO"
  (while t
    (t-funcall function element)))

(cl-defmethod t-empty? ((_ (head t--unbounded-repetitions)))
  "TODO"
  nil)

(cl-defmethod t-non-empty? ((_ (head t--unbounded-repetitions)))
  "TODO"
  t)

(cl-defmethod t-current (((_ element . -) (head t--bounded-repetitions)))
  "TODO"
  element)

(cl-defmethod t-advance (((_ element . n) (head t--bounded-repetitions)))
  "TODO"
  (t-repeat element (1- n)))

(cl-defmethod t--each (((_ element . n) (head t--bounded-repetitions)) function)
  "TODO"
  (while (t-positive? n)
    (t-funcall function element)
    (setq n (1- n))))

(cl-defmethod t-count (((_ _ . n) (head t--bounded-repetitions)))
  "TODO"
  n)

(cl-defmethod t-empty? ((_ (head t--bounded-repetitions)))
  "TODO"
  nil)

(cl-defmethod t-non-empty? ((_ (head t--bounded-repetitions)))
  "TODO"
  t)

;;; t-range

(defun t-range (&rest args)
  "TODO"
  (pcase args
    (`() (t-range 0 nil 1))
    (`(,to) (t-range 0 to))
    (`(,from ,to (guard (< from to))) (t-range from to 1))
    (`(,from ,to (guard (> from to))) (t-range from to -1))
    (`(,_, _) nil)
    (`(,from 'nil ,step) `(t--unbounded-range ,from . ,step))
    ((and `(,from ,to ,step) (guard (and (< from to) (t-positive? step))))
     `(t--right-open-range ,from ,to . ,step)
     )
    ((and `(,from ,to ,step) (guard (> from to) (t-negative? step)))
     `(t--left-open-range ,from ,to . ,step)
     )
    ((and `(,from ,to ,step) (guard (and (/= from to) (t-zero? step))))
     (t-repeat from))
    (`(,_ ,_ ,_) nil)
    (_ (signal 'wrong-number-of-arguments (list '(0 . 3) (length args))))))

(cl-defmethod t-current (((_ from . -) (head t--unbounded-range)))
  "TODO"
  from)

(cl-defmethod t-advance (((_ from . step) (head t--unbounded-range)))
  "TODO"
  `(t--unbounded-range ,(+ from step) . ,step))

(cl-defmethod t--each (((_ from . step) (head t--unbounded-range)) function)
  "TODO"
  (while t
    (t-funcall function from)
    (setq from (+ from step))))

(cl-defmethod t-empty? ((_ (head t--unbounded-range)))
  "TODO"
  nil)

(cl-defmethod t-non-empty? ((_ (head t--unbounded-range)))
  "TODO"
  t)

(cl-defmethod t-current (((_ from _2 . -) (head t--right-open-range)))
  "TODO"
  from)

(cl-defmethod t-advance (((_ from to . step) (head t--right-open-range)))
  "TODO"
  (let ((from (+ from step)))
    (when (< from to)
      `(t--right-open-range ,from ,to . ,step))))

(cl-defmethod t--each (((_ from to . step) (head t--right-open-range)) function)
  "TODO"
  (while (< from to)
    (t-funcall function from)
    (setq from (+ from step))))

(cl-defmethod t-count (((_ from to . -) (head t--right-open-range)))
  "TODO"
  (abs (- from to)))

(cl-defmethod t-empty? ((_ (head t--right-open-range)))
  "TODO"
  nil)

(cl-defmethod t-non-empty? ((_ (head t--right-open-range)))
  "TODO"
  t)

(cl-defmethod t-current (((_ from _2 . -) (head t--left-open-range)))
  "TODO"
  from)

(cl-defmethod t-advance (((_ from to . step) (head t--left-open-range)))
  "TODO"
  (let ((from (+ from step)))
    (when (> from to)
      `(t--left-open-range ,from ,to . ,step))))

(cl-defmethod t--each (((_ from to . step) (head t--left-open-range)) function)
  "TODO"
  (while (> from to)
    (t-funcall function from)
    (setq from (+ from step))))

(cl-defmethod t-count (((_ from to . -) (head t--left-open-range)))
  "TODO"
  (abs (- from to)))

(cl-defmethod t-empty? ((_ (head t--left-open-range)))
  "TODO"
  nil)

(cl-defmethod t-non-empty? ((_ (head t--left-open-range)))
  "TODO"
  t)

;;; Functions on functions.

(defun t-compose (&rest functions)
  "TODO"
  (pcase functions
    (`() #'identity)
    (`(,f) f)
    (`(,f ,g)
     (lambda (&rest args)
       (pcase args
         (`() (t-funcall f (t-funcall g)))
         (`(,x) (t-funcall f (t-funcall g x)))
         (`(,x ,y) (t-funcall f (t-funcall g x y)))
         (`(,x ,y ,z) (t-funcall f (t-funcall g x y z)))
         (`(,x ,y ,z . ,args) (t-funcall f (t-apply g x y z args))))))
    (_ (t-reduce functions #'t-compose))))

(defun t-complement (predicate)
  "TODO"
  (lambda (&rest args)
    (not (t-apply predicate args))))

;;; Transducers

(defmacro t-transformation (&rest cases)
  "TODO"
  (declare (indent defun) (debug (&rest body)))
  (let* ((args (gensym "args")) ;; TODO gensym not needed here, right?
       )
    `(lambda (&rest ,args)
       (pcase ,args
         ,@cases
         (_ (signal 'wrong-number-of-arguments (list '(0 . 2)
                                                     (length ,args))))))))

(defmacro t-transducer (next &rest cases)
  "TODO"
  (declare (indent 1) (debug (form &rest body)))
  `(t-stateful-transducer ,next () ,@cases))

(defmacro t-stateful-transducer (next state &rest cases)
  "TODO"
  (declare (indent 2) (debug (form form &rest body)))
  (unless (cl-find-if (lambda (case)
                        (pcase case
                          (`(`((,c ,_)) . ,_) (eq c '\,))))
                      cases)
    (setq cases (cons `(`(,result) (t-funcall ,next result)) cases)))
  (unless (cl-find-if (lambda (case)
                        (pcase case
                          (`(`() . ,_) t)))
                      cases)
    (setq cases (cons `(`() (t-funcall ,next)) cases)))
  `(cl-function (lambda (,next &aux ,@state)
                  (t-transformation
                   ,@cases))))

(defun t-reverse ()
  "TODO"
  (t-stateful-transducer next ((reversed-input nil))
    (`(,result) (t-reduce reversed-input next result))
    (`(,result ,input)
     (setq reversed-input (cons input reversed-input))
     result)))

(defun t-drop (n)
  "TODO"
  (t-stateful-transducer next ((m n))
    (`(,result ,input)
     (if (t-zero? m)
         (t-funcall next result input)
       (setq m (1- m))
       result))))

(defun t-drop-last (n)
  "TODO"
  (t-stateful-transducer next ((inputs (make-ring n)))
    (`(,result ,input)
     (when (= (ring-length inputs) (ring-size inputs))
       (t-funcall next result (ring-ref inputs (1- (ring-length inputs)))))
     (ring-insert inputs input))))

(defun t-drop-until (predicate)
  "TODO"
  (t-drop-while (t-complement predicate)))

(defun t-drop-while (predicate)
  "TODO"
  (t-stateful-transducer next ((drop t))
    (`(,result ,input)
     (if drop
         (if (t-funcall predicate)
             result
           (setq drop nil)
           (t-funcall next result input))
       (t-funcall next result input)))))

(defun t-take (n)
  "TODO"
  (t-stateful-transducer next ((m n))
    (`(,result ,input)
     (pcase m
       ((pred (< 1))
        (setq m (1- m))
        (t-funcall next result input))
       ((pred (= 1))
        (setq m 0)
        (t--ensure-reduction (t-funcall next result input)))
       (_ result)))))

(defun t-take-last (n)
  "TODO"
  (t-stateful-transducer next ((inputs (make-ring n)))
    (`(,result)
     (t-reduce (nreverse (ring-elements inputs)) next result))
    (`(,result ,input)
     (ring-insert inputs input)
     result)))

;; TODO t-drop-nth.

(defun t-take-nth (n)
  "TODO"
  (t-compose (t-map #'cons (t-range))
             (t-filter (lambda (pair) (t-zero? (% (cdr pair) n))))
             (t-map #'car)))

(defun t-take-until (predicate)
  "TODO"
  (t-take-while (t-complement predicate)))

(defun t-take-while (predicate)
  "TODO"
  (t-transducer next
    (`(,result ,input)
     (if (t-funcall predicate input)
         (t-funcall next result input)
       (t-reduction result)))))

(defun t-mapcat (function)
  "TODO"
  (t-compose (t-map function) #'t-cat))

(defun t-cat (function)
  "TODO"
  (let ((function-preserving-reduction (t--preserving-reduction function)))
    (t-transformation
     (`(,result) (t-funcall function result))
     (`(,result ,input)
      (t-reduce input function-preserving-reduction result)))))

(defun t-keep (function)
  "TODO"
  (t-compose (t-map function) (t-filter #'identity)))

(defun t-filter (predicate)
  "TODO"
  (t-transducer next
    (`(,result ,input)
     (if (t-funcall predicate input)
         (t-funcall next result input)
       result))))

(defun t-remove (predicate)
  "TODO"
  (t-filter (t-complement predicate)))

(cl-defun t-halt-when (predicate &optional (halted-transform
                                            (lambda (_ input) input)))
  "TODO Returns a transducer that ends transduction when pred returns true
  for an input. When retf is supplied it must be a fn of 2 arguments -
  it will be passed the (completed) result so far and the input that
  triggered the predicate, and its return value (if it does not throw
  an exception) will be the return value of the transducer. If retf
  is not supplied, the input that triggered the predicate will be
  returned. If the predicate never returns true the transduction is
  unaffected."
  (t-transducer next
    (`(,result)
     (pcase result
       (`(t-halted . ,input) input)
       (_ (t-funcall next result))))
    (`(,result ,input)
     (if (t-funcall predicate input)
         (t-reduction `(t-halted . ,(t-funcall halted-transform result input)))
       (t-funcall next result input)))))

(defun t-map (function &rest iteratives)
  "TODO"
  (pcase iteratives
    (`()
     (t-transducer next
       (`(,result ,input) (t-funcall next result (t-funcall function input)))
       (`(,result ,input . ,inputs)
        (t-funcall next result (t-apply function input inputs)))))
    (_
     (t-stateful-transducer next ((iterations iteratives))
       (`(,result ,input)
        ;; TODO If this one is kept, move comp up to only generate it once.
        (let ((iterations (t-into nil (t-compose (t-map #'t-iterations)
                                                 (t-halt-when #'null))
                                  iterations))
              (inputs (t-into nil (t-compose (t-map #'t-current)) iterations)))
          (if (null iterations)
              (t-reduction result)
            (setq iterations (t-into nil (t-map #'t-advance) iterations))
            (t-funcall next result (t-apply function input inputs))))

        (let ((inputs (mapcar #'t-funcall iterations)))
          (if (member :t-iteration-limit inputs)
              (t-reduction result)
            (t-funcall next result (t-apply function input inputs))))

        (t-funcall next result (t-apply function input (mapcar #'t-funcall
                                                               iterations))))
       (`(,result ,input . ,inputs)
        (t-funcall next result (t-apply function input
                                        (nconc inputs
                                               (mapcar #'t-funcall
                                                       iterations)))))))))

;; TODO Generic?
(defun t-into (&rest args)
  "TODO"
  (pcase args
    (`() nil)
    (`(,to) to)
    (`(,to ,from) (t-reduce from #'t-conj to))
    (`(,to ,transformation ,from) (t-transduce from transformation #'t-conj to))
    (_ (signal 'wrong-number-of-arguments (list '(0 . 3) (length args))))))

(provide 't)

;;; t.el ends here
