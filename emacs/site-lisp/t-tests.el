;; t-tests.el --- Transducers and sequences tests  -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for transducers and sequences.

;;; Code:

(ert-deftest t-tests-t-apply ()
  "Test ‘t-apply’."
  (should (equal (t-apply #'cons 1 2) '(1 . 2))))

(ert-deftest t-tests-t-funcall ()
  "Test ‘t-funcall’."
  (should (equal (t-funcall #'cons 1 2) '(1 . 2))))

(cl-defstruct (t-tests--cons (:copier nil))
  "Structure used for testing."
  (object nil :read-only t :documentation "The object.")
  (iterative nil :read-only t :documentation "The iterative."))

(defun t-tests--list (&rest arguments)
  "Return a list of ARGUMENTS in a ‘t-test--cons’ sequence."
  (let ((arguments (nreverse arguments))
        (result nil))
    (while arguments
      (setq result (make-t-tests--cons :object (car arguments)
                                       :iterative result)
            arguments (cdr arguments)))
    result))

(cl-defmethod t-iterations ((cons t-tests--cons))
  "Iterate over CONS."
  cons)

(cl-defmethod t-current ((cons t-tests--cons))
  "Return current iteration of CONS."
  (t-tests--cons-object cons))

(cl-defmethod t-advance ((cons t-tests--cons))
  "Advance iteration of CONS."
  (t-tests--cons-iterative cons))

(cl-defmethod t--conj ((cons t-test--cons) element)
  "Apply ‘make-t-test--cons’ to ELEMENT and CONS."
  (make-t-test--cons :object element :iterative cons))

;; t--each

(ert-deftest t-tests-t--each-default-method ()
  "Test default method of ‘t--each’."
  (should (equal (let ((result nil))
                   (t--each (t-tests--list 1 2 3)
                            (lambda (i) (setq result (cons i result))))
                   result)
                 '(3 2 1))))

(ert-deftest t-tests-t-iterate-without-result-returns-nil ()
  "Test that ‘t-iterate’ without a result returns nil."
  (should (equal (t-iterate (v (t-tests--list 1 2 3)))
                 nil)))

(ert-deftest t-tests-t-iterate-with-result-returns-result ()
  "Test that ‘t-iterate’ with a result returns that result."
  (should (equal (t-iterate (v (t-tests--list 1 2 3) 4))
                 4)))

(ert-deftest t-tests-t-iterate-initializes-accumulator-and-returns-it ()
  "Test that ‘t-iterate’ initializes accumulator and returns it
if no other result is given."
  (should (equal (t-iterate ((v (a 4)) (t-tests--list 1 2 3))
                   a)
                 4)))

(ert-deftest t-tests-t-iterate-accumulates-iterations ()
  "Test that ‘t-iterate’ accumulates iterations.
Accumulator is bound within body."
  (should (equal (t-iterate ((v (a 0)) (t-tests--list 1 2 3))
                   (+ a v))
                 6)))

(ert-deftest t-tests-t-iterate-binds-accumulator-for-result ()
  "Test that ‘t-iterate’ binds variable and accumulator for
result."
  (should (equal (t-iterate ((v (a 0)) (t-tests--list 1 2 3) (1+ a))
                   (+ a v))
                 7)))

(ert-deftest t-tests-t-iterate-until ()
  "Test that ‘t-iterate’ stops when :until is t."
  (should (equal (t-iterate ((v (a 0)) (t-tests--list 1 2 3)
                             :until (> a 2))
                   (+ a v))
                 3)))

(ert-deftest t-tests-t-iterate-while ()
  "Test that ‘t-iterate’ stops when :while is nil."
  (should (equal (t-iterate ((v (a 0)) (t-tests--list 1 2 3)
                             :while (< a 1))
                   (+ a v))
                 1)))

(ert-deftest t-tests-t-iterate-with-variable-inside-list-errors ()
  "Test that ‘t-iterate’ with variable inside list but no
accumulator signals."
  (should (equal (should-error (t-iterate ((v) (t-tests--list 1 2 3))))
                 '(wrong-type-argument . (symbolp (v))))))

(ert-deftest t-tests-t-reduction-returns-a-t-reduction? ()
  "Test that ‘t-reduction’ returns a ‘t-reduction?’."
  (should (t-reduction? (t-reduction 1))))

(ert-deftest t-tests-t-reduction?-returns-nil-for-a-non-reduction ()
  "Test that ‘t-reduction?’ returns nil for a non-reduction."
  (should-not (t-reduction? 1)))

(ert-deftest t-tests-t--reduced-returns-result-inside-a-t-reduction? ()
  "Test that ‘t--reduced’ returns result inside a ‘t-reduction?’."
  (should (equal (t--reduced (t-reduction 1)) 1)))

(ert-deftest t-tests-t--reduced-returns-result-outside-a-t-reduction? ()
  "Test that ‘t--reduced’ returns result outside a ‘t-reduction?’."
  (should (equal (t--reduced 1) 1)))

(ert-deftest t-tests-t--ensure-reduction-returns-a-t-reduction? ()
  "Test that ‘t--ensure-reduction’ returns a ‘t-reduction?’."
  (should (t-reduction? (t--ensure-reduction 1))))

(ert-deftest t-tests-t--ensure-reduction-is-idempotent ()
  "Test that ‘t--ensure-reduction’ is idempotent."
  (let ((reduction (t-reduction 1)))
    (should (eq (t--ensure-reduction reduction) reduction))))

(ert-deftest t-tests-t-reduce-reduces-no-iterations-to-initial-value ()
  "Test that ‘t-reduce’ reduces no iterations to initial value."
  (should (equal (t-reduce (t-tests--list) #'+ 4) 4)))

(ert-deftest t-tests-t-reduce-reduces-iterations-with-initial-value ()
  "Test that ‘t-reduce’ reduces iterations with initial value."
  (should (equal (t-reduce (t-tests--list 1 2 3) #'+ 4) 10)))

(ert-deftest t-tests-t-reduce-reduces-no-iterations-and-no-initial-value-to-identity ()
  "Test that ‘t-reduce’ reduces no iterations and no initial
value to identity element."
  (should (equal (t-reduce (t-tests--list) #'+) 0))
  (should (equal (t-reduce (t-tests--list) #'*) 1)))

(ert-deftest t-tests-t-reduce-reduces-iterations-without-initial-value ()
  "Test that ‘t-reduce’ reduces no iterations and no initial
value to identity element."
  (should (equal (t-reduce (t-tests--list 1 2 3) #'+) 6)))

(ert-deftest t-tests-t-reduce-is-interrupted-by-t-reduction? ()
  "Test that ‘t-reduce’ returns a ‘t-reduction?’ result."
  (should (equal (t-reduce (t-tests--list 1 2 3)
                           (lambda (a v)
                             (if (= v 2) (t-reduction (+ a v)) (+ a v))))
                 3)))

(ert-deftest t-tests-t-transduce-transduces ()
  "Test that ‘t-transduce’ applies transformation to monoid-operation."
  (should (equal (t-transduce (t-tests--list 1 2 3)
                              (lambda (next)
                                (lambda (&rest args)
                                  (pcase args
                                    (`() (t-funcall next))
                                    (`(,result) (t-funcall next result))
                                    (`(,result ,input)
                                     (t-funcall next result input)))))
                              #'+
                              4)
                 10)))

;; TODO More tests for ‘t-transduce’.

;;; t-constant

(ert-deftest t-tests-t-constant-returns-a-function-that-always-returns-a-given-value ()
  "Test that ‘t-constant’ returns a function that will always
return a given value."
  (let ((one (t-constant 1)))
    (dotimes (i 10)
      (should (equal (funcall one) 1)))))

(ert-deftest t-tests-t-constant-returns-a-function-that-takes-any-number-of-arguments ()
  "Test that ‘t-constant’ returns a function that takes any
number of arguments."
  (let ((one (t-constant 1)))
    (should (equal (funcall one 1 2 3 4 5) 1))))

;;; TODO t-negative?

;;; TODO t-zero?

;;; t-positive?

(ert-deftest t-tests-t-positive? ()
  "Test ‘t-positive?’."
  (should (t-positive? 1))
  (should (t-poitive? 10))
  (should-not (t-positive? 0))
  (should-not (t-positive? -1))
  (should-not (t-positive? -10)))

(ert-deftest t-tests-t-empty?-default-method ()
  "Test default method of ‘t-empty?’."
  (should (t-empty? (t-tests--list)))
  (should-not (t-empty? (t-tests--list 1))))

(ert-deftest t-tests-t-non-empty?-default-method ()
  "Test default method of ‘t-non-empty?’."
  (should-not (t-non-empty? (t-tests--list)))
  (should (t-non-empty? (t-tests--list 1))))

;;; nil

(ert-deftest t-tests-t-current-nil ()
  "Test that ‘t-current’ for nil returns nil."
  (should (equal (t-current nil) nil)))

(ert-deftest t-tests-t-advance-nil ()
  "Test that ‘t-advance’ for nil returns nil."
  (should (equal (t-advance nil) nil)))

(ert-deftest t-tests-t-count-nil ()
  "Test that ‘t-count’ for nil returns 0."
  (should (equal (t-count nil) 0)))

(ert-deftest t-tests-t-empty?-nil ()
  "Test that ‘t-empty?’ for nil returns t."
  (should (t-empty? nil)))

(ert-deftest t-tests-t-non-empty?-nil ()
  "Test that ‘t-non-empty?’ for nil returns nil."
  (should-not (t-non-empty? nil)))

;;; sequence

(ert-deftest t-tests-t-apply-sequence ()
  "Test ‘t-apply’ for sequences."
  (should (equal (t-apply '(1 2 3) 2) 3))
  (should-error (t-apply '(1 2 3) 2 3)
                :type 'wrong-number-of-arguments)
  (should (equal (should-error (t-apply '(1 2 3) "2"))
                 '(wrong-type-argument numberp "2")))
  (should (equal (t-apply '(1 2 3) 3) nil))
  (should (equal (should-error (t-apply '(1 . 2) 1))
                 '(wrong-type-argument listp 2))))

(ert-deftest t-tests-t-current-sequence ()
  "Test ‘t-current’ for sequences."
  (should (equal (t-current '(1 2 3)) 1)))

(ert-deftest t-tests-t-count-sequence ()
  "Test ‘t-count’ for sequences."
  (should (equal (t-count '(1 2 3)) 3)))

;;; list

(ert-deftest t-tests-t-advance-list ()
  "Test ‘t-advance’ for lists."
  (should (equal (t-advance '(1 2 3)) '(2 3))))

(ert-deftest t-tests-t-empty?-list ()
  "Test ‘t-empty?’ for lists."
  (should-not (t-empty? '(1 2 3))))

(ert-deftest t-tests-t-non-empty?-list ()
  "Test ‘t-non-empty?’ for lists."
  (should (t-non-empty? '(1 2 3))))

;;; array

(ert-deftest t-tests-t-empty?-for-arrays ()
  "Test ‘t-empty?’ for arrays."
  (should (t-empty? []))
  (should-not (t-empty? [1])))

(ert-deftest t-tests-t-non-empty?-for-arrays ()
  "Test ‘t-non-empty?’ for arrays."
  (should (t-non-empty? [1]))
  (should-not (t-non-empty? [])))

;;; string

(ert-deftest t-tests-t-advance-for-strings ()
  "Test ‘t-advance’ for strings."
  (should (equal (t-advance "abc") "bc"))
  (should (equal (t-advance "") nil)))

;;; t-cons

(ert-deftest t-tests-t-current-for-t-cons ()
  "Test ‘t-current’ for ‘t-cons’."
  (should (equal (t-current (t-cons 1 (t-cons 2 (t-tests--list 3))))
                 1)))

(ert-deftest t-tests-t-advance-for-t-cons ()
  "Test ‘t-advance’ for ‘t-cons’."
  (should (equal (t-advance (t-cons 1 (t-cons 2 (t-tests--list 3))))
                 (t-cons 2 (t-tests--list 3)))))

(ert-deftest t-tests-t-count-for-t-cons ()
  "Test ‘t-count’ for ’t-cons’."
  (should (equal (t-count (t-cons 1 (t-tests--list))) 1))
  (should (equal (t-count (t-cons 1 (t-cons 2 (t-tests--list)))) 2))
  (should (equal (t-count (t-cons 1 (t-tests--list 2 3))) 3)))

(ert-deftest t-tests-t-empty?-for-t-cons ()
  "Test ‘t-empty?’ for ‘t-cons’."
  (should-not (t-empty? (t-cons 1 (t-tests--list)))))

(ert-deftest t-tests-t-non-empty?-for-t-cons ()
  "Test ‘t-non-empty?’ for ‘t-cons’."
  (should (t-non-empty? (t-cons 1 (t-tests--list)))))

;;; t-repeat

(ert-deftest t-tests-t-repeat ()
  "Test ‘t-current’ for ‘t-repeat’."
  (should (equal (t-current (t-repeat 'a)) 'a))
  (should (equal (t-current (t-advance (t-repeat 'a))) 'a))
  (should (equal (t-current (t-repeat 1 'a)) 'a))
  (should (equal (t-advance (t-repeat 1 'a) nil))))

(provide 't-tests)

;;; t-tests.el ends here
