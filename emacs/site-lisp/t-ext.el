;;; t-deftype

(defun t-type-name-p (name)
  "TODO"
  (and name (symbolp name) (not (keywordp name))))

(defalias 't-type-slot-name-p 't-type-name-p "TODO")

(defmacro t-deftype (name &optional docstring &rest slots)
  "TODO"
  (unless (stringp docstring)
    (setq slots (cons docstring slots)
          docstring nil))
  (unless (t-type-name-p name)
    (signal 'wrong-type-argument (list 't-type-name-p name 'name)))
  (let ((wrong-type-slots (cl-remove-if #'t-type-slot-name-p slots)))
    (when wrong-type-slots
      (signal 'wrong-type-argument
              (list '(list t-type-name-p) wrong-type-slots 'slots))))
  (let* ((sname (symbol-name name))
         (prefix (concat sname "-")))
    `(progn
       (put ',name 't-slots ',slots)
       (defun ,(intern (concat "make-" sname)) ,slots
         "TODO"
         (cons ',name ,(cl-reduce (lambda (conses slot) `(cons ,slot ,conses))
                                  (reverse slots)))))))
(t-deftype t-repeat element n)

;; TODO Docstring
(defmacro t-defgeneric (name arguments)
  (let* ((typed-arguments (cl-remove-if-not #'consp arguments))
         (typed-argument (car typed-arguments))
         (arguments (mapcar (lambda (argument)
                              (if (consp argument) (car argument) argument))
                            arguments)))
    (let ((non-symbol-arguments (cl-remove-if #'symbolp arguments)))
      (when non-symbol-arguments
        (signal 'wrong-type-argument (list '(list symbolp) non-symbol-arguments
                                           'arguments))))
    (let ((additional-typed-arguments (cdr typed-arguments)))
      (when additional-typed-arguments
        (signal 'wrong-type-argument (list 'symbolp additional-typed-arguments
                                           'arguments))))
    (pcase typed-argument
      (`(,argument ,type)
       (unless (t-type-name-p type)
         (signal 'wrong-type-argument (list 't-type-name-p type 'type)))
       (let ((method (gensym "method")))
         `(progn
            (put ',type 't-methods
                 (plist-put (get ',type 't-methods) ',name (lambda ,arguments)))
            (defun ,name ,arguments
              (let ((,method (plist-get (get (car ,argument) 't-methods)
                                        ',name)))
                (if ,method
                    (funcall ,method ,@arguments)
                  (signal 'void-function (list ',name))))))))
      (_ (signal 'wrong-type-argument (list 'consp typed-argument))))))
(t-defgeneric t-^current ((object t-current)))

;; TODO Do we really want this?
(defun t-implements-p (object protocol)
  (if (symbolp object)
      `(and (t-type-name-p ,object)
            (or (memq ',protocol (get ,object 't-protocols))
                ;; todo this has to iterate over the properties of a plist.
                ;; todo check func-arity matches
                (when (cl-loop with object-methods = (get ,object 't-methods)
                               for method in (get ,protocol 't-methods)
                               for implemented = (and (memq method object-methods))
                               while implemented
                               finally return implemented)
                  (put ,object 't-protocols
                       (cons ,protocol (get ,object 't-protocols))))))
    `(and (consp ,object) (t-type-name-p (car ,object)) )))
(t-implements-p obj 'proto)

(defmacro t-defmethod (name arguments &rest body)
  "TODO
\(fn NAME ARGUMENTS &optional DOCSTRING DECL &rest BODY)"
  (let* ((typed-arguments (cl-remove-if-not #'consp arguments))
         (typed-argument (car typed-arguments))
         (arguments (mapcar (lambda (argument)
                              (if (consp argument) (car argument) argument))
                            arguments)))
    (let ((non-symbol-arguments (cl-remove-if #'symbolp arguments)))
      (when non-symbol-arguments
        (signal 'wrong-type-argument (list '(list symbolp) non-symbol-arguments
                                           'arguments))))
    (let ((additional-typed-arguments (cdr typed-arguments)))
      (when additional-typed-arguments
        (signal 'wrong-type-argument (list 'symbolp additional-typed-arguments
                                           'arguments))))
    (pcase typed-argument
      ((or `(,argument ,type &slots . ,slots)
           (and `(,argument ,type) (let slots nil)))
       (unless (t-type-name-p type)
         (signal 'wrong-type-argument (list 't-type-name-p type 'type)))
       (unless (listp slots)
         (signal 'wrong-type-argument
                 (list '(list t-type-slot-name-p) slots 'slots)))
       (let ((wrong-type-slots (cl-remove-if #'t-type-slot-name-p slots)))
         (when wrong-type-slots
           (signal 'wrong-type-argument
                   (list '(list t-type-slot-name-p) wrong-type-slots 'slots))))
       (let* ((type-slots (when slots (get type 't-slots)))
              (positions (mapcar (lambda (slot)
                                   (cons slot (cl-position slot type-slots)))
                                 slots))
              (unknown-slots (mapcar #'car (cl-remove-if #'cdr positions))))
         (when unknown-slots
           (signal 'wrong-type-argument
                   (list '(list t-type-name-p) unknown-slots 'slots)))
         (let (;; TODO Optimize this to use “binary search” among the
               ;; slots to get them.
               (bindings (mapcar (lambda (slot)
                                   `(,slot (nth ,(1+ (cdr (assoc slot
                                                                 positions)))
                                                ,argument)))
                                 slots)))
           `(put ',type 't-methods
                 (plist-put (get ',type 't-methods) ',name
                            (lambda ,arguments
                              ,@(if slots
                                    `((let ,bindings ,@body))
                                  body)))))))
      (_ (signal 'wrong-type-argument (list 'consp typed-argument))))))
(t-defmethod current ((repeat t-repeat &slots element n) repeat)
             element)

;;; t-typelet

;; TODO Expand to support multiple types
(defmacro t-typelet (bindings &rest body)
  "TODO"
  (declare (indent 1) (debug (symbolp form &rest body)))
  (if (cdr bindings)
      `(t-typelet (,(car bindings)) (t-typelet ,(cdr bindings) ,@body))
    (if (null bindings)
        body
      (let ((name (caar bindings))
            (slots (cadar bindings)))
        (unless (t-type-name-p name)
          (signal 'wrong-type-argument (list 't-type-name-p name 'name)))
        (let ((wrong-type-slots (cl-remove-if #'t-type-slot-name-p slots)))
          (when wrong-type-slots
            (signal 'wrong-type-argument
                    (list '(list t-type-name-p) wrong-type-slots 'slots))))
        `(cl-macrolet ((,(intern (concat "make-" (symbol-name name)))
                        ,slots
                        ,(let ((head (butlast slots))
                               (tail (car (last slots))))
                           (if head
                               ```(,',',name ,,,@head . ,,,tail)
                             ```(,',',name . ,,,tail))))
                       (,(intern (concat (symbol-name name) "-slots"))
                        (slots object &rest body)
                        (let ((missing-slots (cl-remove-if (lambda (slot)
                                                             (memq slot
                                                                   ',slots))
                                                           slots)))
                          (when missing-slots
                            (signal 'wrong-type-argument
                                    (list '(list t-type-name-p) missing-slots
                                          'slots))))
                        (let* ((slots (mapcar (lambda (slot)
                                                (if (memq slot slots) slot '_))
                                              ',slots))
                               (head (butlast slots))
                               (tail (car (last slots)))
                               (pattern (if head
                                            ``(_ ,,@head . ,,tail)
                                          ``(_ . ,,tail))))
                          `(pcase-let ((,pattern ,object)) ,@body))))
           ,@body)))))

(defmacro t-slots (type slots name &rest body)
  (declare (indent defun) (debug (symbolp form body)))
  `(,(intern (concat (symbol-name type) "-slots")) ,slots ,name ,@body))


;; ;;; t-iterator

;; ;; TODO Rename this to t-lambda-iterator or something.  Or perhaps
;; ;; just remove it?  T-next should be implemented for function.
;; (defclass t-iterator ()
;;   ((function :initarg :function)))

;; ;; TODO We need to rename this to t-make-iterator
;; (defun t-iterator (function)
;;   (make-instance 't-iterator :function function))

;; ;; TODO Is this worth keeping?
;; (defmacro t-iterator-body (&rest body)
;;   `(t-make-iterator (lambda () ,@body)))

;; (cl-defmethod t-next ((iterator t-iterator))
;;   (t-funcall (slot-value iterator 'function)))

;; (cl-defmethod t-count ((iterator t-iterator))
;;   (let ((n 0))
;;     (t-do (element iterator n)
;;       (setq n (1+ n)))))

;; (cl-defmethod t--each (function (iterator t-iterator))
;;   (condition-case nil
;;       (while t
;;         (t-funcall function (t-next iterator)))
;;     (t-end-of-iterator)))

;; TODO This is probably the nicest solution to stateless iterations:
;; (defun another-range (n)
;;   (letrec ((next (lambda (i)
;;                    (when (< i n)
;;                      (cons i (lambda () (funcall next (1+ i))))))))
;;     (lambda () (funcall next 0))))



;; (defun another-range (n)
;;   (letrec ((next (lambda (i)
;;                    (when (< i n)
;;                      (cons i (lambda () (funcall next (1+ i)))))))
;;            (chunk-next (lambda (i)
;;                          (if (< (+ i 32) n)
;;                              (let ((j i))
;;                                (lambda ()
;;                                  (if (< j (+ i 32))
;;                                      (prog1
;;                                          j
;;                                        (setq j (1+ j)))
;;                                    (lambda () (funcall chunk-next (+ i 32))))))
;;                           (funcall next i)))))
;;     (lambda (&optional chunk)
;;       (funcall (if chunk chunk-next next) 0))))

(cl-defstruct (test1 (:copier nil) (:type list) :named)
  (n 0 :read-only t))

(cl-defgeneric test-current (test))

(cl-defmethod test-current ((range (head test1)))
  (test1-n range))

(cl-defgeneric test-next (test))

(cl-defmethod test-next ((range (head test1)))
  `(test1 ,(1- (test1-n range))))

(cl-defmethod test-next ((range (head test2))))
(cl-defmethod test-next ((range (head test3))))
(cl-defmethod test-next ((range (head test4))))
(cl-defmethod test-next ((range (head test5))))
(cl-defmethod test-next ((range (head test6))))
(cl-defmethod test-next ((range (head test7))))
(cl-defmethod test-next ((range (head test8))))
(cl-defmethod test-next ((range (head test9))))

(benchmark-run-compiled 1
  (let ((i (make-test1 :n 1000000)))
    (profiler-start 'mem)
    (unwind-protect
        (progn
          (while (> (test-current i) 0)
            (setq i (test-next i)))
          (profiler-report))
      (profiler-stop))))

(cl-defstruct (range (:copier nil))
  (n 0 :read-only t)
  (o 0 :read-only t)
  (q 0 :read-only t))

(defun make-test2 (n)
  ;(make-range :n n)
  ;(record 'range n 0 1 1000000)
  `(range . ,n)
  )

(defun test2-current (range)
  (when (eq (car range) 'range)
    (let ((fn (plist-get (get (car range) 'methods) 'test2-current)))
      (when fn
        (funcall fn range)))))

(defun test2-next (range)
  (when (eq (car range) 'range)
    (let ((fn (plist-get (get (car range) 'methods) 'test2-next)))
      (when fn
        (funcall fn range)))))

(put 'range 'methods
     `(test2-current ,(lambda (range) (cdr range))
       test2-next ,(lambda (range) (make-test2 (1- (test2-current range))))))

(benchmark-run-compiled 1
  (let ((i (make-test2 1000000)))
    (profiler-start 'mem)
    (unwind-protect
        (progn
          (while (> (test2-current i) 0)
            (setq i (test2-next i)))
          (profiler-report))
      (profiler-stop))))

(defun test3-iterations (n)
  (letrec ((next (lambda (i)
                      (when (> i 0)
                        (cons i (lambda () (funcall next (1- i))))))))
    (lambda () (funcall next n))))

(benchmark-run-compiled 1
  (let ((i (test3-iterations 1000000)))
    (profiler-start 'mem)
    (unwind-protect
        (progn
          (while (setq i (funcall i))
            (setq i (cdr i)))
          (profiler-report))
      (profiler-stop))))

(defmacro deftype (arglist &rest body)
  (macroexpand-all
   `(cl-macrolet
        ,(let ((i 0))
           (mapcar (lambda (arg) `(arg () (nth ,(setq i (1+ i)))))
                   arglist))
      ,@body)
   macroexpand-all-environment))

(pcase-let* ((`(,a _ . ,c) something))
  `(,a ,c))

(deftype junk-range (from to))

;; TODO I think we need to give up on iterations, at least as they are
;; today.  We want first and next instead.

;; TODO Add t-deftype macro that defines a type.

;; TODO Add t-defmethod or something that adds a defun to a type.  It
;; should also make it easy to access the slots of the type.  Perhaps
;; add &slots to arglist (or just separate list, as it’ll almost
;; always be non-empty anyway).

;; TODO Add t-call-method or something that invokes the method on a
;; type.

(defsubst junky2 (result)
  (and (consp result) (eq (car result) 't-reduction)))

(defvar myrange '(range . 1000000))

(benchmark-run-compiled 1
  (let ((i 0))
    (while (< i 1000000)
      (junky2 myrange)
      (setq i (1+ i)))))

(cl-defgeneric junky1 (a))

(cl-defmethod junky1 ((_ (head range)))
  t)

(benchmark-run-compiled 1
  (let ((i 0))
    (while (< i 1000000)
      (junky1 myrange)
      (setq i (1+ i)))))

  (t-defmethod t-current ((repetitions t-unbounded-repetitions &slots element))
    element)
  (cl-defmethod t-current (((_ . element) (head t-unbounded-repetitions)))
    element)


(t-transduce '(1 2 3 4 5)
             (t-compose (t-take-while (lambda (i) (< i 5)))
                        (t-filter (lambda (i) (not (= (% i 2) 0))))) #'+) ; 9

(t-into nil (t-compose (t-take-while (lambda (i) (< i 5))) (t-reverse)) '(1 3 5 7))

(t-into nil (t-compose (t-map #'cons (make-instance 't-range :from 0 :to 5 :step 1)) (t-take-last 3)) '(1 3 5 7))

(t-into nil (t-compose (t-drop 3)) '(1 3 5 7))

(t-into nil (t-keep (lambda (i) (if (< i 5) (1+ i) nil))) '(1 3 5 7))

(t-into nil (t-take-nth 2) '(1 3 5 7))

;; get the length
(t-transduce '(1 3 5 7) (t-compose (t-drop 3) (t-map (t-constant 1))) #'+)

;(t-conj '(1 2) 3)
(let* ((base "a.b.c")
       (bases (list base (replace-regexp-in-string "\\." "/" base)))
       (types '("java" "scala"))
       (typed (t-into nil (t-map (lambda (path) (format "%s.java" path))) bases)))
  typed)

;; look at generator.el
;; use cl-defun (&optional (var initform svar))

;; TODO OK, so we will almost never want iterations, but we do want
;; lazy sequences (see map).  So implement t--each with lazy
;; sequences, simply admitting that that implementation is sub-optimal
;; in almost all cases, and then everyone will have to implement
;; t--each and lazy sequences.  So lazy sequences are

(cl-defmacro now-test (&rest body)
  (letrec ((fix (lambda (b)
                  (if (consp b)
                      (mapcar fix b)
                    (if (eq b 'kernel)
                        't-kernel
                      b)))))
    (funcall fix body)))

(now-test kernel)

;; Let imports name modules that we depend on (just require them),
;; then also allow renames (just implement them using cl-labels or
;; whatever emacs has).  Exports must list intended exports and those
;; symbols will receive name as their prefix (and a -) for Emacs.
;; Then renaming is done for all symbols that are exported.  Any
;; symbol starting with - is an internal symbol and it also receives
;; the prefix.
(cl-defmacro namespace (name &optional docstring &rest body &key imports
                             &key exports)
  "TODO"
  (unless (stringp docstring)
    (setq body (cons docstring body)))
  `(let ((known-exports (…)))
     (cl-macrolet ((some-macro-that-defines-and-that-renames-known-exports)))
     …))
