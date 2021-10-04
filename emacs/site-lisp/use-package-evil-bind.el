(require 'use-package-core)
(require 'bind-key)

;;;###autoload
(defun use-package-normalize-evil-binder (name keyword args)
  (let ((arg args)
        args*)
    (while arg
      (let ((x (car arg)))
        (cond
         ;; (KEY . COMMAND)
         ((and (consp x)
               (or (stringp (car x))
                   (vectorp (car x)))
               (or (use-package-recognize-function (cdr x) t #'stringp)))
          (setq args* (nconc args* (list x)))
          (setq arg (cdr arg)))
         ;; KEYWORD
         ;;   :map MODE KEYMAP
         ((and (eq x :map) (symbolp (cadr arg)) (symbolp (caddr arg)))
          (setq args* (nconc args* (list x (cadr arg) (caddr arg))))
          (setq arg (cdddr arg)))
         ;; KEYWORD
         ;;   :prefix-docstring STRING
         ;;   :prefix-map SYMBOL
         ;;   :prefix STRING
         ;;   :filter SEXP
         ;;   :menu-name STRING
         ;;   :package SYMBOL
         ((or (and (eq x :prefix) (stringp (cadr arg)))
              (and (eq x :prefix-map) (symbolp (cadr arg)))
              (and (eq x :prefix-docstring) (stringp (cadr arg)))
              (eq x :filter)
              (and (eq x :menu-name) (stringp (cadr arg)))
              (and (eq x :package) (symbolp (cadr arg))))
          (setq args* (nconc args* (list x (cadr arg))))
          (setq arg (cddr arg)))
         ((listp x)
          (setq args*
                (nconc args* (use-package-normalize-evil-binder name keyword x)))
          (setq arg (cdr arg)))
         (t
          ;; Error!
          (use-package-error
           (concat (symbol-name keyword)
                   " wants arguments acceptable to the `bind-keys' macro,"
                   " or a list of such values"))))))
    args*))

;;;###autoload
(defalias 'use-package-normalize/:evil-bind 'use-package-normalize-evil-binder)
;;;###autoload
(defalias 'use-package-normalize/:evil-bind* 'use-package-normalize-evil-binder)

;; jww (2017-12-07): This is too simplistic. It will fail to determine
;; autoloads in this situation:
;;   (use-package foo
;;     :bind (:map foo-map (("C-a" . func))))
;;;###autoload
(defalias 'use-package-autoloads/:evil-bind 'use-package-autoloads-mode)
;;;###autoload
(defalias 'use-package-autoloads/:evil-bind* 'use-package-autoloads-mode)

;;;###autoload
(defun use-package-handler/:evil-bind
    (name keyword args rest state &optional bind-macro)
  (use-package-concat
   ;; (when (bound-and-true-p byte-compile-current-file)
   ;;   (use-package-plist-append state
   ;;                             :preface
   ;;                             '(declare-function
   ;;                               'evil-get-auxiliary-keymap
   ;;                               "evil-core")))
   (use-package-process-keywords name rest state)
   '((unless (fboundp 'evil-get-auxiliary-keymap)
        (autoload #'evil-get-auxiliary-keymap "evil-core" nil t)))
   (when (bound-and-true-p byte-compile-current-file)
     '((eval-when-compile
         (declare-function 'evil-get-auxiliary-keymap "evil-core"))))
   `(,@(mapcar
        #'(lambda (args)
            (let (maps
                  args*)
              (while args
                (let ((x (car args)))
                  (cond
                   ;; (KEY . COMMAND)
                   ((consp x)
                    (setq args* (nconc args* (list x))
                          args (cdr args)))
                   ;; KEYWORD
                   ;;   :map STATE KEYMAP
                   ((and (eq x :map)
                         (symbolp (cadr args))
                         (symbolp (caddr args)))
                    (let* ((evil-state (cadr args))
                           (map (caddr args))
                           (evil-map (intern (format "evil-%s-state-%s"
                                                     evil-state map))))
                      (setq maps (cons (list evil-state map evil-map) maps)
                            args* (nconc args* (list x evil-map))
                            args (cdddr args))))
                   (t
                    (setq args* (nconc args* (list x (cadr args)))
                          args (cddr args))))))
              `(with-eval-after-load ',name
                 (with-eval-after-load 'evil
                   ,@(mapcar
                      #'(lambda (m)
                          `(defvar ,(caddr m)
                             (evil-get-auxiliary-keymap ,(cadr m) ',(car m) t
                                                        t)))
                      (reverse maps))
                   (,(if bind-macro bind-macro 'bind-keys)
                    :package ,name ,@(use-package-normalize-commands args*))))))
        (use-package-split-list-at-keys :break args)))))

(defun use-package-handler/:evil-bind* (name keyword arg rest state)
  (use-package-handler/:evil-bind name keyword arg rest state 'bind-keys*))

(let ((b (member :bind-keymap* use-package-keywords)))
  (setcdr b (cons :evil-bind (cons :evil-bind* (cdr b)))))

(add-to-list 'use-package-merge-key-alist
             '(:evil-bind . (lambda (new old) (append new (list :break) old)))
             t)

(provide 'use-package-bind-key)
