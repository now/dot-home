(setq nxml-slash-auto-complete-flag t)
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/share/emacs/etc/schema/schemas.xml"))
