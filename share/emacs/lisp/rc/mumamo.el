(autoload 'eruby-html-mumamo "mumamo-fun")
(eval-after-load 'mumamo
  '(setq mumamo-chunk-coloring 511))
(add-to-list 'auto-mode-alist '("html[/\\\\][^/\\\\]*\\.erb\\'" . eruby-html-mumamo))
