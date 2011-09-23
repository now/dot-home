(require 'yasnippet)
(yas/initialize)
(setq yas/root-directory "~/share/emacs/snippets"
      yas/skip-and-clear-key "\C-w")
(yas/load-directory yas/root-directory)
(yas/global-mode 1)