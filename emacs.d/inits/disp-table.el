(require 'disp-table)

(defface wrap-glyph
  '((((min-colors 16) (class color))
     :foreground "blue")
    (t
     :inverse-video t))
  "Face for wrap glyph."
  :group 'basic-faces)

(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code #x21A9 'wrap-glyph))
(set-display-table-slot standard-display-table 'selective-display
                        (vector (make-glyph-code #x2026)))
(set-display-table-slot standard-display-table 'vertical-border
                        (make-glyph-code #x2502))
