(defgroup ned nil
  "Ned emulation within Emacs."
  :prefix "ned-"
  :group 'emulations)

(defgroup ned-faces nil
  "Faces used by Ned."
  :prefix "ned-"
  :group 'ned)

(defface ned-info-on-file-mode
  '((((min-colors 16) (class color))
     :foreground "dark green")
    (t
      :inverse-video t))
    "Face for file mode in ned-info-on-file message."
    :group 'ned-faces)

(defface ned-info-on-file-read-only
  '((((min-colors 16) (class color))
     :foreground "dark red")
    (t
      :inverse-video t))
    "Face for read-only notification in ned-info-on-file message."
    :group 'ned-faces)

(defface ned-info-on-file-coding-system-type
  '((t nil))
  "Face for coding-system type in ned-info-on-file message."
  :group 'ned-faces)

(defgroup ned-misc nil
  "Miscellaneous Ned customizations."
  :prefix "ned-"
  :group 'ned)

(defcustom ned-info-on-file-buffer-modified "+"
   "String used for notifying that the current buffer has been modified."
   :type 'string
   :group 'ned-misc)

(defcustom ned-info-on-file-buffer-read-only "-"
   "String used for notifying that the current buffer is read only."
   :type 'string
   :group 'ned-misc)

(defcustom ned-info-on-file-file-read-only "RO"
   "String used for notifying that the file that the current buffer is visiting
is read only."
   :type 'string
   :group 'ned-misc)

(defcustom ned-info-on-file-base-read-only "→"
   "String used for notifying that the file that the current base buffer is
visiting is read only."
   :type 'string
   :group 'ned-misc)

(defcustom ned-info-on-file-coding-system-big-endian "big-endian"
   "String used for notifying that the coding system of the current buffer is
big endian."
   :type 'string
   :group 'ned-misc)

(defcustom ned-info-on-file-coding-system-bomed "bom"
   "String used for notifying that the coding system of the current buffer is
BOMed."
   :type 'string
   :group 'ned-misc)

(defcustom ned-info-on-file-point "line %d of %d (%d%%%%); column %d"
   "String used for displaying information about point."
   :type 'string
   :group 'ned-misc)

(defcustom ned-info-on-file-point-bytes
   "line %d of %d (%d%%%%); column %d (byte index %d)"
   "String used for displaying information about point when the column index is
not the same as the byte index."
   :type 'string
   :group 'ned-misc)

(defvar ned-info-on-file-mode-line-format
  '('(window-system "" "[%F] ")
    "%b "
    "%[[" mode-name mode-line-process minor-mode-alist "%n" "]%] "
    (:eval
      (loop for (string . more?)
            on (mapcar #'(lambda (c) (if (consp c) (propertize (car c) 'face (cdr c)) c))
                       (remove-if #'(lambda (c) (zerop (length (if (consp c) (car c) c))))
                                  (list*
                                    (cons
                                      (concat
                                        (if (buffer-modified-p) ned-info-on-file-buffer-modified)
                                        (if buffer-read-only ned-info-on-file-buffer-read-only))
                                      'ned-info-on-file-mode)
                                    (cons
                                      (let* ((b (buffer-file-name))
                                             (p (if (not b) (buffer-file-name (buffer-base-buffer))))
                                             (file (or b p)))
                                        (if (and file (not (file-writable-p file)))
                                          (concat
                                            (if p ned-info-on-file-base-read-only)
                                            ned-info-on-file-file-read-only)))
                                      'ned-info-on-file-read-only)
                                    (let* ((cs (if (local-variable-p 'buffer-file-coding-system)
                                                 buffer-file-coding-system
                                                 default-buffer-file-coding-system))
                                           (cs-type (coding-system-type cs))
                                           (cs-eol (coding-system-eol-type-mnemonic cs))
                                           (cs-endian (coding-system-get cs 'endian))
                                           (cs-bom (coding-system-get cs 'bom)))
                                      (list
                                        (if (not (eq cs-type 'utf-8))
                                          (cons (symbol-name cs-type) 'ned-info-on-file-coding-system-type))
                                        (if (eq cs-endian 'big)
                                          ned-info-on-file-coding-system-big-endian)
                                        (if cs-bom
                                          ned-info-on-file-coding-system-bomed)
                                        (if (not (eq cs-eol eol-mnemonic-unix))
                                          cs-eol))))))
            for first? = t then nil
            if first? collect "["
            collect (replace-regexp-in-string "%" "%%" string)
            if more? collect ","
            else collect "] "))
    (:eval (let* ((start (save-excursion (beginning-of-line) (point)))
                  (end (save-excursion (end-of-line) (point)))
                  (current-line (+ (count-lines (point-min) end) (if (= start end) 1 0)))
                  (line-count (count-lines (point-min) (point-max)))
                  (percentage (/ (* 100 current-line) (max line-count current-line)))
                  (column (1+ (current-column)))
                  (byte-index (1+ (- (position-bytes (point)) (position-bytes (line-beginning-position))))))
             (format
               (if (= column byte-index)
                 ned-info-on-file-point
                 ned-info-on-file-point-bytes)
               current-line line-count percentage column byte-index)))))

; TODO: Should use something similar to mode-line-modes
;;;###autoload
(defun ned-info-on-file ()
  (interactive)
  (display-message-or-buffer
    (format-mode-line ned-info-on-file-mode-line-format) "*ned-info*"))

(provide 'ned-info-on-file)
