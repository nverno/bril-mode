((bril-mode
  (eval
   .
   (let ((lmap (make-sparse-keymap)))
     (set-keymap-parent lmap (current-local-map))
     (define-key lmap (kbd "M-C-?") #'smie-config-show-indent)
     (use-local-map lmap)))))
