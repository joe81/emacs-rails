(define-minor-mode rails-system-test-minor-mode
  "Minor mode for Rails system tests."
  :lighter " SystemTest"
  :keymap (let ((map (rails-model-layout:keymap :system-test)))
            (define-key map rails-minor-mode-test-current-method-key 'rails-test:run-current-method)
            (define-key map [menu-bar rails-model-layout run] '("Test current method" . rails-test:run-current-method))
            map)
  (setq rails-secondary-switch-func 'rails-model-layout:menu))

(provide 'rails-system-test-minor-mode)
