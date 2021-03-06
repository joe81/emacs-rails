(define-minor-mode rails-operations-test-minor-mode
  "Minor mode for Trailblazer operation tests."
  :lighter " OperationTest"
  :keymap (let ((map (rails-model-layout:keymap :operations-test)))
            (define-key map rails-minor-mode-test-current-method-key 'rails-test:run-current-method)
            (define-key map [menu-bar rails-model-layout run] '("Test current method" . rails-test:run-current-method))
            map)
  (setq rails-secondary-switch-func 'rails-model-layout:menu))

(provide 'rails-operations-test-minor-mode)
