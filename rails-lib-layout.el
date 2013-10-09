(defun rails-lib-layout:keymap (type)
  (let* ((name (capitalize (substring (symbol-name type) 1)))
         (map (make-sparse-keymap))
         (menu (make-sparse-keymap)))
    (when type
      (define-keys menu
        ([goto-lib]      '(menu-item "Go to Lib"
                                     rails-lib-layout:switch-to-lib
                                     :enable (and (not (eq (rails-core:buffer-type) :lib))
                                                  (rails-core:lib-exist-p (rails-core:current-lib))))))
      (define-keys map
        ((rails-key "l")         'rails-lib-layout:switch-to-lib)
        ((rails-key "r")         'rails-lib-layout:switch-to-models-test)
        ([menu-bar rails-lib-layout] (cons name menu))))
    map))

(defun rails-lib-layout:switch-to (type)
  (let* ((name (capitalize (substring (symbol-name type) 1)))
         (lib (rails-core:current-lib))
         (item (if lib lib))
         (item (case type
                 (:lib (rails-core:lib-file lib)))))
    (if item
        (let ((file (rails-core:file item)))
          (if (file-exists-p file)
              (progn
                (find-file file)
                (message (format "%s: %s" (substring (symbol-name type) 1) item)))
            (message "File %s not exists" file)))
      (message "%s not found" name))))

(defun rails-lib-layout:switch-to-lib () (interactive) (rails-lib-layout:switch-to :lib))

(defun rails-lib-layout:menu ()
  (interactive)
  (let* ((item (list))
         (type (rails-core:buffer-type))
         (title (capitalize (substring (symbol-name type) 1)))
         (lib (rails-core:current-lib)))
    (when lib
      (unless (eq type :lib)
        (add-to-list 'item (cons "Lib" :lib))))
    (when item
      (setq item
            (rails-core:menu
             (list (concat title " " lib)
                   (cons "Please select.."
                         item))))
      (typecase item
        (symbol (rails-lib-layout:switch-to item))
        (string (rails-core:find-file-if-exist item))))))

(provide 'rails-lib-layout)
