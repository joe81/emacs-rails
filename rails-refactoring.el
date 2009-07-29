(defun rails-refactoring:file (class &optional type)
  (cond ((eq type :controller)
         (rails-core:controller-file class))

        ((eq type :functional-test)
         (rails-core:functional-test-file class))

        ((eq type :helper)
         (rails-core:helper-file class))

        ((eql type :helper-test)
         (format "test/unit/helper/%s" (rails-core:file-by-class (concat class "HelperTest"))))
        
        ((eql type :views-dir)
         (rails-core:views-dir class))

        (t (error "not yet implemented type %s" type))))

(defun rails-refactoring:file-exists-p (class &optional type)
  (file-exists-p (rails-core:file (rails-refactoring:file class type))))

(defun rails-refactoring:rename-class (from to &optional type)
  (let ((from-file (rails-refactoring:file from type))
        (to-file (rails-refactoring:file to type)))
    (message "rename file from %s to %s" from-file to-file)
    (rename-file (rails-core:file from-file) (rails-core:file to-file))
    (let ((buffer (get-file-buffer (rails-core:file from-file))))
      (when buffer (kill-buffer buffer)))

    (message "change definition from %s to %s" from to)
    (let ((buffer (get-file-buffer (rails-core:file to-file))))
      (when buffer (kill-buffer buffer)))
    (find-file (rails-core:file to-file))
    (goto-char (point-min))
    (if (re-search-forward (concat "^\\(class\\|module\\)[ \t]+" from) nil t)
      (replace-match (concat "\\1 " to) nil nil)
      (error "failed find class definition for renaming")))
    (save-buffer))

(defun rails-refactoring:controller-names ()
  (mapcar (lambda (name) (remove-postfix name "Controller")) (rails-core:controllers)))

(defun rails-refactoring:current-controller-or-nil ()
  (condition-case nil (rails-core:current-controller) (error nil)))

(defun rails-refactoring:rename-controller (from to)
  (interactive (let* ((from (completing-read "Rename controller: "
                                             (rails-refactoring:controller-names) nil t
                                             (rails-refactoring:current-controller-or-nil)))
                      (to (read-string "To: ")))
                 (list from to)))

  (save-some-buffers)

  (unless (rails-core:controller-exist-p from)
    (error "controller '%s' doesn't exists" from))

  ;; ensure no existing file are in the way
  (let ((file (find-if (lambda (file) (and file (file-exists-p (rails-core:file file))))
                       (append (mapcar (lambda (fn) (funcall fn to))
                                       '(rails-core:controller-file rails-core:helper-file rails-core:views-dir))
                               (list (rails-core:layout-file (decamelize to)))))))
    (when file (error "file '%s' already exists" file)))

  (message "refactoring controller class file")
  (rails-refactoring:rename-class from to :controller)

  (when (rails-refactoring:file-exists-p from :views-dir)
    (let ((from-dir (rails-refactoring:file from :views-dir))
          (to-dir (rails-refactoring:file to :views-dir)))
      (message "rename view directory from %s to %s" from-dir to-dir)
      (rename-file (rails-core:file from-dir) (rails-core:file to-dir))))

  (when (rails-refactoring:file-exists-p from :functional-test)
    (message "refactoring functional test class file")
    (rails-refactoring:rename-class from to :functional-test))

  (when (rails-refactoring:file-exists-p from :helper)
    (message "refactoring helper class file")
    (rails-refactoring:rename-class from to :helper))

  (when (rails-refactoring:file-exists-p from :helper-test)
    (message "refactoring helper test class file")
    (rails-refactoring:rename-class from to :helper-test))

  (error "should rename layout")

  (error "should query replace references to controller class")
  (error "should query replace references to controller in routing")
  (error "should query replace references to helper and includes of class name")
  (error "should query replace render partial / template"))
  