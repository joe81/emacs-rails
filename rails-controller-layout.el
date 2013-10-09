;;; rails-controller-layout.el ---

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL$
;; $Id$

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:

(defvar rails-controller-layout:recent-template-type nil)

(defun rails-controller-layout:switch-to-action-in-controller (controller-name action-name)
  "Open CONTROLLER-NAME and go to ACTION-NAME."
  (if (or (rails-core:find-file-if-exist (rails-core:controller-file controller-name))
          (rails-core:find-file-if-exist (rails-core:mailer-file controller-name)))
      (progn
        (goto-char (point-min))
        (when action-name
          (if (search-forward-regexp (concat "^[ ]*def[ ]*" action-name) nil t)
              (recenter))
          (message (format "%s: %s" (substring (symbol-name (rails-core:buffer-type)) 1) controller-name))))))

(defun rails-controller-layout:switch-to-view (controller-name action-name)
  "Open the ACTION-NAME file for CONTROLLER-NAME in the views directory."
  (when action-name
    (let ((views (rails-controller-layout:view-files controller-name action-name))
          (title (substring (symbol-name (rails-core:buffer-type)) 1)))
      (cond
       ((= (length views) 1)
        (find-file (first views))
        (message "%s: %s#%s" title controller-name action-name))
       ((= (length views) 0)
        (rails-controller-layout:create-view-for-action controller-name action-name))))))

(defun rails-controller-layout:toggle-action-view ()
  (interactive)
  (let ((controller-name (or (rails-core:current-controller) (rails-core:current-mailer)))
        (action-name (rails-core:current-action)))
    (case (rails-core:buffer-type)
      (:view
       (rails-controller-layout:switch-to-action-in-controller controller-name action-name))
      (:mailer
       (rails-controller-layout:switch-to-view controller-name action-name))
      (:controller
       (if action-name
           (rails-controller-layout:switch-to-view controller-name action-name)
         (rails-controller-layout:switch-to :controllers-test))))))

(defun rails-controller-layout:create-view-for-action (controller-name action-name)
  (let ((type
         (if rails-controller-layout:recent-template-type
             rails-controller-layout:recent-template-type
           (car rails-templates-list))))
    (setq type
          (completing-read (format "View for %s#%s not found, create %s.[%s]? "
                                   controller-name action-name action-name type)
                           rails-templates-list
                           nil nil type))
    (setq rails-controller-layout:recent-template-type type)
    (let ((file (rails-core:file (concat "app/views/"
                                         (replace-regexp-in-string "_controller" ""
                                                                   (rails-core:file-by-class controller-name t))))))
        (make-directory file t)
        (find-file (format "%s/%s.%s" file action-name type)))))

(defun rails-controller-layout:view-files (controller-name &optional action)
  "Retun a list containing the view file for CONTROLLER-NAME#ACTION.
If the action is nil, return all views for the controller."
  (rails-project:with-root
   (root)
   (let ((dir (rails-core:file
               (rails-core:views-dir
                (rails-core:short-controller-name controller-name)))))
     (if (file-directory-p dir)
         (directory-files dir t
          (if action
              (concat "^" action (rails-core:regex-for-match-view))
            (rails-core:regex-for-match-view)))))))

(defun rails-controller-layout:views-menu (controller-name)
  "Make menu of view for CONTROLLER-NAME."
  (let (menu)
    (setq menu
          (mapcar (lambda(i) (cons (file-name-nondirectory i) i))
                  (rails-controller-layout:view-files controller-name nil)))
    (when (zerop (length menu))
      (setq menu (list)))
    menu))

(defun rails-controller-layout:keymap (type)
  (let* ((name (capitalize (substring (symbol-name type) 1)))
         (map (make-sparse-keymap))
         (menu (make-sparse-keymap)))
    (when type
      (define-keys menu
        ([goto-migration]  '(menu-item "Go to Migration"
                                       rails-controller-layout:switch-to-migration
                                       :enable (and (not (rails-core:current-mailer))
                                                    (rails-core:migration-file-by-model
                                                     (singularize-string (rails-core:current-controller))))))
        ([goto-model]      '(menu-item "Go to Model"
                                       rails-controller-layout:switch-to-model
                                       :enable (and (rails-core:model-exist-p
                                                     (singularize-string (rails-core:current-controller))))))
        ([goto-decorator]  '(menu-item "Go to Decorator"
                                       rails-controller-layout:switch-to-decorator
                                       :enable (and (rails-core:decorator-exist-p
                                                     (singularize-string (rails-core:current-controller))))))
        ([goto-helper]     '(menu-item "Go to Helper"
                                       rails-controller-layout:switch-to-helper
                                       :enable (and (not (eq (rails-core:buffer-type) :helper)))))
        ([goto-ftest]      '(menu-item "Go to Controllers Test"
                                       rails-controller-layout:switch-to-controllers-test
                                       :enable (and (not (rails-core:current-mailer))
                                                    (not (eq (rails-core:buffer-type) :controllers-test)))))
        ([goto-controller] '(menu-item "Go to Controller"
                                       rails-controller-layout:switch-to-controller
                                       :enable (and (not (rails-core:current-mailer))
                                                    (not (eq (rails-core:buffer-type) :controller)))))
        ([goto-utest]      '(menu-item "Go to Models Test"
                                       rails-controller-layout:switch-to-models-test
                                       :enable (rails-core:current-mailer))))
      (define-keys map
        ((rails-key "g") 'rails-controller-layout:switch-to-migration)
        ((rails-key "m") 'rails-controller-layout:switch-to-model)
        ((rails-key "h") 'rails-controller-layout:switch-to-helper)
        ((rails-key "d") 'rails-controller-layout:switch-to-decorator)
        ((rails-key "f") 'rails-controller-layout:switch-to-controllers-test)
        ((rails-key "c") 'rails-controller-layout:switch-to-controller)
        ((rails-key "u") 'rails-controller-layout:switch-to-models-test)
        ([menu-bar rails-controller-layout] (cons name menu))))
    map))

(defun rails-controller-layout:switch-to (type)
  (let* ((name (capitalize (substring (symbol-name type) 1)))
         (controller (rails-core:current-controller))
         (model (singularize-string controller))
         (mailer (rails-core:current-mailer))
         (item (case type
                 (:helper (rails-core:helper-file controller))
                 (:controllers-test (rails-core:controllers-test-file controller))
                 (:controller (rails-core:controller-file controller))
                 (:model (rails-core:model-file model))
                 (:models-test (rails-core:models-test-file mailer))
                 (:decorator (rails-core:decorator-file-by-model model))
                 (:migration (rails-core:migration-file-by-model model)))))
    (if item
      (find-or-ask-to-create (format "%s does not exists do you want to create it? " item)
                             (rails-core:file item))
      (message "%s not found" name))))

(defun rails-controller-layout:switch-to-helper () (interactive) (rails-controller-layout:switch-to :helper))
(defun rails-controller-layout:switch-to-controllers-test () (interactive) (rails-controller-layout:switch-to :controllers-test))
(defun rails-controller-layout:switch-to-controller () (interactive) (rails-controller-layout:switch-to :controller))
(defun rails-controller-layout:switch-to-model () (interactive) (rails-controller-layout:switch-to :model))
(defun rails-controller-layout:switch-to-migration () (interactive) (rails-controller-layout:switch-to :migration))
(defun rails-controller-layout:switch-to-models-test () (interactive) (rails-controller-layout:switch-to :models-test))
(defun rails-controller-layout:switch-to-decorator () (interactive) (rails-controller-layout:switch-to :decorator))

(defun rails-controller-layout:menu ()
  (interactive)
  (let* ((type (rails-core:buffer-type))
         (title (capitalize (substring (symbol-name type) 1)))
         (controller (rails-core:current-controller))
         (action (rails-core:current-action))
         (model (singularize-string controller))
         (mailer (rails-core:current-mailer))
         (decorator (rails-core:current-decorator))
         (item (rails-controller-layout:views-menu (or controller mailer))))
    (add-to-list 'item (rails-core:menu-separator))
    (when controller
      (when (rails-core:model-exist-p model)
        (when (rails-core:migration-file-by-model model)
          (add-to-list 'item (cons "Migration" :migration)))
        (add-to-list 'item (cons "Model" :model)))
      (when (rails-core:decorator-exist-p decorator)
        (add-to-list 'item (cons "Decorator" :decorator)))
      (unless (eq type :helper)
        (add-to-list 'item (cons "Helper" :helper)))
      (unless (eq type :controllers-test)
        (add-to-list 'item (cons "Controllers Test" :controllers-test)))
      (unless (eq type :controller)
        (add-to-list 'item (cons "Controller" :controller))))
    (when mailer
      (add-to-list 'item (cons "Models Test" (rails-core:models-test-file mailer)))
      (when (eq type :view)
        (add-to-list 'item (cons "Mailer" (rails-core:mailer-file mailer)))))
    (setq item
          (rails-core:menu
           (list (concat title " " controller
                         (when action (format " (%s)" action)))
                 (cons "Please select.."
                       item))))
    (typecase item
      (symbol (rails-controller-layout:switch-to item))
      (string (rails-core:find-file-if-exist item)))))

(provide 'rails-controller-layout)
