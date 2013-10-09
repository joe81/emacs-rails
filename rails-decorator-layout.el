;;; rails-decorator-layout.el ---

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

(defun rails-decorator-layout:keymap (type)
  (let* ((name (capitalize (substring (symbol-name type) 1)))
         (map (make-sparse-keymap))
         (menu (make-sparse-keymap)))
    (when type
      (define-keys menu
        ([goto-decorator]       '(menu-item "Go to Decorator"
                                            rails-decorator-layout:switch-to-decorator
                                            :enable (and (not (eq (rails-core:buffer-type) :decorator)))))
        ([goto-decorators-test] '(menu-item "Go to Decorators Test"
                                            rails-decorator-layout:switch-to-decorators-test
                                            :enable (and (not (eq (rails-core:buffer-type) :decorators-test)))))
        ([goto-model]           '(menu-item "Go to Model"
                                            rails-decorator-layout:switch-to-model
                                            :enable (and (not (eq (rails-core:buffer-type) :model))
                                                         (rails-core:model-exist-p (rails-core:current-decorator)))))
        ([goto-models-test]     '(menu-item "Go to Models Test"
                                            rails-decorator-layout:switch-to-models-test
                                            :enable (and (rails-core:models-test-exist-p (or (rails-core:current-decorator))))))
        ([goto-controller]      '(menu-item "Go to Controller"
                                            rails-decorator-layout:switch-to-controller
                                            :enable (rails-core:controller-file-by-model (rails-core:current-decorator))))
        ([goto-fixture]    '(menu-item "Go to Fixture"
                                       rails-decorator-layout:switch-to-fixture
                                       :enable (and (not (eq (rails-core:buffer-type) :fixture))
                                                    (rails-core:fixture-exist-p (rails-core:current-decorator))))))

      (define-keys map
        ((rails-key "m") 'rails-decorator-layout:switch-to-model)
        ((rails-key "u") 'rails-decorator-layout:switch-to-models-test)
        ((rails-key "c") 'rails-decorator-layout:switch-to-controller)
        ((rails-key "d") 'rails-decorator-layout:switch-to-decorator)
        ((rails-key "t") 'rails-decorator-layout:switch-to-decorators-test)
        ((rails-key "x") 'rails-decorator-layout:switch-to-fixture)
        ([menu-bar rails-decorator-layout] (cons name menu))))
      map))

(defun rails-decorator-layout:switch-to (type)
  (let* ((name (capitalize (substring (symbol-name type) 1)))
         (model (rails-core:current-decorator))
         (decorator (rails-core:current-decorator))
         (item (case type
                 (:decorator (rails-core:decorator-file-by-model model))
                 (:decorators-test (rails-core:decorators-test-file model))
                 (:controller (rails-core:controller-file-by-model model))
                 (:model (rails-core:model-file model))
                 (:models-test (rails-core:models-test-file model))
                 (:fixture (rails-core:fixture-file model)))))
    (if item
      (find-or-ask-to-create (format "%s does not exists do you want to create it? " item)
                             (rails-core:file item))
      (message "%s not found" name))))

(defun rails-decorator-layout:switch-to-decorator () (interactive) (rails-decorator-layout:switch-to :decorator))
(defun rails-decorator-layout:switch-to-decorators-test () (interactive) (rails-decorator-layout:switch-to :decorators-test))
(defun rails-decorator-layout:switch-to-controller () (interactive) (rails-decorator-layout:switch-to :controller))
(defun rails-decorator-layout:switch-to-fixture () (interactive) (rails-decorator-layout:switch-to :fixture))
(defun rails-decorator-layout:switch-to-model () (interactive) (rails-decorator-layout:switch-to :model))
(defun rails-decorator-layout:switch-to-models-test () (interactive) (rails-decorator-layout:switch-to :models-test))

(defun rails-decorator-layout:menu ()
  (interactive)
  (let* ((item (list))
         (type (rails-core:buffer-type))
         (title (capitalize (substring (symbol-name type) 1)))
         (model (rails-core:current-decorator))
         (controller (pluralize-string model)))
    (when model
      (when (and (not (eq type :migration))
                 (rails-core:migration-file-by-model model))
        (add-to-list 'item (cons "Migration" :migration)))
      (unless (eq type :fixture)
        (add-to-list 'item (cons "Fixture" :fixture)))
      (when (rails-core:controller-exist-p controller)
        (add-to-list 'item (cons "Controller" :controller)))
      (when (rails-core:decorator-exist-p model)
        (add-to-list 'item (cons "Decorator" :decorator)))
      (unless (eq type :decorators-test)
        (add-to-list 'item (cons "Decorators Test" :decorators-test)))
      (unless (eq type :models-test)
        (add-to-list 'item (cons "Models Test" :models-test)))
      (unless (eq type :model)
        (add-to-list 'item (cons "Model" :model))))
    (when item
      (setq item
            (rails-core:menu
             (list (concat title " " model)
                   (cons "Please select.."
                         item))))
      (typecase item
        (symbol (rails-decorator-layout:switch-to item))
        (string (rails-core:find-file-if-exist item))))))

(provide 'rails-decorator-layout)
