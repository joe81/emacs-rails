;;; inflections.el ---

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,
;;          Howard Yeh <hayeah at gmail dot com>

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

(require 'cl)

(defstruct inflections-struct singulars plurals irregulars uncountables)

(defvar inflections
  (make-inflections-struct :singulars nil
                           :plurals nil
                           :irregulars nil
                           :uncountables nil))

(defmacro define-inflectors (&rest specs)
  ;; TODO really ought to do error checking here to ensure integrity.
  (let (singulars plurals irregulars uncountables)
    (loop for (type . rest) in specs do
          (case type
            (:singular (push rest (inflections-struct-singulars inflections)))
            (:plural (push rest (inflections-struct-plurals inflections)))
            (:irregular (push rest (inflections-struct-irregulars inflections)))
            (:uncountable (setf (inflections-struct-uncountables inflections)
                                (append rest (inflections-struct-uncountables inflections))))))))

(define-inflectors
  (:plural "$" "s")
  (:plural "s$" "s")
  (:plural "\\(ax\\|test\\)is$" "\\1es")
  (:plural "\\(octop\\|vir\\)us$" "\\1i")
  (:plural "\\(alias\\|status\\)$" "\\1es")
  (:plural "\\(bu\\)s$" "\\1ses")
  (:plural "\\(buffal\\|tomat\\)o$" "\\1oes")
  (:plural "\\([ti]\\)um$" "\\1a")
  (:plural "sis$" "ses")
  (:plural "\\(?:\\([^f]\\)fe\\|\\([lr]\\)f\\)$" "\\1\\2ves")
  (:plural "\\(hive\\)$" "\\1s")
  (:plural "\\([^aeiouy]\\|qu\\)y$" "\\1ies")
  (:plural "\\(x\\|ch\\|ss\\|sh\\)$" "\\1es")
  (:plural "\\(matr\\|vert\\|ind\\)ix\\|ex$" "\\1ices")
  (:plural "\\([m\\|l]\\)ouse$" "\\1ice")
  (:plural "^\\(ox\\)$" "\\1en")
  (:plural "\\(quiz\\)$" "\\1zes")

  (:singular "s$" "")
  (:singular "\\(n\\)ews$" "\\1ews")
  (:singular "\\([ti]\\)a$" "\\1um")
  (:singular "\\(\\(a\\)naly\\|\\(b\\)a\\|\\(d\\)iagno\\|\\(p\\)arenthe\\|\\(p\\)rogno\\|\\(s\\)ynop\\|\\(t\\)he\\)ses$" "\\1\\2sis")
  (:singular "\\(^analy\\)ses$" "\\1sis")
  (:singular "\\([^f]\\)ves$" "\\1fe")
  (:singular "\\(hive\\)s$" "\\1")
  (:singular "\\(tive\\)s$" "\\1")
  (:singular "\\([lr]\\)ves$" "\\1f")
  (:singular "\\([^aeiouy]\\|qu\\)ies$" "\\1y")
  (:singular "\\(s\\)eries$" "\\1eries")
  (:singular "\\(m\\)ovies$" "\\1ovie")
  (:singular "\\(x\\|ch\\|ss\\|sh\\)es$" "\\1")
  (:singular "\\([m\\|l]\\)ice$" "\\1ouse")
  (:singular "\\(bus\\)es$" "\\1")
  (:singular "\\(o\\)es$" "\\1")
  (:singular "\\(shoe\\)s$" "\\1")
  (:singular "\\(cris\\|ax\\|test\\)es$" "\\1is")
  (:singular "\\(octop\\|vir\\)i$" "\\1us")
  (:singular "\\(alias\\|status\\)es$" "\\1")
  (:singular "^\\(ox\\)en" "\\1")
  (:singular "\\(vert\\|ind\\)ices$" "\\1ex")
  (:singular "\\(matr\\)ices$" "\\1ix")
  (:singular "\\(quiz\\)zes$" "\\1")

  (:irregular "person" "people")
  (:irregular "man" "men")
  (:irregular "child" "children")
  (:irregular "sex" "sexes")
  (:irregular "move" "moves")

  (:uncountable "equipment" "information" "rice" "money" "species" "series" "fish" "sheep"))

(defun singularize-string (str)
  (when (stringp str)
    (or (car (member str (inflections-struct-uncountables inflections)))
        (caar (member* str (inflections-struct-irregulars inflections) :key 'cadr :test 'equal))
        (loop for (from to) in (inflections-struct-singulars inflections)
              for singular = (string=~ from str (sub to))
              when singular do (return singular))
        str)))

(defun pluralize-string (str)
  (when (stringp str)
    (or (car (member str (inflections-struct-uncountables inflections)))
        (cadar (member* str (inflections-struct-irregulars inflections) :key 'car :test 'equal))
        (loop for (from to) in (inflections-struct-plurals inflections)
              for plurals = (string=~ from str (sub to))
              when plurals do (return plurals))
        str)))

(provide 'inflections)