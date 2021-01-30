;;; homebrew-manager.el --- Manage your homebrew packages through Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Björn Larsson
;;
;; Author: Björn Larsson <http://github/fuzzycode>
;; Maintainer: Björn Larsson <develop@bjornlarsson.net>
;; Created: January 30, 2021
;; Modified: January 30, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/fuzzycode/homebrew-manager
;; Package-Requires: ((emacs "24.3") (s "1.11.0") (dash "2.12.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'tabulated-list)
(require 'dash)
(require 's)
(require 'cl-lib)

(defun homebrew--call (cmd &rest arguments)
  "Call brew CMD with provided ARGUMENTS."
  (let ((command (format "brew %s%s%s" cmd (if arguments " " "") (string-join arguments " "))))
    (-filter #'s-present?
             (s-split "\n" (shell-command-to-string command)))))

(defun homebrew--list-packages ()
  "List all packages installed through brew."
  (let ((leaves (homebrew--call "leaves"))
        (outdated (homebrew--call "outdated")))
    (cl-labels ((filter (item)
                        (let* ((items (split-string item))
                               (name (car items)))
                          (list name (vconcat (-concat items
                                                       (if (-contains? leaves name) '("*") '(""))
                                                       (if (-contains? outdated name) '("!") '(""))))))))
      (-map #'filter (homebrew--call "list" "--version")))))


(define-derived-mode brew-package-mode tabulated-list-mode "homebrew-package-list"
  "A mode to list all your homebrew installed packages."

  ;; Ensure that brew is up to date when entering
  (shell-command-to-string "brew update")

  (setq truncate-lines t)
  (setq tabulated-list-format [("Name" 20 t)
                               ("Version" 10 nil)
                               ("Leaf" 4 nil)
                               ("Outdated" 1 nil)])

  (setq tabulated-list-entries #'homebrew--list-packages)

  (tabulated-list-init-header)
  (tabulated-list-print))

;;;###autoload
(defun homebrew-list-packages ()
  "Lists all homebrew installed packages in a tabulated list."
  (interactive)
  (let ((buffer (generate-new-buffer "*brew list*")))
    (with-current-buffer buffer
      (brew-package-mode))
    (display-buffer buffer)))

(provide 'homebrew-manager)
;;; homebrew-manager.el ends here
