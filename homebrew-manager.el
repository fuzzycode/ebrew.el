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
(eval-when-compile (require 'cl-lib))

(defgroup homebrew-manager nil
  "A package manager for packages installed with homebrew"
  :prefix "hbm-"
  :group 'applications)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization Variables
(defcustom hbm-name-column-width 30
  "Width of the package name column."
  :type 'integer
  :group 'homebrew-manager)

(defcustom hbm-version-column-width 10
  "Width of the package version column."
  :type 'integer
  :group 'homebrew-manager)


(defun homebrew--call (cmd &rest arguments)
  "Call brew CMD with provided ARGUMENTS."
  (let ((command (format "brew %s %s" cmd (s-join " " arguments))))
    (s-split "\n" (shell-command-to-string command) t)))

(defun homebrew--list-packages ()
  "List all packages installed through brew."
  (let ((leaves (homebrew--call "leaves"))
        (outdated (homebrew--call "outdated"))
        (pinned (homebrew--call "pinned")))
    (cl-labels ((filter (item)
                        (let* ((items (split-string item))
                               (name (car items)))
                          (list name (vconcat (-concat items
                                                       (if (-contains? leaves name) '("*") '(""))
                                                       (if (-contains? outdated name) '("*") '(""))
                                                       (if (-contains? pinned name) '("*") '(""))))))))
      (-map #'filter (homebrew--call "list" "--version")))))


(defvar brew-package-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'homebrew-package-info)
    (define-key map (kbd "m") #'homebrew-package-mark-unmark)
    (define-key map (kbd "U") #'homebrew-update-package-list)
    (define-key map (kbd "d") #'homebrew-package-delete)
    (define-key map (kbd "u") #'homebrew-package-update)
    (define-key map (kbd "p") #'homebrew-package-pin)
    (define-key map (kbd "P") #'homebrew-package-unpin)
    (define-key map (kbd "a") #'homebrew-package-upgrade-all)
    map)
  "Local keymap for `brew-package-mode' buffers.")

(with-eval-after-load 'evil
  (evil-make-overriding-map brew-package-mode-map 'normal))

(define-derived-mode brew-package-mode tabulated-list-mode "homebrew-package-list"
  "A mode to list all your homebrew installed packages."
  (setq truncate-lines t)
  (setq tabulated-list-format `[("Name" ,hbm-name-column-width t)
                               ("Version" ,hbm-version-column-width nil)
                               ("Leaf" 4 nil)
                               ("Outdated" 8 nil)
                               ("Pinned" 0 nil)])

  (setq tabulated-list-entries #'homebrew--list-packages)

  (setq tabulated-list-padding 2)

  (tabulated-list-init-header)
  (tabulated-list-print))

;;;;;;;;;;;;;;;;;;
;; Predicates

(defun homebrew-package-pinned-p (item)
  "Check if ITEM is pinned or not."
  (s-present? (aref item 4)))

(defun homebrew-package-outdated-p (item)
  "Check if ITEM is outdated or not."
  (s-present? (aref item 3)))

(defun homebrew-package-leaf-p (item)
  "Check if ITEM is a leaf or not."
  (s-present? (aref item 2)))

;;;;;;;;;;;;;;;;;;
;; Interaction

(defun homebrew-package-mark-unmark ()
  "Clear any tags on the given item."
  (interactive)
  (tabulated-list-put-tag "" t))

(defun homebrew-package-pin ()
  "Mark item at point for pinning."
  (interactive)
  (unless (homebrew-package-pinned-p (tabulated-list-get-entry) )
    (tabulated-list-put-tag "p" t)))

(defun homebrew-package-unpin ()
  "Mark item at point for un-pinning."
  (interactive)
  (when (homebrew-package-pinned-p (tabulated-list-get-entry))
    (tabulated-list-put-tag "P" t)))

(defun homebrew-package-update ()
  "Mark item at point for updating."
  (interactive)
  (when (homebrew-package-outdated-p (tabulated-list-get-entry))
    (tabulated-list-put-tag "U" t)))

(defun homebrew-package-delete ()
  "Mark item at point for deletion."
  (interactive)
  (when (homebrew-package-leaf-p (tabulated-list-get-entry))
    (tabulated-list-put-tag "D" t)))

(defun homebrew-package-upgrade-all ()
  "Upgrade all outdated packages."
  (interactive)
  (when (yes-or-no-p "Upgrade all packages?")
    (message "Updated")))

;;;###autoload
(defun homebrew-package-info (&optional package)
  "Show the information for PACKAGE or the current package at point."
  (interactive)
  (let* ((pkg (or package (tabulated-list-get-id)))
         (buffer (generate-new-buffer (format "*brew info: %s*" pkg))))
    (with-current-buffer buffer
      (insert (shell-command-to-string (format "brew info %s" pkg)))
      (goto-char (point-min))
      (help-mode)
      (goto-address-mode))
    (display-buffer buffer)))

;;;###autoload
(defun homebrew-update-package-list ()
  "Run brew update to get the newest information."
  (interactive)
  (homebrew--call "update")
  (tabulated-list-print)
  (message "Homebrew package list updated"))

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
