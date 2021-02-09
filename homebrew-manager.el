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
;; Package-Requires: ((emacs "24.3") (s "1.11.0") (dash "2.12.0") (deferred "0.5.1") (xterm-color "2.0"))
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
(require 'deferred)
(require 'xterm-color)
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


(defcustom hbm-outdated-version-face "orange"
  "Face to use when rendering version of outdated packages."
  :type'face
  :group 'homebrew-manager)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internals
(cl-defstruct (homebrew-package (:constructor homebrew-package-create)
                                (:copier nil))
  "Store information about an installed homebrew package"
  name version leaf? pinned? latest type)

(defun homebrew--package-render (package)
  "Render the PACKAGE struct in a tabulated-list friendly format."
  (list package (vector (homebrew-package-name package)
                        (homebrew-package-version package)
                        (if (homebrew-package-latest package) (propertize (homebrew-package-latest package) 'font-lock-face `(:foreground ,hbm-outdated-version-face)) "")
                        (if (homebrew-package-leaf? package) "*" "")
                        (if (homebrew-package-pinned? package) "*" ""))))

(defun homebrew--parse-outdated (outdated)
  "Parse the list of OUTDATED packages into an alist of name version pairs."
  (-map (lambda (item)
          (let* ((split (s-split "<" item))
                 (latest (nth 1 split))
                 (name (car (s-split " " (nth 0 split)))))
            `(,name . ,latest))) outdated))

(defun homebrew--get-latest-version (name outdated)
  "Parse OUTDATED searching for NAME, returning the latest known version of NAME or nil if not found."
  (cdr (assoc-string name outdated)))

(defun brew--update-package-list ()
  "Update the list of packages async."
  (deferred:$
    (deferred:parallel
      (lambda () (deferred:process-shell "brew" "leaves"))
      (lambda () (deferred:process-shell "brew" "list" "--formula" "--versions"))
      (lambda () (deferred:process-shell "brew" "list" "--pinned"))
      (lambda () (deferred:process-shell "brew" "outdated" "--formula")))
    (deferred:nextc it
      (lambda (result)
        (let ((leaves (s-split "\n" (nth 0 result) t))
              (formula (s-split "\n" (nth 1 result) t))
              (pinned (s-split "\n" (nth 2 result) t))
              (outdated (homebrew--parse-outdated (s-split "\n" (nth 3 result)))))
          (cl-loop for f in formula
                   for name = (nth 0 (s-split " " f))
                   for version = (nth 1 (s-split " " f))
                   collect (homebrew-package-create :name name
                                                    :type 'formula
                                                    :version version
                                                    :latest (homebrew--get-latest-version name outdated)
                                                    :pinned? (-contains? pinned name)
                                                    :leaf? (-contains? leaves name))))))
    (deferred:nextc it
      (lambda (packages)
        (let ((buffer (get-buffer-create "*Brew installed packages*")))
          (with-current-buffer buffer
            (switch-to-buffer buffer)
            (brew-package-mode)
            (setq tabulated-list-entries (-map #'homebrew--package-render packages))
            (tabulated-list-print))))))
  t)

(defun homebrew--apply-changes (update delete pin unpin)
  "Apply the changes to all tagged packages."
  (deferred:$
    (deferred:next
      (lambda () (when pin (deferred:process-shell "brew" "pin" (s-join " " (-map #'homebrew-package-name pin))))))
    (deferred:nextc it
      (lambda (_) (when unpin (deferred:process-shell "brew" "unpin" (s-join " " (-map #'homebrew-package-name unpin))))))
    (deferred:nextc it
      (lambda (_) (when update (deferred:process-shell "brew" "upgrade" (s-join " " (-map #'homebrew-package-name update))))))
    (deferred:nextc it
      (lambda (_) (when delete (deferred:process-shell "brew" "delete" (s-join " " (-map #'homebrew-package-name delete))))))
    (deferred:nextc it
      (lambda (_) (brew--update-package-list))))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode

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
    (define-key map (kbd "x") #'homebrew-package-execute)
    map)
  "Local keymap for `brew-package-mode' buffers.")

(with-eval-after-load 'evil
  (evil-make-overriding-map brew-package-mode-map 'normal))

(define-derived-mode brew-package-mode tabulated-list-mode "homebrew-package-list"
  "A mode to list all your homebrew installed packages."
  (setq truncate-lines t)
  (setq tabulated-list-format `[("Name" ,hbm-name-column-width t)
                               ("Version" ,hbm-version-column-width nil)
                               ("Latest" ,hbm-version-column-width nil)
                               ("Leaf" 4 nil)
                               ("Pinned" 6 nil)])

  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;;;;;;;;;;;;;;;;
;; Interaction

(defun homebrew-package-mark-unmark ()
  "Clear any tags on the given item."
  (interactive)
  (tabulated-list-put-tag "" t))

(defun homebrew-package-pin ()
  "Mark item at point for pinning."
  (interactive)
  (unless (homebrew-package-pinned? (tabulated-list-get-id) )
    (tabulated-list-put-tag "p" t)))

(defun homebrew-package-unpin ()
  "Mark item at point for un-pinning."
  (interactive)
  (when (homebrew-package-pinned? (tabulated-list-get-id))
    (tabulated-list-put-tag "P" t)))

(defun homebrew-package-update ()
  "Mark item at point for updating."
  (interactive)
  (when (homebrew-package-latest (tabulated-list-get-id))
    (tabulated-list-put-tag "U" t)))

(defun homebrew-package-delete ()
  "Mark item at point for deletion."
  (interactive)
  (when (homebrew-package-leaf? (tabulated-list-get-id))
    (tabulated-list-put-tag "D" t)))

(defun homebrew-package-upgrade-all ()
  "Upgrade all outdated packages."
  (interactive)
  (when (yes-or-no-p "Upgrade all packages?")
    (message "Updated")))

(defun homebrew-package-execute ()
  "Apply the selected actions to all tagged packages."
  (interactive)
  (let (update-list delete-list pinned-list unpinned-list tag id)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp)) ;; scan the buffer for tagged items
        (setq tag (char-after))
        (setq id (tabulated-list-get-id))
        (cond ((eq tag ?D) (push id delete-list))
              ((eq tag ?U) (push id update-list))
              ((eq tag ?p) (push id pinned-list))
              ((eq tag ?P) (push id unpinned-list)))
        (forward-line))
      ;; Sanity check
      (unless (or update-list delete-list pinned-list unpinned-list)
        (user-error "No package marked for action"))
      (homebrew--apply-changes update-list delete-list pinned-list unpinned-list))))

;;;###autoload
(defun homebrew-package-info (&optional package)
  "Show the information for PACKAGE or the current package at point."
  (interactive)
  (let ((pkg (or package (homebrew-package-name (tabulated-list-get-id)))))
    (deferred:$
      (deferred:process-shell "brew" "info" "--formula" pkg)
      (deferred:nextc it
        (lambda (info)
          (let ((buffer (generate-new-buffer (format "*brew info: %s*" pkg))))
            (with-current-buffer buffer
              (insert (xterm-color-filter info))
              (goto-char (point-min))
              (help-mode)
              (goto-address-mode))
            (display-buffer buffer))))
      (deferred:error it
        (lambda (_)
          (message "No such package found!: %s" package)))))
  package)

;;;###autoload
(defun homebrew-update-package-list ()
  "Run brew update to get the newest information."
  (interactive)
  (deferred:$
    (deferred:process-shell "brew" "update")
    (deferred:nextc it
      (lambda (_)
        (brew--update-package-list)))))

;;;###autoload
(defun homebrew-list-packages ()
  "Lists all homebrew installed packages in a tabulated list."
  (interactive)
  (message "Fetching list of installed packages")
  (display-buffer (generate-new-buffer "*Brew installed packages*"))
  (brew--update-package-list))


(provide 'homebrew-manager)
;;; homebrew-manager.el ends here
