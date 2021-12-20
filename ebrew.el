;;; ebrew.el --- Manage your homebrew packages -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Björn Larsson
;;
;; Author: Björn Larsson <http://github/fuzzycode>
;; Maintainer: Björn Larsson <develop@bjornlarsson.net>
;; Created: January 30, 2021
;; Modified: January 30, 2021
;; Version: 0.0.1
;; Keywords: utility homebrew system
;; Homepage: https://github.com/fuzzycode/homebrew-manager
;; Package-Requires: ((emacs "24.3") (s "1.11.0") (dash "2.12.0") (deferred "0.5.1") (xterm-color "2.0"))
;;
;; This file is not part of GNU Emacs.
;;
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

(defgroup ebrew nil
  "A package manager for packages installed with homebrew"
  :prefix "ebrew-"
  :group 'applications)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization Variables

(defcustom ebrew-name-column-width 30
  "Width of the package name column."
  :type 'integer
  :group 'ebrew)

(defcustom ebrew-version-column-width 10
  "Width of the package version column."
  :type 'integer
  :group 'homebrew-manager)


(defcustom ebrew-outdated-version-face "orange"
  "Face to use when rendering version of outdated packages."
  :type'face
  :group 'ebrew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internals
(cl-defstruct (ebrew-package (:constructor ebrew-package-create)
                                (:copier nil))
  "Store information about an installed homebrew package"
  name version leaf? pinned? latest type)

(defun ebrew--package-render (package)
  "Render the PACKAGE struct in a tabulated-list friendly format."
  (list package (vector (ebrew-package-name package)
                        (ebrew-package-version package)
                        (if (ebrew-package-latest package) (propertize (ebrew-package-latest package) 'font-lock-face `(:foreground ,ebrew-outdated-version-face)) "")
                        (if (ebrew-package-leaf? package) "*" "")
                        (if (ebrew-package-pinned? package) "*" ""))))

(defun ebrew--parse-outdated (outdated)
  "Parse the list of OUTDATED packages into an alist of name version pairs."
  (-map (lambda (item)
          (let* ((split (s-split "<" item))
                 (latest (nth 1 split))
                 (name (car (s-split " " (nth 0 split)))))
            `(,name . ,latest))) outdated))

(defun ebrew--get-latest-version (name outdated)
  "Parse OUTDATED searching for NAME, returning the latest known version of NAME or nil if not found."
  (cdr (assoc-string name outdated)))

(defun ebrew--update-package-list ()
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
              (outdated (ebrew--parse-outdated (s-split "\n" (nth 3 result)))))
          (cl-loop for f in formula
                   for name = (nth 0 (s-split " " f))
                   for version = (nth 1 (s-split " " f))
                   collect (ebrew-package-create :name name
                                                    :type 'formula
                                                    :version version
                                                    :latest (ebrew--get-latest-version name outdated)
                                                    :pinned? (-contains? pinned name)
                                                    :leaf? (-contains? leaves name))))))
    (deferred:nextc it
      (lambda (packages)
        (let ((buffer (get-buffer-create "*Brew installed packages*")))
          (with-current-buffer buffer
            (switch-to-buffer buffer)
            (ebrew-package-mode)
            (setq tabulated-list-entries (-map #'ebrew--package-render packages))
            (tabulated-list-print))))))
  t)

(defun ebrew--apply-changes (update delete pin unpin)
  "Apply the changes to all tagged packages."
  (deferred:$
    (deferred:next
      (lambda () (when pin (deferred:process-shell "brew" "pin" (s-join " " (-map #'ebrew-package-name pin))))))
    (deferred:nextc it
      (lambda (_) (when unpin (deferred:process-shell "brew" "unpin" (s-join " " (-map #'ebrew-package-name unpin))))))
    (deferred:nextc it
      (lambda (_) (when update (deferred:process-shell "brew" "upgrade" (s-join " " (-map #'ebrew-package-name update))))))
    (deferred:nextc it
      (lambda (_) (when delete (deferred:process-shell "brew" "delete" (s-join " " (-map #'ebrew-package-name delete))))))
    (deferred:nextc it
      (lambda (_) (ebrew--update-package-list))))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode

(defvar ebrew-package-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'ebrew-package-info)
    (define-key map (kbd "m") #'ebrew-package-mark-unmark)
    (define-key map (kbd "U") #'ebrew-update-package-list)
    (define-key map (kbd "d") #'ebrew-package-delete)
    (define-key map (kbd "u") #'ebrew-package-update)
    (define-key map (kbd "p") #'ebrew-package-pin)
    (define-key map (kbd "P") #'ebrew-package-unpin)
    (define-key map (kbd "a") #'ebrew-package-upgrade-all)
    (define-key map (kbd "x") #'ebrew-package-execute)
    map)
  "Local keymap for `ebrew-package-mode' buffers.")

(with-eval-after-load 'evil
  (evil-make-overriding-map ebrew-package-mode-map 'normal))

(define-derived-mode ebrew-package-mode tabulated-list-mode "ebrew-package-list"
  "A mode to list all your homebrew installed packages."
  (setq truncate-lines t)
  (setq tabulated-list-format `[("Name" ,ebrew-name-column-width t)
                               ("Version" ,ebrew-version-column-width nil)
                               ("Latest" ,ebrew-version-column-width nil)
                               ("Leaf" 4 nil)
                               ("Pinned" 6 nil)])

  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;;;;;;;;;;;;;;;;
;; Interaction

(defun ebrew-package-mark-unmark ()
  "Clear any tags on the given item."
  (interactive)
  (tabulated-list-put-tag "" t))

(defun ebrew-package-pin ()
  "Mark item at point for pinning."
  (interactive)
  (unless (ebrew-package-pinned? (tabulated-list-get-id) )
    (tabulated-list-put-tag "p" t)))

(defun ebrew-package-unpin ()
  "Mark item at point for un-pinning."
  (interactive)
  (when (ebrew-package-pinned? (tabulated-list-get-id))
    (tabulated-list-put-tag "P" t)))

(defun ebrew-package-update ()
  "Mark item at point for updating."
  (interactive)
  (when (ebrew-package-latest (tabulated-list-get-id))
    (tabulated-list-put-tag "U" t)))

(defun ebrew-package-delete ()
  "Mark item at point for deletion."
  (interactive)
  (when (ebrew-package-leaf? (tabulated-list-get-id))
    (tabulated-list-put-tag "D" t)))

(defun ebrew-package-upgrade-all ()
  "Upgrade all outdated packages."
  (interactive)
  (when (yes-or-no-p "Upgrade all packages?")
    (message "Updated")))

(defun ebrew-package-execute ()
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
      (ebrew--apply-changes update-list delete-list pinned-list unpinned-list))))

;;;###autoload
(defun ebrew-package-info (&optional package)
  "Show the information for PACKAGE or the current package at point."
  (interactive)
  (let ((pkg (or package (ebrew-package-name (tabulated-list-get-id)))))
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
(defun ebrew-update-package-list ()
  "Run brew update to get the newest information."
  (interactive)
  (deferred:$
    (deferred:process-shell "brew" "update")
    (deferred:nextc it
      (lambda (_)
        (ebrew--update-package-list)))))

;;;###autoload
(defun ebrew-list-packages ()
  "Lists all homebrew installed packages in a tabulated list."
  (interactive)
  (let ((progress (make-progress-reporter  "Fetching list of installed packages")))
    (display-buffer (generate-new-buffer "*Brew installed packages*"))
    (ebrew--update-package-list)
    (progress-reporter-done progress)))


(provide 'ebrew)
;;; ebrew.el ends here
