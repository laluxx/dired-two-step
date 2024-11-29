;; dired-two-step.el --- Two-step copy/paste operations in dired -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Laluxx
;; Author: Laluxx
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: dired
;; URL: https://github.com/laluxx/dired-two-step

;;; Commentary:
;; TODO Add two step cut paste
;; This package provides a two-step copy-paste functionality for Dired,
;; allowing users to copy files and directories incrementally and
;; paste them in another Dired buffer.  The copy buffer is cleared
;; after pasting, making it a safer alternative to traditional copy-paste
;; operations.

;; Enhanced features:
;; - Press 'y' to:
;;   * Copy marked files/directories (if marks exist)
;;   * Append current file to copy list (if no marks)
;; - Press 'C-y' to paste all collected files/directories
;; Usage:
;;
;; 1. Either:
;;    - Mark multiple files and press 'y' to copy them all, or
;;    - Press 'y' on individual files to add them to copy list
;; 2. Navigate to destination directory
;; 3. Press 'C-y' to paste all collected items

;;; Code:

(require 'dired)
(require 'pulse)

(defgroup dired-two-step nil
  "Two-step copy/paste operations in dired."
  :group 'dired
  :prefix "dired-two-step-")

(defvar dired-two-step-copied-files nil
  "List of files and directories collected for copying in dired.")

(defface dired-two-step-pulse-face
  '((((class color) (background light))
     :background "#90EE90"
     :extend t)
    (((class color) (background dark))
     :background "#2F4F4F"
     :extend t))
  "Face used for pulsing effect when copying files."
  :group 'dired-two-step)

;; Custom options that control pulsing behavior
(defcustom dired-two-step-pulse-flag t
  "Non-nil means pulse the line when copying files."
  :type 'boolean
  :group 'dired-two-step)

(defcustom dired-two-step-pulse-iterations 1
  "Number of times to pulse the line."
  :type 'integer
  :group 'dired-two-step)

(defcustom dired-two-step-pulse-delay 0.1
  "Delay between pulses in seconds."
  :type 'number
  :group 'dired-two-step)

(defun dired-two-step-pulse-line ()
  "Pulse the current line in dired."
  (when (bound-and-true-p dired-two-step-pulse-flag)
    (let ((pulse-iterations dired-two-step-pulse-iterations)
          (pulse-delay dired-two-step-pulse-delay))
      (save-excursion
        (let ((ov (make-overlay
                   (line-beginning-position)
                   (1+ (line-end-position)))))
          (overlay-put ov 'face 'dired-two-step-pulse-face)
          (sit-for 0.1)
          (delete-overlay ov))))))

(defun dired-two-step-pulse-files (files)
  "Pulse each of the FILES in the dired buffer."
  (when (bound-and-true-p dired-two-step-pulse-flag)
    (save-excursion
      (dolist (file files)
        (when (dired-goto-file file)
          (dired-two-step-pulse-line)
          (sit-for dired-two-step-pulse-delay))))))

;;;###autoload
(defun dired-two-step-smart-copy ()
  "Smart copy function that either:
- Copies marked files/directories if marks exist
- Appends current file to copy list if no marks exist"
  (interactive)
  (let ((marked (dired-get-marked-files nil nil nil nil))) ; Don't include current file if no marks
    (if marked
        ;; If files are marked, copy them all
        (progn
          (setq dired-two-step-copied-files marked)
          (dired-two-step-pulse-files marked)
          (message "Copied %d items to memory. Use C-y to paste."
                   (length marked))
          (dired-unmark-all-marks))
      ;; If no marks, append current file to list (avoiding duplicates)
      (let* ((current-file (dired-get-file-for-visit))
             (display-name (file-relative-name current-file default-directory)))
        (if (member current-file dired-two-step-copied-files)
            (message "File '%s' already in copy list" display-name)
          (push current-file dired-two-step-copied-files)
          (dired-two-step-pulse-line)
          (message "Added '%s' to copy list (%d items total)" 
                   display-name
                   (length dired-two-step-copied-files)))))))

;;;###autoload
(defun dired-two-step-paste ()
  "Paste all collected files to current dired directory.
Clears the copy buffer after pasting.
When pasting a single file, moves cursor to the pasted file."
  (interactive)
  (if (not dired-two-step-copied-files)
      (message "No items in memory! Use 'y' to add files.")
    (let* ((target-dir (dired-current-directory))
           (copied-count 0)
           (last-basename nil)
           (single-file-p (= 1 (length dired-two-step-copied-files))))
      ;; Store the basename of the file we want to jump to
      (when single-file-p
        (setq last-basename (file-name-nondirectory (car dired-two-step-copied-files))))
      ;; Perform the copy operations
      (dolist (file dired-two-step-copied-files)
        (let ((basename (file-name-nondirectory file)))
          (setq last-basename basename) ; Keep track of last copied file
          (if (file-directory-p file)
              ;; For directories, use copy-directory
              (copy-directory file
                              (expand-file-name basename target-dir)
                              t t t) ; copy recursively, keep time, copy contents
            ;; For regular files, use copy-file
            (copy-file file
                       (expand-file-name basename target-dir)
                       nil)))
        (setq copied-count (1+ copied-count)))
      ;; Clear the memory after pasting
      (setq dired-two-step-copied-files nil)
      ;; Refresh the dired buffer and jump to the file
      (revert-buffer)
      ;; After revert, try to move to the relevant file
      (when last-basename
        (let ((attempt-count 0)
              (max-attempts 30))
          (while (and (< attempt-count max-attempts)
                      (not (dired-goto-file 
                            (expand-file-name last-basename target-dir))))
            (setq attempt-count (1+ attempt-count))
            (sit-for 0.1))))
      ;; Show appropriate message
      (if single-file-p
          (message "Copied file to %s and jumped to it" target-dir)
        (message "Copied %d items to %s" copied-count target-dir)))))

;;;###autoload
(defun dired-two-step-clear ()
  "Clear the current copy list."
  (interactive)
  (setq dired-two-step-copied-files nil)
  (message "Copy list cleared"))

;;;###autoload
(defun dired-two-step-show-copied ()
  "Show the list of files currently collected for copying."
  (interactive)
  (if dired-two-step-copied-files
      (with-help-window "*Dired Copy List*"
        (princ "Files to be copied:\n\n")
        (dolist (file dired-two-step-copied-files)
          (princ (format "• %s\n" (file-relative-name file default-directory)))))
    (message "No files in copy list")))

;;;###autoload
(define-minor-mode dired-two-step-mode
  "Minor mode for two-step copy/paste operations in dired."
  :lighter " D2S"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "y") #'dired-two-step-smart-copy)
            (define-key map (kbd "C-y") #'dired-two-step-paste)
            (define-key map (kbd "C-c x") #'dired-two-step-clear)
            (define-key map (kbd "C-c l") #'dired-two-step-show-copied)
            map))

;;;###autoload
(add-hook 'dired-mode-hook #'dired-two-step-mode)

(provide 'dired-two-step)
;;; dired-two-step.el ends here
