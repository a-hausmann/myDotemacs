;; aeh-useful.el  --- -*- lexical-binding: t -*-
;; File name:     aeh-useful.el
;; Last modified: Mon May 29, 2023 15:03:15
;; Author:        Arnold Hausmann
;; Why:           This is where I will keep useful code fragments/functions.

(defun aeh-new-untitled-buffer ()
  "Create new buffer named \"untitled\""
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled"))
  (text-mode))
(global-set-key (kbd "C-c n") 'aeh-new-untitled-buffer)

(defun aeh-ff ()
  "Display positions at begin and end of a region."
  (interactive)
  (message "begin at %s; end at %s" (region-beginning) (region-end)))

(defalias 'ff 'aeh-ff)

(defun aeh-narrow-dwim ()
  "Toggle narrowing."
  (interactive)
  (cond ((region-active-p)
          ;; If region is highlighted, narrow to that
          (call-interactively #'narrow-to-region)
          (deactivate-mark t))
    ((buffer-narrowed-p)
      ;; Otherwise widen if narrowed
      (widen))
    ((derived-mode-p 'org-mode)
      (call-interactively #'org-narrow-to-subtree))
    (t
      (message "Do not know what to narrow to.")
      (call-interactively #'narrow-to-defun))))
(global-set-key (kbd "C-x n w") 'aeh-narrow-dwim)

(defun aeh-make-pretty (p-from p-thru)
  "Prettify Rule code by moving all and/or conjunctions to a new line"
  (interactive "r")
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((change-count 0))
          (goto-char p-from)
          (while (re-search-forward "\\( and \\| or \\)" p-thru t )
            (setq change-count (+ change-count 1))
            (replace-match "
\\1" nil nil))
          (message (format "Made %d changes." change-count)))))))

(defun aeh-prettify-rule-dwim ()
  "The dwim will allow for prettifying by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh-make-pretty (region-beginning) (region-end)))
    (t (aeh-make-pretty (point-min) (point-max)))))

(defun aeh-delete-carriage-return-dwim ()
  "The dwim will delete carriage return by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh-strip-ctl-m (region-beginning) (region-end)))
    (t (aeh-strip-ctl-m (point-min) (point-max)))))

(defun aeh-strip-ctl-m (p-from p-thru)
  "Replace carriage returns (^M) with nil"
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((remove-count 0))
          (goto-char p-from)
          (while (re-search-forward (concat (char-to-string 13) "$") p-thru t)
            (setq remove-count (+ remove-count 1))
            (replace-match "" nil nil))
          (message (format "%d ^M removed from buffer." remove-count)))))))

;; 05/29/2023: Added functions to assist converting spaces to underlines.
(defun aeh-space-to-underline-dwim ()
  "The dwim will replace space with underline in either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh-replace-space-with-underline (region-beginning) (region-end))
          )
    (t
      (aeh-replace-space-with-underline (point-min) (point-max)))))

(general-define-key
 :keymaps 'text-mode-map
 "C-c _" 'aeh-space-to-underline-dwim)

(defun aeh-replace-space-with-underline (p-from p-thru)
  "Replace spaces with underlines"
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((remove-count 0))
          (goto-char p-from)
          (while (search-forward " " p-thru t)
            (replace-match "_" nil nil)))))))

(provide 'aeh-useful)
