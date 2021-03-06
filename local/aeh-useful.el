;; aeh-useful.el  --- -*- lexical-binding: t -*-
;; File name:     aeh-useful.el
;; Last modified: Tue Sep 08, 2020 23:26:19
;; Author:        Arnold Hausmann
;; Why:           This is where I will keep useful code fragments/functions.

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
(global-set-key (kbd "C-c C-n") 'aeh/narrow-dwim)

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

(provide 'aeh-useful)
