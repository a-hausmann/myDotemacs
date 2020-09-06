;; File name:     aeh-useful.el
;; Last modified: Sun Sep 06, 2020 17:22:55
;; Author:        Arnold Hausmann
;; Why:           This is where I will keep useful code fragments/functions.


(defun aeh/narrow-dwim ()
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

(defun aeh/make-pretty (p-from p-thru)
  "Prettify Rule code by moving all and/or conjunctions to a new line"
  (interactive "r")
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((change-count 0))
          (ff)
          (goto-char p-from)
          (while (re-search-forward "\\( and \\| or \\)" p-thru t )
            (setq change-count (+ change-count 1))
            (replace-match "
\\1" nil nil))
          (message (format "Made %d changes." change-count)))))))

(defun aeh/prettify-rule-dwim ()
  "The dwim will allow for prettifying by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh/make-pretty (region-beginning) (region-end)))
    (t (aeh/make-pretty (point-min) (point-max)))))

(provide 'aeh-useful)
