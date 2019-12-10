;; File name:     aeh-useful.el
;; Last modified: Tue Dec 10, 2019 8:39:29
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



(provide 'aeh-useful)
