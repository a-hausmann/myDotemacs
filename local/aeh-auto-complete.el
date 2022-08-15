;; File:          aeh-auto-complete.el  --- -*- lexical-binding: t -*-
;; Created:       2022-08-11 14:24:24
;; Last modified: Fri Aug 12, 2022 15:36:03
;; Purpose:       A separate loader for Auto-Completions, starting with Company.
;;

;; --------------------------------------------------------------------------------
;; I think that Company is acting a bit better now, and on 10/30/2018, I added
;; some code from Oleh Krehel https://oremacs.com/2017/12/27/company-numbers/
;; to show numbers on the popup, and be able to use them to select text.
;; This works like a charm. Oleh's blog article is from December 2017, and he
;; states his git log shows he's been using this setup for three years without
;; any issues.  Grand!

;; 2019-09-05: After setting Counsel to defer, needed to wrap this entire code
;; with an "with-eval-after-load" function.
;; --------------------------------------------------------------------------------
;; Basic setting
(with-eval-after-load 'company
  (setq company-show-numbers t)
  ;; Oleh's function:
  (defun ora-company-number ()
    "Forward to `company-complete-number'.

  Unless the number is potentially part of the candidate.
  In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
          (self-insert-command 1)
          (company-complete-number (string-to-number k)))))

  ;; Add some bindings
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1)))
    ;; This line UNBINDS RET key from closing the popup
    (define-key map (kbd "<return>") nil)))

;; --------------------------------------------------------------------------------
;; Start Company configuration.
;; --------------------------------------------------------------------------------
(use-package company
    :ensure t
    :commands company-complete-common
    :diminish
    :hook ((emacs-lisp-mode . company-mode)
           (shell-mode . company-mode))
    :config
    (setq company-idle-delay .5)  ; half-second delay
    (global-set-key (kbd "C-M-.") 'company-complete-common)
    (setq company-minimum-prefix-length 3)   ; three letters needed for completion
    (setq company-dabbrev-ignore-case t)
    (setq company-dabbrev-downcase nil)      ; return candidates AS IS.
    (with-eval-after-load 'company
      (define-key company-active-map (kbd "M-n") nil)
      (define-key company-active-map (kbd "M-p") nil)
      (define-key company-active-map (kbd "C-n") #'company-select-next)
      (define-key company-active-map (kbd "C-p") #'company-select-previous))
    )

(add-hook 'sql-mode-hook
          #'(lambda ()
              (setq-default company-minimum-prefix-length 4)
              (setq-default company-dabbrev-code-ignore-case t)
              (setq-default completion-ignore-case t)))

(defun shell-mode-company-init ()
  (setq-local company-backends '((company-shell
                                  company-shell-env
                                  company-etags
                                  company-dabbrev-code))))

(use-package company-shell
    :ensure t
    :after company
    :diminish
    :config (add-hook 'shell-mode-hook 'shell-mode-company-init))

;; 2020-07-08, adding support for Elpy
;; Ref: ref: https://medium.com/analytics-vidhya/managing-a-python-development-environment-in-emacs-43897fd48c6a
(use-package company-statistics
    :ensure t
    :after company
    :config (company-statistics-mode))

(use-package company-web
    :ensure t
    :after company)

;; Not sure what I was thinking here and no notes on it.
;; (use-package company-try-hard
;;     :ensure t
;;     :after company
;;     :bind
;;     ;; Change from C-<tab> to "C-." and "C-M-."
;;     (("C-." . company-try-hard)
;;      ("C-M-." . company-try-hard)
;;      :map company-active-map
;;      ("C-." . company-try-hard)
;;      ("C-M-." . company-try-hard)))

(use-package company-quickhelp
    :ensure t
    :after company
    :config
    (company-quickhelp-mode))

;; 2020-07-08, adding company-jedi, which uses jedi-core, but made for Company users
(defun aeh/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(use-package company-jedi
    :ensure t
    :disabled
    :after company
    :hook (python-mode-map . aeh/python-mode-hook))

(setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
         company-capf)          ; completion-at-point-functions
        (company-abbrev company-dabbrev)))


;; End of aeh-auto-complete.el
