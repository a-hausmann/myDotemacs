;; File:          aeh-theme.el  --- -*- lexical-binding: t -*-
;; Created:       2022-06-14
;; Last modified: Thu Jul 06, 2023 17:28:28
;; Purpose:       This file configures the default theme I want to use. Currently
;;                this is the Modus themes. https://protesilaos.com/emacs/modus-themes
;;

;; Ref: https://github.com/protesilaos/modus-themes/

;; The Modus themes are INCLUDED in Emacs 28.1 and above.
;; For versions below, will NEED to ensure latest Modus versions installed.
;; Will NOT CHECK for Emacs version.

;; For the themes that are built into Emacs you cannot require the package.
;; Use the following instead.
;; 2022-08-15: after much experimenting, leave modus-themes-region as nil
;; see documentation: C-h v modus-themes-region <ENT>

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-region nil
      modus-themes-syntax '(alt-syntax)
      modus-themes-fringes 'subtle
      modus-themes-mode-line '(accented borderless padded)
      modus-themes-tabs-accented t
      modus-themes-paren-match '(bold intense)
      modus-themes-prompts '(bold intense)
      ;; modus-themes-completions 'opinionated
      modus-themes-completions '((matches . (extrabold))
                                 (selection . (semibold accented))
                                 (popup . (accented intense)))
      modus-themes-org-blocks 'tinted-background)
(setq modus-themes-headings
      '((1 . (rainbow overline background 1.3))
        (2 . (rainbow background 1.2))
        (3 . (rainbow bold 1.1))
        (t . (semilight 1.1))))
;; Important!
(setq modus-themes-scale-headings t)
(setq modus-themes-org-blocks 'gray-background)
;; Load the theme of your choice:
(load-theme 'modus-vivendi t nil) ;; OR (load-theme 'modus-vivendi)
(define-key global-map (kbd "<f6>") #'modus-themes-select)
(define-key global-map (kbd "<f7>") #'modus-themes-toggle)

