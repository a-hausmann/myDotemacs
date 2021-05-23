;; aeh-ivy-counsel.el    --- -*- lexical-binding: t -*-
(use-package ivy
    :demand
    :diminish ivy-mode
    :config
    (ivy-mode t)
    (setq ivy-wrap t)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-height-alist '((t lambda (_caller) (/ (window-height) 4))))
    (setq ivy-initial-inputs-alist nil)
    :bind (("C-c v" . ivy-push-view)
           ("C-c V" . ivy-switch-view)
           ("C-x b" . ivy-switch-buffer)
           ("<f6>"  . ivy-resume)))

(use-package counsel
    :demand
    :bind (("M-x" . counsel-M-x)
           ("C-c g" . 'counsel-git)
           ("C-c j" . 'counsel-git-grep)
           ("C-c k" . 'counsel-ag)
           ;; ("C-x b" . 'counsel-ibuffer)
           ("C-x l" . 'counsel-locate)
           ("C-x C-f" . 'counsel-find-file)
           ("<f1> f" . 'counsel-describe-function)
           ("<f1> v" . 'counsel-describe-variable)
           ("<f1> l" . 'counsel-find-library)
           ("<f2> i" . 'counsel-info-lookup-symbol)
           ("<f2> u" . 'counsel-unicode-char)
           (:map minibuffer-local-map ("C-r" . counsel-minibuffer-history))))

;; Swiper is an ivy enhanced version of isearch.
(use-package swiper
    :demand
    :bind ("C-S-s" . swiper))

;; Hydra presents menus for ivy commands.
;; 2019-10-02: Change Swiper binding to "C-S-s"
(use-package ivy-hydra
    :after ivy
    :defer 1)

;; These are some key bindings for Ivy/Counsel/Swiper ref: https://github.com/abo-abo/swiper#small-config-example
;; 2019-08-27: Some changes here, ref.: https://oremacs.com/swiper/
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; 04/25/2020: Add ivy-rich support.
;; Ref: https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
    :ensure
    :after ivy
    :init
    (ivy-rich-mode 1)
    :config
    (setq ivy-rich-path-style 'abbreviate)
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; 2020-06-12: add all-the-icons-ivy-rich
;; Ref: https://github.com/seagle0128/all-the-icons-ivy-rich
(use-package all-the-icons-ivy-rich
    :ensure t
    :after ivy
    :init (all-the-icons-ivy-rich-mode 1))

;; 2021-02-22: update config from "daviwil"
;; 2021-05-22: changing completion framework
(use-package lsp-ivy
  :diminish
  :after lsp)

;; Miscellaneous settings.
(setq projectile-completion-system 'ivy)
