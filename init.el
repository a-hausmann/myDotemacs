;;; init.el  --Emacs configuration file
;;
;; Author:    Arnold Hausmann
;; Created:   2018-08-05
;; Email:     arnoldh@comcast.net, aehjr1@gmail.com
;;
;; This program is free software.  You can redistribute it and/or modify it under
;; the terms of the "Do What The Fuck You Want To Public License, version 2, as
;; published by Sam Hocevar. (http://www.wtfpl.net)
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.
;;
;; Following lines load an Org file and build the configuration code out of it.

;; Cannot remember where I first saw this, but after some more research, I think
;; this is NOT a good or complete answer. See references below for reasons.
;; So, it would seem that a better solution is one found in Uncle Dave's init.el
;; file, where he sets it very high, then defines function and hook to reset to
;; lower levels after emacs startup (see hook "emacs-startup-hook", with reference
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Init-File.html#Init-File)
;; 
;; Ref: https://github.com/syl20bnr/spacemacs/issues/3011
;; Ref: http://lists.gnu.org/archive/html/help-gnu-emacs/2007-09/msg00241.html
;; Replacing next line, which sets gc-cons-threshold to highest ONLY for startup,
;; and therefore creates a LET condition on the entire init.el contents.
;; Use Uncle Dave's version instead: https://github.com/daedreth/UncleDavesEmacs/blob/master/init.el
;;
; (let ((gc-cons-threshold most-positive-fixnum))

;;; This fixed garbage collection, makes emacs start up faster ;;;;;;;
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
    gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set repositories
(require 'package)
(setq-default
 load-prefer-newer t
 package-enable-at-startup nil)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; 2018-12-27: Added the line below, had to look up correct URL, ref: https://emacsredux.com/blog/2014/05/16/melpa-stable/
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Again, taking a hint from Uncle Dave, just in case "use-package" in not installed...
;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
    (package-install 'use-package))

;; Install dependencies
(unless (and (package-installed-p 'delight)
             (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'delight t)
  (package-install 'use-package t))
(setq-default
 use-package-always-defer t
 use-package-always-ensure t)

;; 2018-12-16: migrate code from dotemacs.org file to here as it deals with version controlled files.
;; Follow symlinks for version controlled files
(setq vc-follow-symlinks t)

;; Use latest Org
(use-package org :ensure org-plus-contrib)

;; Set Custom file location and if it exists, load it...then start tangling.
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
;; Tangle configuration
;; Now using UD trick of only tangling file if it exists, so if not exists, Emacs will still launch.
(when (file-readable-p "~/.emacs.d/dotemacs.org")
  (org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory)))
(garbage-collect)

;;; init.el ends here

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
