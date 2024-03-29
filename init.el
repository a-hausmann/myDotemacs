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
;; See Bailey Lings comments: http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

;;; This fixed garbage collection, makes emacs start up faster ;;;;;;;
;; 2019-09-05: Lots of experimenting trying to get faster in Windows. Ref: https://blog.d46.us/advanced-emacs-startup/
;; First is code at bottom, sending a startup elapsed time with garbage collection count to *Messages* buffer.
;; Just setting at beginning and end did little, still around 20 seconds with 5-6 garbage collections.
;; The most-positive-fixnum about the same.

;; (setq gc-cons-threshold most-positive-fixnum
;;       gc-cons-percentage 0.6
;;       )

;; (defvar startup/file-name-handler-alist file-name-handler-alist)
;; (setq file-name-handler-alist nil)

;; (defun startup/revert-file-name-handler-alist ()
;;   (setq file-name-handler-alist startup/file-name-handler-alist))

;; (defun startup/reset-gc ()
;;   (setq gc-cons-threshold 16777216
;;         gc-cons-percentage 0.1))

;; (add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
;; (add-hook 'emacs-startup-hook 'startup/reset-gc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2019-07-06: Added to make startup better.
;; Ref: https://www.reddit.com/r/emacs/comments/c9ef5i/tried_emacs_on_windows_giving_up/
;; Looked into setting "w32-pipe-buffer-size" to various: 0, 4096, 8192, but didn't seem to make any difference.
;; The use-package-report shows that the biggest culprit is Evil and Evil-Magit, the latter is worst offender.
;; As I recall, startup used to be around 30 seconds, and setting the w32-pipe-read-delay to 0 easily cuts in half.
;; The buffer size didn't seem to make a difference; in fact, after experimenting, commenting all, startup remained
;; shorter, so it appears (hard to tell for certain) that the big culprit is actually Windows--continuing to do
;; stuff and take up CPU long after startup appears complete. Have found the same with SQL Developer too, so not
;; that big a surprise to find Windows affecting Emacs startup.
;; 2020-01-29: Link: https://www.reddit.com/r/emacs/comments/eumt4g/emacs_win10_slow/
;; Variable w32-get-true-file-attributes with non-nil issues extra system calls to determine accurate link counts.
;; Said to be more useful for NTFS over FAT, but u/zsome sets this to nil, and gets better results (along with
;; changing the buffer size.
;; (when (boundp 'w32-pipe-read-delay)
;;   (setq w32-pipe-read-delay 0)
;;   (setq w32-pipe-buffer-size (* 64 1024))
;;   (setq w32-get-true-file-attributes nil)
;;   )
;; ;; (when (boundp 'w32-pipe-buffer-size)
;; ;;   (setq w32-pipe-buffer-size (* 64 1024)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set message to mark Emacs startup timestamp.
(message "init.el started.")

;; Set repositories
(message "Before require of package")
(require 'package)
(message "After require of package")
;; (setq-default
;;  load-prefer-newer t
;;  package-enable-at-startup nil)

;; 2021-02-22: ELPA is evidently dead cannot get GPG public signature anymore.
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
;; 2018-12-27: Added the line below, had to look up correct URL, ref: https://emacsredux.com/blog/2014/05/16/melpa-stable/
;; (add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(message "Before package-initialize")
(package-initialize)
(message "After package-initialize")

;; Again, taking a hint from Uncle Dave, just in case "use-package" in not installed...
;; Bootstrapping use-package
;; 2019-09-05: duplicated below, should be able to remove easily.
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;     (package-install 'use-package))

;; 2022-01-05: add package-quickstart
(setq package-quickstart t)

;; Install dependencies
(unless (and (package-installed-p 'delight)
             (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'delight t)
  (package-install 'use-package t))
(message "After install dependencies")

;; 2021-02-21: Missed actually requiring use-package, looking at System Crafters videos.
;; Ref: https://github.com/daviwil/dotfiles/blob/master/Emacs.org
(require 'use-package)
;; Uncomment this to get a reading on packages that get loaded at startup
(setq use-package-verbose t)

(setq-default
 use-package-always-defer t
 use-package-always-ensure t
 ;; 2019-06-29: add statistics
 use-package-compute-statistics t)

;; 2019-09-05: installed this to track startup. Ref: https://github.com/dholm/benchmark-init-el
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate)
  )
(benchmark-init/activate)
(message "After benchmark-init/activate")

;; 2018-12-16: migrate code from dotemacs.org file to here as it deals with version controlled files.
;; Follow symlinks for version controlled files
(setq vc-follow-symlinks t)

;; 2020-08-26: Add following to prevent "cl is deprecated" messages.
;; Ref: https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(cl-functions))

;; 2021-02-17: don't think this is really needed, not sure where it came from, commenting for now.
;; Use latest Org
;; (use-package org
;;   :ensure org-plus-contrib)

;; Tangle configuration
;; Now using UD trick of only tangling file if it exists, so if not exists, Emacs will still launch.
;; Takes about 25 seconds to tangle dotemacs. This is a huge problem.
;; Takes 10 seconds from "start tangle" to "(Shell command succeeded with no output)", which is next message.
;; Is that the process that writes that? even if not tangling? ANSWER: shell command message comes from
;; tangling. Without having to tangle, no message, but still 7 seconds from "start tangle" message to
;; next, "Loading...dotemacs.el". Essentially, this is 7 seconds to figure out no tangling needed.
;; That's a BIG chunk right there.
;; Got the tangle on save to work, sort of; tried to remove the org-bable-load-file which tangles only
;; when new org file and just load most recent tangled file, got some org errors on load, something else not right.
;; Live with this for another day or so, work on this weekend? Weirdly, got unusual org errors, and org-bullets
;; was not loaded, but when NO CHANGE in org file and restarted with org-babel-load-file instead of load, it
;; worked normally again.
;; NOTE: tangle on save produced a "dotemacs." file in .emacs.d directory which cannot be removed. Great!
(message "Start load dotemacs.el")
(load "~/.emacs.d/dotemacs")
;; (when (file-readable-p "~/.emacs.d/dotemacs.org")
;;   (org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
;;   (message "dotemacs.org tangled or loaded!"))
(garbage-collect)
(message "Manually collected garbage in init.el")

;; 2019-06-29: After loading dotemacs.el (after babel), display statistics of load in
;; buffer "*use-package statistics*"; this will split the dashboard automatically, which we
;; do NOT want, so  delete-other-windows (point remains in dashboard) to retain buffer unseen.
(use-package-report)
(message "Generated use-package-report in init.el")
(delete-other-windows)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; 2019-09-05: put startup elapsed time with GC counter to *Messages* buffer.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; 2021-02-21: add setup for showing backtrace on errors.
(setq debug-on-error t)

(message "init.el completed.")

;; Ensure all is set to UTF-8
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
;; set the default encoding system
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;;; init.el ends here
