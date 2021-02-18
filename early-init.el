;;; early-init.el
;;
;; Author:    Arnold Hausmann
;; Created:   2021-02-16
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

;; Set first message to mark Emacs startup timestamp.
(message (concat (format-time-string "[%F %T.%3N] ") "early-init.el started."))

;;; This fixed garbage collection, makes emacs start up faster ;;;;;;;
;; 2019-09-05: Lots of experimenting trying to get faster in Windows. Ref: https://blog.d46.us/advanced-emacs-startup/
;; First is code at bottom, sending a startup elapsed time with garbage collection count to *Messages* buffer.
;; Just setting at beginning and end did little, still around 20 seconds with 5-6 garbage collections.
;; The most-positive-fixnum about the same.

(setq gc-cons-threshold most-positive-fixnum
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
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0)
  (setq w32-pipe-buffer-size (* 64 1024))
  (setq w32-get-true-file-attributes nil)
  )
(when (boundp 'w32-pipe-buffer-size)
  (setq w32-pipe-buffer-size (* 64 1024)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default
 load-prefer-newer t
 package-enable-at-startup nil)

;; 2021-02-16: add function/advice to prepend timestamp to messages.
;; Ref: https://emacs.stackexchange.com/questions/32150/how-to-add-a-timestamp-to-each-entry-in-emacs-messages-buffer
(defun aeh--ad-timestamp-message (FORMAT-STRING &rest args)
  "Advice to run before `message' that prepends a timestamp to each message.
    Activate this advice with:
      (advice-add 'message :before 'aeh--ad-timestamp-message)
    Deactivate this advice with:
      (advice-remove 'message 'aeh--ad-timestamp-message)"
  (if message-log-max
      (let ((deactivate-mark nil)
            (inhibit-read-only t))
        (with-current-buffer "*Messages*"
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (insert (format-time-string "[%F %T.%3N] "))))))
(advice-add 'message :before 'aeh--ad-timestamp-message)
