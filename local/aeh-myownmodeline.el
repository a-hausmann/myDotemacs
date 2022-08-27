;; File name:     aeh-myownmodeline.el   --- -*- lexical-binding: t -*-
;; Created:       Sat Jul 23, 2022 17:04:51
;; Last modified: Sat Jul 30, 2022 22:53:57
;; Purpose:       This is to create my own, personalized modeline.
;;

;; 2022-07-30: New ref: https://gitlab.com/jessieh/mood-line/-/blob/master/mood-line.el
;; This is single module simple modeline with left and right definitions. Should be able to use this.

;; Set active window & inactive window modelines to different colors with fringe
;; Ref: https://www.saltycrane.com/blog/2007/10/emacs-mode-line-color-custimization/
;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html
;; (set-face-background 'modeline          "#4466aa")
;; (set-face-background 'modeline-inactive "#99aaff")
;; (set-face-background 'fringe "#809088")

;;------------------------------------------------------------------------------------------
;;; ref: https://emacs.stackexchange.com/questions/13652/how-to-customize-mode-line-format
;; (setq-default mode-line-format
;;               '((:eval
;;                  (cond
;;                    (buffer-read-only
;;                     (propertize " ⚑ "
;;                                 'face '(:foreground "red" :weight 'bold)
;;                                 'help-echo "buffer is read-only!!!"))
;;                    ((buffer-modified-p)
;;                     (propertize " ☡ "
;;                                 'face '(:foreground "orange")
;;                                 'help-echo "buffer modified."))))))
;;------------------------------------------------------------------------------------------


;; I've likes Prot's modeline for a long time, and he now has his emacs config in 
;; pure emacs-lisp, not a literate file. So I will use bits and chunks of that.
;; Ref (main): https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/
;; Ref: https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-emacs-modules/prot-emacs-modeline.el
(setq mode-line-percent-position '(-3 "%p"))
;; (setq mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28
(setq mode-line-position (list "(%l:%c)")) ; Emacs <28
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))

;; There are some good clues to how to get this generically without using a package
;; in "simple-modeline", ref: https://github.com/gexplorer/simple-modeline
(defun aeh/mode-line--format (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT, aligned respectively."
  (let ((reserve (length right)))
    (concat left
            " "
            (propertize " "
                        'display `((space :align-to (- right (- 0 right-margin) ,reserve))))
            right)))


;; Thanks to Daniel Mendler for this!  It removes the square brackets
;; that denote recursive edits in the modeline.  I do not need them
;; because I am using Daniel's `recursion-indicator':
;; <https://github.com/minad/recursion-indicator>.
(setq-default mode-line-modes
              (seq-filter (lambda (s)
                            (not (and (stringp s)
                                      (string-match-p
                                       "^\\(%\\[\\|%\\]\\)$" s))))
                          mode-line-modes))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                ;; mode-line-mule-info
                ;; mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                " | "
                mode-line-position
                mode-line-modes
                (vc-mode vc-mode)
                " | "
                mode-line-misc-info
                mode-line-end-spaces))

;; Ref: https://gitlab.com/jessieh/mood-line/-/blob/master/mood-line.el
;; This is where we can call aeh/mode-line--format to get left and right sides.
;; Shoot, this doesn't work as is, must have missed something somewhere. "format-mode-line" perhaps?
;; (setq-default mode-line-format
;;               '((:eval
;;                  (aeh/mode-line--format
;;                   ;; Left
;;                   (format-mode-line
;;                    '(" "
;;                      (:eval (mode-line-modified))
;;                      (:eval (mode-line-remote))
;;                      (:eval (mode-line-frame-identification))
;;                      (:eval (mode-line-buffer-identification))
;;                      ;; (:eval (mode-line-anzu))
;;                      ;; (:eval (mode-line-multiple-cursors))
;;                      (:eval (mode-line-position))))

;;                   ;; Right
;;                    '((:eval (mode-line-eol))
;;                      (:eval (mode-line-encoding))
;;                      (:eval (vc-mode vc-mode))
;;                      (:eval (mode-line-modes))
;;                      (:eval (mode-line-misc-info))
;;                      ;; (:eval (mode-line-flycheck))
;;                      ;; (:eval (mode-line-flymake))
;;                      ;; (:eval (mode-line-process))
;;                      " ")))))
;; (setq-default mode-line-format
;;               '((:eval
;;                  (aeh/mode-line--format
;;                   ;; Left
;;                   (format-mode-line
;;                    '(" "
;;                      mode-line-modified
;;                      mode-line-remote
;;                      mode-line-frame-identification
;;                      mode-line-buffer-identification
;;                      ;; (:eval (mode-line-anzu))
;;                      ;; (:eval (mode-line-multiple-cursors))
;;                      mode-line-position))
;;                   ;; Right
;;                    (format-mode-line
;;                     (mode-line-eol
;;                      mode-line-encoding
;;                      mode vc-mode
;;                      mode-line-modes
;;                      mode-line-misc-info
;;                      ;; (:eval (mode-line-flycheck))
;;                      ;; (:eval (mode-line-flymake))
;;                      ;; (:eval (mode-line-process))
;;                      " "))))))

(setq minions-prominent-modes
      (list 'defining-kbd-macro
            'beacon-mode
            'flymake-mode))
