;; File:       aeh-spaceline.el
;; Date:       2018-12-02
;; Purpose:    This file tests a version of Spaceline configuration.
;; Reference:  https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#12-mode-line
;;
;; 2018-12-02: This is partly working out as I do see some of the things I want: major mode,
;; file name, color change on edited file, etc. OTOH, I do NOT see line and column, or minor
;; modes (minor consideration). and I need to see Git info if Git file, etc. Git info there
;; still do not have column info, and I prefer Arrow separator, and the spacing of all this
;; stuff is still off.
;;
;; 2018-12-08: Got some good info from:
;;     http://amitp.blogspot.com/2017/01/emacs-spaceline-mode-line.html
;;     https://www.reddit.com/r/emacs/comments/6wb2g0/how_to_define_a_spaceline_segment_showing_the/
;; and was able to tie these back to the main site: https://github.com/TheBB/spaceline
;; 2019-01-03: Had issues immediately upon installing in Windows with the spaceline package--no line at all.
;; Basically, the same issue I had for a long time in Linux and finally solved (missing closing parenthesis),
;; however, this was a different issue. Here I was getting "void-variable spaceline-highlight-face" error
;; message. This was solved by changing the ":face" property from the "main" definition, as evidently, only
;; "highlight-face" exists in Windows (this works properly in Linux, though Windows Spaceline is verion 20181223.)
;; The other annoying propensity was separators which had "E0B2" as characters. Solved this by changing
;; "powerline-default-separator" value from "utf-8" to "bar".
;; BUG: still: the "line-column" segment just does not show in Windows. I can get "column" by itself, but not
;; "line".  These latter two are said to be defined but not used in "default themes" which is a little weird
;; that one should work but not the other.

;; 2019-02-12: copied the "Windows" version, renamed to original, and it worked! The only issues were the
;; changes to the upgraded package, requiring changes to customizations.

;; This configuration also requires "delight"
(use-package delight
  :ensure nil
  :demand
  :preface
  (defun me/delight-powerline-major-mode (original-function &rest arguments)
    (let ((inhibit-mode-name-delight nil)) (apply original-function arguments)))
  (defun me/delight-powerline-minor-mode (original-function &rest arguments)
    (let ((inhibit-mode-name-delight nil)) (apply original-function arguments)))
  :config
  (advice-add 'powerline-major-mode :around #'me/delight-powerline-major-mode)
  (advice-add 'powerline-minor-mode :around #'me/delight-powerline-minor-mode))

;; 2018-12-09: this is copied from my Spacemacs config file, to change face of modified buffers.
;; I can create these custom faces, but there is not much now which "hooks" them into the
;; buffer-modified; there IS no buffer-unmodified or buffer-read-only, so even though I can successfully
;; define them here, I cannot actually USE them. WORSE, no matter WHAT I do, the buffer-modified color
;; does not change based on buffer status.
(custom-set-faces
 '(spaceline-modified ((t (:background "#C11B17" :foreground "#000000"   ; chili pepper red
                                       :inherit (quote mode-line)))))
 '(spaceline-unmodified ((t (:background "#00FF00" :foreground "#000000" ; lime green
                                         :inherit (quote mode-line)))))
 '(spaceline-read-only ((t (:background "#0000FF" :foreground "#FFFFFF"	 ; blue
					:inherit (quote mode-line))))))

;; Copied from the reference source.
(use-package spaceline
  :demand
  :config
  ;; 2018-12-09: attempted to define a buffer status segment to accurately "describe" the actual status.
  ;; This did not go well, and when I use this in spaceline-install, it always shows an empty line after the initial "a"
  ;; character...I don't yet know why.
  ;; 2018-12-10: I've figured out why the "a" in the first character; it's the Ace-window ID, which I've defined as "asdfgh" in sequence.
  ;; And, I finally NAILED it; the bug was a missing parentheses around the "buffer-modified-p" line, as that is a function. I think
  ;; I like this a bit better as I NEVER have a blank spot: "--" for unchanged, "**" for changed, "RO" for read-only. colors seem OK

  (spaceline-define-segment aeh/buffer-status
    "Buffer status: read-only, modified"
    (cond (buffer-read-only (propertize "RO" 'face 'spaceline-read-only))
  	      ((buffer-modified-p) (propertize "**" 'face 'spaceline-modified))
	      ;; (t "--")))
	      (t (propertize "--" 'face 'spaceline-unmodified))))

  ;; This is again one of Mathieu's customizations, but without the custom package function call. I should be able to use it.
  (spaceline-define-segment aeh/version-control
    "Show the current version control branch."
    (when vc-mode
      (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name))))))))

;; Cannot figure out how this is supposed to work....
;; (spaceline-toggle-line-column-on)

;; And now for the spaceline-config package.
(use-package spaceline-config
  :ensure nil
  :demand)
;; Configure the mode-line
(setq-default
 ;; mode-line-format '("%e" (:eval (spaceline-ml-main)))
 ;; powerline-default-separator 'utf-8
 powerline-default-separator 'arrow
 powerline-height 20
 spaceline-highlight-face-func 'spaceline-highlight-face-modified ;; OK, but I'd like to change foreground color.
 spaceline-flycheck-bullet "❖ %s"
 spaceline-window-numbers-unicode t
 ; spaceline-separator-dir-left '(left . left)
 ; spaceline-separator-dir-right '(right . right)
 )
;; (spaceline-helm-mode 1)

;; Build the mode-lines
(spaceline-compile
  ;; `main
  ;; Left hand side (lhs) definition
  '(
    (aeh/buffer-status)
    ((remote-host buffer-id) :face default-face :separator "|")
    ;; (buffer-id :face default-face)
    (evil-state)
    ;; (remote-host buffer-id)
    (process :when active))
  ;; Right hand side (rhs) definition
  '(
    (line-column :face highlight-face)
    (selection-info :face highlight-face)
    (major-mode :face highlight-face :separator " | ")
    (minor-modes)
    ;; ((flycheck-error flycheck-warning flycheck-info))
    ;; (which-function-mode)
    (version-control :when active)
    (buffer-encoding)
    (buffer-position)
    (buffer-size)
    (global :when active)
    (aeh/selection-info)
    )
  )

(spaceline-toggle-hud-on)
(powerline-reset)
