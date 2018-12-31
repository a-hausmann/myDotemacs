;; Name:    aeh-hydras.el
;; Dated:   2018-11-06
;; Purpose: This will contain my personal hydra definitions.  Much of this is 
;;          based on Bailey Ling's hydras, so some changing of names, and deleting
;;          stuff he has that I do not (I favor ivy over helm, he uses both.

(require 'hydra)
(autoload 'hydra-default-pre "hydra")

;; aeh/hydra-insert-date-menu
(defconst aeh/date-simple "%m/%d/%Y" "Simple format: MM/DD/YYYY")
(defconst aeh/date-format "%Y-%m-%d" "Simple date as YYYY-MM-DD")
(defconst aeh/date-time-format "%Y-%m-%d %-H:%M:%S" "Simple Date with Time: YYYY-MM-DD HH24:MI:SS")
(defconst aeh/day-format "%a %b %d, %Y" "English date as: Day Mon Date, Year")
(defconst aeh/day-time-format "%a %b %d, %Y %-H:%M:%S" "English Date Time as: Day Mon Date, Year HH24:MI:SS")
(defconst aeh/full-day-format "%A, %B %d, %Y" "English date as: Day, Month Date, Year")
(defconst aeh/full-day-time-format "%A, %B %d, %Y %-H:%M:%S %p" "English Date Time as: Day, Month Date, Year HH:MI:SS PM")

(defhydra aeh/hydra-insert-date-menu (:color blue)
  "
_q_: quit
_s_: MM/DD/YYYY
_d_: YYYY-MM-DD
_t_: YYYY-MM-DD HH24:MI:SS
_D_: DD Mon Date, Year
_T_: DD Mon Date, Year HH24:MI:SS
_e_: Day, Month Day, Year
_E_: Day, Month Day, Year HH:MI:SS PM
 "
  ("q" nil)
  ("s" (insert (format-time-string aeh/date-simple)))
  ("d" (insert (format-time-string aeh/date-format)))
  ("t" (insert (format-time-string aeh/date-time-format)))
  ("D" (insert (format-time-string aeh/day-format)))
  ("T" (insert (format-time-string aeh/day-time-format)))
  ("e" (insert (format-time-string aeh/full-day-format)))
  ("E" (insert (format-time-string aeh/full-day-time-format))))

;; This is a straight copy from the github site: https://github.com/abo-abo/hydra/wiki/Basics
;; I've removed the mapping as I'll do that via evil-leader.
;; 2018-11-10: I like the idea I found here: https://ericjmritz.wordpress.com/2015/10/14/some-personal-hydras-for-gnu-emacs/
;; This sets more "natural" keys "+", and "-" to zoom in/out, and "0" to reset
(defhydra aeh/hydra-zoom (:hint nil)
  "zoom"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-set 0) "reset" :color blue)
  ("q" nil "quit"))

;; Rockin' the buffers menu!
(defhydra aeh/hydra-buffers (:hint nil :exit t)
  "
   buffers:   _b_ → buffers          _i_ → ibuffer                 _k_ → kill buffer
              _p_ → prev buffer      _m_ → goto messages           _e_ → erase buffer
              _E_ → erase buffer (force)
              _s_ → goto scratch     _S_ → goto scratch (force)    _z_ → zoom
"
  ("b" #'counsel-ibuffer)
  ("k" #'kill-this-buffer)
  ("i" #'ibuffer)
  ("p" #'aeh/switch-to-previous-buffer)
  ("m" (switch-to-buffer "*Messages*"))
  ("e" #'erase-buffer)
  ("E" (let ((inhibit-read-only t)) (erase-buffer)))
  ("s" (switch-to-buffer "*scratch*"))
  ("S" (switch-to-buffer (get-buffer-create "*scratch*")))
  ("z" #'hydra-zoom/body))

;; Hydra for file conversions
(defhydra aeh/hydra-files-convert (:hint nil :exit t)
  "
  convert to:    _d_ → dos       _u_ → unix
  "
  ("d" aeh/set-buffer-to-dos-format)
  ("u" aeh/set-buffer-to-unix-format))

;; Hydra for bookmarks
(defhydra aeh/hydra-bookmarks (:hint nil :exit t)
  "
  bookmarks:    _b_ → jump to bookmark _l_ → list bookmarks    _s_ → set bookmark
                _D_ → delete bookmark
  "
  ("b" #'bookmark-set)
  ("l" #'bookmark-bmenu-list)
  ("s" #'bookmark-set)
  ("D" #'bookmark-delete)
  )

;; Hydra for files
(defhydra aeh/hydra-files (:hint nil :exit t)
  "
  files:
  _f_ → find files      _D_ → delete    _y_ → copy filename   _E_ → edit as root   _z_ → fzf
  _r_ → recent files    _R_ → rename    _c_ → copy file       _C_ → convert        _b_ → bookmarks
  "
  ("D" aeh/delete-current-buffer-file)	;; OK
  ("R" aeh/rename-current-buffer-file)	;; OK
  ("f" #'counsel-find-file)		;; OK
  ("r" #'counsel-recentf)		;; OK
  ("y" aeh/copy-file-name-to-clipboard)	;; OK
  ("E" aeh/edit-file-as-root)		;; OK
  ("c" copy-file)			;; OK
  ("C" aeh/hydra-files-convert/body)	;; OK
  ("b" aeh/hydra-bookmarks/body)        ;; OK
  ("z" #'counsel-fzf))                  ;; OK

;; Hydra for toggles
;; 2018-11-18: have installed "autopair" as pairing package; Bailey used to switch between smartparens and electric-pairs, but
;; I don't see that happening for me at this point, so am removing this toggle.
(defvar aeh/hydras/toggles/vdiff nil)
(defhydra aeh/hydra-toggles (:hint nil :exit t)
  "
   toggle:  _a_ → aggressive indent   _s_ → flycheck   _r_ → read only      _t_ → truncate lines   _e_ → debug on error
            _f_ → auto-fill           _S_ → flyspell   _c_ → completion     _W_ → word wrap        _g_ → debug on quit
            _w_ → whitespace          ^ ^              ^ ^                    _b_ → page break       _d_ → ediff/vdiff
"
  ("a" aggressive-indent-mode)
  ("c" company-mode)
  ("t" toggle-truncate-lines)
  ("e" toggle-debug-on-error)
  ("g" toggle-debug-on-quit)
  ("b" page-break-lines-mode)
  ("s" flycheck-mode)
  ("S" flyspell-mode)
  ("w" whitespace-mode)
  ("W" toggle-word-wrap)
  ("r" read-only-mode)
  ("f" auto-fill-mode)
  ;; Needs work on the "/pairs/toggle" code, copied from Bailey Ling's code base.
  ;; This switches between smartparens and electric-pairs; at this point, I'm not sure what is installed.
  ;; ("p" /pairs/toggle)
  ("d" (progn
	 (if aeh/hydras/toggles/vdiff
	     (progn
	       (/bindings/vdiff/turn-off)
	       (message "using ediff"))
	   (/vcs/setup-vdiff)
	   (/bindings/vdiff/turn-on)
	   (message "using vdiff"))
	 (setq aeh/hydras/toggles/vdiff (not aeh/hydras/toggles/vdiff)))))


;; Hydra for rectangles
(defhydra aeh/hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                         :color pink
                                         :hint nil
                                         :post (deactivate-mark))
  "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
  _h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \- ;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
"
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("d" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("w" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("c" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("g" nil))      ;; ok

(defhydra aeh/hydra-org (:color red :hint nil)
"
Navigation^
---------------------------------------------------------
_q_uit
_j_ next heading
_k_ prev heading
_h_ next heading (same level)
_l_ prev heading (same level)
_u_p higher heading
_g_o to
"
("q" nil :exit t)
("j" outline-next-visible-heading)
("k" outline-previous-visible-heading)
("h" org-forward-heading-same-level)
("l" org-backward-heading-same-level)
("u" outline-up-heading)
("g" org-goto :exit t))

;; 2018-11-19: create yasnippet hydra, set blue overall to quit upon command
(defhydra aeh/hydra-yasnippet (:color blue :hint nil)
  "
Snippets^
---------------------------------------------------------
_q_ uit
_i_ insert snippet
_n_ new snippet
_l_ load directory
_r_ reload all
_v_ visit snippet
_d_ describe table
"
  ("q" nil :exit t)
  ("i" yas-insert-snippet)
  ("n" yas-new-snippet)
  ("l" yas-load-directory)
  ("r" yas-reload-all)
  ("v" yas-visit-snippet-file)
  ("d" yas-describe-tables))


;; 2018-12-31: Add hydra for Counsel commands
(defhydra aeh/hydra-counsel (:color blue :hint nil)
  "
Counsel^
---------------------------------------------------------
_q_ uit
_a_ apropos
_b_ bookmarks
_B_ describe bindings
_f_ describe functions
_v_ describe variables
_c_ describe faces
_C_ list faces
_l_ find library
_L_ load library
_m_ imenu
_M_ mark ring
_T_ load theme
"
  ("q" nil :exit t)
  ("a" counsel-apropos)
  ("b" counsel-bookmark)
  ("B" counsel-descbinds)
  ("f" counsel-describe-function)
  ("v" counsel-describe-variable)
  ("c" counsel-describe-face)
  ("C" counsel-faces)
  ("l" counsel-find-library)
  ("L" counsel-load-library)
  ("m" counsel-imenu)
  ("M" counsel-mark-ring)
  ("T" counsel-load-theme))

(provide 'aeh-hydras)
