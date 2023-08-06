;;; aeh-html-stuff.el    --- -*- lexical-binding: t -*-
;;
;; File name:     aeh-html-stuff.el
;; Created:       Sun Jun 30, 2019 23:52:30
;; Last modified: Fri Jul 28, 2023 15:11:30
;; Purpose:       Define all functions needed to replace my custom Vim HTML key mappings.
;; Version:       0.1

;;; Commentary:
;; This custom package provides functions enabling easier editing of HTML
;; files become available. The objective is to archive HTML pages to \"pure\"
;; text only, removing advertising, pictures, user comments, etc. Most functions
;; change \"smart\" punctuation to straight punctuation and the like.

(defvar aeh-html-stuff-mode-map nil "Keymap for `aeh-html-stuff-mode'")
(setq aeh-html-stuff-mode-map (make-sparse-keymap))
(define-minor-mode aeh-html-stuff-mode
  "Toggle HTML Stuff mode
Interactive without argument, this command toggles the mode.
A positive prefix enables the mode, any other prefix disables it.
"
  :init-value nil
  :lighter " Stuff"
  :group 'html
  :keymap
  '(
    ((kbd "C-c h") . aeh-hydra-html-stuff-menu/body)
))

(add-hook 'html-mode-hook #'aeh-set-politics-directory)

(defun aeh-disable-column-enforce ()
  "Disable column-enforce-mode, specifically created for html-mode-hook."
  (column-enforce-mode -1))
(add-hook 'html-mode-hook #'aeh-disable-column-enforce)

(general-def aeh-html-stuff-mode-map
  "C-c h" 'aeh-hydra-html-stuff-menu/body
  "C-c _" 'aeh-insert-target-clause
  "C-c ." 'aeh-replace-period-ellipsis-dwim
  "C-c a" 'aeh-delete-align-justify
  "C-c b" 'aeh-add-byline-class-title
  "C-c B" 'aeh-replace-line-break-dwim
  "C-c c" 'aeh-add-class-to-body-tag
  "C-c C" 'aeh-insert-css-file
  "C-c d" 'aeh-insert-div-tags
  "C-c D" 'aeh-replace-double-dash-dwim
  "C-c f" 'aeh-position-final-para-tag
  "C-c l" 'aeh-insert-townhall-logo-gif
  "C-c m" 'aeh-delete-carriage-return-dwim
  "C-c n" 'aeh-strip-nbsp-dwim
  "C-c o" 'aeh-replace-smart-chars-dwim
  "C-c P" 'aeh-insert-paragraph-tags
  "C-c q" 'aeh-replace-smart-quotes-dwim
  "C-c s" 'aeh-add-date-span
  "C-c S" 'aeh-split-paragraph-tags-dwim
  "C-c t" 'aeh-add-title-anchor-tags
  "C-c v" 'aeh-insert-div-tags
  "C-c 0" 'aeh-delete-directionality-attr
  "C-c 9" 'aeh-delete-links-relationship
  "C-c M-n" 'aeh-new-untitled-buffer
  "C-x d" 'dired-jump
  )

(defun aeh-insert-today-full-english-day()
  "Insert TODAY as Full English Day"
  (interactive)
  (insert (format-time-string "%A")))

(defun aeh-insert-townhall-logo-gif ()
  "Insert the Townhall logo gif image."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "<body")
    (move-end-of-line nil)
    (newline 2)
    (insert "<img style=\"width: 250px;margin-bottom: 10px;\" ")
    (insert "src=\"https://media.townhall.com/thm/th_color.gif\" ")
    (insert "alt=\"Townhall.com logo\" />")
    (newline))
  (message "Inserted Townhall logo image tag."))

(defun aeh-set-politics-directory ()
  "Set default directory for HTML work to `~/data/arnold/Politics'"
  (interactive)
  (setq default-directory "~/data/arnold/Politics/")
  (message "Set default directory to `~/data/arnold/Politics/'"))

(defun aeh-insert-css-file()
  "Insert custom CSS file at point"
  (interactive)
  (insert-file "~/CSS/inline.css")
  (aeh-position-final-para-tag)
  (message "Inserted `~/CSS/inline.css' at point."))

(defun aeh-insert-div-tags()
  "Insert div tags with id=`Outline'"
  (interactive)
  (move-beginning-of-line nil)
  (insert "<div id=\"Outline\">\n")
  (insert "</div> <!-- ID=Outline -->")
  (newline)
  (previous-line 1)
  (message "Inserted div tags."))

(defun aeh-insert-paragraph-tags ()
  "Insert paragraph tags at beginning and end of the current line."
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (insert "<p>")
    (move-end-of-line nil)
    (insert " </p>"))
  (message "Inserted paragraph tags."))

(defun aeh-delete-directionality-attr ()
  "Delete the directionality attribute from a paragraph tag, generally `dir=\"ltr\"'"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((mod-count 0))
      (while (re-search-forward " dir=\"ltr\"" (point-max) t)
      (setq mod-count (+ mod-count 1))
      (replace-match "" nil t)
      (message (format "%d directionality attributes removed" mod-count))))))

(defun aeh-delete-align-justify ()
  "Delete the align: \"justify\" from a paragraph tag."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((mod-count 0))
      (while (re-search-forward " align=\"justify\"" (point-max) t)
      (setq mod-count (+ mod-count 1))
      (replace-match "" nil t)
      (message (format "%d align justify attributes removed" mod-count))))))

(defun aeh-delete-links-relationship ()
  "Delete the link tags for relationships, `<link rel=...>'"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((mod-count 0))
      (while (re-search-forward "<link rel=.*\n" (point-max) t)
      (setq mod-count (+ mod-count 1))
      (replace-match "" nil t)
      (message (format "%d relational links removed" mod-count))))))

(defun aeh-add-th-icon ()
  "Add the x-icon image link for townhall.com"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "<title>")
    (move-end-of-line nil)
    (newline)
    (insert "<link type=\"image/x-icon\" ")
    (insert "href=\"https://media.townhall.com/townhall/favicon.ico\" ")
    (insert "rel=\"shortcut icon\" />")
    (newline))
  (message "Inserted Townhall icon tag."))

(defun aeh-add-class-to-body-tag ()
  "Add a class to a bare-bones <body> tag, thus enabling our CSS."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "<body>")
    (newline)
    (search-backward ">")
    (insert " class=\"body\"")
    (move-beginning-of-line nil)
    (backward-char)
    (newline))
  (message "Appended class attribute to bare `body' tag."))

(defun aeh-add-title-anchor-tags ()
  "Wrap current line (title) in paragraph and anchor tags."
  (interactive)
  (move-beginning-of-line nil)
  (insert "<p><a target=_blank href=\"\" class=\"h1 clear hover\">")
  (move-end-of-line nil)
  (insert "</a> </p>")
  (move-beginning-of-line nil)
  (search-forward "href=\"")
  (message "Wrapped current line with anchor tags."))

(defun aeh-add-byline-class-title ()
  "Add a class and target to existing author anchor"
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (search-forward "href=")
    (search-backward " ")
    (insert " class=\"byline\" target=_blank"))
  (message "Added class for `byline' to existing author anchor."))

(defun aeh-add-date-span ()
"Wrap current (dated) line with classed <span> tag."
(interactive)
  (move-beginning-of-line nil)
  (insert "<span class=\"dated\">")
  (move-end-of-line nil)
  (insert "</span>")
  (message "Added `span' tag for date line."))

(defun aeh-head-start ()
  "Return point for beginning of <head> tag."
  (interactive)
  (goto-char (point-min))
  (search-forward "<head>")
  (search-backward "<"))

(defun aeh-head-end ()
  "Return point for beginning of </head> tag."
  (interactive)
  (goto-char (point-min))
  (search-forward "</head>")
  (search-backward "<"))

(defun aeh-body-start ()
  "Return point for beginning of <body> tag."
  (interactive)
  (goto-char (point-min))
  (search-forward "<body>")
  (search-backward "<"))

(defun aeh-body-end ()
  "Return point for beginning of </body> tag."
  (interactive)
  (goto-char (point-min))
  (search-forward "</body>")
  (search-backward "<"))

(defun aeh-script-start ()
  "Return point for beginning of <script> tag."
  (interactive)
  (goto-char (point-min))
  (re-search-forward "<script.*?>" (point-max))
  (search-backward "<"))

(defun aeh-script-end ()
  "Return point for beginning of </script> tag."
  (interactive)
  (goto-char (point-min))
  (search-forward "</script>")
  (search-backward "<"))

(defun aeh-replace-smart-quotes-dwim ()
  "The dwim will replace smart quotes by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (message "smart-quotes dwim in region")
          (aeh-replace-smart-quotes (region-beginning) (region-end))
          )
    (t
      (message "smart-quotes dwim in buffer")
      (aeh-replace-smart-quotes (point-min) (point-max)))))

(defun aeh-replace-smart-quotes (p-from p-thru)
  "Replace smart quotes with plain quotes in text"
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((mod-count 0))
          (goto-char p-from)
          ;; smart left double-quote
          (while (re-search-forward "[“]" p-thru t)
            (setq mod-count (+ mod-count 1))
            (replace-match "\&#8220;" nil t))
          (goto-char p-from)
          ;; smart right double-quote
          (while (re-search-forward "[”]" p-thru t)
            (setq mod-count (+ mod-count 1))
            (replace-match "\&#8221;" nil t))
          (goto-char p-from)
          ;; smart left single-quote
          (while (re-search-forward "[‘]" p-thru t)
            (setq mod-count (+ mod-count 1))
            (replace-match "\&#8216;" nil t))
          (goto-char p-from)
          ;; smart right single-quote
          (while (re-search-forward "[’]" p-thru t)
            (setq mod-count (+ mod-count 1))
            (replace-match "\&#8217;" nil t))
          (message (format "%d smart quotes replaced in buffer." mod-count)))))))

(defun aeh-strip-nbsp-dwim ()
  "The dwim will strip &NBSP by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh-strip-nbsp (region-beginning) (region-end)))
    (t (aeh-strip-nbsp (point-min) (point-max)))))

(defun aeh-strip-nbsp (p-from p-thru)
  "Replace &nbsp with space."
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((mod-count 0))
          (goto-char p-from)
          (while (re-search-forward "&nbsp;" p-thru t)
            (setq mod-count (+ mod-count 1))
            (replace-match " " nil t))
          (message (format "%d &nbsp removed from buffer." mod-count)))))))

(defun aeh-split-paragraph-tags-dwim ()
  "The dwim will split paragraph tags by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh-split-paragraph-tags (region-beginning) (region-end)))
    (t (aeh-split-paragraph-tags (point-min) (point-max)))))

(defun aeh-split-paragraph-tags (p-from p-thru)
  "Replace </p><p> with </p>\n\n<p>. Ensure final </p> is preceeded by space."
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((mod-count 0))
          (goto-char p-from)
          (while (re-search-forward " *</p><p>" p-thru t)
            (setq mod-count (+ mod-count 1))
            (replace-match " </p>\n\n<p>" nil t))
          (goto-char p-thru)
          (search-backward "</p")
          (insert " ")
          (message (format "%d paragraphs split." mod-count)))))))

(defun aeh-replace-double-dash-dwim ()
  "The dwim will replace double-dashes by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh-replace-double-dash (region-beginning) (region-end)))
    (t (aeh-replace-double-dash (point-min) (point-max)))))

(defun aeh-replace-double-dash (p-from p-thru)
  "Replace double-dash with HTML character"
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((mod-count 0))
          (goto-char p-from)
          (while (re-search-forward "-\\{2,3\\}" p-thru t)
            (setq mod-count (+ mod-count 1))
            (replace-match "\&#8212;" nil t))
          (message (format "%d double-dashes replaced in buffer." mod-count)))))))

(defun aeh-replace-period-ellipsis-dwim ()
  "The dwim will replace period ellipsis by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh-replace-period-ellipsis (region-beginning) (region-end)))
    (t (aeh-replace-period-ellipsis (point-min) (point-max)))))

(defun aeh-replace-period-ellipsis (p-from p-thru)
  "Replace period ellipsis with HTML character"
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((mod-count 0))
          (goto-char p-from)
          (while (re-search-forward "\\. *\\. *\\." p-thru t)
            (setq mod-count (+ mod-count 1))
            (replace-match "\&#8230;" nil t))
          (message (format "%d period ellipses replaced in buffer." mod-count)))))))

(defun aeh-replace-line-break-dwim ()
  "The dwim will replace period ellipsis by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh-replace-line-break (region-beginning) (region-end)))
    (t (aeh-replace-line-break (point-min) (point-max)))))

(defun aeh-replace-line-break (p-from p-thru)
  "Replace period ellipsis with HTML character"
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((mod-count 0))
          (goto-char p-from)
          (while (re-search-forward " *<br> *<br>" p-thru t)
            (setq mod-count (+ mod-count 1))
            (replace-match " </p>\n\n<p>" nil t))
          (message (format "%d line breaks replaced in buffer." mod-count)))))))

(defun aeh-replace-smart-chars-dwim ()
  "The dwim will replace special characters by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh-replace-smart-chars (region-beginning) (region-end)))
    (t (aeh-replace-smart-chars (point-min) (point-max)))))

(defun aeh-replace-smart-chars (p-from p-thru)
  "Replace special characters with HTML characters"
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((mod-count 0))
          (goto-char p-from)
          ;; m-dash (the wide dash)
          (while (re-search-forward "[—]" p-thru t)
            (setq mod-count (+ mod-count 1))
            (replace-match "\&#8212;" nil t))
          ;; n-dash (the narrower dash)
          (goto-char p-from)
          (while (re-search-forward "[–]" p-thru t)
            (setq mod-count (+ mod-count 1))
            (replace-match "\&#8211;" nil t))
          ;; elipsis
          (goto-char p-from)
          (while (re-search-forward "[…]" p-thru t)
            (setq mod-count (+ mod-count 1))
            (replace-match "\&#8230;" nil t))
          (message (format "%d smart characters replaced in buffer." mod-count)))))))

(defun aeh-replace-ampersand-dwim ()
  "The dwim will replace ampersand characters by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh-replace-ampersand (region-beginning) (region-end)))
    (t (aeh-replace-ampersand (point-min) (point-max)))))

(defun aeh-replace-ampersand (p-from p-thru)
  "Replace ampersand character with HTML character.
This can be relatively dangerous, because if performed after other HTML characters
are in the buffer, this will replace the HTML character prefix--which is an
ampersand--bollixing the HTML which is there.

This function is best used sparingly, and only after narrowing the buffer to the
region in question.
"
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((mod-count 0))
          (goto-char (p-from))
          ;; ampersand
          (while (re-search-forward "[&]" p-thru t)
            (setq mod-count (+ mod-count 1))
            (replace-match "&#38;" nil t))
          (message (format "%d special characters replaced in buffer." mod-count)))))))

(defun aeh-delete-meta-tags ()
  "Delete the meta-data tags found in the HEAD section. Note that if the meta
tag does NOT end with a newline, the remainder of the line will be left intact."
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (aeh-head-start))
      (let ((mod-count 0))
        ;; Because ALL the begin/end functions are direct find, must do save-excursion here.
        (while (re-search-forward "^<meta .*?/>\n*" (save-excursion (aeh-head-end)) t)
          (setq mod-count (+ mod-count 1))
          (replace-match "" nil t))
        (message (format "%d meta tags deleted in buffer." mod-count))))))

(defun aeh-delete-stylesheets ()
  "Delete any stylesheet tags found in the HEAD section. Note that if the link
tag does NOT end with a newline, the remainder of the line will be left intact.
Added the <style> tag to this on 2023-01-09."
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (aeh-head-start))
      (let ((mod-count 0))
        ;; Because ALL the begin/end functions are direct find, must do save-excursion here.
        (while (re-search-forward "<link.*stylesheet.*?>\n*" (save-excursion (aeh-head-end)) t)
          (setq mod-count (+ mod-count 1))
          (replace-match "" nil t))
        (goto-char (aeh-head-start))
        (while (re-search-forward "<style>.*?</style>\n*?" (save-excursion (aeh-head-end)) t)
          (setq mod-count (+ mod-count 1))
          (replace-match "" nil t))
        (message (format "%d stylesheets deleted in buffer." mod-count))))))

(defun aeh-delete-redirect-urls-dwim ()
  "The dwim will delete clauses from anchor tags in buffer or region."
  (interactive)
  (cond ((region-active-p)
          (message "delete-redirect-urls dwim in region")
          (aeh-delete-redirect-urls (region-beginning) (region-end))
          )
    (t
      (message "delete-redirect-urls dwim in buffer")
      (aeh-delete-redirect-urls (point-min) (point-max)))))

(defun aeh-delete-redirect-urls (p-from p-thru)
  "Delete the `data-saferedirecturl' clauses from anchor tags."
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (goto-char p-from)
        (let ((mod-count 0))
          (while (re-search-forward "data-saferedirecturl=.*\\s" p-thru)
            (setq mod-count (+ mod-count 1))
            (replace-match "" nil t))
          (message (format "%d redirect-urls deleted in buffer." mod-count)))))))

(defun aeh-insert-base-href ()
  "Insert a <base href...> tag after the <head> tag line."
  (interactive)
  (save-excursion
    (goto-char (aeh-head-start))
    (end-of-line)
    (newline)
    (insert "<base href=\"http://townhall.com\">")
    (message "Inserted base href tag.")))

(defun aeh-delete-script-tags ()
  "Delete the script tags found in the HEAD section. Note that if the meta
tag does NOT end with a newline, the remainder of the line will be left intact."
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((mod-count 0))
        ;; (while (re-search-forward "<script.*</script>?\n*" (point-max) t)
        (while (re-search-forward "<script.*?>" (point-max) t)
          (setq mod-count (+ mod-count 1))
          (delete-region (aeh-script-start) (progn (aeh-script-end) (search-forward ">"))))
        (message (format "%d script tags deleted in buffer." mod-count))))))

(defun aeh-insert-target-clause ()
  "Prefix an `href' clause with a `target=_blank' clause. 
Need to start at beginning of a line and move forward."
  (interactive)
  (save-excursion
    (search-forward "href=")
    (search-backward " ")
    (insert " target=_blank")))

(defun aeh-position-final-para-tag ()
  "Insert a space before the final end-paragraph tag in the document."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (search-backward "</p>")
    (insert " ")))

;; Option "l" has to be offset to the right one character due to the required
;; backslash in "^M" to force the carat to display on the menu.
(defhydra aeh-hydra-html-stuff-menu (:color red)
  "
Custom HTML functions.
_q_ -> Quit
_1_ -> Delete \^M
_2_ -> Strip &nbsp                 _a_ -> Insert para tags
_3_ -> Delete meta tags            _c_ -> Add body class
_4_ -> Delete stylesheets          _d_ -> Insert div tags 
_5_ -> Delete script tags          _i_ -> Add TH icon
_6_ -> Insert base href            _l_ -> Insert TH logo
_7_ -> Replace smart quotes        _n_ -> New buffer
_8_ -> Replace smart chars         _S_ -> Split paragraphs
_9_ -> Delete relationship links   _s_ -> Add span tags
_0_ -> Delete dir attribute        _t_ -> Insert anchor tags
_C_ -> Insert CSS file             _._ -> Replace ...
_B_ -> Replace <br>
_D_ -> Replace --
"
  ("q" nil)
  ("<esc>" nil)
  ("1" #'aeh-delete-carriage-return-dwim)
  ("2" #'aeh-strip-nbsp-dwim)
  ("3" #'aeh-delete-meta-tags)
  ("4" #'aeh-delete-stylesheets)
  ("5" #'aeh-delete-script-tags)
  ("6" #'aeh-insert-base-href)
  ("7" #'aeh-replace-smart-quotes-dwim)
  ("8" #'aeh-replace-smart-chars-dwim)
  ("9" #'aeh-delete-links-relationship)
  ("0" #'aeh-delete-directionality-attr)
  ("a" #'aeh-insert-paragraph-tags)
  ("B" #'aeh-replace-line-break-dwim)
  ("C" #'aeh-insert-css-file)
  ("c" #'aeh-add-class-to-body-tag)
  ("D" #'aeh-replace-double-dash-dwim)
  ("d" #'aeh-insert-div-tags)
  ("i" #'aeh-add-th-icon)
  ("l" #'aeh-insert-townhall-logo-gif)
  ("n" #'aeh-new-untitled-buffer)
  ("S" #'aeh-split-paragraph-tags-dwim)
  ("s" #'aeh-add-date-span)
  ("t" #'aeh-add-title-anchor-tags)
  ("." #'aeh-replace-period-ellipsis-dwim)
  )

;; Ref: http://ergoemacs.org/emacs/emacs_html_insert_tags.html
;; Ref: https://www.gnu.org/software/emacs/manual/html_mono/autotype.html
;; Ref: https://www.reddit.com/r/emacs/comments/av1h8v/how_i_became_a_vertebrate_or_skeletons_help_lazy/
;; Yasnippet ref: https://stackoverflow.com/questions/22735895/configuring-a-yasnippet-for-two-scenarios-1-region-is-active-2-region-is
;; https://joaotavora.github.io/yasnippet/snippet-expansion.html
;; https://joaotavora.github.io/yasnippet/snippet-development.html#org67f4e69
;; 2022-10-16, have not been able to get this to work, using "aeh-add-title-anchor-tags" instead.
(define-skeleton aeh-href-anchor
  "HTML anchor tag with href attribute."
  "URL: "
  ;; '(setq input "http:")
  "<a class=\"h1 clear hover\" href=\"" str "\">" _ "</a>")

(provide 'aeh-html-stuff)
