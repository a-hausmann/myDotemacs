;;; aeh-html-stuff.el    --- -*- lexical-binding: t -*-
;;
;; File name:     aeh-html-stuff.el
;; Created:       Sun Jun 30, 2019 23:52:30
;; Last modified: Sat Apr 18, 2026 15:37:49
;; Purpose:       Define all functions needed to replace my custom Vim HTML key mappings.
;; Version:       0.1

;;; Commentary:
;; This custom package provides functions enabling easier editing of HTML
;; files become available. The objective is to archive HTML pages to \"pure\"
;; text only, removing advertising, pictures, user comments, etc. Most functions
;; change \"smart\" punctuation to straight punctuation and the like.

;; NOTE: with upgrade to version 30.1, need to use "mhtml-mode-hook" instead of "html-mode-hook"

;; Versions
;; 0.1: The base version as of 02/20/2026, lots of functions and Hydra menu.
;; 0.2: changed the menu to Transient, added version constant and function.

(require 'transient)

(defconst aeh-html-stuff-version "0.3")

(defun aeh-html-stuff-version ()
  "Show current version of `aeh-html-stuff'."
  (interactive)
  (message "Personal HTML version: %s" aeh-html-stuff-version))


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
    ((kbd "C-c h") . my-html-stuff-tmenu)
    ))

(add-hook 'mhtml-mode-hook #'aeh-set-politics-directory)


(defun aeh-kill-span-tags ()
  "Use function `sgml-delete-tag' to kill any `<span>' tags within a region.
As this command should ONLY be used in a region, will check to see if a region
has been specified and exit if this command could affect the entire buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (let ((mod-count 0))
            (goto-char (region-beginning))
            (while (re-search-forward "<span" nil t)
              (setq mod-count (1+ mod-count))
              (sgml-delete-tag 1))
            (message "Deleted %s <span> tags" mod-count)))
        (message "No region specified!"))))


;; 05/31/2025
(defun aeh-disable-smartparens-stuff ()
  "Disable smartparens-global-mode and smartparens-global-strict-mode"
  (smartparens-global-mode -1)
  (smartparens-global-strict-mode -1))
(add-hook 'aeh-html-stuff-mode #'aeh-disable-smartparens-stuff)

(defun aeh-disable-column-enforce ()
  "Disable column-enforce-mode, specifically created for html-mode-hook."
  (column-enforce-mode -1))
(add-hook 'mhtml-mode-hook #'aeh-disable-column-enforce)

;; FIXME: Recode this to NOT REQUIRE General!
(general-def aeh-html-stuff-mode-map
  "C-c h" 'my-html-stuff-tmenu
  "C-c _" 'aeh-insert-target-clause
  "C-c ." 'aeh-replace-period-ellipsis-dwim
  "C-c '" 'aeh-insert-single-quote-dwim
  "C-c \"" 'aeh-insert-double-quote-dwim
  "C-c M-'" 'aeh-sub-single-quote-to-html-quote-dwim
  "C-c M-\"" 'aeh-sub-double-quote-to-html-quote-dwim
  "C-c C-'" 'aeh-insert-single-smart-quote-dwim
  "C-c C-\"" 'aeh-insert-double-smart-quote-dwim
  "C-c C-=" 'aeh-delete-class-s
  "C-c a" 'aeh-delete-align-justify
  "C-c b" 'aeh-add-byline-class-title
  "C-c B" 'aeh-replace-line-break-dwim
  "C-c c" 'aeh-add-class-to-body-tag
  "C-c C" 'aeh-insert-css-file
  "C-c d" 'aeh-insert-div-tags
  "C-c D" 'aeh-replace-double-dash-dwim
  "C-c e" 'aeh-replace-set-ital-dwim
  "C-c E" 'aeh-delete-emphasis-tags-interactively
  "C-c f" 'aeh-position-final-para-tag
  "C-c F" 'aeh-zap-to-char-backwards
  "C-c l" 'aeh-insert-townhall-logo-gif
  "C-c i" 'aeh-add-th-icon
  "C-c I" 'aeh-split-list-item-tags-dwim
  "C-c m" 'aeh-delete-carriage-return-dwim
  "C-c M" 'aeh-delete-multiple-empty-lines
  "C-c n" 'aeh-strip-nbsp-dwim
  "C-c o" 'aeh-replace-smart-chars-dwim
  "C-c p" 'aeh-insert-paragraph-tags
  "C-c P" 'aeh-delete-is-pasted-id
  "C-c q" 'aeh-replace-smart-quotes-dwim
  "C-c R" 'aeh-insert-triangle-bullet
  "C-c s" 'aeh-add-date-span
  "C-c S" 'aeh-split-paragraph-tags-dwim
  "C-c t" 'aeh-add-title-anchor-tags
  "C-c T" 'aeh-zap-up-to-char-backwards
  "C-c u" 'aeh-delete-redirect-urls-dwim
  "C-c x" 'aeh-delete-data-pasted-class
  "C-c 0" 'aeh-delete-directionality-attr
  "C-c 9" 'aeh-delete-links-relationship
  "C-c M-f" 'aeh-flush-empty-lines-dwim
  "C-c C-x s" 'aeh-kill-span-tags
  "C-x d" 'dired-jump
)

(keymap-set aeh-html-stuff-mode-map "C-c r" 'consult-recent-file)
(keymap-set aeh-html-stuff-mode-map
    "C-c -" '("Convert dash(s) to bullets" . aeh-convert-dashs-to-triangle-bullet))
(keymap-set aeh-html-stuff-mode-map
    "C-c C-SPC" '("Compress spaces" . aeh-compress-extra-spaces))


;; 08/30/2025: added anchor tags around the logo icon to link to https://townhall.com/
(defun aeh-insert-townhall-logo-gif ()
  "Insert the Townhall logo gif image."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "<body")
    (move-end-of-line nil)
    (newline 2)
    (insert "<p><a target=_blank href=\"https://townhall.com/\" class=\"h1 clear hover\">")
    (insert "<img style=\"width: 250px;margin-bottom: 10px;\" ")
    ;; (insert "src=\"https://media.townhall.com/thm/th_color.gif\" ")  ;; 12/22/2024: townhall changed logo pics
    (insert "src=\"https://townhall.com/svg/thm/logo-townhall.svg\" ")
    (insert "alt=\"Townhall.com logo\" />")
    (insert " </a> </p>")
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
  ;; Don't need to save excursion as insert-file inserts on the next line leaving point in place.
  (insert-file "~/CSS/inline.css")
  (aeh-position-final-para-tag)
  (message "Inserted `~/CSS/inline.css' at point."))


(defun aeh-insert-div-tags()
  "Insert div tags with id=`Outline'"
  (interactive)
    (move-beginning-of-line nil)
    (insert "<div id=\"Outline\">\n")
    (insert "</div> <!-- ID=Outline -->\n")
    (previous-line)
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
        (replace-match "" nil t))
      (message (format "%d directionality attributes removed" mod-count)))))


(defun aeh-delete-is-pasted-id ()
  "Delete the paragraph `is-pasted' id"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((mod-count 0))
      (while (re-search-forward " id=\"isPasted\"" (point-max) t)
        (setq mod-count (+ mod-count 1))
        (replace-match "" nil t))
      (message (format "%d isPasted paragraph ids removed" mod-count)))))


(defun aeh-delete-data-pasted-class ()
  "Delete the paragraph `data-pasted' class in either full document or selected
region. Returns message of count of texts deleted in either document or region."
  (interactive)
  (save-excursion
    (let ((mod-count 0)
          (p-from (if (region-active-p)
                     (region-beginning)
                     (point-min)))
          (p-thru (if (region-active-p)
                     (region-end)
                     (point-max)))
          (p-width (if (region-active-p)
                       "region"
                       "document")))
      (goto-char p-from)
      (while (re-search-forward " data-pasted=\"true\"" p-thru t)
        (replace-match "" t t)
        (setq mod-count (+ mod-count 1)))
      (message (format "%d `data-pasted' classes deleted in %s."
                       mod-count p-width)))))


(defun aeh-delete-align-justify ()
  "Delete the align: \"justify\" from a paragraph tag."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((mod-count 0))
      (while (re-search-forward " align=\"justify\"" (point-max) t)
        (setq mod-count (+ mod-count 1))
        (replace-match "" nil t))
      (message (format "%d align justify attributes removed" mod-count)))))


(defun aeh-delete-links-relationship ()
  "Delete the link tags line for relationships, `<link rel=.*>\n' These SHOULD
all be on one line, so delete the entire line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((mod-count 0))
      (while (re-search-forward "<link rel=.*>\n" (point-max) t)
        (setq mod-count (+ mod-count 1))
        ;; (replace-match "" nil t)
        (beginning-of-line)
        (kill-whole-line))
      (message (format "%d relational links removed" mod-count)))))


(defun aeh-delete-multiple-empty-lines ()
  "Use regular expression to replace two or more empty lines with a single empty
line in the HEAD section only."
  (interactive)
  (save-excursion
    (let ((mod-count 0))
      (query-replace-regexp "^^J\{2,\}" "^J"
                            (save-excursion (aeh-head-start))
                            (save-excursion (aeh-head-end))))))


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
  (insert "<p><a target=_blank \nhref=\"\"\n class=\"h1 clear hover\">")
  (move-end-of-line nil)
  (insert "</a> </p>")
  (move-beginning-of-line nil)
  (search-backward "href=\"")
  (search-forward "\"\"")
  (message "Wrapped current line with anchor tags."))


(defun aeh-add-byline-class-title ()
  "Add a class and target to existing author anchor, includes paragraph tags."
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (search-forward "href=")
    (search-backward " ")
    (insert " class=\"byline\" target=_blank")
    (aeh-insert-paragraph-tags))
  (message "Added class for `byline' to existing author anchor."))


(defun aeh-add-date-span ()
  "Wrap current (dated) line with classed <span> tag.
If evil-mode, change to normal state as movement is expected."
  (interactive)
  (move-beginning-of-line nil)
  (insert "<span class=\"dated\">")
  (move-end-of-line nil)
  (insert "</span>")
  (if evil-mode
      (evil-normal-state))
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
  (save-excursion
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
      (message (format "%d smart quotes replaced in buffer." mod-count)))))


(defun aeh-strip-nbsp-dwim ()
  "The dwim will strip &NBSP by either region or full buffer."
  (interactive)
  (save-excursion
  (cond ((region-active-p)
          (aeh-strip-nbsp (region-beginning) (region-end)))
    (t (aeh-strip-nbsp (point-min) (point-max))))))


(defun aeh-strip-nbsp (p-from p-thru)
  "Replace &nbsp with space."
  (interactive)
  (save-excursion
    (let ((mod-count 0))
      (goto-char p-from)
      (while (re-search-forward "&nbsp;" p-thru t)
        (setq mod-count (+ mod-count 1))
        (replace-match " " nil t))
      (message (format "%d &nbsp removed from buffer." mod-count)))))


(defun aeh-split-paragraph-tags-dwim ()
  "The dwim will split paragraph tags by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh-split-paragraph-tags (region-beginning) (region-end)))
    (t (aeh-split-paragraph-tags (point-min) (point-max)))))


(defun aeh-split-paragraph-tags (p-from p-thru)
  "Replace </p><p> with </p>\n\n<p>. Ensure final </p> is preceded by space."
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((mod-count 0))
          (goto-char p-from)
          (while (re-search-forward "\s*</p><p>" p-thru t)
            (setq mod-count (+ mod-count 1))
            (replace-match " </p>\n\n<p>" nil t)
            (setq p-thru (+ p-thru 3)))  ; increase end point by characters added
          (goto-char p-thru)
          (search-backward "</p")
          (insert " ")
          (message (format "%d paragraphs split." mod-count)))))))


(defun aeh-split-list-item-tags-dwim ()
  "Replace `</li><li>' with ` </li>\n<li>"
  (interactive)
  (save-excursion
    (let ((mod-count 0)
          (p-from (if (region-active-p)
                      (region-beginning)
                      (point)))
          (p-thru (if (region-active-p)
                      (region-end)
                      (point-max)))
          (p-str (if (region-active-p)
                     "region"
                     "document")))
      (goto-char p-from)
      (while (re-search-forward "</li><li>" p-thru t)
        (replace-match " </li>\n<li>" t t)
        (setq mod-count (+ mod-count 1)))
      (message (format "%s list-item tags split in %s" mod-count p-str)))))


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
  "The dwim will replace double line breaks with paragraph tags by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh-replace-line-break (region-beginning) (region-end)))
    (t (aeh-replace-line-break (point-min) (point-max)))))


(defun aeh-replace-line-break (p-from p-thru)
  "Replace double line breaks with paragraph tags"
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


;; FIXME: convert to single function.
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
  "Delete the meta-data tags lines found in the HEAD section."
  (interactive)
  ;; (save-match-data
    (save-excursion
      (goto-char (aeh-head-start))
      (let ((mod-count 0))
        ;; Because ALL the begin/end functions are direct find, must do save-excursion here.
        ;; (while (re-search-forward "<meta .*?/>" (save-excursion (aeh-head-end)) t)
        (while (search-forward "<meta " (save-excursion (aeh-head-end)) t)
          (setq mod-count (+ mod-count 1))
          ;; (replace-match "" nil t)
          (beginning-of-line)
          (kill-whole-line))
        (message (format "%d meta tags deleted in buffer." mod-count)))))


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
          (message "delete redirect-urls dwim in region")
          (aeh-delete-data-saferedirecturl (region-beginning) (region-end))
          )
    (t
      (message "delete redirect-urls dwim in buffer")
      (aeh-delete-data-saferedirecturl (point-min) (point-max)))))


(defun aeh-delete-data-saferedirecturl (p-from p-thru)
  "Delete the `data-saferedirecturl' part of an anchor tag"
  (interactive)
  (save-excursion
    (goto-char p-from)
    (let ((mod-count 0))
      (while (search-forward " data-saferedirecturl" p-thru t)
        (setq mod-count (+ mod-count 1)) 
        (kill-region (search-backward " ")
                     (progn 
                       (search-forward ">" p-thru t 1)
                       (backward-char 1)
                       (point))))
      (message (format "%d data-saferedirecturl tags deleted in buffer." mod-count)))))


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


(defun aeh-insert-single-quote-dwim ()
  "Change/insert a single-quote to an HTML single-quote"
  (interactive)
  (save-excursion
    (insert "&#39;")))


(defun aeh-insert-double-quote-dwim ()
  "Change/insert a double-quote to an HTML double-quote"
  (interactive)
  (save-excursion
    (insert "&#34;")))


(defun aeh-insert-single-smart-quote-dwim(&optional arg)
  "Insert a single-quote to an HTML smart single-quote.
The function will produce a left smart single-quote, but when the universal
argument is used, will produce a smart right-single quote."
  (interactive "P")
  (save-excursion
    (if arg
      (insert "&#8217")
    (insert "&#8216"))))


(defun aeh-insert-double-smart-quote-dwim(&optional arg)
  "Insert a double-quote to an HTML smart double-quote.
The function will produce a left smart double-quote, but when the universal
argument is used, will produce a smart right-double quote."
  (interactive "P")
  (save-excursion
    (if arg
      (insert "&#8221")
    (insert "&#8220"))))


(defun aeh-position-final-para-tag ()
  "Insert a space before the final end-paragraph tag in the document."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (search-backward "</p>")
    (insert " ")))


(defun aeh-delete-class-s ()
  "Delete the ` class=\"sN\"' paragraph attribute"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((mod-count 0))
      (while (re-search-forward " class=\"s[1-9]\"" (point-max) t)
        (setq mod-count (+ mod-count 1))
        (replace-match "" nil t))
      (message (format "%d paragraph class attributes deleted in buffer." mod-count)))))


(defun aeh-replace-set-ital-dwim ()
  "The dwim will replace `(SET/END ITAL)' markers by either region or full buffer."
  (interactive)
  (save-excursion
    (let ((mod-count 0)
          (p-from (if (region-active-p)
                      (region-beginning)
                      (point)))
          (p-thru (if (region-active-p)
                      (region-end)
                      (point-max))) 
          (p-str (if (region-active-p)
                     "region"
                     "document")))
      (goto-char p-from)
      (while (re-search-forward "(SET ITAL)" p-thru t)
        (setq mod-count (+ mod-count 1))
        (replace-match "<em>" t t))
      (goto-char p-from)
      (while (re-search-forward "(END ITAL)" p-thru t)
        (replace-match "</em>" t t))
    (message (format "%d (SET/END ITAL) markers replaced in buffer." mod-count)))))



(defun aeh-replace-set-ital (p-from p-thru)
  "Replace the `(SET/END ITAL)' markers with <em> and </e> tags."
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((mod-count 0))
          (goto-char p-from)
          (while (re-search-forward "(SET ITAL)" p-thru t)
            (setq mod-count (+ mod-count 1))
            (replace-match "<em>" t t))
          (goto-char p-from)
          (while (re-search-forward "(END ITAL)" p-thru t)
            (replace-match "</em>" t t))
          (message (format "%d (SET/END ITAL) markers replaced in buffer." mod-count)))))))


;; Do I even need this?
(defun aeh-insert-triangle-bullet ()
  "Inserts a `triangle bullet' HTML code character at point."
  (interactive)
  (insert "\&#9656;"))

;; (defun aeh-insert-triangle-bullet (&optional arg)
;;   "Inserts a `triangle bullet' HTML code character at point.
;; With prefix argument ARG, substitute dash or double-dash with
;; triangle bullet ARG times. Negative argument will substitute
;; backward."
;;   (interactive "P")
;;   (if (listp arg)
;;       (progn
;;         (setq arg (car arg))
;;         (if (eq arg '-) (setq arg (* -1 arg)))
;;         )
;;       (insert "\&#9656;")
;;       )
;;   )


(defun aeh-zap-to-char-backwards (char)
  "Implements standard `zap-to-char' with negative argment, zapping backwards."
  (interactive "cZap backwards to char: ")
  (save-match-data
    (save-excursion
      (zap-to-char -1 char))))


(defun aeh-zap-up-to-char-backwards (char)
  "Implements standard `zap-up-to-char' with negative argment, zapping backwards."
  (interactive "cZap backwards up to char: ")
  (save-match-data
    (save-excursion
      (zap-up-to-char -1 char))))


(defun aeh-delete-emphasis-tags-interactively ()
  "Will tag HTML emphasis tags for deletion, requesting action for each match.
ALWAYS USE IN REGION, else it will act from current point to point-max."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (query-replace-regexp "</*em>" "" nil (region-beginning) (region-end))
        (message "No region specified!"))))


(defun aeh-convert-dashs-to-triangle-bullet ()
  "Some authors use a single-, double-, or even a triple-dash as a bullet;
this will convert these to a triangle bullet within a region."
  (interactive)
  (save-excursion)
  (if (region-active-p)
      (let ((mod-count 0))
          (goto-char (region-beginning))
          (while (re-search-forward "-\\{1,3\\}" (region-end) t)
            (replace-match "&#9656; " t t)
            (setq mod-count (+ mod-count 1)))
          (message (format "%d double-dashes replaced in region" mod-count)))
      (message "No region specified!")))



(defun aeh-flush-empty-lines-dwim ()
  "Flush empty lines in either region or point to end of buffer."
  (interactive)
  (cond ((region-active-p)
         (flush-lines "^ *$" (region-beginning) (region-end) t))
        (t (flush-lines "^ *$" (point) (point-max) t))))


(defun aeh-sub-single-quote-to-html-quote-dwim ()
  "Substitute straight single-quote with an HTML character in region or buffer."
  (interactive)
  (save-excursion
  (let ((mod-count 0)
        (p-from (if (region-active-p)
                    (region-beginning)
                    (point)))
        (p-thru (if (region-active-p)
                    (region-end)
                    (point-max))) 
        (p-str (if (region-active-p)
                   "region"
                   "document")))
          (goto-char p-from)
          (while (re-search-forward "'" p-thru t)
            (replace-match "&#39;" t t)
            (setq mod-count (+ mod-count 1)))
          (message (format "%d single-quotes replaced in %s" mod-count p-str)))))


(defun aeh-sub-double-quote-to-html-quote-dwim ()
  "Substitute straight double-quote with an HTML character in region or buffer."
  (interactive)
  (save-excursion)
  (let ((mod-count 0)
        (p-from (if (region-active-p)
                    (region-beginning)
                    (point)))
        (p-thru (if (region-active-p)
                    (region-end)
                    (point-max))) 
        (p-str (if (region-active-p)
                   "region"
                   "document")))
          (goto-char p-from)
          (while (re-search-forward "'" p-thru t)
            (replace-match "&#34; " t t)
            (setq mod-count (+ mod-count 1)))
          (message (format "%d double-quotes replaced in %s" mod-count p-str))))


(defun aeh-compress-extra-spaces ()
  "Use regular expression to `compress' two or more contiguous spaces into
a single space. This should ONLY be used on a region, so if no region is
selected, abort function with appropriate message."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (replace-regexp-in-region "[ ]\\{2,\\}" " " (region-beginning) (region-end))
        (message "No region selected; MUST select region first."))))

  
;; Transient Menu.
(transient-define-prefix my-html-stuff-tmenu ()
  "Transient menu for my Townhall HTML editing"
  ["Townhall HTML Editing.\n"
   ["Always Do"
    ("q" "Quit" keyboard-quit :transient nil)
    ("1" "DEL ^M"
         aeh/delete-carriage-return-dwim :transient t)
    ("2" "DEL &nbsp;"
         aeh-strip-nbsp-dwim :transient t)
    ("3" "DEL Meta Tags"
         aeh-delete-meta-tags :transient t)
    ("4" "DEL Stylesheets"
         aeh-delete-stylesheets :transient t)
    ("5" "DEL Script tags"
         aeh-delete-script-tags :transient t)
    ("6" "INS HREF"
         aeh-insert-base-href :transient t)
    ("7" "REP Smart Quotes"
         aeh-replace-smart-quotes-dwim :transient t)
    ("8" "REP Smart Chars"
         aeh-replace-smart-chars-dwim :transient t)
    ("9" "DEL Link rels"
         aeh-delete-links-relationship :transient t)
    ("i" "INS TH icon"
         aeh-add-th-icon :transient t)
    ("l" "INS TH logo"
         aeh-insert-townhall-logo-gif :transient t)
    ("c" "INS Body=class"
         aeh-add-class-to-body-tag :transient t)]

   ["Sometimes Do (and exit)"
    ("0" "DEL Dir= attr"
         aeh-delete-directionality-attr :transient nil)
    ("a" "DEL <P align="
         aeh-delete-align-justify :transient nil)
    ("d" "INS <DIV>"
         aeh-insert-div-tags :transient nil)
    ("e" "Replace ITAL (dwim)"
         aeh-replace-set-ital-dwim :transient nil)
    ("p" "INS <P>"
         aeh-insert-paragraph-tags :transient nil)
    ("s" "Add <SPAN> (date)"
         aeh-add-date-span :transient nil)
    ("t" "Add <A> (title)"
         aeh-add-title-anchor-tags :transient nil)
    ("u" "DEL safe URL (dwim)"
         aeh-delete-redirect-urls-dwim :transient nil)
    ("x" "DEL pasted class"
         aeh-delete-data-pasted-class :transient nil)
    ("B" "REP <BR> (dwim)"
         aeh-replace-line-break-dwim :transient nil)
    ("C" "INS CSS file"
         aeh-insert-css-file :transient nil)
    ("D" "REP -- (dwim)"
         aeh-replace-double-dash-dwim :transient nil)
    ("P" "DEL pasted ID"
         aeh-delete-is-pasted-id :transient nil)
    ("." "REP ellipsis (dwim)"
         aeh-replace-period-ellipsis-dwim :transient nil)]

   ["Rarely Do (and exit)"
    ("F" "Flush empty lines (dwim)"
         aeh-flush-empty-lines-dwim :transient nil)
    ("I" "Split list-item tags (dwim)"
         aeh-split-list-item-tags-dwim :transient nil)
    ("'" "Sub single-quote to HTML"
         aeh-sub-single-quote-to-html-quote-dwim :transient nil)
    ("\"" "Sub double-quote to HTML"
          aeh-sub-double-quote-to-html-quote-dwim :transient nil)
    ("SPC" "Compress spaces to single space"
           aeh-compress-extra-spaces :transient nil)
    ("-" "Sub dash(s) to triangle bullet"
         aeh-convert-dashs-to-triangle-bullet :transient nil)]
  ])




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
