;;; aeh-html-stuff.el    --- -*- lexical-binding: t -*-
;;
;; File name:     aeh-html-stuff.el
;; Created:       Sun Jun 30, 2019 23:52:30
;; Last modified: Tue Sep 15, 2020 23:48:48
;; Purpose:       Define all functions needed to replace my custom Vim HTML key mappings.
;; Version:       0.1

;;; Commentary:
;; This custom package provides functions enabling easier editing of HTML
;; files become available. The objective is to archive HTML pages to \"pure\"
;; text only, removing advertising, pictures, user comments, etc. Most functions
;; change \"smart\" punctuation to straight punctuation and the like.

;;;###autoload
(define-minor-mode aeh-html-stuff-mode
  "Toggle HTML Stuff mode
Interactive without argument, this command toggles the mode.
A positive prefix enables the mode, any other prefix disables it.
"
  :init-value nil
  :lighter " Stuff"
  ;; keymap definitions are weird; cannot use kbd function and in the format below,
  ;; everything EXCEPT the dash must be escaped, even the spaces.  Weird!!!
  :keymap
  '(
     ("\C-\c\ \;" . aeh-hydra-html-stuff-menu/body)   ;define "C-c ;"
     ("\C-\c\ \q" . aeh-replace-smart-quotes-dwim)    ;define "C-c q"
     ("\C-\c\ \n" . aeh-strip-nbsp-dwim)              ;define "C-c n"
     ("\C-\c\ \a" . aeh-replace-ampersand-dwim)       ;define "C-c a"
     ("\C-\c\ \o" . aeh-replace-smart-chars-dwim)     ;define "C-c o"
     ("\C-\c\ \m" . aeh/delete-carriage-return-dwim)  ;define "C-c m"
     ;; ("\C-\c\ \C-\c\ \t" . aeh-href-anchor)           ;define "C-c C-c t"
     )
  :group 'html)

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
      (aeh-replace-smart-quotes (point-min) (point-max))
      )))

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
            (setq mod-count (+ mod-count 1)))
          ;; n-dash (the narrower dash)
          (goto-char p-from)
          (while (re-search-forward "[–]" p-thru t)
            (setq mod-count (+ mod-count 1)))
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
      (save-restriction
        (goto-char (aeh-head-start))
        (let ((mod-count 0))
          ;; Because ALL the begin/end functions are direct find, must do save-excursion here.
          (while (re-search-forward "^<meta .*?/>\n*" (save-excursion (aeh-head-end)) t)
            (setq mod-count (+ mod-count 1))
            (replace-match "" nil t))
          (message (format "%d meta tags deleted in buffer." mod-count)))))))

(defun aeh-delete-stylesheets ()
  "Delete any stylesheet tags found in the HEAD section. Note that if the link
tag does NOT end with a newline, the remainder of the line will be left intact."
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (goto-char (aeh-head-start))
        (let ((mod-count 0))
          ;; Because ALL the begin/end functions are direct find, must do save-excursion here.
          (while (re-search-forward "<link.*stylesheet.*?>\n*" (save-excursion (aeh-head-end)) t)
            (setq mod-count (+ mod-count 1))
            (replace-match "" nil t))
          (message (format "%d stylesheets deleted in buffer." mod-count)))))))

(defun aeh-insert-base-href ()
  "Insert a <base href...> tag after the <head> tag line."
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (goto-char (aeh-head-start))
        (end-of-line)
        (newline)
        (insert "<base href=\"http://townhall.com\">")
        (message "Inserted base href tag.")))))

(defun aeh-delete-script-tags ()
  "Delete the script tags found in the HEAD section. Note that if the meta
tag does NOT end with a newline, the remainder of the line will be left intact."
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (let ((mod-count 0))
          ;; (while (re-search-forward "<script.*</script>?\n*" (point-max) t)
          (while (re-search-forward "<script.*?>" (point-max) t)
            (setq mod-count (+ mod-count 1))
            (delete-region (aeh-script-start) (progn (aeh-script-end) (search-forward ">"))))
          (message (format "%d script tags deleted in buffer." mod-count)))))))

(defhydra aeh-hydra-html-stuff-menu (:color pink)
  "Custom HTML functions."
  ("q" nil "Quit")
  ("1" (aeh-delete-carriage-return-dwim) "Delete ^M")
  ("2" (aeh-strip-nbsp-dwim) "Strip &nbsp; dwim")
  ("3" (aeh-delete-meta-tags) "Delete meta tags")
  ("4" (aeh-delete-stylesheets) "Delete stylesheets")
  ("5" (aeh-delete-script-tags) "Delete script tags")
  ("6" (aeh-insert-base-href) "Insert Base href")
  ("7" (aeh-replace-smart-quotes-dwim) "Replace smart quotes dwim")
  ("8" (aeh-replace-smart-chars-dwim) "Replace smart chars dwim")
  ("t" (skeleton-insert aeh-href-anchor -1) "Insert anchor tags")
  )

;; Ref: http://ergoemacs.org/emacs/emacs_html_insert_tags.html
;; Ref: https://www.gnu.org/software/emacs/manual/html_mono/autotype.html
;; Ref: https://www.reddit.com/r/emacs/comments/av1h8v/how_i_became_a_vertebrate_or_skeletons_help_lazy/
;; Yasnippet ref: https://stackoverflow.com/questions/22735895/configuring-a-yasnippet-for-two-scenarios-1-region-is-active-2-region-is
;; https://joaotavora.github.io/yasnippet/snippet-expansion.html
;; https://joaotavora.github.io/yasnippet/snippet-development.html#org67f4e69
(define-skeleton aeh-href-anchor
  "HTML anchor tag with href attribute."
  "URL: "
  ;; '(setq input "http:")
  "<a class=\"h1 clear hover\" href=\"" str "\">" _ "</a>")

(provide 'aeh-html-stuff)
