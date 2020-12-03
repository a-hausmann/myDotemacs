;;; aeh-font.el --- -*- lexical-binding: t -*-
;;
;; Filename:      aeh-font.el
;; Author:        Arnold Hausmann
;; Created:       2020-07-19
;; Last modified: Sun Jul 19, 2020 23:49:40
;; Email:         arnoldh@comcast.net, aehjr1@gmail.com
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Commentary:
;;
;; Configure fonts and icons
;;
;; This version of file (config-font.el is original) is meant for current
;; Evil mode version of Dotemacs.org file.
;; Ref: https://protesilaos.com/codelog/2020-07-16-emacs-focused-editing/
;; Ref: https://protesilaos.com/codelog/2020-07-17-emacs-mixed-fonts-org/
;; Ref: https://protesilaos.com/codelog/2020-07-18-emacs-concept-org-tweaked-focus/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; From : https://protesilaos.com/codelog/2020-07-17-emacs-mixed-fonts-org/
;; There are multiple ways to set your font in Emacs.  I find that doing so
;; at the “face” level is effective and yields consistent results.

;; I will show you how to do it, but we must briefly cover the basics
;; before we deal with the actual code.

;; In Emacs parlance a “face” is a display construct that contains data
;; such as an associated foreground and background colour, as well as all
;; typographic properties (height, weight, font family, slant, etc.).

;; Couched in those terms, an Emacs theme is, in essence, a program that
;; controls such faces.  You can already spot several faces on display here
;; (the current theme is my Modus Vivendi---in some of my other videos I
;;   use its light counterpart: Modus Operandi).

;; *** The three faces we need to take care of

;; To make things work, we will be using the =set-face-attribute= function.
;; And we need to configure three faces:

;; 1. =default= :: This one is the standard font family that all other faces
;; refer to when they have no font specification of their own.  In
;; general, faces *should not* have a typeface spec, unless they need to.

;; 2. =fixed-pitch= :: This should be given a monospaced typeface and is
;; meant to be inherited by faces that must always be presented with
;; fixed-spacing.

;; 3. =variable-pitch= :: Whereas this should be a proportionately-spaced
;; font.  Again, it is meant to be inhereted by faces that are supposed
;; to be presented with such a typeface (though you do not need to
;; specify those faces, as =M-x variable-pitch-mode= does it automatically).

;; For me a monospaced font should be the standard, so in practice I
;; configure =default= and =fixed-pitch= to use the same typeface.

;; ** Themes will affect your results

;; Note that your theme must have configured everything properly for this
;; to work.  More specifically, you must make sure that all indentation or
;; spacing-sensitive faces are designed to always inherit from =fixed-pitch=.
;; Petition your theme's developer to account for this workflow of mixing
;; fonts.

;; Or use my Modus themes which are designed to meet the highest
;; accessibility standard for colour contrast.  Basically this is about
;; readability, while the technical spec is called “WCAG AAA”: it
;; corresponds to a *minimum contrast ratio* between background and
;; foreground values of 7:1.

;; The themes are /highly customisable/ and have wide package coverage.

;; See  https://gitlab.com/protesilaos/modus-themes for details.

;; *** Notes for theme devs and DIY users

;; These are the faces for =org-mode= that your theme needs to configure with
;; the =:inherit fixed-pitch= attribute.  Special thanks to Gitlab user “Ben”
;; for bringing these to my attention a while ago!

;; + org-block
;; + org-block-begin-line
;; + org-block-end-line
;; + org-code
;; + org-document-info-keyword
;; + org-meta-line
;; + org-table
;; + org-verbatim

;; Optionally also consider these for =markdown-mode=:

;; + markdown-code-face
;; + markdown-html-attr-name-face
;; + markdown-html-attr-value-face
;; + markdown-html-entity-face
;; + markdown-html-tag-delimiter-face
;; + markdown-html-tag-name-face
;; + markdown-inline-code-face
;; + markdown-language-info-face
;; + markdown-language-keyword-face
;; + markdown-pre-face
;; + markdown-table-face;;


;; End of comments!

(eval-when-compile
  (require 'config-constant))


"⁌", "◎", "⦿", "ʘ", "⦾",
