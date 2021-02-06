;;; superstar-kit.el --- An org-superstar template for other modes -*- lexical-binding: t; -*-

;; Copyright (C) 2020  D. Williams

;; Author: THIS IS A TEMPLATE
;; Maintainer: THIS IS A TEMPLATE
;; Keywords: faces, outlines
;; Version: 0.0.0
;; Homepage: https://github.com/integral-dw/superstar-kit
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; THIS PACKAGE IS MERELY A TEMPLATE AND UNFIT FOR DIRECT USE.  See
;; below for a list of modifications needed to make this package your
;; own.  DO NOT try to use this package directly.

;;  A basic template mode to create an ‘org-superstar’-experience for
;;  modes other than Org.

;; Here are some Unicode blocks which are generally nifty resources
;; for this package:
;;
;; General Punctuation (U+2000-U+206F): Bullets, leaders, asterisms.
;; Dingbats (U+2700-U+27BF)
;; Miscellaneous Symbols and Arrows (U+2B00-U+2BFF):
;;     Further stars and arrowheads.
;; Miscellaneous Symbols (U+2600–U+26FF): Smileys and card suits.
;; Supplemental Arrows-C (U+1F800-U+1F8FF)
;; Geometric Shapes (U+25A0-U+25FF): Circles, shapes within shapes.
;; Geometric Shapes Extended (U+1F780-U+1F7FF):
;;     More of the above, and stars.
;;

;;; TO DO:

;; Here are the minimum necessary steps to make this package useful to
;; others (and yourself).  The text below is formatted as an Org
;; checklist for your convenience, and coincides with the file
;; CHECKLIST.org.

;;; Code:

(require 'wid-edit)

(defgroup superstar-kit nil
  "Use UTF8 bullets for headlines and plain lists."
  ;; FIXME: Change this to the appropriate group of MODE
  :group 'emacs)

;;; Bullet Variables

(defcustom superstar-kit-headline-bullets-list
  '(?◉ ?○ ?🞛 ?▷)
  "List of bullets used in MODE headings.
It can contain any number of bullets, the Nth entry usually
corresponding to the bullet used for level N.  The way this list
is cycled through can be fine-tuned by customizing
‘superstar-kit-cycle-headline-bullets’.

Every entry in this list can either be a character or a cons
cell.  Characters are used as simple, verbatim replacements of
the headline character for every display (be it graphical or
terminal).  If the list element is a cons cell, it should be a
proper list of the form
\(COMPOSE-STRING CHARACTER)

where COMPOSE-STRING should be a string according to the rules of
the third argument of ‘compose-region’.  It will be used to
compose the specific headline bullet.  CHARACTER is the fallback
character used in terminal displays, where composing characters
cannot be relied upon.

You should re-enable KITM after changing this variable
for your changes to take effect."
  :group 'superstar-kit
  :type '(repeat (choice
                  (character :value ?◉
                             :format "Bullet character: %v\n"
                             :tag "Simple bullet character")
                  (list :tag "Advanced string and fallback"
                        (string :value "◉"
                                :format "String of characters to compose: %v")
                        (character :value ?◉
                                   :format "Fallback character for terminal: %v\n")))))

(defun superstar-kit--set-lbullet (symbol value)
  "Set SYMBOL ‘superstar-kit-leading-bullet’ to VALUE.
If set to a character, also set ‘superstar-kit-leading-fallback’."
  (set-default symbol value)
  (when (characterp value)
    (set-default 'superstar-kit-leading-fallback value)))

(defcustom superstar-kit-leading-bullet ?.
  "A special bullet used for leading STARs.
Normally, this variable is a character replacing the default
STARs.  If it’s a string, list, or vector, compose the
replacement according to the rules of ‘compose-region’ for the
COMPONENTS argument.

Leading STARs in a headline are represented as a sequence of this
bullet using the face ‘superstar-kit-leading’.

This variable is only used for graphical displays.
‘superstar-kit-leading-fallback’ is used for terminal displays
instead.

You should re-enable KITM after changing this
variable for your changes to take effect."
  :group 'superstar-kit
  :type '(choice
          (character :tag "Single character to display"
                     :format "\n%t: %v\n"
                     :value ?.)
          (string :tag "String of characters to compose replacement from"
                  :format "\n%t:\n%v"
                  :value "."))
  :set #'superstar-kit--set-lbullet)

(defcustom superstar-kit-leading-fallback
  (cond ((characterp superstar-kit-leading-bullet)
         superstar-kit-leading-bullet)
        (t ?.))
  "A special bullet used for leading STARs.
This variable is a character replacing the default STARs in
terminal displays instead of ‘superstar-kit-leading-bullet’.

If the leading bullet is set to a character before the package is
loaded, this variable’s default value is set to that character as
well.  Setting the leading bullet to a character using the custom
interface also automatically sets this variable.

You should re-enable KITM after changing this
variable for your changes to take effect."
  :group 'superstar-kit
  :type '(character :tag "Single character to display"
                    :format "\n%t: %v\n"
                    :value ?.))


;;; Other Custom Variables

(defcustom superstar-kit-cycle-headline-bullets t
  "Non-nil means cycle through available headline bullets.

The following values are meaningful:

An integer value of N cycles through the first N entries of the
list instead of the whole list.

If otherwise non-nil, cycle through the entirety of the list.

If nil, repeat the final list entry for all successive levels.

You should re-enable KITM after changing this
variable for your changes to take effect."
  :group 'superstar-kit
  :type '(choice
          (const :tag "Cycle through the whole list." t)
          (const :tag "Repeat the last element indefinitely." nil)
          (integer :tag "Repeat the first <integer> elements only."
                   :format "Repeat the first %v entries exclusively.\n"
                   :size 8
                   :value 1
                   :validate superstar-kit--validate-hcycle)))

(defun superstar-kit--validate-hcycle (text-field)
  "Raise an error if TEXT-FIELD’s value is an invalid hbullet number.
This function is used for ‘superstar-kit-cycle-headline-bullets’.
If the integer exceeds the length of
‘superstar-kit-headline-bullets-list’, set it to the length and
raise an error."
  (let ((ncycle (widget-value text-field))
        (maxcycle (superstar-kit--hbullets-length)))
    (unless (<= 1 ncycle maxcycle)
      (widget-put
       text-field
       :error (format "Value must be between 1 and %i"
                      maxcycle))
      (widget-value-set text-field maxcycle)
      text-field)))

(defcustom superstar-kit-remove-leading-chars nil
  "Non-nil means font-lock should hide leading STAR characters.

The indentation caused by leading STARs is completely removed.

You should re-enable KITM after changing this
variable for your changes to take effect."
  :group 'superstar-kit
  :type 'boolean)


;;; Faces

(defface superstar-kit-leading
  '((default . (:inherit default :foreground "gray")))
  "Face used to display prettified leading STARs in a headline."
  :group 'superstar-kit)

(defface superstar-kit-header-bullet
  '((default . nil))
  "Face containing distinguishing features headline bullets.
This face is applied to header bullets \"on top of\" existing
fontification provided by org, allowing you to inherit the
default look of a heading line while still being able to make
modifications.  Every specified face property will replace those
currently in place."
  :group 'superstar-kit)


;;; Accessor Functions

(defun superstar-kit--hbullets-length ()
  "Return the length of ‘superstar-kit-headline-bullets-list’."
  (length superstar-kit-headline-bullets-list))

(defun superstar-kit--hbullet (level)
  "Return the desired headline bullet replacement for LEVEL N.

For more information on how to customize headline bullets, see
‘superstar-kit-headline-bullets-list’.

See also ‘superstar-kit-cycle-headline-bullets’."
  (let ((max-bullets superstar-kit-cycle-headline-bullets)
        (n  (1- level)))
    (cond ((integerp max-bullets)
           (superstar-kit--nth-headline-bullet (% n max-bullets)))
          (max-bullets
           (superstar-kit--nth-headline-bullet
            (% n (superstar-kit--hbullets-length))))
          (t
           (superstar-kit--nth-headline-bullet
            (min n (1- (superstar-kit--hbullets-length))))))))

(defun superstar-kit--nth-headline-bullet (n)
  "Return the Nth specified headline bullet or its corresponding fallback.
N counts from zero.  Headline bullets are specified in
‘superstar-kit-headline-bullets-list’."
  (let ((bullet-entry
         (elt superstar-kit-headline-bullets-list n)))
    (cond
     ((characterp bullet-entry)
      bullet-entry)
     ((display-graphic-p)
      (elt bullet-entry 0))
     (t
      (elt bullet-entry 1)))))

(defun superstar-kit--lbullet ()
  "Return the correct leading bullet for the current display."
  (if (display-graphic-p)
      superstar-kit-leading-bullet
    superstar-kit-leading-fallback))

(defun superstar-kit--heading-level ()
  "Return the heading level of the currently matched headline."
  (- (match-end 0) (match-beginning 0) 1))


;;; Fontification

(defun superstar-kit--prettify-main-hbullet ()
  "Prettify the trailing STAR in a headline."
  (let ((level (superstar-kit--heading-level)))
    (compose-region (match-beginning 1) (match-end 1)
                    (superstar-kit--hbullet level)))
  'superstar-kit-header-bullet)

(defun superstar-kit--prettify-leading-hbullets ()
  "Prettify the leading bullets of a header line.
Each leading STAR is rendered as ‘superstar-kit-leading-bullet’
and inherits face properties from ‘superstar-kit-leading’.

If viewed from a terminal, ‘superstar-kit-leading-fallback’ is
used instead of the regular leading bullet to avoid errors."
  (let ((star-beg (match-beginning 2))
        (lead-end (match-end 2)))
    (while (< star-beg lead-end)
      (compose-region star-beg (setq star-beg (1+ star-beg))
                      (superstar-kit--lbullet)))
    'superstar-kit-leading))

(defun superstar-kit--make-invisible (subexp)
  "Make part of the text matched by the last search invisible.
SUBEXP, a number, specifies which parenthesized expression in the
last regexp.  If there is no SUBEXPth pair, do nothing."
  (let ((start (match-beginning subexp))
        (end (match-end subexp)))
    (when start
      (add-text-properties
       start end '(invisible superstar-kit-hide)))))

(defun superstar-kit--unprettify-hbullets ()
  "Revert visual tweaks made to header bullets in current buffer."
  (save-excursion
    (goto-char (point-min))
    ;; FIXME: Replace REGEXP to match your headlines.
    (while (re-search-forward "^\\*+ " nil t)
      (decompose-region (match-beginning 0) (match-end 0)))))


;;; Font Lock

(defvar-local superstar-kit--font-lock-keywords nil)

(defun superstar-kit--update-font-lock-keywords ()
  "Set ‘superstar-kit--font-lock-keywords’ to reflect current settings.
You should not call this function to avoid confusing this mode’s
cleanup routines."
  (setq superstar-kit--font-lock-keywords
        ;; FIXME: Replace REGEXP to match your headlines.
        `(("^\\(?2:\\**?\\)\\(?1:\\*\\) "
           (1 (superstar-kit--prettify-main-hbullet) prepend)
           ,@(unless superstar-kit-remove-leading-chars
               '((2 (superstar-kit--prettify-leading-hbullets)
                    t)))
           ,@(when superstar-kit-remove-leading-chars
               '((2 (superstar-kit--make-invisible 2))))))))

(defun superstar-kit--fontify-buffer ()
  "Fontify the buffer."
  (when font-lock-mode
    (save-restriction
      (widen)
      (font-lock-ensure)
      (font-lock-flush))))

;;; Mode commands
;;;###autoload
(define-minor-mode superstar-kit-mode
  "Use UTF8 bullets for headlines and plain lists."
  nil nil nil
  :group 'superstar-kit
  :require 'M-PKG
  (cond
   ;; Set up KITM.
   (superstar-kit-mode
    (font-lock-remove-keywords nil superstar-kit--font-lock-keywords)
    (superstar-kit--update-font-lock-keywords)
    (font-lock-add-keywords nil superstar-kit--font-lock-keywords
                            'append)
    (superstar-kit--fontify-buffer)
    (add-to-invisibility-spec '(superstar-kit-hide)))
   ;; Clean up and exit.
   (t
    (remove-from-invisibility-spec '(superstar-kit-hide))
    (font-lock-remove-keywords nil superstar-kit--font-lock-keywords)
    (superstar-kit--unprettify-hbullets)
    (superstar-kit--fontify-buffer))))

(provide 'superstar-kit)
;;; superstar-kit.el ends here
