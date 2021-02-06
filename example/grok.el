;;; grok.el --- Grok major mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Hubert Grokbold

;; Author: Hubert Grokbold
;; Keywords: outlines
;; Version: 2.7.8
;; Homepage: https://github.com/h-grokbold/grok-mode
;; Package-Requires: ((emacs "26.1"))

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

;; Major mode for editing Grok files.

;;; Code:

(defgroup grok nil
  "Support for Grok files."
  :prefix "grok-"
  :group 'text)

(define-derived-mode grok-mode text-mode "Grok"
  "Set major mode for editing Grok files.")

(provide 'grok)
;;; grok.el ends here
