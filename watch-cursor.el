;;; watch-cursor.el --- Display cursor from all live windows  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-10-27 13:25:37

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Display cursor from all live windows.
;; Keyword: cursor
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs-elpa/watch-cursor

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
;;
;; Display cursor from all live windows.
;;

;;; Code:

(defgroup watch-cursor nil
  "Display cursor from all live windows."
  :prefix "watch-cursor-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/watch-cursor"))

(defvar-local watch-cursor--buffer nil
  "")

(defun watch-cursor--post-command ()
  ""
  )

;;
;; (@* "Entry" )
;;

(defun watch-cursor--enable ()
  "Enable `watch-cursor' in current buffer."
  (setq watch-cursor--buffer (current-buffer))
  (add-hook 'post-command-hook 'watch-cursor--post-command nil t))

(defun watch-cursor--disable ()
  "Disable `watch-cursor' in current buffer."
  (remove-hook 'post-command-hook 'watch-cursor--post-command t))

;;;###autoload
(define-minor-mode watch-cursor-mode
  "Minor mode 'watch-cursor-mode'."
  :lighter " WC"
  :group watch-cursor
  (if watch-cursor-mode (watch-cursor--enable) (watch-cursor--disable)))

(provide 'watch-cursor)
;;; watch-cursor.el ends here
