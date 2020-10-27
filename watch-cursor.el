;;; watch-cursor.el --- Display cursor from all live windows  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-10-27 13:25:37

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Display cursor from all live windows.
;; Keyword: cursor
;; Version: 0.1.0
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

(defface watch-cursor-face
  '((t :box '(:line-width 0 :color "#40FF40" :style nil)))
  "Face for fake cursor."
  :group 'watch-cursor)

(defvar-local watch-cursor--buffer nil
  "Buffer we are currently watching.")

(defvar-local watch-cursor--cursors '()
  "List of cursor data.")

(defvar-local watch-cursor--overlays '()
  "List of overlay represent as fake cursors.")

;;
;; (@* "Utility" )
;;

(defmacro watch-cursor--walk-windows (&rest body)
  "Macro for function `walk-windows', execute BODY for each window."
  (declare (indent 0) (debug t))
  `(walk-windows (lambda (win) (select-window win) (progn ,@body)) nil t))

(defun watch-cursor--watching-p ()
  "Return non-nil if current buffer is the buffer we are watching."
  (eq watch-cursor--buffer (current-buffer)))

(defun watch-cursor--delete-overlays ()
  "Delete all overlays in list."
  (dolist (ov watch-cursor--overlays) (delete-overlay ov))
  (setq watch-cursor--overlays nil))

(defun watch-cursor--make-overlay (pt win)
  "Create a overlay at PT inside WIN."
  (let ((ol (make-overlay pt (1+ pt))))
    (overlay-put ol 'face 'watch-cursor-face)
    (overlay-put ol 'priority 0)
    (overlay-put ol 'window win)
    (push ol watch-cursor--overlays)  ; NOTE: Eventually get managed bt list.
    ol))

(defun watch-cursor--make-overlays ()
  "Make all fake cursors."
  (when (< 1 (length watch-cursor--cursors))
    (let (win pt)
      (watch-cursor--walk-windows
        (dolist (data watch-cursor--cursors)
          (setq win (car data) pt (cdr data))
          (unless (equal win (selected-window))
            (watch-cursor--make-overlay pt (selected-window))))))))

;;
;; (@* "Core" )
;;

(defun watch-cursor--get-cursors-data ()
  "Update cursor data once."
  (setq watch-cursor--cursors '())
  (watch-cursor--walk-windows
    (when (watch-cursor--watching-p)
      (push (cons (selected-window) (point)) watch-cursor--cursors))))

(defun watch-cursor--pre-command ()
  "Pre-command hook for `watch-cursor'."
  (watch-cursor--delete-overlays))

(defun watch-cursor--post-command ()
  "Post-command hook for `watch-cursor'."
  (watch-cursor--get-cursors-data)
  (watch-cursor--make-overlays))

;;
;; (@* "Entry" )
;;

(defun watch-cursor--enable ()
  "Enable `watch-cursor' in current buffer."
  (setq watch-cursor--buffer (current-buffer))
  (add-hook 'pre-command-hook 'watch-cursor--pre-command nil t)
  (add-hook 'post-command-hook 'watch-cursor--post-command nil t))

(defun watch-cursor--disable ()
  "Disable `watch-cursor' in current buffer."
  (watch-cursor--delete-overlays)
  (remove-hook 'pre-command-hook 'watch-cursor--pre-command t)
  (remove-hook 'post-command-hook 'watch-cursor--post-command t))

;;;###autoload
(define-minor-mode watch-cursor-mode
  "Minor mode 'watch-cursor-mode'."
  :lighter " WC"
  :group watch-cursor
  (if watch-cursor-mode (watch-cursor--enable) (watch-cursor--disable)))

(provide 'watch-cursor)
;;; watch-cursor.el ends here
