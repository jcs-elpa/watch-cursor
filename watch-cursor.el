;;; watch-cursor.el --- Show all cursors from other windows  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024  Shen, Jen-Chieh
;; Created date 2020-10-27 13:25:37

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/watch-cursor
;; Version: 0.1.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience cursor

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
;; This minor mode display all cursors when there are 2 or more windows
;; displaying the same buffer at a time.
;;
;; To enable this minor mode, please do the following execution
;;
;;   `(watch-cursor-mode 1)`
;;
;; You can customize the fake cursors by customizing the face `watch-cursor-face'.
;;

;;; Code:

(defgroup watch-cursor nil
  "Show all cursors from other windows."
  :prefix "watch-cursor-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/watch-cursor"))

(defface watch-cursor-face
  '((t :background "#40FF40"))
  "Face for fake cursor."
  :group 'watch-cursor)

(defcustom watch-cursor-delay 0.2
  "Seconds of delay before displaying fake cursors."
  :type 'float
  :group 'watch-cursor)

(defvar-local watch-cursor--buffer nil
  "Buffer we are currently watching.")

(defvar-local watch-cursor--cursors '()
  "List of cursor data.")

(defvar-local watch-cursor--overlays '()
  "List of overlay represent as fake cursors.")

(defvar-local watch-cursor--timer nil
  "Timer to display fake cursors.")

;;
;; (@* "Utility" )
;;

(defun watch-cursor--kill-timer (timer)
  "Kill TIMER the safe way."
  (when (timerp timer) (cancel-timer timer)))

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

(defun watch-cursor--make-overlay (pt win owner-win)
  "Create a overlay at PT inside WIN.

Argument OWNER-WIN is for echoing tip."
  (let ((ol (make-overlay pt (1+ pt))))
    (overlay-put ol 'face 'watch-cursor-face)
    (overlay-put ol 'priority 0)
    (overlay-put ol 'window win)
    (overlay-put ol 'help-echo (format "Cursor from %s" owner-win))
    (push ol watch-cursor--overlays)  ; NOTE: Eventually get managed bt list.
    ol))

(defun watch-cursor--make-overlays ()
  "Make all fake cursors."
  (when (< 1 (length watch-cursor--cursors))
    (watch-cursor--walk-windows
      (dolist (data watch-cursor--cursors)
        (let ((win (car data)) (pt (cdr data)))
          (unless (equal win (selected-window))
            (watch-cursor--make-overlay pt (selected-window) win)))))))

;;
;; (@* "Core" )
;;

(defun watch-cursor--get-cursors-data ()
  "Update cursor data once."
  (setq watch-cursor--cursors '())
  (watch-cursor--walk-windows
    (when (watch-cursor--watching-p)
      (push (cons (selected-window) (point)) watch-cursor--cursors))))

(defun watch-cursor--display ()
  "Display all fake cursors."
  (watch-cursor--get-cursors-data)
  (watch-cursor--make-overlays))

(defun watch-cursor--pre-command ()
  "Pre-command hook for `watch-cursor'."
  (watch-cursor--delete-overlays))

(defun watch-cursor--post-command ()
  "Post-command hook for `watch-cursor'."
  (watch-cursor--kill-timer watch-cursor--timer)
  (setq watch-cursor--timer (run-with-timer watch-cursor-delay nil
                                            #'watch-cursor--display)))

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
