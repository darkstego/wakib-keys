;;; wakib-mode.el --- Minor Mode for Modern Keybindings -*- lexical-binding: t -*-

;; Author: Abdulla Bubshait
;; URL: https://github.com/darkstego/wakib-mode
;; Created: 6 April 2018
;; Keywords: vim, evil, leader, keybindings, keys
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; Emacs minor mode that provides a modern, efficient and easy to learn keybindings

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


;; Functions & Macros


(defun wakib-dynamic-binding (key)
  "Act as prefix definition in the current context.
This uses an extended menu item's capability of dynamically computing a
definition. This idea came from general.el"
  `(menu-item
	 ,""
	 nil
	 :filter
	 ,(lambda (&optional _)
		 (wakib-key-binding key))))


;; should probably use let instead of double call to (car x)
(defun wakib-minor-mode-key-binding (key)
  (let ((active-maps nil))
	 (mapc (lambda (x) 
				(when (and (symbolp (car x)) (symbol-value (car x)))
				  (add-to-list 'active-maps  (lookup-key (cdr x) (kbd key)))))
			 minor-mode-map-alist )
	 (make-composed-keymap active-maps)))


;; might need to do keymap inheretence to perserve priority
(defun wakib-key-binding (key)
  (make-composed-keymap (list (wakib-minor-mode-key-binding key) (local-key-binding (kbd key)) (global-key-binding (kbd key)))))

;; Commands

(defun wakib-previous (&optional arg)
  "Perform context aware Previous function"
  (interactive "p")
  (cond ((eq last-command 'yank)
			(yank-pop arg))
		  ))

(defun wakib-next (&optional arg)
  "Perform context aware Next function"
  (interactive "p")
  (cond ((eq last-command 'yank)
			(yank-pop (- arg)))))



;; might be a more functional way to do this
(defun wakib-select-line-block-all ()
  "Selects line, if line is selected then selects block, if block
is selected then selects entire buffer"
  (interactive)
  (let ((p1 (region-beginning))
		  (p2 (region-end))
		  (x1)
		  (x2)
		  (end-p))
	 (unless (region-active-p)
		(setq p1 (point))
		(setq p2 (point)))
	 (setq end-p (eq p2 (point)))
	 (goto-char p1)
	 (beginning-of-line)
	 (setq x1 (point))
	 (push-mark x1 t t)
	 (goto-char p2)
	 (end-of-line)
	 (setq x2 (point)) 
	 (when (and (eq x1 p1)
					(eq x2 p2))
		(goto-char p1)
		(when (re-search-backward "\n[ \t]*\n" nil "move")
		  (re-search-forward "\n[ \t]*\n"))
		(setq x1 (point))
		(push-mark x1 t t)
		(goto-char p2)
		(when (re-search-forward "\n[ \t]*\n" nil "move")
		  (re-search-backward "\n[ \t]*\n"))
		(setq x2 (point)))
	 (when (and (eq x1 p1)
					(eq x2 p2))
		(beginning-of-buffer)
		(setq x1 (point))
		(push-mark x1 t t)
		(end-of-buffer)
		(setq x2 (point)))
	 (when (not end-p)
		(push-mark x2 t t)
		(goto-char x1))))

(defun wakib-back-to-indentation-or-beginning () (interactive)
		 (if (= (point) (progn (back-to-indentation) (point)))
			  (beginning-of-line)))


(defun wakib-beginning-line-or-block ()
  "Move to the beginning of line, if there then move to beginning of block"
  (interactive)
  (let ((p (point)))
	 (beginning-of-line)
	 (when (eq p (point))
		(when (re-search-backward "\n[ \t]*\n" nil "move")
		  (re-search-forward "\n[ \t]*\n")))
	 (when (eq p (point))
		(re-search-backward "\n[ \t]*\n" nil "move")
		(when (re-search-backward "\n[ \t]*\n" nil "move")
		  (re-search-forward "\n[ \t]*\n")))))

(defun wakib-end-line-or-block ()
  "Move to the end of line, if there then move to end of block"
  (interactive)
  (let ((p (point)))
	 (end-of-line)
	 (when (eq p (point))
		(when (re-search-forward "\n[ \t]*\n" nil "move")
		  (re-search-backward "\n[ \t]*\n")))
	 (when (eq p (point))
		(re-search-forward "\n[ \t]*\n" nil "move")
		(when (re-search-forward "\n[ \t]*\n" nil "move")
		  (re-search-backward "\n[ \t]*\n")))))


(defun wakib-new-empty-buffer ()
  "Create a new empty buffer and switch to it.
New buffer will be named “untitled” or “untitled<2>”, etc.
It returns the buffer (for elisp programing)."
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
	 (switch-to-buffer $buf)
	 (funcall initial-major-mode)
	 (setq buffer-offer-save t)
	 $buf))

(defun wakib-insert-newline-before ()
  "Insert a newline and indent before current line"
  (interactive)
  (move-beginning-of-line)
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command))


(defun wakib-close-current-buffer ()
  "kill current buffer"
  (interactive)
  (kill-buffer (current-buffer)))


(defun wakib-beginning-of-line-or-block ()
  "Move cursor to beginning of line or previous paragraph.
• When called first time, move cursor to beginning of char in current line. (if already, move to beginning of line.)
• When called again, move cursor backward by jumping over any sequence of whitespaces containing 2 blank lines."
  (interactive)
  (let (($p (point)))
    (if (or (equal (point) (line-beginning-position))
            (equal last-command this-command ))
        (if (re-search-backward "\n[\t\n ]*\n+" nil "move")
            (progn
              (skip-chars-backward "\n\t ")
              ;; (forward-char )
              )
          (goto-char (point-min)))
      (progn
        (back-to-indentation)
        (when (eq $p (point))
          (beginning-of-line))))))

(defun wakib-end-of-line-or-block ()
  "Move cursor to end of line or next paragraph.
• When called first time, move cursor to end of line.
• When called again, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines."
  (interactive)
  (if (or (equal (point) (line-end-position))
          (equal last-command this-command ))
      (progn
        (re-search-forward "\n[\t\n ]*\n+" nil "move" ))
    (end-of-line)))


;; Setup for keymap
;; I should probably move this into a let statement,
;; no need expose any of these functions or variables
;; except for wakib-mode-map

(defvar wakib-mode-map (make-sparse-keymap) "Key bindings for Wakib minor mode")

(defun wakib-define-keys (keymap keylist)
  "Map over alist (keylist) and add to keymap"
  (interactive)
  (mapc (lambda (pair)
          (define-key keymap (kbd (car pair)) (cdr pair)))
        keylist))

(defvar wakib-keylist
  '(("M-j" . backward-char)
	 ("M-l" . forward-char)
	 ("M-i" . previous-line)
	 ("M-k" . next-line)
	 ("M-u" . backward-word)
	 ("M-o" . forward-word)
	 ("M-U" . wakib-beginning-line-or-block)
	 ("M-O" . wakib-end-line-or-block)
	 ("M-I" . scroll-down)
	 ("M-K" . scroll-up)
	 ("C-n" . wakib-new-empty-buffer)
	 ("C-o" . find-file)
	 ("C-w" . wakib-close-current-buffer)
	 ("C-<next>" . next-buffer)
	 ("C-<prior>" . previous-buffer)
	 ("C-v" . yank)
	 ("C-z" . undo)
	 ("C-f" . isearch-forward)
	 ("C-S-f" . isearch-backward)
	 ("C-r" . query-replace)
	 ("C-S-r" . query-replace-regexp)
	 ("C-s" . save-buffer)
	 ("C-p" . print-buffer)
	 ("C-a" . wakib-select-line-block-all)
	 ("C-+" . text-scale-increase)
	 ("C-=" . text-scale-increase)
	 ("C--" . text-scale-decrease)
	 ("C-j" . wakib-previous)
	 ("C-l" . wakib-next)
	 ("M-s" . other-window)
	 ("M-M" . goto-line)
	 ("M-4" . split-window-right)
	 ("M-$" . split-window-below)
	 ("M-3" . delete-other-windows)
	 ("M-2" . delete-window)
	 ("M-e" . backward-kill-word)
	 ("M-r" . kill-word)
	 ("M-a" . execute-extended-command)
	 ("M-<f4>" . save-buffers-kill-terminal)
	 ("M-d" . delete-backward-char)
	 ("M-f" . delete-char)
	 ("M-a" . kill-whole-line)
 	 ("M-SPC" . set-mark-command)
	 ("M-S-SPC" . set-rectangular-region-anchor)
	 ("S-RET" . wakib-insert-newline-before)
	 ("C-b" . switch-to-buffer)
	 ("M-X" . pp-eval-expression)
	 ("<escape>" . keyboard-quit) ;; should quit minibuffer too
	 ("C-S-a" . er/expand-region)
	 ("M-m" . avy-goto-char-2))
  "List of all wakib mode keybindings")


(wakib-define-keys wakib-mode-map wakib-keylist)

(define-key wakib-mode-map (kbd "C-e") (wakib-dynamic-binding "C-x"))
(define-key wakib-mode-map (kbd "C-d") (wakib-dynamic-binding "C-c"))


(defvar wakib-override-mode-map (make-sparse-keymap) "Keybinding for override keys")
(define-key wakib-override-mode-map (kbd "C-c") 'kill-ring-save)
(define-key wakib-override-mode-map (kbd "C-x") 'kill-region)
(defun wakib-override ()
  "Add modemap to override C-c into minor-mode-overriding-map-alist"
  (interactive)
  (add-to-list 'minor-mode-overriding-map-alist (cons 'wakib-override-mode wakib-override-mode-map)))
(add-hook 'after-change-major-mode-hook 'wakib-override)


;; Remove overrides on mode exit
(defun wakib-update-override ()
  (setq wakib-override-mode wakib-mode))
(add-hook 'wakib-mode-hook 'wakib-update-override)


;; Modifying other modules
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-l") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-j") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)


(define-minor-mode wakib-mode
  "This mode brings modern style keybindings to Emacs.
Major changes is proper CUA key bindings by moving C-c and C-x to
C-d and C-e respectively. This allow access to all the keybindings of
Emacs while not tripping up users who do not want a steep learning curve
just to use their editor.

Note that only the first prefix is changed. So C-c C-c becomes C-d C-c."
  :lighter " Wakib"
  :keymap wakib-mode-map
  :init-value t)



(provide 'wakib-mode)


;;; wakib-mode.el ends here
