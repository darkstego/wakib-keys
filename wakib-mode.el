;;; wakib-mode.el --- Minor Mode for Modern Keybindings -*- lexical-binding: t -*-

;; Author: Abdulla Bubshait
;; URL: https://github.com/darkstego/wakib-mode
;; Created: 6 April 2018
;; Keywords: convenience, keybindings, keys
;; License: GPL v3
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.3.10

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

;; This package aims to provide new keybindings for basic Emacs
;; functions.  The goal of this package is to provide

;;; Code:


;; Functions & Macros


(defun wakib-dynamic-binding (key)
  "Act as KEY in the current context.
This uses an extended menu item's capability of dynamically computing a
definition.  This idea came from general.el"
  `(menu-item
	 ,""
	 nil
	 :filter
	 ,(lambda (&optional _)
		 (wakib-key-binding key))))


;; should probably use let instead of double call to (car x)
(defun wakib-minor-mode-key-binding (key)
  "Function return all keymaps defind to KEY within minor modes.
This function ignores the overriding maps that will be used to override
KEY"
  (let ((active-maps nil))
	 (mapc (lambda (x)
				(when (and (symbolp (car x)) (symbol-value (car x)))
				  (add-to-list 'active-maps  (lookup-key (cdr x) (kbd key)))))
			 minor-mode-map-alist )
	 (make-composed-keymap active-maps)))


;; might need to do keymap inheretence to perserve priority
(defun wakib-key-binding (key)
  "Return the full keymap bindings of KEY."
  (make-composed-keymap (list (wakib-minor-mode-key-binding key) (local-key-binding (kbd key)) (global-key-binding (kbd key)))))

;; Commands

(defun wakib-previous (&optional arg)
  "Perform context aware Previous function.
ARG used as repeat function for interactive"
  (interactive "p")
  (cond ((eq last-command 'yank)
			(yank-pop arg))
		  ))

(defun wakib-next (&optional arg)
  "Perform context aware Next function.
ARG used as repeat for interactive function."
  (interactive "p")
  (cond ((eq last-command 'yank)
			(yank-pop (- arg)))))



;; might be a more functional way to do this
(defun wakib-select-line-block-all ()
  "Select line.  Expands to block and then entire buffer."
  (interactive)
  (unless (mark) (set-mark (point)))
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
	   (goto-char (point-min))
	   (setq x1 (point))
	   (push-mark x1 t t)
	   (goto-char (point-max))
	   (setq x2 (point)))
	 (when (not end-p)
	   (push-mark x2 t t)
	   (goto-char x1))))

(defun wakib-back-to-indentation-or-beginning ()
  "Move to start of text or start of line."
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))


(defun wakib-beginning-line-or-block ()
  "Move to the beginning of line, if there then move to beginning of block."
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
  "Move to the end of line, if there then move to end of block."
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
It returns the buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (switch-to-buffer buffer)
    (setq buffer-offer-save t)
    buffer))

(defun wakib-insert-newline-before ()
  "Insert a newline and indent before current line."
  (interactive)
  (move-beginning-of-line 1)
  (newline-and-indent)
  (forward-line -1)
  (indent-for-tab-command))


(defun wakib-beginning-of-line-or-block ()
  "Move cursor to beginning of line or previous paragraph."
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
  "Move cursor to end of line or next paragraph."
  (interactive)
  (if (or (equal (point) (line-end-position))
          (equal last-command this-command ))
      (progn
        (re-search-forward "\n[\t\n ]*\n+" nil "move" ))
    (end-of-line)))

(defun wakib-backward-kill-line ()
  "Kill from cursor to start of line."
  (interactive)
  (kill-line 0))

;; Setup for keymap
;; I should probably move this into a let statement,
;; no need expose any of these functions or variables
;; except for wakib-mode-map

(defvar wakib-mode-map (make-sparse-keymap) "Key bindings for Wakib minor mode.")

(defun wakib-define-keys (keymap keylist)
  "Add to KEYMAP all keys in KEYLIST.  
Then add C-d and C-e to KEYMAP"
  (interactive)
  (mapc (lambda (pair)
          (define-key keymap (kbd (car pair)) (cdr pair)))
        keylist)
  (define-key keymap (kbd "C-e") (wakib-dynamic-binding "C-x"))
  (define-key keymap (kbd "C-d") (wakib-dynamic-binding "C-c")))

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
    ("M-n" . beginning-of-buffer)
    ("M-N" . end-of-buffer)
    ("C-n" . wakib-new-empty-buffer)
    ("C-o" . find-file)
    ("C-S-o" . revert-buffer)
    ("C-w" . kill-this-buffer)
    ("C-q" . save-buffers-kill-terminal)
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
    ("M-S-e" . wakib-backward-kill-line)
    ("M-S-r" . kill-line)
    ("M-w" . kill-whole-line)
    ("M-a" . execute-extended-command)
    ("M-<f4>" . save-buffers-kill-emacs)
    ("M-d" . delete-backward-char)
    ("M-f" . delete-char)
    ("M-a" . wakib-select-line-block-all)
    ("M-SPC" . set-mark-command)
    ("M-S-SPC" . set-rectangular-region-anchor)
    ("S-RET" . wakib-insert-newline-before)
    ("C-b" . switch-to-buffer)
    ("M-X" . pp-eval-expression)
    ("<escape>" . keyboard-quit) ;; should quit minibuffer too
    ("M-m" . goto-line))
  "List of all wakib mode keybindings.")


(wakib-define-keys wakib-mode-map wakib-keylist)

(defvar wakib-override-mode nil)
(defvar wakib-override-mode-map (make-sparse-keymap) "Keybinding for override keys.")
(define-key wakib-override-mode-map (kbd "C-c") 'kill-ring-save)
(define-key wakib-override-mode-map (kbd "C-x") 'kill-region)
(add-to-list 'emulation-mode-map-alists
	     `((wakib-override-mode . ,wakib-override-mode-map)))


(defun wakib-global-mode ()
  "Enable wakib bindings globally. This cannot be disabled with negative argument"
  (interactive)
  (wakib-define-keys (current-global-map) wakib-keylist)
  (setq wakib-override-mode t))


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
  :init-value nil
  :global t
  (setq wakib-override-mode wakib-mode))

(provide 'wakib-mode)


;;; wakib-mode.el ends here
