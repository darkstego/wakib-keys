;; Functions & Macros

(cl-defmacro wakib-cc-key ()
  "Act as C-c definition in the current context.
This uses an extended menu item's capability of dynamically computing a
definition. This idea came from general.el"
  `'(menu-item
     ,""
     nil
     :filter
     (lambda (&optional _)
		 ,`(wakib-get-all-keymaps))))


;; use let instead of double call to (car x)
(defun wakib-minor-cc-keymaps()
  (let ((active-maps nil))
	 (mapc (lambda (x) 
				(when (and (symbolp (car x)) (symbol-value (car x)))
				  (add-to-list 'active-maps  (lookup-key (cdr x) (kbd "C-c")))))
			 minor-mode-map-alist )
	 (make-composed-keymap active-maps)))



;; might need to do keymap inheretence to perserve priority
(defun wakib-get-all-keymaps ()
  (make-composed-keymap (list (wakib-minor-cc-keymaps) (local-key-binding (kbd "C-c")) (global-key-binding (kbd "C-c")))))


;; Commands

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


;; Remove overrides on mode exit
(defun wakib-update-cc-override ()
  (setq wakib-cc-mode wakib-mode))
(add-hook 'wakib-mode-hook 'wakib-update-cc-override)


(defvar wakib-cc-mode-map (make-sparse-keymap))
(define-key wakib-cc-mode-map (kbd "C-c") 'kill-ring-save)
(defun wakib-cc-override ()
  "Add modemap to override C-c into minor-mode-overriding-map-alist"
  (interactive)
  (add-to-list 'minor-mode-overriding-map-alist (cons 'wakib-cc-mode wakib-cc-mode-map)))
(add-hook 'after-change-major-mode-hook 'wakib-cc-override)


(defvar wakib-keylist
  '(("M-j" . backward-char)
	 ("M-l" . forward-char)
	 ("M-i" . previous-line)
	 ("M-k" . next-line)
	 ("M-u" . backward-word)
	 ("M-o" . forward-word)
	 ("M-U" . wakib-beginning-of-line-or-block)
	 ("M-O" . wakib-end-of-line-or-block)
	 ("M-I" . scroll-down)
	 ("M-K" . scroll-up)
	 ("C-n" . wakib-new-empty-buffer)
	 ("C-o" . find-file)
	 ("C-w" . wakib-close-current-buffer)
	 ("C-<next>" . next-buffer)
	 ("C-<prior>" . previous-buffer) 
	 ("C-x" . kill-region)
	 ("C-v" . yank)
	 ("C-z" . undo)
	 ("C-f" . isearch-forward)
	 ("C-F" . isearch-backward)
	 ("C-s" . save-buffer)
	 ("C-p" . print-buffer)
	 ("C-=" . text-scale-increase)
	 ("C--" . text-scale-decrease)
	 ("M-e" . backward-kill-word)
	 ("M-r" . kill-word)
	 ("M-a" . execute-extended-command)
	 ("M-<f4>" . save-buffers-kill-terminal)
	 ("M-d" . delete-backward-char)
	 ("M-f" . delete-char)
	 ("M-SPC" . set-mark-command)
	 ("M-RET" . wakib-insert-newline-before)
	 ("C-b" . switch-to-buffer)
	 ("<escape>" . keyboard-escape-quit)) ;; doesn't work well, check ergoemacs
  "List of all wakib mode keybindings")
  
  
(wakib-define-keys wakib-mode-map wakib-keylist)

(define-key wakib-mode-map (kbd "C-e") ctl-x-map)
(define-key wakib-mode-map (kbd "C-d") (wakib-cc-key))


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
