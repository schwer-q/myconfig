;; Load path
(add-to-list 'load-path "~/.emacs.d/lisp")

;; pakage repository
(require 'package)
(setq package-archives '(("gnu"	      . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa"     . "https://melpa.milkbox.net/packages/")))
(package-initialize)

(setq c-default-style "bsd")

;; Highlight lines with more than 80 columns
(setq column-enforce-comments nil)
(setq column-enforce-column 80)

;; vala
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/vala-mode-0.1"))
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))

;; c#
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; ;; flymake for python
;; (setq pylint "epylint")
;; (when (load "flymake" t)
;;   (defun flymake-pylint-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		       'flymake-create-temp-inplace))
;; 	   (local-file (file-relative-name
;; 			temp-file
;; 			(file-name-directory buffer-file-name))))
;;       (list (expand-file-name pylint "") (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;; 	       '("\\.py\\'" flymake-pylint-init)))

;; (add-hook 'python-mode-hook '(lambda () (flymake-mode)))

;; git
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/git-commit-mode-0.12"))
;; (require 'git-commit)
;; (add-hook 'git-commit-mode-hook 'turn-on-flyspell)
;; (add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0)))

;; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Show column/line number
(setq column-number-mode t)
(setq line-number-mode t)

;; Show paren
;; (setq show-paren-mode t)
(show-paren-mode 1)

;; Comfirm before kill emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; Disable insert key
(global-set-key [insert] (lambda () (interactive)))
(global-set-key [insertchar] (lambda () (interactive)))

;; Clock
(setq display-time-and-date t
      display-time-24hr-format t)
(display-time)
