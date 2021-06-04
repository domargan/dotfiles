;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               domargan's GNU Emacs config              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -*- mode: emacs-lisp; -*-

;; Me
(setq user-full-name "Domagoj Margan")
(setq user-mail-address "dm@domargan.net")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Package managment ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Refresh contents if errors about missing packages (known Emacs bug):
;; M-x package-refresh-contents
;; and then restart.

;; Fetch from local MELPA mirrors, see:
;; https://github.com/d12frosted/elpa-mirror
;; https://github.com/ninrod/emacs-antiproxy
;; (setq package-archives
;;	  `(("melpa" . "elpa-mirror-master/melpa/")
;;		("org"   . "elpa-mirror-master/org/")
;;		("gnu"   . "elpa-mirror-master/gnu/")))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Auto-install packages by default on all machines
(defvar domargan-packages
  '(ace-jump-mode auto-complete auto-package-update beacon diff-hl direx
		  drag-stuff elisp-format exec-path-from-shell
		  fill-column-indicator flx flx-ido flycheck
		  flycheck-irony ggtags google-this goto-last-change
		  highlight-escape-sequences ido ido-completing-read+
		  ido-vertical-mode irony modern-cpp-font-lock
		  projectile project-explorer rich-minority smex
		  smooth-scrolling undo-tree visual-regexp-steroids
		  window-purpose windresize zoom-window)
  "Install all the packages!")

;; Fetch the list of packages available
(unless package-archive-contents (package-refresh-contents))

;; Install the missing packages
(dolist (package domargan-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Automatically update packages
(auto-package-update-maybe)

;; Manual package loading
(add-to-list 'load-path "~/.emacs.d/lisp/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Themes and fonts ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'manoj-dark 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; General ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensure environment variables inside Emacs look the same as in my shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Start Emacs server during init unles there is a server already running
(if (and (fboundp 'server-running-p)
	 (not (server-running-p)))
    (server-start))

;; Restore last session on startup
(desktop-save-mode 1)

;; Don't create #autosave# files
(setq auto-save-default nil)

;; Don't create #temp files
(setq create-lockfiles nil)

;; Don't create backup~ files
;;(setq make-backup-files nil)

;; Backup versioned files as well
(setq vc-make-backup-files t)

;; Handle multipe backups
(setq backup-by-copying t delete-old-versions t kept-new-versions 10
      kept-old-versions 10
      version-control t)

;; Save per-save backup files in a separate dir
(setq backup-directory-alist '(("" . "~/backups/per_save")))

;; Make a special "per session" backup at the first save of each emacs session.
(defun force-backup-of-buffer ()
  (when (not buffer-backed-up)
    (let ((backup-directory-alist '(("" . "~/backups/per_session")))
	  (kept-new-versions 3))
      (backup-buffer)))
  (let ((buffer-backed-up nil))
    (backup-buffer)))

;; Remove splash screen
(setq inhibit-splash-screen 1)
(setq inhibit-startup-message 1)

;; Remove menu bar, toolbar, scrollbar
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Credits: https://unix.stackexchange.com/questions/19874/prevent-unwanted-buffers-from-opening/
;; Remove *Messages* buffer
;;(setq-default message-log-max nil)
;;(kill-buffer "*Messages*")

;; Removes *Completions* buffer
(add-hook 'minibuffer-exit-hook '(lambda ()
				   (let ((buffer "*Completions*"))
				     (and (get-buffer buffer)
					  (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time
(setq inhibit-startup-buffer-menu 1)

;; Show only one active window when opening multiple files at the same time
(add-hook 'window-setup-hook 'delete-other-windows)

;; Kill buffer and delete its window at the same time
					;(substitute-key-definition 'kill-buffer 'kill-buffer-and-window global-map)

;; Don't show minor modes on modeline
(rich-minority-mode 1)
(setq rm-whitelist (format "^ \\(%s\\)$" (mapconcat #'identity '("Fly.*") "\\|")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Interface interaction and navigation ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Type y or n instead yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Copy/paste outside Emacs in MacOS
(if (eq system-type 'darwin)
    (defun copy-from-macos ()
      (shell-command-to-string "pbpaste"))
  (defun paste-to-macos (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
	(process-send-string proc text)
	(process-send-eof proc))))
  (setq interprogram-cut-function 'paste-to-macos)
  (setq interprogram-paste-function 'copy-from-macos))

;; Don't ask for confirmation for region case conversion
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Disable C-z
(global-unset-key "\^z")

;; Disable Insert key
(put 'overwrite-mode 'disabled 1)

;; Enable smooth scrolling
(smooth-scrolling-mode 1)

;; Preserve cursor position while scrolling
(setq scroll-preserve-screen-position 1)

;; scroll window up/down by one line
(global-set-key (kbd "M-n")
		(kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p")
		(kbd "C-u 1 M-v"))

;; Highlight cursor on every scroll
(beacon-mode 1)

;; Navigate through windows using S-<Arrow>
(windmove-default-keybindings)

;; Resize windows easier
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Zoom window with C-x C-z
(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
;;(setq zoom-window-mode-line-color "DarkGreen")

;; Enable undoing window configurations
(winner-mode 1)

;; Dedicate purposes to windows by mode and name
(require 'window-purpose)
(purpose-mode 1)

(setq purpose-user-mode-purposes '((term-mode . terminal)
				   (ansi-term-mode . terminal)
				   (prog-mode . coding)
				   (compilation-mode . messages)))

(setq purpose-user-name-purposes '(("*Messages*" . messages)
				   ("*Warnings*" . messages)
				   ("*ggtags-global*" . messages)
				   ("*Flycheck error messages*" . messages)))

(setq purpose-use-default-configuration t)
(purpose-compile-user-configuration)

;; Don't close the window if there are more buffers in the same mode
(require 'window-purpose-x)
(purpose-x-kill-setup)

;; Increase or decrease font size with C-+/-
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Open recent files menu entry
(recentf-mode 1)

;; Enable and setup IDO everywhere
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(flx-ido-mode 1)
(setq ido-separator "\n")
(setq ido-ignore-buffers '("\\` " "^\*"))

;; Enable smex
(global-set-key (kbd "M-x") 'smex)

;; Enable directory tree navigation
(global-set-key (kbd "C-x C-j") 'direx-project:jump-to-project-root)

;; Enable navigation in the same buffer with dired
(put 'dired-find-alternate-file 'disabled nil)

;; Kill all dired buffers at once
(defun kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
	  (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
	    (kill-buffer buffer)))
	(buffer-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Terminal emulator ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Always use bash with ansi-term
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; Kill the term buffer after the terminal is exited
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc)
	    '(signal
	      exit))
      (let ((buffer (process-buffer proc))) ad-do-it (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Loop through all open ansi-terms with F3
;; Credits: https://stackoverflow.com/questions/9983731/emacs-and-ansi-term-elisp-iterate-through-a-list-of-buffers
(global-set-key (kbd "<f3>") 'cycle-ansi-term)
(defun cycle-ansi-term ()
  "cycle through buffers whose major mode is
term-mode"
  (interactive)
  (when (string= "term-mode" major-mode)
    (bury-buffer))
  (let ((buffers (cdr (buffer-list))))
    (while buffers (when (with-current-buffer (car buffers)
			   (string= "term-mode" major-mode))
		     (switch-to-buffer (car buffers))
		     (setq buffers nil))
	   (setq buffers (cdr buffers)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; General file editing ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set SSH as default remote edit protocol
(setq-default tramp-default-method "ssh")

;; Enable visual line mode by default (line-wrap)
;; (global-visual-line-mode 1)

;;Disable line wrap globally
(set-default 'truncate-lines 1)

;; Highlight escape sequences
(hes-mode 1)

;; Move the cursor with ace-jump-mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Go to line
(global-set-key (kbd "\C-cl") 'goto-line)

;; Go to last change
(global-set-key "\C-x\C-\\" 'goto-last-change)

;; Comment regions
(global-set-key (kbd "\C-cx") 'comment-region)
(global-set-key (kbd "\C-xc") 'uncomment-region)

;; Use visual-regexp instead of the built-in isearch
(define-key global-map (kbd "C-r") 'vr/isearch-backward)
(define-key global-map (kbd "C-s") 'vr/isearch-forward)
;; Use visual-regexp for search-and-replace
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

;; Drag stuff
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; Undo tree
(global-undo-tree-mode 1)

;; Clean whitespaces on save
(add-hook 'before-save-hook 'whitespace-cleanup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Coding (general) ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable diff highlighting in margins
(setq-default right-margin-width 2)
(setq-default diff-hl-side 'right)
(setq-default diff-hl-margin-side 'right)
(add-hook 'prog-mode-hook 'diff-hl-margin-mode)
(add-hook 'prog-mode-hook 'diff-hl-flydiff-mode)

;; Enable projectile
(add-hook 'prog-mode-hook 'projectile-mode)

;; Enable tagging with GNU Global for C/C++
;; Requires manual download and compile from https://www.gnu.org/software/global/
(add-hook 'c-mode-common-hook 'ggtags-mode)

;; Enable auto-complete
(ac-config-default)
(require 'auto-complete-config)

;; Advanced auto-complete for C/C++
;; Requires CMake >= 2.8.3 and libclang
;;(add-hook 'c++-mode-hook 'irony-mode)
;;(add-hook 'c-mode-hook 'irony-mode)
;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Enable global syntax checking
(add-hook 'prog-mode-hook 'flycheck-mode)

;; Enchance C/C++ syntax checking
;; Requires CMake >= 2.8.3 and libclang
;;(eval-after-load 'flycheck
;;  '(add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

;; Break lines automatically at spaces when the line becomes too wide
(add-hook 'prog-mode-hook 'auto-fill-mode)

;; Close brackets automatically
;; (add-hook 'prog-mode-hook 'autopair-mode)

;; See matching pairs of parentheses
(add-hook 'prog-mode-hook 'show-paren-mode)

;; Hide/show blocks of code
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Enable line numbers
(add-hook 'prog-mode-hook 'linum-mode)

;; Enable column indicator
(add-hook 'prog-mode-hook 'fci-mode)

;; Enchance sytntax highlighting for modern C++
(add-hook 'c++-mode-hook 'modern-c++-font-lock-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Custom set vars and faces ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(elisp-format zoom-window windresize
					    window-purpose
					    visual-regexp-steroids
					    undo-tree smooth-scrolling
					    smex
					    rich-minority
					    project-explorer
					    projectile
					    modern-cpp-font-lock
					    ido-vertical-mode
					    ido-completing-read+
					    highlight-escape-sequences
					    goto-last-change ggtags
					    flycheck-irony flycheck
					    flx-ido
					    flx fill-column-indicator
					    drag-stuff direx diff-hl
					    beacon google-this
					    auto-package-update
					    auto-complete
					    ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
