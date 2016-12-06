;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               GNU Emacs config              ;;
;;            Author: Domagoj Margan           ;;
;;            Email: dm@domargan.net           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Package managment ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") 1)
(package-initialize)

;; Auto-install packages by default on all machines
(defvar domargan-packages
  '(auctex ace-jump-mode auto-complete auto-package-update autopair beacon direx drag-stuff
	   elisp-format erc fill-column-indicator flx-ido flycheck go-autocomplete go-direx go-eldoc
	   go-mode google-this highlight-escape-sequences ido ido-ubiquitous ido-vertical-mode
	   indent-guide interleave smart-mode-line smex symon synosaurus org undo-tree
	   visual-regexp-steroids wc-mode windresize zenburn-theme zoom-window)
  "Install all the packages!")

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package domargan-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Automatically update packages
(auto-package-update-maybe)

;; Manual package loading
(add-to-list 'load-path "~/.emacs.d/lisp/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; General ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Restore last session on startup
(desktop-save-mode 1)

;; Enable fullscreen toggle on X11 with F11
(defun toggle-fullscreen ()
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter nil 'fullscreen
			 (when (not (frame-parameter nil 'fullscreen))
			   'fullboth))))
(global-set-key [f11] 'toggle-fullscreen)

;; Increase or decrease font in GUI with mouse scroll
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

;; Increase or decrease font with C-+/-
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Disable C-z
(global-unset-key "\^z")

;; Disable Insert key
(put 'overwrite-mode 'disabled 1)

;; Clean whitespaces on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Type y or n instead yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Indentation settings
(setq-default indent-tabs-mode 1)
(setq-default tab-width 8)

;; Enable global syntax checking
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Spellchecking settings
(setq ispell-dictionary "english")
(setq ispell-program-name "aspell")

;; Enable auto-complete
(ac-config-default)
(require 'auto-complete-config)

;; Close brackets automatically
(autopair-global-mode 1)

;; Don't ask for confirmation for region case conversion
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Set SSH as default remote work protocol
(setq tramp-default-method "ssh")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Themes and fonts ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t
	     (:family "Hack"
		      :slant normal
		      :weight normal
		      :height 123
		      :width normal
		      :foundry "simp")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("e87a2bd5abc8448f8676365692e908b709b93f2d3869c42a4371223aab7d9cf8"
			      default))))

(load-theme 'zenburn 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Interface and layout modifications ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Open recent files menu entry
(recentf-mode 1)

;; Mode line
(setf rm-blacklist "")
(sml/setup)

;; Display which line and column the cursor is currently on
(line-number-mode 1)
(column-number-mode 1)

;; Display file size on mode line
(size-indication-mode 1)

;; Display time
(setq display-time-default-load-average nil)
(setq display-time-24hr-format 1)
(display-time-mode 1)

;; Display system monitor
(setq symon-refresh-rate 1)
(symon-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Visual Helpers ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Highlight cursor on every scroll
(beacon-mode 1)

;; Highlight escape sequences
(hes-mode 1)

;; See matching pairs of parentheses
(show-paren-mode 1)

;; Enable indentation guide
(indent-guide-global-mode 1)

;; Display 80 chars indicator
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-width 5)
(setq fci-rule-column 80)

;; Enable visual line mode by default
(global-visual-line-mode 1)
(global-linum-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Navigation and user interaction ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Move the cursor with ace-jump-mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Use visual-regexp instead of the built-in isearch
(define-key global-map (kbd "C-r") 'vr/isearch-backward)
(define-key global-map (kbd "C-s") 'vr/isearch-forward)
;; Use visual-regexp for search-and-replace
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

;; Enable and setup IDO everywhere
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(ido-vertical-mode 1)
(flx-ido-mode 1)
(setq ido-separator "\n")
(setq ido-ignore-buffers '("\\` " "^\*"))

;; Enable smex
(global-set-key (kbd "M-x") 'smex)

;; Enable projectile
(projectile-global-mode 1)

;; Enable directory tree navigation
(global-set-key (kbd "C-x C-j") 'direx-project:jump-to-project-root)

;; Enable navigation in the same buffer with dired
(put 'dired-find-alternate-file 'disabled nil)

;; Credits: https://unix.stackexchange.com/questions/19874/prevent-unwanted-buffers-from-opening/152151#152151
;; Remove *messages* buffer
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* buffer
(add-hook 'minibuffer-exit-hook
	  '(lambda ()
	     (let ((buffer "*Completions*"))
	       (and (get-buffer buffer)
		    (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time
(setq inhibit-startup-buffer-menu 1)

;; Show only one active window when opening multiple files at the same time
(add-hook 'window-setup-hook 'delete-other-windows)

;; Navigate through windows using S-<Arrow>
(windmove-default-keybindings)

;; Resize windows easier
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Zoom window with C-x C-z
(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
(setq zoom-window-mode-line-color "DarkGreen")

;; Enable undoing window configurations
(winner-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Terminal Emulator ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Alias
(defalias 'sh 'ansi-term)

;; Set default shell
(setq explicit-shell-file-name "/bin/bash")

;; Close terminal on exit
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc)
	    '(signal
	      exit))
      (let ((buffer (process-buffer proc)))
	ad-do-it
	(kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Paste into terminal
(eval-after-load "term" '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))


;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; IRC ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;

;; Name and email
(setq erc-user-full-name "Domagoj Margan")
(setq erc-email-userid "dm@domargan.net")

;; Auto identification
(load "~/.ercpass")
(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords `((freenode (("domargan" . ,freenode-domargan-pass)))))

;; Rename server buffers
(setq erc-rename-buffers 1)

;; Hide join, part, and quit messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Recconect in the background
;; (setq erc-join-buffer 'bury)

;; Autojoin channels
;; (setq erc-autojoin-channels-alist '(("freenode.net" "#hulk-ri" "#linux.hr" "#go-nuts")))
;; (erc :server "irc.freenode.net" :port 6667 :nick "domargan")

;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part 1)

;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit 1)

;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit 1)

;; utf-8 support
(setq erc-server-coding-system '(utf-8 . utf-8))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Org Mode ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make org-mode the default mode for .org, .org_archive, and .txt files
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; Define key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Use ido completion
(setq org-completion-use-ido 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Additional features ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Undo tree
(global-undo-tree-mode 1)

;; Drag stuff
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; Wordcount
(require 'wc-mode)
(wc-mode 1)

;; Find synonyms with Synosaurus (requires WordNet)
(synosaurus-mode 1)

;; Google this
(google-this-mode 1)

;; Pomodoro
(load "pomodoro")
(require 'pomodoro)
(pomodoro-add-to-mode-line)

;; Elisp Formatter
(require 'elisp-format)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; LaTeX essentials ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic configuration
(require 'tex-site)
(setq TeX-auto-save 1)
(setq TeX-parse-self 1)
(setq TeX-save-query nil)
(setq-default TeX-master nil)
(setq TeX-PDF-mode 1)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (LaTeX-fold-mode 1)))

;; Enable spellchecking in .tex docs
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

;; External viewers
(setq TeX-output-view-style (quote (("^pdf$" "." "evince -f %o")
				    ("^html?$" "." "firefox %o"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Go programming essentials ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Credits: http://arenzana.org/2015/Emacs-for-Go/

;; Prerequisites:
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u github.com/golang/lint/golint
;; go get -u github.com/kisielk/errcheck
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/nsf/gocode
;; go get -u github.com/dougm/goflymake

(defun go-mode-setup ()
  ;; Custom Compile Command
  (setq compile-command "go build -v -race && go test -v -race && go vet && golint && errcheck")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  ;; Take care of imports
  (setq gofmt-command "goimports")
  ;; Format with fmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Provide eldoc for Go
  (go-eldoc-setup)
  ;; Show function definition when calling godef-jump
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'go-mode-setup)

;; Load auto-complete for Go files (with gocode)
(require 'go-autocomplete)

;; Run golint with M-x golint
(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

;; Check syntax with flycheck
(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake"))
(require 'go-flycheck)
