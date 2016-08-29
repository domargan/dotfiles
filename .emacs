;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Package managment ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Auto-install packages by default on all machines
(defvar domargan-packages
  '(
    zenburn-theme
    smart-mode-line
    windresize
    fill-column-indicator
    erc
    auto-complete
    wc-mode
    auctex
    go-mode
    go-eldoc
    go-autocomplete)
  "Install all the packages!")

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package domargan-packages)
  (unless (package-installed-p package)
    (package-install package)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Themes, fonts, and layout ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Mono" :foundry "unknown" :slant normal :weight normal :height 158 :width normal)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("e87a2bd5abc8448f8676365692e908b709b93f2d3869c42a4371223aab7d9cf8" default))))

(load-theme 'zenburn t)

;; Display 80 chars indicator
(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)
(setq fci-rule-width 5)

;; Remove splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Indentation settings
(setq-default indent-tabs-mode t)
(setq-default tab-width 8)

;; Remove menu bar & toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Remove scrollbar
(toggle-scroll-bar -1)

;; Mode line
(require 'smart-mode-line)
(setf rm-blacklist "")
(sml/setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Buffers ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Credits: https://unix.stackexchange.com/questions/19874/prevent-unwanted-buffers-from-opening/152151#152151

;; Enable easier cycling through buffers
(ido-mode 1)
(setq ido-separator "\n")
(setq ido-ignore-buffers '("\\` " "^\*"))

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
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time
(add-hook 'window-setup-hook 'delete-other-windows)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Other features ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Open recent files menu entry
(recentf-mode)

;; Restore last session on startup
(desktop-save-mode 1)

;; Enable fullscreen toggle on X11 with F11
(defun toggle-fullscreen ()
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))
(global-set-key [f11] 'toggle-fullscreen)

;; Enable visual line mode by default
(global-visual-line-mode t)
(global-linum-mode t)

;; Clean whitespaces on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Type y or n instead yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Navigate through windows using S-<Arrow>
(windmove-default-keybindings)

;; Resize windows easier
(require 'windresize)

;; Disable C-z
(global-unset-key "\^z")

;; Disable Insert key
(put 'overwrite-mode 'disabled t)

;; Wordcount
(require 'wc-mode)
(wc-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Terminal Emulator ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Alias
(defalias 'sh 'ansi-term)

;; Set default shell
(setq explicit-shell-file-name "/bin/bash")

;; Close terminal on exit
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
	ad-do-it
	(kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Paste into terminal
(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))


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
(setq erc-nickserv-passwords
      `((freenode
	 (("domargan" . ,freenode-domargan-pass)))))

;; Rename server buffers
(setq erc-rename-buffers t)

;; Hide join, part, and quit messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Recconect in the background
; (setq erc-join-buffer 'bury)

;; Autojoin channels
; (setq erc-autojoin-channels-alist '(("freenode.net" "#hulk-ri" "#linux.hr" "#go-nuts")))
; (erc :server "irc.freenode.net" :port 6667 :nick "domargan")

;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)

;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)

;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

;; utf-8 support
(setq erc-server-coding-system '(utf-8 . utf-8))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; LaTeX essentials ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic configuration
(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook
	  (lambda () (LaTeX-fold-mode 1)))

;; Syntaxchecking
(require 'flymake)
(defun flymake-get-tex-args (file-name)
(list "pdflatex"
(list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))
(add-hook 'LaTeX-mode-hook 'flymake-mode)

;; Spellchecking
(setq ispell-dictionary "english")
(setq ispell-program-name "aspell")
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

;; RefTeX
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq LaTeX-eqnarray-label "eq"
LaTeX-equation-label "eq"
LaTeX-figure-label "fig"
LaTeX-table-label "tab"
LaTeX-myChapter-label "chap"
TeX-auto-save t
TeX-newline-function 'reindent-then-newline-and-indent
TeX-parse-self t
TeX-style-path
'("style/" "auto/"
"/usr/share/emacs24/site-lisp/auctex/style/"
"/var/lib/auctex/emacs24/"
"/usr/local/share/emacs/site-lisp/auctex/style/")
LaTeX-section-hook
'(LaTeX-section-heading
LaTeX-section-title
LaTeX-section-toc
LaTeX-section-section
LaTeX-section-label))

;; External viewers
(setq TeX-output-view-style
      (quote
       (("^pdf$" "." "evince -f %o")
	("^html?$" "." "iceweasel %o"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Go programming essentials ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Credits: http://arenzana.org/2015/Emacs-for-Go/

;; Load Go-specific language syntax
(defun go-mode-setup ()
  (go-eldoc-setup))
(add-hook 'go-mode-hook 'go-mode-setup)

;; Format with fmt before saving
(defun go-mode-setup ()
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'go-mode-setup)

;; Take care of imports
(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'go-mode-setup)

;; Show function definition when calling godef-jump
(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'go-mode-setup)

;; Custom Compile Command
(defun go-mode-setup ()
  (setq compile-command "go build -v -race && go test -v -race && go vet && golint && errcheck")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'go-mode-setup)

;; Load auto-complete
(ac-config-default)
(require 'auto-complete-config)
(require 'go-autocomplete)

;; Run golint with M-x golint
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)
