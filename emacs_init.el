;; init.el --- Emacs configuration

; ============================
;          PACKAGES
; ============================

;; connect to melpa package repo
(require 'package)
(add-to-list 'package-archives
       '("melpa" . "http://stable.melpa.org/packages/"))

;; connect to elpa package repo
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; not sure
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

9; ============================
;        CUSTOM FACES
; ============================

;; custom set faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "OpenDyslexicMono" :foundry "UKWN" :slant normal :weight normal :height 98 :width normal))))
 '(font-latex-math-face ((t (:foreground "rosy brown"))))
 '(font-latex-sectioning-0-face ((t (:foreground "gold"))))
 '(font-latex-sectioning-1-face ((t (:foreground "gold"))))
 '(font-latex-sectioning-2-face ((t (:foreground "gold"))))
 '(font-latex-sectioning-3-face ((t nil)))
 '(font-latex-sectioning-4-face ((t (:foreground "gold"))))
 '(font-latex-sectioning-5-face ((t (:foreground "gold"))))
 '(font-latex-sedate-face ((t (:foreground "deep sky blue"))))
 '(font-latex-subscript-face ((t nil)))
 '(font-latex-superscript-face ((t nil))))

(tool-bar-mode -1)

; =============================
;      KEYBOARD SHORTCUTS
; =============================

;; multiple cursors
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

;; truncating lines
(global-set-key (kbd "\C-c t") 'toggle-truncate-lines)

;; jump to files
(set-register ?e (cons 'file "~/.emacs.d/init.el"))
(set-register ?d (cons 'file "~/Dropbox/org/daniel.org"))

; =============================
;       CUSTOM SETTINGS
; =============================

;; put buffer name in title bar
(setq frame-title-format "%b")

;; line length
(setq-default fill-column 60)
                                                                
;; type over selected text
(delete-selection-mode 1) 

;; line numbers
;(global-linum-mode t)
;(setq linum-format "%4d \u2502 ")
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;(custom-set-variables
; ;; custom-set-variables was added by Custom.
; ;; If you edit it by hand, you could mess it up, so be careful.
; ;; Your init file should contain only one such instance.
; ;; If there is more than one, they won't work right.
; '(ansi-color-names-vector
;   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
; '(custom-enabled-themes nil)
; '(custom-safe-themes
;   (quote
;    ("4561c67b0764aa6343d710bb0a6f3a96319252b2169d371802cc94adfea5cfc9" default)))
; '(font-latex-fontify-script nil t)
; '(fringe-mode 0 nil (fringe))
; '(indicate-empty-lines t)
; '(line-number-mode nil)
; '(org-agenda-files
;   (quote
;    ("~/Dropbox/org/daniel.org" "~/Dropbox/org/research.org" "~/Dropbox/org/act2019.org")))
; '(org-file-apps
;   (quote
;    ((auto-mode . emacs)
;     ("\\.pdf\\'" . "evince %s")
;     ("\\.x?html?\\'" . default)
;     ("\\.pdf\\'" . default))))
; '(org-refile-targets (quote ((org-agenda-files :maxlevel . 1))))
; '(package-selected-packages
;   (quote
;    (multiple-cursors auctex org material-theme markdown-mode elpy better-defaults)))
; '(reftex-insert-label-flags (quote (nil t)))
; '(scroll-bar-mode nil)
; '(tool-bar-mode nil)
; '(tool-bar-position (quote right))
; '(tooltip-mode nil))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;; adds dir to load path
 (add-to-list 'load-path "~/.emacs.d/lisp/") 

;; add /lisp and subdirectories to load-path. Place all .el's here
(let ((default-directory (expand-file-name "~/.emacs.d/lisp/")))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path)))
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; load color theme
;(require 'weyland-yutani-theme)		
(load-theme 'zenburn t)

;; Proper line wrapping
(global-visual-line-mode t) 

;; Matches parentheses and such in every mode
; (show-paren-mode 1)

;; Disable fringe because I use visual-line-mode
(set-fringe-mode '(0 . 0)) 

;; Default frame height.
(add-to-list 'default-frame-alist '(height . 59))

;; makes latext-mode default
(setq-default major-mode 'LaTeX-mode)

;; autocomplete mode
(add-hook 'text-mode-hook 'auto-fill-mode)

;; hide the startup message
(setq inhibit-startup-message t) 

; =============================
;        AUCTEX
; =============================

;; fontify
(setq font-latex-fontify-script nil) ; no sup/subscripts in tex file

;; path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2018/bin/x86_64-linux/"))  
(setq exec-path (append exec-path '("/usr/local/texlive/2018/bin/x86_64-linux/")))

;; parsing
(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.

;; set up RefTex
;(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

;; turn on folding	
(add-hook 'LaTeX-mode-hook 
	(lambda ()
	  (TeX-fold-mode 1)))

; ===========================
;         YAS SNIPPET
; ===========================

(require 'yasnippet)
(yas-global-mode t) 

;; yas snippets embedded in snippets
(setq yas-triggers-in-field t)

; ===========================
;       ORG MODE
; ==========================

(require 'org)
(require 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; capture. types are in notes.org and C-c c is the key
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/Dropbox/org/refile.org")

;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)

;; Define the custum capture templates
(setq org-capture-templates
      '(("e" "entry" entry
	 (file+headline org-default-notes-file "_notebook_")
	 "* %? ")
	("m" "link this buffer"
	 entry(file+headline org-default-notes-file "_notebook_")
	 "* %? \n%a\n") ))

;; Tells agenda which org-files to pull from
(setq org-agenda-files
    (file-expand-wildcards "~/Dropbox/org/*.org"))

;; Include the todo keywords
;; (setq org-fast-tag-selection-include-todo t)

;; list of 'todo' states. Those to the left of the
;; bar are considered unfinished and those to the
;; right of the bar are considered completed.
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "WAITING(w@)" "DONE(d@)")))

;; Custom colors for the keywords
;;(setq org-use-fast-todo-selection t)
(setq org-todo-keyword-faces
      '(("TODO" :foreground "pink" :weight bold)
	("NEXT" :foreground "light blue" :weight bold)
	("DONE" :foreground "green" :weight bold)
	("WAITING" :foreground "magenta" :weight bold)
	("INACTIVE" :foreground "magenta" :weight bold)
	("CANCELLED" :foreground "forest green" :weight bold)
	("MEETING" :foreground "forest green" :weight bold)))

;; Auto-update tags whenever the state is changed
;;(setq org-todo-state-tags-triggers
;;      '(("CANCELLED" ("CANCELLED" . t))
;;	("WAITING" ("WAITING" . t))
;;	("INACTIVE" ("WAITING") ("INACTIVE" . t))
;;	(done ("WAITING") ("INACTIVE"))
;;	("TODO" ("WAITING") ("CANCELLED") ("INACTIVE"))
;;	("NEXT" ("WAITING") ("CANCELLED") ("INACTIVE"))
;;	("DONE" ("WAITING") ("CANCELLED") ("INACTIVE"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/calendar.org" "~/Dropbox/org/daniel.org" "~/Dropbox/org/personal.org" "~/Dropbox/org/refile.org" "~/Dropbox/org/research.org" "~/Dropbox/org/teach.org")))
 '(package-selected-packages
   (quote
    (haskell-mode zenburn-theme yasnippet multiple-cursors auctex))))
