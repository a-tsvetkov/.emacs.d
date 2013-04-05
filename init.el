;; Set the debug option to enable a backtrace when a
;; problem occurs.
(setq debug-on-error nil)

(setq current-path (file-name-directory load-file-name))
(setq modules-path (file-name-as-directory (concat current-path "modules")))

(require 'package)
(package-initialize)
;; Add MELPA package archive
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; font configuration
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 100)

;; line and column numbers
(setq column-number-mode t)
(setq line-number-mode t)

;; Ненужные пробелы
;; (setq show-trailing-whitespace t)

;; unused lines
(setq indicate-empty-lines t)

(add-to-list 'load-path modules-path)
(let ((default-directory modules-path))
      (normal-top-level-add-subdirs-to-load-path))


;; setup window if running with x
(defun setup-x-window-frame ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (toggle-scroll-bar nil)

    (if (> (x-display-pixel-width) 1280)
       (add-to-list 'default-frame-alist (cons 'width 190))
       (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
     (cons 'height (/ (- (x-display-pixel-height) 50)
                 (frame-char-height)))))))

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
             (if (equal 'fullboth current-value)
                 (if (boundp 'old-fullscreen) old-fullscreen nil)
               (progn (setq old-fullscreen current-value)
                  'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)
; Make new frames fullscreen by default. Note: this hook doesn't do
; anything to the initial frame if it's in your .emacs, since that file is
; read _after_ the initial frame is created.
(add-hook 'after-make-frame-functions 'toggle-fullscreen)

;; initiate workspace
(defun init-workspace ()
  (global-linum-mode 1)
  (toggle-fullscreen)
  (delete-other-windows)
  (menu-bar-mode 0)
  (split-window-horizontally)
  (enlarge-window-horizontally 3))

(setup-x-window-frame)
(init-workspace)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; initiate and setup php-mode
(require 'php-mode)
(add-to-list 'auto-mode-alist
          '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))

;; (defun wicked/php-mode-init ()
;;   "Set some buffer-local variables."
;;   (setq case-fold-search t)
;;   (c-set-offset 'arglist-cont 0)
;;   (c-set-offset 'arglist-intro '+)
;;   (setq show-paren-mode t)
;;   (c-set-offset 'case-label 4)
;;   (c-set-offset 'substatement-open 0)
;;   (c-set-offset 'arglist-close 0))
;; (add-hook 'php-mode-hook 'wicked/php-mode-init)

;; fill column
(setq fill-column 79)

;; indent with spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; nxml identation
(setq nxml-child-indent 4)

;;Whitespace mode
(require 'whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq whitespace-style
  '(face trailing tabs empty indentation space-after-tab space-before-tab))
(global-whitespace-mode)

;; miscelaneous tweaks
(set-default 'truncate-lines t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; Yasnippet initialize in emacs-for-python
;; (add-to-list 'load-path (concat modules-path "yasnippet"))
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; load magit
(require 'magit)

(add-to-list 'load-path (concat modules-path "mo-git-blame"))
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)
;; load magithub
(add-to-list 'load-path (concat modules-path "magihub"))
(require 'magithub)
(global-set-key (kbd "C-x g") 'magit-status)

(add-to-list 'load-path (concat modules-path "multiple-cursors.el"))
;; Multiple cursors configuration
(require 'multiple-cursors)

(global-set-key (kbd "C-s-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-s->") 'mc/mark-next-word-like-this)
(global-set-key (kbd "C-s-<") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Setup expand region
(add-to-list 'load-path (concat modules-path "expand-region.el"))
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; load pg
(require 'pg)

;; linum teaks
(require 'linum-off)
(add-hook 'linum-before-numbering-hook (lambda() (setq linum-format (lambda (line) (propertize (format (let ((w (length (number-to-string (count-lines (point-min) (point-max)))))) (concat " %" (number-to-string w) "d ")) line) 'face 'linum)))))

(require 'tramp)


(load-file (concat modules-path "emacs-for-python/epy-init.el"))

(require 'epy-setup)      ;; It will setup other loads, it is required!
(require 'epy-python)     ;; If you want the python facilities [optional]
;;(require 'epy-completion) ;; If you want the autocompletion settings [optional]
(require 'epy-editing)    ;; For configurations related to editing [optional]
(require 'epy-bindings)   ;; For my suggested keybindings [optional]
(require 'epy-nose)       ;; For nose integration

(require 'python-pep8)
(require 'python-pylint)

;; Python completion with jedi
(setq jedi:setup-keys t)
(autoload 'jedi:setup "jedi" nil t)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-flake8-maximum-line-length 120)
(setq-default flycheck-highlighting-mode "lines")

(global-set-key (kbd "RET") 'newline-and-indent)

(require 'column-marker)
(add-hook 'php-mode-hook (lambda() (interactive) (column-marker-2 80)))
(add-hook 'python-mode-hook
          (lambda()
            (jedi:setup)
            (column-marker-1 80)
            (flycheck-select-checker python-flake8)
            )
          )
(epy-django-snippets)
(epy-setup-ipython)

;; autopair mode
(require 'autopair)
;; (autopair-global-mode)

(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation)
(set-face-background 'highlight-indent-face "#073642")

(autoload 'django-html-mumamo-mode (concat modules-path "nxhtml/autostart.el"))
(setq auto-mode-alist
      (append '(("\\.html?$" . django-html-mumamo-mode)) auto-mode-alist))
(setq mumamo-background-colors nil)
(add-to-list 'auto-mode-alist '("\\.html$" . django-html-mumamo-mode))

;; Mumamo is making emacs 23.3 freak out:
(when (and (equal emacs-major-version 23)
       (equal emacs-minor-version 3))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
          'font-lock-beginning-of-syntax-function))
  ;; tramp-compat.el clobbers this variable!
  (eval-after-load "tramp-compat"
    '(add-to-list 'byte-compile-not-obsolete-vars
          'font-lock-beginning-of-syntax-function)))

(load-file (concat modules-path "pycoverage.el/pycov2.el"))
(require 'linum)
(require 'pycov2)
(add-hook 'python-mode-hook
      (function (lambda ()
              (pycov2-mode)
              (linum-mode))))

;; Setup pony mode
;; (add-to-list 'load-path (concat modules-path "pony-mode/src"))
;; (require 'pony-mode)

(add-to-list 'load-path (concat modules-path "python-django.el"))
(require 'python-django)
(global-set-key (kbd "C-x j") 'python-django-open-project)

;; Comment spell checking
;; (setq flyspell-issue-welcome-flag nil)
;; (add-hook 'python-mode-hook 'flyspell-prog-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(safe-local-variable-values (quote ((python-shell-interpreter-args . "/home/venya/projects/hrbrand-v2012/manage.py shell") (python-shell-interpreter . "ipython") (python-shell-completion-string-code . "';'.join(get_ipython().Completer.all_completions('''%s'''))
") (python-shell-completion-module-string-code . "';'.join(module_completion('''%s'''))
") (python-shell-interpreter-args . "/home/venya/projects/socaial-network-apps/SocialVacancy/hhsocialvacancy/manage.py shell") (python-shell-completion-string-code . "';'.join(get_ipython().Completer.all_completions('''%s'''))") (python-shell-completion-module-string-code . "';'.join(module_completion('''%s'''))") (python-shell-completion-setup-code . "from IPython.core.completerlib import module_completion") (python-shell-interpreter-args . "/home/venya/projects/career-fair/hhcareeffair/manage.py shell") (python-shell-interpreter . "python")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(load-theme 'solarized-dark)
