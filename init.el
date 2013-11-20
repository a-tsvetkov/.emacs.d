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
;; fetch the list of packages available
(package-refresh-contents)

;; check if the packages is installed; if not, install it.
(defvar required-packages
  '(autopair
    column-marker
    expand-region
    flycheck
    dash
    jedi
    auto-complete
    js2-mode
    magit
    multiple-cursors
    php-mode
    solarized-theme
    yaml-mode
    yasnippet
    scala-mode2
    fullscreen-mode
    web-mode
))

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 required-packages)


;; font configuration
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 100)

;; line and column numbers
(setq column-number-mode t)
(setq line-number-mode t)

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

(require 'fullscreen-mode)
;; Make new frames fullscreen by default. Note: this hook doesn't do
;; anything to the initial frame if it's in your .emacs, since that file is
;; read _after_ the initial frame is created.
(add-hook 'after-make-frame-functions 'fullscreen-mode-fullscreen)

;; initiate workspace
(defun init-workspace ()
  (global-linum-mode 1)
  (fullscreen-mode-fullscreen)
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

;; miscelaneous tweaks
(set-default 'truncate-lines t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; load magit
(require 'magit)

(add-to-list 'load-path (concat modules-path "mo-git-blame"))
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)
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
;;(require 'epy-nose)       ;; For nose integration

(require 'python-pep8)
(require 'python-pylint)

;; Python completion with jedi
(setq jedi:setup-keys t)
(autoload 'jedi:setup "jedi" nil t)

(require 'nose)
(require 'highlight-indentation)

(set-face-background 'highlight-indent-face "#073642")
(add-hook 'php-mode-hook (lambda() (interactive) (column-marker-2 80)))
(add-hook 'python-mode-hook
          (lambda()
            (local-set-key (kbd "C-c a") 'nosetests-all)
            (local-set-key (kbd "C-c M") 'nosetests-module)  ;; C-c m conflicts w/ pylint
            (local-set-key (kbd "C-c .") 'nosetests-one)
            (local-set-key (kbd "C-c x") 'nosetests-stop)
            (local-set-key (kbd "C-c p a") 'nosetests-pdb-all)
            (local-set-key (kbd "C-c p m") 'nosetests-pdb-module)
            (local-set-key (kbd "C-c p .") 'nosetests-pdb-one)
            (highlight-indentation)
            (interactive)
            (whitespace-mode t)
            (setq autopair-handle-action-fns
                  (list #'autopair-default-handle-action
                        #'autopair-python-triple-quote-action))
            (jedi:setup)
            (column-marker-1 80)
            (flycheck-select-checker 'python-flake8)
            (pycov2-mode)
            (linum-mode)
            )
          )

;; Scala-mode and ensime
(require 'scala-mode2)

(add-hook 'scala-mode-hook '(lambda ()

  ;; Alternatively, bind the 'newline-and-indent' command and
  ;; 'scala-indent:insert-asterisk-on-multiline-comment' to RET in
  ;; order to get indentation and asterisk-insertion within multi-line
  ;; comments.
  (local-set-key (kbd "RET") '(lambda ()
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment)))

  ;; Bind the backtab (shift tab) to
  ;; 'scala-indent:indent-with-reluctant-strategy command. This is usefull
  ;; when using the 'eager' mode by default and you want to "outdent" a
  ;; code line as a new statement.
  (local-set-key (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)

  ;; and other bindings here
  ;; Rely on ENSIME for syntax checking
  (interactive)
  (whitespace-mode t)
  (flycheck-mode -1)
))

;; load the ensime lisp code...
(add-to-list 'load-path "ENSIME_ROOT/elisp/")
(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(require 'flycheck)
;; Fix to dispay pep8 error as warnings and pyflakes errors as errors
;; (flycheck-define-checker python-flake8
;;   "A Python syntax and style checker using Flake8.

;; For best error reporting, use Flake8 2.0 or newer.

;; See URL `http://pypi.python.org/pypi/flake8'."
;;   :command ("flake8"
;;              (config-file "--config" flycheck-flake8rc)
;;              (option "--max-complexity"
;;                      flycheck-flake8-maximum-complexity
;;                      flycheck-option-int)
;;              (option "--max-line-length"
;;                      flycheck-flake8-maximum-line-length
;;                      flycheck-option-int)
;;              source-inplace)
;;   :error-patterns
;;   '(("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:E[0-8][0-9]+.*\\)$"
;;      warning)                           ; PEP8 Coding style errors
;;     ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:E9[0-9]+.*\\)$"
;;      error)                             ; PEP8 SyntaxError and IOError
;;     ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:F[0-9]+.*\\)$"
;;      error)                             ; Flake8 >= 2.0
;;     ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:W[0-9]+.*\\)$"
;;      warning)                           ; Flake8 < 2.0
;;     ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:C[0-9]+.*\\)$"
;;      warning)                           ; McCabe complexity in Flake8 > 2.0
;;     ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:N[0-9]+.*\\)$"
;;      warning)                           ; pep8-naming Flake8 plugin.
;;     ;; Syntax errors in Flake8 < 2.0, in Flake8 >= 2.0 syntax errors are caught
;;     ;; by the E.* pattern above
;;     ("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*\\)$" error))
;;   :modes 'python-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-flake8-maximum-line-length 120)
(setq-default flycheck-flake8-maximum-complexity 10)
(setq-default flycheck-highlighting-mode 'lines)


(global-set-key (kbd "RET") 'newline-and-indent)

;; autopair mode
(setq skeleton-pair nil)
(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)


(require 'column-marker)
(epy-django-snippets)
(epy-setup-ipython)



(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-code-indent-offset 4)

(setq web-mode-engines-alist
'(("django" . "\\.html\\'")
  ("razor"     . "\\.scala\\.html\\'")))

(load-file (concat modules-path "pycoverage.el/pycov2.el"))
(require 'linum)
(require 'pycov2)

(add-to-list 'load-path (concat modules-path "python-django.el"))
(require 'python-django)
(global-set-key (kbd "C-x j") 'python-django-open-project)

;; Comment spell checking
;; (setq flyspell-issue-welcome-flag nil)
;; (add-hook 'python-mode-hook 'flyspell-prog-mode)

;; js2 mode setup
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(safe-local-variable-values (quote ((encoding . utf-8) (python-shell-completion-string-code . "';'.join(get_ipython().Completer.all_completions('''%s'''))
") (python-shell-completion-module-string-code . "';'.join(module_completion('''%s'''))
") (python-shell-interpreter-args . "/home/venya/projects/hrbrand-v2012/manage.py shell") (python-shell-interpreter . "ipython") (python-shell-completion-string-code . "';'.join(get_ipython().Completer.all_completions('''%s'''))") (python-shell-completion-module-string-code . "';'.join(module_completion('''%s'''))") (python-shell-interpreter-args . "/home/venya/projects/socaial-network-apps/SocialVacancy/hhsocialvacancy/manage.py shell") (python-shell-completion-string-code . "';'.join(get_ipython().Completer.all_completions('''%s'''))") (python-shell-completion-module-string-code . "';'.join(module_completion('''%s'''))") (python-shell-completion-setup-code . "from IPython.core.completerlib import module_completion") (python-shell-interpreter-args . "/home/venya/projects/career-fair/hhcareeffair/manage.py shell") (python-shell-interpreter . "python")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Workaround the annoying warnings:
;;    Warning (mumamo-per-buffer-local-vars):
;;    Already 'permanent-local t: buffer-file-name
(when (equal emacs-major-version 24)
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))

(load-theme 'solarized-dark)
