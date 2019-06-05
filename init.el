;;; init.el --- Pablo's personal emacs configuration
;;---------------------------------------------------------------------------------

;;---------------------------------------------------------------------------------
;;; commentary:
;;---------------------------------------------------------------------------------

;; Personal Emacs configuration mid migration to use-package

;; Copyright (C) 2018  Pablo Berganza

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;---------------------------------------------------------------------------------
;;; code:
;;---------------------------------------------------------------------------------

;; Window size configuration
;;---------------------------------------------------------------------------------
(when (display-graphic-p)
  (progn
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (let ((window-config
           `(,(if (eq system-type 'darwin)
                  '(width . 80)
                '(width . 255)) ; chars
             (height . 60) ; lines
             (left . 50)
             (top . 50))))
      (setq initial-frame-alist window-config)
      (setq default-frame-alist window-config))))
(setq inhibit-startup-screen t)
(setq package-enable-at-startup nil)
;; (setq x-select-enable-clipboard-manager nil)

(when (display-graphic-p)
  (server-start))


;; Bell configuration
;; ---------------------------------------------------------------------------------
(if (and (display-graphic-p) (not (eq system-type 'darwin)))
    (setq visible-bell t)
  (setq ring-bell-function (lambda () (message "*beep*"))))


;; Font configuration
;;---------------------------------------------------------------------------------
(set-face-attribute
 'default nil :font
 ;;"-CTDB-Fira Code-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1" :height 90)
 ;;"-CYEL-Iosevka-light-normal-normal-*-13-*-*-*-d-0-iso10646-1")
 "-SRC-Hack-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"
 :height (if (eq system-type 'darwin) 120 100))

;; Env configuration
;; ---------------------------------------------------------------------------------
(defun read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defvar env-variables)
(setq env-variables
      (mapcar
       (lambda (var) (split-string var "="))
       (read-lines (expand-file-name "~/.emacs.d/.env"))))

(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ;; The first argument to concat is a string containing a literal tab
                                   ,(concat
                                     "	"
                                     (list (decode-char
                                            'ucs
                                            (cadr regex-char-pair)))))))))
          '(("\\(www\\)"                   #Xe100)
            ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
            ("\\(\\*\\*\\*\\)"             #Xe102)
            ("\\(\\*\\*/\\)"               #Xe103)
            ("\\(\\*>\\)"                  #Xe104)
            ("[^*]\\(\\*/\\)"              #Xe105)
            ("\\(\\\\\\\\\\)"              #Xe106)
            ("\\(\\\\\\\\\\\\\\)"          #Xe107)
            ("\\({-\\)"                    #Xe108)
            ;; ("\\(\\[\\]\\)"                #Xe109) REMOVED due to being bothersome on array definitions
            ("\\(::\\)"                    #Xe10a)
            ("\\(:::\\)"                   #Xe10b)
            ("[^=]\\(:=\\)"                #Xe10c)
            ("\\(!!\\)"                    #Xe10d)
            ("\\(!=\\)"                    #Xe10e)
            ("\\(!==\\)"                   #Xe10f)
            ("\\(-}\\)"                    #Xe110)
            ("\\(--\\)"                    #Xe111)
            ("\\(---\\)"                   #Xe112)
            ("\\(-->\\)"                   #Xe113)
            ("[^-]\\(->\\)"                #Xe114)
            ("\\(->>\\)"                   #Xe115)
            ("\\(-<\\)"                    #Xe116)
            ("\\(-<<\\)"                   #Xe117)
            ("\\(-~\\)"                    #Xe118)
            ("\\(#{\\)"                    #Xe119)
            ("\\(#\\[\\)"                  #Xe11a)
            ("\\(##\\)"                    #Xe11b)
            ("\\(###\\)"                   #Xe11c)
            ("\\(####\\)"                  #Xe11d)
            ;; ("\\(#(\\)"                    #Xe11e)
            ("\\(#\\?\\)"                  #Xe11f)
            ("\\(#_\\)"                    #Xe120)
            ("\\(#_(\\)"                   #Xe121)
            ("\\(\\.-\\)"                  #Xe122)
            ("\\(\\.=\\)"                  #Xe123)
            ("\\(\\.\\.\\)"                #Xe124)
            ("\\(\\.\\.<\\)"               #Xe125)
            ("\\(\\.\\.\\.\\)"             #Xe126)
            ("\\(\\?=\\)"                  #Xe127)
            ("\\(\\?\\?\\)"                #Xe128)
            ("\\(;;\\)"                    #Xe129)
            ("\\(/\\*\\)"                  #Xe12a)
            ("\\(/\\*\\*\\)"               #Xe12b)
            ("\\(/=\\)"                    #Xe12c)
            ("\\(/==\\)"                   #Xe12d)
            ("\\(/>\\)"                    #Xe12e)
            ("\\(//\\)"                    #Xe12f)
            ("\\(///\\)"                   #Xe130)
            ("\\(&&\\)"                    #Xe131)
            ("\\(||\\)"                    #Xe132)
            ("\\(||=\\)"                   #Xe133)
            ("[^|]\\(|=\\)"                #Xe134)
            ("\\(|>\\)"                    #Xe135)
            ("\\(\\^=\\)"                  #Xe136)
            ("\\(\\$>\\)"                  #Xe137)
            ("\\(\\+\\+\\)"                #Xe138)
            ("\\(\\+\\+\\+\\)"             #Xe139)
            ("\\(\\+>\\)"                  #Xe13a)
            ("\\(=:=\\)"                   #Xe13b)
            ("[^!/]\\(==\\)[^>]"           #Xe13c)
            ("\\(===\\)"                   #Xe13d)
            ("\\(==>\\)"                   #Xe13e)
            ("[^=]\\(=>\\)"                #Xe13f)
            ("\\(=>>\\)"                   #Xe140)
            ("\\(<=\\)"                    #Xe141)
            ("\\(=<<\\)"                   #Xe142)
            ("\\(=/=\\)"                   #Xe143)
            ("\\(>-\\)"                    #Xe144)
            ("\\(>=\\)"                    #Xe145)
            ("\\(>=>\\)"                   #Xe146)
            ("[^-=]\\(>>\\)"               #Xe147)
            ("\\(>>-\\)"                   #Xe148)
            ("\\(>>=\\)"                   #Xe149)
            ("\\(>>>\\)"                   #Xe14a)
            ("\\(<\\*\\)"                  #Xe14b)
            ("\\(<\\*>\\)"                 #Xe14c)
            ("\\(<|\\)"                    #Xe14d)
            ("\\(<|>\\)"                   #Xe14e)
            ("\\(<\\$\\)"                  #Xe14f)
            ("\\(<\\$>\\)"                 #Xe150)
            ("\\(<!--\\)"                  #Xe151)
            ("\\(<-\\)"                    #Xe152)
            ("\\(<--\\)"                   #Xe153)
            ("\\(<->\\)"                   #Xe154)
            ("\\(<\\+\\)"                  #Xe155)
            ("\\(<\\+>\\)"                 #Xe156)
            ("\\(<=\\)"                    #Xe157)
            ("\\(<==\\)"                   #Xe158)
            ("\\(<=>\\)"                   #Xe159)
            ("\\(<=<\\)"                   #Xe15a)
            ("\\(<>\\)"                    #Xe15b)
            ("[^-=]\\(<<\\)"               #Xe15c)
            ("\\(<<-\\)"                   #Xe15d)
            ("\\(<<=\\)"                   #Xe15e)
            ("\\(<<<\\)"                   #Xe15f)
            ("\\(<~\\)"                    #Xe160)
            ("\\(<~~\\)"                   #Xe161)
            ("\\(</\\)"                    #Xe162)
            ("\\(</>\\)"                   #Xe163)
            ("\\(~@\\)"                    #Xe164)
            ("\\(~-\\)"                    #Xe165)
            ("\\(~=\\)"                    #Xe166)
            ("\\(~>\\)"                    #Xe167)
            ("[^<]\\(~~\\)"                #Xe168)
            ("\\(~~>\\)"                   #Xe169)
            ("\\(%%\\)"                    #Xe16a)
            ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so i'm leaving it out.
            ("[^:=]\\(:\\)[^:=]"           #Xe16c)
            ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
            ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

(defun add-fira-code-symbol-keywords ()
  "Add fira code symbol keywords."
  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

(when (display-graphic-p)
  ;; This works when using emacs --daemon + emacsclient
  (add-hook 'after-make-frame-functions (lambda (frame)
                                          (set-fontset-font
                                           t
                                           '(#Xe100 . #Xe16f)
                                           "Fira Code Symbol")))
  ;; This works when using emacs without server/client
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
  ;; I haven't found one statement that makes both of the above situations work, so I use both for now

  (add-hook 'prog-mode-hook
            #'add-fira-code-symbol-keywords))


;; Package manager configuration
;;---------------------------------------------------------------------------------
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list
   'package-archives
   (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives
                 '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(beacon-mode t)
 '(custom-enabled-themes (quote (moe-dark)))
 '(custom-safe-themes
   (quote
    ("436b185b423b78eb5d110dc23f4b95d78a1f002d156f226b7e6e5b1f6493dda0" "c53b6a09c7d997c3185cb1598de1d0ff15e1679f5445f9a6cb8b2bf4fc4e565a" "c8d19a09f9d2cb1d6aa6c57e1a86b2dab863cc77a3fc7225a4e60baba96726a1" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" default)))
 '(js-indent-level 2)
 '(nyan-mode t)
 '(org-confirm-babel-evaluate nil)
 '(package-selected-packages
   (quote
    (graphql-mode ob-clojurescript langtool flycheck-vale csharp-mode protobuf-mode flycheck-joker speed-type hy-mode lispy intero haskell-mode counsel-projectile projectile elisp-demos helpful docker carbon-now-sh lsp-ui which-key dockerfile-mode godoctor go-guru company-go go-mode htmlize py-autopep8 julia-repl julia-mode company-lsp flycheck-flow rjsx-mode visual-regexp visual-regexp-steroids octave-mode gitmoji company-emoji emojify diminish google-this pipenv company-jedi elpy powerline dimmer focus unicode-fonts moe-theme ledger-mode atomic-chrome sql-indent ob-http ob-restclient plantuml-mode drawille xkcd beacon hacker-typer cargo flycheck-rust rust-mode clojure-mode cider zone-nyan ivy-hydra nyan-mode indium counsel swiper ivy markdown-mode editorconfig json-mode neotree vue-mode tide typescript-mode evil restclient company-tern ag xref-js2 js2-refactor exec-path-from-shell js-format magit apib-mode yaml-mode racket-mode ac-js2 use-package erc-hl-nicks weechat js2-mode smartparens auto-package-update web-mode php-mode flycheck)))
 '(speedbar-show-unknown-files t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))
(eval-when-compile
  (require 'diminish))


;; Appearance configuration
;;---------------------------------------------------------------------------------
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package zone
  :diminish
  :if (display-graphic-p)
  :config
  (when (fboundp 'zone-when-idle)
    (zone-when-idle 300)))

(use-package moe-theme
  :ensure t
  :config
  (defvar moe-theme-highlight-buffer-id)
  (setq moe-theme-highlight-buffer-id t)
  (when (fboundp 'moe-theme-set-color)
    (moe-theme-set-color 'purple))
  (when (fboundp 'moe-dark)
    (moe-dark)))

(use-package powerline
  :ensure t
  :config

  (when (memq window-system '(mac ns x))
    (setq powerline-image-apple-rgb t))
  (when (fboundp 'powerline-moe-theme)
    (powerline-moe-theme))
  (when (fboundp 'moe-dark)
    (moe-dark))
  :after (moe-theme))

(use-package nyan-mode
  :ensure t
  :if (display-graphic-p)
  :config
  (when (fboundp 'nyan-mode)
    (nyan-mode))
  (when (fboundp 'nyan-toggle-wavy-trail)
    (nyan-toggle-wavy-trail))
  (when (fboundp 'nyan-start-animation)
    (nyan-start-animation)))

(use-package dimmer
  :ensure t)

(use-package emojify
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-emojify-mode))

(use-package gitmoji
  :load-path "~/.emacs.d/lisp/gitmoji"
  :after (ivy emojify magit)
  :config
  (setq gitmoji-ask-ticket t))

;; MacOS configuration
;; ---------------------------------------------------------------------------------
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize))
  (defvar ns-alternate-modifier)
  (defvar ns-right-alternate-modifier)
  (setq ns-alternate-modifier 'meta)
  (setq ns-right-alternate-modifier 'none))


;; Tools configuration
;; ---------------------------------------------------------------------------------
(use-package visual-regexp
  :ensure t)

(use-package visual-regexp-steroids
  :ensure t
  :after (visual-regexp)
  :bind ())

(use-package flycheck-vale
  :ensure t
  :config
  (flycheck-vale-setup))

(use-package langtool
  :ensure t
  :init
  (setq langtool-language-tool-jar "/home/pberganza/Downloads/LanguageTool-4.4/languagetool-commandline.jar"))

;; Miscellaneous packages
;; ---------------------------------------------------------------------------------
(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :after (ivy)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  (if (fboundp 'projectile-register-project-type)
      (progn
        (projectile-register-project-type 'yarn '("yarn.lock")
                                          :compile "yarn"
                                          :test "yarn test:cov"
                                          :run "yarn start:dev"
                                          :test-suffix ".spec")
        (projectile-register-project-type 'pipenv '("Pipfile")
                                          :compile "pipenv install"))
    (error "Not defined: %s" "projectile-register-project-type")))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :config
  (counsel-projectile-mode +1))

(use-package editorconfig
  :ensure t
  :diminish
  :config
  (editorconfig-mode 1))

(use-package carbon-now-sh
  :ensure t)

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

(use-package google-this
  :ensure t
  :defer t
  :bind-keymap ("C-c /" . google-this-mode-submap))

(use-package flycheck
  :ensure t
  :config
  ;; https://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    "Get local eslint executable."
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name
                         "node_modules/eslint/bin/eslint.js"
                         root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (if (fboundp 'my/use-eslint-from-node-modules)
      (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
    (error "Not defined: %s"  "my/use-eslint-from-node-modules"))
  ;; (flycheck-add-next-checker 'python-flake8 'python-mypy)
  )

;; (use-package flycheck-clojure
;;   :ensure t
;;   :after (flycheck)
;;   :config
;;   (flycheck-clojure-setup))

(use-package objed
  :ensure t)

(use-package restclient
  :ensure t)

(use-package ob-http
  :ensure t)

(use-package ob-restclient
  :ensure t)

(use-package ob-clojurescript
  :ensure t)

(use-package ledger-mode
  :ensure t)

(use-package atomic-chrome
  :ensure t
  :config
  (atomic-chrome-start-server))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package ace-window
  :ensure t
  :bind ( "M-o" . ace-window))

(defun set-fill-column-80 ()
  "Set fill column to 80."
  (setq fill-column 80))

;; (use-package fill-column-indicator
;;   :load-path "~/.emacs.d/lisp/fill-column-indicator"
;;   :hook ((prog-mode . fci-mode)
;;          (prog-mode . set-fill-column-80)
;;          (clojure-mode . set-fill-column-80)))

(use-package speed-type
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-c m c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;; Docker configuration
;;---------------------------------------------------------------------------------
(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; Apiary configuration
;;---------------------------------------------------------------------------------
(defun apiary-start-server-current ()
  "Start apiary preview with current path."
  (interactive)
  (async-shell-command
   (concat
    (shell-command-to-string "ruby -e 'print Gem.user_dir'")
    "/bin/apiary preview --server --watch --path="
    (buffer-file-name (current-buffer))))
  (sleep-for 2)
  (other-window 1)
  (delete-window)
  (browse-url "localhost:8080"))


(defun apiary-preview-current-buffer ()
  "Execute apiary preview on current buffer."
  (interactive)
  (let ((new-file-name (concat "temp-" (number-to-string
                                        (random most-positive-fixnum)))))
    (write-region (point-min) (point-max) (concat "/tmp/" new-file-name))
    (shell-command
     (concat
      (shell-command-to-string "ruby -e 'print Gem.user_dir'")
      "/bin/apiary preview --path="
      (concat "/tmp/" new-file-name)))
    (delete-file (concat "/tmp/" new-file-name))))

(defun apiary-publish (apiary-api-name)
  "Publish apib document to specified APIARY-API-NAME."
  (interactive "sEnter API name: ")
  (let ((new-file-name (concat "temp-" (number-to-string
                                        (random most-positive-fixnum)))))
    (write-region (point-min) (point-max) (concat "/tmp/" new-file-name))
    (shell-command
     (concat
      "APIARY_API_KEY="
      (when (fboundp 'second)
        (second (assoc "APIARY_KEY" env-variables)))
      " "
      (shell-command-to-string "ruby -e 'print Gem.user_dir'")
      "/bin/apiary publish --path="
      "/tmp/"
      new-file-name
      " --api-name="
      apiary-api-name
      ))
    (delete-file (concat "/tmp/" new-file-name))))

(use-package apib-mode
  :ensure t
  :mode "\\.apib\\'"
  :bind (:map apib-mode-map
              ("C-c C-a P" . apiary-publish)
              ("C-c C-a p" . apiary-preview-current-buffer)
              ("C-c C-a s" . apiary-start-server-current)))

(use-package flycheck-apib
  :load-path "~/.emacs.d/lisp/flycheck-apib"
  :after (flycheck))


;; Python configuration
;;---------------------------------------------------------------------------------
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  (setq elpy-modules (delete 'elpy-module-flymake elpy-modules))
  (setq flycheck-python-flake8-executable "flake8"))

(use-package pipenv
  :ensure t
  :config
  (setq pipenv-with-flycheck nil)
  :hook (python-mode . pipenv-mode))

(use-package company-jedi
  :ensure t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package highlight-indentation
  :diminish)

(use-package py-autopep8
  :ensure t
  :hook (elpy-mode . py-autopep8-enable-on-save))

(use-package hy-mode
  :ensure t)

;; JavaScript configuration
;;---------------------------------------------------------------------------------
(load-file "~/.emacs.d/flow-for-emacs/flow.el")

(use-package indium
  :ensure t
  :hook (js2-mode . indium-interaction-mode))

(use-package company
  :ensure t
  :hook ((js2-mode . company-mode)
         (vue-mode . company-mode)
         (cider-repl-mode . company-mode)
         (cider-mode . company-mode)
         (go-mode . company-mode)))

(use-package company-tern
  :ensure t
  :config
  (add-to-list 'company-backends 'company-tern)
  :hook (js2-mode . tern-mode)
  :bind (:map tern-mode-keymap
              ("M-." . nil)
              ("M-," . nil)
              ))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :bind (:map js2-mode-map
              ("C-k" . js2r-kill))
  :config
  (setq js2-mode-show-strict-warnings nil))

(use-package js2-refactor
  :ensure t
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-refactor-mode))
  :config
  (when (fboundp 'js2r-add-keybindings-with-prefix)
    (js2r-add-keybindings-with-prefix "C-c r"))
  :after (js2-mode))

(use-package xref-js2
  :ensure t
  :hook ((js2-mode . (lambda ()
                       (add-hook
                        'xref-backend-functions
                        #'xref-js2-xref-backend nil t))))
  :after (js2-mode))

(use-package js
  :bind (:map js-mode-map
              ("M-." . nil)))

(use-package json-mode
  :ensure t)

(use-package rjsx-mode
  :ensure t)


;; TypeScript configuration
;;---------------------------------------------------------------------------------
(defvar flycheck-check-syntax-automatically)
(defun setup-tide-mode ()
  "Function for setting up tide."
  (interactive)
  (message "Setting up tide")
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :ensure t
  :hook ((typescript-mode . setup-tide-mode)
         (web-mode . (lambda ()
		       (when (string-equal
                              "tsx" (file-name-extension buffer-file-name))
		         (setup-tide-mode))))
         (before-save . tide-format-before-save))
  :config
  (setq tide-format-options
        '(:indentSize 2 :tabSize 2))
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t
                company-dabbrev-code-everywhere t)
  (setq company-auto-complete t)
  (defvar web-mode-enable-auto-quoting)
  :after (company flycheck))

(if (fboundp 'flycheck-add-mode)
    (flycheck-add-mode 'typescript-tslint 'web-mode)
  (error "Not defined: %s"  "flycheck-add-mode"))
;; aligns annotation to the right hand side
(defvar typescript-indent-level)
(setq typescript-indent-level 2)


;; Vue configuration
;; ---------------------------------------------------------------------------------
(use-package vue-mode
  :ensure t
  :bind (:map vue-mode-map
              ("C-c i" . vue-insert-template))
  :config
  (defun vue-insert-template ()
    "Insert template for a VueJS single file component."
    (interactive)
    (let (tag-id (name (read-string "Component name "))
                 (tag (read-string "Initial tag: ")))
      (insert "<template>\n")
      (when (> (length tag) 0)
        (setq tag-id (read-string "Enter id for tag: "))
        (when (> (length tag-id) 0)
          (setq tag-id (concat " id=\"" tag-id "\"")))
        (insert "  <" tag tag-id ">\n\n  </" tag ">"))
      (insert "\n</template>")
      (insert
       "\n<script>\n"
       "export default ({\n")
      (insert "  name: '" name "',\n")
      (insert
       "});"
       "\n</script>")
      (insert "\n<style scoped>\n\n</style>")
      (if (fboundp 'vue-mode-reparse)
          (vue-mode-reparse)
        (error "Not defined: %s"  "vue-mode-reparse"))))
  (setq vue-html-extra-indent 2))

(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t
  :config
  (add-to-list 'flycheck-checkers 'lsp-ui))

;; (use-package lsp-vue
;;   :ensure t
;;   :hook (vue-mode . lsp-vue-mmm-enable))

(use-package company-lsp
  :ensure t
  :config
  (add-to-list 'company-backends 'company-lsp)
  :after (company))

;; Lisp configuration
;;---------------------------------------------------------------------------------
(use-package lispy
  :ensure t
  :diminish
  :hook ((emacs-lisp-mode . lispy-mode)
         (clojure-mode . lispy-mode)
         (hy-mode . lispy-mode)
         (scheme-mode . lispy-mode))
  :config
  (add-to-list 'lispy-compat 'cider))

;; Clojure configuration
;;---------------------------------------------------------------------------------
(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode))

(use-package flycheck-joker
  :ensure t)

(use-package racket-mode
  :ensure t)

;; Golang configuration
(defun go-compile-and-run ()
  "Run and compile go program."
  (interactive)
  (let ((file-dir (file-name-directory (buffer-file-name))))
    (when (not (file-exists-p (concat file-dir "makefile")))
      (write-region
       (concat "run:\n\tgo build\n\t"
               (substring (buffer-file-name) 0 -3))
       nil
       (concat file-dir "makefile")))
    (compile "make run")))

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
              ("C-c C-c" . go-compile-make)
              ("C-c C-e" . go-run)
              ("C-c C-q" . go-stop))
  :config
  (defun go-compile-make ()
    "Compile code for go."
    (interactive)
    (shell-command "go build"))
  (defun go-run (file-path)
    "Start go process using FILE-PATH."
    (interactive "fEnter file: ")
    (async-shell-command file-path "*Go Process*"))
  (defun go-stop ()
    "Stop go process."
    (interactive)
    (interrupt-process "*Go Process*")
    (switch-to-buffer "*Go Process*"))
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-guru
  :ensure t
  :after (go-mode))

(use-package godoctor
  :ensure t
  :after (go-mode))

(use-package company-go
  :ensure t
  :config
  (add-to-list 'company-backends 'company-go)
  :after (company))

;; Octave configuration
;;---------------------------------------------------------------------------------
(use-package octave-mode
  :mode "\\.m$"
  :config
  (add-hook 'octave-mode-hook
            (lambda ()
              (abbrev-mode 1)
              (auto-fill-mode 1)
              (if (eq window-system 'x)
                  (font-lock-mode 1)))))


;; Plantuml configuration
;;---------------------------------------------------------------------------------
(use-package plantuml-mode
  :ensure t
  :mode "\\.puml\\'"
  :config
  (defvar org-plantuml-jar-path)
  (setq org-plantuml-jar-path
        (expand-file-name "~/plantuml.jar")))


;; Org babel configuration
;;---------------------------------------------------------------------------------
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (ledger . t)
   (lilypond . t)
   (sql . t)
   (python . t)
   (js . t)
   (http . t)
   (restclient . t)
   (shell . t)
   (plantuml . t)
   (clojure . t)))

(defvar org-babel-clojure-backend)
(setq org-babel-clojure-backend 'cider)

(use-package yaml-mode
  :ensure t)

(use-package sql-indent
  :ensure t
  :hook (sql-mode . sqlind-minor-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :after (ivy))

;; Ivy configuration
;;---------------------------------------------------------------------------------
(defvar ivy-on-del-error-funtcion)

(use-package ivy
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume))
  :config
  (with-no-warnings
    (ivy-mode 1)
    (setq ivy-on-del-error-funtcion #'ignore)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (defun counsel-emojify-apropos-emoji ()
      "Forward to emojify-apropos-emoji."
      (interactive)
      (ivy-read "Look for emoji: "
                (let (cands)
                  (maphash (lambda (emoji data)
                             (push (gethash "name" data) cands))
                           emojify-emojis)
                  (delete-dups cands))
                :preselect 0
                :require-match t
                :sort t
                :action (lambda (x)
                          (emojify-apropos-emoji x))
                :caller 'counsel-emojify-apropos-emoji))))

(use-package ivy-hydra
  :ensure t
  :after (ivy))

(use-package counsel
  :ensure t
  :bind (("C-s" . counsel-grep-or-swiper)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-S-o" . counsel-rhythmbox)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :after (ag))

(use-package ag
  :ensure t)

(use-package beacon
  :ensure t
  :diminish
  :config
  (if (display-graphic-p)
      (beacon-mode 1)
    (beacon-mode -1)))

(use-package erc
  :config
  (setq erc-nick "Pabc1")
  (defvar erc-fill-column)
  (setq erc-fill-column (- (window-width) 2))
  (add-hook 'erc-mode-hook
            (lambda ()
	      (set (make-local-variable 'scroll-conservatively) 100))))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :ensure t
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t))

;; Julia configuration
;;---------------------------------------------------------------------------------
(use-package julia-mode
  :ensure t)

(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode))

;; C# configuration
;; --------------------------------------------------------------------------------
(use-package csharp-mode
  :ensure t)

;; Protocol Buffers configuration
;; --------------------------------------------------------------------------------
(use-package protobuf-mode
  :ensure t)

;; Haskell configuration
;;---------------------------------------------------------------------------------
(use-package haskell-mode
  :ensure t)

(use-package intero
  :ensure t
  :hook (haskell-mode . intero-mode))

;; GraphQL configuration
;; --------------------------------------------------------------------------------
(use-package graphql-mode
  :ensure t)

;; Slack configuration
;;---------------------------------------------------------------------------------
;; (use-package slack
;;   :ensure t
;;   :defer t
;;   :init
;;   (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
;;   (setq slack-prefer-current-team t)
;;   :config
;;   (slack-register-team
;;    :name "applaudostudios"
;;    :default t
;;    :token (second (assoc "SLACK_TOKEN" env-variables))
;;    :client-id (second (assoc "SLACK_CLIENT_ID" env-variables))
;;    :client-secret (second (assoc "SLACK_CLIENT_SECRET" env-variables))
;;    :subscribed-channels '(general arauco arauco-feeds devteam web-devs)
;;    :full-and-display-names t))

;; (use-package alert
;;   :ensure t
;;   :commands (alert)
;;   :init
;;   (setq alert-default-style 'notifier))

;; Org mode configuration
;;---------------------------------------------------------------------------------
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(defvar org-agenda-files)
(setq org-agenda-files (list "~/org/work.org" "~/org/personal.org"))
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)" "CANCELLED(x)")
        (sequence "FIX(f)" "HOLD(h)" "|" "DONE(d)" "CANCELLED(x)")
        ))

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(with-eval-after-load 'rust-mode
  (if (fboundp 'flycheck-rust-setup)
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    (error "Not defined: %s" "flycheck-rust-setup")))

(setq-default indent-tabs-mode nil)
(setq tab-width 2) ; or any other preferred value
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvar org-log-done)
(setq org-log-done t)
(defvar org-edit-src-content-indentation)
(setq org-edit-src-content-indentation 0)
;; (defvar org-src-preserve-indentation)
;; (setq org-src-preserve-indentation 't)

;; Help configuration
;;---------------------------------------------------------------------------------
(use-package helpful
  :ensure t
  :bind (("C-h k" . helpful-key)
         ("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable))
  :after (counsel))

(use-package elisp-demos
  :ensure t
  :after (helpful)
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; Miscellaneous functions
;;---------------------------------------------------------------------------------
(defun insert-date ()
  "Insert date to buffer."
  (interactive)
  (insert (shell-command-to-string "date \"+%Y-%m-%d %T\""))
  (delete-char -1))

(defun carbon-now-execute-dwim (&optional argument)
  "Execute command depending on region and optionally append ARGUMENT."
    (if (not mark-active)
      (shell-command (concat "carbon-now -h " (buffer-file-name)))
    (let ((begin-line (line-number-at-pos (region-beginning)))
          (end-line (line-number-at-pos (region-end))))
      (shell-command (concat "carbon-now -h -s "
                             (number-to-string begin-line)
                             " -e "
                             (number-to-string end-line)
                             (if argument
                                 (concat " " argument " ")
                               " ")
                               (buffer-file-name))))))

(defun carbon-now-save-dwim ()
  "Send file or region to carbon-now-cli."
  (interactive)
  (carbon-now-execute-dwim))

(defun carbon-now-save-dwim-to-file (file-path)
  "Send file or region to carbon-now-cli and save it to FILE-PATH.
if FILE_PATH is a file, it will be saved with that file-name.
If it is a directory, it will be saved with a generated name."
  (interactive "GEnter file path: ")
  (carbon-now-execute-dwim
   (concat "-l "
           (substring (file-name-directory file-path) 0 -1)
           (if (not (string= "" (file-name-nondirectory file-path)))
               (concat " -t " (file-name-nondirectory file-path))
             ""))))

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

;; make backup to a designated dir, mirroring the full path
;;---------------------------------------------------------------------------------
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path (FPATH).
If the new path's directories does not exist, create them."
  (let* (
         (backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath ))
         (backupFilePath
          (replace-regexp-in-string "//" "/"
                                    (concat backupRootDir filePath "~") )))
    (make-directory
     (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))

(setq make-backup-file-name-function 'my-backup-file-name)

(provide 'init)
;;; init.el ends here
