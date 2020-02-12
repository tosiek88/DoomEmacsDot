;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;in case you will need sql workbanch in feature
;;(load! "/home/tocha/.doom.d/lisp/swb-iconnection.el")
;;(load! "/home/tocha/.doom.d/lisp/swb-connection-mysql.el")
;;(load! "/home/tocha/.doom.d/lisp/sql-workbench.el")
;;(load! "/home/tocha/.doom.d/lisp/company-swb")

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!
;;(push 'company-swb company-backends)


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)
(split-window-vertically)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;;
(defun sp-paredit-like-close-round ()
  "If the next character is a closing character as according to smartparens skip it, otherwise insert `last-input-event'."
  (interactive)
  (let ((pt (point)))
    (if (and (< pt (point-max))
             (sp--char-is-part-of-closing (buffer-substring-no-properties pt (1+ pt))))
        (forward-char 1)
      (call-interactively #'self-insert-command))))
;; ======== INDENTATION/BRACKET MANAGEMENT ========
;; evil-smartparens
(use-package smartparens)
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(define-key smartparens-mode-map (kbd ")") #'sp-paredit-like-close-round)
(use-package evil-smartparens)

;; ======== ORG-MODE ========
(defun make-youtube-link(youtube_id)
  (browse-url (concat "https://www.youtube.com/watch?v=" youtube_id)))

(after! org (org-add-link-type "yt" #'make-youtube-link))

;; ======== HYDRA ========
(use-package hydra
  :defer t)

;; ======== WEB MODE ========
(require 'web-mode)
(use-package web-mode
  :defer t)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

 (add-hook 'web-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-web-html))
                            (company-mode t)))
(add-hook 'web-mode-hook #'smartparens-mode)
(add-hook 'web-mode-hook #'evil-smartparens-mode)

;; ======== JAVASCRIPT ========
(use-package js2-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (setq js2-include-node-externs t)
  (setq js2-include-browser-externs t)
  (setq js2-highlight-level 3)
  :config
  ;; better imenu
  ;; (js2-imenu-extras-mode)
  ;; tern autocompletion
  ;; (use-package company-tern)
  ;; (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook (lambda ()
                             ;; (tern-mode)
                             (company-mode)
                             (smartparens-mode)
                             (evil-smartparens-mode)
                             (flycheck-mode)))
  (define-key js-mode-map (kbd "M-.") nil)
  )


(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package xref-js2
  :defer t
  :config
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  )
;;============MAGIT GIT FLOW ===========
;;; C-f in the magit status buffer invokes the magit-gitflow popup. If you
;;; would like to use a different key, set the magit-gitflow-popup-key variable
;;; before loading magit-gitflow
;; (setq magit-gitflow-popup-key "C-n")

(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;;===========MAGIT-TODO=================
(require 'magit-todos)
;;
;;===========INDIUM=============================
(use-package indium)
(require 'indium)
(add-hook 'js-mode-hook #'indium-interaction-mode)
;; add-node-modules-path retrives binaries from node_modules for things like eslint
(use-package add-node-modules-path
  :hook (js2-mode . add-node-modules-path))
;;Just for keybinding in REPL mode
(evil-make-overriding-map indium-debugger-mode-map)
(add-hook 'test-hook 'indium-launch)

(defun test-hook()
  (message "test hooka")
  )
(evil-make-overriding-map indium-debugger-locals-mode-map)
;; setup mode hooks
(sp-local-pair 'js2-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; js2-refactor hydra
(defhydra hydra-js2-refactor (:color blue :hint nil)
  "
^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
------------------------------------------------------------------------------------------------------------------------------
[_lp_] Localize Parameter      [_ev_] Extract variable   [_wi_] Wrap buffer in IIFE    [_k_]  js2 kill      [_lt_] log this
[_ef_] Extract function        [_iv_] Inline variable    [_ig_] Inject global in IIFE  [_ss_] split string  [_dt_] debug this
[_ip_] Introduce parameter     [_rv_] Rename variable    [_ee_] Expand node at point   [_sl_] forward slurp
[_em_] Extract method          [_vt_] Var to this        [_cc_] Contract node at point [_ba_] forward barf
[_ao_] Arguments to object     [_sv_] Split var decl.    [_uw_] unwrap
[_tf_] Toggle fun exp and decl [_ag_] Add var to globals
[_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
[_z_] return                   [_q_]  quit"
  ("ee" js2r-expand-node-at-point)
  ("cc" js2r-contract-node-at-point)
  ("ef" js2r-extract-function)
  ("em" js2r-extract-method)
  ("tf" js2r-toggle-function-expression-and-declaration)
  ("ta" js2r-toggle-arrow-function-and-expression)
  ("ip" js2r-introduce-parameter)
  ("lp" js2r-localize-parameter)
  ("wi" js2r-wrap-buffer-in-iife)
  ("ig" js2r-inject-global-in-iife)
  ("ag" js2r-add-to-globals-annotation)
  ("ev" js2r-extract-var)
  ("iv" js2r-inline-var)
  ("rv" js2r-rename-var)
  ("vt" js2r-var-to-this)
  ("ao" js2r-arguments-to-object)
  ("ti" js2r-ternary-to-if)
  ("sv" js2r-split-var-declaration)
  ("ss" js2r-split-string)
  ("uw" js2r-unwrap)
  ("lt" js2r-log-this)
  ("dt" js2r-debug-this)
  ("sl" js2r-forward-slurp)
  ("ba" js2r-forward-barf)
  ("k" js2r-kill)
  ("q" nil)
  ("z" hydra-javascript/body)
  )

(defhydra hydra-javascript (:color red
                                   :hint nil)
  "
  ^Buffer^                    ^Errors/Format^             ^Refactor^                   ^Indium^                 ^Tide^
---------------------------------------------------------------------------------------------------------------------------------------
[_d_]   Documentation         [_e_] Flycheck            [_rs_]  Rename symbol         [_in_]  Indium node       [_*_]  Restart server
[_fd_]  Find definition       [_a_] Apply error fix     [_rf_]  Refactor              [_ic_]  Indium chrome     [_v_]  Verify setup
[_fr_]  Find references       [_t_]  Tide format        [_rj_]  js2-refactor          [_is_]  Indium scratch    [_oi_]  Organize imports
[_fj_]  Jump to func def      [_c_]  JSDoc comment
[_fw_]  Show func def window
[_fx_]  xref find refs
"
  ("d" tide-documentation-at-point :exit t)
  ("fd" tide-jump-to-definition :exit t)
  ("fr" tide-references :exit t)
  ("fj" xref-find-definitions)
  ("fw" xref-find-definitions-other-window)
  ("fx" xref-find-references)
  ("e" hydra-flycheck/body :exit t)
  ("a" tide-fix :exit t)
  ("t" tide-format :exit t)
  ("c" tide-jsdoc-template :exit t)
  ("rs" tide-rename-symbol :exit t)
  ("rf" tide-refactor :exit t)
  ("rj" hydra-js2-refactor/body :exit t)
  ("in" indium-connect-to-nodejs :exit t)
  ("ic" indium-connect-to-chrome :exit t)
  ("is" indium-scratch :exit t)
  ("*" tide-restart-server :exit t)
  ("v" tide-verify-setup :exit t)
  ("oi" tide-organize-imports :exit t)
  )
;; ============ TYPESCRIPT =================
(require 'prettier-js)
(require 'tide)
(dolist (hook (list
               'js2-mode-hook
               'rjsx-mode-hook
               'typescript-mode-hook
               ))
  (add-hook hook (lambda ()
                   ;; 初始化 tide
                   (interactive)
                   (tide-setup)
                   (flycheck-mode +1)
                   (setq flycheck-check-syntax-automatically '(mode-enable save))
                   (flycheck-add-next-checker 'typescript-tide
                                              'typescript-tslint)
                   (prettier-js-mode)

                   (eldoc-mode)
                   (eldoc-mode +1)
                   (tide-hl-identifier-mode +1)

                   (setq tide-user-preferences '(:includeCompletionsForModuleExports t :includeCompletionsWithInsertText t :allowTextChangesInNewFiles t))
                   ;; company is an optional dependency. You have to
                   ;; install it separately via package-install
                   ;; `M-x package-install [ret] company`
                   (company-mode +1)
                   (set (make-local-variable 'company-backends)
                        '((company-tide company-files :with company-yasnippet)
                          (company-dabbrev-code company-dabbrev)))
                   (setq company-minimum-prefix-length 1)
                   ;; 当 tsserver 服务没有启动时自动重新启动
                   (unless (tide-current-server)
                     (tide-restart-server))
                   )))
(setq display-line-numbers-type 'relative)

;; configure smartparens
(add-hook 'typescript-mode-hook #'smartparens-mode)
(add-hook 'typescript-mode-hook #'evil-smartparens-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook #'indium-interaction-mode)

(sp-local-pair 'typescript-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(add-hook 'typescript-mode-hook #'add-node-modules-path)
(require 'restclient)
(defhydra hydra-typescript (:color red
                                   :hint nil )

  "
  ^Buffer^                 ^Errors^                   ^Refactor^                   ^Format^                 ^Tide^
------------------------------------------------------------------------------------------------------------------------------------
[_d_]   Documentation      [_e_] Errors              [_rs_]  Rename symbol         [_t_]  Tide format       [_*_]  Restart server
[_fd_]  Find definition    [_a_] Apply error fix     [_rf_]  Refactor              [_c_]  JSDoc comment     [_v_]  Verify setup
[_fr_]  Find references                                                                               [_i_]  Organize imports
"
  ("a" tide-fix :exit t)
  ("d" tide-documentation-at-point :exit t)
  ("fd" tide-jump-to-definition :exit t)
  ("fr" tide-references :exit t)
  ("c" tide-jsdoc-template :exit t)
  ("e" tide-project-errors :exit t)
  ("rs" tide-rename-symbol :exit t)
  ("rf" tide-refactor :exit t)
  ("t" tide-format :exit t)
  ("*" tide-restart-server :exit t)
  ("v" tide-verify-setup :exit t)
  ("i" tide-organize-imports :exit t)
  )
;;COMPANY MODE key bidings
(use-package company
  :demand t
  :bind (;; Replace `completion-at-point' and `complete-symbol' with
         ;; `company-manual-begin'. You might think this could be put
         ;; in the `:bind*' declaration below, but it seems that
         ;; `bind-key*' does not work with remappings.
         ([remap completion-at-point] . company-manual-begin)
         ([remap complete-symbol] . company-manual-begin)

         ;; The following are keybindings that take effect whenever
         ;; the completions menu is visible, even if the user has not
         ;; explicitly interacted with Company.

         :map company-active-map

         ;; Make TAB always complete the current selection. Note that
         ;; <tab> is for windowed Emacs and TAB is for terminal Emacs.
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)

         ;; Prevent SPC from ever triggering a completion.
         ("SPC" . nil)

         ;; The following are keybindings that only take effect if the
         ;; user has explicitly interacted with Company.

         :map company-active-map
         :filter (company-explicit-action-p)

         ;; Make RET trigger a completion if and only if the user has
         ;; explicitly interacted with Company. Note that <return> is
         ;; for windowed Emacs and RET is for terminal Emacs.
         ("<return>" . company-complete-selection)
         ("RET" . company-complete-selection)

         ;; We then do the same for the up and down arrows. Note that
         ;; we use `company-select-previous' instead of
         ;; `company-select-previous-or-abort'. I think the former
         ;; makes more sense since the general idea of this `company'
         ;; configuration is to decide whether or not to steal
         ;; keypresses based on whether the user has explicitly
         ;; interacted with `company', not based on the number of
         ;; candidates.

         ("<up>" . company-select-previous)
         ("<down>" . company-select-next))

  :bind* (;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. Here we
          ;; make sure that no minor modes override this keybinding.
          ("M-TAB" . company-manual-begin))

  :diminish company-mode
  :config

  ;; Turn on Company everywhere.
  (global-company-mode 1)

  ;; Show completions instantly, rather than after half a second.
  (setq company-idle-delay 0)

  ;; Show completions after typing a single character, rather than
  ;; after typing three characters.
  (setq company-minimum-prefix-length 1)

  ;; Show a maximum of 10 suggestions. This is the default but I think
  ;; it's best to be explicit.
  (setq company-tooltip-limit 10)

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

  ;; Company appears to override our settings in `company-active-map'
  ;; based on `company-auto-complete-chars'. Turning it off ensures we
  ;; have full control.
  (setq company-auto-complete-chars nil)

  ;; Prevent Company completions from being lowercased in the
  ;; completion menu. This has only been observed to happen for
  ;; comments and strings in Clojure.
  (setq company-dabbrev-downcase nil)

  ;; Only search the current buffer to get suggestions for
  ;; company-dabbrev (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; Make company-dabbrev case-sensitive. Case insensitivity seems
  ;; like a great idea, but it turns out to look really bad when you
  ;; have domain-specific words that have particular casing.
  (setq company-dabbrev-ignore-case nil)

  ;; Make it so that Company's keymap overrides Yasnippet's keymap
  ;; when a snippet is active. This way, you can TAB to complete a
  ;; suggestion for the current field in a snippet, and then TAB to
  ;; move to the next field. Plus, C-g will dismiss the Company
  ;; completions menu rather than cancelling the snippet and moving
  ;; the cursor while leaving the completions menu on-screen in the
  ;; same location.

  (with-eval-after-load 'yasnippet
    ;; TODO: this is all a horrible hack, can it be done with
    ;; `bind-key' instead?

    ;; This function translates the "event types" I get from
    ;; `map-keymap' into things that I can pass to `lookup-key'
    ;; and `define-key'. It's a hack, and I'd like to find a
    ;; built-in function that accomplishes the same thing while
    ;; taking care of any edge cases I might have missed in this
    ;; ad-hoc solution.
    (defun radian--normalize-event (event)
      (if (vectorp event)
          event
        (vector event)))

    ;; Here we define a hybrid keymap that delegates first to
    ;; `company-active-map' and then to `yas-keymap'.
    (setq radian--yas-company-keymap
          ;; It starts out as a copy of `yas-keymap', and then we
          ;; merge in all of the bindings from
          ;; `company-active-map'.
          (let ((keymap (copy-keymap yas-keymap)))
            (map-keymap
             (lambda (event company-cmd)
               (let* ((event (radian--normalize-event event))
                      (yas-cmd (lookup-key yas-keymap event)))
                 ;; Here we use an extended menu item with the
                 ;; `:filter' option, which allows us to
                 ;; dynamically decide which command we want to
                 ;; run when a key is pressed.
                 (define-key keymap event
                   `(menu-item
                     nil ,company-cmd :filter
                     (lambda (cmd)
                       ;; There doesn't seem to be any obvious
                       ;; function from Company to tell whether or
                       ;; not a completion is in progress (à la
                       ;; `company-explicit-action-p'), so I just
                       ;; check whether or not `company-my-keymap'
                       ;; is defined, which seems to be good
                       ;; enough.
                       (if company-my-keymap
                           ',company-cmd
                         ',yas-cmd))))))
             company-active-map)
            keymap))

    ;; The function `yas--make-control-overlay' uses the current
    ;; value of `yas-keymap' to build the Yasnippet overlay, so to
    ;; override the Yasnippet keymap we only need to dynamically
    ;; rebind `yas-keymap' for the duration of that function.
    (defun radian--advice-company-overrides-yasnippet
        (yas--make-control-overlay &rest args)
      "Allow `company' to override `yasnippet'.
This is an `:around' advice for `yas--make-control-overlay'."
      (let ((yas-keymap radian--yas-company-keymap))
        (apply yas--make-control-overlay args)))

    (advice-add #'yas--make-control-overlay :around
                #'radian--advice-company-overrides-yasnippet)))
;;; Prevent suggestions from being triggered automatically. In particular,
  ;;; this makes it so that:
  ;;; - TAB will always complete the current selection.
  ;;; - RET will only complete the current selection if the user has explicitly
  ;;;   interacted with Company.
  ;;; - SPC will never complete the current selection.
  ;;;
  ;;; Based on:
  ;;; - https://github.com/company-mode/company-mode/issues/530#issuecomment-226566961
  ;;; - https://emacs.stackexchange.com/a/13290/12534
  ;;; - http://stackoverflow.com/a/22863701/3538165
  ;;;
  ;;; See also:
  ;;; - https://emacs.stackexchange.com/a/24800/12534
  ;;; - https://emacs.stackexchange.com/q/27459/12534

;; <return> is for windowed Emacs; RET is for terminal Emacs
(dolist (key '("<return>" "RET"))
  ;; Here we are using an advanced feature of define-key that lets
  ;; us pass an "extended menu item" instead of an interactive
  ;; function. Doing this allows RET to regain its usual
  ;; functionality when the user has not explicitly interacted with
  ;; Company.
  (define-key company-active-map (kbd key)
    `(menu-item nil company-complete
                :filter ,(lambda (cmd)
                           (when (company-explicit-action-p)
                             cmd)))))
(define-key company-active-map (kbd "TAB") #'company-complete-selection)
(define-key company-active-map (kbd "SPC") nil)


;; Company appears to override the above keymap based on company-auto-complete-chars.
;; Turning it off ensures we have full control.
(setq company-auto-complete-chars nil)

(global-set-key (kbd "C-c z") 'hydra-typescript/body)
;; active Babel languages
(add-to-list 'load-path "./lisp/ob-typescript.el")
(require 'ob-typescript)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (typescript . t)))
;; add additional languages with '((language . t)))

;; ================= C # =============================
;; (package-install 'omnisharp)
(require 'omnisharp)
(use-package omnisharp)
(eval-after-load
  'company
  '(add-to-list 'company-backends 'company-omnisharp))
(add-hook 'csharp-mode-hook #'company-mode)
(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode +1)
  (set (make-local-variable 'company-backends)
       '((company-omnisharp company-files :with company-yasnippet)
         (company-dabbrev-code company-dabbrev)))
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  ;csharp-mode README.md recommends this too
  ;(electric-pair-mode 1)       ;; Emacs 24
  ;(electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

(add-hook 'csharp-mode-hook #'flycheck-mode)
;;================= C ++ ============================
(setq cquery-executable "/home/tocha/develop/cquery/cquery/build/release/bin/cquery")

(defun cquery//enable ()
  (condition-case nil
      (lsp)
    (user-error nil)))
(require 'cquery)
  (use-package cquery
    :commands lsp
    :init (add-hook 'c-mode-hook #'cquery//enable)
    (add-hook 'c++-mode-hook #'cquery//enable))
(setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
;;================= Real Gud  ============================

(require 'realgud)
;; (add-to-list 'display-buffer-alist
;;              `(,(rx bos "*" "gdb " (+? nonl) "*" eos)
;;                (display-buffer-in-side-window)
;;                (reusable-frames . visible)
;;                (side            . right)
;;                (slot            . 1)
;;                (window-width    . 0.5)))

(defun cb-gud--setup-realgud-windows (&optional buffer)
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
          (src-buffer (realgud-get-srcbuf buffer))
          (cmd-buffer (realgud-get-cmdbuf buffer)))
    (display-buffer cmd-buffer)
    (select-window (display-buffer src-buffer))))

(with-eval-after-load 'realgud
  (defalias 'realgud-window-src-undisturb-cmd #'cb-gud--setup-realgud-windows))
(require 'subr-x)

(defun cb-gud--realgud-command-for-mode (mode)
  (pcase mode
    (`python-mode
      (lambda ()
        (let* ((file (shell-quote-argument (file-relative-name (buffer-file-name))))
               (args (list "python" "-m" "pdb" file)))
          (realgud:run-process "pdb" (buffer-file-name) args 'realgud:pdb-minibuffer-history))))))

(defun realgud ()
  (interactive)
  (let ((buf (current-buffer)))
    (unless (or (realgud-get-cmdbuf) (realgud-get-current-srcbuf))
      (if-let (command (cb-gud--realgud-command-for-mode major-mode))
          (funcall command)
        (error "No realgud support for %s" major-mode)))
    (cb-gud--setup-realgud-windows buf)))
