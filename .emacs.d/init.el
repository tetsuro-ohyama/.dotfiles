;;; 
;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;; -------------------------------------------------

;; Add directory to load-path
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))
(add-to-load-path "elisp" "elpa") ; e.g. (add-to-load-path "elisp" "xxx" "xxx")

;; package.el
(when (require 'package nil t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  )
(package-initialize)

;;; 
;;; Key bindings
;;; -----------------------------------------------

(global-set-key (kbd "C-r")     'replace-string)

(global-set-key (kbd "C-c s")   'sr-speedbar-toggle)

;;;
;;; Color theme
;;; -----------------------------------------------

(let ((class '((class color) (min-colors 89))))
  (custom-set-faces
   ;; Ensure sufficient contrast on 256-color xterms.
   `(default ((((class color) (min-colors 4096))
	       (:background "#2d3743" :foreground "#e1e1e0"))
	      (,class
	       (:background "#000000" :foreground "#e1e1e0"))))
   `(cursor ((,class (:background "#819170"))))
   ;; Highlighting faces
   `(fringe ((,class (:background "#2e3748"))))
   `(highlight ((,class (:background "#035f56"))))
   `(region ((,class (:background "#2d4948" :foreground "#000000"))))
   `(isearch ((,class (:background "#fcffad" :foreground "#000000"))))
   `(lazy-highlight ((,class (:background "#338f86"))))
   `(trailing-whitespace ((,class (:background "#ff4242"))))
   ;; Mode line faces
   `(mode-line ((,class (:background "#212931" :foreground "#eeeeec"))))
   `(mode-line-inactive
     ((,class (:background "#878787" :foreground "#eeeeec"))))
   `(header-line ((,class (:background "#e5e5e5" :foreground "#333333"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground "#729fcf" :weight bold))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#23d7d7"))))
   `(font-lock-comment-face ((,class (:foreground "#74af68"))))
   `(font-lock-constant-face ((,class (:foreground "#008b8b"))))
   `(font-lock-function-name-face
     ((,class (:foreground "#00ede1" :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground "#ffad29" :weight bold))))
   `(font-lock-string-face ((,class (:foreground "#e67128"))))
   `(font-lock-type-face ((,class (:foreground "#34cae2"))))
   `(font-lock-variable-name-face ((,class (:foreground "#dbdb95"))))
   `(font-lock-warning-face ((,class (:foreground "#ff4242" :weight bold))))
   ;; Buttons and links
   `(button ((,class (:underline t))))
   `(link ((,class (:foreground "#59e9ff" :underline t))))
   `(link-visited ((,class (:foreground "#ed74cd" :underline t))))
   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:foreground "#ff4242" :weight bold))))
   `(gnus-group-news-1-low ((,class (:foreground "#ff4242"))))
   `(gnus-group-news-2 ((,class (:foreground "#00ede1" :weight bold))))
   `(gnus-group-news-2-low ((,class (:foreground "#00ede1"))))
   `(gnus-group-news-3 ((,class (:foreground "#23d7d7" :weight bold))))
   `(gnus-group-news-3-low ((,class (:foreground "#23d7d7"))))
   `(gnus-group-news-4 ((,class (:foreground "#74af68" :weight bold))))
   `(gnus-group-news-4-low ((,class (:foreground "#74af68"))))
   `(gnus-group-news-5 ((,class (:foreground "#dbdb95" :weight bold))))
   `(gnus-group-news-5-low ((,class (:foreground "#dbdb95"))))
   `(gnus-group-news-low ((,class (:foreground "#008b8b"))))
   `(gnus-group-mail-1 ((,class (:foreground "#ff4242" :weight bold))))
   `(gnus-group-mail-1-low ((,class (:foreground "#ff4242"))))
   `(gnus-group-mail-2 ((,class (:foreground "#00ede1" :weight bold))))
   `(gnus-group-mail-2-low ((,class (:foreground "#00ede1"))))
   `(gnus-group-mail-3 ((,class (:foreground "#23d7d7"  :weight bold))))
   `(gnus-group-mail-3-low ((,class (:foreground "#23d7d7"))))
   `(gnus-group-mail-low ((,class (:foreground "#008b8b"))))
   `(gnus-header-content ((,class (:weight normal :foreground "#ffad29"))))
   `(gnus-header-from ((,class (:foreground "#e67128" :weight bold))))
   `(gnus-header-subject ((,class (:foreground "#dbdb95"))))
   `(gnus-header-name ((,class (:foreground "#00ede1"))))
   `(gnus-header-newsgroups ((,class (:foreground "#e67128"))))
   ;; Message faces
   `(message-header-name ((,class (:foreground "#ffad29" :weight bold))))
   `(message-header-cc ((,class (:foreground "#e67128"))))
   `(message-header-other ((,class (:foreground "#e67128"))))
   `(message-header-subject ((,class (:foreground "#dbdb95"))))
   `(message-header-to ((,class (:foreground "#00ede1"))))
   `(message-cited-text ((,class (:foreground "#74af68"))))
   `(message-separator ((,class (:foreground "#23d7d7"))))))
(global-hl-line-mode)

;;; 
;;; Mode Line
;;; -----------------------------------------------

;; git-status
(require 'git-status)

;; mode-line-setup
(setq-default
 mode-line-position
 '(
   " "
   ;; Position, including warning for 80 columns
   (:propertize "%4l" face mode-line-position-face)
   (:propertize "/" face mode-line-delim-face-1)
   (:eval
    (number-to-string (count-lines (point-min) (point-max))))
   " "
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   " "
   ))

;; form
(setq-default
 mode-line-format
 '("%e"
   mode-line-mule-info
   ;; emacsclient [default -- keep?]
   mode-line-client
   mode-line-remote
   ;; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize "RO" 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize "**" 'face 'mode-line-modified-face))
          (t "--")))
   " "
   ;; evil-mode-line-tag
   mode-line-position
   ;; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 20))
                face mode-line-folder-face)
   (:propertize "%b" face mode-line-filename-face)
   ;; narrow [default -- keep?]
   " %n"
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   " %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%]"
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   " "
   (:propertize mode-line-process
                face mode-line-process-face)
   " "
   user-login-name
   "@"
   system-name
   " "
   (global-mode-string global-mode-string)
   ;; " "
   ;; nyan-mode uses nyan cat as an alternative to %p
   ;; (:eval (when nyan-mode (list (nyan-create))))
   ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(set-face-attribute 'mode-line nil
		    :foreground "gray80" :background "gray15"
		    :inverse-video nil
		    :weight 'normal
		    :height 1.0
		    :box '(:line-width 2 :color "gray10" :style nil))
(set-face-attribute 'mode-line-inactive nil
		    :foreground "gray80" :background "gray40"
		    :inverse-video nil
		    :weight 'extra-light
		    :height 1.0
		    :box '(:line-width 2 :color "gray30" :style nil))
;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)
(make-face 'mode-line-delim-face-1)

(set-face-attribute 'mode-line-read-only-face nil
		    :inherit 'mode-line-face
		    :foreground "#4271ae"
		    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
		    :inherit 'mode-line-face
		    :foreground "#c82829"
		    :background "#ffffff"
		    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
		    :inherit 'mode-line-face
		    :weight 'extra-light
		    :height 0.8
		    :foreground "gray90")
(set-face-attribute 'mode-line-filename-face nil
		    :inherit 'mode-line-face
		    :foreground "#eab700"
		    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
		    :inherit 'mode-line-face
		    :family "Menlo")
(set-face-attribute 'mode-line-mode-face nil
		    :inherit 'mode-line-face
		    :foreground "white")
(set-face-attribute 'mode-line-minor-mode-face nil
		    :inherit 'mode-line-mode-face
		    :foreground "gray60"
		    :height 0.8)
(set-face-attribute 'mode-line-process-face nil
		    :inherit 'mode-line-face
		    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
		    :inherit 'mode-line-position-face
		    :foreground "black" :background "#eab700")
(set-face-attribute 'mode-line-delim-face-1 nil
		    :inherit 'mode-line-face
		    :foreground "white")

;;;
;;; Eshell
;;; -----------------------------------------------

;; prompt文字列
(setq eshell-prompt-function
      (lambda ()
        (concat
;;         "[" (format-time-string "%Y/%m/%d(%a) %H:%M") "]" ;; 時間
;;         " "
;;         (user-login-name) "@" (system-name) ;; ユーザ名@ホスト名
;;         ":"
         (let ((pwd (eshell/pwd))
               (homestring (directory-file-name (expand-file-name (getenv "HOME"))))
               )
           (let ((homelength (length homestring))
                 (pwdlength (length pwd)))
             (if (>= pwdlength homelength)
                 (let ((subhome (substring pwd 0 homelength)))
                   (if (string= subhome homestring)
                       (concat "~" (substring pwd homelength (length pwd)))
                     pwd
                    )
                   )
               pwd)
           ))
;;         (vc-git-mode-line-string (eshell/pwd))
         (curr-dir-git-branch-string (eshell/pwd))
;;         (vc-git-state (eshell/pwd))
;;         "[" (vc-git-registered (eshell/pwd)) "]"
         " "
         (if (= (user-uid) 0)
             "#"
           "$")
         " "
         )))
(setq eshell-prompt-regexp "^[^#$]*[$#] ")

(defun chomp (str)
  (replace-regexp-in-string "[\n\r]+$" "" str))

(defun gitroot ()
  (interactive)
  (cd (chomp
          (shell-command-to-string "git rev-parse --show-toplevel")))
  )

(require 'vc-git)
(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat "["
              (if (> (length git-output) 0)
                    (concat
                     (substring git-output 0 -1)
                     (shell-command-to-string "[[ $(git status | grep \"nothing to commit\") == \"\" ]] && echo -n \"*\"")
                    )
                "(no branch)")
              "]") 'face `(:foreground "green"))
      )))

;; Emacs 起動時に Eshell を起動
(add-hook 'after-init-hook  (lambda ()
                              (cd "~")
                              (eshell)
                              ))

;; 補完時に大文字小文字を区別しない
(setq eshell-cmpl-ignore-case t)
;; 確認なしでヒストリ保存
(setq eshell-ask-to-save-history (quote always))
;; 1コマンドごとにヒストリ保存
(defadvice eshell-send-input (after eshell-send-input-after-advice)
  (eshell-save-some-history)
  (eshell-save-some-last-dir))

(ad-activate 'eshell-send-input)
;; 補完時にサイクルする
;;(setq eshell-cmpl-cycle-completions t)
(setq eshell-cmpl-cycle-completions nil)
;;補完候補がこの数値以下だとサイクルせずに候補表示
;;(setq eshell-cmpl-cycle-cutoff-length 5)
;; 履歴で重複を無視する
(setq eshell-hist-ignoredups t)
;; 文字を入力すれば末尾へジャンプ
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-show-maximum-output t)
(setq comint-scroll-to-bottom-on-output 'all)

;; promptのあとも補完可能に
(defun pcomplete/sudo ()
  "Completion rules for the `sudo' command."
  (let ((pcomplete-help "complete after sudo"))
    (pcomplete-here (pcomplete-here (eshell-complete-commands-list)))))

;; トグルする設定
(defun my-toggle-term ()
  "eshell と直前のバッファを行き来する。C-u 付きで呼ぶと 今いるバッファと同じディレクトリに cd して開く"
  (interactive)
  (let ((ignore-list '("*Help*" "*Minibuf-1*" "*Messages*" "*Completions*"
                       "*terminal<1>*" "*terminal<2>*" "*terminal<3>*"))
        (dir default-directory))
    (labels
        ((_my-toggle-term (target)
                          (if (null (member (buffer-name (second target)) ignore-list))
                              (if (equal eshell-buffer-name (buffer-name (window-buffer)))
                                  (switch-to-buffer (second target))
                                (switch-to-buffer eshell-buffer-name)
                                (when current-prefix-arg
                                  (cd dir)
                                  (eshell-interactive-print (concat "cd " dir "\n"))
                                  (eshell-emit-prompt)))
                            (_my-toggle-term (cdr target)))))
      (_my-toggle-term (buffer-list)))))
(global-set-key (kbd "C-z") 'my-toggle-term)

;; eshellを新規で開く
(defun eshell-add-new-buffer (arg)
  (interactive "p")
  (case arg
    (4 (let ((input (read-string "eshell buffer name: ")))
         (my-eshell-create input))
       )
    (t (my-eshell-create "")))
  )
(defun my-eshell-create (input)
  (let ((bname (if (string= input "") "" (concat "<" input ">"))))
    (let ((buf (generate-new-buffer (concat eshell-buffer-name bname))))
      (pop-to-buffer buf)
      (unless (fboundp 'eshell-mode)
        (error "`eshell-auto' must be loaded before Eshell can be used"))
      (unless (eq major-mode 'eshell-mode)
        (eshell-mode))
      buf
      ))
  )

;; ファイルの場所へcdする
(defun eshell-jump-to-current-directory ()
  (interactive)
  (let ((dir default-directory))
    (eshell)
    (cd dir)
    (eshell-interactive-print (concat "cd " dir "\n"))
    (eshell-emit-prompt))
  )

;; キーバインドの変更
(add-hook 'eshell-mode-hook
          '(lambda ()
             (progn
;;             (my-ac-eshell-mode)
               (define-key eshell-mode-map (kbd "C-a") 'eshell-bol)
               (define-key eshell-mode-map [up] 'eshell-previous-matching-input-from-input)
               (define-key eshell-mode-map [down] 'eshell-next-matching-input-from-input)
               (define-key eshell-mode-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
               (define-key eshell-mode-map (kbd "C-n") 'eshell-next-matching-input-from-input)
               (define-key eshell-mode-map (kbd "M-p") 'previous-line)
               (define-key eshell-mode-map (kbd "M-n") 'next-line)
               (define-key eshell-mode-map (kbd "C-c C-p") 'anything-eshell-history)
               (define-key eshell-mode-map (kbd "C-c C-n") 'anything-esh-pcomplete)
               )
             ))

;;; 
;;; Others
;;; -----------------------------------------------

;; indent
(setq tab-width 4)
(setq indent-tabs-mode nil)

;; Don't make backup files
(setq make-backup-files nil) ; *.~
(setq auto-save-default nil) ; #*.#
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;; auto revert-buffer
(global-auto-revert-mode 1)

;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(custom-set-variables '(read-file-name-completion-ignore-case t))

;; 同名ファイルをフォルダで識別
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; 行番号表示
(global-linum-mode)
(setq linum-format "%4d")

;; ファイルを管理者権限で開く
(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))


;;;; ----------------------------------------------------------------------
;;;; Packages
;;;; ----------------------------------------------------------------------

;;;
;;; undo-tree.el
;;; -----------------------------------------------
(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  (global-set-key (kbd "C--") 'undo-tree-undo)
  (global-set-key (kbd "M--") 'undo-tree-redo)
)

;;;
;;; anything.el
;;; -----------------------------------------------
(when (require 'anything-config nil t)
  ;; key bindings
  (global-set-key (kbd "C-x C-b") 'anything-filelist+)
  (global-set-key (kbd "M-y") 'anything-show-kill-ring)
  (global-set-key (kbd "M-x") 'anything-M-x)

  ;; filelist
  (setq anything-c-filelist-file-name "/tmp/all.filelist")

  ;; anything-kill-ring
  (setq kill-ring-max 50)
  (setq anything-kill-ring-threshold 5)

  ;; delay
  (setq anything-idle-delay 0.1)
  (setq anything-input-idle-delay 0.1)

)

;;;
;;; auto-complete.el
;;; -----------------------------------------------
(when (require 'auto-complete nil t)
  (require 'auto-complete-config)
  
  ;; キーバインド
  (setq ac-use-menu-map t)
  (define-key ac-menu-map (kbd "C-n") 'ac-next)
  (define-key ac-menu-map (kbd "C-p") 'ac-previous)
  (define-key ac-menu-map (kbd "TAB") 'ac-next)
  (define-key ac-menu-map (kbd "S-TAB") 'ac-previous)
  (define-key ac-mode-map (kbd "M-/") 'auto-complete)
  (ac-set-trigger-key "TAB")
  ;; 自動的に補完開始
  (setq ac-auto-start t)
  ;; 補完メニューを自動表示
  (setq ac-auto-show-menu t)
  ;; 最適なカラム計算をオフ
  ;;(setq popup-use-optimized-column-computation nil)
  ;; ツールチップの表示なし
  (setq ac-use-quick-help nil)
  ;; do i what mean
  ;;(setq ac-dwim t)
  ;; 大文字小文字を区別しない
  (setq ac-ignore-case t)
  ;; lisp編集情報源
  (add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols)))
  ;; ファイル名情報
  (setq-default ac-sources '(ac-source-yasnippet
                             ac-source-abbrev
                             ac-source-words-in-same-mode-buffers
                             ac-source-filename
                             ac-source-dictionary))
  ;; 起動モード
  (global-auto-complete-mode t)
  (add-to-list 'ac-modes 'text-mode)
  (add-to-list 'ac-modes 'fundamental-mode)
  (add-to-list 'ac-modes 'web-mode)
  ;; eshell-mode
  ;; (add-to-list 'ac-modes 'eshell-mode)
  ;; (ac-define-source pcomplete
  ;;   '((candidates . pcomplete-completions)))
  ;; (defvar my-eshell-ac-sources
  ;;   '(ac-source-pcomplete
  ;;     ac-source-filename
  ;;     ac-source-words-in-same-mode-buffers
  ;;     ac-source-dictionary))
  ;; (defun ac-eshell-mode-setup ()
  ;;   (setq-default ac-sources my-eshell-ac-sources))
  ;; (add-hook 'eshell-mode-hook 'ac-eshell-mode-setup)

  ;; css-mode
  (add-to-list 'ac-modes 'css-mode)
  (defun ac-css-mode-setup ()
    (setq-default ac-sources (append '(ac-source-css-property) ac-sources)))
  (add-hook 'css-mode-hook 'ac-css-mode-setup)

  ;; php-mode
  (require 'php-mode)
  (add-to-list 'ac-modes 'php-mode)
  (require 'php-completion)
  (php-completion-mode t)
  (defun ac-php-mode-setup ()
    (setq-default ac-sources (append '(ac-source-php-completion) ac-sources)))
  (add-hook 'php-mode-hook 'ac-php-mode-setup)

  ;; (when (require 'auto-complete-latex nil t)
  ;;    (setq ac-l-dict-directory "~/.emacs.d/elisp/auto-complete/ac-l-dict/")
  ;;    (add-to-list 'ac-modes 'latex-mode)
  ;;    (add-hook 'LaTeX-mode-hook 'ac-l-setup))

  ;; lookで英単語補完
  (when (executable-find "look")
    (defun my-ac-look ()
      "look コマンドの出力をリストで返す"
      (interactive)
      (unless (executable-find "look")
        (error "look コマンドがありません"))
      (let ((search-word (thing-at-point 'word)))
        (with-temp-buffer
          (call-process-shell-command "look" nil t 0 search-word)
          (split-string-and-unquote (buffer-string) "\n"))))

    (defun ac-complete-look ()
      (interactive)
      (let ((ac-menu-height 50)
            (ac-candidate-limit t))
        (auto-complete '(ac-source-look))))

    (defvar ac-source-look
      '((candidates . my-ac-look)
        (requires . 2)))  ;; 2文字以上ある場合にのみ対応させる

    (global-set-key (kbd "C-c l") 'ac-complete-look))
)

;;;
;;; web-mode.el
;;; -----------------------------------------------
(when (require 'web-mode nil t)
  (defun web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-css-indent-offset    4)
    (setq web-mode-code-indent-offset   4)
    (setq web-mode-style-padding  0)
    (setq web-mode-script-padding 0)
    (setq web-mode-block-padding  0)
    (setq web-mode-enable-auto-pairing nil)
    )
  (add-hook 'web-mode-hook 'web-mode-hook)
  
  ;; color 
  (custom-set-faces
   '(web-mode-doctype-face
     ((t (:foreground "#82AE46"))))
   '(web-mode-html-tag-face
    ((t (:foreground "#E6B422" :weight bold))))
   '(web-mode-html-attr-name-face
   ((t (:foreground "#C97586"))))
   '(web-mode-html-attr-value-face
     ((t (:foreground "#82AE46"))))
   '(web-mode-comment-face
     ((t (:foreground "#D9333F"))))
   '(web-mode-server-comment-face
     ((t (:foreground "#D9333F"))))
   '(web-mode-css-rule-face
     ((t (:foreground "#A0D8EF"))))
   '(web-mode-css-pseudo-class-face
   ((t (:foreground "#FF7F00"))))
   '(web-mode-css-at-rule-face
     ((t (:foreground "#FF7F00"))))
   )
)

;;;
;;; php-mode.el
;;; -----------------------------------------------
(when (require 'php-mode nil t)
  (add-hook 'php-mode-hook
	    (lambda ()
	      (defun ywb-php-lineup-arglist-intro (langelem)
		(save-excursion
		  (goto-char (cdr langelem))
		  (vector (+ (current-column) c-basic-offset))))
	      (defun ywb-php-lineup-arglist-close (langelem)
		(save-excursion
		  (goto-char (cdr langelem))
		  (vector (current-column))))
	      (c-set-style "stroustrup")
	      (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
	      (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)
	      (c-set-offset 'arglist-cont-nonempty' 4)
	      (c-set-offset 'case-label' 4)
	      (make-local-variable 'tab-width)
	      (make-local-variable 'indent-tabs-mode)
	      (setq tab-width 4)
	      (setq indent-tabs-mode nil)))
)

;;;
;;; sr-speedbar.el
;;; -----------------------------------------------
(when (require 'sr-speedbar)
  (setq sr-speedbar-right-side nil)
  (setq speedbar-directory-unshown-regexp "^\\'")
  (setq speedbar-use-images nil)
  (custom-set-variables
   '(speedbar-show-unknown-files t)
   )
)
