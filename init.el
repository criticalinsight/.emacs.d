(progn ; packages initialization
  (require 'package)

  (setq load-prefer-newer t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize)

  (when (not (file-exists-p "~/.emacs.d/.initialized"))
    (package-refresh-contents)
    (load-file "~/.emacs.d/installed-packages.el")
    (mapc
     (lambda (package)
       (or (package-installed-p package)
           (package-install package)))
     unlogic-installed-packages)

    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (write-region "" nil "~/.emacs.d/.initialized"))

  (require 'use-package)
  (put 'use-package 'lisp-indent-function 'defun)

  (push :keys (cdr (member :bind use-package-keywords)))

  (defun use-package-handler/:keys
      (name keyword arg rest state &optional override)
    (let ((global-keys nil) (override-keys nil) (curr-group :global)
          (local-map nil) (local-keys nil) (commands nil))
      (while arg
        (cond ((equal (car arg) :local)
               (progn
                 (setq curr-group :local)
                 (setq local-map (intern (concat (symbol-name name) "-mode-map")))
                 (add-to-list 'local-keys (cons local-map nil))
                 (setq arg (cdr arg))))
              ((keywordp (car arg))
               (progn
                 (setq curr-group (car arg))
                 (setq arg (cdr arg))))
              ((symbolp (car arg)) ;; means the mode map for local bindings
               (progn
                 (setq curr-group :local)
                 (setq local-map (car arg))
                 (add-to-list 'local-keys (cons local-map nil))
                 (setq arg (cdr arg))))
              ((stringp (car arg))
               (let ((bind-pair (cons (car arg) (cadr arg))))
                 (case curr-group
                   (:local (push bind-pair (cdr (assoc local-map local-keys))))
                   (:global (add-to-list 'global-keys bind-pair))
                   (:override (add-to-list 'override-keys bind-pair)))
                 (add-to-list 'commands (cadr arg))
                 (setq arg (cddr arg))))
              (t (error (concat "Malformed :keys list, wrong element: "
                                (prin1-to-string (car arg)))))))
      (use-package-concat
       (use-package-process-keywords name
         (use-package-sort-keywords
          (use-package-plist-maybe-put rest :defer t))
         (use-package-plist-append state :commands commands))
       `(,(when local-keys
            `(eval-after-load ',name
               ',(macroexp-progn
                  (mapcar (lambda (bindings)
                            `(bind-keys :map ,(car bindings) ,@(cdr bindings)))
                          local-keys))))
         (ignore (progn
                   ,(when global-keys `(bind-keys ,@global-keys))
                   ,(when override-keys `(bind-keys* ,@override-keys)))))))))

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/sunrise-commander/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-reveal/")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-grammarly-mode/")

(load-file "~/.emacs.d/bindings.el") ;; Load bindings

(load-file "~/.emacs.d/esk.el") ;; Load Emacs starter kit leftovers

(load (setq custom-file (expand-file-name (locate-user-emacs-file "custom.el"))))

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(cd "~") ;; start from userdir

;;; Day-to-day usage

(use-package s :ensure t)

(use-package sudo :commands sudo-find-file)

(use-package stesla
  :bind* (("C-." . stesla-rotate-buffers)
          ("C-," . stesla-rotate-backwards)))

(use-package sunrise-commander
  :bind (("<f7>" . sunrise)
         ("<C-f7>" . sunrise-cd-resize)

         :map sr-mode-map
         (";" . dired-next-line)
         ("C-;" . sr-advertised-find-file)
         ("C-h" . sr-go-home)
         ("j" . ido-sunrise)
         ("C-c C-o" . sr-open-custom-terminal)

         :map sr-tabs-mode-map
         ("C-j" . sr-cycle-bookmark)
         ("C-p" . sr-dired-prev-subdir))
  :config
  (use-package sunrise-x-checkpoints)
  (use-package sunrise-x-loop)
  (use-package sunrise-x-mirror)
  (use-package sunrise-x-tabs)

  (defun sunrise-cd-resize ()
    (interactive)
    (setq sr-panes-height (* 2 (/ (frame-height) 3)))
    (sunrise-cd))

  (setq bookmarks '("~/.emacs.d/" "~/clojure/" "~/projects/grammarly/"))

  (let ((bookmark-counter 0))
    (defun sr-cycle-bookmark ()
      (interactive)
      (let ((bookmark (nth bookmark-counter bookmarks)))
        (setq bookmark-counter (+ bookmark-counter 1))
        (if (>= bookmark-counter (length bookmarks))
            (setq bookmark-counter 0))
        (sr-goto-dir bookmark))))

  (defun sr-go-home ()
    (interactive)
    (sr-goto-dir "~"))

  (defun sr-open-custom-terminal ()
    (interactive)
    (shell-command (concat "urxvt -cd \"" (expand-file-name (sr-choose-cd-target)) "\" -e zsh")))

  (defun ido-sunrise ()
    "Call `sunrise' the ido way.
    The directory is selected interactively by typing a substring.
    For details on keybindings, see `ido-find-file'."
    (interactive)
    (let ((ido-report-no-match nil)
          (ido-auto-merge-work-directories-length -1))
      (ido-file-internal 'read-only 'sr-advertised-find-file nil "Sunrise: " 'dir)))

  (defun sunrise-reset-directories ()
    "To be used if the remembered directories are non-existent."
    (interactive)
    (setq sr-left-directory "~")
    (setq sr-right-directory "~")
    (sunrise))

  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  (use-package javad
    :config
    (use-package javap-mode :ensure t)
    (add-hook 'find-file-hook 'javad-find-class))

  (openwith-mode t))

(use-package multiple-cursors :ensure t
  :bind (("C-M-<mouse-1>" . mc/add-cursor-on-click)
         ("<C-down>" . mc/mark-next-like-this)
         ("<C-M-down>" . mc/mark-next-like-this-symbol)
         ("C-c m" . mc/mark-all-like-this-dwim)))

(use-package phi-search :ensure t
  :bind (("C-c s s" . phi-search)
         ("C-c s r" . phi-search-backward)))

(use-package visual-regexp :ensure t
  :bind (("M-%" . vr/query-replace))
  :init
  ;; Redefine dired search-and-replace
  (defun dired-do-find-regexp-and-replace (from to arg)
    (interactive
     (let ((common
            (query-replace-read-args
             "Query replace regexp in marked files" t t)))
       (list (nth 0 common) (nth 1 common) current-prefix-arg)))
    (save-current-buffer
      (dolist (file (dired-get-marked-files))
        (find-file file)
        (if arg
            (replace-regexp from to nil (point-min) (point-max))
          (query-replace-regexp from to nil (point-min) (point-max)))
        ;; (vr/query-replace from to (point-min) (point-max))
        (save-buffer)
        (kill-buffer)))))

(use-package ediff
  :bind (("C-c d" . ediff-opened-buffers))
  :commands ediff
  :config
  (add-hook 'ediff-startup-hook (lambda () (ediff-toggle-split)))

  (defun ediff-opened-buffers ()
    "Run Ediff on a pair of buffers, BUFFER-A and BUFFER-B."
    (interactive)
    (let* ((bA (ediff-other-buffer ""))
           (bB (progn
                 ;; realign buffers so that two visible bufs will be
                 ;; at the top
                 (save-window-excursion (other-window 1))
                 (ediff-other-buffer bA))))
      (setq job-name 'ediff-buffers)
      (ediff-buffers-internal bA bB nil '(ediff-toggle-split) nil)))

  (defvar ediff-do-hexl-diff nil
    "variable used to store trigger for doing diff in hexl-mode")

  (defadvice ediff-files-internal (around ediff-files-internal-for-binary-files activate)
    "catch the condition when the binary files differ
the reason for catching the error out here (when re-thrown from the inner advice)
is to let the stack continue to unwind before we start the new diff
otherwise some code in the middle of the stack expects some output that
isn't there and triggers an error"
    (let ((file-A (ad-get-arg 0))
          (file-B (ad-get-arg 1))
          ediff-do-hexl-diff)
      (condition-case err
          (progn
            ad-do-it)
        (error
         (if ediff-do-hexl-diff
             (let ((buf-A (find-file-noselect file-A))
                   (buf-B (find-file-noselect file-B)))
               (with-current-buffer buf-A
                 (hexl-mode 1))
               (with-current-buffer buf-B
                 (hexl-mode 1))
               (ediff-buffers buf-A buf-B))
           (error (error-message-string err)))))))

  (defadvice ediff-setup-diff-regions (around ediff-setup-diff-regions-for-binary-files activate)
    "when binary files differ, set the variable "
    (condition-case err
        (progn
          ad-do-it)
      (error
       (setq ediff-do-hexl-diff
             (and (string-match-p "^Errors in diff output.  Diff output is in.*"
                                  (error-message-string err))
                  (string-match-p "^\\(Binary \\)?[fF]iles .* and .* differ"
                                  (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))
                  (y-or-n-p "The binary files differ, look at the differences in hexl-mode? ")))
       (error (error-message-string err))))))

(use-package midnight :demand t
  :config (midnight-delay-set 'midnight-delay "11:59pm"))

(use-package calendar
  :config
  (calendar-set-date-style 'european)
  (setq calendar-week-start-day 1))

(use-package smex :ensure t
  :bind (("M-x" . smex))
  :config
  (setq smex-save-file (concat user-emacs-directory "var/.smex-items"))
  (smex-initialize))

(use-package saveplace :demand t
  :init
  (setq save-place-file (concat user-emacs-directory "var/places"))
  (setq-default save-place t))

(use-package recentf :demand t
  :config
  (recentf-mode 1))

(use-package undo-tree :ensure t :demand t :config (global-undo-tree-mode))

(use-package mainline :demand t ;; custom status line
  :config
  (setq mainline-arrow-shape 'arrow)
  (mainline-activate))

(use-package daycycle :demand t ;; set theme and switch it during the day
  :config
  (defun -theme-set (time)
    (if (eq time 'day)
        (progn
          (setq mainline-color1 "#d6d6d6")
          (setq mainline-color2 "#efefef")
          (setq mainline-color3 "#70c0b1")
          (setq mainline-color-fg "black")
          ;; (set-face-background 'mode-line "#d6d6d6")
          (custom-set-faces
           '(show-paren-match ((t (:foreground "grey70" :bold nil :background "#008800"))))
           '(show-paren-mismatch ((t (:foreground "grey70" :bold nil :background "#880000"))))
           '(mode-line ((t (:background "#d6d6d6" :box nil)))))
          (color-theme-sanityinc-tomorrow-day))
      (setq mainline-color1 "#444444")
      (setq mainline-color2 "#222222")
      (setq mainline-color3 "#293B3A")
      (setq mainline-color-fg "white")
      ;; (set-face-background 'mode-line "#444444")
      (custom-set-faces
       '(show-paren-match ((t (:foreground "#00ff00" :bold t :background unspecified))))
       '(show-paren-mismatch ((t (:foreground "#ff0000" :bold t :background unspecified))))
       '(mode-line ((t (:background "#444444" :box nil)))))
      (color-theme-sanityinc-tomorrow-eighties))
    (setq fci-rule-color "sienna")
    (setq-default fci-rule-color "sienna")
    (custom-set-faces
     `(fringe ((t (:background ,(face-attribute 'default :background)))))))

  (daycycle-init '-theme-set 'auto))

(use-package usefuls :demand t
  :bind* (("C-M-q" . narrow-or-widen-dwim))
  :config
  ;; (usefuls-zone-screensaver)
  )

(use-package vlf :ensure t)

(use-package calc
  :config
  (setq math-additional-units '((GiB "1024 * MiB" "Giga Byte")
                                (MiB "1024 * KiB" "Mega Byte")
                                (KiB "1024 * B" "Kilo Byte")
                                (B nil "Byte")
                                (Gib "1024 * Mib" "Giga Bit")
                                (Mib "1024 * Kib" "Mega Bit")
                                (Kib "1024 * b" "Kilo Bit")
                                (b "B / 8" "Bit"))))

;;; Programming/Version Control

(use-package magit :ensure t
  :bind (("C-x g" . magit-status)
         ("<f8>" . magit-blame))
  :commands (magit-show-commit)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-section-visibility-indicator nil)
  (remove-hook 'magit-status-sections-hook 'magit-insert-stashes)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-stashes 'magit-insert-untracked-files)
  ;; Don't show recent commits
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)

  (defun magit-section-highlight-less (section _)
    (magit-section-case
      ((untracked unstaged staged unpushed unpulled pulls branch)
       (magit-section-make-overlay (magit-section-start   section)
                                   (magit-section-content section)
                                   'magit-section-highlight)
       t)))
  (add-hook 'magit-section-highlight-hook 'magit-section-highlight-less)
  (remove-hook 'magit-status-mode-hook 'whitespace-mode)

  (use-package magit-gh-pulls :ensure t
    :init (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)))

(use-package git-timemachine :ensure t
  :bind (("<C-f8>" . git-timemachine)
         :map git-timemachine-mode-map
         ("c" . git-timemachine-show-commit))
  :config
  (defun git-timemachine-show-commit ()
    (interactive)
    (magit-show-commit (car git-timemachine-revision)))

  (defface git-timemachine-minibuffer-author-face
    '((t (:foreground "firebrick")))
    "How to display the minibuffer detail"
    :group 'git-timemachine)

  ;; Overriden because I don't remember why ¯\_(ツ)_/¯
  (defun git-timemachine--revisions ()
    "List git revisions of current buffers file."
    (if git-timemachine--revisions-cache
        git-timemachine--revisions-cache
      (setq git-timemachine--revisions-cache
            (prog2
                (message "Fetching Revisions...")
                (let ((default-directory git-timemachine-directory)
                      (file git-timemachine-file))
                  (with-temp-buffer
                    (unless (zerop (process-file vc-git-program nil t nil "--no-pager" "log" "--name-only" "--follow" "--date=short" "--pretty=format:%H:%ar:%ad:%an:%s" file))
                      (error "Git log command exited with non-zero exit status for file: %s" file))
                    (goto-char (point-min))
                    (let ((lines)
                          (commit-number (/ (1+ (count-lines (point-min) (point-max))) 3)))
                      (while (not (eobp))
                        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                          (string-match "\\([^:]*\\):\\([^:]*\\):\\(.*\\):\\(.*\\):\\(.*\\)" line)
                          (let ((commit (match-string 1 line))
                                (date-relative (match-string 2 line))
                                (date-full (match-string 3 line))
                                (author (match-string 4 line))
                                (subject (match-string 5 line)))
                            (forward-line 1)
                            (let ((file-name (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                              (push (list commit file-name commit-number date-relative date-full subject author) lines))))
                        (setq commit-number (1- commit-number))
                        (forward-line 2))
                      (nreverse lines))))
              (message "Fetching Revisions...done")))))

  ;; Overriden to customize m
  (defun git-timemachine--show-minibuffer-details (revision)
    "Show details for REVISION in minibuffer."
    (let ((detail (nth 5 revision))
          (date-relative (nth 3 revision))
          (date-full (nth 4 revision))
          (author (nth 6 revision)))
      (message (format "%s (%s) [%s (%s)]" (propertize detail 'face 'git-timemachine-minibuffer-detail-face)
                       (propertize author 'face 'git-timemachine-minibuffer-author-face)
                       date-full date-relative)))))

(use-package git-gutter :ensure t :demand t
  :config
  (global-git-gutter-mode 1)
  (setq-default git-gutter:modified-sign "~"))

(use-package vc-annotate :demand t
  :commands vc-annotate
  :bind (:map vc-annotate-mode-map
         ("c" . vc-annotate-show-commit-at-line))
  :config
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  (defun vc-annotate-show-commit-at-line ()
    (interactive)
    (let* ((rev (car (vc-annotate-extract-revision-at-line)))
           (rev (if (string= (substring rev 0 1) "^")
                    (substring rev 1)
                  rev)))
      (magit-show-commit rev)))

  (defun vc-git-annotate-command (file buf &optional rev)
    (let ((name (file-relative-name file)))
      (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))

  (defun vc-annotate-get-time-set-line-props ()
    (let ((bol (point))
          (date (vc-call-backend vc-annotate-backend 'annotate-time))
          (inhibit-read-only t))
      (cl-assert (>= (point) bol))
      (put-text-property bol (point) 'invisible 'vc-annotate-annotation)
      (let ((boc (point)))
        (save-excursion
          (search-backward-regexp "[0-9][0-9]:[0-9][0-9]:[0-9][0-9] \\+[0-9][0-9][0-9][0-9] +[0-9]+)")
          (when (< (- boc (point)) 40)
            (put-text-property (point) boc 'invisible t))
          (search-backward-regexp "(")
          (let ((paren-point (point)))
            (beginning-of-line)
            (when (> (- paren-point (point) 10))
              (put-text-property (+ (point) 9) paren-point 'invisible t)))))
      date))

  (defvar --vc-annotate-current-rev nil)

  (defun --vc-annotate-post-hook (file rev &rest rst)
    (setq --vc-annotate-current-rev rev)
    (vc-run-delayed
      (unless (active-minibuffer-window)
        (message (vc-git--run-command-string
                  nil "log" "--pretty=format:[%an] %s (%ar)" "-n 1" --vc-annotate-current-rev)))))

  (add-function :after (symbol-function 'vc-annotate) #'--vc-annotate-post-hook))

;;; Programming/Clojure & Lisps

(use-package clojure-mode :ensure t
  :config

  (defun clojure-pretty-fn ()
    (font-lock-add-keywords
     'clojure-mode `(("(\\(fn\\>\\)"
                      (0 (progn (compose-region (match-beginning 1)
                                                (match-end 1)
                                                ?λ) nil)))))
    (font-lock-add-keywords
     'clojure-mode `(("\\(#\\)("
                      (0 (progn (compose-region (match-beginning 1)
                                                (match-end 1) "ƒ")
                                nil)))))
    (font-lock-add-keywords
     'clojure-mode `(("\\(#\\){"
                      (0 (progn (compose-region (match-beginning 1)
                                                (match-end 1) "∪")
                                nil))))))

  (add-hook 'clojure-mode-hook 'clojure-pretty-fn)
  ;; (add-hook 'clojure-mode-hook
  ;;           (lambda ()
  ;;             (put 's/defn 'clojure-doc-string-elt 4)))
  )

(use-package cider :ensure t ; :pin melpa-stable
  :bind (:map
         cider-mode-map
         ("C-c t" . cider-toggle-trace-var)
         ("C-c i" . cider-inspect)
         ;; ("C-c C-z" . cider-switch-to-repl-connection-buffer)
         ("C-c C-e" . cider-eval-last-sexp-in-context)
         ("C-c C-t M-." . cider-test-jump-to-function-test)
         ("C-c C-t a" . cider-test-macroexpand-are)
         ("C-c C-s w" . sesman-pop-browser)

         :map
         cider-repl-mode-map
         ("C-c C-l" . cider-repl-clear-buffer))
  :commands (cider-connect cider-jack-in)
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)

  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  ;; (add-hook 'cider-repl-mode-hook (lambda () (auto-complete-mode -1)))
  ;; (add-hook 'cider-mode-hook (lambda () (auto-complete-mode -1)))

  (use-package cider-inspector :demand t
    :bind (:map cider-inspector-mode-map
                (";" . cider-inspector-next-inspectable-object)
                ("p" . cider-inspector-previous-inspectable-object)
                ("C-;" . cider-inspector-operate-on-point)
                ("C-p" . cider-inspector-pop)
                ("r" . cider-reinspect)))

  (use-package sesman
    :bind (:map sesman-browser-mode-map
                ("q" . sesman-browser-close-browser)
                ("k" . sesman-browser-quit-session))
    :config
    (defvar sesman--window-config-coming-from nil)
    (defun sesman--restore-window-config ()
      (when sesman--window-config-coming-from
        (let ((frame (selected-frame)))
          (unwind-protect
	      (set-window-configuration sesman--window-config-coming-from)
	    (select-frame frame)))
        (setq bs--window-config-coming-from nil)))

    (defun sesman-browser ()
      "Display an interactive session browser.
See `sesman-browser-mode' for more details."
      (interactive)
      (let* ((system (sesman--system))
             (pop-to (called-interactively-p 'any))
             (sessions (sesman-sessions system))
             (cur-session (when pop-to
                            (sesman-current-session 'CIDER)))
             (buff (get-buffer-create (format "*sesman %s browser*" system))))
        (with-current-buffer buff
          (setq-local sesman-system system)
          (sesman-browser-mode)
          (cursor-sensor-mode 1)
          (let ((inhibit-read-only t)
                (sessions (pcase sesman-browser-sort-type
                            ('name (seq-sort (lambda (a b) (string-greaterp (car b) (car a)))
                                             sessions))
                            ('relevance (sesman--sort-sessions system sessions))
                            (_ (error "Invalid `sesman-browser-sort-type'"))))
                (i 0))
            (erase-buffer)
            (insert "\n ")
            (insert (propertize (format "%s Sessions:" system)
                                'face '(bold font-lock-keyword-face)))
            (insert "\n\n")
            (dolist (ses sessions)
              (setq i (1+ i))
              (sesman-browser--insert-session system ses i))
            ;; (when pop-to
            ;;   (pop-to-buffer buff)
            ;;   (sesman-browser--goto-stop (car cur-session)))
            (sesman-browser--sensor-function)))))

    (defun sesman-pop-browser ()
      (interactive)
      (sesman--restore-window-config)
      (setq sesman--window-config-coming-from (current-window-configuration))
      (when (> (window-height) 7)
        (ignore-errors (select-window (split-window-below))))
      (sesman-browser)
      (switch-to-buffer (get-buffer-create (format "*sesman %s browser*"
                                                   (sesman--system)))))

    (defun sesman-browser-close-browser ()
      "Quite session at point."
      (interactive)
      (kill-buffer (current-buffer))
      (sesman--restore-window-config)))

  (defvar cider-inspect-last-inspected-expr nil)

  (defun cider-inspect-last-sexp ()
    "Inspect the result of the the expression preceding point."
    (interactive)
    (let ((expr (cider-last-sexp)))
      (setq cider-inspect-last-inspected-expr expr)
      (cider-inspect-expr expr (cider-current-ns))))

  (defun cider-reinspect ()
    "Like refresh, but re-evaluates the last expression."
    (interactive)
    (cider-popup-buffer-quit-function)
    (cider-inspect-expr cider-inspect-last-inspected-expr (cider-current-ns)))

  ;; (defun cider-switch-to-repl-connection-buffer (&optional set-namespace)
  ;;   (interactive "P")
  ;;   (cider--switch-to-repl-buffer (cider-current-connection) set-namespace))

  ;; Prevent CIDER from jumping to source in other window.
  (defun cider--jump-to-loc-from-info-always-same-window (orig-fn info &rest _)
    (funcall orig-fn info))

  (advice-add 'cider--jump-to-loc-from-info :around #'cider--jump-to-loc-from-info-always-same-window)

  (defun cider--switch-to-nrepl-server-when-jack-in (orig-fn params &rest _)
    (let ((process (funcall orig-fn params)))
      (switch-to-buffer (process-buffer process))
      (insert "\n\n===\n\nPlease wait...\n")
      (beginning-of-buffer)))

  (advice-add 'cider-jack-in-clj :around #'cider--switch-to-nrepl-server-when-jack-in)
  ;; (advice-remove 'cider-jack-in-clj 'cider--jump-to-nrepl-server-when-jack-in)

  (defun cider-test-jump-to-function-test ()
    (interactive)
    (cider-try-symbol-at-point
     "Symbol"
     (lambda (var)
       (let* ((info (cider-var-info var))
              (ns (nrepl-dict-get info "ns"))
              (var (nrepl-dict-get info "name")))
         (cider-find-var nil (concat ns "-test/" var "-test") nil)))))

  ;; Temp hack for setting boot.user namespace on startup.
  (defun cider-repl--set-initial-ns (buffer)
    (with-current-buffer buffer
      (cider-set-buffer-ns "boot.user")))

  (defun cider-test-macroexpand-are ()
    (interactive)
    (cider-macroexpand-1-inplace)
    (cider-macroexpand-1-inplace))

  (defun find-all-files (dir)
    "Open all files and sub-directories below the given directory."
    (interactive "DBase directory: ")
    (let* ((list (directory-files dir t "^[^.]"))
           (files (remove-if 'file-directory-p list))
           (dirs (remove-if-not 'file-directory-p list)))
      (dolist (file files)
        (find-file-noselect file))
      (dolist (dir dirs)
        (find-file-noselect dir)
        (find-all-files dir))))

  (defun cider-boot-get-java-dirs ()
    (interactive)
    (let ((prefix (read-from-string (nrepl-dict-get (cider-nrepl-sync-request:eval "(boot.core/get-env :java-source-path)") "value"))))
      (remove-if-not (lambda (f) (string-suffix-p ".java" f)) (projectile-current-project-files))
      )
    )

  (defun cider-list-java-files-in-project ()
    "Compiles the currently open .java class against the active REPL."
    (interactive)


    (cider-interactive-eval
     (format "(grammarly.boot-tasks.javac/compile-java [] (javax.tools.DiagnosticCollector.)
{\"grammarly.sprawl.Test2\" %s})" (prin1-to-string (buffer-substring-no-properties (point-min) (point-max))))
     nil nil))

  (defun cider-compile-javaclass-remotely ()
    "Compiles the currently open .java class against the active REPL."
    (interactive)
    (cider-interactive-eval
     (format "(grammarly.boot-tasks.javac/compile-java [] (javax.tools.DiagnosticCollector.)
{\"grammarly.sprawl.Test2\" %s})" (prin1-to-string (buffer-substring-no-properties (point-min) (point-max))))
     nil nil))
  )

(use-package company :ensure t :demand t
  :bind (:map company-mode-map
              ("TAB" . company-indent-or-complete-must-have-prefix)
              ("M-SPC" . company-complete)

              :map company-active-map
              ("TAB" . company-complete-selection)
              ("<tab>" . company-complete-selection))
  :config
  (add-hook 'emacs-lisp-mode-hook 'company-mode)

  (defun company-indent-or-complete-must-have-prefix ()
    "Indent the current line or region, or complete the common
part if there is prefix."
    (interactive)
    (if (looking-at "\\_>")
        (company-indent-or-complete-common)
      (call-interactively #'indent-for-tab-command)))

  (use-package company-quickhelp :ensure t :demand t
    :config
    (company-quickhelp-mode 1)))

(use-package clj-refactor :ensure t
  :config
  (cljr-add-keybindings-with-prefix "C-c C-r")
  (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
  (add-hook 'clojure-mode-hook 'yas-minor-mode-on)

  (use-package yasnippet :demand t
    :init
    (define-key yas-minor-mode-map [(tab)] nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)))

(use-package auto-complete-config
  :commands ac-config-default
  :init (add-hook 'lisp-mode-hook 'ac-config-default))

(use-package comment-sexp :demand t)

;;; Programming/Other languages

(use-package cc-mode :demand t
  :config
  (defun java-default-formatting ()
    (c-set-style "java")
    (setq ;; c-basic-offset 4
     tab-width 4
     indent-tabs-mode nil))

  (defun java-clojure-compiler-formatting ()
    (c-set-style "whitesmith")
    (setq ;; c-basic-offset 4
     tab-width 4
     indent-tabs-mode t))

  ;; (add-hook 'java-mode-hook 'java-clojure-compiler-formatting)
  (add-hook 'java-mode-hook 'java-default-formatting)

  (use-package java-snippets :ensure t
    :init
    (add-hook 'java-mode-hook 'yas-minor-mode)))

(use-package web-mode :ensure t
  :config
  (add-hook 'web-mode-hook (lambda () (setq  web-mode-markup-indent-offset 2))))

(use-package dockerfile-mode :ensure t :demand t)

(use-package hcl-mode :ensure t :demand t)

(use-package rockerfile-mode :demand t)

(use-package rust-mode :ensure t)

(use-package go-mode :ensure t
  :config
  (add-hook 'go-mode-hook (lambda () (setq whitespace-style '(face trailing empty)
                                      indent-tabs-mode t
                                      tab-width 4)
                            (whitespace-mode -1))))

(use-package toml-mode :ensure t)

(use-package markdown-mode :ensure t
  :bind (:map markdown-mode-map
         ("C-c C-l" . markdown-smart-insert-link)
         ("C-c C-c C-c" . markdown-insert-gfm-code-block))
  :config
  (use-package markdown-preview-mode :ensure t
    :config
    (setq markdown-preview-stylesheets (list "http://thomasf.github.io/solarized-css/solarized-light.min.css")))

  (defun markdown-smart-insert-link ()
    (interactive)
    (let (link text)
      (if (use-region-p)
          (let ((bounds (markdown-wrap-or-insert "[" "]")))
            (setq link (read-string "Link: "))
            (goto-char (cdr bounds))
            (insert (concat "("
                            (if (string= link "")
                                (buffer-substring-no-properties
                                 (1+ (car bounds)) (1- (cdr bounds)))
                              link)
                            ")")))
        (setq link (read-string "Link: "))
        (setq text (read-string "Text: "))
        (insert (concat "[" (if (string= text "") link text) "](" link ")")))))

  (defvar github-link-to-issue-history ())
  (defun github-link-to-issue ()
    (interactive)
    (let* ((choices (mapcar #'car grammarly-cider-services))
           (minibuffer-completion-table choices)
           (ido-max-prospects 10))
      (let* ((issue (buffer-substring-no-properties (region-beginning) (region-end)))
             (number (substring issue 1))
             (repo
              (ido-completing-read "Github repository: " nil nil nil
                                   nil 'github-link-to-issue-history (car github-link-to-issue-history))))
        (delete-region (region-beginning) (region-end))
        (insert (format "[%s](https://github.com/%s/issues/%s)" issue repo number))))))

(use-package terraform-mode :ensure t)

(use-package zencoding-mode :ensure t)

(use-package sass-mode :ensure t)

(use-package systemd :ensure t)

(use-package elm-mode :ensure t)

(use-package yaml-mode :ensure t)

(use-package groovy-mode :ensure t)

(use-package json-mode :ensure t)

;;; Programming/Miscellaneous

(use-package rainbow-mode :ensure t
  :commands rainbow-turn-on
  :init
  (add-hook 'prog-mode-hook 'rainbow-turn-on)
  (add-hook 'nxml-mode-hook 'rainbow-turn-on)
  (add-hook 'sgml-mode-hook 'rainbow-turn-on)
  (add-hook 'web-mode-hook 'rainbow-turn-on)
  (add-hook 'css-mode-hook 'rainbow-turn-on))

(use-package projectile :ensure t
  :bind (("M-f" . projectile-find-file)

         :map ido-file-dir-completion-map
         ("M-f" . projectile-find-file-from-ido))
  :init
  (defvar last-ido-dir nil)

  ;; (defun find-file-at-point (&optional _)
  ;;   (interactive)
  ;;   (let ((projectile-cached-project-root nil)
  ;;         (projectile-cached-project-name nil)
  ;;         (default-directory last-ido-dir))
  ;;     (projectile-find-file)))

  (defun projectile-find-file-from-ido ()
    "Invoke p-f-file while interactively opening a file in ido."
    (interactive)
    (setq last-ido-dir ido-current-directory)
    (setq ido-exit 'ffap)
    (ido-exit-minibuffer))

  :config
  (projectile-global-mode)
  (add-hook 'find-file-hook
            (lambda ()
              (when (file-remote-p default-directory)
                (setq-local projectile-mode-line "Projectile")))))

(use-package helm-ag :ensure t ;; helm-grep with silver searcher
  :bind* (("M-h" . helm-do-ag-project-root-custom)
          ("M-H" . helm-do-ag))
  :bind (:map helm-ag-map
              ("C-;" . helm-next-line)
              ("M-;" . helm-goto-next-file)
              ("M-p" . helm-goto-precedent-file)
              ("<right>" . helm-execute-persistent-action))
  :config
  (defun helm-do-ag-project-root-custom (sym-at-p)
    (interactive "P")
    (let ((helm-ag-insert-at-point (when sym-at-p 'symbol)))
      (helm-do-ag-project-root))))

(use-package hippie-exp
  :bind (("M-/" . hippie-expand))
  :config
  (dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
    (delete f hippie-expand-try-functions-list))

  ;; Add this back in at the end of the list.
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t))

(use-package idle-highlight-mode :ensure t :demand t
  :commands idle-highlight-mode
  :config
  ;; Hack idle-highlight to support symbols like $SOMETHING.
  (defun idle-highlight-word-at-point ()
    "Highlight the word under the point."
    (if idle-highlight-mode
        (let* ((target-symbol (symbol-at-point))
               (target (symbol-name target-symbol)))
          (idle-highlight-unhighlight)
          (when (and target-symbol
                     (not (in-string-p))
                     (looking-at-p "\\s_\\|\\sw") ;; Symbol characters
                     (not (member target idle-highlight-exceptions)))
            (setq idle-highlight-regexp (concat "\\_<" (regexp-quote target) "\\_>"))
            (highlight-regexp idle-highlight-regexp 'idle-highlight)))))
  (add-hook 'prog-mode-hook 'idle-highlight-mode))

(use-package paren :demand t :config (show-paren-mode 1))

(use-package wakatime-mode :ensure t :demand t
  :config (global-wakatime-mode))

;; (use-package fill-column-indicator :ensure t :demand t
;;   :config
;;   (add-hook 'prog-mode-hook 'fci-mode)
;;   (add-hook 'clojure-mode-hook 'fci-mode))

(use-package hideshow :ensure t :demand t
  :config
  (defun hs-clojure-hide-namespace-and-folds ()
    "Hide the first (ns ...) expression in the file, and also all
the (^:fold ...) expressions."
    (interactive)
    (hs-life-goes-on
     (save-excursion
       (goto-char (point-min))
       (when (ignore-errors (re-search-forward "^(ns "))
         (hs-hide-block))

       (while (ignore-errors (re-search-forward "\\^:fold"))
         (hs-hide-block)
         (next-line))

       (beginning-of-buffer)
       (while (ignore-errors (re-search-forward "^(s/fdef"))
         (hs-hide-block)
         (next-line)))))

  (defun hs-clojure-mode-hook ()
    (interactive)
    (hs-minor-mode 1)
    (hs-clojure-hide-namespace-and-folds))

  (add-hook 'c-mode-common-hook   'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'java-mode-hook       'hs-minor-mode)
  (add-hook 'lisp-mode-hook       'hs-minor-mode)
  (add-hook 'perl-mode-hook       'hs-minor-mode)
  (add-hook 'sh-mode-hook         'hs-minor-mode)
  (add-hook 'clojure-mode-hook    'hs-clojure-mode-hook)
  )

;;; Writing

(use-package unicode-fonts :ensure t :demand t
  :config
  (unicode-fonts-setup))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (:map org-mode-map
         ("C-'" . forward-char))
  :config
  (use-package ox-reveal
    :demand t
    :config
    (setq org-reveal-root (expand-file-name "~/Software/reveal-js")))
  (setq org-inhibit-startup-visibility-stuff t))

(use-package centered-window-mode :ensure t
  :config
  (defvar-local hidden-mode-line-mode nil)

  (define-minor-mode hidden-mode-line-mode
    "Minor mode to hide the mode-line in the current buffer."
    :init-value nil
    :global t
    :variable hidden-mode-line-mode
    :group 'editing-basics
    (if (not (null mode-line-format))
        (progn
          (setq hide-mode-line mode-line-format
                mode-line-format nil)
          (setq-default mode-line-format nil))
      (setq-default mode-line-format hide-mode-line)
      (setq mode-line-format hide-mode-line
            hide-mode-line nil))
    (force-mode-line-update)
    ;; Apparently force-mode-line-update is not always enough to
    ;; redisplay the mode-line
    (redraw-display)
    (when (and (called-interactively-p 'interactive)
               hidden-mode-line-mode)
      (run-with-idle-timer
       0 nil 'message
       (concat "Hidden Mode Line Mode enabled.  "
               "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

  (defun serenity-mode ()
    (interactive)
    (if (null mode-line-format)
        (progn
          (centered-window-mode -1)
          (hidden-mode-line-mode -1)
          (set-fringe-mode "default"))
      (progn
        (centered-window-mode 1)
        (hidden-mode-line-mode 1)
        (set-fringe-mode "no-fringes")))))

(use-package flyspell :ensure t
  :config
  (flyspell-mode 0) ;; Off flyspell by default
  (add-hook 'org-mode-hook (lambda () (flyspell-mode 1) (flyspell-buffer)))
  (add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1) (flyspell-buffer)))

  ;; Enable flyspell-prog-mode for programming languages
  (add-hook 'clojure-mode-hook 'flyspell-prog-mode)
  (add-hook 'java-mode-hook 'flyspell-prog-mode)
  (add-hook 'lua-mode-hook 'flyspell-prog-mode)
  (add-hook 'lisp-mode-hook 'flyspell-prog-mode))

(use-package ascii-one-liners :demand t)

(defun grammarly ()
  (interactive)
  (find-file "~/grammarly/work.org"))

(defun work-new-day ()
  (interactive)
  (end-of-buffer)
  (org-insert-heading-respect-content)
  (org-metaright)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)"))
  (org-metaleft)
  (reindent-then-newline-and-indent)
  (reindent-then-newline-and-indent)
  (org-cycle)
  (insert "- "))

(defun phantasm-connect ()
  (interactive)
  (cider-connect "phantasm-dev.grammarly.io" 9999 nil))

(setq grammarly-cider-services
      '(("phantasm" . "phantasm.gnlp.io")
        ("feedbacks-pipeline" . "feedbacks.cpgr.io")
        ("thesaurus" . "thesaurus.cpgr.io")
        ("livestream" . "livestream.cpgr.io")))

(defvar grammarly-cider-connect-history ())

(defun grammarly-cider-connect-read-server-name ()
  (let* ((choices (mapcar #'car grammarly-cider-services))
         (minibuffer-completion-table choices)
         (ido-max-prospects 10))
    (let ((answer
           (ido-completing-read "Server: " choices nil nil
                                nil 'grammarly-cider-connect-history (car choices))))
      (if-let (server (cdr (assoc answer grammarly-cider-services)))
          server
        answer))))

(defun grammarly-cider-connect ()
  (interactive)
  (let* ((server (grammarly-cider-connect-read-server-name)))
    (cider-connect (list :host server :port 9999))))

;; Customizations

(progn ;; Smooth scrolling
  (setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering
  (setq mouse-wheel-progressive-speed nil) ;; on a long mouse scroll keep scrolling by 1 line
  )

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Don't litter my fs tree
(setq backup-directory-alist '(("." . "~/.local/share/emacs-saves"))
      auto-save-file-name-transforms '((".*" "~/.local/share/emacs-saves/" t)))

;; Advice yanking to auto-indent yank content
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode  lua-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-generic-program "open")

;; Disabled

;; (use-package slime :ensure t
;;   :commands slime
;;   :config
;;   (setq-default slime-lisp-implementations
;;                 '((sbcl ("sbcl" "--dynamic-space-size" "9500"))))

;;   (use-package ac-slime :ensure t :demand t
;;     :init
;;     (add-hook 'slime-mode-hook 'set-up-slime-ac)
;;     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;;     (eval-after-load "auto-complete"
;;       '(add-to-list 'ac-modes 'slime-repl-mode)))
;;   (setq slime-contribs '(slime-asdf))
;;   (slime-setup '(slime-autodoc))
;;   (slime-setup '(slime-fancy slime-scratch slime-editing-commands
;;                              slime-fuzzy slime-repl slime-fancy-inspector
;;                              slime-presentations slime-asdf
;;                              slime-indentation))
;;   (require 'slime-autoloads))

;; (use-package ivy :ensure t :demand t
;;   :bind (("M-x" . counsel-M-x)
;;          ;; ("C-x C-f" . counsel-find-file)
;;          )
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq enable-recursive-minibuffers t)
;;   ;; (global-set-key "\C-s" 'swiper)
;;   ;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
;;   ;; (global-set-key (kbd "<f6>") 'ivy-resume)
;;   ;; (global-set-key (kbd "M-x") 'counsel-M-x)
;;   ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;   ;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;   ;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;   ;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;;   ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;   ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;;   ;; (global-set-key (kbd "C-c g") 'counsel-git)
;;   ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;;   ;; (global-set-key (kbd "C-c k") 'counsel-ag)
;;   ;; (global-set-key (kbd "C-x l") 'counsel-locate)
;;   ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;   ;; (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
;;   )

;; Local Variables:
;; eval: (hs-hide-all)
;; End:
