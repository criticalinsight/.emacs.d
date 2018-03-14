;;; mainline.el --- modeline replacement forked from powerline.el, much simplified

;; Author: Jason Milkins
;; Version: 1.0.2
;; Keywords: statusline / modeline

;;; Add a require to .emacs (or install from elpa/marmalade which will
;;; setup autoloads)
;;;
;;;     (require 'mainline)

(require 'projectile)

(defcustom mainline-color1 "#123550"
  "Mainline color 1 info blocks background")

(defcustom mainline-color2 "#112230"
  "Mainline color 2 vcs info middle block background")

(defcustom mainline-arrow-shape 'arrow
  "Mainline graphic shape")

(set-face-attribute 'mode-line nil
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :box nil)

(scroll-bar-mode -1)

(defun mainline-make-face
    (bg &optional fg)
  (if bg
      (let ((fg mainline-color-fg)
            (cface (intern (concat "mainline-"
                                   bg
                                   "-"
                                   (if fg
                                       (format "%s" fg)
                                     "white")))))
        (make-face cface)
        (if fg
            (if (eq fg 0)
                (set-face-attribute cface nil
                                    :background bg
                                    :box nil)
              (set-face-attribute cface nil
                                  :foreground fg
                                  :background bg
                                  :box nil))
          (set-face-attribute cface nil
                              :foreground "white"
                              :background bg
                              :box nil))
        cface)
    nil))

(defun mainline-make-left
    (string color1 &optional color2 localmap)
  (let ((plface (mainline-make-face color1))
        (arrow  (and color2 (not (string= color1 color2)))))
    (concat
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface))
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if arrow
         (propertize (all-the-icons-alltheicon "wave-left" :v-adjust -0.15)
                     'face `(:height 1.2
                             :family ,(all-the-icons-alltheicon-family)
                             :foreground ,color2
                             :background ,color1))
       ""))))

(defun mainline-make-right
    (string color2 &optional color1 localmap)
  (let ((plface (mainline-make-face color2))
        (arrow  (and color1 (not (string= color1 color2)))))
    (concat
     (if arrow
         (propertize (all-the-icons-alltheicon "wave-right"  :v-adjust -0.0)
                     'face `(:height 1.25
                             :family ,(all-the-icons-alltheicon-family)
                             :foreground ,color1
                             :background ,color2))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface)))))

(defun mainline-make-text
    (string color &optional fg localmap)
  (let ((plface (mainline-make-face color)))
    (if string
        (if localmap
            (propertize string 'face plface 'mouse-face plface 'local-map localmap)
          (propertize string 'face plface))
      "")))

(defun mainline-make (side string color1 &optional color2 localmap)
  (cond ((and (eq side 'right) color2) (mainline-make-right  string color1 color2 localmap))
        ((and (eq side 'left) color2)  (mainline-make-left   string color1 color2 localmap))
        ((eq side 'left)               (mainline-make-left   string color1 color1 localmap))
        ((eq side 'right)              (mainline-make-right  string color1 color1 localmap))
        (t                             (mainline-make-text   string color1 localmap))))

(defmacro defmainline (name string)
  `(defun ,(intern (concat "mainline-" (symbol-name name)))
       (side color1 &optional color2)
     (mainline-make side
                    ,string
                    color1 color2)))

(defmainline major-mode
  (propertize (if (stringp mode-name) mode-name "SGML")
              'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line down-mouse-1]
                             `(menu-item ,(purecopy "Menu Bar") ignore
                                         :filter (lambda (_) (mouse-menu-major-mode-map))))
                           (define-key map [mode-line mouse-2] 'describe-mode)
                           (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                           map)))

(defvar mms-cache (make-hash-table :test 'equal))

(defun mainline-interesting-minor-modes ()
  (let ((cached (gethash minor-mode-alist mms-cache)))
    (or cached
        (substring (propertize
                    (format-mode-line
                     (remove-if (lambda (mm)
                                  (let ((mm-sym (car mm)))
                                    (or (eq mm-sym 'auto-fill-function)
                                        (eq mm-sym 'global-whitespace-mode)
                                        (eq mm-sym 'undo-tree-mode)
                                        (eq mm-sym 'projectile-mode)
                                        (eq mm-sym 'eldoc-mode)
                                        (eq mm-sym 'elisp-slime-nav-mode)
                                        (eq mm-sym 'git-gutter-mode)
                                        (eq mm-sym 'hi-lock-mode)
                                        (eq mm-sym 'wakatime-mode)
                                        (eq mm-sym 'hs-minor-mode))))
                                minor-mode-alist)))
                   1))))

(defun mainline-center-format (str c)
  (let* ((l (length str)))
    (if (< l c)
        (let ((p (/ (- c l) 2)))
          (concat (make-string p 32) str (make-string (- c p l) 32)))
      str)))

(defun mainline-percentage (padding)
  (let ((p (round (/ (* 100.0 (point)) (point-max)))))
    (replace-regexp-in-string "|" "%%"
                              (format (concat "%" (number-to-string padding) "s")
                                      (cond ((= p 0) "Top")
                                            ((> p 98) "Bot")
                                            (t (concat (number-to-string p) "|")))))))

(defun mainline-trimmed-buffer-name (bn n)
  (let* ((l (length bn)))
    (if (> l n)
        (concat ".." (substring bn (- l (- n 2))))
      bn)))

(defun mainline-activate ()
  (setq-default
   mode-line-format
   '("%e" (:eval
           (let* ((buffer-state (format-mode-line "%*"))
                  (modified-icon (cond
                                  ((string= buffer-state "-") (all-the-icons-faicon "toggle-off" :v-adjust -0.05))
                                  ((string= buffer-state "*") (all-the-icons-faicon "toggle-on" :v-adjust -0.05))
                                  ((string= buffer-state "%") (all-the-icons-faicon "lock" :v-adjust -0.05))))
                  (classic-bn-length 20)
                  (ww (window-width))
                  (full-buffer-name (mainline-center-format (buffer-name) classic-bn-length))
                  (position-length 16)
                  (mms (mainline-interesting-minor-modes))
                  (mms-length (if (> (length mms) 0) (length mms) -1))
                  (total-length (+ 3 (length full-buffer-name) 3 position-length 3
                                   (length mode-name)
                                   mms-length 2))
                  (compact? (< ww total-length))
                  (space-for-buffer-name (+ (length full-buffer-name)
                                            (- ww (- total-length mms-length))))
                  (real-buffer-name (if compact?
                                        (mainline-center-format (mainline-trimmed-buffer-name (buffer-name) space-for-buffer-name)
                                                                (min classic-bn-length space-for-buffer-name))
                                      full-buffer-name)))
             (concat
              (mainline-make 'left current-input-method-title mainline-color3)
              (mainline-make 'left modified-icon mainline-color3)
              (mainline-make 'left real-buffer-name mainline-color3 mainline-color1)
              (mainline-make 'left (mainline-percentage 3) mainline-color1)
              (mainline-make 'left "(%4l : %3c)" mainline-color1 mainline-color2)
              (mainline-major-mode 'left mainline-color2)
              (mainline-make 'left "﻿" mainline-color2)
              (if compact? ""
               (mainline-make 'right mms mainline-color1 mainline-color2))))))))
;; (mainline-activate)

(provide 'mainline)
