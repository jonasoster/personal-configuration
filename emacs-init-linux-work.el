;;;;; Emacs configuration 
;;;;; Jonas Öster

;; Start Emacs server
(server-start)

;; Show me beautiful colours
(setq font-lock-maximum-decoration 2)
(global-font-lock-mode 1)

;; I will need this to configure keybindings later
(require 'dired)

;; I hate tabs
;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 8)

;; I know what I'm doing, mostly
(setq-default dired-recursive-deletes 'top)
(setq-default dired-recursive-copies 'always)

;; I want to use all the good stuff
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Let me see my selection 
(transient-mark-mode t)

;; Let me see matching parentheses
(show-paren-mode 1)

;; Make the Emacs window big enough
;(set-frame-height (selected-frame) 47)
;(set-frame-width (selected-frame) 95)

;; Pages make it easier to navigate
(require 'page-ext)

;; Do the right thing when I press enter
(substitute-key-definition
 'newline 'newline-and-indent (current-global-map))

;; always end a file with a newline
(setq require-final-newline t)

;; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; Move by logical lines
(setq line-move-visual nil)

;; Switch buffers
(global-set-key [(meta o)]
  (lambda ()
    (interactive)
    (switch-to-buffer (other-buffer))))

;; I never understood what the font locking in shell mode
;; is supposed to show me.
(add-hook 'shell-mode-hook
          (lambda ()
            (font-lock-mode -1)))

;; This also helps in shell mode
(setenv "PAGER" "cat")

;; Needed for paying work...
(require 'cc-mode)

;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
(setq c-recognize-knr-p nil)

;; I got a REAL job! Yeah!
(setq c-default-style "linux")

;; Setup formatting in C mode
(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; (setq c-basic-offset 3)
	    ;; (c-set-offset 'substatement-open 0)
	    ;; (c-set-offset 'inextern-lang 0)
	    ;; (c-set-offset 'brace-list-open 0)
	    ;; (c-set-offset 'arglist-cont-nonempty '+)
	    ;; (c-set-offset 'arglist-intro '+)
	    (define-key c-mode-base-map "\C-m" 'newline-and-indent)))
       
;; Use the nice extra modifiers available
(setq w32-apps-modifier 'super)
(setq w32-lwindow-modifier 'hyper)
(setq w32-rwindow-modifier 'alt)
;; Don't pass them on to Windows
(setq w32-pass-rwindow-to-system nil)
(setq w32-pass-lwindow-to-system nil)
(setq w32-pass-alt-to-system nil)

;; Stolen from the wiki:
;; Copy current buffer path to kill ring C-c n, C-u C-c n copies the folder
(defun jonas:kill-buffer-file-name (&optional n) 
  (interactive "P")
  (kill-new (jonas:windows-file-name
    (if n
      ;if C-u pressed we return the directory
      (file-name-directory (buffer-file-name))
      ;else we return the entire path to buffer
      (buffer-file-name)))))

(defun jonas:kill-buffer-file-name (&optional n)
  (interactive "P")
  (let ((bfn))
    (setq bfn
          (if n
              ;;if C-u pressed we return the directory
              (file-name-directory (buffer-file-name))
            ;;else we return the entire path to buffer
            (buffer-file-name)))
    (if (eq window-system 'w32)
        (setq bfn (jonas:windows-file-name bfn)))
    (kill-new bfn)))

;; Show me where I am (C-c w)
(defun jonas:show-directory ()
  "Show the directory name of the current buffer in the echo area."
  (interactive)
  (message (file-name-directory (buffer-file-name))))

;; Open my todo file
(defun jonas:todo-file ()
  "Find the file ~/todo.txt."
  (interactive)
  (find-file (format-time-string "~/todo.txt")))

;; This saves me from caps lock
(defun jonas:upcase ()
  "Changes the identifier that point is on or directly after to
uppercase and substitutes - with _."
  (interactive)
  (let ((begin
	 (save-excursion (re-search-backward "[^a-zA-Z-_]" nil 'move) (point)))
        (end
	 (save-excursion (re-search-forward "[^a-zA-Z-_]" nil 'move) (point))))
    (upcase-region begin end)
    (subst-char-in-region begin end ?- ?_)))

;; This generally does the right wrt aligning
(defun jonas:align-around-point ()
  "Aligns the text around point."
  (interactive)
  (align nil nil))

(defun jonas:windows-file-name (unix-file-name)
  "Turn UNIX-FILE-NAME into a Windows file name that external
  programs can understand."
  (dired-string-replace-match "/" unix-file-name "\\" t t))

(defun jonas:show-bases (n)
  "Shows a number in decimal and hexadecimal."
  (interactive "nNumber: ")
  (message (format "Decimal %d, hexadecimal %x" n n)))

(defun jonas:switch-to-shell (n)
  "Start a shell or switch to the shell buffer if one exists."
  (interactive "P")
  (let ((shell-buffer-name (if n
                               (format "*shell*<%d>" n)
                             "*shell*")))
    (if (get-buffer shell-buffer-name)
        (switch-to-buffer shell-buffer-name)
      (shell))))

(defun jonas:set-tab-width-to-3 ()
  "Set the tab width to 3."
  (interactive)
  (set-variable 'tab-width 3))

(defun jonas:set-tab-width-to-4 ()
  "Set the tab width to 4."
  (interactive)
  (set-variable 'tab-width 4))

;; Set nice font for my old eyes
;(set-frame-font "DejaVu Sans Mono-14" nil t)
(set-frame-font "DejaVu Sans Mono-18" nil t)

(defun jonas:set-font-size (&optional n)
  (interactive "P")
  (if n
      (set-frame-font "DejaVu Sans Mono-18" nil t)
    (set-frame-font "DejaVu Sans Mono-14" nil t)))

;; A few bindings I like
(global-set-key [(control tab)] 'complete-symbol)
(global-set-key [S-mouse-2] 'imenu)
;;Copy current buffer path to kill ring C-c n, C-u C-c n copies the folder
(global-set-key "\C-cn"  'jonas:kill-buffer-file-name)
(global-set-key "\C-cw"  'jonas:show-directory)
(global-set-key "\C-cb" 'jonas:show-bases)
(global-set-key "\C-cf" 'jonas:set-font-size)
(global-set-key "\C-cx" 'jonas:switch-to-shell)
(global-set-key "\C-c3" 'jonas:set-tab-width-to-3)
(global-set-key "\C-c4" 'jonas:set-tab-width-to-4)
(global-set-key "\C-cg" 'revert-buffer)

(global-set-key [(f11)] 'next-error)
(global-set-key [(f12)] 'recompile)
;; This is bound to ^ by default.  Since that is a dead key on most
;; European keyboards, I rebind it to r, which seems to be unused by
;; dired and is much more comfortable to use.
(define-key dired-mode-map [(r)] 'dired-up-directory)
(define-key dired-mode-map [(return)] 'dired-view-file)
(define-key dired-mode-map [(?\r)] 'dired-view-file)

;; I never use list-directory, but I often mistype C-x C-d when I want
;; dired
(global-set-key "\C-x\C-d" 'dired)

;; Hack to support Windows Alt+keypad to enter characters by numerical
;; code.  Needed to support insertion of European characters from
;; Autohotkey scripts.
(defvar jonas:alt-keypad-seq nil)

(defun jonas:alt-keypad-value (key)
  (case key
    (M-kp-0 0)
    (M-kp-1 1)
    (M-kp-2 2)
    (M-kp-3 3)
    (M-kp-4 4)
    (M-kp-5 5)
    (M-kp-6 6)
    (M-kp-7 7)
    (M-kp-8 8)
    (M-kp-9 9)))

(defun jonas:alt-keypad-translate (seq)
  (let ((acc 0))
    (dolist (elem seq acc)
      (setq acc (+ (jonas:alt-keypad-value elem)
                   (* 10 acc))))))

(defun jonas:alt-keypad ()
  ""
  (interactive)
  (unless (eq last-command 'jonas:alt-keypad)
    (setq jonas:alt-keypad-seq nil))
  (push last-command-char jonas:alt-keypad-seq)
  (if (= (length jonas:alt-keypad-seq) 4)
      (progn
        (ucs-insert (jonas:alt-keypad-translate (reverse jonas:alt-keypad-seq)))
        (setq  jonas:alt-keypad-seq nil))))

(global-set-key [M-kp-0] 'jonas:alt-keypad)
(global-set-key [M-kp-1] 'jonas:alt-keypad)
(global-set-key [M-kp-2] 'jonas:alt-keypad)
(global-set-key [M-kp-3] 'jonas:alt-keypad)
(global-set-key [M-kp-4] 'jonas:alt-keypad)
(global-set-key [M-kp-5] 'jonas:alt-keypad)
(global-set-key [M-kp-6] 'jonas:alt-keypad)
(global-set-key [M-kp-7] 'jonas:alt-keypad)
(global-set-key [M-kp-8] 'jonas:alt-keypad)
(global-set-key [M-kp-9] 'jonas:alt-keypad)

;; The same thing under Linux/Autokey
(defun jonas:read-unicode ()
  "Insert a Unicode character by hexidecimal code."
  (interactive)
  (let* ((a (read-char "A" nil 2))
	 (b (read-char "B" nil 2))
	 (c (read-char "C" nil 2))
	 (d (read-char "D" nil 2))
	 (e (read-char "E" nil 2))
	 (all (string a b c d e))
	 (ch (string-to-number all 16)))
    (insert ch)))
(global-set-key [(control shift u)] 'jonas:read-unicode)

;; This one's a life-saver
(ido-mode)

;; Save my screen real estate, please
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; Ediff is great
(require 'ediff)
;; I want two windows next to each other
(setq ediff-split-window-function 'split-window-horizontally)
;; I don't want a separate frame for the control buffer
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Get straight to work
(setq inhibit-splash-screen t)

;; Keep quiet
(setq ring-bell-function 'ignore)

(use-package doom-themes
  ; :init (load-theme 'doom-plain-dark t))
  :init (load-theme 'doom-palenight t))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (docker-tramp markdown-mode doom-modeline all-the-icons doom-themes use-package transient yaml-mode gnu-elpa-keyring-update magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
