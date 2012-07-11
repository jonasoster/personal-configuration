;;;;; Emacs configuration 
;;;;; Jonas Öster (CM-DI/PJ-CF31) jonas.oester@de.bosch.com

;; Save my screen real estate, please
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Make sure I can access Cygwin
;; (setenv "PATH" (concat (getenv "PATH")
;;                        ";c:\\cygwin\\bin"))
;; (add-to-list 'exec-path "c:/cygwin/bin" t)
;; (add-to-list 'exec-path "C:/Program Files/CollabNet/Subversion Server" t)

;; Set up my load path
(setq load-path (cons "~" load-path))
(setq load-path (cons "~/.emacs.d" load-path))
(setq load-path (cons "~/.emacs.d/csharp" load-path))
;; (setq load-path (cons "c:/amb" load-path))

(server-start)

;; Enable CEDET (keep it separate form the CEDET included in Emacs)
;; (load-file "~/src/cedet/common/cedet.el")
;; (semantic-load-enable-code-helpers)
;; (require 'semantic-ia)

;; Color theme package
;; (add-to-list 'load-path "~/elisp/color-theme-6.6.0")
;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-resolve)))

;; Sensible default keybindings
;(pc-bindings-mode)

;; Show me beautiful colours
(global-font-lock-mode 1)

;; I will need this to configure keybindings later
(require 'dired)

;; I hate tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; I know what I'm doing, mostly
(setq-default dired-recursive-deletes 'top)
(setq-default dired-recursive-copies 'always)

;; My rodent sucks, this makes it tolerable
(setq w32-num-mouse-buttons 2)
(setq w32-swap-mouse-buttons t)

;; Let me see my selection 
(transient-mark-mode t)

;; Let me see matching parentheses
(show-paren-mode 1)

;; Make the Emacs window big enough
(set-frame-height (selected-frame) 47)
(set-frame-width (selected-frame) 95)

;; Pages make it easier to navigate
(require 'page-ext)

;; Do the right thing when I press enter
(substitute-key-definition
 'newline 'newline-and-indent (current-global-map))

;; ;; Save my poor eyes
;; (setq jonas:font
;;       ;; Consolas looks better than DejaVu when ClearType is activated
;;       "-outline-Consolas-normal-r-normal-normal-16-120-96-96-c-*-iso8859-1")
;; (set-default-font jonas:font)

;; And make sure new frames have resonable defaults
;; (push (cons 'font jonas:font) default-frame-alist)

;; always end a file with a newline
(setq require-final-newline t)

;; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; Switch buffers
(global-set-key [(meta o)]
  (lambda ()
    (interactive)
    (switch-to-buffer (other-buffer))))

(add-hook 'shell-mode-hook
          (lambda ()
            (font-lock-mode -1)))

(require 'cc-mode)

;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
(setq c-recognize-knr-p nil)

;; Change the indentation amount to 3 spaces instead of 2.
;; You have to do it in this complicated way because of the
;; strange way the cc-mode initializes the value of `c-basic-offset'.
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (setq c-basic-offset 3)
	    (c-set-offset 'substatement-open 0)
	    (c-set-offset 'inextern-lang 0)
	    (c-set-offset 'brace-list-open 0)
	    (c-set-offset 'arglist-cont-nonempty '+)
	    (c-set-offset 'arglist-intro '+)
	    (define-key c-mode-base-map "\C-m" 'newline-and-indent)))
       
;; Use the nice extra modifiers available
(setq w32-apps-modifier 'super)
(setq w32-lwindow-modifier 'hyper)
(setq w32-rwindow-modifier 'alt)
;; Don't pass them on to Windows
(setq w32-pass-rwindow-to-system nil)
(setq w32-pass-lwindow-to-system nil)
(setq w32-pass-alt-to-system nil)

;; Skeletons make life easier
(define-skeleton jonas:c-braces
  "Insert a pair of braces."
  nil
  \n "{" >
  \n > _
  \n  "}" >
  )

(define-skeleton jonas:c-for
  "Insert a for statement."
  nil
  \n "for(" _ ";" _ ";" _ ")" >
  \n "{" >
  \n > _
  \n  "}" >
  )

(define-skeleton jonas:c-if
  "Insert an if statement."
  nil
  \n "if(" _ ")" >
  \n "{" >
  \n > _
  \n  "}" >
  )

(define-skeleton jonas:c-doxygen-comment
  "Insert a Doxygen comment template."
  nil
  \n "/*!" >
  \n "* \\brief "  > _
  \n  "*/" >
  )

;; (require 'snippet)
;; (snippet-with-abbrev-table 'c-mode-abbrev-table
;;                            ("jfor" . "$>for(int $${i}=$${0};$${i}<$${len};++$${i})\n{$>\n$>$.\n$>}$>"))

(defun jonas:camel-case (s)
  "CamelCase S."
  (apply #'concat (split-string (upcase-initials s) "_")))

;; Stolen from the wiki:
;; Copy current buffer path to kill ring C-c n, C-u C-c n copies the folder
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

;; Open my diary file for today
(defun jonas:diary-file ()
  "Find the file ~/diary/diary-YY-MM-DD.txt."
  (interactive)
  (find-file (format-time-string "~/diary/diary-%Y-%m-%d.txt")))

;; Open my working-from-home log file for this month
(defun jonas:vpn-file ()
  "Find the file ~/vpn-diary-YY-MM.txt."
  (interactive)
  (find-file (format-time-string "~/vpn-diary-%Y-%m.txt"))
  (goto-char (point-max)))

(defun jonas:vpn-entry ()
  "Insert an empty vpn log file entry for today's date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d: 0:00\n\nempty\n\n")))

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
  (let ((begin (save-excursion (re-search-backward "[^a-zA-Z-_]" nil 'move) (point)))
        (end (save-excursion (re-search-forward "[^a-zA-Z-_]" nil 'move) (point))))
    (upcase-region begin end)
    (subst-char-in-region begin end ?- ?_)))

;; Convenient functions for buffer switching
;; (load-library "ska-buffer-switch")

;; Call my create_tags script
(defun jonas:create-tags (dir)
  "Call create_tags on directory DIR."
  (interactive "D")
  (call-process-shell-command "c:\\jonas\\create_tags\\create_tags.bat" nil nil nil dir)
  (message "Done!"))

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

;; Clearcase support functions
(defun jonas:clearcase-check-out ()
  "Checkout from ClearCase."
  (interactive)
  (if buffer-file-name
      (progn
        (shell-command (format "cleartool co %s" buffer-file-name))
        (revert-buffer nil t))
    (message "Open a file first!")))

(defun jonas:clearcase-check-in ()
  "Checkin to ClearCase."
  (interactive)
  (if buffer-file-name
      (progn
        (shell-command (format "cleartool ci %s" buffer-file-name))
        (revert-buffer nil t))
    (message "Open a file first!")))

(defun jonas:clearcase-hijack ()
  "Allah akbar!"
  (interactive)
  (if buffer-file-name
      (progn
        (shell-command (format "chmod +w %s" buffer-file-name))
        (revert-buffer nil t))
    (message "Open a file first!")))

(defun jonas:clearcase-version-tree ()
  "Start the graphical version tree browser"
  (interactive)
  (if buffer-file-name
      (start-process "lsvtree" nil "cleartool" "lsvtree" "-graphical" buffer-file-name)
    (message "Open a file first!")))

(global-set-key "\C-cqo" 'jonas:clearcase-check-out)
(global-set-key "\C-cqi" 'jonas:clearcase-check-in)
(global-set-key "\C-cqh" 'jonas:clearcase-hijack)
(global-set-key "\C-cqv" 'jonas:clearcase-version-tree)

(defun jonas:text-bold (str)
  "Insert a \\textbf command in a Latex file."
  (interactive "s")
  (insert "\\textbf{")
  (insert str)
  (insert "}"))

;; A few bindings I like
;; (global-set-key [(super w)] 'jonas:c-braces)
;; (global-set-key [(control super l)] 'jonas:upcase)
;; (global-set-key [(control super k)] 'hippie-expand)
;; (global-set-key [(control super e)] 'call-last-kbd-macro)
;; (global-set-key [(control super a)] 'jonas:align-around-point)
;(global-set-key [(super right)] 'ska-next-buffer)
;(global-set-key [(super left)] 'ska-previous-buffer)
(global-set-key [(control tab)] 'complete-symbol)
(global-set-key [S-mouse-2] 'imenu)
;;Copy current buffer path to kill ring C-c n, C-u C-c n copies the folder
(global-set-key "\C-cn"  'jonas:kill-buffer-file-name)
(global-set-key "\C-cw"  'jonas:show-directory)
(global-set-key "\C-cd" 'jonas:c-doxygen-comment)
(global-set-key "\C-cf" 'jonas:c-for)
(global-set-key "\C-ci" 'jonas:c-if)
(global-set-key "\C-cb" 'jonas:show-bases)
(global-set-key "\C-cx" 'jonas:switch-to-shell)
(global-set-key "\C-c3" 'jonas:set-tab-width-to-3)
(global-set-key "\C-c4" 'jonas:set-tab-width-to-4)
;; (global-set-key [(control super d)] 'jonas:diary-file)
;; (global-set-key [(control super v)] 'jonas:vpn-file)
;; (global-set-key [(control super b)] 'jonas:vpn-entry)
(global-set-key [(control super t)] 'jonas:todo-file)

;; I map the useless caps lock key to SUPER.  Key chords with
;; SUPER+right hand key become very comfortable and should be used for
;; the most often used functions.  This allows me to leave all
;; standard Emacs keybindings as they are and still define
;; easy-to-reach chords with a more sensible geometric layout, for
;; example the following point movement commands based on VI:
(global-set-key [(super k)] 'previous-line) 
(global-set-key [(super j)] 'next-line)
(global-set-key [(super shift k)] 'scroll-down)
(global-set-key [(super shift j)] 'scroll-up)
(global-set-key [(super h)] 'backward-char)
(global-set-key [(super l)] 'forward-char)
(global-set-key [(super shift h)] 'backward-word)
(global-set-key [(super shift l)] 'forward-word)
(global-set-key [(super meta h)] 'backward-word)
(global-set-key [(super meta l)] 'forward-word)

;; This used to be:

;; (global-set-key [(super i)] 'previous-line) 
;; (global-set-key [(super u)] 'next-line)
;; (global-set-key [(super shift i)] 'scroll-down)
;; (global-set-key [(super shift u)] 'scroll-up)
;; (global-set-key [(super j)] 'backward-char)
;; (global-set-key [(super k)] 'forward-char)
;; (global-set-key [(super shift j)] 'backward-word)
;; (global-set-key [(super shift k)] 'forward-word)
;; (global-set-key [(super meta j)] 'backward-word)
;; (global-set-key [(super meta k)] 'forward-word)

;; Sometimes, it seems reasonable to duplicate standard Emacs CTRL key
;; bindings with SUPER as the modifier:
(global-set-key [(super ?\s)] 'set-mark-command)
(global-set-key [(super a)] 'move-beginning-of-line)
(global-set-key [(super e)] 'move-end-of-line)
(global-set-key [(super d)] 'delete-char)
(global-set-key [(super shift d)] 'kill-word)
(global-set-key [(f11)] 'next-error)
(global-set-key [(f12)] 'recompile)
(global-set-key [(control return)] 'tempo-complete-tag)
;; This is bound to ^ by default.  Since that is a dead key on most
;; European keyboards, I rebind it to r, which seems to be unused by
;; dired and is much more comfortable to use.
;(define-key dired-mode-map [(r)] 'dired-up-directory)
; I don't use European keyboards any more
(define-key dired-mode-map [(return)] 'dired-view-file)

;; I never use list-directory, but I often mistype C-x C-d when I want
;; dired
(global-set-key "\C-x\C-d" 'dired)


;; Hack for losing Swedish keyboards
;(global-set-key [2212] '(lambda ()
;                        (interactive)
;                        (insert "$")))

;; Support for Blaupunkt
;(require 'bp)

;; This one's a life-saver
(iswitchb-mode 1)
(global-set-key [(super m)] 'iswitchb-buffer)


;; .pro are usually TTFIS log files in my world
(setq auto-mode-alist (cons '("\\.pro\\'" . fundamental-mode) auto-mode-alist))

;; Support for ARM RCVT compiler
(require 'compile)
(add-to-list 'compilation-error-regexp-alist
             '("^\\(.*\\)(\\([0-9]+\\),\\([0-9]+\\))[ \t]*:.*$" 1 2 3))
(add-to-list 'compilation-error-regexp-alist
             '("^[ \t]*\\(.*\\)(\\([0-9]+\\)[ \t]*:" 1 2))
;(require 'ambient)

;; Sometimes, I use Subversion
;(require 'psvn)

;; Set up paths for grep
(require 'grep)
;; (setq find-program "C:\\cygwin\\bin\\find.exe")
;; (setq grep-program "C:\\cygwin\\bin\\grep.exe")

;; (setq ediff-diff-program  "C:\\cygwin\\bin\\diff.exe")
;; (setq ediff-diff3-program "C:\\cygwin\\bin\\diff3.exe")

;; Ediff is great
(require 'ediff)
;; I want two windows next to each other
(setq ediff-split-window-function 'split-window-horizontally)
;; I don't want a separate frame for the control buffer
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(put 'upcase-region 'disabled nil)

;; Tempo templates
(require 'tempo)
(setq tempo-interactive t)

(tempo-define-template
   "jfor"
   '("for( " (p "expr1:") "; "
     (p "expr2:") "; "
     (p "expr3:") " )" n>
     "{" > n
     > r n>
     "}" > %)
   "jfor")

(tempo-define-template
   "jdef"
   '(& > (p "Type:" type) " " (p "Variable name:" var) " = new " (s type) "();" > n>)
   "jdef")

(tempo-define-template
   "jget"
   '(& > "private " (p "Type:" type) " " (p "Variable name:" var) ";" n>
     "public " (s type) " " (jonas:camel-case (tempo-lookup-named 'var)) " {" n>
     "get { return " (s var) "; }" n
     "}" > n>)
   "jget")

(tempo-define-template
   "jgetset"
   '(& > "private " (p "Type:" type) " " (p "Variable name:" var) ";" n>
     "public " (s type) " " (jonas:camel-case (tempo-lookup-named 'var)) " {" n>
     "get { return " (s var) "; }" n>
     "set { " (s var) " = value; }" n>
     "}" > n>)
   "jgetset")

(tempo-define-template
   "jcall"
   '(> "<arrow type=\"call\" from=\"" (p "From:" from) "\" to=\"" (p "To:" to) "\">" r "</arrow>" n>
     "<activate label=\"" (s to) "\"/>" n>
     "<step/>" n>
     "" n>
     "<arrow type=\"return\" from=\"" (s to) "\" to=\"" (s from) "\"/>" n>
     "<deactivate label=\"" (s to) "\"/>" n>
     "<step/>")
   "jcall")

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
   (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

(defun jonas-csharp-mode-hook ()
  "C# mode setup"
  (setq c-basic-offset 4)
  ;; C# code completion
  ;; (require 'csharp-completion)
  ;; (csharp-analysis-mode 1)
  ;; (local-set-key "\M-\\"   'cscomp-complete-at-point)
  ;; (local-set-key "\M-\."   'cscomp-complete-at-point-menu)
  )

(add-hook 'csharp-mode-hook 'jonas-csharp-mode-hook t)

;; ;; This is needed to debug Cygwin programs
;; (require 'cygwin-mount)
;; (cygwin-mount-activate)
;(require 'w32-print)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-command-list (quote (("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX") ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX") ("Makeinfo" "makeinfo %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with Info output") ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with HTML output") ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX") ("ConTeXt" "context  %t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once") ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion") ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX") ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer") ("Print" "%p" TeX-run-command t t :help "Print the file") ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command) ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file") ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file") ("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for correctness") ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document") ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files") ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files") ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(compilation-scroll-output t)
 '(dired-dwim-target t)
 '(inhibit-startup-screen t)
 '(line-move-visual nil)
 '(noprint-hide-print-in-menus t)
 '(noprint-hide-ps-print-in-menus t)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(show-paren-mode t)
 '(svn-status-prefix-key [(super s)])
 '(truncate-partial-width-windows nil))
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "SystemWindow" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "outline" :family "DejaVu Sans Mono")))))
; '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "outline" :family "DejaVu Sans Mono")))))

(put 'narrow-to-region 'disabled nil)
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(default ((t (:box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "outline" :family "DejaVu Sans Mono")))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
