;; * Immediate Setup/Helpers
;; ** Settings
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; don't garbage collect during initialization
(setq gc-cons-threshold most-positive-fixnum
      ;; prevent prompt when loading org file
      vc-follow-symlinks t
      ;; don't want emacs touching this file
      custom-file "~/.emacs.d/custom.el")

(defalias 'yes-or-no-p #'y-or-n-p)

;; ** Line Numbers in Debugger
;; wasamasa's function to display number lines in debugger
;; https://emacs.stackexchange.com/questions/7852/show-line-number-on-error?lq=1
(with-eval-after-load 'debug
  (defun debugger-setup-buffer (debugger-args)
    "Initialize the `*Backtrace*' buffer for entry to the debugger.
That buffer should be current already."
    (setq buffer-read-only nil)
    (erase-buffer)
    (set-buffer-multibyte t)          ;Why was it nil ?  -stef
    (setq buffer-undo-list t)
    (let ((standard-output (current-buffer))
          (print-escape-newlines t)
          (print-level 8)
          (print-length 50))
      (backtrace))
    (goto-char (point-min))
    (delete-region (point)
                   (progn
                     (search-forward "\n  debug(")
                     (forward-line (if (eq (car debugger-args) 'debug)
                                       2 ; Remove implement-debug-on-entry frame.
                                     1))
                     (point)))
    (insert "Debugger entered")
    ;; lambda is for debug-on-call when a function call is next.
    ;; debug is for debug-on-entry function called.
    (pcase (car debugger-args)
      ((or `lambda `debug)
       (insert "--entering a function:\n"))
      ;; Exiting a function.
      (`exit
       (insert "--returning value: ")
       (setq debugger-value (nth 1 debugger-args))
       (prin1 debugger-value (current-buffer))
       (insert ?\n)
       (delete-char 1)
       (insert ? )
       (beginning-of-line))
      ;; Debugger entered for an error.
      (`error
       (insert "--Lisp error: ")
       (prin1 (nth 1 debugger-args) (current-buffer))
       (insert ?\n))
      ;; debug-on-call, when the next thing is an eval.
      (`t
       (insert "--beginning evaluation of function call form:\n"))
      ;; User calls debug directly.
      (_
       (insert ": ")
       (prin1 (if (eq (car debugger-args) 'nil)
                  (cdr debugger-args) debugger-args)
              (current-buffer))
       (insert ?\n)))
    ;; After any frame that uses eval-buffer,
    ;; insert a line that states the buffer position it's reading at.
    (save-excursion
      (let ((tem eval-buffer-list))
        (while (and tem
                    (re-search-forward "^  eval-\\(buffer\\|region\\)(" nil t))
          (beginning-of-line)
          (insert (format "Error at line %d in %s: "
                          (with-current-buffer (car tem)
                            (line-number-at-pos (point)))
                          (with-current-buffer (car tem)
                            (buffer-name))))
          (pop tem))))
    (debugger-make-xrefs)))

;; ** Faster Untangling
;; http://www.holgerschurig.de/en/emacs-efficiently-untangling-elisp/
;; Ideally untangling would be done before startup. However, this is so much
;; faster than org-babel-load-file for me that it almost doesn't matter.

;; This is GPLv2. If you still don't know the details, read
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html

(defun schurig-tangle-section-canceled ()
  "Return t if the current section header was CANCELED, else nil."
  (save-excursion
    (if (re-search-backward "^\\*+\\s-+\\(.*?\\)?\\s-*$" nil t)
        (string-prefix-p "CANCELED" (match-string 1))
      nil)))

;; This uses partially derived code from ob-core.el. So this snippet
;; is GPLv3 or later. If you still don't know the details, read
;; http://www.gnu.org/licenses/

(defun schurig-tangle-config-org (orgfile elfile)
  "This function will write all source blocks from =config.org= into
=config.el= that are ...

- not marked as :tangle no
- have a source-code of =emacs-lisp=
- doesn't have the todo-marker CANCELED"
  (let* ((body-list ())
         (gc-cons-threshold most-positive-fixnum)
         (org-babel-src-block-regexp
          (concat
           ;; (1) indentation                 (2) lang
           "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
           ;; (3) switches
           "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
           ;; (4) header arguments
           "\\([^\n]*\\)\n"
           ;; (5) body
           "\\([^\000]*?\n\\)??[ \t]*#\\+end_src")))
    (with-temp-buffer
      (insert-file-contents orgfile)
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (let ((lang (match-string 2))
              (args (match-string 4))
              (body (match-string 5))
              (canc (schurig-tangle-section-canceled)))
          (when (and (string= lang "emacs-lisp")
                     (not (string-match-p ":tangle\\s-+no" args))
                     (not canc))
            (add-to-list 'body-list body)))))
    (with-temp-file elfile
      (insert (format ";; Don't edit this file, edit %s instead ...\n\n"
                      orgfile))
      (apply 'insert (reverse body-list)))
    (message "Wrote %s ..." elfile)))

(defun noct:tangle-init (&optional load)
  "Tangle init file if it has not already been tangled.
If LOAD is non-nil, load it as well."
  (let ((org-init "~/.emacs.d/awaken.org")
        (init-tangled "~/.emacs.d/awaken.el"))
    ;; compiling/loading a compiled init has caused a lot of problems
    ;; (e.g. emacs hanging, errors silenced); stop compiling for now
    (when (or (not (file-exists-p init-tangled))
              (file-newer-than-file-p org-init init-tangled))
      (schurig-tangle-config-org org-init init-tangled))
    (when load
      (load-file init-tangled))))

;; * Load Org Config
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(let ((file-name-handler-alist nil))
  (noct:tangle-init t)
  ;; reset gc-cons-threshold to slightly higher than default
  ;; idle timer suggested by vermiculus
  (run-with-idle-timer
   10 nil
   (lambda ()
     ;; recommended amount by flx
     (setq gc-cons-threshold (* 20 100 100))
     (message "gc-cons-threshold restored to %S"
              gc-cons-threshold))))
