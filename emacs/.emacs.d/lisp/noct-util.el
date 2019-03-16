;;; noct-util.el --- Helper functions for use with async.el. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; * Faster Untangling
;; http://www.holgerschurig.de/en/emacs-efficiently-untangling-elisp/

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

;; modified from the original to enable lexical binding and optionally demote
;; errors
(defun schurig-tangle-config-org (orgfile elfile &optional demote-errors)
  "Write all source blocks from ORGFILE into ELFILE.

If DEMOTE-ERRORS is non-nil, wrap each source block with `with-demoted-errors'.

Only source blocks that meet these requirements will be tangled:
- not marked as :tangle no
- have a source-code of =emacs-lisp=
- doesn't have the todo-marker CANCELED"
  (let* (body-list
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
            (if demote-errors
                (push (concat "(let (debug-on-error)\n"
                              "(with-demoted-errors \"Init error: %S\"\n"
                              body
                              "))\n\n")
                      body-list)
              (push (concat body "\n")
                    body-list))))))
    (with-temp-file elfile
      ;; NOTE this could potentially cause problems
      (insert (format ";; Don't edit this file, edit %s instead ... -*- lexical-binding: t -*-\n\n"
                      orgfile))
      (apply #'insert (reverse body-list)))
    (message "Wrote %s ..." elfile)))

(defun noct:tangle-org-init (file &optional load compile demote-errors)
  "Tangle org init FILE if it has not already been tangled.
If LOAD is non-nil, load it as well (the newest between the compiled and
non-compiled). If COMPILE is non-nil and the uncompiled file is newer, compile
it afterwards. If DEMOTE-ERRORS is non-nil, wrap each source block with
`with-demoted-errors'.

When there are no errors loading the tangled file, save it with a \"-stable.el\"
or \"-stable.elc\" suffix (depending on which was loaded).

Compiling after loading ensures that all required functionality is available. A
nil LOAD and non-nil COMPILE will not work for my personal config (have to add
everything under ~/.emacs.d/straight/build to `load-path' in an
`eval-when-compile', run `straight-use-package-mode' so :straight is recognized,
etc., etc.; it's easier to compile afterwards)."
  (let* ((base-file (file-name-sans-extension file))
         (org-init file)
         (init-tangled (if demote-errors
                           (format "%s-demoted-errors.el" base-file)
                         (concat base-file ".el")))
         (init-compiled (concat init-tangled "c"))
         (init-tangled-stable (concat base-file "-stable.el"))
         (init-compiled-stable (concat init-tangled-stable "c")))
    (when (or (not (file-exists-p init-tangled))
              (file-newer-than-file-p org-init init-tangled))
      (schurig-tangle-config-org org-init init-tangled demote-errors))
    (when load
      (let ((load-prefer-newer t))
        (load-file init-tangled)
        (unless demote-errors
          ;; successfully loaded without errors; save stable configuration
          (if (file-newer-than-file-p init-tangled init-compiled)
              (copy-file init-tangled init-tangled-stable t)
            (when (file-exists-p init-compiled)
              (copy-file init-compiled init-compiled-stable t))))))
    (when (and compile
               (file-newer-than-file-p init-tangled init-compiled))
      (byte-compile-file init-tangled))))

(defun noct:tangle-awaken (&optional load compile demote-errors)
  "Tangle awaken.org."
  (noct:tangle-org-init "~/.emacs.d/awaken.org" load compile demote-errors)
  ;; TODO clean and move everything to awaken.org
  (when (file-exists-p "~/.emacs.d/unclean.org")
    (noct:tangle-org-init "~/.emacs.d/unclean.org" load compile demote-errors)))

(provide 'noct-util)
;;; noct-util.el ends here
