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
                              "))")
                      body-list)
              (push body body-list))))))
    (with-temp-file elfile
      ;; NOTE this could potentially cause problems
      (insert (format ";; Don't edit this file, edit %s instead ... -*- lexical-binding: t -*-\n\n"
                      orgfile))
      (apply #'insert (reverse body-list)))
    (message "Wrote %s ..." elfile)))

(defun noct:tangle-org-init (file &optional load compile demote-errors)
  "Tangle org init FILE if it has not already been tangled.
If LOAD is non-nil, load it as well. If COMPILE is non-nil, compile it first. If
DEMOTE-ERRORS is non-nil, wrap each source block with `with-demoted-errors'."
  (let* ((base-file (file-name-sans-extension file))
         (org-init file)
         (init-tangled (if demote-errors
                           (format "%s-demoted-errors.el" base-file)
                         (concat base-file ".el")))
         (init-compiled (concat init-tangled "c")))
    (when (or (not (file-exists-p init-tangled))
              (file-newer-than-file-p org-init init-tangled))
      (schurig-tangle-config-org org-init init-tangled demote-errors))
    (when (and compile
               (file-newer-than-file-p init-tangled init-compiled))
      (byte-compile-file init-tangled load))
    (when (and load (not compile))
      (load-file init-tangled))))

(defun noct:tangle-awaken (&optional load compile demote-errors)
  "Tangle awaken.org."
  (noct:tangle-org-init "~/.emacs.d/awaken.org" load compile demote-errors)
  ;; TODO clean and move everything to awaken.org
  (when (file-exists-p "~/.emacs.d/unclean.org")
    (noct:tangle-org-init "~/.emacs.d/unclean.org" load compile demote-errors)))

(provide 'noct-util)
;;; noct-util.el ends here
