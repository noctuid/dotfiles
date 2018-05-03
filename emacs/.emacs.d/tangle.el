;; -*- lexical-binding: t -*-
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
      ;; NOTE this could potentially cause problems
      (insert (format ";; Don't edit this file, edit %s instead ... -*- lexical-binding: t -*-\n\n"
                      orgfile))
      (apply 'insert (reverse body-list)))
    (message "Wrote %s ..." elfile)))

(defun noct:tangle-init (&optional load compile async)
  "Tangle init file if it has not already been tangled.
If LOAD is non-nil, load it as well. If COMPILE is non-nil, compile it first."
  (let ((org-init "~/.emacs.d/awaken.org")
        (init-tangled "~/.emacs.d/awaken.el")
        (init-compiled "~/.emacs.d/awaken.elc"))
    (when (or (not (file-exists-p init-tangled))
              (file-newer-than-file-p org-init init-tangled))
      (schurig-tangle-config-org org-init init-tangled))
    (when (and compile
               (file-newer-than-file-p init-tangled init-compiled))
      (byte-compile-file init-tangled load))
    (when (and load (not compile))
      (load-file init-tangled))))
