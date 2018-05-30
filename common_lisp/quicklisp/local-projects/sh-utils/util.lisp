(in-package #:sh-utils)

;;; * Directories
(defun ensure-trailing-slash (string)
  "Return STRING with / at the end."
  (if (ends-with? "/" string)
      string
      (concat string "/")))

(defun mkdirp (&rest directories)
  "Ensure that all DIRECTORIES exist."
  (dolist (dir directories)
    (ensure-directories-exist (ensure-trailing-slash dir))))

(defun takedir (dir)
  "Create DIR if it does not exist and make it the current directory."
  (mkdirp dir)
  (chdir dir))

;;; * Output
(defun echo (&rest objects)
  "Print OBJECTS followed by a newline."
  (dolist (object objects)
    (format t "~S" object))
  (format t "~%"))

(defun pwd (&optional (path (getcwd)))
  "Print PATH."
  (echo (namestring path)))

(defmacro notify (&rest args)
  "Pass ARGS to notify-send."
  `(run/nil '(notify-send ,@args)))

(defmacro notify-success (&rest args)
  "Call notify send with --icon=trophy-gold and ARGS."
  `(notify --icon=trophy-gold ,@args))

(defmacro notify-failure (&rest args)
  "Call notify send with --icon=face-angry and ARGS then exit with code 1."
  `(progn (notify ,@args)
          (quit 1)))

;;; * Exit Codes
(defmacro sh-if (command then-form else-form)
  "If COMMAND has a zero exit code run THEN-FORM; otherwise run ELSE-FORM.
Like if in bash."
  ;; third value is exit code
  `(if (zerop (nth-value 2 (run/nil ,command :on-error nil )))
       ,then-form
       ,else-form))
