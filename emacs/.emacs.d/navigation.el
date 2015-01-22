;; only can have 10 screens
(define-key evil-normal-state-map (kbd "SPC a") (lambda () (interactive) (elscreen-goto 0)))
(define-key evil-normal-state-map (kbd "SPC r") (lambda () (interactive) (elscreen-goto 1)))
(define-key evil-normal-state-map (kbd "SPC s") (lambda () (interactive) (elscreen-goto 2)))
(define-key evil-normal-state-map (kbd "SPC t") (lambda () (interactive) (elscreen-goto 3)))
(define-key evil-normal-state-map (kbd "SPC d") (lambda () (interactive) (elscreen-goto 4)))
(define-key evil-normal-state-map (kbd "SPC h") (lambda () (interactive) (elscreen-goto 5)))
(define-key evil-normal-state-map (kbd "SPC n") (lambda () (interactive) (elscreen-goto 6)))
(define-key evil-normal-state-map (kbd "SPC e") (lambda () (interactive) (elscreen-goto 7)))
(define-key evil-normal-state-map (kbd "SPC i") (lambda () (interactive) (elscreen-goto 8)))
(define-key evil-normal-state-map (kbd "SPC o") (lambda () (interactive) (elscreen-goto 9)))



(let ((tab-title (elscreen-get-screen-nickname (elscreen-get-current-screen))))
(cond ((equal tab-title "main")
    ;; unmap comma
    (define-key evil-normal-state-map "\," nil)
    (define-key evil-normal-state-map (kbd "\,\,") (lambda () (interactive) (find-file "~/ag-sys/Else/everything/log\.txt")))
    (define-key evil-normal-state-map (kbd "SPC b") (lambda () (interactive) (elscreen-goto 2))))
   ((equal tab-title "other") nil)
   ))
