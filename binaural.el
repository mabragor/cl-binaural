;;; binaural.el --- define Emacs mode, useful for calibration of individual HRTF.

(require 'slime)

(defvar binaural-connection nil
  "Connection to CL-BINAURAL on the Common Lisp side.")

(defvar binaural-buffer-name "*binaural*")
(defvar binaural-port 4007)

(defun binaural-connect (&optional port)
  (setq binaural-connection
	(slime-net-connect "127.0.0.1" (or port binaural-port)))
  (slime-init-connection-state binaural-connection))

(defun binaural ()
  (interactive)
  (if (not binaural-connection)
      (binaural-connect))
  (get-buffer-create binaural-buffer-name)
  (with-current-buffer binaural-buffer-name
    (binaural-mode)
    (setq slime-buffer-connection binaural-connection)
    ;; (if (not (slime-eval '(cl-flexoplan::emacs-flexoplan-mysql-login)))
    ;;     (slime-eval `(cl-flexoplan::emacs-flexoplan-mysql-login ,(read-string "MySQL login: "))))
    ;; (if (not (slime-eval '(cl-flexoplan::emacs-flexoplan-mysql-password)))
    ;;     (slime-eval `(cl-flexoplan::emacs-flexoplan-mysql-password ,(read-passwd "MySQL password: "))))
    ;; (slime-eval '(cl-flexoplan::%connect))
    ;; (slime-eval '(cl-user::setf cl-flexoplan::*last-id* (cl-flexoplan::get-last-id-from-database))))
    )
  (binaural-switch-to-buffer))

(define-derived-mode binaural-mode text-mode "Binaural"
  "Major mode for calibration of HRTFs in CL-BINAURAL system.
Special commands:
  \\{binaural-mode-map}"
  (make-local-variable 'slime-buffer-connection)
  (make-local-variable 'buffer-read-only)
  (setf buffer-read-only t))
  
(defun binaural-reset-mixer ()
  "Recreates a mixer in CL-BINAURAL."
  (interactive)
  (slime-eval `(cl-binaural::binaural-reset-mixer))
  (message "Mixer reset"))

(defun binaural-remove-all-dots ()
  (interactive)
  (slime-eval `(cl-binaural::binaural-remove-all-dots))
  (with-current-buffer binaural-buffer-name
    (let ((buffer-read-only nil))
      (delete-region (point-min) (point-max))))
  (message "All dots removed, starting clean"))

(defun binaural-silence-all-dots ()
  (interactive)
  (slime-eval `(cl-binaural::binaural-silence-all-dots))
  (message "All dots silenced"))

(defun binaural-string-from-dot-info (info)
  (destructuring-bind (vocalized-p id freq r phi theta left-mult right-mult) info
      (format "%s id: %3d freq: %5d r: %7.3f phi: %5.3f theta: %5.3f vol-left: %6.2f vol-right: %6.2f"
              vocalized-p id freq r phi theta left-mult right-mult)))
  
(defun binaural-make-new-dot ()
  (interactive)
  (let ((info (cons " " (slime-eval `(cl-binaural::binaural-make-new-material-dot)))))
    (with-current-buffer binaural-buffer-name
      (let ((buffer-read-only nil))
        (let ((line-pos (- (point) (line-beginning-position))))
          (forward-line)
          (insert (binaural-string-from-dot-info info) ?\n)
          (forward-line -1)
          (incf (point) line-pos))))))

(defmacro crude-save-pos (&rest body)
  (let ((g!-point-pos (gensym "POINT-POS")))
    `(let ((,g!-point-pos (point)))
       (unwind-protect (progn ,@body)
         (setf (point) ,g!-point-pos)))))

(defun binaural-silence-dot-at-point (info)
  (slime-eval `(cl-binaural::binaural-silence-dot ,(string-to-number (car info))))
  (setf (car info) " ")
  (with-current-buffer binaural-buffer-name
    (crude-save-pos
     (let ((buffer-read-only nil))
       (beginning-of-line)
       (kill-line)
       (insert (binaural-string-from-dot-info info))))))

(defun binaural-vocalize-dot-at-point (info)
  (slime-eval `(cl-binaural::binaural-vocalize-dot ,(string-to-number (car info))))
  (setf (car info) "*")
  (with-current-buffer binaural-buffer-name
    (crude-save-pos
     (let ((buffer-read-only nil))
       (beginning-of-line)
       (kill-line)
       (insert (binaural-string-from-dot-info info))))))

(defun parse-dot-info-at-point ()
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (if (re-search-forward (concat "^\\(\\*\\| \\) "
                                     "id: +\\([[:digit:]]+\\) "
                                     "freq: +\\([[:digit:]]+\\) "
                                     "r: +\\([[:digit:]]+\\.[[:digit:]]+\\) "
                                     "phi: +\\([[:digit:]]+\\.[[:digit:]]+\\) "
                                     "theta: +\\([[:digit:]]+\\.[[:digit:]]+\\) "
                                     "vol-left: +\\([[:digit:]]+\\.[[:digit:]]+\\) "
                                     "vol-right: +\\([[:digit:]]+\\.[[:digit:]]+\\)$")
                             (line-end-position) t)
          (cons (match-string 1)
                (mapcar (lambda (x) (string-to-number (match-string x)))
                        '(2 3 4 5 6 7 8)))
        (error "Current line does not look like a dot description.")))))

(defun vocalized-p (info)
  (cond ((equal (car info) " ") nil)
        ((equal (car info) "*") t)
        (t (error "Unknown vocalization symbol %s" (car info)))))

(defun binaural-toggle-vocalization-at-point ()
  (interactive)
  (let ((info (parse-dot-info-at-point)))
    (if (vocalized-p info)
      (binaural-silence-dot-at-point info)
    (binaural-vocalize-dot-at-point info))))


(define-key binaural-mode-map "\C-cr" 'binaural-reset-mixer)
(define-key binaural-mode-map "\C-cD" 'binaural-remove-all-dots)
(define-key binaural-mode-map "\C-cs" 'binaural-silence-all-dots)
(define-key binaural-mode-map "\C-cn" 'binaural-make-new-dot)
(define-key binaural-mode-map "\C-cp" 'parse-dot-info-at-point)
(define-key binaural-mode-map "\C-c\C-c" 'binaural-toggle-vocalization-at-point)
(global-set-key "\C-c\C-b" 'binaural)


(defun binaural-switch-to-buffer ()
  (interactive)
  (if (equal (buffer-name) binaural-buffer-name)
      (switch-to-buffer nil)
    (switch-to-buffer binaural-buffer-name)))


(provide 'binaural)

;;;  Now I need to be able to move the source around from my binaural mode
