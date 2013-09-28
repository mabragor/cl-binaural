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
  (apply #'format `("%s id: %3d freq: %5d r: %7.3f phi: %5.3f theta: %5.3f vol-left: %6.2f vol-right: %6.2f"
                    ,@(mapcar (lambda (x)
                                (cdr (assoc x info)))
                              '(:vocal-p :id :freq :r :phi :theta :vol-left :vol-right)))))
  
(defun binaural-make-new-dot ()
  (interactive)
  (let ((info (pairlis '(:vocal-p :id :freq :r :phi :theta :vol-left :vol-right)
                       (cons " " (slime-eval `(cl-binaural::binaural-make-new-material-dot))))))
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

(defun replace-current-line-with-info (info)
  (crude-save-pos
   (let ((buffer-read-only nil))
     (beginning-of-line)
     (kill-line)
     (insert (binaural-string-from-dot-info info)))))

(defun binaural-silence-dot-at-point (info)
  (slime-eval `(cl-binaural::binaural-silence-dot ,(cdr (assoc :id info))))
  (setf (cdr (assoc :vocal-p info)) " ")
  (with-current-buffer binaural-buffer-name
    (replace-current-line-with-info info)))

(defun binaural-vocalize-dot-at-point (info)
  (slime-eval `(cl-binaural::binaural-vocalize-dot ,(cdr (assoc :id info))))
  (setf (cdr (assoc :vocal-p info)) "*")
  (with-current-buffer binaural-buffer-name
    (replace-current-line-with-info info)))

(defun parse-dot-info-at-point ()
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (if (re-search-forward (concat "^\\(\\*\\| \\) "
                                     "id: +\\([[:digit:]]+\\) "
                                     "freq: +\\([[:digit:]]+\\) "
                                     "r: +\\(-?[[:digit:]]+\\.[[:digit:]]+\\) "
                                     "phi: +\\(-?[[:digit:]]+\\.[[:digit:]]+\\) "
                                     "theta: +\\(-?[[:digit:]]+\\.[[:digit:]]+\\) "
                                     "vol-left: +\\(-?[[:digit:]]+\\.[[:digit:]]+\\) "
                                     "vol-right: +\\(-?[[:digit:]]+\\.[[:digit:]]+\\)$")
                             (line-end-position) t)
          (cons `(:vocal-p . ,(match-string 1))
                (pairlis '(:id :freq :r :phi :theta :vol-left :vol-right)
                         (mapcar (lambda (x) (string-to-number (match-string x)))
                                 '(2 3 4 5 6 7 8))))
        (error "Current line does not look like a dot description.")))))

(defun vocalized-p (info)
  (let ((it (cdr (assoc :vocal-p info))))
    (cond ((equal it " ") nil)
          ((equal it "*") t)
          (t (error "Unknown vocalization symbol %s" it)))))

(defun binaural-toggle-vocalization-at-point ()
  (interactive)
  (let ((info (parse-dot-info-at-point)))
    (if (vocalized-p info)
      (binaural-silence-dot-at-point info)
    (binaural-vocalize-dot-at-point info))))

;;;  Now I need to be able to move the source around from my binaural mode

(defmacro define-dot-mover (name dr dphi dtheta)
  `(defun ,name ()
     (interactive)
     (let ((info (parse-dot-info-at-point)))
       (destructuring-bind (r phi theta)
           (slime-eval `(cl-binaural::binaural-move-dot ,(cdr (assoc :id info)) ,',dr ,',dphi ,',dtheta))
         (setf (cdr (assoc :r info)) r
               (cdr (assoc :phi info)) phi
               (cdr (assoc :theta info)) theta)
         (replace-current-line-with-info info)))))

(defmacro define-dot-movers (name value)
  `(progn (define-dot-mover ,(intern (concat "binaural-" (symbol-name name) "-decf-theta")) 0 0 ,(- value))
          (define-dot-mover ,(intern (concat "binaural-" (symbol-name name) "-incf-theta")) 0 0 ,value)
          (define-dot-mover ,(intern (concat "binaural-" (symbol-name name) "-decf-phi")) 0 ,(- value) 0)
          (define-dot-mover ,(intern (concat "binaural-" (symbol-name name) "-incf-phi")) 0 ,value 0)
          (define-dot-mover ,(intern (concat "binaural-" (symbol-name name) "-decf-r")) ,(- value) 0 0)
          (define-dot-mover ,(intern (concat "binaural-" (symbol-name name) "-incf-r")) ,value 0 0)))

(define-dot-movers slightly 0.01)
(define-dot-movers normally 0.1)
(define-dot-movers fast 1)


(defmacro define-dot-volchanger (name dleft dright)
  `(defun ,name ()
     (interactive)
     (let ((info (parse-dot-info-at-point)))
       (destructuring-bind (lvol rvol)
           (slime-eval `(cl-binaural::binaural-change-dot-volume ,(cdr (assoc :id info)) ,',dleft ,',dright))
         (setf (cdr (assoc :vol-left info)) lvol
               (cdr (assoc :vol-right info)) rvol)
         (replace-current-line-with-info info)))))

(defmacro define-dot-volchangers (name value)
  `(progn (define-dot-volchanger ,(intern (concat "binaural-" (symbol-name name) "-decf-rvol")) 0 ,(- value))
          (define-dot-volchanger ,(intern (concat "binaural-" (symbol-name name) "-incf-rvol")) 0 ,value)
          (define-dot-volchanger ,(intern (concat "binaural-" (symbol-name name) "-decf-vol")) ,(- value) ,(- value))
          (define-dot-volchanger ,(intern (concat "binaural-" (symbol-name name) "-incf-vol")) ,value ,value)
          (define-dot-volchanger ,(intern (concat "binaural-" (symbol-name name) "-decf-lvol")) ,(- value) 0)
          (define-dot-volchanger ,(intern (concat "binaural-" (symbol-name name) "-incf-lvol")) ,value 0)))

(define-dot-volchangers slightly 0.1)
(define-dot-volchangers normally 1)

(defmacro define-dot-freqchanger (name d-freq)
  `(defun ,name ()
     (interactive)
     (let ((info (parse-dot-info-at-point)))
       (let ((freq (slime-eval `(cl-binaural::binaural-change-dot-freq ,(cdr (assoc :id info)) ,',d-freq))))
         (setf (cdr (assoc :freq info)) freq)
         (replace-current-line-with-info info)))))

(defmacro define-dot-freqchangers (name value)
  `(progn (define-dot-freqchanger ,(intern (concat "binaural-" (symbol-name name) "-decf-freq")) ,(- value))
          (define-dot-freqchanger ,(intern (concat "binaural-" (symbol-name name) "-incf-freq")) ,value)))

(define-dot-freqchangers slightly 1)
(define-dot-freqchangers normally 10)


;;; creating/destruction/toggling of dots
(define-key binaural-mode-map "\C-cr" 'binaural-reset-mixer)
(define-key binaural-mode-map "\C-cD" 'binaural-remove-all-dots)
(define-key binaural-mode-map "\C-cs" 'binaural-silence-all-dots)
(define-key binaural-mode-map "\C-cn" 'binaural-make-new-dot)
(define-key binaural-mode-map "\C-cp" 'parse-dot-info-at-point)
(define-key binaural-mode-map "\C-c\C-c" 'binaural-toggle-vocalization-at-point)

;;; moving of dots
(define-key binaural-mode-map "n" 'binaural-slightly-decf-theta)
(define-key binaural-mode-map "p" 'binaural-slightly-incf-theta)
(define-key binaural-mode-map "f" 'binaural-slightly-decf-phi)
(define-key binaural-mode-map "b" 'binaural-slightly-incf-phi)
(define-key binaural-mode-map "i" 'binaural-slightly-decf-r)
(define-key binaural-mode-map "o" 'binaural-slightly-incf-r)
(define-key binaural-mode-map "N" 'binaural-normally-decf-theta)
(define-key binaural-mode-map "P" 'binaural-normally-incf-theta)
(define-key binaural-mode-map "F" 'binaural-normally-decf-phi)
(define-key binaural-mode-map "B" 'binaural-normally-incf-phi)
(define-key binaural-mode-map "I" 'binaural-normally-decf-r)
(define-key binaural-mode-map "O" 'binaural-normally-incf-r)

;;; Changing dots volume
(define-key binaural-mode-map "q" 'binaural-slightly-incf-lvol)
(define-key binaural-mode-map "w" 'binaural-slightly-incf-vol)
(define-key binaural-mode-map "e" 'binaural-slightly-incf-rvol)
(define-key binaural-mode-map "a" 'binaural-slightly-decf-lvol)
(define-key binaural-mode-map "s" 'binaural-slightly-decf-vol)
(define-key binaural-mode-map "d" 'binaural-slightly-decf-rvol)
(define-key binaural-mode-map "Q" 'binaural-normally-incf-lvol)
(define-key binaural-mode-map "W" 'binaural-normally-incf-vol)
(define-key binaural-mode-map "E" 'binaural-normally-incf-rvol)
(define-key binaural-mode-map "A" 'binaural-normally-decf-lvol)
(define-key binaural-mode-map "S" 'binaural-normally-decf-vol)
(define-key binaural-mode-map "D" 'binaural-normally-decf-rvol)

;;; Changing dots frequency
(define-key binaural-mode-map "," 'binaural-slightly-decf-freq)
(define-key binaural-mode-map "." 'binaural-slightly-incf-freq)
(define-key binaural-mode-map "<" 'binaural-normally-decf-freq)
(define-key binaural-mode-map ">" 'binaural-normally-incf-freq)


;;; The shortcut for the mode itself
(global-set-key "\C-c\C-b" 'binaural)


(defun binaural-switch-to-buffer ()
  (interactive)
  (if (equal (buffer-name) binaural-buffer-name)
      (switch-to-buffer nil)
    (switch-to-buffer binaural-buffer-name)))


(provide 'binaural)


