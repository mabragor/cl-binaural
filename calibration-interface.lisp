;;; Functions, intended to be called from Emacs frontend.

(in-package #:cl-binaural)

(defparameter *binaural-port* 4007)
(defun start-server (&optional (port *binaural-port*))
  (swank:create-server :port port))

;; For now, there will be no persistence

(defparameter *streamers* (make-hash-table :test #'equal))
(defparameter *binaural-mixer* (create-mixer))

(defun search-free-id ()
  (iter (for i from 0)
        (multiple-value-bind (item got) (gethash i *streamers*)
          (declare (ignore item))
          (if (not got)
              (return i)))))

(defun binaural-make-new-material-dot ()
  (let ((id (search-free-id))
        (new-streamer (make-instance 'material-dot)))
    (setf (gethash id *streamers*) new-streamer)
    (with-slots (freq r phi theta left-mult right-mult) new-streamer
      `(,id ,freq ,.(mapcar (lambda (x)
                              (coerce (/ (fround x 0.001) 1000) 'single-float))
                            (list r phi theta left-mult right-mult))))))

(defun binaural-silence-all-dots ()
  (mixer-remove-all-streamers *binaural-mixer*)
  t)

(defun binaural-remove-all-dots ()
  (binaural-silence-all-dots)
  (clrhash *streamers*)
  t)

(defun binaural-reset-mixer ()
  (handler-case (mixer-remove-all-streamers *binaural-mixer*)
    (error () nil))
  (setf *binaural-mixer* (create-mixer))
  t)
        
(defun binaural-vocalize-dot (id)
  (mixer-add-streamer *binaural-mixer* (gethash id *streamers*))
  t)
(defun binaural-silence-dot (id)
  (mixer-remove-streamer *binaural-mixer* (gethash id *streamers*))
  t)

(defun binaural-move-dot (id dr dphi dtheta)
  (mapcar (lambda (x)
            (coerce x 'single-float))
          (multiple-value-list (move-streamer (gethash id *streamers*) dr dphi dtheta))))
  
