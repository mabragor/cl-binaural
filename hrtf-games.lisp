
(in-package #:cl-binaural)

(defmacro proxy-streamer-methods (streamer proxy-slot)
  "Redirect all (important) STREAMER methods to streamer, located in its slot named PROXY-SLOT."
  `(progn (defmethod streamer-mix-into ((streamer ,streamer) mixer buffer offset length time)
            (streamer-mix-into (slot-value streamer ',proxy-slot) mixer buffer offset length time))
          (defmethod streamer-cleanup ((streamer ,streamer) mixer)
            (streamer-cleanup (slot-value streamer ',proxy-slot) mixer))))

(defclass wave-packet ()
  ((freq :initform 500 :initarg :frequency :accessor wp-frequency)
   (duration :initform 1 :initarg :duration :accessor wp-duration)
   (phase :initform 0.0)
   (elapsed :initform 0.0)))

(defmethod streamer-mix-into ((streamer wave-packet) mixer buffer offset length time)
  (declare (ignore time))
  (with-slots (freq phase duration elapsed) streamer
    (with-slots ((rate mixalot::rate)) mixer
      (loop for index upfrom offset
	 repeat length
	 with dp = (* 2.0 pi freq (/ rate))
	 as sample = (round (* 10000 (sin phase)))
	 do
	   (stereo-incf (aref buffer index) (mono->stereo sample))
	   (incf phase dp)
           (incf elapsed (/ rate))
           (when (> elapsed duration)
             (signal-playback-finish (- index offset)))))))

(defclass silence ()
  ((duration :initform 1 :initarg :duration :accessor s-duration)
   (elapsed :initform 0.0)))

(defmethod streamer-mix-into ((streamer silence) mixer buffer offset length time)
  (declare (ignore time))
  (with-slots (duration elapsed) streamer
    (with-slots ((rate mixalot::rate)) mixer
      (incf elapsed (/ length rate))
      (when (> elapsed duration)
	(signal-playback-finish)))))

(defclass streamer-cat ()
  ((streamers :initform nil :initarg :streamers :reader concatenator-streamers)
   cur-streamers-head
   my-mixer)
  (:documentation "streamer, which plays all the provided streamers one after another."))

(defmethod streamer-mix-into ((streamer streamer-cat) mixer buffer offset length time)
  (if (not (slot-boundp streamer 'my-mixer))
      (setf (slot-value streamer 'my-mixer) (make-instance 'dummy-mixer :rate (slot-value mixer 'mixalot::rate))
            (slot-value streamer 'cur-streamers-head) (concatenator-streamers streamer)))
  (with-slots (cur-streamers-head my-mixer) streamer
    (if (not cur-streamers-head)
        (signal-playback-finish 0)
        (handler-case (streamer-mix-into (car cur-streamers-head) my-mixer buffer offset length time)
          (playback-finished (pf)
            (let ((written-length (pf-length pf)))
              (setf cur-streamers-head (cdr cur-streamers-head))
              (if (> length written-length)
                  (handler-case (streamer-mix-into streamer mixer buffer
                                                   (+ offset (pf-length pf))
                                                   (- length (pf-length pf))
                                                   time)
                    (playback-finished (pf)
                      (signal-playback-finish (+ written-length (pf-length pf))))))))))))

(defmethod streamer-cleanup ((stream streamer-cat) mixer)
  (iter (for streamer in (concatenator-streamers stream))
        (streamer-cleanup streamer (slot-value stream 'my-mixer))))

(defclass streamer-capture ()
  ((capture-finished-p :initform nil)
   (position :initform 0)
   (streamer :initform (error "You should provide the streamer to record")
             :initarg :streamer)
   buffer
   size
   my-mixer)
  (:documentation "streamer, which records all output from the other streamer, and later can be used
  to play recorded sound from arbitrary position arbitrary number of times."))

(defmethod streamer-mix-into ((streamer streamer-capture) mixer buffer offset length time)
  (if (not (slot-boundp streamer 'my-mixer))
      (setf (slot-value streamer 'my-mixer) (make-instance 'dummy-mixer :rate (slot-value mixer 'mixalot::rate))
            (slot-value streamer 'buffer) (make-array 0 :element-type 'stereo-sample)))
  (with-slots (capture-finished-p position size (my-streamer streamer) (my-buffer buffer) my-mixer) streamer
    (if capture-finished-p
        (iter (for i from 0 below length)
              (stereo-incf (aref buffer (+ i offset)) (aref my-buffer position))
              (when (equal (incf position) size)
                (signal-playback-finish (1+ i))))
        (progn (setf position (length my-buffer)
                     my-buffer (adjust-array my-buffer (+ (length my-buffer) length) :initial-element 0))
               (macrolet ((generate-output (length)
                            `(iter (for i from 0 below ,length)
                                   (stereo-incf (aref buffer (+ i offset)) (aref my-buffer (+ i position))))))
                 (handler-case (streamer-mix-into my-streamer my-mixer my-buffer position length time)
                   (playback-finished (pf)
                     (setf capture-finished-p t)
                     (if (< (pf-length pf) length)
                         (setf my-buffer (adjust-array my-buffer (+ (pf-length pf)
                                                                    (- (length my-buffer) length)))))
                     (generate-output (pf-length pf))
                     (setf size (length my-buffer)
                           position (length my-buffer))
                     (signal-playback-finish (pf-length pf)))
                   (:no-error (&rest args)
                     (declare (ignore args))
                     (generate-output length)
                     )))))))
                                    
(defmethod streamer-cleanup ((streamer streamer-capture) mixer)
  (streamer-cleanup (slot-value streamer 'streamer) (slot-value streamer 'my-mixer)))
                   
(defmethod streamer-seekable-p ((streamer streamer-capture) mixer)
  (declare (ignore mixer))
  (slot-value streamer 'capture-finished-p))

(defmethod streamer-length ((streamer streamer-capture) mixer)
  (declare (ignore mixer))
  (if (slot-value streamer 'capture-finished-p)
      (slot-value streamer 'size)))

(defmethod streamer-seek ((streamer streamer-capture) mixer position &key &allow-other-keys)
  (declare (ignore mixer))
  (setf (slot-value streamer 'position) position))

(defmethod streamer-position ((streamer streamer-capture) mixer)
  (declare (ignore mixer))
  (slot-value streamer 'position))


(defclass streamer-loop ()
  ((streamer :initform (error "You should provide the streamer to loop over")
             :initarg :streamer)
   capture-streamer
   (repeat :initform :infinity :initarg :repeat)
   (nrepeats :initform 0))
  (:documentation "streamer, which repeats contents of the supplied streamer arbitrary number of times."))
    
(defmethod streamer-mix-into ((streamer streamer-loop) mixer buffer offset length time)
  (with-slots ((my-streamer streamer) (my-capturer capture-streamer) repeat nrepeats) streamer
    (when (not (slot-boundp streamer 'capture-streamer))
      (setf my-capturer (make-instance 'streamer-capture :streamer my-streamer)))
    (handler-case (streamer-mix-into my-capturer mixer buffer offset length time)
      (playback-finished (pf)
        (if (and (not (eq repeat :infinity))
                 (equal (incf nrepeats) repeat)) ; this increment does not even get calculated in :INFINITY case!
            (signal-playback-finish (pf-length pf))
            (progn (streamer-seek my-capturer mixer 0)
                   (let ((written-length (pf-length pf)))
                     (handler-case (streamer-mix-into streamer mixer buffer
                                                      (+ offset written-length)
                                                      (- length written-length)
                                                      time)
                       (playback-finished (pf)
                         (signal-playback-finish (+ written-length (pf-length pf))))))))))))
                                                      
(defmethod streamer-cleanup ((streamer streamer-loop) mixer)
  (if (slot-boundp streamer 'capture-streamer)
      (streamer-cleanup (slot-value streamer 'capture-streamer) mixer)
      (streamer-cleanup (slot-value streamer 'streamer) mixer)))


(defclass pinger ()
  ((freq :initform 500 :initarg :frequency :accessor pinger-frequency)
   (time-on :initform 0.2 :initarg :time-on :accessor pinger-time-on)
   (time-off :initform 0.4 :initarg :time-off :accessor pinger-time-off)
   repeater))

(defmethod initialize-instance :after ((streamer pinger) &key &allow-other-keys)
  (with-slots (freq time-on time-off repeater) streamer
    (setf repeater
          (make-instance 'streamer-loop
                         :streamer (make-instance 'streamer-cat
                                                  :streamers (list (make-instance 'wave-packet
                                                                                  :duration time-on
                                                                                  :frequency freq)
                                                                   (make-instance 'silence :duration time-off)))))))

(proxy-streamer-methods pinger repeater)

(defclass adv-pinger-oneshot ()
  ((freq :initform 500 :initarg :frequency :accessor pinger-frequency)
   (on-off-times :initform '(0.1 0.1 0.1 0.4) :initarg :on-off-times)
   my-streamer))

(defmethod initialize-instance :after ((streamer adv-pinger-oneshot) &key &allow-other-keys)
  (with-slots (freq on-off-times my-streamer) streamer
    (iter (for time in on-off-times)
          (for i from 0)
          (collect (if (equal 0 (mod i 2))
                       (make-instance 'wave-packet :duration time :frequency freq)
                       (make-instance 'silence :duration time))
            into res)
          (finally (setf my-streamer (make-instance 'streamer-cat :streamers res))))))

(proxy-streamer-methods adv-pinger-oneshot my-streamer)

;; It seems reasonable, that each 'ping' should still be performed with a fixed frequency
;; Therefore, I shoudd write
(defclass adv-pinger-recreative ()
  ())
;; which creates an instance of adv-pinger every time anew.

;; and for that I need creative-repeater

(defclass streamer-revive ()
  ((callback :initform (error "You should provide a lambda which creates a streamer to loop over")
             :initarg :revive-lambda)
   my-streamer
   (repeat :initform :infinity :initarg :repeat)
   (nrepeats :initform 0))
  (:documentation "streamer, which recreates streamer using provided lambda REPEAT times."))

(defmethod streamer-mix-into ((streamer streamer-revive) mixer buffer offset length time)
  (with-slots (my-streamer repeat nrepeats callback) streamer
    (macrolet ((create-my-streamer ()
                 `(let ((*streamer* streamer))
                    (declare (special *streamer*))
                    (setf my-streamer (funcall callback)))))
      (when (not (slot-boundp streamer 'my-streamer))
        (create-my-streamer))
      (handler-case (streamer-mix-into my-streamer mixer buffer offset length time)
        (playback-finished (pf)
          (if (and (not (eq repeat :infinity))
                   (equal (incf nrepeats) repeat)) ; this increment does not even get calculated in :INFINITY case!
              (signal-playback-finish (pf-length pf))
              (progn (streamer-cleanup my-streamer mixer)
                     (create-my-streamer)
                     (let ((written-length (pf-length pf)))
                       (handler-case (streamer-mix-into streamer mixer buffer
                                                        (+ offset written-length)
                                                        (- length written-length)
                                                        time)
                         (playback-finished (pf)
                           (signal-playback-finish (+ written-length (pf-length pf)))))))))))))
                                                      
(defmethod streamer-cleanup ((streamer streamer-revive) mixer)
  (if (slot-boundp streamer 'my-streamer)
      (streamer-cleanup (slot-value streamer 'my-streamer) mixer)))

(defclass adv-pinger ()
  ((freq :initform 500 :initarg :frequency :accessor pinger-frequency)
   (on-off-times :initform '(0.1 0.1 0.1 0.4) :initarg :on-off-times)
   (repeat :initform :infinity :initarg :repeat)
   repeater))

(defmethod initialize-instance :after ((streamer adv-pinger) &key &allow-other-keys)
  (with-slots (freq on-off-times repeat repeater) streamer
    (setf repeater (make-instance 'streamer-loop
                                  :streamer (make-instance 'adv-pinger-oneshot
                                                           :on-off-times on-off-times
                                                           :frequency freq)
                                  :repeat repeat))))

(proxy-streamer-methods adv-pinger repeater)

(defclass adv-revive-pinger ()
  ((freq :initform 500 :initarg :frequency :accessor pinger-frequency)
   (on-off-times :initform '(0.1 0.1 0.1 0.4) :initarg :on-off-times)
   (repeat :initform :infinity :initarg :repeat)
   revive-repeater))

(defmethod initialize-instance :after ((streamer adv-revive-pinger) &key &allow-other-keys)
  (with-slots (freq on-off-times repeat revive-repeater) streamer
    (setf revive-repeater (make-instance 'streamer-revive
                                         :revive-lambda (lambda ()
                                                          (make-instance 'adv-pinger-oneshot
                                                                         :on-off-times on-off-times
                                                                         :frequency freq))
                                         :repeat repeat))))

(proxy-streamer-methods adv-revive-pinger revive-repeater)


(defclass material-dot ()
  ((freq :initform 500 :initarg :frequency :accessor pinger-frequency)
   (r :initform 15 :initarg :radius)
   (phi :initform (/ pi 2) :initarg :phi)
   (theta :initform 0 :initarg :theta)
   (left-mult :initform 1)
   (right-mult :initform 1)
   pinger
   binaurer
   my-mixer))

(defmethod initialize-instance :after ((streamer material-dot) &key &allow-other-keys)
  (with-slots (freq r phi theta pinger binaurer) streamer
    (setf pinger (make-instance 'adv-revive-pinger :frequency freq)
          binaurer (make-instance 'naive-binaurer :radius r :phi phi :theta theta
                                  :streamer pinger))))

(defmethod streamer-mix-into ((streamer material-dot) mixer buffer offset length time)
  (with-slots (binaurer my-mixer left-mult right-mult) streamer
    (if (not (slot-boundp streamer 'my-mixer))
        (setf my-mixer (make-instance 'dummy-mixer :rate (slot-value mixer 'mixalot::rate))))
    (let ((my-buffer (make-array length :element-type '(unsigned-byte 32) :initial-element 0)))
      (macrolet ((generate-output (length)
                   `(iter (for i from 0 below ,length)
                          (stereo-incf (aref buffer (+ offset i))
                                       (stereo->volumed-stereo left-mult right-mult (aref my-buffer i))))))
        (handler-case (streamer-mix-into binaurer my-mixer my-buffer 0 length time)
          (playback-finished (pf)
            (generate-output (pf-length pf))
            (signal-playback-finish (pf-length pf)))
          (:no-error (&rest args)
            (declare (ignore args))
            (generate-output length)))))))

(defmethod streamer-cleanup ((streamer material-dot) mixer)
  ;; pinger gets cleaned up automatically through binaurer
  (streamer-cleanup (slot-value streamer 'binaurer) mixer))

(defmethod move-streamer ((streamer material-dot) dr dphi dtheta)
  (move-streamer (slot-value streamer 'binaurer) dr dphi dtheta))

(defgeneric change-volume-left (streamer dvol)
  (:method ((streamer material-dot) dvol)
    (incf (slot-value streamer 'left-mult) dvol)))
(defgeneric change-volume-right (streamer dvol)
  (:method ((streamer material-dot) dvol)
    (incf (slot-value streamer 'right-mult) dvol)))
(defgeneric change-volume (streamer dvol)
  (:method ((streamer material-dot) dvol)
    (change-volume-left streamer dvol)
    (change-volume-right streamer dvol)
    (values (slot-value streamer 'left-mult) (slot-value streamer 'right-mult))))
(defgeneric skew-volume (streamer dvol)
  (:method ((streamer material-dot) dvol)
    (change-volume-left streamer (- dvol))
    (change-volume-right streamer dvol)
    (values (slot-value streamer 'left-mult) (slot-value streamer 'right-mult))))

(defgeneric change-freq (streamer d-freq)
  (:method ((streamer material-dot) d-freq)
    (incf (pinger-frequency streamer) d-freq)
    (change-freq (slot-value streamer 'pinger) d-freq))
  (:method ((streamer adv-revive-pinger) d-freq)
    (incf (pinger-frequency streamer) d-freq)))
    

;; OK, what now?
;; 1. (done) I need to incorporate 3d coordinate system
;;    commands to move a source in this system
;; 2. (done) I need to be dynamically change HRTFs
;;    overal volume
;;    volumes of left and right ears separately
;;    ... and calculate HRTF from it
;; 3. emacs mode for convenient pinger source creation, movement and volume adjustment
;; 4. persistence engine
