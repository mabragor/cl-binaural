;;;; cl-binaural.lisp

(in-package #:cl-binaural)

;; First, let's start with implementing custom sound generator from the example

;;; Utility functions
(defun mono->volumed-stereo (left right mono-sample)
  "Returns a new sample with volumes adjusted accordingly."
  (the stereo-sample (stereo-sample (floor (* left mono-sample)) (floor (* right mono-sample)))))

(defun mono->volumed-mono (mult sample)
  (the mono-sample (floor (* mult sample))))

(defparameter *speed-of-sound* 34320 "Speed of sound in centimeters-per-second")
(defparameter *interaural-distance* 20 "Distance between ears, also in centimeters.")
(defparameter *size-of-earcanal* 0.5 "Characteristic size of the receiver, in centimeters.")
(defparameter *sample-rate* 44100 "Rate to use in mixer and in streamers")

(defun simplest-binaural (r phi)
  "Based on R and PHI of where the sound originated, calculates needed ratio of amplitudes
and time delay. PHI is the angle between *right* ear and sound direction."
  (let* ((r-left (sqrt (+ (expt r 2) (/ (expt *interaural-distance* 2) 4) (* r *interaural-distance* (cos phi)))))
	 (r-right (sqrt (+ (expt r 2) (/ (expt *interaural-distance* 2) 4) (- (* r *interaural-distance* (cos phi)))))))
    (values (/ *size-of-earcanal* r-left) (/ *size-of-earcanal* r-right)
	    (/ r-left *speed-of-sound*) (/ r-right *speed-of-sound*))))


(defclass test-streamer ()
  ((n :initform 0)
   (phase :initform 0.0)
   (r :initform 10 :initarg :radius :reader test-streamer-radius)
   (phi :initform (/ pi 2) :initarg :angle :reader test-streamer-angle)
   left-mult right-mult left-delay right-delay
   left-circ-buffer
   (left-pointer :initform 0)
   right-circ-buffer
   (right-pointer :initform 0)))

(defmethod initialize-instance :after ((streamer test-streamer) &key &allow-other-keys)
  (with-slots (r phi left-mult right-mult left-delay right-delay) streamer
    (multiple-value-bind (left-mult-new right-mult-new left-delay-new right-delay-new)
	(simplest-binaural r phi)
      (setf left-mult left-mult-new
	    right-mult right-mult-new
	    left-delay (ceiling (* *sample-rate* left-delay-new)) ; Let's measure time in sample-ticks, not in seconds
	    right-delay (ceiling (* *sample-rate* right-delay-new)))
      (setf (slot-value streamer 'left-circ-buffer) (make-array (1+ left-delay)
								:element-type 'mono-sample
								:initial-element 0)
	    (slot-value streamer 'right-circ-buffer) (make-array (1+ right-delay)
								 :element-type 'mono-sample
								 :initial-element 0)))))

(defmethod mixalot:streamer-mix-into ((streamer test-streamer) mixer buffer offset length time)
  (declare (ignore time))
  (with-slots (n phase left-mult right-mult left-delay right-delay
		 left-circ-buffer right-circ-buffer left-pointer right-pointer) streamer
    (declare (ignorable left-delay right-delay))
    (loop for index upfrom offset
	 repeat length
	 with freq = (+ 200 (* n 200))
	 with dp = (* 2.0 pi freq (/ *sample-rate*))
	 as sample = (round (* 10000 (sin phase)))
	 do
	 (progn (mixalot:stereo-incf (aref buffer index) (stereo-sample (aref left-circ-buffer left-pointer)
									(aref right-circ-buffer right-pointer)))
		(setf (aref left-circ-buffer (mod (+ left-pointer left-delay) (1+ left-delay)))
		      (mono->volumed-mono left-mult sample)
		      (aref right-circ-buffer (mod (+ right-pointer right-delay) (1+ right-delay)))
		      (mono->volumed-mono right-mult sample))
		(setf left-pointer (mod (1+ left-pointer) (1+ left-delay))
		      right-pointer (mod (1+ right-pointer) (1+ right-delay))))
	 (incf phase dp))
    (when (= (incf n) 6)
      (mixalot:mixer-remove-streamer mixer streamer))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *mixer* (create-mixer :rate *sample-rate*)))

(defun randomly-placed-source (&optional (each 1))
  (iter (while 1)
	(mixer-add-streamer *mixer* (make-instance 'test-streamer :angle (random (* 2 pi)) :radius (random 20)))
	(sleep each)))
	

;; OK, so I should really try to do some simple combination of
;; 1) change volume
;; 2) interaural time difference
;; 3) maybe, that's enough to generate some innerhead audio plane

;; Fake mixer to incorporate superposition of effects on mixers
(defstruct dummy-mixer
  (rate 44100))

(defmethod mixer-remove-streamer ((mixer dummy-mixer) streamer)
  (declare (ignore mixer streamer)))
