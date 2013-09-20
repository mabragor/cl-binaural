;;;; cl-binaural.lisp

(in-package #:cl-binaural)

(defun mono->volumed-stereo (left right mono-sample)
  "Returns a new sample with volumes adjusted accordingly."
  (the stereo-sample (stereo-sample (floor (* left mono-sample)) (floor (* right mono-sample)))))

(defun mono->volumed-mono (mult sample)
  (the mono-sample (floor (* mult sample))))

(defun stereo->volumed-stereo (left right sample)
  "Returns a new sample with volumes adjusted accordingly."
  (the stereo-sample (stereo-sample (floor (* left (stereo-left sample)))
                                    (floor (* right (stereo-right sample))))))


(defparameter *speed-of-sound* 34320 "Speed of sound in centimeters-per-second")
(defparameter *interaural-distance* 20 "Distance between ears, also in centimeters.")
(defparameter *size-of-earcanal* 0.5 "Characteristic size of the receiver, in centimeters.")
(defparameter *maximum-source-distance* 1000 "Maximum distance to the source, in centimeters.")

(defun p (x)
  (warn "~a" x)
  x)

(defun pyramidal-angle-cosine (phi theta)
  (handler-case (/ (- (+ 1
                         (/ (expt (cos phi) 2) (expt (cos theta) 2)))
                      (+ (expt (sin phi) 2)
                         (* (expt (tan theta) 2) (expt (cos phi) 2))))
                   (* 2 (cos phi) (/ (cos theta))))
    (division-by-zero () 0)))

(defun simplest-binaural (r phi theta)
  "Based on R, PHI and THETA of where the sound originated, calculates needed ratio of amplitudes
and time delay.
  PHI == 0, THETA == 0 - direction to the right ear
  PHI == 0, THETA == pi/2 - direction up"
  (let* ((cos-psi (pyramidal-angle-cosine phi theta))
         (r-left (sqrt (+ (expt r 2) (/ (expt *interaural-distance* 2) 4) (* r *interaural-distance* cos-psi))))
	 (r-right (sqrt (+ (expt r 2) (/ (expt *interaural-distance* 2) 4) (- (* r *interaural-distance* cos-psi))))))
    (values (/ *size-of-earcanal* r-left) (/ *size-of-earcanal* r-right)
	    (/ r-left *speed-of-sound*) (/ r-right *speed-of-sound*))))


(defclass naive-binaurer ()
  ((r :initform 10 :initarg :radius)
   (phi :initform (/ pi 2) :initarg :phi)
   (theta :initform 0 :initarg :theta)
   (streamer :initform (error "You should specify, what streamer should I binaurize") :initarg :streamer)
   left-mult right-mult
   left-delay-buffer
   right-delay-buffer
   (left-stop :initform nil)
   (right-stop :initform nil)
   (left-finished-p :initform nil)
   (right-finished-p :initform nil)
   rate
   my-mixer))

(defclass delay-buffer ()
  ((buf :accessor db-buf)
   (index :initform 0 :accessor db-index)
   (delay :accessor db-delay)
   (size :accessor db-size)))

(defmethod initialize-instance :after ((buffer delay-buffer)
				       &key element-type delay (size (1+ delay)) (initial-element 0))
  (assert (> size delay))
  (with-slots (buf index (s-delay delay) (s-size size)) buffer
    (setf buf (make-array size :element-type element-type :initial-element initial-element)
	  s-delay delay
	  s-size size)))

(defgeneric setf-delay-buffer (delay-buffer value)
  (:documentation "Writes a VALUE into one place of a buffer, while returning the other (earlier) in the buffer.")
  (:method ((delay-buffer delay-buffer) value)
    (with-slots (buf index delay size) delay-buffer
      (progn (setf (aref buf (mod (+ index delay) size)) value)
	     (let ((it (aref buf index)))
	       (setf index (mod (1+ index) size))
	       it)))))
			 
(defclass stoppable-delay-buffer (delay-buffer)
  ((stopped-p :initform nil :accessor sdb-stopped-p)
   (depleted-p :initform nil :accessor sdb-depleted-p)
   (stop-position :accessor sdb-stop-position)))

(defmethod setf-delay-buffer ((delay-buffer stoppable-delay-buffer) value)
  (if (sdb-stopped-p delay-buffer)
      (progn (when (and (slot-boundp delay-buffer 'stop-position)
			(equal (db-index delay-buffer) (sdb-stop-position delay-buffer)))
	       (setf (sdb-depleted-p delay-buffer) t))
	     (if (sdb-depleted-p delay-buffer)
		 0
		 (call-next-method delay-buffer 0)))
      (call-next-method)))

(defgeneric toggle-stop (buffer)
  (:method ((buffer stoppable-delay-buffer))
    (with-slots (size delay index) buffer
      (setf (sdb-stop-position buffer) (mod (+ index delay) size)
	    (sdb-stopped-p buffer) t))))

(defun init-simple-binaurer (streamer mixer)
  "Perform initialization of various constants and data buffers.
   We can only do this, once we know the rate of the mixer we are playing for."
    (with-slots (r phi theta
                   left-mult right-mult
		   left-delay-buffer right-delay-buffer
		   my-mixer rate) streamer
      (setf rate (slot-value mixer 'mixalot::rate))
      (multiple-value-bind (left-mult-new right-mult-new left-delay-new right-delay-new)
	  (simplest-binaural r phi theta)
	(setf left-mult left-mult-new
	      right-mult right-mult-new
	      left-delay-buffer (make-instance 'stoppable-delay-buffer
					       :delay (ceiling (* rate left-delay-new))
					       :element-type 'mono-sample
					       :size (ceiling (* rate (/ *maximum-source-distance* *speed-of-sound*))))
	      right-delay-buffer (make-instance 'stoppable-delay-buffer
						:delay (ceiling (* rate right-delay-new))
						:element-type 'mono-sample
						:size (ceiling (* rate (/ *maximum-source-distance* *speed-of-sound*)))))
	(setf my-mixer (make-instance 'dummy-mixer
				      :rate (slot-value mixer 'mixalot::rate)
				      :callback-on-streamer-remove (lambda ()
								     (toggle-stop left-delay-buffer)
								     (toggle-stop right-delay-buffer)))))))


(defmethod streamer-mix-into ((streamer naive-binaurer) mixer buffer offset length time)
  (declare (ignorable time))
  (when (not (slot-boundp streamer 'left-delay-buffer)) ; any related to buffers quantity will do here
    (init-simple-binaurer streamer mixer))
  (with-slots (n phase left-mult right-mult left-delay-buffer right-delay-buffer
		 my-mixer) streamer
    (let ((my-buffer (make-array length :element-type 'stereo-sample :initial-element 0)))
      (if (not (sdb-stopped-p left-delay-buffer)) ; could've tested right buffer as well
	  (streamer-write-into (slot-value streamer 'streamer) my-mixer my-buffer 0 length time))
      (loop for index upfrom 0
	 repeat length
	 as sample = (stereo->mono (aref my-buffer index))
	 do
	   (stereo-incf (aref buffer (+ index offset))
			(stereo-sample (setf-delay-buffer left-delay-buffer (mono->volumed-mono left-mult sample))
				       (setf-delay-buffer right-delay-buffer (mono->volumed-mono right-mult sample)))))
      (when (and (sdb-depleted-p left-delay-buffer) (sdb-depleted-p right-delay-buffer))
	(mixer-remove-streamer mixer streamer)))))

(defmethod streamer-cleanup ((stream naive-binaurer) mixer)
  (streamer-cleanup (slot-value stream 'streamer) (slot-value stream 'my-mixer)))

(defgeneric move-streamer (streamer dr dphi dtheta)
  (:documentation "Move the streamer, while it is being played.")
  (:method ((streamer naive-binaurer) dr dphi dtheta)
    (with-slots (r phi theta
                   left-mult right-mult
		   left-delay-buffer right-delay-buffer
		   my-mixer rate) streamer
      (if (or (> (+ theta dtheta) (/ pi 2))
              (< (+ theta dtheta) (- (/ pi 2))))
          (warn "Resulting theta would not fit interval [-pi/2, pi/2], not moving.")
          (multiple-value-bind (left-mult-new right-mult-new left-delay-new right-delay-new)
              (simplest-binaural (+ r dr) (+ phi dphi) (+ theta dtheta))
            (let ((left-delay-new (ceiling (* rate left-delay-new)))
                  (right-delay-new (ceiling (* rate right-delay-new))))
              (if (or (>= left-delay-new (db-size left-delay-buffer))
                      (>= right-delay-new (db-size right-delay-buffer)))
                  (warn "Too distant source (requested buffer size > than initially allocated), not moving.")
                  (progn (setf left-mult left-mult-new
                               right-mult right-mult-new
                               r (+ r dr)
                               phi (+ phi dphi)
                               theta (+ theta dtheta))
                         (macrolet ((frob (new-delay buffer)
                                      `(with-slots (buf index size delay) ,buffer
                                         (when (> ,new-delay delay)
                                           (iter (for i from delay to ,new-delay)
                                                 (setf (aref buf (mod (+ i index) size))
                                                       (aref buf (mod (+ delay index) size)))))
                                         (setf delay ,new-delay))))
                           (frob left-delay-new left-delay-buffer)
                           (frob right-delay-new right-delay-buffer)))))))
      (values r phi theta))))

    

(defclass test-streamer ()
  ((n :initform 0)
   (phase :initform 0.0)))

(defmethod streamer-mix-into ((streamer test-streamer) mixer buffer offset length time)
  (declare (ignore time))
  (with-slots (n phase) streamer
    (loop for index upfrom offset
	 repeat length
	 with freq = (+ 200 (* n 200))
	 with dp = (* 2.0 pi freq (/ (slot-value mixer 'mixalot::rate)))
	 as sample = (round (* 10000 (sin phase)))
	 do
	 (stereo-incf (aref buffer index) (mono->stereo sample))
	 (incf phase dp))
    (when (= (incf n) 6)
      (mixalot:mixer-remove-streamer mixer streamer))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *mixer* (create-mixer)))

(defmacro randomly-placed-source (streamer &optional (each-form 1))
  `(iter (while 1)
	 (mixer-add-streamer ,(intern "*MIXER*") (make-instance 'naive-binaurer
								:angle (random (* 2 pi))
								:radius (random 20)
								:streamer ,streamer
								))
	 (sleep ,each-form)))
	

