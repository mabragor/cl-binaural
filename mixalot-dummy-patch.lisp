(in-package #:mixalot)

(defclass dummy-mixer ()
  ((rate :initform 44100 :initarg :rate)
   (callback :initform (lambda () nil) :initarg :callback-on-streamer-remove :reader dummy-mixer-callback)))

(defmethod mixer-remove-streamer ((mixer dummy-mixer) streamer)
  (declare (ignore streamer))
  (funcall (dummy-mixer-callback mixer)))

(export '(dummy-mixer))

