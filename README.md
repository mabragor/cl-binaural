cl-binaural
===========

Generate binaural sound, located at some point in space, from mono or stereo sound.

To get the idea, put your headphones on (this will not work very well with speakers) and execute

        CL-USER> (ql:quickload 'cl-binaural)
        CL-USER> (in-package cl-binaural)
        CL-BINAURAL> (randomly-placed-source (make-instance 'test-streamer))

You should hear one sound pattern appearing in different parts of space.

RANDOMLY-PLACED-SOURCE is a macro, which expands into repeated creation of streamer NAIVE-BINAURER
(which is the core of the package), with random position in space.
Another streamer, in this case, TEST-STREAMER, acts as a parameter to NAIVE-BINAURER.

Needless to say, you may feed your favorite streamer, such as (make-mp3-streamer "/my-file.mp3") to
RANDOMLY-PLACED-SOURCE.

The following snippet illustrates typical use of NAIVE-BINAURER

        CL-BINAURAL> (defparameter *mixer* (create-mixer))
        CL-BINAURAL> (mixer-add-streamer *mixer*
                       (make-instance 'naive-binaurer
                                      :angle (/ pi 6) ; angle between ears axis and the source direction
                                      :radius 20 ; distance to the source, in centimeters
                                      :streamer #<MY-STREAMER> ; streamer object to binaurize
                                      ))
        ;; This will lead to the illusion that MY-STREAMER is sounding from point (:radius, :angle)

So far NAIVE-BINAURER implements really naive binaural model, in which no head-related-transfer-functions are
taken into account.
What is, however, taken into account, is time, that it takes for sound to reach each ear, and
decrease of amplitude of sound with distance to the source.


Binaural streamers can be moved around, while they are being played, for example

        CL-BINAURAL> (defparameter *streamer* (make-instance 'naive-binaural ...)) ; name for future reference
        CL-BINAURAL> (mixer-add-streamer *mixer* *streamer*) ; start playback
        CL-BINAURAL> (move-streamer *streamer* 5 0.1) ; this will increase radius by 5 cm and phi by 0.1 radian.

Hence, repeatedly calling MOVE-STREAMER with small increments really feels like source moving continuously!

TODO:
  - of course, it would be nice to incorporate some HRTFs, to make this feasible for creation of
    'virtual audio reality'
  - algorithms are far from optimal, and the whole thing lags, if you simultaneously feed enough
    NAIVE-BINAURERs to the mixer, hence it would be good to speed up.
