(cl:defpackage :hello-gamekit
  (:use :cl)
  (:export hello-gamekit))

(cl:in-package :hello-gamekit)

;;;
;;; Some state we need
;;;
(defvar *canvas-width* 800)
(defvar *canvas-height* 600)
(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *head-grabbed-p* nil)
(defvar *curve* (make-array 4 :initial-contents (list (gamekit:vec2 300 300)
                                                      (gamekit:vec2 375 300)
                                                      (gamekit:vec2 425 300)
                                                      (gamekit:vec2 500 300))))

;;;
;;; Main class we use to run our application
;;;
(defclass hello-gamekit (gamekit:gamekit-system) ()
  (:default-initargs
   :resource-path (asdf:system-relative-pathname :hello-gamekit "assets/")
   :viewport-width *canvas-width*
   :viewport-height *canvas-height*
   :viewport-title "Hello Gamekit!"))


;;;
;;; Here we initialize resources we saved in /tmp/hello-gamekit-assets/
;;; directory
;;;
(defmethod gamekit:initialize-resources ((app hello-gamekit))
  (gamekit:import-image :snake-head "snake-head.png")
  (gamekit:import-sound :snake-grab "snake-grab.ogg"))


;;;
;;; Every time system starts, we need to rebind actions
;;;
(defmethod gamekit:post-initialize ((app hello-gamekit))
  (gamekit:bind-cursor (lambda (x y)
                       "When left mouse button is pressed, update snake's head position"
                       (when *head-grabbed-p*
                         (let ((head-position (aref *curve* 3)))
                           (setf (gamekit:x head-position) x
                                 (gamekit:y head-position) y)))))

  (gamekit:bind-button :mouse-left :pressed
                       (lambda ()
                         (gamekit:play :snake-grab)
                         (setf *head-grabbed-p* t)))

  (gamekit:bind-button :mouse-left :released
                       (lambda () (setf *head-grabbed-p* nil))))


(defun real-time-seconds ()
  "Return seconds since certain point of time"
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun update-position (position time)
  (let* ((subsecond (nth-value 1 (truncate time)))
         (angle (* 2 pi subsecond)))
    (setf (gamekit:y position) (+ 300 (* 100 (sin angle))))))


;;;
;;; All the drawing should happend in this method
;;;
(defmethod gamekit:draw ((app hello-gamekit))
  (gamekit:print-text "A snake that is!" 300 400)
  (update-position (aref *curve* 1) (real-time-seconds))
  (update-position (aref *curve* 2) (+ 0.1 (real-time-seconds)))
  (gamekit:draw-curve (aref *curve* 0)
                      (aref *curve* 3)
                      (aref *curve* 1)
                      (aref *curve* 2)
                      *black*
                      :thickness 5.0)
  ;; let's center image position properly first
  (let ((head-image-position (gamekit:subt (aref *curve* 3) (gamekit:vec2 32 32))))
    ;; then draw it where it belongs
    (gamekit:draw-image head-image-position :snake-head)))
