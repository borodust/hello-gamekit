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

(gamekit:register-resource-package :keyword
                                   (asdf:system-relative-pathname :hello-gamekit "assets/"))


(gamekit:define-image :snake-head "snake-head.png")
(gamekit:define-sound :snake-grab "snake-grab.ogg")

;;;
;;; Main class we use to run our application
;;;
(gamekit:defgame hello-gamekit () ()
  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "Hello Gamekit!")
  (:panels 'ui-demo-window))


(ge.ui:defpanel (ui-demo-window
              (:title "UI Demo")
              (:origin 200 50)
              (:width 400) (:height 400)
              (:options :movable :resizable
                        :minimizable :scrollable
                        :closable))
  (ge.ui:label :text "Nested:")
  (ge.ui:horizontal-layout
   (ge.ui:radio-group
    (ge.ui:radio :label "Option 1")
    (ge.ui:radio :label "Option 2" :activated t))
   (ge.ui:vertical-layout
    (ge.ui:check-box :label "Check 1" :width 100)
    (ge.ui:check-box :label "Check 2"))
   (ge.ui:vertical-layout
    (ge.ui:label :text "Awesomely" :align :left)
    (ge.ui:label :text "Stacked" :align :middle)
    (ge.ui:label :text "Labels" :align :right)))
  (ge.ui:label :text "Expand by width:")
  (ge.ui:horizontal-layout
   (ge.ui:button :label "Dynamic")
   (ge.ui:button :label "Min-Width" :width 80)
   (ge.ui:button :label "Fixed-Width" :expandable nil :width 100))
  (ge.ui:label :text "Expand by ratio:")
  (ge.ui:horizontal-layout
   (ge.ui:button :label "1.0" :expand-ratio 1.0)
   (ge.ui:button :label "0.75" :expand-ratio 0.75)
   (ge.ui:button :label "0.5" :expand-ratio 0.5))
  (ge.ui:label :text "Rest:")
  (ge.ui:button :label "Top-Level Button"))


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


(defmethod gamekit:act ((app hello-gamekit))
  (update-position (aref *curve* 1) (real-time-seconds))
  (update-position (aref *curve* 2) (+ 0.1 (real-time-seconds))))

;;;
;;; All the drawing should happend in this method
;;;
(defmethod gamekit:draw ((app hello-gamekit))
  (gamekit:draw-text "A snake that is!" (gamekit:vec2 300 400))
  (gamekit:draw-curve (aref *curve* 0)
                      (aref *curve* 3)
                      (aref *curve* 1)
                      (aref *curve* 2)
                      *black*
                      :thickness 5.0)
  ;; let's center image position properly first
  (let* ((half-width (/ (gamekit:image-width :snake-head) 2))
         (half-height (/ (gamekit:image-height :snake-head) 2))
         (head-image-position (gamekit:subt (aref *curve* 3)
                                            (gamekit:vec2 half-width half-height))))
    ;; then draw it where it belongs
    (gamekit:draw-image head-image-position :snake-head)))
