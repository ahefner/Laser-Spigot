(in-package :g1)

;;;(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0)))

(ffi:clines "#include \"sys.h\"")

(defun uic-mouse-vector (uic)
  (v2 (uic-mx uic) (uic-my uic)))

(defun child-uic (uic &key (dx 0) (dy 0) width height (active t))
  (let ((new (copy-uic uic)))
    (setf (uic-active new) (and active (uic-active uic))) ; Don't override deactivation by a parent.
    (incf (uic-abx new) dx)
    (incf (uic-aby new) dy)
    (decf (uic-mx  new) dx)
    (decf (uic-my  new) dy)
    (when width  (setf (uic-width  new) width))
    (when height (setf (uic-height new) height))
    new))

(defun reparent-gadget (gadget new-parent)
  (setf (next-gadget (parent-gadget gadget)) new-parent
        (parent-gadget new-parent) (parent-gadget gadget)
        (next-gadget new-parent) gadget
        (parent-gadget gadget) new-parent))

(defgeneric gadget-key-pressed (gadget uic keysym char)
  (:method (gadget uic keysym char)
    (declare (ignore gadget uic keysym char))))

(defgeneric gadget-key-released (gadget uic keysym)
  (:method (gadget uic keysym)
    (declare (ignore gadget uic keysym))))

(defgeneric gadget-run (gadget uic)
  (:method ((gadget null) uic)
    (declare (ignore gadget uic)))
  (:method ((gadget gadget) uic)
    (gadget-run (next-gadget gadget) uic)))

(defun reactivate-gadget (gadget)
  (setf *gadget-root* gadget))

(defun activate-new-gadget (gadget)
  (setf (next-gadget gadget) *gadget-root* 
        *gadget-root* gadget))

(defun pop-gadget (gadget)
  ;; Maybe this is a stupid check, and we ought to pop everything
  ;; ahead of the gadget as well.
  (assert (not (null (next-gadget gadget))))
  (assert (eql gadget *gadget-root*))
  (finalize-object *gadget-root*)
  (reactivate-gadget (next-gadget gadget)))

(defun update-modifier-masks (uic last-uic)
  (let ((now (uic-modifiers uic))
        (then (uic-modifiers last-uic)))
    (setf (uic-modifiers-pressed  uic) (logand now  (logxor now then))
          (uic-modifiers-released uic) (logand then (logxor now then)))))

;;;; Mouse grabbing semantics: Grabbing is only intended to last the
;;;; duration that the left button is held. Once you've grabbed the
;;;; mouse, the global UIC is deactivated until the grab (or the
;;;; button) is released.
(defun grab-mouse (grab-id)
  (cond
    (*grab-id* (format *trace-output* "Attempt to grab pointer by ~A, but already grabbed by ~A" grab-id *grab-id*))
    (t (setf *grab-id* grab-id))))

(defun release-mouse (grab-id)
  (unless (eql *grab-id* grab-id)
    (format *trace-output* "Release grab attempt by ~A, but *grab-id* is ~A~%" grab-id *grab-id*))
  (setf *grab-id* nil))

;;; Why the fuck is this here?
(defvar *swank-running* nil)

(defun start-swank (&key background-p)
  (declare (ignorable background-p))
  (unless (or *swank-running* (not *devmode*))
    (require :asdf)
    (eval (read-from-string 
           "(push '(MERGE-PATHNAMES \".sbcl/systems/\" (USER-HOMEDIR-PATHNAME)) asdf:*central-registry*)"))
    (eval (read-from-string "(asdf:oos 'asdf:load-op :swank)"))
    (flet ((run ()
             (unwind-protect
                  (progn
                    (setf *swank-running* t)
                    (format t "~&------------ STARTING SWANK SERVER -----------~%")
                    (eval (read-from-string "(swank:create-server :port 0 :coding-system :utf-8)")))
               (setf *swank-running* nil))))
      #-threads (run)
      #+threads
      (if background-p (mp:process-run-function 'swank-process #'run) (run)))))


;;;; Helper functions - Clicked/Released are edge-triggered and the
;;;; mask is a disjunction. Held mask is a conjunction.

(defun clicked? (uic &optional (button-mask +left+))
  (and (uic-active uic)
       (not (zerop (logand (uic-buttons-pressed uic) button-mask)))))

(defun released? (uic &optional (button-mask +left+))
  (and (uic-active uic)
       (not (zerop (logand (uic-buttons-released uic) button-mask)))))

(defun held? (uic button-mask)
  (= button-mask (logand (uic-buttons uic) button-mask)))

(defun no-modifiers (uic)
  (zerop (logand (uic-modifiers-pressed uic) (logior +alt-mask+ +control-mask+))))

(defun pointer-normsq* (uic x y) (+ (square (- x (uic-mx uic))) (square (- y (uic-my uic)))))
(defun pointer-in-radius* (uic radius x y) (<= (pointer-normsq* uic x y) (square radius)))
  

;;;; Presentations

(defun push-new-presentation (object type children)
  (push (make-presentation :object object :type (or type object) :children children)
        *presentation-stack*))


;;;; Gadgetry - Image Button

(defun run-img-button (uic img-up img-down x y &key (clicked-sound :click-mid))
  (let ((in (and (uic-active uic) (pointer-in-img-rect uic img-up x y))))
    (draw-img (if (and in (held? uic +left+)) img-down img-up) x y)
    (and in (released? uic +left+) (prog1 t (and clicked-sound (play-sound clicked-sound))))))

;;;; Gadgetry - Labelled Button

(defun labelled-button-rect (button-style label x y &key min-width (center-x t))
  (let* ((style (button-style-released button-style))
         (label-width (max (or min-width 0) (if label (img-width label) 0)))
         (bar-width (+ label-width (bar-style-width style)))
         (lx (if center-x (- x (ash bar-width -1)) x)))
  (values lx y (+ lx bar-width) (+ y (bar-style-height style)))))

(defun run-labelled-button (uic label x y &key min-width (center-x t) (color (vector 255 255 255)) (style *button-a*)
                            (clicked-sound (sound-effect :click-high)))
  (let ((in (and (uic-active uic)
                 (multiple-value-call #'pointer-in-rect* uic
                   (labelled-button-rect style label x y :min-width min-width :center-x center-x)))))
    (draw-button style label (and in (held? uic +left+)) x y :min-width min-width :center-x center-x :color color)
    (and in (released? uic +left+) 
         (prog1 t (and clicked-sound (play-sound clicked-sound))))))

;;;; Gadgetry - Slider

(defun run-slider (id uic x y value range &optional disable)
  (let ((fill (min 160 (round (* 160 (if (zerop range) 0 (/ value range))))))
        (in (pointer-in-rect* uic x y (+ x 160) (+ y 17))))
    (bind-texobj (texture :slider-160))
    (draw-tile x y (+ x fill) (+ y 17) 0 0 (if disable #(150 150 150) #(255 255 255)))
    (draw-tile (+ x fill) y (+ x 160) (+ y 17) fill 0 (if disable #(100 100 100) #(190 190 190)))
    (when (and id in (clicked? uic +left+)) (grab-mouse id))
    (if (or (eq id *grab-id*)
            (and (uic-active uic) in (held? uic +left+)))
        (round (* range (/ (clamp (- (uic-mx uic) x) 0 160) 160)))
        value)))

;;;; Panels

(defgeneric dismiss-panel (panel)
  (:method :before (panel) (setf (closing-p panel) t))
  (:method (panel) (declare (ignore panel)) (values)))

(defun draw-panel-background (uic bottom)
  (let* ((left (img :panel-left))
         (right (img :panel-right))
         (edge-top (- bottom (img-height left))))
    (draw-bar* left right (texture :panel-fill) 0 edge-top (uic-width uic))
    (fill-rect* 0 0 (uic-width uic) edge-top 7 7 7 244)))

(defun run-hosted-panel (uic host bottom)
  (let* ((pointer-in-host (< (uic-my uic) bottom))
         (child-uic (child-uic uic :active (not pointer-in-host))))
    (with-slots (panel panel-y closing-panel) host
      (cond 
        (panel
         (let* ((target (if closing-panel 0 (+ bottom (panel-height panel))))
                (dist (- target panel-y))
                (rate (* 10 (if (< dist 0)
                                (min -1 (- (floor (* (uic-delta-t uic) (sqrt (- dist))))))
                                (max  1 (ceiling  (* (uic-delta-t uic) (sqrt dist))))))))
           (setf panel-y (clamp (+ panel-y rate) 0 (+ bottom (panel-height panel))))
           (when closing-panel (setf (uic-active child-uic) nil))
           (run-panel panel child-uic panel-y)
           (when (and closing-panel (<= panel-y bottom))
             (finalize-object panel)
             (setf panel nil
                   panel-y 0
                   closing-panel nil))))
        (t (gadget-run (next-gadget host) child-uic))))))

(defun close-panels (&optional (host *gameui*))
  (with-slots (panel closing-panel) host
    (when panel 
      ;; Tricky: This might be called when the panel is already
      ;; closing, even every frame, so only play the sound once.
      (unless closing-panel (play-sound :close1))
      (dismiss-panel panel)
      (setf closing-panel t))))

(defun activate-panel (new-panel &key (host *gameui*) (update-p nil))
  (with-slots (panel closing-panel) host
    (when panel
      (setf (panel-y-of host) (+ (- (panel-y-of host) (panel-height panel))
                                 (panel-height new-panel)))
      (finalize-object panel))
    (unless update-p
      (play-sound (if panel :click-high :open1)))
    (setf panel new-panel
          (host-of panel) host
          closing-panel nil)))

(defun update-panel  (new-panel &key (host *gameui*))
  (activate-panel new-panel :host host :update-p t))

;;;; Cursor Layout Utility

(defun cursor-draw-img (cursor img &key (color (cursor-color cursor)) (shadow nil))
  (when (cursor-newline-p cursor)
    (setf (cursor-x cursor) (cursor-left cursor)
          (cursor-newline-p cursor) nil))
  (when shadow
    (draw-img-deluxe img (1- (cursor-x cursor)) (1+ (cursor-y cursor)) #(0 0 0 255)))
  (draw-img-deluxe img (cursor-x cursor) (cursor-y cursor) color)
  (maxf (cursor-descent cursor) (- (img-height img) (img-y-offset img)))
  (incf (cursor-x cursor) (img-width img)))

(defun cursor-newline (cursor)
  (incf (cursor-y cursor) (max (cursor-min-line-height cursor)
                               (+ (cursor-y-pad cursor) (cursor-descent cursor))))
  (setf (cursor-newline-p cursor) t
        (cursor-descent cursor) 0
        (cursor-x cursor) (cursor-left cursor)))

(defun cursor-draw-line (cursor images)
  (if (or (listp images) (vectorp images))
      (map nil (lambda (img) (cursor-draw-img cursor img)) images)
      (cursor-draw-img cursor images))
  (cursor-newline cursor))

(defun cursor-draw-lines (cursor line-seq)
  (map nil (lambda (line) (cursor-draw-line cursor line)) line-seq))

(defun cursor-advance (cursor advance)
  (unless (cursor-x cursor)
    (setf (cursor-x cursor) (cursor-left cursor)))
  (setf (cursor-newline-p cursor) nil)
  (incf (cursor-x cursor) advance))

(defun cursor-tab (cursor tabstop &optional (min-spacing 20))
  (cursor-advance cursor 0)
  (let ((next (* tabstop (1+ (floor (cursor-x cursor) tabstop)))))
    (cond
      ((< next (+ (cursor-x cursor) min-spacing)) 
       (cursor-advance cursor min-spacing))
      (t (setf (cursor-x cursor) next)))))

;;;; Gadget to fade out the screen, display a child gadget, then fade
;;;; back in when the child pops.

(defclass in-and-out ()
  ((io-level :reader level-of :initform 0.0)
   (io-state :initform :in)
   (io-rate  :initform 2.0 :initarg :rate)))

(defclass fade-transition-gadget (gadget in-and-out)
  ((child :initarg :child)
   (color :initform #(0 0 0) :initarg :color)))

(defun run-in-and-out (io delta-t)
  "Run in/out transition state, returning level, state transition (:opened or :closed) and state (:in, t, :out, nil)."
  (with-slots (io-level io-state io-rate) io
    (setf io-level (clamp (+ io-level (* (case io-state ((nil :out) -1.0) (otherwise 1.0))
                                         io-rate
                                         (max 0.0001 delta-t)))
                          #|min|# 0.0 #|max|# 1.0))
    (let ((old-state io-state))
      (case io-state
        (:in (when (> io-level 0.9999) (setf io-state t)))
        (:out (when (< io-level 0.001) (setf io-state nil))))
      (values io-level
              (and (not (eq old-state io-state))
                   (if (eq io-state t) :opened :closed))
              io-state))))

(defun io-request-close (io &optional new-rate)
  (with-slots (io-state io-rate) io
    (setf io-rate (or new-rate io-rate))
    (when io-state
      (setf io-state :out))))

(defmethod gadget-run ((gadget fade-transition-gadget) uic)
  (with-slots (child color) gadget
    (multiple-value-bind (level transition state) (run-in-and-out gadget (uic-delta-t uic))
      (unless (> level 0.999)
        (call-next-method gadget (child-uic uic :active nil)))
      (with-vector (c color)
        (fill-rect* 0 0 (uic-width uic) (uic-height uic) c.x c.y c.z (* 255.0 level)))
      
      (case transition
        (:opened (activate-new-gadget child))
        (:closed (pop-gadget gadget)))
      
      (when (and (eql state t) (eql *gadget-root* gadget))
        (io-request-close gadget)))))

;;;; Modal bottom-panel host gadget.

(defclass modal-bottom-panel-host (gadget)
  ((panel :accessor panel-of :initform nil :initarg :init-panel)
   (child-queue :reader child-queue-of :initform nil :initarg :child-queue)
   (fade :initform 128.0 :initarg :fade)
   (fade-io  :initform (make-instance 'in-and-out))
   (panel-io :initform (make-instance 'in-and-out))))

;; I didn't bother making a generic API for panels to request close.

(defun enqueue-next-panel (bottom-panel-host panel)
  (with-slots (child-queue) bottom-panel-host
    (setf child-queue (append child-queue (list panel)))))

;; (bottom-panel-host-request-close bottom-panel-host)

(defun bottom-panel-host-request-close (panel-host)
  (with-slots (child-queue fade-io panel-io) panel-host
    (io-request-close panel-io)
    (when (null child-queue)
      (io-request-close fade-io))))

(defun bottom-panel-request-close (panel)
  (bottom-panel-host-request-close (host-of panel)))

(defmethod gadget-run ((gadget modal-bottom-panel-host) uic)
  (with-slots (panel fade fade-io panel-io child-queue) gadget
    (when (null panel) (setf panel (pop child-queue)))
    (multiple-value-bind (fade-level fade-transition) (run-in-and-out fade-io (uic-delta-t uic))
      (unless (< (* fade fade-level) 1.0)
        (call-next-method gadget (child-uic uic :active nil)))
      (fill-rect* 0 0 (uic-width uic) (uic-height uic) 0 0 0 (round (* fade fade-level)))
      (multiple-value-bind (p-level p-transition p-state) (run-in-and-out panel-io (uic-delta-t uic))
        (when panel
          (setf (host-of panel) gadget)
          (run-panel panel uic
                     (- (uic-height uic)
                        (round (* (expt p-level (if (eql p-state :out) 2 0.5))
                                  (panel-height panel)))))
          (when (eql p-transition :closed)
            (finalize-object panel)
            (setf panel nil)
            (when child-queue
              (setf panel (pop child-queue)
                    panel-io (make-instance 'in-and-out)))))

        (when (eql fade-transition :closed)
          (pop-gadget gadget))))))

;;;; Sequencer gadget - runs a list of functions, which may do
;;;; processing or choose to invoke modal UIs. If a function modifies
;;;; the UI root, subsequent functions will be delayed until the
;;;; sequencer-gadget becomes the UI root again.

(defclass sequencer-gadget (gadget)
  ((functions :initarg :functions :accessor functions-of)))

(defmethod gadget-run ((gadget sequencer-gadget) uic)
  (call-next-method gadget (child-uic uic :active nil))
  (loop while (and (functions-of gadget) (eq *gadget-root* gadget))
        do (funcall (pop (functions-of gadget))))
  (when (and (eq *gadget-root* gadget) (not (functions-of gadget)))
    (pop-gadget gadget)))

