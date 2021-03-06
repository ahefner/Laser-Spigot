;;;; Global definitions.

;;;; We have to put them here and define them during compilation,
;;;; otherwise the compiler will warn when compiling files that use
;;;; them, because it hasn't necessarily seen the definition.

;;;; That said, I don't see why I couldn't move some of this shit to
;;;; uim-defs.lisp.

(in-package :g1)

(defvar *packset* nil)

(defvar *gadget-root*)
(defvar *gameui*)
(defvar *presentation-stack*)
(defvar *grab-id* nil)

(defparameter *presentation-query* (constantly :discard))

(defvar *global-owner* nil)

(defvar *player*)
(defvar *universe*)

(defvar *button-a* nil)                     ; Standard button-style

;; Really, you need one of these per text style. This will be for the "default" text style (:sans 11)
(defvar *word-map* (make-hash-table :test 'equal))

(defvar *debug-show-packset* nil)

(defvar *total-frames* 0)
(defvar *devmode* t)

;;;; ...

(defstruct cacheobj value derived)      ; Nowhere else reasonable to define this..

;;;; Inline functions

(declaim (inline f->b))

(defun f->b (f)
  (clamp (round (* f 255)) 0 255))

