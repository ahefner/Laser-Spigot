(in-package :g1)

;;;; Stolen from Alexandria:

(defun random-elt (sequence &key (start 0) end)
  (declare (sequence sequence) (fixnum start) (type (or fixnum null) end))
  (elt sequence (+ start (random (- (or end (length sequence)) start)))))


(defun shuffle (sequence &key (start 0) end)
  "Returns a random permutation of SEQUENCE bounded by START and END.
Permuted sequence may share storage with the original one. Signals an
error if SEQUENCE is not a proper sequence."
  (declare (fixnum start) (type (or fixnum null) end))
  (typecase sequence
    (list
     (let* ((end (or end (length sequence)))
            (n (- end start)))
       (do ((tail (nthcdr start sequence) (cdr tail)))
           ((zerop n))
         (rotatef (car tail) (car (nthcdr (random n) tail)))
         (decf n))))
    (vector
     (let ((end (or end (length sequence))))
       (loop for i from (- end 1) downto start
             do (rotatef (aref sequence i) (aref sequence (random (+ i 1)))))))
    (sequence
     (let ((end (or end (length sequence))))
       (loop for i from (- end 1) downto start
             do (rotatef (elt sequence i) (elt sequence (random (+ i 1))))))))
  sequence)

;;; Note that while I declare this inline, it won't actually happen
;;; unless you put util.lisp in the compile-time deps of the relevant
;;; source module.

(declaim (inline clamp lerp nonnull))

(defun nonnull (foo)
  (assert foo)
  foo)

(defun clamp (x min max)
  (max min (min max x)))

(defun lerp (param from to)
  (+ (* from (- 1.0f0 param)) (* to param)))

(defun unit-ramp-in (ramp-start ramp-end value)
  (clamp (/ (- value ramp-start) (float (- ramp-end ramp-start) 0.0f0)) 0.0f0 1.0f0))

(defun printl (&rest args) 
  (print args)
  (finish-output))

(defun whenzero (string number)
  (if (zerop number) string number))

(defun color-lighten (color)
  (macrolet ((f (x) `(ash (+ ,x 255) -1)))
    (vector (f (aref color 0)) (f (aref color 1)) (f (aref color 2)))))

(defun color-darken (color)
  (macrolet ((f (x) `(ash ,x -1)))
    (vector (f (aref color 0)) (f (aref color 1)) (f (aref color 2)))))

(defun color-half-darken (color)
  (macrolet ((f (x) `(round (* ,x 0.666))))
    (vector (f (aref color 0)) (f (aref color 1)) (f (aref color 2)))))

(defun color-with-alpha (color alpha)
  (vector (aref color 0) (aref color 1) (aref color 2) alpha))

;;; Like delete-if, but guaranteed to modify vector in place and
;;; adjust fill pointer.. that you can't rely on CL:DELETE* functions
;;; in this case is idiotic beyond belief.
(declaim (inline better-delete-if))
(defun sane-delete-if (predicate vector-with-fill-pointer)
  (loop with v = vector-with-fill-pointer
        with n = 0
        for item across vector-with-fill-pointer
        unless (funcall predicate item)
        do
        (setf (aref v n) item)
        (incf n)
        finally
        (setf (fill-pointer v) n)
        (return v)))

(defun pretty-sym (symbol)
  (string-capitalize (substitute #\Space #\- (symbol-name symbol))))

(defun iftype (type) (lambda (object) (typep object type)))

(defun file (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax ()
      (let ((*read-eval* nil)
            (*package* (find-package :g1)))
        (read in)))))

(defun maybe-file (filename)
  (if (probe-file filename)
      (values (file filename) t)
      (values nil nil)))

(defun show (name value)
  (printl name value)
  value)

(defun degrees->radians (degrees) (* degrees 2 pi (/ 360)))

(defun copy-bit-matrix (matrix)
  (let ((copy (make-array (array-dimensions matrix) :element-type 'bit)))
    (loop for i from 0 below (array-total-size matrix)
          do (setf (row-major-aref copy i) (row-major-aref matrix i))
          finally (return copy))))

(defun copy-matrix-into (matrix into)
  (loop for i from 0 below (array-total-size matrix)
        do (setf (row-major-aref into i) (row-major-aref matrix i))
        finally (return into)))

(defun copy-matrix (matrix)
  (copy-matrix-into matrix (make-array (array-dimensions matrix))))

(defun map-matrix-into (matrix into fn)
  (loop for i from 0 below (array-total-size matrix)
        do (setf (row-major-aref into i) (funcall fn (row-major-aref matrix i)))
        finally (return into)))

(defun map-matrix (matrix fn)
  (map-matrix-into matrix (make-array (array-dimensions matrix)) fn))

;;;; Minheap


(declaim (inline heap-parent-index heap-left-index heap-right-index))
(defun heap-parent-index (n) (ash (1- n) -1))
(defun heap-left-index (n) (1+ (* 2 n)))
(defun heap-right-index (n) (+ 2 (* 2 n)))

(defstruct minheap (array (make-array 128)) (n 0))

(defun minheap-insert (heap item-pair)
  (declare (optimize (speed 3) (debug 0)))
  (let ((array (minheap-array heap))
        (n (minheap-n heap)))
    ;; Check length, enlarge if needed.
    (when (= n (length array))
      #+NIL
      (format *trace-output* "~&Growing heap from ~:D to ~:D~%"
              (length array)
              (* 2 (length array)))
      (let ((new (make-array (* (length array) 2) :initial-element nil)))        
        (loop for i from 0 below n do (setf (aref new i) (aref array i)))
        (setf (minheap-array heap) new
              array new)))
    (incf (minheap-n heap))
    ;; Insert and bubble up.
    (setf (aref array n) item-pair)
    (loop as parent = (heap-parent-index n)
          while (and (> n 0) 
                     (> (car (aref array parent))
                        (car (aref array n)))) do
          (rotatef (aref array parent) (aref array n))
          (setf n (heap-parent-index n)))))

(defun minheap-empty-p (heap) (zerop (minheap-n heap)))

(defun minheap-remove (heap)
  (declare (optimize (speed 3) (debug 0)))
  (let ((array (minheap-array heap))
        (n (1- (minheap-n heap)))
        (index 0)
        value)
    ;; Here, 'n' becomes the index of the last item.
    (prog1 (aref array 0)
      (when (= n 0)
        (decf (minheap-n heap))
        (setf (aref array 0) nil))
      (unless (<= n 0)
        (rotatef (aref array 0) (aref array n))
        (decf (minheap-n heap))
        (decf n)
        (setf value (car (aref array 0)))
        (tagbody recurse
           (let* ((left-index (heap-left-index index))
                  (right-index (1+ left-index)))
             (cond
               ((> left-index n) #|Done!|#)
               ;; Have at least a left child. If we don't have a right
               ;; child, the left child can't have any children either.
               ((> right-index n)
                (when (< (car (aref array left-index)) value)
                  (rotatef (aref array left-index) (aref array index))))
               ;; Must have two children. Swap with lowest child and recurse.
               (t (let* ((left  (car (aref array left-index)))
                         (right (car (aref array right-index)))
                         (min-value (min left right))
                         (min-index (if (eql left min-value) left-index right-index)))
                    (when (> value min-value)
                      (rotatef (aref array index) (aref array min-index))
                      (setf index min-index)
                      (go recurse)))))))))))