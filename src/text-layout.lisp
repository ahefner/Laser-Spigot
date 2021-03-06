;;;; Word-wrapping text layout engine.

;;;; How to use this: Call TYPESET-TEXT to get a list of typeset
;;;; words. Cache this. Call DRAW-TYPESET-TEXT to display them.

(in-package :g1)

(defun split-text (string &optional (start 0) (end (length string)))
  (loop with paragraphs = nil do
        (multiple-value-bind (paragraph next-index)
            (paragraph-split-words string start end)
          (push paragraph paragraphs)
          (when (not next-index) (return-from split-text (nreverse paragraphs)))
          (setf start next-index))))

;;; No, I'm not interested in your newfangled regexp engines.
(defun paragraph-split-words (string &optional (start 0) (end (length string)))
  (loop with words = nil 
        while (< start end) do
        (let ((char (aref string start)))
          (cond
            ((char= char #\Space) (incf start))
            ((char= char #\Newline) (return-from paragraph-split-words (values (nreverse words) (1+ start))))
            (t (let ((next (position-if (lambda (c) (or (char= c #\Space) (char= c #\Newline)))
                                        string :start start :end end)))
                 (unless (eql start (or next end))
                   (push (subseq string start next) words))
                 (unless next
                   (return-from paragraph-split-words (values (nreverse words) nil)))
                 (setf start next)))))))

(defun ensure-word (wordmap string)
  ;; Obvious quick kludge to support typesetting in multiple text styles.
  (orf (gethash string wordmap) (global-label (gethash :style wordmap :bold) 
                                              (gethash :size wordmap 12) 
                                              string)))

;;;#+SBCL (defun global-label (a b string) (make-img :name string :width (random 50))) ; Testing hack

(defparameter *justify-threshold* 0.80)

(defstruct typeset-word x y img)

(defun typeset-line (column-width y imgs space-width justify-p)
  (let* ((total-width (reduce #'+ imgs :key #'img-width))
         (num-words (length imgs))
         (justify-p (and justify-p (>= (+ total-width (* space-width (1- num-words)))
                                       (* column-width *justify-threshold*))))
         (eff-space-width (if justify-p
                              (float (min (* space-width 2)
                                          (/ (- column-width total-width) (max 1 (1- num-words)))))
                              space-width)))
    (loop for img in imgs
          with x = 0
          collect (make-typeset-word :x (round x) :y y :img img)
          do
          (incf x (+ (img-width img) eff-space-width)))))

(defun typeset-paragraph (wordmap words column-width line-height &key (justify t) (space-width 4) (y 0))
  (let* ((imgs (mapcar (lambda (word) (ensure-word wordmap word)) words))
         (next-y 0))
    (labels ((grind (imgs y)
               (if (not imgs)
                   (prog1 nil (setf next-y y))
                   (loop for num-words upfrom 0
                         for img in imgs
                         for width = (+ space-width (img-width img)) then (+ width (img-width img))
                         ;; Important: Don't let one overly wide image cause infinite recursion
                         while (or (zerop num-words) (<= width column-width))
                         collect img into line
                         do (incf width space-width)
                         finally (return (nconc
                                          (typeset-line column-width y line space-width justify)
                                           (grind (nthcdr num-words imgs) (+ y line-height))))))))
      (values (grind imgs y) next-y)))) ; Ugly, what the hell am I doing?

(defun typeset-text (wordmap column-width text &key
                     (line-height 14)
                     (justify t)
                     (space-width 3)
                     (y0 0)
                     (paragraph-spacing 9))
  (loop with tspara = nil
        with y = y0
        for paragraph in (split-text text)
        do
        (setf (values tspara y) (typeset-paragraph wordmap paragraph column-width line-height
                                                   :justify justify :space-width space-width :y y))
        (incf y paragraph-spacing)
        nconcing tspara))

(defun draw-typeset-text (text x0 y0 color)
  (loop for tsword in text do (draw-img-deluxe (typeset-word-img tsword)
                                               (+ x0 (typeset-word-x tsword))
                                               (+ y0 (typeset-word-y tsword))
                                               color)))

(with-vars ((last-text nil)
            (last-n 0))
  (defun fancy-draw-typeset-text (text x0 y0 color transition)
    (when (not (eq last-text text))
      (setf last-text text)
      (setf last-n 0))
    (loop for tsword in text
          for n upfrom 0
          for base upfrom 0 by 64
          as alpha = (clamp (- transition base) 0 255)
          do
          ;; Screw this idea.
          #+NIL
          (when (zerop alpha)
            (unless (eql last-n n)
              (play-sound (random-elt '(:foo :foo2) #+NIL '(:blip1 :blip2 :blip3)))
              (setf last-n n))
            (loop-finish))

          (draw-img-deluxe (typeset-word-img tsword)
                           (+ -1 x0 (typeset-word-x tsword))
                           (+  1 y0 (typeset-word-y tsword))
                           (vector 0 0 0 alpha))
          (draw-img-deluxe (typeset-word-img tsword)
                           (+ x0 (typeset-word-x tsword))
                           (+ y0 (typeset-word-y tsword))
                           (color-with-alpha color alpha)))))
