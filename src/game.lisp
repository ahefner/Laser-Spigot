(in-package :g1)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0)))

(defvar *turnid* (gensym))

(defparameter *msg* (make-string-output-stream))
(defparameter *typeset-messages* nil)
(defparameter *message-buffer* nil)
(defvar *displayed-message-buffer* nil)

(defclass game (gadget)
  ())

(defclass named ()
  ((name :accessor name-of :initarg :name :initform nil)))

(declaim (inline vref set-vref))
(defun vref (matrix vector) (aref matrix (v2.y vector) (v2.x vector)))
(defun set-vref (matrix vector value) (setf (aref matrix (v2.y vector) (v2.x vector)) value))
(defsetf vref set-vref)

(defparameter *tile-colors*
  #(#( 66 182 118 255)
    #( 66 182  74 255)
    #(101 182  66 255)
    #(130 182  66 255)
    #(152 182  66 255)
    #(174 182  66 255)
    #(182 149  66 255)
    #(182 127  66 255)))

(defclass level ()
  ((terrain    :initarg :terrain)
   (affinity   :initarg :affinity)
   (item-map   :initarg :item-map)
   (ent-map    :initarg :ent-map)
   (charge     :initarg :charge)
   (function-map :initarg :function-map)
   (level      :initarg :level :initform 0 :accessor level-level)
   (depth      :initarg :depth :initform 2)))

(defun level-from-terrain (terrain)
  (make-instance 'level
                 :terrain terrain
                 :affinity (map-matrix terrain (constantly 0))
                 :ent-map  (map-matrix terrain (constantly nil))
                 :item-map (map-matrix terrain (constantly nil))
                 :charge   (map-matrix terrain (constantly 4))
                 :function-map (map-matrix terrain (constantly nil))))

(defclass player (named)
  ((energy :accessor energy-of :initform 500 :initarg :energy)
   (has-egg :accessor has-egg? :initform nil))
  (:default-initargs :name "Player"))

(defmethod draw-ent ((ent player) x y)
  (declare (ignore ent))
  (draw-shadowed (charsprite :gothic 56 #\@) x (+ y 10)))

(defvar *player* nil)

(defclass death-egg (named) ()
  (:default-initargs :name "apocalyptic egg"))

;;; Bot component format: (name size hitpoints &key (critical nil))
(defun component-name (c) (first c))
(defun component-size (c) (second c))
(defun component-hitpoints (c) (third c))
(defun component-properties (c) (cdddr c))
(defun component-critical? (c) (eql t (getf (component-properties c) :critical)))
(defun set-third (list value) (setf (third list) value))
(defsetf component-hitpoints set-third)

(defun copy-list-of-lists (list) (map 'list #'copy-list list))
(defun find-component (name components)
  (when (typep components 'bot) (setf components (bot-components components)))
  (let ((index (position name components :test #'equal :key #'component-name)))
    (and index (values (elt components index) index))))

(defclass bot (named)
  ((char :reader bot-char :initarg :char :initform #\b)
   (heading :accessor bot-heading :initform (random 6))
   (winding :accessor bot-winding :initform 1)
   (pissed :accessor bot-pissed? :initform nil)
   (components :accessor bot-components :initarg :components)))

(defmethod draw-ent ((ent bot) x y)
  (draw-shadowed (charsprite :gothic 38 (bot-char ent)) x (+ y 10)))

(defgeneric repair-bot (bot))
(defmethod repair-bot ((a bot))
  (setf (bot-components a) (bot-components (make-instance (class-of a)))))

(defclass wired () ())
(defclass unwired () ())
(defclass angerable () ())

(defclass patrolbot (bot wired) ()
  (:default-initargs :name "Patrolbot" :char #\p))

(defun robot-components (&rest args) (copy-list-of-lists args))

(defclass custodian (bot wired) ()
  (:default-initargs 
   :name "CustodialBot" :char (code-char #xA9)
   :components (robot-components
                '("sensory apparatus"  3  2)
                '("motivator"         10  4)
                '("chassis"           20  5 :critical t)
                '("manipulator"        7  3)
                '("CPU"                1  1 :critical t))))

(defclass seeker (bot unwired) ()
  (:default-initargs
   :name "Seeker" :char (code-char #x436)
   :components (robot-components
                '("scanner"   3 2 :critical t)
                '("thruster"  3 2 :critical t)
                '("CPU"       1 1 :critical t)
                '("fuel cell" 3 4 :critical t))))

(defclass repairbot (bot unwired angerable) ()
  (:default-initargs
   :name "RepairBot" :char #xAE
   :components (robot-components
                '("vision matrix"        3  1)
                '("anterior appendages"  8  4)
                '("exoskeleton"         15  5 :critical t)
                '("CPU"                  1  1 :critical t)
                '("plasma welder"        3  3)
                '("microsurgery extension" 2 1))))

(defclass friendbot (bot unwired) ()
  (:default-initargs
   :name "FriendBot" :char (code-char #x414)
   :components (robot-components
                '("infrared sensor" 1 2)
                '("emotion engine"  1 1 :critical t)
                '("mobilitator"     3 4)
                '("exhaust port"    2 2 :critical t))))

(defun randvec (maxvec) (v2 (random (v2.x maxvec)) (random (v2.y maxvec))))
(defun dimvec (matrix) (v2 (array-dimension matrix 1) (array-dimension matrix 0)))
(defun level-size (level) (dimvec (slot-value level 'terrain)))

(defgeneric small? (object)
  (:method (object) (declare (ignore object)) t)
  (:method ((a bot)) (declare (ignore a)) nil))

(defgeneric passable? (ent level position))
(defgeneric terrain-passable? (ent terrain))

(defmethod terrain-passable? (ent terrain)
  (declare (ignore ent))
  (not (zerop terrain)))

(defmethod terrain-passable? ((ent wired) terrain)
  (declare (ignore ent))
  (eql terrain 2))

(defmethod terrain-passable? ((ent unwired) terrain)
  (declare (ignore ent))
  (not (zerop terrain)))

(defmethod terrain-passable? ((ent player) terrain)
  (declare (ignore ent))
  (not (zerop terrain)))

(defgeneric polarization-passable? (ent polarization)
  (:method ((ent player) x) (declare (ignore ent x)) t)
  (:method ((ent bot) polarization) (declare (ignore ent)) (zerop polarization)))

(defun conservatively-passable? (level position) (passable? nil level position))

(defmethod passable? (ent level position)
  (with-slots (terrain affinity ent-map) level
    (and (terrain-passable? ent (vref terrain position))
         (polarization-passable? ent (vref affinity position))
         (not (vref ent-map position)))))

(defvar *level* nil)

(defun distance-from-edge (terrain pos)
  (min (v2.x pos) (- (array-dimension terrain 1) (v2.x pos))
       (v2.y pos) (- (array-dimension terrain 0) (v2.y pos))))

(defun extend-hallway (terrain start direction &key (max-length 100) (winding 1) last-dist (straightaway 0) (forks 3))
  (unless (<= max-length 0)
    (incf (vref terrain start) max-length)
    (let* ((distance (distance-from-edge terrain start))
           (approaching (<= distance (or last-dist distance))))
      ;;(printl max-length :distance distance :position start :approaching approaching :straightaway straightaway)
      (cond
        ;; Too close to edge? Abort.
        ((<= distance 2) (return-from extend-hallway))
        ((and approaching (<= distance 5) (zerop (random 2)))
         (setf direction (mod (+ direction winding) 6)
               straightaway 0))
        ((and (> straightaway 6) (<= 0 (- 30 (random straightaway))))
         ;;(printl :turning)
         (setf direction (mod (+ direction (* winding #+NIL (1+ (random 2)))) 6)
               straightaway 0)
         (when (zerop (random 3))
           ;;(printl :reversing-winding)
          (setf winding (* winding -1)))))
      (when (and (not (zerop forks)) (zerop (random 20)))
        (extend-hallway terrain start (mod (+ direction (* (if (zerop (random 2)) 1 -1) (1+ (random 2)))) 6)
                      :max-length (1- max-length)
                      :forks (1- forks)
                      :winding winding
                      :last-dist distance
                      :straightaway (1+ straightaway)))
      (extend-hallway terrain (+dir start direction) direction
                      :max-length (1- max-length)
                      :forks forks
                      :winding winding
                      :last-dist distance
                      :straightaway (1+ straightaway)))))

(defun count-neighbors* (terrain row col)
  (loop for dir below 6 summing (if (zerop (vref terrain (+dir (v2 col row) dir))) 0 1)))

(defun widen-passages (terrain)
  (let ((nrows (array-dimension terrain 0))
        (ncols (array-dimension terrain 1))
        (to-widen nil))
    (do-matrix (terrain (tile row col))
      (when (and (not (zerop tile))
                 (> row 2) (< row (- nrows 3))
                 (> col 2) (< col (- ncols 3))
                 ;;(zerop (random 2))
                 (< 0 (count-neighbors* terrain row col) 4))
        (push (cons (v2 col row) tile) to-widen)))
    (dolist (pos.n to-widen) (setf (vref terrain (+dir (car pos.n) (random 6))) (+ (cdr pos.n) (random 3))))))

(defun gen-random-terrain (rows cols)
  (let ((terrain (make-array (list rows cols) :initial-element 0))
        (start (v2 (+ 4 (random (- cols 8))) (+ 4 (random (- rows 8))))))
    (loop with dir = (random 6) repeat 2 for foo upfrom 0 by 3 
          do (extend-hallway terrain start (mod (+ dir foo) 6)))
    (widen-passages terrain)
    (widen-passages terrain)
    ;; Collapse tree depths to 1/2 terrain types
    (setf terrain (map-matrix terrain (lambda (x) (if (zerop x) 0 (if (zerop (logand x 8)) 2 1)))))    
    (values terrain start)))

(defun collect-wired-coordinates (level)
  (with-slots (terrain ent-map function-map) level
    (let ((coordinates nil))
      (do-matrix (terrain (tile row col))
        (when (and (eql tile 2)
                   (null (aref ent-map row col))
                   (null (aref function-map row col)))
          (push (v2 col row) coordinates)))
        coordinates)))

(defun choose-stair-location (level player-start)
  (first (sort (always (collect-wired-coordinates level)) #'>
               :key (lambda (c) (+ (mod (sxhash c) 31) (v2len (v2- c player-start)))))))

(defstruct stairs-up to)
(defstruct stairs-down to)

(defun populate-bots (level)
  (with-slots (ent-map) level
    (let* ((coords (shuffle (collect-wired-coordinates level)))
           (lvl (level-level level))
           (depth (slot-value level 'depth))
           (area (length coords))
           (num-custodians (+ 2 (floor depth 20) (random (max 1 (floor area 38)))))
           (num-repairbots (floor area 180))
           (num-friendbots
            (cond
              ((< lvl 2) 0)
              ((eql lvl 2) 2)
              ((eql lvl 3) 4)
              ((eql lvl 4) 6)
              ((eql lvl 5) 9)
              (t 12)))
           (num-seekers (cond
                          ((eql lvl 0) 0)
                          ((eql lvl 1) 2)
                          (t (+ 1 (if (>= lvl 5) (floor area 150) 0)
                                (random (max 1 (floor area 80))))))))
      (printl :populate-bots :area area :custodians num-custodians :repair num-repairbots)
      (flet ((add (num class)
               (loop while coords repeat num do
                     (setf (vref ent-map (pop coords)) (make-instance class)))))
        (add num-custodians 'custodian)
        (add num-repairbots 'repairbot)
        (add num-friendbots 'friendbot)
        (add num-seekers 'seeker)))))

(defparameter *last-level* 8)

(defun create-random-level (&key previous-level
                            (level 0)
                            (depth 2)
                            (size 100)
                            (player (make-instance 'player) player-p))
  (unless player-p (setf *player* player))
  (multiple-value-bind (terrain player-start) (gen-random-terrain size size)
    (setf *level* (level-from-terrain terrain)
          (level-level *level*) level)
    (setf (slot-value *level* 'depth) depth)
    (with-slots (terrain ent-map item-map function-map) *level*
      (setf (vref ent-map player-start) player)
      (when t ;;previous-level
        (setf (vref function-map player-start) (make-stairs-up :to previous-level)))
      (cond
        ((eql level *last-level*)
         (setf (vref item-map (choose-stair-location *level* player-start))
               (list (make-instance 'death-egg))))
        (t
         (setf (vref function-map (choose-stair-location *level* player-start))
               (make-stairs-down :to nil))))))
  (populate-bots *level*)
  (values *level*))

(defun start-new-game ()
  (clear-messages)
  (create-random-level :size 60)
  (play-sound :beamin))

(defun primary-color () (aref *tile-colors* 0))

(with-vars ((cache (make-hash-table :test 'equal)))  
  (defun charsprite (face size character)
    (orf (gethash (list face size character) cache)
         (render-glyph *global-owner* face size character
                       :align-x :center :align-y :bottom))))
(defvar *last-work-done* 0)
(defun costmap (level start &key 
                (passablefn #'conservatively-passable?)
                (max-cost 60)
                (initial-cost 0)
                (costfn (constantly 1)))
  (with-slots (terrain) level
    (let ((worklist (make-minheap))
          (work-done 1)
          (map (map-matrix terrain (constantly nil))))
      (minheap-insert worklist (cons initial-cost start))
      (setf (vref map start) initial-cost)
      (loop until (minheap-empty-p worklist) do
       (let* ((here (cdr (minheap-remove worklist)))
              (cost (vref map here)))
         (assert cost)
         (unless (>= cost max-cost)
           (loop for direction below 6
                 as neighbor = (+dir here direction)
                 as neighbor-cost? = (vref map neighbor)
                 as passable = (funcall passablefn level neighbor)
                 as newcost = (and passable (+ cost (funcall costfn level neighbor)))
                 when (and passable (or (null neighbor-cost?) (< newcost neighbor-cost?))) do
                 (incf work-done)                 
                 (setf (vref map neighbor) newcost)
                 (minheap-insert worklist (cons newcost neighbor))))))
      ;; Finally, return the cost map:
      (unless (eql work-done *last-work-done*)
        (setf *last-work-done* work-done)
        #+NIL (printl :work-done work-done))
      map)))

(defgeneric ent-affinity-cost (ent affinity)
  (:method (ent affinity)
    (declare (ignore ent))
    (1+ affinity)))

(defun ent-costmap (ent level start &key (max-cost 60))
  (costmap level start
           :max-cost max-cost
           :passablefn (lambda (level position) (passable? ent level position))
           :costfn (lambda (level pos) (ent-affinity-cost ent (affinity-at level pos)))))

(defun clampelt (seq index)
  (elt seq (clamp index 0 (1- (length seq)))))

(defvar *camera* nil)
(defvar *last-camera-target* nil)
(defvar *camera-update-time* nil)

(defun pos*->xy (col row) (values (* col 63) (+ (* row 52) (* -26 (logand col 1)))))
(defun pos->xy (pos) (pos*->xy (v2.x pos) (v2.y pos)))
(defun pos->v (pos)
  (multiple-value-bind (x y) (pos*->xy (v2.x pos) (v2.y pos)) (v2 x y)))
(defun camera-target () (pos->v (find-player)))

(defstruct floater x y img color lifetime start-time)
(defvar *floaters* nil)

(defun spawn-floater (x y img color &key (lifetime 500000))
  (push (make-floater :x x :y y :img img :color color :lifetime lifetime) *floaters*))

(defun draw-floaters (uic xoff yoff)
  (setf *floaters*
        (mapcan (lambda (fl)
                  ;;(printl fl)
                  (unless (floater-start-time fl)
                    (setf (floater-start-time fl) (uic-time uic)))
                  (let ((d (- (uic-time uic) (floater-start-time fl))))
                    ;;(printl :y (round (* d 0.0001)))
                    (draw-shadowed (floater-img fl) 
                                   (- (- (floater-x fl) xoff)
                                      (ash (img-width (floater-img fl)) -1))
                                   (- (floater-y fl) yoff (round (* d 0.00006)))
                                   :color (floater-color fl))
                    (if (>= d (floater-lifetime fl))
                        nil
                        (list fl))))
                *floaters*)))

(defparameter *debug-costmap* nil)
(defun draw-map (xoff yoff map uic)
  (with-slots (terrain affinity item-map ent-map function-map) map
    (let* ((images (vector nil (img :hex1) (img :hex2)))
           (width (uic-width uic))
           (height (uic-height uic))
           (rows (array-dimension terrain 0))
           (cols (array-dimension terrain 1))
           ;;(ppos (find-player))
           ;;(prow (v2.y ppos)) ;;(pcol (v2.x ppos))
           #+NIL (hack-map (ent-costmap nil *level* (find-player))))
      (loop named draw-rows
            for row from (max 0 (- (floor yoff 52) 1)) below rows
            do
       (loop for col from (max 0 (floor xoff 63)) below cols
             as tile = (aref terrain row col)
             as x = (+ (- xoff) (* col 63))
             as y = (+ (- yoff) (* row 52) (* -26 (logand col 1)))
             do
        (when (> y (+ 100 height)) (return-from draw-rows))
        (when (> x (+ 100 width)) (loop-finish))
        (unless (or (< (min x y) -100))
         (let ((ent (aref ent-map row col))
               (items (aref item-map row col))
               (function (aref function-map row col))
               (color (clampelt *tile-colors* (aref affinity row col))))
            ;; Hex Tile
            (unless (zerop tile)        ;XXX
              (draw-img-deluxe (aref images tile) x y color))
            ;; Special Function Markers
            (when function
              (draw-img-deluxe (img (etypecase function
                                      (stairs-up :upsquare)
                                      (stairs-down :downsquare)))
                               x y color))
            ;; Items
            (when items (draw-img-deluxe (img "item-marker.png") x y color))
            (dolist (item items)
              (when (typep item 'death-egg)
                (draw-shadowed (charsprite :gothic 26 #xE6)
                               x y)))
            (when ent (draw-ent ent x y))
            ;;(draw-img (global-label :sans 11 (princ-to-string (count-neighbors* terrain row col))) x y)
            #+NIL
            (when *debug-costmap*
              (draw-img (global-label :sans 11 (princ-to-string (aref *debug-costmap* row col))) x y))
            #+NIL (draw-img (global-label :sans 11 (princ-to-string (aref hack-map row col))) x y)))))
      (draw-floaters uic xoff yoff))))

(defun draw-shadowed (img x y &key (color #(255 255 255 255)))
  (draw-img-deluxe img (1- x) (1+ y) #(0 0 0 255))
  (draw-img-deluxe img x y color))

 
(with-vars ((labels (make-hash-table :test 'equal)))
  (defun draw-status-bar (uic player)
    (let ((cursor (make-cursor :left 8 :y (- (uic-height uic) 10))))
      (flet ((draw (img) (cursor-draw-img cursor img :shadow t))
             (label (text) (global-label :sans 17 text))
             (attrib (key text)
               (cachef ((gethash key labels) text :delete free-img)
                 (render-label :status-bar :sans 17 text))))
        (draw (label "e="))
        (draw (attrib :energy (format nil "~:D" (energy-of player))))
        (draw (label (format nil " kg~Cm~C/s~C" (code-char 183) (code-char 178) (code-char 178))))
        (cursor-tab cursor 90)
        (draw (attrib :depth (format nil "d=~:D m" (slot-value *level* 'depth))))))))

(defparameter *wordmap* (make-hash-table :test 'equal))

(defvar *text-transition-base* nil)

(defmethod gadget-run ((gadget game) uic)
  (declare (ignore gadget))
  (let ((tex (filtered-texture "falloff.jpg")))
    (bind-texobj tex)
    (set-blend-mode :replace-alpha)
    (draw-tile-scaled 0 0 (uic-width uic) (uic-height uic)
                      0 0 (texture-width tex) (texture-height tex)
                      (color-half-darken (primary-color))))
  (set-blend-mode :blend)
  (cond
    ((null *camera*) (setf *camera* (camera-target)))
    (t (let ((dv (v2- (camera-target) *camera*)))         
         (unless (< (v2normsq dv) 800)
           ;;(printl :dv dv (v2normsq dv))
           (setf *camera* (v2+ *camera* (v2scale dv (expt (uic-delta-t uic) 0.75))))))))
  (draw-map (- (v2.x *camera*) (ash (uic-width uic) -1))
            (- (v2.y *camera*) (ash (uic-height uic) -1))
            *level* uic)
  (set-blend-mode :blend)
  (draw-status-bar uic *player*)
  ;;(draw-shadowed (global-label :gothic 30 "Hello, world!") 10 28 :color (color-lighten (primary-color)))
  (let ((message-width 400))
    (unless (eql *message-buffer* *displayed-message-buffer*)
      (setf *displayed-message-buffer* *message-buffer*)
      (setf *typeset-messages* (typeset-text *wordmap* (- message-width 20) *message-buffer* :line-height 12)))
    (orf *text-transition-base* (uic-time uic))
    (fancy-draw-typeset-text *typeset-messages* (- (uic-width uic) message-width) 14 #(255 255 255 255)
                             (or (ignore-errors (round (expt (* 0.0008 (- (uic-time uic) *text-transition-base*)) 1.1)))
                                 10000000))))

(defvar *find-ent-last-level* nil)
(defvar *find-ent-cache* (make-hash-table :test 'eq))
(defparameter *find-ent-stats* (vector 0 0 0))

(defun find-ent (ent)
  (unless (eq *find-ent-last-level* *level*)
    (setf *find-ent-last-level* *level*)
    (clrhash *find-ent-cache*))
  (let ((cached (gethash ent *find-ent-cache*))
        (ent-map (slot-value *level* 'ent-map)))
    ;; Cache hit.
    (when (and cached (eq ent (vref ent-map cached))) 
      (incf (aref *find-ent-stats* 0))
      (return-from find-ent (values cached ent)))
    ;; Cache disagrees with level. Search neighbors for ent and update if found.
    (when cached
      (remhash ent *find-ent-cache*)
      (loop for dir below 6 as pos = (+dir cached dir)
            when (eq ent (vref ent-map pos)) do
            (setf (gethash ent *find-ent-cache*) pos)
            (incf (aref *find-ent-stats* 1))
            (return-from find-ent (values pos ent))))
    ;; Not found in cache, or object has moved too far. Search and update.
    (let ((pos (%find-ent ent)))
      (incf (aref *find-ent-stats* 2))
      (when pos
        (setf (gethash ent *find-ent-cache*) pos)
        (values pos ent)))))

(defun %find-ent (ent)
  (let ((ent-map (slot-value *level* 'ent-map)))
    (do-matrix (ent-map (ent-here row col))
      (when (eql ent ent-here) (return-from %find-ent (values (v2 col row) ent))))))

(defun find-player ()
  (multiple-value-bind (pos player) (find-ent *player*)
    (if pos
        (values pos player)
        (error "Can't find player!"))))

(defun dir-to-vector (dir pos)
  (v2 (aref #(-1 0 1 1 0 -1) dir)
      (- (aref #(0 -1 0 1 1 1) dir)
         (* (aref #(1 0 1 1 0 1) dir) (mod (v2.x pos) 2)))))

(defun +dir (position dir) (v2+ position (dir-to-vector dir position)))

(defgeneric describe-item (item)
  (:method (item) (name-of item))
  (:method ((a bot)) (format nil "broken ~A" (name-of a))))

(defun describe-location ()
  (let ((items (items-at *level* (always (find-player)))))
    (cond
      ((null items))
      ((null (rest items)) (format *msg* "~&You see ~A ~A here.~%" 
                                   (article-for (first items)) (describe-item (first items))))
      (t (format *msg* "~&You see here:~%")
         (loop for item in items for n from 1 upto 10 do
               (format *msg* "~&(~D) ~A ~A~%" (mod n 10) (article-for item) (describe-item item)))))))

(defun clear-messages ()
  (setf *message-buffer* (get-output-stream-string *msg*)
        *msg* (make-string-output-stream)
        *text-transition-base* nil))

(defun turn-update ()
  (with-slots (terrain affinity) *level*
    (setf affinity (map-matrix affinity (lambda (x) (max 0 (1- x))))))
  (run-ents)
  (describe-location)
  (maybe-kill-player)
  (clear-messages)
  (setf *turnid* (gensym)))

(defun terrain-at  (level position) (vref (slot-value level 'terrain)  position))
(defun affinity-at (level position) (vref (slot-value level 'affinity) position))
(defun items-at    (level position) (vref (slot-value level 'item-map) position))
(defun entity-at   (level position) (vref (slot-value level 'ent-map)  position))
(defun set-entity-at (level position entity)
  (with-slots (ent-map) level
    (if entity 
        (assert (not (vref ent-map position)))
        (assert (vref ent-map position)))
    (setf (vref ent-map position) entity)))
(defsetf entity-at set-entity-at)

(defvar *wired-costmap-cache* nil)
(defvar *unwired-costmap-cache* nil)

(defgeneric ent-sim (ent level position))

(defun maybe-kill-player ()
  (when (<= (energy-of *player*) 0)
    (run-death-screen)))

(defmethod ent-sim ((player player) level position)
  (decf (energy-of player) 1)
  ;; Check for egg.
  (with-slots (item-map) level
    (let ((egg (find 'death-egg (vref item-map position) :key 'type-of)))
      (when egg
        (deletef (vref item-map position) egg)
        (setf (has-egg? player) t)
        (play-sound :egg)
        (format *msg* "~&You've found the Apocalyptic Egg! Hurry back to the surface, so it can be defused!~%"))))
  ;; Fatigue wired tiles
  (when (eql 2 (terrain-at level position))    
    (with-slots (terrain charge) level
      (decf (vref charge position))
      (when (>= 0 (vref charge position))
        (play-sound :close1)
        (setf (vref terrain position) 1))
      (incf (energy-of player) 3)))
  ;; Update energy; maybe kill off player.
  (setf (energy-of player) (clamp (energy-of player) 0 600)))

(defgeneric ent-sim-movement (ent level position)
  (:method (a b c)
    (declare (ignore a b c))))

(defmethod ent-sim ((a bot) level position)
  (ent-sim-movement a level position))

(defun maybe-move-ent (ent level position direction)
  (cond
    ((passable? ent level (+dir position direction))
     (move-ent ent level position direction)
     t)
    (t nil)))

(defun try-movement (ent level position directions)
  "Try moving in all of the given direction, returning the first one which is successful."
  (loop for direction in directions
        when (maybe-move-ent ent level position direction) 
        do (return-from try-movement direction))
  nil)

(defun wander-randomly (ent level position)
  "Move bot randomly, returning chosen direction, or NIL if trapped."
  (try-movement ent level position (shuffle (list 0 1 2 3 4 5))))

(defun wander-bouncy (bot level position)
  (or (maybe-move-ent bot level position (bot-heading bot)) 
      (setf (bot-heading bot) (or (wander-randomly bot level position)
                                  (random 6)))))

(defun wander-patrol (bot level position)
  (loop with considered-change = nil repeat 6 do
        (cond
          ((maybe-move-ent bot level position (bot-heading bot))
           (loop-finish))
          (t 
           (unless considered-change
             (setf considered-change t)
             (when (zerop (random 3)) (setf (bot-winding bot) (* -1 (bot-winding bot)))))
           (setf (bot-heading bot) (mod (+ (bot-winding bot) (bot-heading bot)) 6))))))

;;; Performance problems lead to this..
(defun compute-unwired-costmap ()
  (with-slots (terrain ent-map affinity) *level*
    (let ((terrain terrain) (ent-map ent-map) (affinity affinity))      
      (costmap *level* (find-player)
               :passablefn
               (lambda (level position)
                 (declare (ignore level))
                 (let ((row (v2.y position))
                       (col (v2.x position)))
                   (and (not (zerop (aref terrain row col)))
                        (zerop (aref affinity row col))
                        (null (aref ent-map row col)))))
               :costfn
               (lambda (level pos)
                 (declare (ignore level))
                 (1+ (vref affinity pos)))
               :max-cost 20))))

(defun find-moves-toward-player (bot level position &key (max-cost 60))
  (let* ((costmap 
          (cond
            ((typep bot 'wired)
             (cachef (*wired-costmap-cache* *turnid*)
              (ent-costmap bot level (find-player) :max-cost max-cost)))
            (t
             (cachef (*unwired-costmap-cache* *turnid*)
              (compute-unwired-costmap)))))
         (costs (loop for dir below 6 collect (vref costmap (+dir position dir))))
         (costs* (remove nil costs))
         (min (and costs* (reduce #'min costs*)))
         (min-directions (loop for dir below 6 for cost in costs when (eql cost min) collect dir)))
    (setf *debug-costmap* costmap)
    ;;(printl :costs costs :min min :min-directions min-directions)
    (and min min-directions)))

(defun move-toward-player (bot level position &key (max-cost 60))
  (try-movement bot level position 
                (find-moves-toward-player bot level position :max-cost max-cost)))

(defun adjacent? (this that)
  (loop with pos = (always (find-ent this))
        for dir below 6 
        when (eql that (entity-at *level* (+dir pos dir)))
        do (return (values dir that))))

(defun move-ent (ent level position direction)
  (assert (passable? ent level (+dir position direction)))
  ;;(printl :moving ent :from position :to (+dir position direction))
  (with-slots (ent-map) level
    (rotatef (vref ent-map position) (vref ent-map (+dir position direction)))))

(defmethod ent-sim-movement ((a custodian) level position)
  ;; Robot behavior depends on what robot systems are still functional.
  (cond
    ((not (find-component "motivator" a)) (values))
    ((find-component "sensory apparatus" a)
     (or (move-toward-player a level position :max-cost 50)
         (and (adjacent? a *player*)
              (prog1 t 
                (when (zerop (random 4))
                  (format *msg* (random-elt '("~&~A stares at you.~%"
                                              "~&~A sits motionless before you.~%"
                                              "~&~A appears to study you intently.~%"
                                              "~&~A whirs and beeps.~%"))
                          (as-subject a)))))
         (wander-patrol a level position)))
    (t (wander-randomly a level position))))

(defmethod compute-attack-damage ((a repairbot) target)
  (declare (ignore target))
  (+ (if (find-component "plasma welder" a) (random 10) 0)
     (if (find-component "microsurgery extension" a) (random 5) 0)))

(defgeneric repairable? (object)
  (:method (object) (declare (ignore object)) nil)
  (:method ((object bot)) (declare (ignore object)) t))

(defun attempt-repair (repairer broken pos)
  (case (random 3)
    ((0 1) (format *msg* "~&~A pokes and prods the ~A.~%" 
                   (as-subject repairer) (describe-item broken)))
    (2 (format *msg* "~&With the help of ~A, the ~A springs to life!~%"
               (as-object repairer) (describe-item broken))
       (repair-bot broken)
       (play-sound :repair)
       (with-slots (item-map) *level*
         (deletef (vref item-map pos) broken)
         (setf (entity-at *level* pos) broken)))))

(defmethod ent-sim-movement ((a repairbot) level position)
  (let ((repair-ability (or (find-component "plasma welder" a)
                            (find-component "microsurgery extension" a)))
        (repairable
         (first
          (loop for dir below 6 as np = (+dir position dir)
                as found = (find-if #'repairable? (items-at level np))
                when (and found (not (entity-at *level* np))) collect (list found np)))))
    (cond
      ;; Damaged?
      ((and repair-ability 
            (not (equalp (bot-components a)
                         (bot-components (make-instance 'repairbot))))
            (zerop (random 5)))
       (format *msg* "~&~A repairs itself.~%" (as-subject a))
       (repair-bot a))
      ;; Attack player?
      ((and (bot-pissed? a) (adjacent? a *player*) repair-ability)
       (attack a *player*))
      ;; Repair terrain?
      ((and repair-ability (eql 1 (terrain-at level position)) (zerop (random 14)))
       (setf (vref (slot-value level 'terrain) position)  2
             (vref (slot-value level 'charge)  position) 10)
       (play-sound :open1))
      ;; Immobilized?
      ((not (find-component "anterior appendages" a)) (values))
      ;; Blinded?
      ((not (find-component "vision matrix" a)) (wander-randomly a level position))
      ;; Repairable bot nearby?
      ((and repair-ability repairable) 
       (attempt-repair a (first repairable) (second repairable)))
      ;; Angry?
      ((and (bot-pissed? a) (move-toward-player a level position)))
      ;; Look for work.
      (t (wander-bouncy a level position)))))

(defmethod ent-sim-movement ((a friendbot) level position)
  (cond
    ;; Player nearby?
    ((and (adjacent? a *player*) (find-component "infrared sensor" a))
     (when (zerop (random 3))
       (format *msg* (random-elt '("~&~A wants to be your friend.~%"
                                   "~&~A is glad to see you.~%"
                                   "~&~A wants to tag along.~%"))
               (as-subject a))))
    ;; Immobilized?
    ((not (find-component "mobilitator" a)) (values))
    ;; Seek player?
    ((and (find-component "infrared sensor" a)
          (move-toward-player a level position))
     (values))
    ;; Otherwise, wander around.
    (t (wander-patrol a level position))))

(defclass explosion! (named) ()
  (:default-initargs :name "explosion"))

(defmethod ent-sim-movement ((a seeker) level position)
  (cond
    ((and (adjacent? a *player*) (zerop (random 2))) 
     (format *msg* "~&~A explodes!~%" (as-subject a))
     (play-sound :explode)
     (loop for dir below 6 as np = (+dir position dir) as neighbor = (entity-at level np)
           when (typep neighbor 'bot) do (deal-damage (make-instance 'explosion!) neighbor (+ 2 (random 20))))
     (hurt-player 25)
     (setf (entity-at level position) nil))
    ((not (zerop (random 4)))
     (or (move-toward-player a level position)
         (wander-bouncy a level position)))))

(defun run-ents ()
  (let ((ent-map (slot-value *level* 'ent-map))
        (touched-ents (make-hash-table)))
    (do-matrix (ent-map (ent row col))
      ;; Beware! 'ent' is a symbol macro accessing the array position.
      (let ((ent ent))
        (when (and ent (not (gethash ent touched-ents)))
          (ent-sim ent *level* (v2 col row))
          (setf (gethash ent touched-ents) t))))))

(defgeneric attackable? (ent)
  (:method (ent) (declare (ignore ent)) nil)
  (:method ((ent bot)) (declare (ignore ent)) t))

(defgeneric attack (attacker target))

(defgeneric compute-attack-damage (attacker target)
  (:method (attacker target) (declare (ignore attacker target)) (max 0 (+ -1 (random 6)))))

(defun weighted-random-choice (items weightfn)
  (let ((total-weights (reduce #'+ items :key weightfn)))
    (when (> total-weights 0)
      (loop with nchoice = (random total-weights)            
            with sofar = 0
            for index upfrom 0
            for item in items do
            (incf sofar (funcall weightfn item))
            (when (> sofar nchoice) (return (values item index)))
            finally (error "Bug in weighted random choice!")))))

(defgeneric as-subject (ent)
  (:method ((ent named)) (format nil "The ~A" (name-of ent)))
  (:method ((ent player)) (declare (ignore ent)) "You")
  (:method (ent) (declare (ignore ent)) "It"))

(defgeneric as-object (ent)
  (:method ((ent named)) (format nil "the ~A" (name-of ent)))
  (:method ((ent player)) (declare (ignore ent)) "you")
  (:method (ent) (declare (ignore ent)) "it"))

(defgeneric as-possessive (ent)
  (:method ((ent named)) (format nil "the ~A's" (name-of ent)))
  (:method ((ent player)) (declare (ignore ent)) "your")
  (:method (ent) (declare (ignore ent)) "its"))

(defgeneric article-for (ent)
  (:method (ent) (declare (ignore ent)) "a"))

(defun pluralize-verb (verb) (format nil "~As" verb))

(defgeneric pluralize-for (ent verb)
  (:method (ent verb)
    (let ((printed (as-subject ent)))
      (cond
        ((or (eql 4 (mismatch "The " printed :test #'char-equal))
             (eql 2 (mismatch "A " printed :test #'char-equal)))
         (pluralize-verb verb))
        (t verb)))))

(defgeneric produce-corpse (ent)
  (:method (ent) (declare (ignore ent)) nil)
  (:method ((it bot)) it))

(defgeneric kill (ent)
  (:method (ent)
    (let ((corpse (produce-corpse ent))
          (pos (find-ent ent)))
      (printl :kill ent :at pos)
      (cond
        ((not pos) (warn "Trying to kill ~A, but can't find it!" ent))
        (t (assert (eql ent (entity-at *level* pos)))           
           (setf (vref (slot-value *level* 'ent-map) pos) nil)
           (when corpse (push corpse (vref (slot-value *level* 'item-map) pos))))))))

(defun bot-damage-component (bot damage component)
  "Deal damage to bot component. Returns T if bot is killed."
  (decf (component-hitpoints component) damage)
  (when (<= (component-hitpoints component) 0)
    (setf (bot-components bot) (delete component (bot-components bot)))
    (cond
      ((component-critical? component) (kill bot) t)
      (t nil))))

(defun always (value &optional (key #'identity)) (assert (funcall key value)) value)

(defgeneric deal-damage (attacker victim damage)
  (:method (attacker victim damage) 
    (declare (ignore damage)) 
   (format *msg* "~&~A is oblivious to ~A attack.~%" (as-subject victim) (as-possessive attacker)))
  (:method (attacker (you player) damage) 
    (declare (ignore you))
    (format *msg* "~&Your shield renders ~A attack futile.~%" (as-possessive attacker))
    (play-sound :zap)
    (hurt-player damage))
  (:method (attacker (bot bot) damage)
    (let* ((part (always (weighted-random-choice (bot-components bot) #'component-size)))
           (killed (bot-damage-component bot damage part)))
      ;;(printl :attack attacker bot :damage damage :killed killed :hit part :components (bot-components bot))
      (play-sound :hit1)
      (cond
        ((<= (component-hitpoints part) 0)
         (format *msg* "~&~A ~A the ~A, destroying its ~A." 
                 (as-subject attacker) (pluralize-for attacker "strike") (as-object bot) (component-name part))
         (when killed 
           (format *msg* " It collapses.")
           (play-sound :chirp))
         (terpri *msg*))
        ((<= damage 1) (format *msg* "~A ~A a glancing blow to the ~A ~A.~%"
                               (as-subject attacker) (pluralize-for attacker "deal")
                               (as-possessive bot) (component-name part)))
        ((<= damage 2) (format *msg* "~A ~A the ~A ~A.~%"
                               (as-subject attacker) (pluralize-for attacker "hit")
                               (as-possessive bot) (component-name part)))
        ((<= damage 4) (format *msg* "~A ~A ~A the ~A ~A.~%"
                               (as-subject attacker) 
                               (random-elt '("punishingly" "brutally" "crushingly" "critically" "deftly"))
                               (pluralize-for attacker "strike")
                               (as-possessive bot) (component-name part))))))
  (:method :after ((attacker player) (bot angerable) damage)
    (declare (ignore attacker damage))
    (when (zerop (random 2))
      (unless (bot-pissed? bot)
        (setf (bot-pissed? bot) t)
        (format *msg* "~&~A is pissed!~%" (as-subject bot))))))

(defmethod deal-damage :before (attacker victim damage)
  (declare (ignore attacker))
  (unless (or (zerop damage) (typep victim 'player))
    (let ((pos (find-ent victim)))
      (when pos
        (multiple-value-bind (x y) (pos->xy pos)
          (spawn-floater x (- y 10) (global-label :bold 14 (format nil "~:D" damage)) #(255 160 160 255)))))))

(defmethod attack (attacker target)
  (let ((damage (compute-attack-damage attacker target)))
    (cond
      ((zerop damage) 
       (format *msg* "~A missed." (as-subject attacker))
       (play-sound :miss))
      (t (deal-damage attacker target damage)))))

(defmethod attack :after ((attacker player) target)
  (declare (ignore target))
  (decf (energy-of attacker) 3))

(defparameter +max-affinity+ 8)

(defgeneric player-enters-function (fun pos)
  (:method ((fun null) pos) (declare (ignore fun pos))))

(defmethod player-enters-function ((stairs stairs-up) pos)
  (declare (ignore pos))
  (cond
    ;; This is very silly fun. Notice we don't set the player's
    ;; position when swapping levels... the player ent never left the
    ;; level! :)
    ((stairs-up-to stairs)
     (play-sound :transport-back)
     (setf *level* (stairs-up-to stairs)))
    (t (cond
         ((has-egg? *player*)
          (run-victory-screen))
         (t (format *msg* "~&The exit to the surface is here. To leave now is unthinkable, as failure will surely result in the destruction of your world.~%"))))))

(defmethod player-enters-function ((stairs stairs-down) pos)
  (declare (ignore pos))
  (unless (stairs-down-to stairs)
    (setf (stairs-down-to stairs) 
          (create-random-level :previous-level *level*
                               :player *player*
                               :level (1+ (level-level *level*))
                               :depth (+ 2 (random 4) (slot-value *level* 'depth)))))
  (play-sound :transport)
  (setf *level* (stairs-down-to stairs)))

(defun hurt-player (damage)
  (decf (energy-of *player*) damage)
  (multiple-value-bind (x y) (pos->xy (always (find-player))) 
    (spawn-floater x (- y 30)    
      (global-label :bold 14 (format nil "-~:D" damage))
      #(160 255 160 255)
      :lifetime 800000)))

(defvar *step* 0)

(defun play-step-sound ()
  (play-sound (aref (vector :step :step2) *step*))
  (setf *step* (logxor *step* 1)))

(defun move-player (direction)
  (multiple-value-bind (pos player) (find-player)
    (let ((newpos (+dir pos direction)))
      (with-slots (ent-map affinity function-map) *level*
        (let ((dest-ent (vref ent-map newpos)))
          (cond
            ((attackable? dest-ent) (attack player dest-ent))
            (dest-ent (format *msg* "~&You are blocked by a ~A.~%" (name-of dest-ent)))
            ((passable? player *level* newpos)
             (decf (energy-of player) 1)
             (play-step-sound)
             (setf (vref affinity newpos) +max-affinity+)
             (rotatef (vref ent-map pos) (vref ent-map newpos))
             (player-enters-function (vref function-map newpos) newpos))
            (t (format *msg* "~&You can't move there.~%")))))))
  (turn-update))

(defun char->direction (char)
  (case char 
    (#\u 0)
    (#\i 1)
    (#\o 2)
    (#\j 5)
    (#\k 4)
    (#\l 3)
    (otherwise nil)))

(defun spawn (pos ent) (setf (vref (slot-value *level* 'ent-map) pos) ent))

(defclass laser-beam (named) 
  ((dir :accessor laser-dir :initarg :dir))
  (:default-initargs :name "laser"))

(defun fire-laser (start direction)
  (let ((pos (+dir start direction)))
    (cond
      ((zerop (terrain-at *level* pos))
       (format *msg* "~&Your laser is blocked.~%"))
      ((entity-at *level* pos)
       (format *msg* "~&You may not fire your laser at point-blank range.~%"))
      (t (decf (energy-of *player*) 3)
         (play-sound :laser)
         (spawn pos (make-instance 'laser-beam :dir direction))))))

(defmethod draw-ent ((ent laser-beam) x y)
  (draw-img (ecase (laser-dir ent)
              ((0 3) (img :laser03))
              ((1 4) (img :laser14))
              ((2 5) (img :laser25)))
            x y))

(defmethod ent-sim ((laser laser-beam) level position)
  (let ((newpos (+dir position (laser-dir laser))))
    (with-slots (terrain ent-map) level
      (setf (vref ent-map position) nil)
      (cond
        ((zerop (vref terrain newpos)))
        ((vref ent-map newpos)
         (typecase (vref ent-map newpos)
           (bot (deal-damage laser (vref ent-map newpos) (+ 3 (random 10))))))
        (t (setf (vref ent-map newpos) laser))))))

(defmethod gadget-key-pressed ((gadget game) uic keysym char)
  (declare (ignore keysym gadget uic))
  (cond
    ((null char))
    ((equal char #\Esc) (exit-game))
    ((char->direction char) (move-player (char->direction char)))
    ((char->direction (char-downcase char))
     (turn-update)
     (fire-laser (find-player) (char->direction (char-downcase char))))
    ((equal char #\r) (start-new-game))
    ;; ((equal char #\x) (error "This is a pretend error for testing."))
    ((equal char #\.) 
     (play-sound :click-mid)
     (turn-update))))





;; (dolist (sym (list  'move-player 'turn-update 'ent-sim-movement 'find-moves-toward-player 'try-movement 'move-toward-player 'ent-sim 'maybe-move-ent 'find-player 'find-ent '%find-ent 'costmap)) (eval (list 'profile:profile sym)))

;;;; Victory message

(defclass info-screen (gadget)
  ((start-time :initform nil)
   (typeset :initform nil)
   (show-rate :initarg :show-rate)
   (fill-bg :initform nil :initarg :fill-bg)
   (allow-quit  :initform t :initarg :allow-quit)
   (allow-close :initform t :initarg :allow-close)
   (message :initarg :message)))

(defmethod gadget-key-pressed ((gadget info-screen) uic keysym char)
  (declare (ignore uic keysym))
  (with-slots (allow-quit allow-close) gadget
    (cond
      ((and allow-quit (equal char #\Esc)) (exit-game))
      ((and allow-close char (not (equal char #\Esc))) 
       (pop-gadget gadget)))))

(defparameter *win-wordmap* (make-hash-table :test 'equal))

(defmethod gadget-run ((gadget info-screen) uic)
  (call-next-method)
  (setf (gethash :style *win-wordmap*) :gothic
        (gethash :size  *win-wordmap*) 17)
  (with-slots (fill-bg show-rate start-time typeset message) gadget    
    (orf typeset
         (typeset-text *win-wordmap* 600 message :line-height 22))
    
    (orf start-time (uic-time uic))
    (when fill-bg
      (fill-rect (v2 0 0) (v2 (uic-width uic) (uic-height uic)) fill-bg))
    (fancy-draw-typeset-text 
     typeset
     (- (ash (uic-width uic) -1) 300)
     40
     #(255 255 255 255)
     (or (ignore-errors
           (round (expt (* show-rate (- (uic-time uic) start-time)) 1.1)))
         10000000))))

(defclass win-screen (info-screen) ()
  (:default-initargs
   :allow-quit t :allow-close nil
   :show-rate 0.0004 
   :message
"Victorious, you emerge from the wreckage of the alien mothership, now a smoldering crater in the desert. As your eyes adjust to the harsh midday sun, you begin the long hike toward the perimeter of the crash site, anticipating a hero's welcome.
Arriving at the perimeter checkpoint, you beam with pride as the Apocalyptic Egg is delivered to be defused. Entranced by visions of fame, glory, and an inevitable promotion, your daydream is cut short by a flash of light and a sudden stabbing pain in your back. The last image you see before you die is the figure of your commanding officer standing over you, pistol in hand. Your body is dumped in a dirt hole and forgotten. Your commanding officer takes credit for your victory, being immediately promoted to General and hailed as the savior of your entire planet.
Thanks for playing!


This has been a Funcall Games production. Special thanks to: Juan Jose Garcia-Ripoll (for his tireless work on ECL), Tobias C. Rittweiler (for his work on SLIME and ECL, which was instrumental to this project), and Elmer Hefner (for sound effect field recording).
Press ESCAPE to exit."))

(defun run-victory-screen ()
  (play-sound :beamout)
  (activate-new-gadget
        (make-instance 'fade-transition-gadget
                       :child (make-instance 'win-screen)
                       :rate 0.5
                       :color #(10 20 70))))

;;;; Death screen

(defclass death-page (info-screen) ()
  (:default-initargs
   :show-rate 0.0010
   :allow-quit t :allow-close t
   :message
"Carelessly dashing through the smashed corridors of the alien mothership, you pay no mind to your powered armor's energy gauge. Suddenly an alarm sounds, and you realize your mistake. The fuel supply is completely drained. Within seconds the shields fall, leaving you defenseless. The life support system shuts down. Unable to breathe the inert vapor of the alien habitat, you slowly asphyxiate. The last moments of your life pass as a custodian robot drags you away for waste disposal. Another robot disassembles your armor for spare parts.
Several hours later, the countdown begins. Unable to repair the ship, the robots have assessed the situation hopeless and activated the self-destruct mechanism. Deep in the heart of the alien mothership, the Apocalyptic Egg glows red. Its surface bubbles and dissolves. Finally, without warning, a ring of energy rips outward from the egg, cleaving the planet in two and blasting most of the surface into space.
Your failure has cost not only your own life, but that of your entire species. Congratulations.


Press ESCAPE to exit, or any other key to restart."))

(defun run-death-screen ()
  (play-sound :beamout)
  (activate-new-gadget
   (make-instance 'fade-transition-gadget
                  :child (make-instance 'death-page)
                  :rate 2.0
                  :color #(70 20 10))))

(defmethod finalize-object ((gadget death-page))
  (declare (ignore gadget))
  (start-new-game))

;;;; Briefing screen

(defclass briefing-page (info-screen) ()
  (:default-initargs
   :next-gadget (make-instance 'game)
   :fill-bg #(0 0 0 255)
   :show-rate 0.0004
   :allow-quit nil :allow-close t
   :message
"Mission briefing, 3/12/2077, 8:45 AM:
[TOP SECRET] An immense alien spacecraft has crashed nearby in the wilderness. It is known to carry a doomsday device known as the Apocalpyic Egg, capable of destroying an entire planet. We believe the aliens will invoke this device as a self-destruct mechanism. It is only a matter of hours. Your mission is to infiltrate the craft and capture this device so that it may be deactivated. Due to time constrants, only your small team of heavy troopers is available. You will wear the latest power armor, equipped with a shield generator capable of deflecting any physical attack, and life support systems to provide oxygen inside the alien environment. Retrieve the Egg at all costs.
Addendum, 10:20 AM:
The situation is dire. Dozens of defensive robots and the full complement of their crew fought to the death with superior weapons and armor. Our team was cut down like cattle. I'm the last man standing. No choice but to proceed alone. Energy reserves dangerously low. The mothership is smashed, power's out to half the grid. Good when it means the robots can't follow me; bad because my reserves are already low. My armor has a polarizing effect on the grid which briefly stops the robots from following me. This might be useful.
Press any key to continue."))

;;;;

(defvar *button-color* (vector 255 180 40 255))

(defclass pagepage (gadget)
  ((next-class :initarg :next-class)))

(defmethod gadget-key-pressed ((gadget pagepage) uic keysym char)
  (declare (ignore uic keysym))
  (when char
    (play-sound :click-high)
    (setf *gadget-root* (make-instance (slot-value gadget 'next-class)))))

;;;; Title screen

(defclass title-page (pagepage) ()
  (:default-initargs :next-class 'help-screen))

(defun show-pointer (uic)
  (draw-img (imgblock :pointer) (- (uic-mx uic) 5) (- (uic-my uic) 2)))

(defmethod gadget-run ((gadget title-page) uic)
  (declare (ignore gadget))
  (let* ((width (uic-width uic))
         (width/2 (ash width -1))
         (height (uic-height uic))
         (height/2 (ash height -1)))
    (img :help)                         ; Preload this.    
    (fill-rect* 0 0 width height 0 0 0 255)
    (draw-img (center-img (global-label :gothic 20 (format nil "A 7-Day Roguelike Game. With robots. And lasers." (code-char #xA9))))
              width/2 (+ 100 height/2))
    (draw-img (center-img (global-label :gothic 17 (format nil "~C 2010 Funcall Games" (code-char #xA9))))
              width/2 (+ 200 height/2))
    (draw-img (center-img (global-label :gothic 13 (format nil "Made with secret alien technology.")))
              width/2 (+ 215 height/2))
    (when (run-labelled-button uic (global-label :bold 14 "Next")
                               (- width 53) (- height 30)
                               :color *button-color*)
      (setf *gadget-root* (make-instance 'help-screen)))
    (show-pointer uic)
    (draw-img (img :logo3) width/2 height/2)))

;;;; Help screen

(defclass help-screen (pagepage) ()
  (:default-initargs :next-class 'briefing-page))

(defmethod gadget-run ((gadget help-screen) uic)
  (declare (ignore gadget))
  (let* ((width (uic-width uic))       
         (width/2 (ash width -1))
         (height (uic-height uic))
         (height/2 (ash height -1)))
    (fill-rect* 0 0 width height 0 0 0 255)
    (when (run-labelled-button uic (global-label :bold 14 "Begin Game")
                               (- width 80) (- height 30)
                               :color *button-color*)
      (setf *gadget-root* (make-instance 'briefing-page)))
    (show-pointer uic)
    (draw-img (img :help) width/2 height/2)))
