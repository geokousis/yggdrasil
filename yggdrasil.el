;;; yggdrasil.el --- Visualize Newick phylogenetic trees -*- lexical-binding: t; -*-

;; Author: Yggdrasil Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools, science
;; URL: https://github.com/yggdrasil/yggdrasil.el

;;; Commentary:

;; Detects Newick phylogenetic tree strings at point, parses them, and
;; displays a vertical (top-down) ASCII tree in a child frame with the
;; cursor's branch highlighted in bold.
;;
;; Usage:
;;   Place cursor inside a Newick string and run M-x yggdrasil-visualize.
;;   Press q to dismiss, t to toggle proportional branch lengths.

;;; Code:

(require 'cl-lib)

;;;; Customization

(defgroup yggdrasil nil
  "Newick tree visualizer."
  :group 'tools
  :prefix "yggdrasil-")

(defcustom yggdrasil-auto-close nil
  "If non-nil, close the tree frame when cursor leaves the Newick string."
  :type 'boolean
  :group 'yggdrasil)

(defcustom yggdrasil-show-branch-lengths nil
  "If non-nil, draw proportional branch lengths instead of uniform topology."
  :type 'boolean
  :group 'yggdrasil)

(defcustom yggdrasil-display-method 'auto
  "How to display the rendered tree.
`auto' prefers child frames in GUI Emacs and falls back to a window.
`child-frame' forces child frame display.
`window' always uses a normal Emacs window."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Child frame" child-frame)
                 (const :tag "Window" window))
  :group 'yggdrasil)

(defcustom yggdrasil-frame-parameters
  '((internal-border-width . 1)
    (left-fringe . 0)
    (right-fringe . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (line-spacing . 0)
    (no-special-glyphs . t)
    (undecorated . t))
  "Extra frame parameters for the child frame."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'yggdrasil)

;;;; Faces

(defface yggdrasil-highlight
  '((t :inherit success :weight bold))
  "Face for the highlighted node in the tree display."
  :group 'yggdrasil)

;;;; Data structures

(cl-defstruct (yggdrasil-node (:constructor yggdrasil-node-create))
  "A node in a parsed Newick tree."
  (name "" :type string)
  (length nil :type (or null number))
  (children nil :type list)
  (start-pos 0 :type integer)
  (end-pos 0 :type integer)
  (x 0 :type integer)
  (y 0 :type integer)
  (subtree-width 0 :type integer))

;;;; Internal state

(defvar-local yggdrasil--frame nil
  "The child frame displaying the tree.")

(defvar-local yggdrasil--root nil
  "The parsed tree root node.")

(defvar-local yggdrasil--newick-bounds nil
  "Cons (BEG . END) of the detected Newick string in source buffer.")

(defvar-local yggdrasil--source-buffer nil
  "The source buffer containing the Newick string.")

(defvar-local yggdrasil--orientation 'top-down
  "Current tree orientation.  Either `top-down' or `left-to-right'.")

(defvar-local yggdrasil--display-buffer nil
  "The buffer used for tree display.")

;;;; Newick detection

(defun yggdrasil--detect-newick ()
  "Detect a Newick string around point.
Return (BEG . END) or nil.  A Newick string is a balanced
parenthesized expression (or bare label) ending with `;'."
  (save-excursion
    (let ((orig (point))
          beg end)
      ;; Find the terminating semicolon
      (when (search-forward ";" nil t)
        (setq end (point))
        (let ((semi-pos (1- end))
              (depth 0)
              (found-paren nil))
          ;; Scan backward from just before ; counting parens
          (goto-char semi-pos)
          (while (and (> (point) (point-min))
                      (not (and found-paren (= depth 0))))
            (backward-char)
            (let ((ch (char-after)))
              (cond
               ((eq ch ?\)) (cl-incf depth) (setq found-paren t))
               ((eq ch ?\() (cl-decf depth))
               ((and (not found-paren) (memq ch '(?\; ?\n)))
                (forward-char)
                (goto-char (point-min))))))
          (if found-paren
              (when (= depth 0)
                (setq beg (point)))
            ;; Bare label: scan back from semicolon
            (goto-char semi-pos)
            (skip-chars-backward "^;\n()")
            (setq beg (point)))
          (when (and beg (<= beg orig) (<= orig end))
            (cons beg end)))))))

;;;; Newick parser

(defun yggdrasil--parse (str offset)
  "Parse Newick string STR with buffer OFFSET.
Return the root `yggdrasil-node'."
  (let ((state (cons str 0)))
    (let ((root (yggdrasil--parse-tree state offset)))
      ;; Consume the trailing ';' if present
      (when (and (< (cdr state) (length (car state)))
                 (eq (aref (car state) (cdr state)) ?\;))
        (setf (cdr state) (1+ (cdr state))))
      root)))

(defun yggdrasil--parse-peek (state)
  "Peek at the current character in parser STATE, or nil at end."
  (when (< (cdr state) (length (car state)))
    (aref (car state) (cdr state))))

(defun yggdrasil--parse-advance (state)
  "Advance parser STATE by one character, return the consumed char."
  (prog1 (yggdrasil--parse-peek state)
    (setf (cdr state) (1+ (cdr state)))))

(defun yggdrasil--parse-tree (state offset)
  "Parse a tree/subtree from STATE with buffer OFFSET."
  (let ((start-idx (cdr state))
        children name len)
    (when (eq (yggdrasil--parse-peek state) ?\()
      (yggdrasil--parse-advance state) ; consume (
      (push (yggdrasil--parse-tree state offset) children)
      (while (eq (yggdrasil--parse-peek state) ?,)
        (yggdrasil--parse-advance state) ; consume ,
        (push (yggdrasil--parse-tree state offset) children))
      (when (eq (yggdrasil--parse-peek state) ?\))
        (yggdrasil--parse-advance state)) ; consume )
      (setq children (nreverse children)))
    ;; Parse optional label
    (setq name (yggdrasil--parse-label state))
    ;; Parse optional branch length
    (when (eq (yggdrasil--parse-peek state) ?:)
      (yggdrasil--parse-advance state) ; consume :
      (setq len (yggdrasil--parse-number state)))
    (let ((end-idx (cdr state)))
      (yggdrasil-node-create
       :name (or name "")
       :length len
       :children children
       :start-pos (+ offset start-idx)
       :end-pos (+ offset end-idx)
       :x 0 :y 0 :subtree-width 0))))

(defun yggdrasil--parse-label (state)
  "Parse a node label from STATE.  Return the label string or nil."
  (let ((start (cdr state))
        (str (car state)))
    (while (and (< (cdr state) (length str))
                (not (memq (aref str (cdr state)) '(?\( ?\) ?, ?\: ?\;))))
      (setf (cdr state) (1+ (cdr state))))
    (when (> (cdr state) start)
      (substring str start (cdr state)))))

(defun yggdrasil--parse-number (state)
  "Parse a number (integer or float) from STATE."
  (let ((start (cdr state))
        (str (car state)))
    (while (and (< (cdr state) (length str))
                (let ((ch (aref str (cdr state))))
                  (or (and (>= ch ?0) (<= ch ?9))
                      (eq ch ?.)
                      (eq ch ?-)
                      (eq ch ?+)
                      (eq ch ?e)
                      (eq ch ?E))))
      (setf (cdr state) (1+ (cdr state))))
    (when (> (cdr state) start)
      (string-to-number (substring str start (cdr state))))))

;;;; Position-to-node mapping

(defun yggdrasil--node-at-pos (node pos)
  "Find the deepest node in NODE tree whose [start-pos, end-pos) contains POS."
  (when (and (<= (yggdrasil-node-start-pos node) pos)
             (< pos (yggdrasil-node-end-pos node)))
    (or (cl-some (lambda (child) (yggdrasil--node-at-pos child pos))
                 (yggdrasil-node-children node))
        node)))

;;;; ASCII renderer

(defconst yggdrasil--vert-step 3
  "Vertical distance between levels in topology mode.")

(defconst yggdrasil-min-internal-width 3
  "Minimum width of internal nodes.")

(defun yggdrasil--compute-widths (node)
  "Post-order: compute subtree-width for each NODE."
  (let ((children (yggdrasil-node-children node)))
    (if (null children)
        ;; Leaf: width is label length, minimum 1
        (setf (yggdrasil-node-subtree-width node)
              (max 3 (length (yggdrasil-node-name node))))
      ;; Internal: sum of children widths + gaps
      (dolist (c children)
        (yggdrasil--compute-widths c))
      (setf (yggdrasil-node-subtree-width node)
            (max (length (yggdrasil-node-name node))
				 yggdrasil-min-internal-width
				 (+ (cl-reduce #'+ children
                               :key #'yggdrasil-node-subtree-width)
                    (* 2 (1- (length children)))))))))

(defun yggdrasil--assign-positions (node x y scale)
  "Pre-order: assign x,y coordinates to NODE starting at X, Y.
SCALE is used for proportional mode."
  (setf (yggdrasil-node-x node) x
        (yggdrasil-node-y node) y)
  (let* ((children (yggdrasil-node-children node))
         (total-children-width
          (when children
            (+ (cl-reduce #'+ children :key #'yggdrasil-node-subtree-width)
               (* 2 (1- (length children))))))
         ;; Center the node label above its children
         (child-start-x (when children
                          (- x (/ total-children-width 2)))))
    ;; Center x over children if internal
    (when children
      (let ((cx child-start-x))
        (dolist (c children)
          (let* ((cw (yggdrasil-node-subtree-width c))
                 (child-center (+ cx (/ cw 2)))
                 (child-y (if (and yggdrasil-show-branch-lengths
                                   (yggdrasil-node-length c)
                                   (> (yggdrasil-node-length c) 0))
                              (+ y (max 2 (round (* (yggdrasil-node-length c) scale))))
                            (+ y yggdrasil--vert-step))))
            (yggdrasil--assign-positions c child-center child-y scale)
            (setq cx (+ cx cw 2))))))
      ;; Recenter this node over its children
    (when (and children (> (length children) 0))
      (let ((first-x (yggdrasil-node-x (car children)))
            (last-x (yggdrasil-node-x (car (last children)))))
        (setf (yggdrasil-node-x node) (/ (+ first-x last-x) 2))))))

(defun yggdrasil--normalize-positions (node)
  "Shift all node positions so minimum x is 0.  Return (max-x . max-y)."
  (let ((min-x most-positive-fixnum)
        (max-x most-negative-fixnum)
        (max-y 0))
    ;; Find bounds
    (yggdrasil--walk node
                     (lambda (n)
                       (let ((nx (yggdrasil-node-x n))
                             (ny (yggdrasil-node-y n))
                             (half-label (/ (length (yggdrasil-node-name n)) 2)))
                         (setq min-x (min min-x (- nx half-label)))
                         (setq max-x (max max-x (+ nx half-label)))
                         (setq max-y (max max-y ny)))))
    ;; Shift so min-x becomes 1 (1 col left margin)
    (let ((shift (- 1 min-x)))
      (yggdrasil--walk node
                       (lambda (n)
                         (cl-incf (yggdrasil-node-x n) shift)))
      (cons (+ max-x shift 1) max-y))))

(defun yggdrasil--walk (node fn)
  "Walk NODE tree depth-first, calling FN on each node."
  (funcall fn node)
  (dolist (c (yggdrasil-node-children node))
    (yggdrasil--walk c fn)))

(defun yggdrasil--make-grid (width height)
  "Create a 2D grid (vector of strings) of WIDTH x HEIGHT spaces."
  (let ((grid (make-vector height nil)))
    (dotimes (i height)
      (aset grid i (string-to-multibyte (make-string width ?\s))))
    grid))

(defun yggdrasil--grid-set (grid x y ch &optional face)
  "Set character at X, Y in GRID to CH with optional FACE."
  (when (and (>= y 0) (< y (length grid))
             (>= x 0) (< x (length (aref grid y))))
    (let ((row (aref grid y)))
      (aset row x ch)
      (when face
        (put-text-property x (1+ x) 'face face row)))))

(defun yggdrasil--grid-put-string (grid x y str &optional face)
  "Write STR at position X, Y in GRID with optional FACE."
  (let ((row (aref grid y))
        (len (length str)))
    (dotimes (i len)
      (when (and (>= (+ x i) 0) (< (+ x i) (length row)))
        (aset row (+ x i) (aref str i))
        (when face
          (put-text-property (+ x i) (+ x i 1) 'face face row))))))

(defun yggdrasil--grid-put-node-link (grid x y width node)
  "Attach source-jump properties for NODE on GRID at X,Y over WIDTH chars."
  (let* ((row (aref grid y))
         (row-len (length row))
         (beg (max 0 x))
         (end (min row-len (+ x width)))
         (src-pos (yggdrasil-node-start-pos node)))
    (when (< beg end)
      (add-text-properties
       beg end
       `(yggdrasil-source-pos ,src-pos
                              mouse-face highlight
                              help-echo "mouse-1/RET: jump to source")
       row))))

(defun yggdrasil--junction-char (up down left right)
  "Return the appropriate box-drawing character given direction flags."
  (cond
   ;; Four-way
   ((and up down left right) ?+)
   ;; Three-way
   ((and up left right) ?+)
   ((and down left right) ?+)
   ((and up down right) ?+)
   ((and up down left) ?+)
   ;; Two-way corners
   ((and down right) ?+)
   ((and down left) ?+)
   ((and up right) ?+)
   ((and up left) ?+)
   ;; Straight
   ((and left right) ?-)
   ((and up down) ?|)
   ;; Single direction
   (left ?-)
   (right ?-)
   (up ?|)
   (down ?|)
   (t ?\s)))

(defun yggdrasil--draw-connectors (grid node)
  "Draw connectors from NODE to its children on GRID."
  (let ((children (yggdrasil-node-children node))
        (px (yggdrasil-node-x node))
        (py (yggdrasil-node-y node)))
    (when children
      (if (= (length children) 1)
          ;; Single child: just vertical line
          (let* ((child (car children))
                 (cy (yggdrasil-node-y child))
                 (cx (yggdrasil-node-x child)))
            ;; Vertical segment from parent down
            (cl-loop for row from (1+ py) below  cy
                     do (yggdrasil--grid-set grid px row ?|))
            ;; If child is offset, draw horizontal + vertical
            (when (/= px cx)
              (let ((min-x (min px cx))
                    (max-x (max px cx)))
                ;; Horizontal bar at row py+1
                (cl-loop for col from (1+ min-x) below max-x
                         do (yggdrasil--grid-set grid col (1+ py) ?-))
                ;; Junction at parent x
                (yggdrasil--grid-set grid px (1+ py)
                                     (yggdrasil--junction-char nil t (> px cx) (< px cx)))
                ;; Junction at child x
                (yggdrasil--grid-set grid cx (1+ py)
                                     (yggdrasil--junction-char nil nil (< cx px) (> cx px)))
                ;; Vertical from junction down to child
                (cl-loop for row from (+ py 2) below cy
                         do (yggdrasil--grid-set grid cx row ?|)))))
        ;; Multiple children: horizontal bar connecting all
        (let* ((child-xs (mapcar #'yggdrasil-node-x children))
               (min-cx (apply #'min child-xs))
               (max-cx (apply #'max child-xs))
               (bar-y (1+ py)))
          ;; Vertical from parent to bar
          (yggdrasil--grid-set grid px bar-y
                               (yggdrasil--junction-char
                                t nil
                                (> px min-cx)
                                (< px max-cx)))
          ;; If parent is above bar, draw down segment
          (when (> bar-y (1+ py))
            (cl-loop for row from (1+ py) below bar-y
                     do (yggdrasil--grid-set grid px row ?|)))
          ;; Draw horizontal bar
          (cl-loop for col from min-cx to max-cx
                   do (let* ((is-child-x (member col child-xs))
                             (is-parent-x (= col px))
                             (has-left (> col min-cx))
                             (has-right (< col max-cx)))
                        (cond
                         (is-parent-x
                          ;; Already set above, but re-set with full info
                          (yggdrasil--grid-set
                           grid col bar-y
                           (yggdrasil--junction-char t t has-left has-right)))
                         (is-child-x
                          (yggdrasil--grid-set
                           grid col bar-y
                           (yggdrasil--junction-char nil t has-left has-right)))
                         (t
                          (yggdrasil--grid-set grid col bar-y ?-)))))
          ;; Vertical segments from bar down to each child
          (dolist (child children)
            (let ((cx (yggdrasil-node-x child))
                  (cy (yggdrasil-node-y child)))
              (cl-loop for row from (1+ bar-y) below (1+ cy)
                       do (yggdrasil--grid-set grid cx row ?|)))))))))

(defun yggdrasil--render (root highlighted-nodes)
  "Render ROOT tree to a string.
HIGHLIGHTED-NODES is a hash table of nodes to draw with highlight face."
  (if (eq yggdrasil--orientation 'left-to-right)
      (yggdrasil--render-horizontal root highlighted-nodes)
    (yggdrasil--render-vertical root highlighted-nodes)))

(defun yggdrasil--grid-to-string (grid)
  "Join GRID rows into a single string, trimming trailing spaces."
  (mapconcat (lambda (row)
               (replace-regexp-in-string "\\s-+$" "" row))
             grid "\n"))

(defun yggdrasil--compute-scale (root)
  "Compute a length scale for proportional mode from ROOT."
  (let ((scale 10))
    (when yggdrasil-show-branch-lengths
      (let ((max-len 0))
        (yggdrasil--walk root
                         (lambda (n)
                           (when (yggdrasil-node-length n)
                             (setq max-len (max max-len (yggdrasil-node-length n))))))
        (when (> max-len 0)
          (setq scale (/ 20.0 max-len)))))
    scale))

(defun yggdrasil--draw-labels (grid root highlighted-nodes center-x)
  "Draw node labels onto GRID for ROOT tree.
HIGHLIGHTED-NODES gets the highlight face.  If CENTER-X is non-nil,
labels are centered horizontally at each node's x; otherwise placed
starting at x."
  (yggdrasil--walk
   root
   (lambda (n)
     (let* ((name (yggdrasil-node-name n))
            (nx (yggdrasil-node-x n))
            (ny (yggdrasil-node-y n))
            (label-len (length name))
            (lx (if center-x (- nx (/ label-len 2)) nx))
            (face (when (and highlighted-nodes
                             (gethash n highlighted-nodes))
                    'yggdrasil-highlight)))
       (if (> label-len 0)
           (progn
             (yggdrasil--grid-put-string grid lx ny name face)
             (yggdrasil--grid-put-node-link grid lx ny label-len n))
         (progn
           (when face
             (let ((ch (aref (aref grid ny) nx)))
               (yggdrasil--grid-set grid nx ny ch 'yggdrasil-highlight)))
           ;; Unnamed internal nodes still get a 1-char clickable anchor.
           (yggdrasil--grid-put-node-link grid nx ny 1 n)))))))

;;; Vertical (top-down) rendering

(defun yggdrasil--render-vertical (root highlighted-nodes)
  "Render ROOT as a top-down tree.  HIGHLIGHTED-NODES are highlighted."
  (yggdrasil--compute-widths root)
  (let* ((scale (yggdrasil--compute-scale root))
         (root-width (yggdrasil-node-subtree-width root))
         (root-x (/ root-width 2)))
    (yggdrasil--assign-positions root (max root-x 2) 0 scale))
  (let* ((bounds (yggdrasil--normalize-positions root))
         (grid-width (+ (car bounds) 2))
         (grid-height (+ (cdr bounds) 2))
         (grid (yggdrasil--make-grid grid-width grid-height)))
    (yggdrasil--walk root
                     (lambda (n) (yggdrasil--draw-connectors grid n)))
    (yggdrasil--draw-labels grid root highlighted-nodes t)
    (yggdrasil--grid-to-string grid)))

;;; Horizontal (left-to-right) rendering

(defun yggdrasil--compute-vspan (node)
  "Post-order: compute vertical span for horizontal layout.
Reuses the `subtree-width' field.  Leaves get span 1, internal
nodes get the sum of children spans plus 1-row gaps."
  (let ((children (yggdrasil-node-children node)))
    (if (null children)
        (setf (yggdrasil-node-subtree-width node) 3)
      (dolist (c children) (yggdrasil--compute-vspan c))
      (setf (yggdrasil-node-subtree-width node)
            (+ (cl-reduce #'+ children :key #'yggdrasil-node-subtree-width)
               (1- (length children)))))))

(defun yggdrasil--max-label-length (root)
  "Return the maximum label length in ROOT tree."
  (let ((m 0))
    (yggdrasil--walk root (lambda (n) (setq m (max m (length (yggdrasil-node-name n))))))
    m))

(defun yggdrasil--assign-horiz-positions (node x y h-step scale)
  "Assign positions for horizontal layout.
X is the column (depth), Y is the row (vertical spread).
H-STEP is the horizontal distance between levels.  SCALE is for
proportional mode."
  (setf (yggdrasil-node-x node) x
        (yggdrasil-node-y node) y)
  (let ((children (yggdrasil-node-children node)))
    (when children
      (let ((cy (- y (/ (1- (yggdrasil-node-subtree-width node)) 2))))
        (dolist (c children)
          (let* ((cspan (yggdrasil-node-subtree-width c))
                 (child-center (+ cy (/ cspan 2)))
                 (child-x (if (and yggdrasil-show-branch-lengths
                                   (yggdrasil-node-length c)
                                   (> (yggdrasil-node-length c) 0))
                              (+ x (max h-step (round (* (yggdrasil-node-length c) scale))))
                            (+ x h-step))))
            (yggdrasil--assign-horiz-positions c child-x child-center h-step scale)
            (setq cy (+ cy cspan 1))))
        ;; Recenter over children
        (let ((first-y (yggdrasil-node-y (car children)))
              (last-y (yggdrasil-node-y (car (last children)))))
          (setf (yggdrasil-node-y node) (/ (+ first-y last-y) 2)))))))

(defun yggdrasil--draw-horiz-connectors (grid node)
  "Draw left-to-right connectors from NODE to its children on GRID."
  (let ((children (yggdrasil-node-children node))
        (px (yggdrasil-node-x node))
        (py (yggdrasil-node-y node)))
    (when children
      (let* ((label-end (+ px (length (yggdrasil-node-name node))))
             (junc-x (1+ label-end))
             (child-ys (mapcar #'yggdrasil-node-y children))
             (min-cy (apply #'min child-ys))
             (max-cy (apply #'max child-ys)))
        ;; Horizontal line from label end to junction column
        (cl-loop for col from label-end to junc-x
                 do (yggdrasil--grid-set grid col py ?-))
        (if (= (length children) 1)
            ;; Single child: horizontal line straight across
            (let* ((child (car children))
                   (cx (yggdrasil-node-x child))
                   (cy (yggdrasil-node-y child)))
              (if (= py cy)
                  ;; Same row: just extend horizontal line
                  (cl-loop for col from (1+ junc-x) below cx
                           do (yggdrasil--grid-set grid col cy ?-))
                ;; Different row: go horizontal to junc, vertical, then horizontal
                (yggdrasil--grid-set grid junc-x py
                                     (yggdrasil--junction-char (> py cy) (< py cy) t nil))
                (cl-loop for row from (1+ (min py cy)) below (max py cy)
                         do (yggdrasil--grid-set grid junc-x row ?|))
                (yggdrasil--grid-set grid junc-x cy
                                     (yggdrasil--junction-char (< cy py) (> cy py) nil t))
                (cl-loop for col from (1+ junc-x) below cx
                         do (yggdrasil--grid-set grid col cy ?-))))
          ;; Multiple children: vertical bar at junction column
          (cl-loop for row from min-cy to max-cy
                   do (let* ((is-child (member row child-ys))
                             (is-parent (= row py))
                             (has-up (> row min-cy))
                             (has-down (< row max-cy))
                             (has-left is-parent)
                             (has-right is-child))
                        (when (or is-child is-parent has-up has-down)
                          (yggdrasil--grid-set
                           grid junc-x row
                           (yggdrasil--junction-char has-up has-down has-left has-right)))))
          ;; Horizontal lines from junction to each child
          (dolist (child children)
            (let ((cx (yggdrasil-node-x child))
                  (cy (yggdrasil-node-y child)))
              (cl-loop for col from (1+ junc-x) below cx
                       do (yggdrasil--grid-set grid col cy ?-)))))))))

(defun yggdrasil--render-horizontal (root highlighted-nodes)
  "Render ROOT as a left-to-right tree.  HIGHLIGHTED-NODES are highlighted."
  (yggdrasil--compute-vspan root)
  (let* ((scale (yggdrasil--compute-scale root))
         (h-step (+ (yggdrasil--max-label-length root) 3))
         (root-vspan (yggdrasil-node-subtree-width root))
         (root-y (/ root-vspan 2)))
    (yggdrasil--assign-horiz-positions root 1 (max root-y 1) h-step scale))
  (let* ((bounds (yggdrasil--normalize-positions root))
         (grid-width (+ (car bounds) (yggdrasil--max-label-length root) 2))
         (grid-height (+ (cdr bounds) 2))
         (grid (yggdrasil--make-grid grid-width grid-height)))
    (yggdrasil--walk root
                     (lambda (n) (yggdrasil--draw-horiz-connectors grid n)))
    (yggdrasil--draw-labels grid root highlighted-nodes nil)
    (yggdrasil--grid-to-string grid)))

;;;; Child frame / display

(defun yggdrasil--char-pixel-size ()
  "Return (CHAR-WIDTH . CHAR-HEIGHT) in pixels for the current frame."
  (cons (frame-char-width) (frame-char-height)))

(defun yggdrasil--show-frame (content source-buffer)
  "Display CONTENT in a child frame near point in SOURCE-BUFFER."
  (let* ((buf (get-buffer-create "*yggdrasil*"))
         (lines (split-string content "\n"))
         (max-line-len (apply #'max (mapcar #'length lines)))
         (num-lines (length lines)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min)))
      (yggdrasil-mode)
      (setq-local yggdrasil--source-buffer source-buffer))
    (with-current-buffer source-buffer
      (setq yggdrasil--display-buffer buf))
    (pcase yggdrasil-display-method
      ('window
       (yggdrasil--show-window buf source-buffer))
      ('child-frame
       (if (display-graphic-p)
           (yggdrasil--show-child-frame buf max-line-len num-lines source-buffer)
         (user-error "Yggdrasil child-frame display requires GUI Emacs")))
      (_
       ;; Auto mode: prefer child frames, but always fall back to a normal
       ;; window if frame creation fails (common on some WM/GUI setups).
       (if (display-graphic-p)
           (condition-case err
               (yggdrasil--show-child-frame buf max-line-len num-lines source-buffer)
             (error
              (message "Yggdrasil: child frame failed (%s), using window."
                       (error-message-string err))
              (yggdrasil--show-window buf source-buffer)))
         (yggdrasil--show-window buf source-buffer))))))

(defun yggdrasil--show-child-frame (buf width height source-buffer)
  "Show BUF in a child frame of WIDTH x HEIGHT chars.
SOURCE-BUFFER is the originating buffer."
  (with-current-buffer source-buffer
    ;; Close existing frame if any
    (when (and yggdrasil--frame (frame-live-p yggdrasil--frame))
      (delete-frame yggdrasil--frame))
    (let* ((char-size (yggdrasil--char-pixel-size))
           (pixel-w (+ (* (+ width 2) (car char-size)) 20))
           (pixel-h (+ (* (+ height 1) (cdr char-size)) 10))
           (pos (or (window-absolute-pixel-position)
                    (cons 100 100)))
           (frame-x (car pos))
           (frame-y (+ (cdr pos) (cdr char-size)))
           (params (append
                    `((parent-frame . ,(selected-frame))
                      (no-accept-focus . t)
                      (width . (text-pixels . ,pixel-w))
                      (height . (text-pixels . ,pixel-h))
                      (left . ,frame-x)
                      (top . ,frame-y)
                      (minibuffer . nil)
                      (visibility . t))
                    yggdrasil-frame-parameters))
           (frame (make-frame params)))
      (set-frame-parameter frame 'background-color
                           (face-attribute 'default :background nil t))
      (let ((win (frame-root-window frame)))
        (set-window-buffer win buf)
        (set-window-dedicated-p win t))
      (setq yggdrasil--frame frame))))

(defun yggdrasil--show-window (buf source-buffer)
  "Show BUF in a split window (terminal fallback).
SOURCE-BUFFER tracks the display buffer."
  (with-current-buffer source-buffer
    (setq yggdrasil--display-buffer buf))
  (display-buffer buf '(display-buffer-below-selected
                        (window-height . fit-window-to-buffer))))

(defun yggdrasil--collect-highlighted-nodes (root bounds)
  "Return a hash table of nodes to highlight for ROOT within BOUNDS.
If region is active, include all nodes touched by the selected range.
Otherwise include the single deepest node at point."
  (let ((selected (make-hash-table :test 'eq)))
    (when bounds
      (if (use-region-p)
          (let ((beg (max (car bounds) (region-beginning)))
                (end (min (cdr bounds) (region-end))))
            (when (< beg end)
              (save-excursion
                (goto-char beg)
                (while (< (point) end)
                  (let ((node (yggdrasil--node-at-pos root (point))))
                    (when node
                      (puthash node t selected)))
                  (forward-char 1)))))
        (when (and (>= (point) (car bounds))
                   (<= (point) (cdr bounds)))
          (let ((node (yggdrasil--node-at-pos root (point))))
            (when node
              (puthash node t selected))))))
    (and (> (hash-table-count selected) 0) selected)))

(defun yggdrasil--source-pos-at-point ()
  "Return source position linked at point in a yggdrasil display buffer."
  (or (get-text-property (point) 'yggdrasil-source-pos)
      (when (< (point) (point-max))
        (get-text-property (1+ (point)) 'yggdrasil-source-pos))
      (when (> (point) (point-min))
        (get-text-property (1- (point)) 'yggdrasil-source-pos))))

(defun yggdrasil--resolve-source-buffer ()
  "Resolve source buffer for current yggdrasil display buffer."
  (let ((display-buf (current-buffer)))
    (or (and yggdrasil--source-buffer
             (buffer-live-p yggdrasil--source-buffer)
             yggdrasil--source-buffer)
        (cl-loop for buf in (buffer-list)
                 thereis
                 (with-current-buffer buf
                   (and (buffer-live-p yggdrasil--display-buffer)
                        (eq yggdrasil--display-buffer display-buf)
                        buf))))))

(defun yggdrasil--jump-to-source-pos (pos)
  "Jump to POS in the originating Newick source buffer."
  (let ((src (yggdrasil--resolve-source-buffer)))
    (unless (and src (buffer-live-p src))
      (user-error "Source buffer is no longer available"))
    (let ((win (or (get-buffer-window src t)
                   (display-buffer src))))
      (select-window win)
      (goto-char (min (point-max) (max (point-min) pos)))
      (recenter))))

(defun yggdrasil-visit-source ()
  "Jump to the source Newick position for node under point."
  (interactive)
  (let ((pos (yggdrasil--source-pos-at-point)))
    (unless pos
      (user-error "No node at point"))
    (yggdrasil--jump-to-source-pos pos)))

(defun yggdrasil-visit-source-mouse (event)
  "Jump to source Newick position for node clicked with mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (yggdrasil-visit-source))

;;;; Modes

(defvar yggdrasil-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'yggdrasil-visit-source)
    (define-key map (kbd "<return>") #'yggdrasil-visit-source)
    (define-key map (kbd "C-m") #'yggdrasil-visit-source)
    (define-key map (kbd "f") #'yggdrasil-visit-source)
    (define-key map [mouse-1] #'yggdrasil-visit-source-mouse)
    map)
  "Keymap for `yggdrasil-mode'.")

(define-derived-mode yggdrasil-mode special-mode "Yggdrasil"
  "Major mode for displaying Newick tree visualizations."
  (setq-local truncate-lines t)
  (setq-local cursor-type nil))

(defvar yggdrasil-active-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'yggdrasil-dismiss)
    (define-key map (kbd "t") #'yggdrasil-toggle-lengths)
    (define-key map (kbd "r") #'yggdrasil-rotate)
    map)
  "Keymap for `yggdrasil-active-mode'.")

(define-minor-mode yggdrasil-active-mode
  "Minor mode active in the source buffer while a tree is displayed."
  :lighter " Ygg"
  :keymap yggdrasil-active-mode-map
  (if yggdrasil-active-mode
      (progn
        (add-hook 'post-command-hook #'yggdrasil--post-command nil t)
        (if (boundp 'enable-theme-functions)
            (add-hook 'enable-theme-functions #'yggdrasil--on-theme-change nil t)
          (advice-add 'load-theme :after #'yggdrasil--on-theme-change)))
    (remove-hook 'post-command-hook #'yggdrasil--post-command t)
    (if (boundp 'enable-theme-functions)
        (remove-hook 'enable-theme-functions #'yggdrasil--on-theme-change t)
      (advice-remove 'load-theme #'yggdrasil--on-theme-change))))

;;;; Lifecycle

(defun yggdrasil--refresh-display (content)
  "Update the display buffer with CONTENT."
  (when (and yggdrasil--display-buffer
             (buffer-live-p yggdrasil--display-buffer))
    (with-current-buffer yggdrasil--display-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min))))))

(defun yggdrasil--on-theme-change (&rest _)
  "Re-render the tree display after a theme change."
  (when (and yggdrasil-active-mode yggdrasil--root)
    ;; Update child frame background if in GUI
    (when (and yggdrasil--frame (frame-live-p yggdrasil--frame))
      (set-frame-parameter yggdrasil--frame 'background-color
                           (face-attribute 'default :background nil t)))
    (yggdrasil--update-display)))

(defun yggdrasil--post-command ()
  "Post-command handler: update highlight or auto-close."
  (when yggdrasil-active-mode
    (let ((bounds yggdrasil--newick-bounds))
      (if (and bounds
               (>= (point) (car bounds))
               (<= (point) (cdr bounds)))
          ;; Still inside Newick string — re-render with updated highlight
          (yggdrasil--update-display)
        ;; Outside
        (if yggdrasil-auto-close
            (yggdrasil-dismiss)
          ;; Still keep the frame but clear highlight
          (yggdrasil--update-display))))))

(defun yggdrasil--update-display ()
  "Re-render the tree with point/region-driven highlighting."
  (when yggdrasil--root
    (let* ((bounds yggdrasil--newick-bounds)
           (highlighted (yggdrasil--collect-highlighted-nodes
                         yggdrasil--root bounds))
           (content (yggdrasil--render yggdrasil--root highlighted))
           (buf (or yggdrasil--display-buffer
                    (get-buffer-create "*yggdrasil*"))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert content)
            (goto-char (point-min))))))))

;;;; Commands

;;;###autoload
(defun yggdrasil-visualize ()
  "Detect, parse, and visualize a Newick tree at point."
  (interactive)
  (let ((bounds (yggdrasil--detect-newick)))
    (unless bounds
      (user-error "No Newick string found at point"))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (str (buffer-substring-no-properties beg end))
           (root (yggdrasil--parse str beg))
           (highlighted (yggdrasil--collect-highlighted-nodes root bounds))
           (content (yggdrasil--render root highlighted)))
      (setq yggdrasil--root root
            yggdrasil--newick-bounds bounds
            yggdrasil--source-buffer (current-buffer))
      (yggdrasil--show-frame content (current-buffer))
      (yggdrasil-active-mode 1)
      (message "Yggdrasil: tree visualized. Click/RET jumps to source; q dismisses, t toggles lengths."))))

(defun yggdrasil-dismiss ()
  "Close the tree visualization and clean up."
  (interactive)
  (when (and yggdrasil--frame (frame-live-p yggdrasil--frame))
    (delete-frame yggdrasil--frame)
    (setq yggdrasil--frame nil))
  (when (and yggdrasil--display-buffer (buffer-live-p yggdrasil--display-buffer))
    ;; Close window if displayed in terminal mode
    (let ((win (get-buffer-window yggdrasil--display-buffer)))
      (when win (delete-window win)))
    (kill-buffer yggdrasil--display-buffer)
    (setq yggdrasil--display-buffer nil))
  (setq yggdrasil--root nil
        yggdrasil--newick-bounds nil
        yggdrasil--orientation 'top-down)
  (yggdrasil-active-mode -1)
  (message "Yggdrasil: dismissed."))

(defun yggdrasil-toggle-lengths ()
  "Toggle proportional branch lengths and re-render."
  (interactive)
  (setq yggdrasil-show-branch-lengths (not yggdrasil-show-branch-lengths))
  (when yggdrasil--root
    (let* ((bounds yggdrasil--newick-bounds)
           (str (buffer-substring-no-properties (car bounds) (cdr bounds)))
           (root (yggdrasil--parse str (car bounds)))
           (highlighted (yggdrasil--collect-highlighted-nodes root bounds))
           (content (yggdrasil--render root highlighted)))
      (setq yggdrasil--root root)
      (yggdrasil--refresh-display content)))
  (message "Yggdrasil: branch lengths %s."
           (if yggdrasil-show-branch-lengths "shown" "hidden")))

(defun yggdrasil-rotate ()
  "Toggle tree orientation between top-down and left-to-right."
  (interactive)
  (setq yggdrasil--orientation
        (if (eq yggdrasil--orientation 'top-down) 'left-to-right 'top-down))
  (when yggdrasil--root
    (let* ((bounds yggdrasil--newick-bounds)
           (str (buffer-substring-no-properties (car bounds) (cdr bounds)))
           (root (yggdrasil--parse str (car bounds)))
           (highlighted (yggdrasil--collect-highlighted-nodes root bounds))
           (content (yggdrasil--render root highlighted)))
      (setq yggdrasil--root root)
      (yggdrasil--refresh-display content)))
  (message "Yggdrasil: %s." yggdrasil--orientation))

(provide 'yggdrasil)

;;; yggdrasil.el ends here
