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

(defconst yggdrasil--vert-step 2
  "Vertical distance between levels in topology mode.")

(defun yggdrasil--compute-widths (node)
  "Post-order: compute subtree-width for each NODE."
  (let ((children (yggdrasil-node-children node)))
    (if (null children)
        ;; Leaf: width is label length, minimum 1
        (setf (yggdrasil-node-subtree-width node)
              (max 1 (length (yggdrasil-node-name node))))
      ;; Internal: sum of children widths + gaps
      (dolist (c children)
        (yggdrasil--compute-widths c))
      (setf (yggdrasil-node-subtree-width node)
            (max (length (yggdrasil-node-name node))
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
      (aset grid i (make-string width ?\s)))
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

(defun yggdrasil--junction-char (up down left right)
  "Return the appropriate box-drawing character given direction flags."
  (cond
   ;; Four-way
   ((and up down left right) ?┼)
   ;; Three-way
   ((and up left right) ?┴)
   ((and down left right) ?┬)
   ((and up down right) ?├)
   ((and up down left) ?┤)
   ;; Two-way corners
   ((and down right) ?┌)
   ((and down left) ?┐)
   ((and up right) ?└)
   ((and up left) ?┘)
   ;; Straight
   ((and left right) ?─)
   ((and up down) ?│)
   ;; Single direction
   (left ?─)
   (right ?─)
   (up ?│)
   (down ?│)
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
            (cl-loop for row from (1+ py) below cy
                     do (yggdrasil--grid-set grid px row ?│))
            ;; If child is offset, draw horizontal + vertical
            (when (/= px cx)
              (let ((min-x (min px cx))
                    (max-x (max px cx)))
                ;; Horizontal bar at row py+1
                (cl-loop for col from (1+ min-x) below max-x
                         do (yggdrasil--grid-set grid col (1+ py) ?─))
                ;; Junction at parent x
                (yggdrasil--grid-set grid px (1+ py)
                                     (yggdrasil--junction-char nil t (> px cx) (< px cx)))
                ;; Junction at child x
                (yggdrasil--grid-set grid cx (1+ py)
                                     (yggdrasil--junction-char nil nil (< cx px) (> cx px)))
                ;; Vertical from junction down to child
                (cl-loop for row from (+ py 2) below cy
                         do (yggdrasil--grid-set grid cx row ?│)))))
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
                     do (yggdrasil--grid-set grid px row ?│)))
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
                          (yggdrasil--grid-set grid col bar-y ?─)))))
          ;; Vertical segments from bar down to each child
          (dolist (child children)
            (let ((cx (yggdrasil-node-x child))
                  (cy (yggdrasil-node-y child)))
              (cl-loop for row from (1+ bar-y) below cy
                       do (yggdrasil--grid-set grid cx row ?│)))))))))

(defun yggdrasil--render (root highlighted-node)
  "Render ROOT tree to a string.  HIGHLIGHTED-NODE gets bold face."
  (yggdrasil--compute-widths root)
  ;; Compute a scale for proportional mode
  (let ((scale 10))
    (when yggdrasil-show-branch-lengths
      ;; Find max branch length to calibrate scale
      (let ((max-len 0))
        (yggdrasil--walk root
                         (lambda (n)
                           (when (yggdrasil-node-length n)
                             (setq max-len (max max-len (yggdrasil-node-length n))))))
        (when (> max-len 0)
          ;; Target ~20 rows max height
          (setq scale (/ 20.0 max-len)))))
    (let* ((root-width (yggdrasil-node-subtree-width root))
           (root-x (/ root-width 2)))
      (yggdrasil--assign-positions root (max root-x 1) 0 scale)))
  ;; Normalize so everything is in positive coords
  (let* ((bounds (yggdrasil--normalize-positions root))
         (grid-width (+ (car bounds) 2))
         (grid-height (+ (cdr bounds) 2))
         (grid (yggdrasil--make-grid grid-width grid-height)))
    ;; Draw connectors first
    (yggdrasil--walk root
                     (lambda (n) (yggdrasil--draw-connectors grid n)))
    ;; Draw labels on top
    (yggdrasil--walk
     root
     (lambda (n)
       (let* ((name (yggdrasil-node-name n))
              (nx (yggdrasil-node-x n))
              (ny (yggdrasil-node-y n))
              (label-len (length name))
              (lx (- nx (/ label-len 2)))
              (face (when (eq n highlighted-node) 'bold)))
         (if (> label-len 0)
             (yggdrasil--grid-put-string grid lx ny name face)
           ;; Unnamed node: just ensure junction char is present
           (when face
             (let ((ch (aref (aref grid ny) nx)))
               (yggdrasil--grid-set grid nx ny ch 'bold)))))))
    ;; Join grid rows into final string
    (mapconcat (lambda (row)
                 ;; Trim trailing spaces
                 (replace-regexp-in-string "\\s-+$" "" row))
               grid "\n")))

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
      (yggdrasil-mode))
    (with-current-buffer source-buffer
      (setq yggdrasil--display-buffer buf))
    (if (display-graphic-p)
        (yggdrasil--show-child-frame buf max-line-len num-lines source-buffer)
      (yggdrasil--show-window buf source-buffer))))

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

;;;; Modes

(defvar yggdrasil-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
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
    map)
  "Keymap for `yggdrasil-active-mode'.")

(define-minor-mode yggdrasil-active-mode
  "Minor mode active in the source buffer while a tree is displayed."
  :lighter " Ygg"
  :keymap yggdrasil-active-mode-map
  (if yggdrasil-active-mode
      (add-hook 'post-command-hook #'yggdrasil--post-command nil t)
    (remove-hook 'post-command-hook #'yggdrasil--post-command t)))

;;;; Lifecycle

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
  "Re-render the tree with the current cursor position highlighted."
  (when yggdrasil--root
    (let* ((bounds yggdrasil--newick-bounds)
           (highlighted (when (and bounds
                                   (>= (point) (car bounds))
                                   (<= (point) (cdr bounds)))
                          (yggdrasil--node-at-pos yggdrasil--root (point))))
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
           (highlighted (yggdrasil--node-at-pos root (point)))
           (content (yggdrasil--render root highlighted)))
      (setq yggdrasil--root root
            yggdrasil--newick-bounds bounds
            yggdrasil--source-buffer (current-buffer))
      (yggdrasil--show-frame content (current-buffer))
      (yggdrasil-active-mode 1)
      (message "Yggdrasil: tree visualized. Press q to dismiss, t to toggle lengths."))))

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
        yggdrasil--newick-bounds nil)
  (yggdrasil-active-mode -1)
  (message "Yggdrasil: dismissed."))

(defun yggdrasil-toggle-lengths ()
  "Toggle proportional branch lengths and re-render."
  (interactive)
  (setq yggdrasil-show-branch-lengths (not yggdrasil-show-branch-lengths))
  (when yggdrasil--root
    ;; Re-parse to get fresh node structs (positions get reset)
    (let* ((bounds yggdrasil--newick-bounds)
           (str (buffer-substring-no-properties (car bounds) (cdr bounds)))
           (root (yggdrasil--parse str (car bounds)))
           (highlighted (yggdrasil--node-at-pos root (point)))
           (content (yggdrasil--render root highlighted)))
      (setq yggdrasil--root root)
      (when (and yggdrasil--display-buffer
                 (buffer-live-p yggdrasil--display-buffer))
        (with-current-buffer yggdrasil--display-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert content)
            (goto-char (point-min)))))))
  (message "Yggdrasil: branch lengths %s."
           (if yggdrasil-show-branch-lengths "shown" "hidden")))

(provide 'yggdrasil)

;;; yggdrasil.el ends here
