(require 'cl)


(cl-defstruct jong-cursor-location
  buffer
  cursor-offset
  buffer-line)


(defvar jong-cursor-history-queue-size 20)
(defcustom jong-cursor-history-queue nil
  "This is a queue storing a history of cursor positions."
  :type 'list)
(setq jong-cursor-history-queue '())

(defcustom jong-cursor-current 0
  "This is the element sequence of the cursor history queue."
  :type 'integer)


(defun jong-cursor-push-history-queue (location)
  "."
  (let ((length-of-queue (length jong-cursor-history-queue))
        (last-elem))
    (if (not (equal jong-cursor-history-queue nil))
        (progn
          (when (equal length-of-queue nil) (setq length-of-queue 0))
          (setq last-elem (nth (1- length-of-queue) jong-cursor-history-queue))
          
          (if (equal (jong-cursor-location-buffer last-elem) (jong-cursor-location-buffer location))
              (when (equal (jong-cursor-location-cursor-offset last-elem) (jong-cursor-location-cursor-offset location)) nil)
            (if (> length-of-queue jong-cursor-history-queue-size)
                (pop jong-cursor-history-queue))
            (add-to-list 'jong-cursor-history-queue location))
          t)
      (progn
        (add-to-list 'jong-cursor-history-queue location)
        t)))
  )

(defun jong-cursor-pop-history-queue ()
  "."
  (let ((ret))
    (if (> (length jong-cursor-history-queue) 0)
        (pop jong-cursor-history-queue)
      nil))
  )

(defun jong-cursor-get-location ()
  "."
  (let ((ret))
    (setq ret (make-jong-cursor-location
               :buffer (buffer-name)
               :cursor-offset (point)
               :buffer-line (thing-at-point 'line t)))
    ret)
  )


(defun jong-cursor-current-location ()
  (interactive)
  "Save the position of cusor to history ring."
  (jong-cursor-push-history-queue
   (jong-cursor-get-location))
  )

(defun jong-cursor-history-pre-command-hook ()
  (interactive)
  "Hook"
  (let ((cursor-location))
    (message "current current : %d, total : %d" jong-cursor-current (length jong-cursor-history-queue))
    (setq cursor-location (jong-cursor-get-location))
    (if (equal (jong-cursor-push-history-queue cursor-location) nil)
        (message "Couldnt push the location to the queue.")
      ))
  )

(defun jong-cursor-move-to-position (position)
  n  "."
  (let ((buffer-namep)
        (offset)
        (line))
    (if (not (typep position 'jong-cursor-location))
        nil
      (progn
        (setq buffer-name (jong-cursor-location-buffer position))
        (setq offset (jong-cursor-location-cursor-offset position))
        (setq line (jong-cursor-location-buffer-line position))
        (with-current-buffer (get-buffer buffer-name)
          (goto-char (offset))
          (display-buffer (current-buffer)))
        )))
  )


(defun jong-cursor-jump-next ()
  "Jump to the next cursor position."
  (interactive)n
  (let ((position))
    )
  )

(defun jong-cursor-jump-prev ()
  "Jump to the previous cursor position."
  (interactive)
  (let ((position))
    (setq position (nth jong-cursor-current jong-cursor-history-queue))
    (if position
        (jong-cursor-move-to-position position)
      (message "The cursor position was nil.")))
  )


(add-hook 'pre-command-hook 'jong-cursor-history-pre-command-hook)



;; (defvar point-undo-ring-length 20)
;; (defvar point-undo-ring (make-ring point-undo-ring-length))
;; (make-variable-buffer-local 'point-undo-ring)

;; (defvar point-redo-ring (make-ring point-undo-ring-length))
;; (make-variable-buffer-local 'point-redo-ring)

;; (defun point-undo-pre-command-hook ()
;; "Save positions before command."
;; (unless (or (eq this-command 'point-undo)
;; (eq this-command 'point-redo))
;; (let ((line (line-number-at-pos)))
;; (when (eq line (cdr (nth 0 (ring-elements point-undo-ring))))
;; (ring-remove point-undo-ring 0))
;; (ring-insert point-undo-ring (cons (point) line))
;; (setq point-redo-ring (make-ring point-undo-ring-length)))))


;; (defun point-undo-doit (ring1 ring2)
;; "ring1, ring2 = {point-undo-ring, point-redo-ring}"
;; (condition-case nil
;; (progn
;; (goto-char (car (nth 0 (ring-elements ring1)))) 
;; (ring-insert ring2 (ring-remove ring1 0)))
;; (error nil)))

;; (defun point-undo ()
;; "Undo position."
;; (interactive)
;; (point-undo-doit point-undo-ring point-redo-ring))

;; (defun point-redo ()
;; "Redo position."
;; (interactive)
;; (when (or (eq last-command 'point-undo)
;; (eq last-command 'point-redo))
;; (point-undo-doit point-redo-ring point-undo-ring)))





(provide 'jong-cursor-tracker)
