(in-package :cl-ecs)

(defvar *ecs* nil)

(defstruct ecs
  (entities (make-hash-table))
  (components (make-hash-table))
  (systems (make-hash-table))
  (schedules (make-hash-table))
  (system-names (list))
  (graveyard (list)))

(defun init-ecs ()
  "Initialize a new ECS system."
  (let ((id 0))
    (setf *ecs* (make-ecs))
    (defun new-id ()
      (incf id))
    (values)))
