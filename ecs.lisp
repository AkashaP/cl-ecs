(in-package :cl-ecs)

(defvar *ecs* nil)
(defvar *graveyard* (list))

(defstruct ecs
  (entities (make-hash-table))
  (components (make-hash-table))
  (systems (make-hash-table)))

(defun init-ecs ()
  "Initialize a new ECS system."
  (let ((id 0))
    (setf *ecs* (make-ecs))
    (defun new-id ()
      (incf id))
    (values)))
