(in-package :cl-ecs)

(defvar *ecs* nil)
(defvar *graveyard* nil)
(defvar *system-names* nil)
(defvar *flags* nil)

(defstruct ecs
  (entities (make-hash-table))
  (components (make-hash-table))
  (systems (make-hash-table)))

(defun init-ecs ()
  "Initialize a new ECS system."
  (let ((id 0))
    (setf *ecs* (make-ecs))
    (setf *system-names* (list))
    (setf *flags* (make-hash-table))
    (defun new-id ()
      (incf id))
    (values)))
