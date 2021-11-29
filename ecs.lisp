(in-package :cl-ecs)

(defvar *ecs* nil)
(defvar *graveyard* nil)

(defstruct ecs
  (entities (make-hash-table))
  (components (make-hash-table))
  (systems (make-hash-table))
  (system-names (list)))

(defun init-ecs ()
  "Initialize a new ECS system."
  (let ((id 0))
    (setf *ecs* (make-ecs))
    (defun new-id ()
      (incf id))
    (values)))

;; Cant really do this without making a copier for each defstruct inside this program
;; Instead i should generalise i think
;; (defun destructively-copy-ecs (to &optional (from *ecs*))
;;   (check-type to ecs)
;;   (check-type from ecs)

;;   ;; Update cons cells
;;   (loop for oe in (ecs-entities to)
;;         for ne in (ecs-entities from)
;;         for i from 0
;;         do (setf (ecs-entities to))
;;         finally
;;            (loop while ))
;;   ())
