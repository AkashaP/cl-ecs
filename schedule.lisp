(in-package :cl-ecs) 
;; A schedule is a collection of systems.

(defun schedule (name)
  (or (gethash name (ecs-schedules *ecs*))
      (error "No such schedule: ~A" name)))

(declaim (notinline do-schedule))
(defun do-schedule (name)
  (dolist (s (schedule name))
    (do-system s))
  (cremate-dead-entities))

(defun collect-schedule (name)
  (loop :for sys :in (ecs:all-systems)
        :if (%schedule-filter-fn name sys) :collect sys))

(defun cache-schedules ()
  (loop :for name :being :the :hash-keys :of (ecs-schedules *ecs*) 
        :do (setf (gethash name (ecs-schedules *ecs*))
                  (collect-schedule name))))

(let ((h (gensym)))
  (defmacro defschedule (name ((system-argn) &body filter-fn))
    `(progn
       (defmethod %schedule-filter-fn ((name (eql ',name)) ,h)
         (symbol-macrolet ((,system-argn ,h))
           ,@filter-fn))
       (unless (gethash ',name (ecs-schedules *ecs*)) 
         (setf (gethash ',name (ecs-schedules *ecs*)) (list)))
       (cache-schedules))))

