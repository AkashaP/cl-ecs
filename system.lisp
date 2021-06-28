(in-package :cl-ecs)

(defstruct (system (:conc-name nil))
  processing
  required
  grouping
  entities)

;; in most ECS a System owns one family, and uses the family matching patterns "All", "Any" and "None" but i find "All" the only sensible choice, especially since if you use "Any" you might have to distinctify each component in the system logic, defeating the purpose of ECS caching entities for us already
;; it also tends to treat all entities with similar behaviour, which I don't feel is the purpose of ECS

;; A major difference in this (forked) implementation is that system now owns multiple families
;; it compares each entity from each family once
;; This allows distinction between different types of entities
;; e.g enemy's collision hits player's collision
;; you have more direct, subtle interaction between the player and the enemy here
;; you could do things like give only the player invincibility frames but not the enemy
;; or play a player-only hurt sound

;; if this was normal ECS you would have to do this very indirectly with the data somehow
;; this still keeps decoupled game logic in my opinion because (now) you can also turn systems on and off
;; without needing them there
;; e.g you could iterate over all collision boxes but you dont have a good way of knowing who the collision box belongs to

;; the only problem i have with this method is i can't think of a sensible reason
;; why systems would operate on more than two families at once.

;; This syntax is a variant of defsys that specifies owned families in a letargs format
;;

(defmacro defsys (name (&body letargs) &rest body)
  (let ((entities (gensym)))
    `(progn
       (setf (gethash ',name (ecs-systems *ecs*))
             (make-system :processing t
                          :required ',(mapcar #'cadr letargs)
                          :grouping ',(mapcar #'car letargs)))
       (cache-system-entities)
       (defmethod %do-entities ((system (eql ',name)) &rest ,entities)
         (block ,name
           (destructuring-bind ,(mapcar #'car letargs) ,entities
             ,@body)
           )))))

(defmacro defsys-old (name (required grouping) &body body)
  "Define a new system."
  (let ((entities (gensym)))
    `(progn
       (setf (gethash ',name (ecs-systems *ecs*))
             (make-system :required ',required
                          :grouping ',grouping))
       (cache-system-entities)
       (defmethod %do-entities ((system (eql ',name)) &rest ,entities)
         (block ,name
           (destructuring-bind ,grouping ,entities
             ,@body))))))


(defgeneric %do-entities (system &rest entities))

(defun all-systems ()
  "Get a list of all defined systems."
  (hash-table-keys (ecs-systems *ecs*)))

(defun system-processing (system)
  "Asks whether a system is processing.
   This only affects (do-system [system]) calls."
  (multiple-value-bind (sys b)
      (gethash system (ecs-systems *ecs*))
    (when b
      (processing sys))))

(defun (setf system-processing) (value system)
  "Turns system processing on or off for (do-system) calls"
  (multiple-value-bind (sys b)
      (gethash system (ecs-systems *ecs*))
    (when b
      (setf (processing sys) value))))


(defun required-components (system)
  "Get a list of the specified system's required components."
  (required (gethash system (ecs-systems *ecs*))))

(defun (setf required-components) (value system)
  "Assign a list of required components to the specified system."
  (setf (required (gethash system (ecs-systems *ecs*))) value))

(defun system-grouping (system)
  "Get the list of grouping information for the specified system."
  (grouping (gethash system (ecs-systems *ecs*))))

(defun collect-system-entities (system)
  "Create a list of all of a system's entities."
  (loop :with rs = (required-components system)
        :for r :in rs
        collect
        (loop
          :for (id . e) :in (hash-table-alist (ecs-entities *ecs*))
          :for c = (components e)
          :when (or (not r)
                    (and (listp r) (all r c))
                    (and (eq r :none) (not c))
                    (and (eq r :any) c))
            :collect id)))

(defun system-entities (system)
  "Get a list of all of a system's entities."
  (entities (gethash system (ecs-systems *ecs*))))

(defun (setf system-entities) (value system)
  "Assign a list of entities to the specified system."
  (setf (entities (gethash system (ecs-systems *ecs*))) value))

(defun cache-system-entities ()
  "Update the the list of entities for all systems."
  (loop :for system :in (all-systems)
        :do (setf (system-entities system) (collect-system-entities system))))

(defun duplicatesp (list)
  (mapl (lambda (cdr) (if (eql (first cdr) (second cdr))
                          (return-from duplicatesp T)))
        (sort list '<)) nil)

(defmethod do-system (system)
  "Execute the specified system. The system definition's grouping determines
parallel processing of entities."
  (when (system-processing system)
    (let ((result))
      (apply #'metatilities:map-combinations
             (lambda (&rest x)
               ;; Prevent entities from comparing themselves if they match multiple families
               (when (not (duplicatesp x))
                 (setf result (apply #'%do-entities system x))))
             (system-entities system))
      (when (eq (ecs-rip *ecs*) 'remove-all)
         (delete-all-entities)
         (setf (ecs-rip *ecs*) (list)))
       (loop :for e :in (ecs-rip *ecs*)
             do (delete-entity e))
      result)))

(defun cycle-systems ()
  "Cycle through all defined systems."
  (dolist (system (all-systems))
    (do-system system)))

