(in-package :cl-ecs)

(defstruct (entity (:conc-name nil))
  components
  attributes
  tags ;; static data
  (flags (make-hash-table)) ;; dynamic 'garbageless' bit field, reset every frame, for every system to process
  (blame-flags (make-hash-table)) ;; map of flags to system who modified it. mostly for debugging
  (marks (make-hash-table))  
  (blame-marks (make-hash-table)))

(defun reformat-components (components)
  "A helper function to reformat component data given to ADD-ENTITY."
  (mapcar
   (lambda (component)
     (loop :with name = (car component)
           :for (k . v) :in (alexandria:plist-alist (cdr component))
           :collect (alexandria:make-keyword (format nil "~A/~A" name k)) :into slots
           :collect (gensym (symbol-name k)) :into vars
           :collect v :into values
           :finally (return (list name slots (mapcar #'list vars values)))))
   components))

(defun entity-exists-p (id)
  (if (gethash id (ecs-entities *ecs*)) t nil))

(defmacro add-entity (prototype &body components)
  "A helper macro to create an entity."
  (let ((parts (reformat-components components)))
    `(let (,@(mapcan #'third parts))
       (%add-entity
        ',prototype
        (list ,@(mapcan
                 (lambda (part)
                   (list `(list ',(first part)
                                ,@(mapcan
                                   (lambda (sym bind)
                                     `(,sym ,(first bind)))
                                   (second part) (third part)))))
                 parts))))))

(defun all-entities ()
  "Get a list of all defined entities."
  (alexandria:hash-table-keys (ecs-entities *ecs*)))

(defun entity-components (id)
  "Get a list of all components of the specified entity."
  (components (gethash id (ecs-entities *ecs*))))

(defun (setf entity-components) (value id)
  "Assign a list of components to the specified entity."
  (setf (components (gethash id (ecs-entities *ecs*))) value))

(defun entity-tags (id)
  "Get a list of all tags for the specified entity."
  (tags (gethash id (ecs-entities *ecs*))))

(defun component-attrs (id comp)
  (loop for field in (component-fields comp)
        collect (funcall field id)))

(defun (setf entity-tags) (value id)
  "Assign a list of all tags for the specified entity."
  (setf (tags (gethash id (ecs-entities *ecs*))) value))

(defun all-tags-p (id &rest tags)
  "Check if an entity has all of the specified tags."
  (all tags (entity-tags id)))

(defun some-tags-p (id &rest tags)
  "Check if an entity has some of the specified tags."
  (any tags (entity-tags id)))

(defun no-tags-p (id &rest tags)
  "Check if an entity has no specified tags."
  (not (any tags (entity-tags id))))

(defun add-tags (id &rest tags)
  "Add some tags to the specified entity."
  (symbol-macrolet ((all (entity-tags id)))
    (map nil (lambda (x) (pushnew x all)) tags)))

(defun remove-tags (id &rest tags)
  "Remove some tags from the specified entity."
  (symbol-macrolet ((all (entity-tags id)))
    (map nil (lambda (x) (deletef all x)) tags)))

(defun flag (id &rest flags)
  "Turns on a temporary flag that lasts until the next iteration of the system it was toggled in")

(defun entity-flags (id)
  "Get a list of all flags for the specified entity."
  (flags (gethash id (ecs-entities *ecs*))))

(defun entity-marks (id)
  "Get a list of all flags for the specified entity."
  (marks (gethash id (ecs-entities *ecs*))))

(defun entity-blame-marks (id)
  "Get a list of systems that caused marks to change"
  (blame-marks (gethash id (ecs-entities *ecs*))))

(defun entity-blame-flags (id)
  "Get a list of systems that caused flags to change"
  (blame-flags (gethash id (ecs-entities *ecs*))))

;; (defun mark-flags (id &rest flags)
;;   (symbol-macrolet ((eflags (flags id)))
;;     (map nil (lambda (x) (setf (gethash x flags) t)) eflags)))

(defun all-flags-p (id &rest flags)
  (symbol-macrolet ((eflags (flags (gethash id (ecs-entities *ecs*)))))
    (every (lambda (x) (gethash x eflags)) flags)))

(defun some-flags-p (id &rest flags)
  (symbol-macrolet ((eflags (flags (gethash id (ecs-entities *ecs*)))))
    (some (lambda (x) (gethash x eflags)) flags)))

(defun no-flags-p (id &rest flags)
  (symbol-macrolet ((eflags (flags (gethash id (ecs-entities *ecs*)))))
    (notany (lambda (x) (gethash x eflags)) flags)))

(defun unmark-flags (id)
  (symbol-macrolet ((eflags (flags (gethash id (ecs-entities *ecs*)))))
    (loop for x being the hash-keys of eflags
          do (setf (gethash x eflags) nil))))

(defun flag-blame (id flag)
  (let ((x (gethash id (ecs-entities *ecs*))))
    (if x
        (gethash flag (blame x)))))

(defun all-marks-p (id &rest marks)
  (symbol-macrolet ((emarks (marks (gethash id (ecs-entities *ecs*)))))
    (every (lambda (x) (gethash x emarks)) marks)))

(defun some-marks-p (id &rest marks)
  (symbol-macrolet ((emarks (marks (gethash id (ecs-entities *ecs*)))))
    (some (lambda (x) (gethash x emarks)) marks)))

(defun no-marks-p (id &rest marks)
  (symbol-macrolet ((emarks (marks (gethash id (ecs-entities *ecs*)))))
    (notany (lambda (x) (gethash x emarks)) marks)))

(defun unmark-marks (id)
  (symbol-macrolet ((emarks (marks (gethash id (ecs-entities *ecs*)))))
    (loop for x being the hash-keys of emarks
          do (setf (gethash x emarks) nil))))

(defun mark-blame (id flag)
  (let ((x (gethash id (ecs-entities *ecs*))))
    (if x
        (gethash flag (blame-mark x)))))

(defun entity-attrs (id)
  "Get a list of the specified entity's attributes."
  (attributes (gethash id (ecs-entities *ecs*))))

(defun (setf entity-attrs) (value id)
  "Assign a list of attributes to the specified entity."
  (setf (attributes (gethash id (ecs-entities *ecs*))) value))

(defun entity-attr (id field &optional fallback)
  "Get the value of one of an entity's attributes."
  (getf (entity-attrs id) field fallback))

(defun (setf entity-attr) (value id field)
  "Set the value of one of an entity's attributes."
  (setf (getf (entity-attrs id) field) value))

(defun remove-entity-attr (id field)
  "Remove one of an entity's attributes."
  (delete-from-plistf (entity-attrs id) field))

(defun copy-prototype (from to)
  (when from
    (setf (entity-components to) (copy-seq (entity-components from))
          (entity-attrs to) (copy-seq (entity-attrs from)))))

(defun %add-entity (prototype components)
  "Internal function for creating a new entity."
  (let ((id (new-id)))
    (setf (gethash id (ecs-entities *ecs*)) (make-entity))
    (copy-prototype prototype id)
    (loop :for (name . attrs) :in components
          :do (add-component id name attrs))
    ;;(cache-entity id)
    (cache-system-entities)
    id))

(defun remove-entity (id)
  "Stages an entity to be removed."
  (push id *graveyard*))

(defun remove-all-entities ()
  "Stages all entities to be removed."
  (setf *graveyard* 'remove-all))

(defun delete-entity (id)
  "Remove an entity.
   DO NOT DO THIS IN A SYSTEM WHILE IT IS PROCESSING.
   It will screw over iteration. Use remove-entity instead."
  (remhash id (ecs-entities *ecs*))
  (cache-system-entities))

(defun delete-all-entities ()
  "Removes all entities."
  (let ((es (all-entities)))
    (loop for id in es
          do (remhash id (ecs-entities *ecs*)))
    (cache-system-entities)))

(defun cremate-dead-entities ()
  (when (eq *graveyard* 'remove-all)
    (delete-all-entities)
    (setf *graveyard* (list)))
  (loop :for e :in *graveyard*
        :do (delete-entity e)))
