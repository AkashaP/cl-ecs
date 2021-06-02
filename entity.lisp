(in-package :cl-ecs)

(defstruct (entity (:conc-name nil))
  components
  attributes
  tags)

(defun reformat-components (components)
  "A helper function to reformat component data given to ADD-ENTITY."
  (mapcar
   (lambda (component)
     (loop :with name = (car component)
           :for (k . v) :in (plist-alist (cdr component))
           :collect (make-keyword (format nil "~A/~A" name k)) :into slots
           :collect (gensym (symbol-name k)) :into vars
           :collect v :into values
           :finally (return (list name slots (mapcar #'list vars values)))))
   components))

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
  (hash-table-keys (ecs-entities *ecs*)))

(defun entity-components (id)
  "Get a list of all components of the specified entity."
  (components (gethash id (ecs-entities *ecs*))))

(defun (setf entity-components) (value id)
  "Assign a list of components to the specified entity."
  (setf (components (gethash id (ecs-entities *ecs*))) value))

(defun entity-tags (id)
  "Get a list of all tags for the specified entity."
  (tags (gethash id (ecs-entities *ecs*))))

(defun (setf entity-tags) (value id)
  "Assign a list of all tags for the specified entity."
  (setf (tags (gethash id (ecs-entities *ecs*))) value))

(defun all-tags-p (id &rest tags)
  "Check if an entity has all of the specified tags."
  (all tags (entity-tags id)))

(defun some-tags-p (id &rest tags)
  "Check if an entity has some of the specified tags."
  (any tags (entity-tags id)))

(defun add-tags (id &rest tags)
  "Add some tags to the specified entity."
  (symbol-macrolet ((all (entity-tags id)))
    (map nil (lambda (x) (pushnew x all)) tags)))

(defun remove-tags (id &rest tags)
  "Remove some tags from the specified entity."
  (symbol-macrolet ((all (entity-tags id)))
    (map nil (lambda (x) (deletef all x)) tags)))

(defun entity-attrs (id)
  "Get a list of the specified entity's attributes."
  (attributes (gethash id (ecs-entities *ecs*))))

(defun (setf entity-attrs) (value id)
  "Assign a list of attributes to the specified entity."
  (setf (attributes (gethash id (ecs-entities *ecs*))) value))

(defun entity-attr (id field)
  "Get the value of one of an entity's attributes."
  (getf (entity-attrs id) field))

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
    (cache-system-entities)
    (loop :for (name . attrs) :in components
          :do (add-component id name attrs))
    id))

(defun remove-entity (id)
  "Remove an entity."
  (remhash id (ecs-entities *ecs*))
  (cache-system-entities))
  
(defun remove-all-entities ()
  "Removes all entities."
  (let ((es (all-entities)))
    (loop for id in es
          do (remhash id (ecs-entities *ecs*)))
    (cache-system-entities)))
