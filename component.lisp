(in-package :cl-ecs)

(defstruct (component (:conc-name nil))
  fields)

;; the let form should expand into a let* so that it can access the previous values
(defmacro defcomponent (name &body fields)
  "Define a new component with the specified fields.
Also defines accessors for each field to be used on an entity."
  (let ((field-list (mapcar (lambda (x)
                              (intern (format nil "~A/~A" name
                                              (if (symbolp x) x (car x)))))
                            fields))
        (field-vals (mapcar (lambda (x)
                              (if (symbolp x) nil
                                  (cadr x)))
                            fields)))
    `(progn
       (setf (gethash ',name (ecs-components *ecs*))
             (make-component :fields ',field-list))
       ,@(loop :for field :in field-list
               :for defaults :in field-vals
               :for key = (make-keyword field)
               :collect `(defun ,field (id)
                           (entity-attr id ,key ,defaults))
               :collect `(defun (setf ,field) (value id)
                           (setf (entity-attr id ,key) value)))
       ',name)))

(defun all-components ()
  "Get a list of all defined components."
  (hash-table-keys (ecs-components *ecs*)))

(defun component-fields (component)
  "Get a list of fields for the specified component."
  (fields (gethash component (ecs-components *ecs*))))

(defun (setf component-fields) (value component)
  "Assign a list of fields to the specified component."
  (setf (fields (gethash component (ecs-components *ecs*))) value))

(defun add-component (id component attrs)
  (declare (dynamic-extent attrs))
  "Add a new component to the specified entity."
  (when (member component (all-components))
    (pushnew component (entity-components id)))
  (cache-system-entities)
  (loop :for (field . value) :in (plist-alist attrs)
        :for fields = (mapcar #'make-keyword (component-fields component))
        :when (member field fields)
          :do (setf (entity-attr id field) value)))

(defun remove-component (id component)
  "Remove a component from the specified entity."
  (deletef (entity-components id) component)
  (cache-system-entities)
  (loop :for field :in (mapcar #'make-keyword (component-fields component))
        :when (entity-attr id field)
          :do (remove-entity-attr id field)))
