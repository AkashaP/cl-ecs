(defpackage #:cl-ecs
  (:use #:cl
        #:alexandria)
  (:export #:init-ecs
           #:defcomponent
           #:add-component
           #:remove-component
           #:all-components
           #:component-fields
           #:entity
           #:add-entity
           #:remove-entity
           #:all-entities
           #:remove-all-entities
           #:entity-components
           #:entity-tags
           #:all-tags-p
           #:some-tags-p
           #:add-tags
           #:remove-tags
           #:entity-attrs
           #:entity-attr
           #:defsys
           #:do-system
           #:cycle-systems))

(in-package :cl-ecs)
