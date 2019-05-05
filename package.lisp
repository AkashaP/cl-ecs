(defpackage #:cl-ecs
  (:use #:cl
        #:alexandria)
  (:export #:init-ecs
           #:defcomponent
           #:add-component
           #:remove-component
           #:entity
           #:add-entity
           #:remove-entity
           #:all-entities
           #:defsys
           #:do-system
           #:cycle-systems))

(in-package :cl-ecs)
