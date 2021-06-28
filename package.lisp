(defpackage #:cl-ecs
  (:nicknames #:ecs)
  (:use #:cl
        #:alexandria)
  (:shadowing-import-from :metatilities #:map-combinations)
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
           #:act-tags
           #:add-tags
           #:remove-tags
           #:entity-attrs
           #:entity-attr
           #:defsys
           #:required-components
           #:system-grouping
           #:system-processing
           #:do-system
           #:cycle-systems))

(in-package :cl-ecs)
