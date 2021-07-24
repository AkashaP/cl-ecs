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
           #:component-attrs
           #:entity
           #:add-entity
           #:remove-entity
           #:all-entities
           #:remove-all-entities
           #:entity-components
           #:entity-tags
           #:entity-flags
           #:entity-marks
           #:entity-blame-flags
           #:entity-blame-marks
           #:all-tags-p
           #:some-tags-p
           #:no-tags-p
           #:act-tags
           #:add-tags
           #:remove-tags
           #:all-flags-p
           #:some-flags-p
           #:no-flags-p
           #:flag-blame
           #:flag
           #:all-marks-p
           #:some-marks-p
           #:no-marks-p
           #:mark
           #:demk
           #:entity-attrs
           #:entity-attr
           #:entity-exists-p
           #:defsys
           #:required-components
           #:system-grouping
           #:system-processing
           #:do-system
           #:cycle-systems
           #:ack
           #:get-ack))

(in-package :cl-ecs)
