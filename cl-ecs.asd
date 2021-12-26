(defsystem #:cl-ecs
  :name "CL-ECS"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :version "0.1"
  :license "MIT"
  :description "An implementation of the Entity-Component-System pattern mostly used in game development."
  :depends-on (#:alexandria
               #:graph
               #:metatilities)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "ecs")
               (:file "entity")
               (:file "component") 
               (:file "schedule")
               (:file "system")))
