(in-package :cl-ecs)

(defstruct (system (:conc-name nil))
  processing
  required
  grouping
  entities
  iteration ;; what kind of iteration function to use on the entities
  runnable) ;; flag when grouping is sufficient to perform iteration

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

(defmacro defsys (name (&rest letargs) &body body)
  "Defines a system.
   When executed, The groupings (the cars of letargs) determines the
   parallel processing of entities who match the cdrs of letargs."
  (let ((entities (gensym)))
    `(labels ((flag (id &rest flags-to-set)
                (symbol-macrolet ((e (gethash id (ecs-entities *ecs*))))
                  (symbol-macrolet ((eflags (flags e)) (eblame (blame-flags e)))
                    (map nil (lambda (x)
                               (setf (gethash x eflags) t)
                               (setf (gethash x eblame) ',name))
                         flags-to-set))))
              (mark (id &rest marks-to-set)
                (symbol-macrolet ((e (gethash id (ecs-entities *ecs*))))
                  (symbol-macrolet ((emarks (marks e)) (eblame (blame-marks e)))
                    (map nil (lambda (x)
                               (setf (gethash x emarks) t)
                               (setf (gethash x eblame) ',name))
                         marks-to-set))))
              (demk (id &rest marks-to-set)
                (symbol-macrolet ((e (gethash id (ecs-entities *ecs*))))
                  (symbol-macrolet ((emarks (marks e)) (eblame (blame-marks e)))
                    (map nil (lambda (x)
                               (setf (gethash x emarks) nil)
                               (setf (gethash x eblame) ',name))
                         marks-to-set)))))
       (declare (ignorable #'flag #'mark #'demk))
       (setf (gethash ',name (ecs-systems *ecs*))
             (make-system :processing t
                          :required ',(mapcar #'cadr letargs)
                          :grouping ',(mapcar #'car letargs)
                          :iteration #'default-process))
       (remove ',name *system-names*)
       (setf *system-names* (append *system-names* '(,name)))
       (cache-system-entities)
       (defmethod %do-entities ((system (eql ',name)) &rest ,entities)
         (block ,name
           (destructuring-bind ,(mapcar #'car letargs) ,entities
             ,@body))))))

(defmacro defsysl (name (&rest letargs) &body body)
  "Defines a system.
   When executed, The letargs are respectively bound to all matching entities.
   The entities may be processed by manually looping over the cars of letargs in any way desired."
  (let ((entities (gensym)))
    `(labels ((flag (id &rest flags-to-set)
                (symbol-macrolet ((e (gethash id (ecs-entities *ecs*))))
                  (symbol-macrolet ((eflags (flags e)) (eblame (blame-flags e)))
                    (map nil (lambda (x)
                               (setf (gethash x eflags) t)
                               (setf (gethash x eblame) ',name))
                         flags-to-set))))
              (mark (id &rest marks-to-set)
                (symbol-macrolet ((e (gethash id (ecs-entities *ecs*))))
                  (symbol-macrolet ((emarks (marks e)) (eblame (blame-marks e)))
                    (map nil (lambda (x)
                               (setf (gethash x emarks) t)
                               (setf (gethash x eblame) ',name))
                         marks-to-set))))
              (demk (id &rest marks-to-set)
                (symbol-macrolet ((e (gethash id (ecs-entities *ecs*))))
                  (symbol-macrolet ((emarks (marks e)) (eblame (blame-marks e)))
                    (map nil (lambda (x)
                               (setf (gethash x emarks) nil)
                               (setf (gethash x eblame) ',name))
                         marks-to-set)))))
       (declare (ignorable #'flag #'mark #'demk))
       (setf (gethash ',name (ecs-systems *ecs*))
             (make-system :processing t
                          :required ',(mapcar #'cadr letargs)
                          :grouping ',(mapcar #'car letargs)
                          :iteration #'manual-process))
       (remove ',name *system-names*)
       (setf *system-names* (append *system-names* '(,name)))
       (cache-system-entities)
       (defmethod %do-entities ((system (eql ',name)) ,@(mapcar #'car letargs))
         (block ,name
           ,@body))))
  `(progn
     (defsys ,name ,letargs ,@body)
     (setf (iteration (gethash ',name (ecs-systems *ecs*))) #'manual-process)))

;;TODO verify letargs correctness
;; (defmacro defsys (name (&rest letargs) (&key iteration-function) &body body)
;;   (let ((entities (gensym)))
;;     `(labels ((flag (id &rest flags-to-set)
;;                 (symbol-macrolet ((e (gethash id (ecs-entities *ecs*))))
;;                   (symbol-macrolet ((eflags (flags e)) (eblame (blame-flags e)))
;;                     (map nil (lambda (x)
;;                                (setf (gethash x eflags) t)
;;                                (setf (gethash x eblame) ',name))
;;                          flags-to-set))))
;;               (mark (id &rest marks-to-set)
;;                 (symbol-macrolet ((e (gethash id (ecs-entities *ecs*))))
;;                   (symbol-macrolet ((emarks (marks e)) (eblame (blame-marks e)))
;;                     (map nil (lambda (x)
;;                                (setf (gethash x emarks) t)
;;                                (setf (gethash x eblame) ',name))
;;                          marks-to-set))))
;;               (demk (id &rest marks-to-set)
;;                 (symbol-macrolet ((e (gethash id (ecs-entities *ecs*))))
;;                   (symbol-macrolet ((emarks (marks e)) (eblame (blame-marks e)))
;;                     (map nil (lambda (x)
;;                                (setf (gethash x emarks) nil)
;;                                (setf (gethash x eblame) ',name))
;;                          marks-to-set)))))
;;        (setf (gethash ',name (ecs-systems *ecs*))
;;              (make-system :processing t
;;                           :required ',(mapcar #'cadr letargs)
;;                           :grouping ',(mapcar #'car letargs)))
;;        (setf *system-names* (append *system-names* '(,name)))
;;        (cache-system-entities)
;;        (defmethod %do-entities ((system (eql ',name)) &rest ,entities)
;;          (block ,name
;;            (destructuring-bind ,(mapcar #'car letargs) ,entities
;;              ,@body))))))

(defun mark (id &rest marks-to-set)
  "Turns on a mark"
  (symbol-macrolet ((e (gethash id (ecs-entities *ecs*))))
    (symbol-macrolet ((emarks (marks e)) (eblame (blame-marks e)))
      (map nil (lambda (x)
                 (setf (gethash x emarks) t)
                 (setf (gethash x eblame) nil))
           marks-to-set))))

(defun demk (id &rest marks-to-set)
  "Turns off a mark"
  (symbol-macrolet ((e (gethash id (ecs-entities *ecs*))))
    (symbol-macrolet ((emarks (marks e)) (eblame (blame-marks e)))
      (map nil (lambda (x)
                 (setf (gethash x emarks) nil)
                 (setf (gethash x eblame) nil))
           marks-to-set))))

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
  ;;(hash-table-keys (ecs-systems *ecs*))
  *system-names*
  )

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

(defun system-runnable (system)
  "Asks whether a system is runnable.
   This only affects (do-system [system]) calls."
  (multiple-value-bind (sys b)
      (gethash system (ecs-systems *ecs*))
    (when b
      (runnable sys))))

(defun system-iteration (system)
  "Asks whether a system is processing.
   This only affects (do-system [system]) calls."
  (multiple-value-bind (sys b)
      (gethash system (ecs-systems *ecs*))
    (when b
      (iteration sys))))

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
                    (and (listp r)
                         (all (remove-if-not #'symbolp r) c)
                         (every (lambda (g)
                                  (case (car g)
                                    (:not (not (any (cdr g) c)))
                                    (:any (any (cdr g) c))
                                    (:all (all (cdr g) c))))
                                (remove-if-not #'listp r))))
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
        :do (let ((new-entities (collect-system-entities system)))
              (setf (system-entities system)
                    new-entities)
              (setf (runnable (gethash system (ecs-systems *ecs*)))
                    (every #'identity new-entities)))))

(defun duplicatesp (the-list &key (test #'eq))
  (loop named check
        for x in the-list
        for i from 0
        do (loop for y in the-list
                 for j from 0
                 if (and (not (eq i j))
                         (funcall test x y))
                   do (return-from check t))
        finally (return-from check nil)))

(defun default-process (system system-entities)
  (let ((result))
    (when (every #'identity system-entities)
      (apply #'metatilities:map-combinations
             (lambda (&rest x)
               ;; Prevent entities from comparing themselves if they match multiple families
               ;; by simply skipping them
               (when (not (duplicatesp x))
                 (setq result (apply #'%do-entities system x))))
             system-entities))
    result))

(defun manual-process (system system-entities)
  (apply #'%do-entities system system-entities))

(defmethod do-system (system)
  "Execute the specified system."
  (when (system-processing system)
    ;; Reset flags that were set by this system previously
    (loop :for g :in (system-entities system)
          :do (loop :for e-num :in g
                    :do (loop :for flag being the hash-keys of (blame-flags (gethash e-num (ecs-entities *ecs*)))
                                :using (hash-value sys)
                              :if (eq sys system)
                                :do ;;(progn
                                    (setf (gethash flag (flags (gethash e-num (ecs-entities *ecs*)))) nil))))
    (let ((result (funcall (system-iteration system) system (system-entities system))))
      ;; delete entities at the end of each system processing
      (cremate-dead-entities)
      result)))

;; (defmethod do-system (system)
;;   "Execute the specified system. The system definition's grouping determines
;; parallel processing of entities."
;;   (when (system-processing system)
;;     (let ((result))
;;       (loop :for g :in (system-entities system)
;;             :do (loop :for e-num :in g
;;                       :do (loop :for flag being the hash-keys of (blame-flags (gethash e-num (ecs-entities *ecs*)))
;;                                   :using (hash-value sys)
;;                                 :if (eq sys system)
;;                                   :do ;;(progn
;;                                       (setf (gethash flag (flags (gethash e-num (ecs-entities *ecs*)))) nil))))

;;       (when (every #'identity (system-entities system))
;;         (apply #'metatilities:map-combinations
;;                (lambda (&rest x)
;;                  ;; Prevent entities from comparing themselves if they match multiple families
;;                  (when (not (duplicatesp x))
;;                    (setf result (apply #'%do-entities system x))))
;;                (system-entities system)))
;;       ;; delete entities at the end of each system processing
;;       (cremate-dead-entities)
;;       result)))

(defun cycle-systems ()
  "Cycle through all defined systems."
  (dolist (system (all-systems))
    (do-system system))
  (cremate-dead-entities))

(defmacro do-cycle-systems ((sys-name) &body body)
  "Cycle through all defined systems with custom logic before doing each system."
  `(progn
     (dolist (,sys-name (all-systems))
       ,@body
       (do-system ,sys-name))
     (cremate-dead-entities)))

