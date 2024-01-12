(ns ru.nsu.fit.smp.object_model.model)

;; Init storage for class hierarchy

(def class-hierarchy (ref {}))

;; Place class Object on the top of hierarchy
(dosync
  (alter
    class-hierarchy
    (fn [ch]
      (assoc
        ch :Object
           {::type   :Object
            ::super  nil
            ::fields #{}}))))

;; Init storage for methods names

(def defined-methods-names (ref []))

;; Allowed methods qualifiers

(def method-qualifiers (ref {:before 0 :after 1 :primary 2}))

;; Class methods

(defmacro def-class
  "Class definition macro. Adds a map containing class information to class hierarchy.
   There is no checking for crossing of attributes and methods"
  [name & sections]
  {:pre (keyword? name)}
  (let [s-map (if (empty? sections)
                {}
                (apply assoc
                       (cons {} (mapcat identity sections))))
        super-class (if (s-map :super)
                      (if (keyword? (s-map :super))
                        [(s-map :super)]
                        (s-map :super)
                        )
                      [:Object]
                      )
        fields (set (s-map :fields))]
    `(dosync
       (alter
         class-hierarchy
         (fn [ch#]
           (assoc
             ch# ~name
                 {::type   ~name
                  ::super  ~super-class
                  ::fields ~fields}))))))

(defn super-class
  "Returns parent class of given class."
  [class]
  {:pre (keyword? class)}
  (if (contains? @class-hierarchy class)
    ((@class-hierarchy class) ::super)
    (throw (Exception. "Unknown class."))))

(defn is-class?
  "Check for input val is a class from hierarchy"
  [class]
  {:pre (keyword? class)}
  (contains? @class-hierarchy class))

(defn has-field?
  "Checks whether class contains field including super classes."
  [class field]
  {:pre [(keyword? class) (keyword? field)]}
  (if (contains? @class-hierarchy class)
    (or (contains? ((@class-hierarchy class) ::fields) field)
        (some #(= true %) (map #(has-field? % field)
                               ((@class-hierarchy class) ::super))))
    (throw (Exception. "Unknown class."))))

;; Object methods

(defn create-obj [class & fields]
  "Create object"
  (let [f-map (apply hash-map fields)
        state (ref {})]
    (dosync
      (doseq [kv f-map]
        (when (has-field? class (first kv))
          (alter state
                 #(assoc % (first kv) (second kv))))))
    {::class  class
     ::fields state}
    ))

(defn getf
  "Get field value"
  [obj field]
  ((obj ::fields) field)
  )

(defn setf!
  "Set field value"
  [obj field val]
  (dosync (alter (obj ::fields) assoc-in [field] val))
  )

;; Methods dispatching

(defn visited?
  "Predicate which returns true if the node v has been visited already, false otherwise."
  [v coll]
  (some #(= % v) coll))

(defn get-bfs-order
  "Traverses a graph in Breadth First Search (BFS). Returns nodes order "
  [eff-type]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY eff-type)
         visited []]
    (if (empty? queue) visited
                       (let [v (peek queue)
                             neighbors (super-class v)
                             not-visited (filter (complement #(visited? % visited)) neighbors)
                             new-queue (apply conj (pop queue) not-visited)
                             ]
                         (if (visited? v visited)
                           (recur new-queue visited)
                           (recur new-queue (conj visited v)))))))

(defn dispatch
  "Dispatcher - use BFS and return lists of methods divided by qualifiers"
  [vtable eff-type]
  (let [ordered-superclasses (get-bfs-order eff-type)
        existing-methods (filter #(some? (nth % 1))
                                 (map #(list % (first (vtable %)) (second (vtable %)))
                                      ordered-superclasses))
        before-methods (filter #(= :before (nth % 2)) existing-methods)
        after-methods (reverse (filter #(= :after (nth % 2)) existing-methods))
        primary-methods (filter #(= :primary (nth % 2)) existing-methods)
        ]
    (if (empty? primary-methods)
      (throw (Exception. "Have no suitable method."))
      (list primary-methods before-methods after-methods)
      )))

;; Stub for call-next-method
(def ^:dynamic call-next-method (fn [] ()))

(defn perform-primary-method
  "Execute primary method and (call-next-method). Works recursively"
  [next-methods high-fn obj & args]
  (if (empty? next-methods)
    (throw (Exception. "Have no suitable next method for 'call-next-method' in " high-fn))
    (let [[d-type eff-fn] (first next-methods)]
      (binding [call-next-method (partial
                                   perform-primary-method
                                   (rest next-methods) eff-fn obj)]
        (apply eff-fn (cons obj args))
        ))))

(defn perform-effective-command
  "Find and execute command"
  [vtable eff-type obj & args]
  (let [[primary-methods before-methods after-methods] (dispatch vtable eff-type)]
      (dosync
        (doseq [before-fn before-methods] (apply (second before-fn) (cons obj args)))
        (apply perform-primary-method (concat (list primary-methods nil obj) args))
        (doseq [after-fn after-methods] (apply (second after-fn) (cons obj args))))))

(defn perform-effective-query
  "Find and execute query"
  [vtable eff-type obj & args]
  (let [[primary-methods before-methods after-methods] (dispatch vtable eff-type)]
      (doseq [before-fn before-methods] (apply (second before-fn) (cons obj args)))
      (let [res (apply perform-primary-method (concat (list primary-methods nil obj) args))]
        (doseq [after-fn after-methods] (apply (second after-fn) (cons obj args)))
        res
        )))

(defmacro def-command
  "From lecture. Call or register command - method, which returns nothing and change object"
  [name]
  `(let [vtable# (ref {})]
     (if (.contains @defined-methods-names '~name)
       (throw (Exception. "Error: method with such name already exists."))
       (dosync (alter defined-methods-names conj '~name))
       )
     (defn ~name [obj# & args#]
       (if (not (is-class? (first obj#)))
         ;; Standard call
         (apply perform-effective-command
                (concat (list @vtable# (obj# ::class) obj#) args#))
         ;; Special call: method registration
         (dosync
           (alter vtable#
                  #(assoc % (first obj#) [(nth obj# 2) (second obj#)]))))
       )))

(defmacro def-query
  "Function with state, from lecture. Call or register query - method, which returns value and change nothing"
  [name]
  `(let [vtable# (ref {})]
     (if (.contains @defined-methods-names '~name)
       (throw (Exception. "Error: method with such name already exists."))
       (dosync (alter defined-methods-names conj '~name))
       )
     (defn ~name [obj# & args#]
       (if (not (is-class? (first obj#)))
         ;; Standard call
         (apply perform-effective-query
                (concat (list @vtable# (obj# ::class) obj#) args#))
         ;; Special call: method registration
         (dosync
           (alter vtable#
                  #(assoc % (first obj#) [(nth obj# 2) (second obj#)]))))
       )))

(defmacro def-method
  "Define method specification and implementation"
  [command-name class & args]
  (let [[qualifier argv & body]
        (if (contains? @method-qualifiers (first args))
          args
          (cons :primary args))]

    `(~command-name [~class ~qualifier (fn (~argv ~@body))])
    )
  )
