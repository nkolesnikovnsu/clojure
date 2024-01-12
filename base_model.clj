(ns ru.nsu.fit.smp.object_model.base_model)

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
  ;; Simple dispatcher - use BFS and return first suitable method
  [vtable eff-type]
  (let [suitable-methods (filter #(some? (nth % 1))
                                 (map #(list % (vtable %))
                                      (get-bfs-order eff-type)))]
    (if (empty? suitable-methods)
      (throw (Exception. "Have no suitable method."))
      (first suitable-methods)
      )))

;; Stub for super
(def ^:dynamic super nil)

(defn perform-effective-command
  ;; Find and execute command
  ;; Super class is a first defined parent
  [vtable eff-type obj & args]
  (let [[d-type eff-fn] (dispatch vtable eff-type)
        d-super-type (first (super-class d-type))]
    (binding [super (partial
                      perform-effective-command
                      vtable d-super-type obj)]
      (dosync
        (apply eff-fn (cons obj args))))))

(defn perform-effective-query
  ;; Find and execute command
  ;; Super class is a first defined parent
  [vtable eff-type obj & args]
  (let [[d-type eff-fn] (dispatch vtable eff-type)
        d-super-type (first (super-class d-type))]
    (binding [super (partial
                      perform-effective-command
                      vtable d-super-type obj)]
      (apply eff-fn (cons obj args)))))

(defmacro def-command
  ;; From lecture. Call or register command - method, which returns nothing and change object
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
                  #(assoc % (first obj#) (second obj#)))))
       )))

(defmacro def-query
  ;; Function with state, from lecture. Call or register query - method, which returns value and change nothing
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
                  #(assoc % (first obj#) (second obj#)))))
       )))

(defmacro def-method [command-name class argv & body]
  ;; Define method specification and implementation
  `(~command-name [~class (fn ~argv ~@body)]))
