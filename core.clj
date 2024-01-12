(ns ru.nsu.fit.smp.object_model.core
  ;;(:use ru.nsu.fit.smp.object_model.base_model)
  (:use ru.nsu.fit.smp.object_model.model)
  )


(defn -main
  []
  ;; Base model demo

  ;; Create simple "diamond" hierarchy
  ;(def-class :Animal
  ;           (:fields (:name)))
  ;(def-class :Cat
  ;           (:super :Animal)
  ;           (:fields (:breed)))
  ;(def-class :Dog
  ;           (:super :Animal)
  ;           (:fields (:weight)))
  ;(def-class :CatDog
  ;           (:super [:Cat :Dog])
  ;           (:fields (:age)))
  ;(println "Default hierarchy: " @class-hierarchy)
  ;
  ;; Demonstration of working with objects
  ;(println "\nOBJECT DEMO\n")
  ;(let [cat (create-obj :Cat :breed "Siamese" :name "Murka")]
  ;  (println "Cat object: " cat)
  ;  (println "Cat breed: " (getf cat :breed))
  ;  (setf! cat :breed "Sphinx")
  ;  (println "Cat changed breed: " (getf cat :breed))
  ;  (println "Cat object: " cat)
  ;  (setf! cat :name "Vaska")
  ;  (println "Cat changed name (superclass field): " (getf cat :name))
  ;  )
  ;
  ;; Demonstration of working with functions
  ;(println "\n --------------------------------- ")
  ;(println "\nFUNCTION DEMO\n")
  ;
  ;; Create query - returns value, change nothing
  ;(def-query speak)
  ;(def-method speak :Animal [obj]
  ;            (str "Animal say`s: " (getf obj :name)))
  ;(def-method speak :Cat [obj]
  ;            (str "Cat say`s: " (getf obj :name)))
  ;; Method can be redefined
  ;(def-method speak :Cat [obj]
  ;            (str "Cat say`s: " (getf obj :name)))
  ;; Create command - returns nothing, change object
  ;(def-command rename)
  ;(def-method rename :Animal [obj new-name]
  ;            (setf! obj :name new-name))
  ;(let [cat (create-obj :Cat :breed "Siamese" :name "Murka")
  ;      dog (create-obj :Dog :weight 15 :name "Sharik")
  ;      ]
  ;  ; Cat use its own "speak" method instead of Animals`s method
  ;  (println (speak cat))
  ;  ; Dog use Animal`s "speak" method
  ;  (println (speak dog))
  ;  (rename cat "Vaska")
  ;  (println "After renaming:")
  ;  (println (speak cat))
  ;  )
  ;
  ;; Multi-inheritance demonstration
  ;;
  ;; In base task said that no need to pay attention to the problem of multiple inheritance
  ;(println "\n --------------------------------- ")
  ;(println "\nMULTI-INHERITANCE DEMO\n")
  ;
  ;(def-method speak :Dog [obj]
  ;            (str "Dog say`s: " (getf obj :name)))
  ;; Animal, Cat and Dog have methods "speak", but Cat defined earlier in CatDog superclasses
  ;(let [cat-dog (create-obj :CatDog :breed "Siamese" :name "Kotopes" :weight 10)]
  ;  (println (speak cat-dog))
  ;  )

  ;; Extended model demo

  ; Create simple "diamond" hierarchy
  (def-class :Animal
             (:fields (:name)))
  (def-class :Cat
             (:super :Animal)
             (:fields (:breed)))
  (def-class :Dog
             (:super :Animal)
             (:fields (:weight)))
  (def-class :CatDog
             (:super [:Cat :Dog])
             (:fields (:age)))
  (println "Default hierarchy: " @class-hierarchy)

  ; Demonstration of working with objects
  (println "\nOBJECT DEMO\n")
  (let [cat (create-obj :Cat :breed "Siamese" :name "Murka")]
    (println "Cat object: " cat)
    (println "Cat breed: " (getf cat :breed))
    (setf! cat :breed "Sphinx")
    (println "Cat changed breed: " (getf cat :breed))
    (println "Cat object: " cat)
    (setf! cat :name "Vaska")
    (println "Cat changed name (superclass field): " (getf cat :name))
    )

  ; Demonstration of working with functions
  (println "\n --------------------------------- ")
  (println "\nFUNCTION DEMO\n")

  ; Create query - returns value, change nothing
  (def-query speak)
  (def-method speak :Animal [obj]
              (str "Animal say`s: " (getf obj :name)))
  (def-method speak :Cat [obj]
              (str "Cat say`s: " (getf obj :name)))
  ; Method can be redefined
  (def-method speak :Cat [obj]
              (str "Cat say`s: " (getf obj :name)))
  ; Create command - returns nothing, change object
  (def-command rename)
  (def-method rename :Animal [obj new-name]
              (setf! obj :name new-name))
  (let [cat (create-obj :Cat :breed "Siamese" :name "Murka")
        dog (create-obj :Dog :weight 15 :name "Sharik")
        ]
    ; Cat use its own "speak" method instead of Animals`s method
    (println (speak cat))
    ; Dog use Animal`s "speak" method
    (println (speak dog))
    (rename cat "Vaska")
    (println "After renaming:")
    (println (speak cat))
    )

  ; Multi-inheritance demonstration
  ;
  ; In base task said that no need to pay attention to the problem of multiple inheritance
  (println "\n --------------------------------- ")
  (println "\nMULTI-INHERITANCE DEMO\n")

  (def-method speak :Dog [obj]
              (str "Dog say`s: " (getf obj :name)))
  ; Animal, Cat and Dog have methods "speak", but Cat defined earlier in CatDog superclasses
  (let [cat-dog (create-obj :CatDog :breed "Siamese" :name "Kotopes" :weight 10)]
    (println (speak cat-dog))
    )

  ; Auxiliary methods demonstration
  (println "\n --------------------------------- ")
  (println "\nAUXILIARY METHODS DEMO\n")
  (def-method speak :Animal [obj]
              (println "Is in Animal")
              ;(call-next-method)
              (str "Animal say`s: " (getf obj :name)))
  (def-method speak :Cat :before [obj]
              ;(call-next-method)
              (println "It is 'before' function from Cat")
              (str "Cat say`s: " (getf obj :name)))
  (def-method speak :Dog :after [obj]
              (call-next-method)
              (println "It is 'after' function from Dog")
              (str "Dog say`s: " (getf obj :name)))
  (def-method speak :CatDog [obj]
              (call-next-method)
              (println "It is in CatDog")
              (str "CatDog say`s: " (getf obj :name)))
  (let [cat-dog (create-obj :CatDog :breed "Siamese" :name "Kotopes" :weight 10)
        dog (create-obj :Dog :weight 15 :name "Sharik")]
     (println (speak cat-dog))
     (println "---------------")
     (println (speak dog))
   )
  )
