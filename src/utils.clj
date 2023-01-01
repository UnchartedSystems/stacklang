(ns utils)

;; NOTE: A handful of pure functions (+ 1 macro) used by methods.clj to
;; assist with interpretation and generation of clean output syntax.

(defmacro let-> [val name bindings & forms]
  `(let ~(vec (concat [name val] bindings))
     (-> ~name ~@forms)))

(defn is-var? [symbol]
  (let [s (name symbol)]
    (cond (and (= \! (first s)) (= \+ (last s))) ':bind-var
          (= \! (first s)) ':push-var
          :else symbol)))

(defn- pop-args [^clojure.lang.PersistentVector stack ^java.lang.Long arity]
  (loop [counter  arity
         stack    stack
         args     []]
    (if (< counter 1)
      (list stack args)
      (recur (dec counter)
             (pop stack)
             (conj args (peek stack))))))

(defn wrap-op [^clojure.lang.PersistentVector stack op ^Long arity]
  (let [[stack args] (pop-args stack arity)]
    (eval `(conj ~stack (~op ~@args)))))


#_(if (> arity (count stack))
    (throw (AssertionError. (str "pop-args: arity '" arity
                                 "' cannot exceed count of stack '" stack "'"))))

;; NOTE: Was used for runtime error checking. I decided the default
;;       stack traces did a good job on popping an empty vector.
