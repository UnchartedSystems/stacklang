(ns core
  (:require [methods :as m]))

;; REVIEW: 'defstackfn' creates our stacklang function base and threads
;;          an empty vector into the accumulated clojure code generated
;;          by 'methods/iter-forms'.

;; NOTE: stacklang vars and forms are passed between interpreter code as
;;       part of an 'environment map' that can be extended by user defined
;;       methods. 'user.clj' gives guided examples of user defined extension.

(defmacro defstackfn [f args & forms]
  `(defn ~f ~args
     (-> [] ~@(m/iter-forms {:vars (into #{} args)
                             :forms forms}))))
