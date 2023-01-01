(ns methods
  (:require [utils :as u]))


(defmulti interpret #(type (first (:forms %))))

(defmulti interpret-symbol #(u/is-var? (first (:forms %))))

(defmulti interpret-list #(first (first (:forms %))))

;; REVIEW: 'iter-forms' iterates over stacklang forms and sends them
;;          to the 'interpret' dispatch function, which routes that form
;;          through relevant dispatch functions to the appropriate method.

;;          The return vector includes a bool instructing whether iteration
;;          stops, and the resulting clojure code corresponding to the
;;          dispatched stacklang form, which is aaccumulated and returned
;;          to 'core/defstackfn'

(defn iter-forms
  [{forms :forms :as all}]
  (loop [code  `()
         [stop? expr] [false nil]
         forms forms]
    (if (or stop? (empty? forms))
      (reverse (conj code expr))
      (recur (if (nil? expr) code (conj code expr))
             (interpret (assoc all :forms forms))
             (rest forms)))))


;; Symbols
(defmethod interpret clojure.lang.Symbol [data]
  (interpret-symbol data))

(defmethod interpret clojure.lang.PersistentList [data]
  (interpret-list data))

(defmethod interpret :default [{[form] :forms}]
  [false `(conj ~form)])


(defmethod interpret-symbol '<pop> [_]
  [false '(pop)])

;; REVIEW: Note that iter-forms is used recursively to interpreted nested
;;         stacklang forms or interpret further forms under new variable
;;         bindings

(defmethod interpret-symbol :bind-var [{vars :vars [sym & forms] :forms :as all}]
  (let [var-str (reduce str "" (butlast (name sym)))
        var (read-string var-str)
        vars (conj vars var)]
    [true `(u/let-> ~'stack [~var (peek ~'stack)]
                    ~@`~(if forms
                          (iter-forms (assoc all :vars vars :forms forms))
                          '(identity)))]))

(defmethod interpret-symbol :push-var [{vars :vars [sym] :forms}]
  (if  (contains? vars sym)
    [false `(conj ~sym)]
    (throw (AssertionError. (str "Symbol " (name sym) " does not match existing vars: " vars)))))

(defmethod interpret-symbol :default [{[sym] :forms}]
    (throw (AssertionError. (str (name sym) " is not a valid stack operation or variable."))))

#_(defmethod interpret-symbol :default [{[sym & forms] :forms}]
  [false `(conj ~sym)])


;; Lists
(defmethod interpret-list 'invoke> [{[li] :forms}]
  [false `(u/wrap-op ~@(rest li))])

(defmethod interpret-list 'if> [{[li] :forms :as all}]
  (let [[t-branch _ f-branch] (partition-by #(= 'else> %) (rest li))]
    [false `(u/let-> ~'stack [~'element (peek ~'stack)
                              ~'stack (pop ~'stack)]
                     (as-> ~'stack
                         (if ~'element
                           (-> ~'stack ~@`~(iter-forms (assoc all :forms t-branch)))
                           (-> ~'stack ~@`~(if f-branch (iter-forms (assoc all :forms f-branch)) '(identity))))))]))

(defmethod interpret-list :default [{[li] :forms}]
  [false `(conj ~li)])

#_(defmethod interpret-list :default [{[li] :forms}]
    (throw (AssertionError. (str l " is not a valid stack list operation."))))
