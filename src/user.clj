(ns user
  (:require [core :as c]
            [methods :as m]))

;; NOTE: this page shows the extensibility of the language through examples.
(set! *warn-on-reflection* true)

;; REVIEW: The original example in the spec sheet:

(c/defstackfn example [!a !b !c]
  !a !b
  (invoke> + 2)
  !v1+ !c !c <pop> 2
  (invoke> * 2)
  !v2+
  (invoke> = 2)
  (if> !v1 !v2
       (invoke> - 2)
       else>
       "false!!"
       (invoke> println 1)
       <pop> !v1 !v2
       (invoke> * 2)))

(example 1 2 4)
;; => [24]


;; NOTE: The spec sheet doesn't explicitly say whether stackfns should
;;       return the top value or the full contents of the stack. Let's
;;       extend Stacklang with a <peek!> operator.

(defmethod m/interpret-symbol '<peek!> [_]
  [false '(peek)])

(c/defstackfn example-peek [!a !b !c]
  !a !b
  (invoke> + 2)
  !v1+ !c !c <pop> 2
  (invoke> * 2)
  !v2+
  (invoke> = 2)
  (if> !v1 !v2
       (invoke> - 2)
       else>
       "false!!"
       (invoke> println 1)
       <pop> !v1 !v2
       (invoke> * 2))
  <peek!>)

(example-peek 1 2 4)
;; => 24


;; REVIEW: <peek!> allows stacklang to cleanly support recursion.

(c/defstackfn countdown [!t]
  !t (invoke> println 1)
  <pop> 0 !t
  (invoke> <= 2)
  (if> "Liftoff!" <peek!>
       else> !t (invoke> dec 1) (invoke> countdown 1) <peek!>))

(countdown 10)
;; => "Liftoff!"


;; REVIEW: Stacklang variables can be used as arguments to stack operations.

(c/defstackfn higher-order [!f !arity !a1 !a2 !a3]
  !a1 !a2 !a3 (invoke> !f !arity) <peek!>)

(higher-order + 3 32 19 18)
;; => 69


;; REVIEW: If we take a look at this score evaluator we can see
;;         another opportunity for custom extension of Stacklang

(c/defstackfn score-eval [!scorea !scoreb]
  !scoreb !scorea (invoke> - 2) !scoreb (invoke> <= 2)
  (if> "Skunked!!" else> !scoreb !scorea (invoke> > 2)
       (if> "Win!" else> !scoreb !scorea (invoke> = 2)
            (if> "Tie." else> "Lose...")))
  <peek!>)

(score-eval 6 3)
;; => "Skunked!!"


;; NOTE: The need to repeatedly push values onto the stack
;;       to evaluate them makes for clunky options.
;;       We have options on how to express this more succinctly

;; NOTE: If we are willing to cheat a little on stack semantics
;;       we can create a cond statement.

(defmethod m/interpret-list 'cond> [{[li] :forms :as all}]
  (let [forms (rest li)
        lists (map #(m/iter-forms (assoc all :forms %)) forms)
        preds (map #(concat % (list peek)) (take-nth 2 lists))
        exprs (take-nth 2 (rest lists))
        clauses (map #(conj % 'stack '->) (interleave preds exprs))]
    [false `(as-> ~'stack (cond ~@clauses))]))

#_:clj-kondo/ignore
(c/defstackfn score-cond [!scorea !scoreb]
  !scoreb !scorea
  (cond> ((invoke> - 2) !scoreb (invoke> <= 2)) ("Skunked!! :D")
         ((invoke> > 2))  ("Win!")
         ((invoke> = 2))  ("Tie.")
         (true)           ("Lose... :("))
  <peek!>)

(score-cond 6 3)
;; => "Skunked! :D"


;; NOTE: The problem of repetitive chains of actions generally can
;;       be improved on if we use a custom extension to write state
;;       to the  map that gets pased recursively down during the
;;       iteration and interpretation of forms.

(defmethod m/interpret-list 'record> [{[li & forms] :forms :as all}]
  (let [[kw & record] (rest li)]
    [true `(-> ~@`~(if forms
                     (m/iter-forms (assoc all :forms forms kw record))
                     '(identity)))]))

(defmethod m/interpret-list 'replay> [{[[_ kw cycles]] :forms :as all}]
  (let [record (kw all)
        elements (count record)]
    [false `(-> ~@(m/iter-forms (assoc all :forms (take (* cycles elements)
                                                        (cycle record)))))]))


;; REVIEW:  Here we use this primitive Stacklang 'macro' to
;;          repeatedly exponentiate an input.

(c/defstackfn pow-10 [!x]
  1 (record> :exp !x (invoke> * 2))
  (replay> :exp 10)
  <peek!>)

(pow-10 2)
;; => [1024]

;; NOTE: This extension exposes a limitation of Stacklangs design:
;;       Iterating and interpreting forms into clojure code happens at
;;       compile time, when the value of stacklang variables have not
;;       been determined, I experimented with an extension to  allow
;;       interpretation to happen at runtime to allow for variable args
;;       to 'macros', but more systemic changes to Stacklang are needed
;;       for this kind of evaluation to be enabled in an elegant way.

;; REVIEW: We can still reduce repitition with functional composition
;;         and recursion.

(c/defstackfn pow-iter [!val !x !y]
  0 !y (invoke> dec 1) !y+ (invoke> >= 2)
  (if> !y !x !val !x (invoke> * 2) (invoke> pow-iter 3)
       else> !val)
  <peek!>)

(c/defstackfn pow [!x !y]
  !y !x 1 (invoke> pow-iter 3) <peek!>)

(pow 2 5)
;; => 32


;; Extensibility aside this is a godawful language to read.
;; Reading it feels like trying to find the "middle" between the values
;; added to the stack and the functions collapsing those values, and then
;; working outwards as if you were reading nested polish notation.

;; REVIEW: The pythagorean theorem using Newton's square root method

(c/defstackfn improve-sqrt [!x !guess]
  2 !guess !guess !x
  (invoke> / 2) (invoke> + 2)
  (invoke> / 2) <peek!>)

(c/defstackfn approx-sqrt [!x !guess]
  0.00000000001 !guess !guess !x        ; absurd precision for newton's method
  (invoke> / 2) (invoke> - 2)
  (invoke> abs 1) (invoke> < 2)
  <peek!>)

(c/defstackfn sqrt-iter [!x !guess]
  !guess !x (invoke> approx-sqrt 2)
  (if> !guess
    else> !guess !x (invoke> improve-sqrt 2) !x (invoke> sqrt-iter 2))
  <peek!>)

(c/defstackfn sqrt [!x]
  1.0 !x (invoke> sqrt-iter 2) <peek!>)

(c/defstackfn pythagorean [!a !b]
  !a !a (invoke> * 2)
  !b !b (invoke> * 2)
  (invoke> + 2) (invoke> sqrt 1)
  <peek!>)

(pythagorean 8 6)
;; => 10.0


;; I finish this with the macroexpansion and output syntax.

#_(use 'clojure.walk)
(macroexpand '(c/defstackfn example [!a !b !c]
                    !a !b
                    (invoke> + 2)
                    !v1+ !c !c <pop> 2
                    (invoke> * 2)
                    !v2+
                    (invoke> = 2)
                    (if> !v1 !v2
                         (invoke> - 2)
                         else>
                         "false!!"
                         (invoke> println 1)
                         <pop> !v1 !v2
                         (invoke> * 2))))


;; REVIEW: macroexpand

#_
(def example
  (clojure.core/fn
    ([!a !b !c]
     (clojure.core/->
      []
      (clojure.core/conj !a)
      (clojure.core/conj !b)
      (utils/wrap-op + 2)
      (utils/let->
       stack
       [!v1 (clojure.core/peek stack)]
       (clojure.core/conj !c)
       (clojure.core/conj !c)
       (pop)
       (clojure.core/conj 2)
       (utils/wrap-op * 2)
       (utils/let->
        stack
        [!v2 (clojure.core/peek stack)]
        (utils/wrap-op = 2)
        (utils/let->
         stack
         [element (clojure.core/peek stack) stack (clojure.core/pop stack)]
         (clojure.core/as->
             stack
             (if element
               (clojure.core/->
                stack
                (clojure.core/conj !v1)
                (clojure.core/conj !v2)
                (utils/wrap-op - 2))
               (clojure.core/->
                stack
                (clojure.core/conj "false!!")
                (utils/wrap-op println 1)
                (pop)
                (clojure.core/conj !v1)
                (clojure.core/conj !v2)
                (utils/wrap-op * 2)))))))))))


;; REVIEW: macroexpand-all

#_(def
    example
    (fn*
     ([!a !b !c]
      (let*
          [stack
           (utils/wrap-op (clojure.core/conj (clojure.core/conj [] !a) !b) + 2)
           !v1
           (clojure.core/peek stack)]
        (let*
            [stack
             (utils/wrap-op
              (clojure.core/conj
               (pop (clojure.core/conj (clojure.core/conj stack !c) !c))
               2)
              *
              2)
             !v2
             (clojure.core/peek stack)]
          (let*
              [stack
               (utils/wrap-op stack = 2)
               element
               (clojure.core/peek stack)
               stack
               (clojure.core/pop stack)]
            (let*
                [stack stack]
              (if
                  element
                (utils/wrap-op
                 (clojure.core/conj (clojure.core/conj stack !v1) !v2)
                 -
                 2)
                (utils/wrap-op
                 (clojure.core/conj
                  (clojure.core/conj
                   (pop (utils/wrap-op (clojure.core/conj stack "false!!") println 1))
                   !v1)
                  !v2)
                 *
                 2)))))))))
