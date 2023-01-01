(ns scratch
  (:require [utils :as u]))

;; A place to experiment and iterate on macro output syntaxes
;; Further down is older.
;; NOTE: code changes mean these no longer evaluate, but you can
;;       still see my shift over time to a threaded syntax.

#_:clj-kondo/ignore
#_(defn example-fn4 [!a !b !c]
  (let-> [] stack [vars '(!a !b !c)]
         (u/push !a)
         (u/push !b)
         (u/wrap-op + 2)
         (let-> stack [vars '(!a !b !c !v1)
                       !v1 (peek stack)]
                (u/push !c)
                (u/push !c)
                (pop)
                (u/push 2)
                (u/wrap-op * 2)
                (let-> stack [vars '(!a !b !c !v1 !v2)
                              !v2 (peek stack)]
                       (u/wrap-op = 2)
                       (let-> stack [if-cond (peek stack)
                                     stack (pop stack)]
                              (as-> stack
                                  (if if-cond
                                    (-> stack (u/push !v1) (u/push !v2) (u/wrap-op - 2))
                                    (-> stack (u/push "false!!") (u/wrap-op println 1) (pop) (u/push !v1) (u/push !v2) (u/wrap-op * 2)))))
                       (u/push 1)))))

#_(example-fn4 1 2 4)

#_(defn example-fn3
  [!a !b !c]
  (let [stack []
        vars '(!a !b !c)]
    (-> stack
        (u/push !a)
        (u/push !b)
        (u/wrap-op + 2)
        (as-> stack
            (let [vars '(!a !b !c !v1)
                  !v1 (peek stack)]
              (-> stack
                  (u/push !c)
                  (u/push !c)
                  (pop)
                  (u/push 2)
                  (u/wrap-op * 2)
                  (as-> stack
                      (let [vars '(!a !b !c !v1 !v2)
                            !v2 (peek stack)]
                        (-> stack
                            (u/wrap-op = 2)
                            (as-> stack
                                (let [if-cond (peek stack)
                                      stack (pop stack)]
                                  (if if-cond
                                    (-> stack (u/push !v1) (u/push !v2) (u/wrap-op - 2))
                                    (-> stack (u/push "false!!") (u/wrap-op println 1) (pop) (u/push !v1) (u/push !v2) (u/wrap-op * 2)))))
                            (u/push 1))))))))))

;; (example-fn3 1 2 4)

#_(defn example-fn2
  [!a !b !c]
  (let [stack []
        vars '(!a !b !c)]
    (let [stack (->> stack (u/push !a) (u/push !b) (u/wrap-op + 2))
          vars '(!a !b !c !v1)
          !v1 (peek stack)]
      (let [stack (->> stack (u/push !c) (u/push !c) (pop) (u/push 2) (u/wrap-op * 2))
            vars '(!a !b !c !v1 !v2)
            !v2 (peek stack)]
        (let [stack (->> stack (u/wrap-op = 2))
              if-cond (peek stack)
              stack (pop stack)]
          (if if-cond
            (->> stack (u/push !v1) (u/push !v2) (u/wrap-op - 2))
            (->> stack (u/push "false!!") (u/wrap-op println 1) (pop) (u/push !v1) (u/push !v2) (u/wrap-op * 2))))))))

;; (example-fn2 1 2 4)

#_(defn example-fn1
  [!a !b !c]
  (let [stack (u/wrap-op + 2 (u/push !b (u/push !a [])))
        !v1 (peek stack)]
    (let [stack (u/wrap-op * 2 (u/push 2 (pop (u/push !c (u/push !c stack)))))
          !v2 (peek stack)]
      (let [stack (u/wrap-op = 2 stack)
            if-cond (peek stack)
            stack (pop stack)]
        (if if-cond
          (u/wrap-op - 2 (u/push !v2 (u/push !v1 stack)))
          (u/wrap-op * 2 (u/push !v2 (u/push !v1 (pop (u/wrap-op println 1 (u/push "False" stack)))))))))))

;; (example-fn1 1 2 4)
