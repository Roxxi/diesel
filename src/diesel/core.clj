(ns diesel.core)


;; (interpreter [a b c]
;;              add => :add
;;              sub => :sub
;;              ~div => div-expr



(defn expression-dispatch
  "It's assumed that e is a list, and thus, has a first"
  [e op-dispatches]
  (let [op (first e)]
    (loop [dispatches op-dispatches]
      (let [dispatch (first dispatches)]
        (println dispatches)
        (println dispatch)
        (println op)
        (cond (empty? dispatches)
              (str "Unknown handler for " op)
              (= op (first dispatch))
              (second dispatch)
              :else
              (recur (next dispatches)))))))

;; (interpreter [a b c]
;;              add => :add
;;              sub => :sub
;;              ~div => div-expr)

(defn div? [x y]
  (zero? y))

(defmacro definterp [name formals & dispatches]
  ;; check that dispatches is divisible by 3
  (let [dispatches (map #(identity `['~(nth % 0) ~(nth % 2)])
                        (partition 3 dispatches))
        dispatches (into [] dispatches)]
    `(defmulti ~name
       (fn ~(into [] (concat ['expr] formals))
         (if (list? ~'expr)
           (do
             (println "hello!")
             (expression-dispatch ~'expr ~dispatches))
           ~'expr)))))


(macroexpand-1
'(definterp test []
   add => :add
   sub => :sub
   #(div? %) => :div))

(definterp test []
  add => :add
  sub => :sub
  #(div? %) => :div)

(defmethod test :add [expr]
  (println expr))

(test '(div 5 6 8))


;; (defmulti interp [e]
;;   (cond
;;    (list? e) (interp
