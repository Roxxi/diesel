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

(defn div? [expr]
  (and (number? expr)
       (zero? expr)))

(defn of-form? [expr proc-or-sym]
  (if (fn? proc-or-sym)
    (proc-or-sym expr)
    (and (seq? expr)
         (not (empty? expr))
         (= (first expr) proc-or-sym))))

(defn remove-dispatch-cruft [dispatch-expr]
  [(first dispatch-expr) (last dispatch-expr)])

(defmacro definterp [name formals & dispatches]
  (let [dispatches (map remove-dispatch-cruft dispatches)]
  `(defmulti ~name
     (fn ~(into [] (concat ['expr] formals))
       (loop [forms# [~@dispatches]]
         (if forms#
           (let [f# (first forms#)]
             (if (of-form? ~'expr (first f#))
               (last f#)
               (recur (next forms#))))
           (do
             ;;(println ~'expr)
             (str "Unknown handler for " (first ~'expr)
                  "when handling " ~'expr))))))))


(macroexpand-1
'(definterp test []
   ['add => :add]
   ['sub => :sub]
   [div? => :div]))

(definterp test []
   ['add => :add]
   ['sub => :sub]
   [div? => :div])

(defmethod test :add [expr]
  (println expr))

(test '(div 5 6 8))


;; (defmulti interp [e]
;;   (cond
;;    (list? e) (interp
