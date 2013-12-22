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

(defn of-form? [expr proc-or-op]
  (if (fn? proc-or-op)
    (proc-or-op expr)
    (and (seq? expr)
         (not (empty? expr))
         (= (first expr) proc-or-op))))

(defn remove-dispatch-form-cruft
  "Our dispatch forms have `=>` to make them pretty.
Since that isn't valid syntax, this removes that value
and effectively turns `[a => b]` into `[a b]`."
  [dispatch-expr]
  [(first dispatch-expr) (last dispatch-expr)])

(defmacro definterp [name formals & dispatches]
  (let [dispatches (map remove-dispatch-form-cruft dispatches)]
    `(defmulti ~name
       (fn intepreter-fn ~(into [] (concat ['expr] formals))
         (loop [forms# [~@dispatches]]
           (when forms#
             (let [f# (first forms#)]
               (if (of-form? ~'expr (first f#))
                 (last f#)
                 (recur (next forms#)))))))
       :default :unknown-operator)))



(macroexpand-1
'(definterp test [x y]
   ['add => :add]
   ['sub => :sub]
   [div? => :div]))

(definterp test [x y]
   ['add => :add]
   ['sub => :sub]
   [div? => :div])

(defmethod test :add [expr x y]
  (println expr x y))

(defmethod test :unknown-operator [expr x y]
  (str "Unknown handler for `" (first expr) "` when handling `" expr "`"))

(test '(div 6 8) :hi :hello )


;; (defmulti interp [e]
;;   (cond
;;    (list? e) (interp
