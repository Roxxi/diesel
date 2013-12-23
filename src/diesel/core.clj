(ns diesel.core
  (:use [roxxi.utils.print]))

(defn of-form? [fn-or-op expr]
  (if (fn? fn-or-op)
    (fn-or-op expr)
    (and (list? expr)
         (not (empty? expr))
         (= (first expr) fn-or-op))))

(defn remove-dispatch-form-cruft
  "Our dispatch forms have `=>` to make them pretty.
Since that isn't valid syntax, this removes that value
and effectively turns `[a => b]` into `[a b]`."
  [dispatch-expr]
  [(first dispatch-expr) (last dispatch-expr)])

(defn ordered-expr-interp [expr dispatch-mappings]
  (loop [op-fns=>ds dispatch-mappings]
    (when op-fns=>ds
      (let [op-fn=>d (first op-fns=>ds)
            op-fn (first op-fn=>d)
            d-val (last op-fn=>d)]
        (if (of-form? op-fn expr)
          d-val
          (recur (next op-fns=>ds)))))))

(defmacro definterp [name formals & dispatch-mappings]
  (let [dispatch-mappings (vec (map remove-dispatch-form-cruft dispatch-mappings))
        expr+formals (vec (cons 'expr formals))]
    `(defmulti ~name
       (fn ~'intepreter-fn ~expr+formals
         (if (list? ~'expr)
           (ordered-expr-interp ~'expr ~dispatch-mappings)
           ~'expr))
       :default :unknown-operator)))



(defn div? [expr]
  (and (number? expr)
       (zero? expr)))

(macroexpand-1
'(definterp my-interp []
   ['add => :add]
   ['sub => :sub]
   [div? => :div]))

(definterp my-interp []
  ['add => :add]
  ['sub => :sub]
  [div? => :div])

(defn my-interp-add-args [expr]
  (rest expr))

(defmethod my-interp :add [expr]
  (apply + (map my-interp (my-interp-add-args expr))))

(def my-interp-sub-args my-interp-add-args)

(defmethod my-interp :sub [expr]
  (let [args (my-interp-sub-args expr)]
    (if (= (count args) 1)
      (my-interp (first args))
      (apply - (map my-interp (my-interp-sub-args expr))))))

(defmethod my-interp :unknown-operator [expr]
  (str "Unknown handler for `" (first expr)
       "` when handling `" expr "`"))

(my-interp '(add 6 7))



;; (defmulti interp [e]
;;   (cond
;;    (list? e) (interp
