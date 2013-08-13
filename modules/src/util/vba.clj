(ns spork.util.vba)


(defmacro get-sub [nm args body]
  (let [binds (get-binds body)]
    `(defn ~nm [~@args] 
       (do ~binds))))

