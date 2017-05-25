(ns tomita.rules)


(defn grammar-symbol
  "Symbol in a grammar. Either terminal or non-terminal."
  ([surface terminal?]
   {:surface surface
    :terminal? terminal?})
  ([surface]
   (grammar-symbol surface (not (keyword? surface)))))

(grammar-symbol :GREETING)

(defn derives-to
  "Creates a grammar rule. The mother symbol derives to its children symbols."
  [mother children]
  {:mother (grammar-symbol mother)
   :children (vec (map grammar-symbol children))})

(derives-to :WHOLE-GREETING [:GREETING :WHITE-SPACE :PARAGRAPH-BREAK])
(derives-to :WHITE-SPACE [" "])
