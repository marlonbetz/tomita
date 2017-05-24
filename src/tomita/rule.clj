(ns tomita.rule)


(defn grammar-symbol
  ([surface terminal?]
   {:surface surface
    :terminal? terminal?})
  ([surface]
   (grammar-symbol surface (not (keyword? surface)))))

(grammar-symbol :GREETING)

(defn derives-to
  [mother children]
  {:mother (grammar-symbol mother)
   :children (vec (map grammar-symbol children))})

(derives-to :WHOLE-GREETING [:GREETING :WHITE-SPACE :PARAGRAPH-BREAK])
(derives-to :WHITE-SPACE [" "])




(defn make-derivations-map
  [mapping-function & rules]
  (loop [rules-left rules
         derivation-map {}]
   (if (empty? rules-left)
    derivation-map
    (let [current-rule (first rules-left)
          mother (mapping-function (:mother current-rule))
          children (map mapping-function (:children current-rule))
          updated-children (if-let [previous-dervs (mother derivation-map)]
                                   (conj previous-dervs children)
                                   children)]
     (recur (rest rules-left)
            (assoc derivation-map
                   mother
                   updated-children))))))

(make-derivations-map
         :surface
         (derives-to :WHOLE-GREETING [:GREETING])
         (derives-to :WHOLE-GREETING [:GREETING :WHITE-SPACE :PARAGRAPH-BREAK])
         (derives-to :GREETING ["hello"])
         (derives-to :GREETING ["hi"])
         (derives-to :GREETING ["hey"])
         (derives-to :GREETING ["heyho"])
         (derives-to :WHITE-SPACE [" "])
         (derives-to :PARAGRAPH-BREAK [","])
         (derives-to :PARAGRAPH-BREAK ["!"])
         (derives-to :PARAGRAPH-BREAK ["."])
         (derives-to :PARAGRAPH-BREAK [":"])
         (derives-to :PARAGRAPH-BREAK ["\n\n"]))


(defn parsing-state-member
  [mother processed-children unprocessed-children]
  {:mother mother
   :processed-children processed-children
   :next-child (first unprocessed-children)
   :children-left (vec (rest unprocessed-children))})

(defn rule->initial-parsing-state-member
  [rule]
  (parsing-state-member (:surface (:mother rule))
                        []
                        (vec (map :surface (:children rule)))))

(rule->initial-parsing-state-member
 (derives-to :WHOLE-GREETING
             [:GREETING :WHITE-SPACE :PARAGRAPH-BREAK]))

(defn update-parsing-state-member
  [old-parsing-state-member new-token]
  (if (= (:next-child old-parsing-state-member) new-token)
    (parsing-state-member
     (:mother old-parsing-state-member)
     (conj (:processed-children old-parsing-state-member) new-token)
     (:children-left old-parsing-state-member))
    old-parsing-state-member))

(update-parsing-state-member
 (update-parsing-state-member
  (rule->initial-parsing-state-member
   (derives-to :WHOLE-GREETING
              [:GREETING :WHITE-SPACE :PARAGRAPH-BREAK]))
  :GREETING)
 :WHITE-SPACE)
