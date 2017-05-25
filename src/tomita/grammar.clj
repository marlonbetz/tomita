(ns tomita.grammar
  (:require [tomita.rules :as r]))



(defn action
  "either shift or reduce to next state"
  [action-type next-state]
  {:action-type action-type
   :next-state next-state})

(action :shift 1)

(defn goto
  "go to next state"
  [next-state]
  {:next-state next-state})


(defn dotted-rule
  "dotted rule with 1 look-ahead"
  [mother processed-children unprocessed-children]
  {:mother mother
   :processed-children processed-children
   :next-child (first unprocessed-children)
   :children-left (vec (rest unprocessed-children))})

(defn rule->initial-dotted-rule
  [rule]
  (dotted-rule (:mother rule)
               []
               (:children rule)))

(rule->initial-dotted-rule
 (r/derives-to :WHOLE-GREETING
             [:GREETING " " :PARAGRAPH-BREAK]))

(defn update-dotted-rule
  "returns updated dotted rule when seeing one token"
  [old-dotted-rule new-token]
  (if (= (:next-child old-dotted-rule) new-token)
    (dotted-rule
     (:mother old-dotted-rule)
     (conj (:processed-children old-dotted-rule) new-token)
     (:children-left old-dotted-rule))
    old-dotted-rule))

(update-dotted-rule
 (update-dotted-rule
  (rule->initial-dotted-rule
   (r/derives-to :WHOLE-GREETING
              [:GREETING " " :PARAGRAPH-BREAK]))
  (r/grammar-symbol :GREETING))
 (r/grammar-symbol " "))



(defn parse-table-row
  "one row a in parse table"
  [state dotted-rules terminal->actions non-terminal->goto]
  {:state state
   :dotted-rules dotted-rules
   :terminal->actions terminal->actions
   :non-terminal->goto non-terminal->goto})

(defn empty-parse-table-row
  "initiates empty parse table row"
  [state]
  (parse-table-row state #{} {} {}))

(def empty-parse-table
  "initiates empty parse table"
  (empty-parse-table-row 0))

(def some-greeting-derivations
  (vector
         (r/derives-to :WHOLE-GREETING [:GREETING])
         (r/derives-to :WHOLE-GREETING [:GREETING :WHITE-SPACE :PARAGRAPH-BREAK])
         (r/derives-to :GREETING [:INFORMAL-GREETING])
         (r/derives-to :INFORMAL-GREETING ["hello"])
         (r/derives-to :INFORMAL-GREETING ["hi"])
         (r/derives-to :INFORMAL-GREETING ["hey"])
         (r/derives-to :INFORMAL-GREETING ["heyho"])
         (r/derives-to :INFORMAL-GREETING ["sheesh"])
         (r/derives-to :GREETING [:FORMAL-GREETING])
         (r/derives-to :FORMAL-GREETING ["dear sir"])
         (r/derives-to :FORMAL-GREETING ["dear madam"])
         (r/derives-to :FORMAL-GREETING ["dear customer team"])
         (r/derives-to :WHITE-SPACE [" "])
         (r/derives-to :PARAGRAPH-BREAK [","])
         (r/derives-to :PARAGRAPH-BREAK ["!"])
         (r/derives-to :PARAGRAPH-BREAK ["."])
         (r/derives-to :PARAGRAPH-BREAK [":"])
         (r/derives-to :PARAGRAPH-BREAK ["\n\n"])))

(def some-recursive-derivations
  (vector (r/derives-to :S [:S "a"])
          (r/derives-to :S ["a"])))

(defn expand-dotted-rule
  "expands a given dotted rule by one level"
  [some-dotted-rule list-of-derivations]
  (apply hash-set (map rule->initial-dotted-rule
                    (filter #(= (:next-child some-dotted-rule) (:mother %))
                            list-of-derivations))))


(defn generate-dotted-rule-set
  "generates a set of dotted rules by full expansion of a given list of dotted rules"
  [some-dotted-rules list-of-derivations]
  (loop [rules-left some-dotted-rules
         expansions-so-far #{}]
    (let [expansion (expand-dotted-rule (first rules-left) list-of-derivations)]
      (if (empty? rules-left)
        expansions-so-far
        (if (every? expansions-so-far expansion)
          (recur (rest rules-left)
                 expansions-so-far)
          (recur (apply conj (rest rules-left) expansion)
                 (apply conj expansions-so-far expansion)))))))

(generate-dotted-rule-set
  [(rule->initial-dotted-rule
    (r/derives-to :WHOLE-GREETING [:GREETING :WHITE-SPACE :PARAGRAPH-BREAK]))]
  some-greeting-derivations)

(generate-dotted-rule-set
  [(rule->initial-dotted-rule
    (r/derives-to :S [:S "a"]))]
  some-recursive-derivations)
