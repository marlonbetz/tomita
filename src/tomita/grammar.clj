(ns tomita.grammar
  (:require [tomita.rules :as r]
            [clojure.pprint :as pprint]))



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


(defn update-dotted-rule
  "returns updated dotted rule when seeing one token"
  [old-dotted-rule new-token]
  (if (= (:next-child old-dotted-rule) new-token)
    (dotted-rule
     (:mother old-dotted-rule)
     (conj (:processed-children old-dotted-rule) new-token)
     (:children-left old-dotted-rule))
    old-dotted-rule))




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

(def some-simple-derivations
  (vector (r/derives-to :S [:C :S])
          (r/derives-to :S [:C])
          (r/derives-to :C [:A :B])
          (r/derives-to :A ["a"])
          (r/derives-to :B ["b"])))


(def some-recursive-derivations
  (vector (r/derives-to :S ["a" :S])
          (r/derives-to :S ["b" :S])
          (r/derives-to :S ["a"])
          (r/derives-to :S ["b"])))

(defn expand-dotted-rule
  "expands a given dotted rule by one level"
  [some-dotted-rule list-of-derivations]
  (apply hash-set (map rule->initial-dotted-rule
                    (filter #(= (:next-child some-dotted-rule) (:mother %))
                            list-of-derivations))))

(def expand-dotted-rule-memo
  "memoized version of expand-dotted-rule"
  (memoize expand-dotted-rule))

(defn generate-dotted-rule-set
  "generates a set of dotted rules by full expansion of a given list of dotted rules"
  [some-dotted-rules list-of-derivations]
  (loop [rules-left some-dotted-rules
         expansions-so-far (apply hash-set some-dotted-rules)]
    (let [expansion (expand-dotted-rule-memo (first rules-left) list-of-derivations)]
      (if (empty? rules-left)
        expansions-so-far
        (if (every? expansions-so-far expansion)
          (recur
                 (rest rules-left)
                 expansions-so-far)
          (recur (apply conj (rest rules-left) expansion)
                 (apply conj expansions-so-far expansion)))))))



(defn get-next-kernel
  "update a dotted rule set when seeing some token
   to create a kernel for some next state"
  [dotted-rule-set token]
  (filter #(not (contains? dotted-rule-set %))
    (map #(update-dotted-rule % token)
         dotted-rule-set)))



(defn update-dotted-rule-set
  "update dotted rule set after seeing some token"
  [dotted-rule-set list-of-derivations token]
  (generate-dotted-rule-set
    (get-next-kernel dotted-rule-set
                     token)
    list-of-derivations))

(defn get-next-possible-symbols
  "returs set of next possible symbols"
  [dotted-rule-set]
  (into #{}
    (filter #(not= nil %)
       (map :next-child dotted-rule-set))))


(defn get-next-states
  "returns a map of symbols to next states"
  [dotted-rule-set list-of-derivations]
  (reduce
    #(assoc
      %1
      %2
      (let [updated-set (update-dotted-rule-set dotted-rule-set list-of-derivations %2)]
        (if (empty? updated-set)
          dotted-rule-set
          updated-set)))
    {}
    (get-next-possible-symbols dotted-rule-set)))


(defn fsa-state-table-entry
  "simple state table entry"
  [dotted-rules symbol->next-state]
  {:dotted-rules dotted-rules
   :symbol->next-state symbol->next-state})

(defn dotted-rule-set->fsa-state-table-entry
  "creates transitions from one state to another"
  [dotted-rule-set list-of-derivations]
  (let [generated-set (generate-dotted-rule-set
                        dotted-rule-set
                        list-of-derivations)
        next-state-map (get-next-states generated-set list-of-derivations)]
    (fsa-state-table-entry generated-set next-state-map)))

(defn create-state-table
  "Creates simple table mapping fron one state via some symbol to another state"
  [initial-dotted-rule-set list-of-derivations]
  (loop [dotted-rule-sets-left [initial-dotted-rule-set]
         table-so-far #{}]
    (if
      (empty? dotted-rule-sets-left)
      table-so-far
      (let [current-rule-set (first dotted-rule-sets-left)
            next-entry (dotted-rule-set->fsa-state-table-entry
                         current-rule-set
                         list-of-derivations)]
        (if (contains? table-so-far next-entry)
          (recur (rest dotted-rule-sets-left) table-so-far)
          (recur (apply conj
                        (rest dotted-rule-sets-left)
                        (vals (:symbol->next-state next-entry)))
                 (conj table-so-far next-entry)))))))


(get-next-states
  (get
    (get-next-states
      (generate-dotted-rule-set
        [(rule->initial-dotted-rule
          (r/derives-to :START [:S]))]
        some-recursive-derivations)
      some-recursive-derivations)
   {:surface "a", :terminal? true})
  some-recursive-derivations)




(create-state-table
  (generate-dotted-rule-set
    [(rule->initial-dotted-rule
      (r/derives-to :START [:S]))]
    some-recursive-derivations)
  some-recursive-derivations)
