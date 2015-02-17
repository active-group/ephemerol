(ns active.ephemerol.scanner
  (:require [active.ephemerol.regexp :refer :all]
            [active.ephemerol.char-set :refer :all]
            [active.ephemerol.scanner-run :refer :all]
            [active.ephemerol.compact-table :refer :all])
  (:import [java.util ArrayList]))

; Automaton & scanner generation

; We use a tunnel automaton according to:
; J. Grosch, Efficient Generation of Table-Driven Scanners, Cocktail Document No. 2, 1987.
; ftp://www.cocolab.com/products/cocktail/doc.pdf/scangen.pdf

; A scanner specification is a list of pairs (a "spec"), and each pair
; consists of a regexp and an expression evaluating to an action procedure like so:
; action : list(char) position list(char) -> <encoding>  list(char) position
; arguments:
;   - lexeme
;   - position
;   - remaining input after lexeme
; returns:
;   - encoded lexeme
;   - remaining input; the action may consume more of remaining input
;   - position of the remaining input


(definterface IState
  (state_spec [])
  (set_state_spec [x])
  (state_transitions [])
  (set_state_transitions [x])
  (state_tunnel []))

(deftype State 
    [^:volatile-mutable spec 
     ^:volatile-mutable transitions
     tunnel]
  IState
  (state_spec [_] spec)
  (set_state_spec [_ x] (set! spec x))
  (state_transitions [_] transitions)
  (set_state_transitions [_ x] (set! transitions x))
  (state_tunnel [_] tunnel))

(defn make-state
  [spec transitions tunnel]
  (State. spec transitions tunnel))

(defn state-spec
  [^State st]
  (.state_spec st))

(defn set-state-spec!
  [^State st sp]
  (.set_state_spec st sp))

  ;; map of character class indices and state indices

(defn state-transitions
  [^State st]
  (.state_transitions st))

(defn set-state-transitions!
  [^State st tr]
  (.set_state_transitions st tr))

  ;; index
(defn state-tunnel
  [^State st]
  (.-tunnel st))

; remove the eols from the regexps, and, on each of these rules,
; turn the action into an :eol-action.
(defn spec-clean-eols
  [spec]
  (map (fn [p]
	 (let [re (first p)
               action (second p)]
	   (if (eol? re)
	       [(eol-re re)
                (make-eol-action action nil)]
	       p)))
       spec))

(defn state-next
  [^ArrayList maton state i]
  (loop [state state]
    (or (get (state-transitions state) i)
        (if-let [tunnel-index (state-tunnel state)]
          (recur (.get maton tunnel-index))
          false))))

(defn state-next-ambiguous?
  [^ArrayList maton state i]
  (loop [state state]
    (if (contains? (state-transitions state) i)
     (> (count (state-transitions state)) 1)
     (if-let [tunnel-index (state-tunnel state)]
       (recur (.get maton tunnel-index))
       false))))

(defn state-add-transition!
  [state next index]
  (set-state-transitions! state
                          (assoc (state-transitions state) 
                            next index)))

(defn spec=?
  [spec-1 spec-2]
  (and (= (count spec-1)
	  (count spec-2))
       (every? (fn [[p1 p2]]
                 (and (= (first p1) (first p2))
                      (identical? (second p1) (second p2))))
               (map (fn [p1 p2] [p1 p2]) spec-1 spec-2))))

(defn spec-after
  [spec i]
  (filter (fn [p]
	    (not (empty-set? (first p))))
	  (map (fn [p]
		 [(regexp-after (first p) i)
                  (second p)])
	       spec)))

(defn find-spec
  [^ArrayList maton spec]
  (let [size (.size maton)]
    (loop [i 0]
      (cond
       (>= i size) false
       (spec=? spec (state-spec (.get maton i))) i
       :else (recur (+ 1 i))))))

(defn class-i-fy-spec
  [spec]
  (let [char-sets 
        (apply concat
               (map regexp-char-sets (map first spec)))
        part (partition-char-sets char-sets)]
    [(map (fn [p]
            [(class-i-fy-regexp (first p) part)
             (second p)])
          spec)
     part]))

(defn spec-for-each-next
  [proc spec part-count]
  (let [done? (boolean-array part-count false)]
    (doseq [p spec]
      (doseq [next (set-regexp-next (first p))]
        (when-not (aget done? next)
          (do
            (proc next)
            (aset-boolean done? next true)))))))

(defn state-replace-transition!
  [state for to]
  (set-state-transitions! state (assoc (state-transitions state) for to)))

(defn compute-ambiguous
  [maton]
  (let [size (.size maton)
        pred-counts (int-array size 0)]
    (loop [i 0]
      (if (< i size)
        (do
          (doseq [t (state-transitions (.get maton i))]
            (aset-int pred-counts (val t)
                      (+ 1 (aget pred-counts (val t)))))
          (recur (+ 1 i)))
        (mapv #(> % 1) pred-counts)))))

(defn- partition-coll
  [pred lis]
  (let [mp (group-by pred lis)]
    [(mp true) (mp false)]))

(declare add-constant-rule!)

; list(pair(regexp, data)) -> (values vvector(state) vector(charset) boolean)
(defn compute-automaton
  [spec]
  (let [spec (spec-clean-eols spec)
        [spec part] (class-i-fy-spec spec)
        [constant-spec spec] (partition-coll (fn [p]
                                               (regexp-constant? (first p)))
                                             spec)
        part-count (count part)
        initial (make-state spec {} nil)
        after-bot (if (some bol? (map first spec))
                    (make-state (spec-after spec nil) {} nil)
                    nil)
        to-do (if after-bot
                [initial after-bot]
                [initial])
        maton (ArrayList. to-do)]
    ;; compute regular DFA by exhausting to-do
    (loop [to-do (seq to-do)]
      (if (empty? to-do)
        [maton part]
        (let [state (first to-do)
              spec (state-spec state)
              to-do (atom (rest to-do))]
          (spec-for-each-next
           (fn [next]
             (let [target-spec (spec-after spec next)]
               (when-not (empty? target-spec)
                 (let [target-index
                       (or (find-spec maton target-spec)
                           (let [new (make-state target-spec {} nil)]
                             (swap! to-do conj new)
                             (.add maton new)
                             (- (.size maton) 1)))]
                   (state-add-transition! state next target-index)))))
           spec part-count)
          (recur @to-do))))
      ;; add tunnel transitions
      (let [ambiguous (compute-ambiguous maton)
            ambiguous? (fn [state-index]
                         (and (< state-index (count ambiguous))
                              (get ambiguous state-index)))]

	(doseq [p constant-spec]
          (add-constant-rule! maton ambiguous? p)))

      [maton part (and after-bot true)]))

(defn finalize-state!
  [state p]
  (set-state-spec! state
		   (cons [the-epsilon (second p)]
			 (state-spec state))))

(defn extend-path!
  [maton state sq p]
  (loop [state state
         sq (seq sq)]
    (if (empty? sq)
      (finalize-state! state p)
      (let [new-state (make-state [] {} nil)
            new-state-index (do 
                              (.add maton new-state)
                              (- (.size maton) 1))]
        (state-add-transition! state (first sq) new-state-index)
        (recur new-state (rest sq))))))

; add a rule with a constant regexp to the tunnel automaton; see Grosch's paper
(defn add-constant-rule!
  [maton ambiguous? p]

  (loop [state (.get maton 0)
         sq (seq (constant-regexp->list (first p)))]
    (if (empty? sq)
      ;; Case 1
      (finalize-state! state p)
      (if-let [target-index (state-next maton state (first sq))]
        (if (ambiguous? target-index)
          ;; Case 3
          (let [orig-state (.get maton target-index)
                tunnel-state (make-state (state-spec orig-state) {} target-index)
                tunnel-state-index (do
                                     (.add maton tunnel-state)
                                     (- (.size maton) 1))]
                (state-replace-transition! state (first sq) tunnel-state-index)
		 (loop [orig-state orig-state ; s_i
                        tunnel-state tunnel-state ; z_i
                        sq (rest sq)]
		   (if (empty? sq)
                     (finalize-state! tunnel-state p)
                     (if-let [orig-next-index (state-next maton orig-state (first sq))]
                       (let [orig-next (.get maton orig-next-index)
                             new-tunnel (make-state (state-spec orig-next) {} orig-next-index)
                             new-tunnel-index (do
                                                (.add maton new-tunnel)
                                                (- (.size maton) 1))]
                         (state-add-transition! tunnel-state (first sq) new-tunnel-index)
                         (recur orig-next new-tunnel (rest sq)))
                       ;; Case 4
                       (extend-path! maton tunnel-state sq p)))))
          ;; Case 1/2
          (recur (.get maton target-index) (rest sq)))
        ;; Case 2
        (extend-path! maton state sq p)))))

(declare final-state-action)

(defn write-automaton-dot
  [maton filename]
  (binding [*out* (java.io.FileWriter. filename)]
    (println "digraph M {")
    (let [size (.size maton)]
      (loop [i 0]
        (when (< i size)
          (let [state (.get maton i)
                trans (state-transitions state)]
            (when (final-state-action state)
              (println " " i "[shape=box]"))
            (doseq [target (set (map val trans))]
              (let [classes (map key
                                 (filter (fn [p]
                                           (= (val p) target))
                                         trans))]
                (println " " i "->" target "[label=\"" classes "\"];")))

            (when-let [target (state-tunnel state)]
              (println " " i "->" target "[style=dotted];")))
          (recur (+ i 1)))))
    (println "}")
    (println)))

(declare list-index)

(defn partition->compact-table
  [part bits]
  (compute-compact-table
   (let [counter (atom 0)]
     (fn []
       (cond
	(> @counter 0x10FFFF)
        nil

        (and (>= @counter 0xD800)
             (<= @counter 0xDFFF))
        (do (swap! counter inc)
            -1)

        :else
        (let [c @counter
              val (or (list-index (fn [cs]
                                    (char-set-contains? cs c))
                                  part)
                      -1)]
          (swap! counter inc)
          val))))
   (bit-shift-left 1 bits)))

(defn list-index
  [pred sq]
  (loop [sq (seq sq)
         n 0]
    (and (not-empty sq)
         (if (pred (first sq))
           n 
           (recur (rest sq) (+ n 1))))))

; returns two vectors:
; the first is the state table, each state a range of size (+ 1 (length part)),
; containing the transitions, and the tunnel ("no state" encoded as -1)
; the second is a table of final states, where each entry is either a regular action
; or a :eol-action record

(defn automaton->table
  [maton part]
  (let [part-count (count part)
        state-count (.size maton)
        states (int-array (* state-count (+ part-count 1)) -1) ; + 1 for tunnel
        final (object-array state-count)]
    (loop [state-index 0]
      (when (< state-index state-count)
        (let [state (.get maton state-index)
              base (* state-index (+ 1 part-count))]
          ;; generate state stable
          (doseq [t (state-transitions state)]
            (aset-int states (+ base (key t)) (val t)))
          ;; tunnel transitions
          (when-let [tunnel-index (state-tunnel state)]
            (aset-int states (+ base part-count) tunnel-index))
          ;; final states
          (if-let [action (final-state-action state)]
            (aset final state-index action)))
        (recur (+ 1 state-index))))
    [states final]))

; returns action if final state, nil otherwise
(defn final-state-action
  [state]
  (let [accept-empty (filter (fn [p]
                               (regexp-accepts-empty? (first p)))
                             (state-spec state))]
    (if (empty? accept-empty) 
      nil
      (if-let [p (some (fn [p]
                         (and (eol-action? (second p))
                              p))
                       accept-empty)]
        (make-eol-action (eol-action-at-eol (second p))
                         (some (fn [p]
                                 (and (not (eol-action? (second p)))
                                      (second p)))
                               accept-empty))
        (second (first accept-empty))))))

;; extract ALL <<eof>> actions. However, there should only be
;; one, which we can check later for correctness.
(defn collect-eof-action
  [spec]
  (cond (empty? spec) '()

        (= (ffirst spec) :<<eof>>) 
        ;; I only need the eof-action functions themselves....
        (cons (second (first spec)) (collect-eof-action (rest spec)))

        :else (recur (rest spec))))

;; Strip out all of the <<eof>> actions from the specification.
(defn strip-eof-action
  [spec]
  (cond (empty? spec) '()

        (= (ffirst spec) :<<eof>>)
        (recur (rest spec))
        
        :else (cons (first spec) (strip-eof-action (rest spec)))))

(defn compute-scanner
  [spec & [maybe-part-encoding-bits]]
  (let [part-encoding-bits (or maybe-part-encoding-bits 8)]
    (let [eof-?action (collect-eof-action spec)
          ;; if there isn't an eof action, make a default one which returns
          ;; nil. If there are actions, take the first one defined.
          eof-action (if (empty? eof-?action)
                        `(fn [~'lexeme ~'start-position ~'last-input ~'last-position]
                           [nil ~'last-input ~'last-position])
                        (first eof-?action))
          nspec (strip-eof-action spec)]
      (let [[maton part bot-state?] (compute-automaton nspec)
            [states final] (automaton->table maton part)
            [indices encodings] (partition->compact-table part part-encoding-bits)]
	(make-scanner 
	 maton bot-state? states final (count part) 
	 part-encoding-bits indices (int-array encodings) eof-action)))))

(defmacro scanner-spec
  [& pairs]
  `(list ~@(map (fn [p]
                  (if (= (first p) '<<eof>>)
                    `[:<<eof>> '~(second p)]
                    `[(re ~(first p)) '~(second p)]))
                pairs)))
