(ns active.ephemerol.regexp
  (:require [active.ephemerol.char-set :refer :all]
            [active.clojure.record :refer :all]
            [active.clojure.condition :refer (guard raise)]
            [active.clojure.condition :as c]
            [clojure.set :as set]))

; also, single characters and strings work

; terminology is roughly the same as PLT's lex library

(define-record-type Concatenation
  (make-concatenation res)
  concatenation?
  [res concatenation-res])

(def the-epsilon (make-concatenation []))

(defn epsilon?
  [re]
  (and (concatenation? re)
       (empty? (concatenation-res re))))

(declare regexp the-empty-set empty-set?)

(defn concatenation 
  [& res]
  (let [res
        (filterv (fn [re]
                   (not (epsilon? re)))
                 (apply concat
                        (map (fn [re]
                               (if (concatenation? re)
                                 (concatenation-res re)
                                 (let [re (regexp re)]
                                   (if (concatenation? re)
                                     (concatenation-res re)
                                     [re]))))
                             res)))]

    (cond
     (empty? res) the-epsilon

     (some empty-set? res) the-empty-set

     (= 1 (count res)) (first res)
     
     :else (make-concatenation res))))

(define-record-type Union
  (make-union res)
  union?
  [res union-res])

(defn cset?
  [thing]
  (or (set? thing)
      (char-set? thing)))

(defn csets-union 
  [css]
  (loop [cs1 (first css)
         css (rest css)]
    (if (empty? css)
      cs1
      (let [cs2 (first css)]
        (if (set? cs1)
          (recur (set/union cs1 cs2) (rest css))
	  (recur (char-set-union cs1 cs2) (rest css)))))))

(defn empty-cset?
  [cs]
  (or (and (set? cs) (empty? cs))
      (and (char-set? cs)
	   (char-set=? cs char-set:empty))))

(defn- partition-coll
  [pred lis]
  (let [mp (group-by pred lis)]
    [(mp true) (mp false)]))

(defn union 
  [& res]
  (let [[csets res] (partition-coll cset? (map regexp res))
        res (reduce (fn [unique re]
                      (if (union? re)
                        (set/union unique (union-res re))
                        (conj unique re)))
                    #{}
                    res)]
    (cond
     (not-empty csets)
     (let [cset (csets-union csets)]
       (if (empty? res)
         cset
         (make-union (cons cset res))))
     
     (empty? res) the-empty-set

     (empty? (rest res)) (first res)

     :else  (make-union res))))
			      
(def the-empty-set (make-union '()))

(defn empty-set?
  [re]
  (and (union? re)
       (empty? (union-res re))))

(define-record-type Repetition
  (make-repetition re)
  repetition?
  [re repetition-re])

(defn repetition-n
  [count re]
  (if (zero? count)
    the-epsilon
    (loop [count count res '()]
      (if (zero? count)
        (make-concatenation res)
        (recur (- count 1) (cons re res))))))

(defn repetition
  [lo hi re]
  (letfn
      [(repetition
         [lo hi re]
         (when (or (neg? lo)
                   (and hi (neg? hi)))
           (c/assertion-violation `repetition "negative bound" lo hi))
         (cond
          (empty-set? re)
          the-empty-set

          (pos? lo)
          (concatenation (repetition-n lo re)
                         (repetition 0 hi re))

          (not hi)
          (if (epsilon? re)
            the-epsilon
            (make-repetition re))
          
          (zero? hi)
          the-epsilon

          (zero? lo)
          (union the-epsilon
                 (repetition 1 (- hi 1) re))))]
    (repetition lo hi (regexp re))))

; BOL and EOL are only valid at the top level
; The constructors work so that EOL is always outermost.

(define-record-type Bol
  (make-bol re)
  bol?
  [re bol-re])

(declare make-eol eol? eol-re)

(defn bol
  [re]
  (if (eol? re)
    (make-eol (make-bol (eol-re re)))
    (make-bol re)))

(define-record-type Eol
  (make-eol re)
  eol?
  [re eol-re])

(def eol make-eol)

; predicate for regexps
; obj -> bool
(defn regexp?
  [thing]
  (or (union? thing)
      (concatenation? thing)
      (repetition? thing)
      (cset? thing)
      (bol? thing)
      (eol? thing)))

; transform characters and string to regexps
; union(string, char, regexp) -> regexp
(defn regexp
  [thing]
  (cond
   (string? thing)
   (make-concatenation (map char-set thing))

   (char? thing)
   (char-set thing)

   (empty-cset? thing)
   the-empty-set

   (regexp? thing)
   thing

   :else
    (c/assertion-violation `regexp "not a regexp" thing)))

; collect all character sets in a regexp
; regexp -> list(char-set)
(defn regexp-char-sets
  [re]
  (cond
   (char-set? re)
   (list re)

   (concatenation? re)
   (apply concat
          (map regexp-char-sets (concatenation-res re)))

   (union? re)
   (apply concat
          (map regexp-char-sets (union-res re)))

   (repetition? re)
   (regexp-char-sets (repetition-re re))

   (bol? re)
   (regexp-char-sets (bol-re re))

   (eol? re)
   (regexp-char-sets (eol-re re))

   :else
   (c/assertion-violation `regexp-char-sets "not a regexp" re)))

; iterate over all elements and tails of a list
; (a list(a) -> ?) list(a) -> ?
; DELETEME
(defn- for-each-tail
  [proc lis]
  (loop [lis lis]
    (if-let [lis (seq lis)]
      (proc (first lis) lis)
      (recur (rest lis)))))

(defn- iterate-for-each-tail
  [proc lis]
  (loop [lis lis
         res []]
    (if-let [lis (seq lis)]
      (let [res (proc (first lis) lis)]
        (recur (rest res)
               (conj res (first res))))
      res)))
 
; partition a list of character-sets into non-overlapping subsets
; list(char-set) -> list(char-set)
(defn partition-char-sets
  [sets]
  (loop [sets (seq sets)
         res []]
    (if (empty? sets)
      res
      (let [[splitter sets]
            (loop [splitter (first sets)
                   targets (rest sets)
                   res []]
              (let [target (first targets)]
                (cond
                 (empty? targets) [splitter res]
                 (char-set=? target splitter)
                 (recur splitter (rest targets) res)
                 :else
                 (let [[diff int] (char-set-diff+intersection target splitter)]
                   (cond 
                    (char-set=? int char-set:empty)
                    (recur splitter (rest targets) (conj res target))
                   
                    (char-set=? int splitter)
                    (recur splitter (rest targets) (conj res diff))
                          
                    (char-set=? int target)
                    (recur (char-set-difference splitter target)
                           (rest targets)
                           (conj res target)) 
                          
                    :else
                    (recur (char-set-difference splitter target) 
                           (cons int (rest targets))
                           (conj res diff)))))))]
        (recur sets (conj res splitter))))))

; turn a character set into its constituent indices from the partition
; char-set list(char-set) -> list(int)
(defn char-set->class-list
  [set part]
  (loop [part part
         i 0
         res []]
    (if-let [part (seq part)]
      (if (char-set<= (first part) set)
        (recur (rest part) (+ 1 i)
               (conj res i))
        (recur (rest part) (+ 1 i)
               res))
      res)))

; replace the character sets in a regexp by constituent index lists
; regexp(char-set) list(char-set) -> regexp(list(int))
(defn class-i-fy-regexp
  [re part]
  (letfn
      [(recurse [re]
         (cond
          (char-set? re)
          (char-set->class-list re part)

          (concatenation? re)
          (make-concatenation (map recurse (concatenation-res re)))

          (union? re)
          (make-union (map recurse (union-res re)))

          (repetition? re)
          (make-repetition (class-i-fy-regexp (repetition-re re) part))

          (bol? re)
          (make-bol (class-i-fy-regexp (bol-re re) part))

          (eol? re)
          (make-eol (class-i-fy-regexp (eol-re re) part))

          :else
          (c/assertion-violation `class-i-fy-regexp "not a regexp" re)))]
    (recurse re)))

; does a regexp accept the empty string?
; regexp -> bool
(defn regexp-accepts-empty?
  [re]
  (cond
   (set? re) false

   (concatenation? re)
   (every? regexp-accepts-empty? (concatenation-res re))

   (union? re)
   (some regexp-accepts-empty? (union-res re))

   (repetition? re) true

   (bol? re) false

   (eol? re) false

   (char-set? re) false

   :else
   (c/assertion-violation `regexp-accepts-empty? "not a regexp" re)))

; compute a residual regexp after a character has come in
; regexp(a) (union a '())  -> regexp(a)
; nil means "beginning of text"
(defn regexp-after
  [re next]
  (cond
   (bol? re)
   (if (nil? next)
     (bol-re re)
     the-empty-set)

   (nil? next) re

   (nil? re) the-empty-set

   (set? re)
   (if (contains? re next)
     the-epsilon
     the-empty-set)

   (char-set? re)
   (if (char-set-contains? re next)
     the-epsilon
     the-empty-set)

   (concatenation? re)
   (letfn
       [(recurse
          [res]
          (if (empty? res)
            the-empty-set
            (union (apply concatenation
                          (regexp-after (first res) next)
                          (rest res))
                   (if (regexp-accepts-empty? (first res))
                     (recurse (rest res))
                     the-empty-set))))]
     (recurse (concatenation-res re)))

   (union? re)
   (apply union 
          (map #(regexp-after % next)
               (union-res re)))

   (repetition? re)
   (concatenation (regexp-after (repetition-re re) next)
                  re)

   (bol? re)
   (c/assertion-violation `regexp-after
                          "bol regexp not allowed here" re next)

   (eol? re)
   (c/assertion-violation `regexp-after
                          "eol regexp not allowed here" re next)))
    

; compute the set of next possible characters matched by a regexp

(defn regexp-do-next
  [re set-empty set-empty? set-union]
  (letfn [(next [re]
            (cond
             (set? re) re
             (char-set? re) re

             (concatenation? re)
             (letfn [(recurse [res]
                       (cond
                        (empty? res) set-empty

                        (regexp-accepts-empty? (first res))

                        (set-union (next (first res))
                                   (recurse (rest res)))

                        :else (next (first res))))]
               (recurse (concatenation-res re)))

             (union? re)
             (apply set-union
                    (map next (union-res re)))

             (bol? re)
             set-empty

             (eol? re)
             (next (eol-re re))

             (repetition? re)
             (next (repetition-re re))))]
    (next re)))

(defn char-regexp-next
  [re]
  (regexp-do-next re
		  char-set:empty
		  #(char-set=? % char-set:empty) 
                  char-set-union))

(defn set-regexp-next
  [re]
  (regexp-do-next re
		  #{}
		  empty?
                  set/union))

(defn one-char-regexp?
  [re]
  (or (and (char-set? re)
	   (= 1 (char-set-size re)))
      (and (set? re)
           (= 1 (count re)))))

(defn one-char-regexp-ref
  [re]
  (cond
   (char-set? re) (first (char-set->list re))

   (set? re) (first re)

   :else nil))
  
(defn regexp-constant?
  [re]
  (or (one-char-regexp? re)
      (and (concatenation? re)
	   (every? one-char-regexp?
                   (concatenation-res re)))))

(defn constant-regexp->list
  [re]
  (if-let [c (one-char-regexp-ref re)]
    (list c)
    (map one-char-regexp-ref
	 (concatenation-res re))))

(defn expand-re-1
  [form]
  (if (and (list? form)
           (not-empty form))
    (case (first form)
      / `(concatenation ~@(map expand-re-1 (rest form)))

      * `(repetition 0 nil ~(expand-re-1 (second form)))

      ? `(repetition 0 1 ~(expand-re-1 (second form)))

      + `(repetition 1 nil ~(expand-re-1 (second form)))

      or `(union ~@(map expand-re-1 (rest form)))

      unquote (second form)

      `(regexp ~form))
    `(regexp ~form)))

(defn expand-re
  [form]
  (if (and (list? form)
           (not-empty form))
    (case (first form)
      $
      (let [inner (second form)]
        (if (and (list? inner)
                 (not-empty inner)
                 (= '% (first inner)))
            `(make-eol (make-bol ~(expand-re-1 (second inner))))
            `(make-eol ~(expand-re-1 inner))))
      %
      (let [inner (second form)]
        (if (and (list? inner)
                 (not-empty inner)
                 (= '$ (first inner)))
          `(make-eol (make-bol ~(expand-re-1 (second inner))))
          `(make-bol ~(expand-re-1 inner))))
      
      (expand-re-1 form))
    (expand-re-1 form)))

(defmacro re
  [form]
  (expand-re form))

(defmacro re-1
  [form]
  (expand-re-1 form))

