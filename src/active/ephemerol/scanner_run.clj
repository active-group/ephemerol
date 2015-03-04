(ns active.ephemerol.scanner-run
  (:require [active.clojure.record :refer :all])
  (:import [java.io Reader IOException]))

(definterface IState
  (position_row [])
  (set_position_row [x])
  (position_column [])
  (set_position_column [x]))

(deftype Position
    [^:volatile-mutable row
     ^:volatile-mutable column]
  Object
  (equals
    [this other]
    (and (instance? Position other)
         (let [^Position other-pos other]
           (= row (.position_row other-pos))
           (= column (.position_column other-pos)))))
  (hashCode
    [this]
    (+ row column))
  IState
  (position_row [_] row)
  (set_position_row [_ x] (set! row x))
  (position_column [_] column)
  (set_position_column [_ x] (set! column x)))

(defn make-position
  [r c]
  (Position. r c))

(defn position-row
  [^Position p]
  (.position_row p))

(defn set-position-row!
  [^Position p r]
  (.set_position_row p r))

(defn position-column
  [^Position p]
  (.position_column p))

(defn set-position-column!
  [^Position p c]
  (.set_position_column p c))

(defn copy-position
  [pos]
  (Position. (position-row pos)
             (position-column pos)))

(defn position=?
  [pos1 pos2]
  (and (= (position-row pos1) (position-row pos2))
       (= (position-column pos1) (position-column pos2))))

(def ^:private linefeed (int \newline))
(def ^:private tab (int \tab))

(defn update-position!
  [pos ^long ch]
  (case ch
    10 ; linefeed
    (do
      (set-position-column! pos 0)
      (set-position-row! pos (+ 1 (position-row pos))))

    9 ; tab
    (let [col (position-column pos)]
      (set-position-column! pos (* 8 (quot (+ 7 col) 8))))

    (set-position-column! pos (+ 1 (position-column pos)))))

(define-record-type ScanError
  (make-scan-error cause)
  scan-error?
  [cause scan-error-cause])

(def stuck-scan-error (make-scan-error :stuck))
(def eof-scan-error (make-scan-error :eof))

(define-record-type Scanner
  (make-scanner automaton bot-state? states final partition-size partition-bits indices encodings eof-action)
  scanner?
  ;; for debugging only; may be nil; not preserved across scanner->expression
  [automaton scanner-automaton
   ;; says whether state #1 is the after-bot state
   bot-state? scanner-bot-state?
   states scanner-states
   ; array of final states, where each entry is either a regular action or a EolAction record
   final scanner-final
   partition-size scanner-partition-size
   partition-bits scanner-partition-bits
   indices scanner-indices
   encodings scanner-encodings
   eof-action scanner-eof-action])

; internal wrapper to mark actions valid only at eol
(define-record-type EolAction
  (make-eol-action at-eol vanilla)
  eol-action?
  [at-eol eol-action-at-eol
   ;; action valid at the same place, without eol
   vanilla eol-action-vanilla])

(defn- new-bindings+map
  [bindings action->name action new-name]
  (if (contains? action->name action)
    [bindings action->name]
    [(conj bindings new-name action)
     (assoc action->name action new-name)]))

(defn- fill-final-expression
  [^objects final final-name]
  (let [size (count final)]
    (loop [bindings []
           action->name {}
           i 0]
      (if (< i size)
        (if-let [thing (aget final i)]
          (if (eol-action? thing)
            (let [[bindings action->name] (new-bindings+map bindings action->name (eol-action-at-eol thing) (symbol (str "eol" i)))
                  [bindings action->name] (new-bindings+map bindings action->name (eol-action-vanilla thing) (symbol (str "vanilla" i)))]
              (recur bindings action->name (+ 1 i)))
            (let [[bindings action->name] (new-bindings+map bindings action->name thing (symbol (str "action" i)))]
              (recur bindings action->name (+ 1 i))))
          (recur bindings action->name (+ 1 i)))
        ;; next up
        (loop [statements []
               i 0]
          (if (< i size)
            (if-let [thing (aget final i)]
              (if (eol-action? thing)
                (recur (conj statements `(aset ~final-name ~i
                                               (make-eol-action ~(get action->name (eol-action-at-eol thing))
                                                                ~(get action->name (eol-action-vanilla thing)))))
                       (+ 1 i))
                (recur (conj statements `(aset ~final-name ~i
                                               ~(get action->name thing)))
                       (+ 1 i)))
              (recur statements (+ 1 i)))
            ;; ... and one
            `(let ~bindings ~@statements)))))))

(defn- encode-int-array
  [ar]
  ;; work around "method size too large" and string literals < 64k
  (loop [s (str (vec ar))
         ss '()]
    (cond
     (= "" s)
     (if (= (count ss) 1)
       `(int-array (read-string ~(first ss)))
       `(int-array (read-string (string/join [~@(reverse ss)]))))
     
     (> (count s) 65535)
     (recur (subs s 65535)
            (cons (subs s 0 65535) ss))

     :else
     (recur "" (cons s ss)))))

(defn scanner->expression
  [scanner]
  (let [final (scanner-final scanner)]
    `(let [~'final (object-array ~(count final))
           ~'scanner (make-scanner nil
                                 ~(scanner-bot-state? scanner)
                                 ~(encode-int-array (scanner-states scanner))
                                 ~'final
                                 ~(scanner-partition-size scanner)
                                 ~(scanner-partition-bits scanner)
                                 ~(encode-int-array (scanner-indices scanner))
                                 ~(encode-int-array (scanner-encodings scanner))
                                 ~(scanner-eof-action scanner))]
       ~(fill-final-expression final 'final)
       ~'scanner)))

(defn write-scanner-ns
  [scanner ns-name reqs writer-arg]
  (with-open [writer (clojure.java.io/writer writer-arg)]
    (binding [*out* writer]
      (pr `(ns ~ns-name
             (:require [clojure.string :as ~'string]
                       [active.ephemerol.scanner-run :refer :all]
                       ~@reqs)))
      (println)
      (println `(declare ~'scanner ~'scan-one))
      (pr `(def ~'scanner ~(scanner->expression scanner)))
      (println)
      (pr `(def ~'scan-one (make-scan-one ~'scanner)))
      (println)
      (pr `(defn ~'scan
             [x#]
             (with-open [r# (clojure.java.io/reader x#)]
               (scan-to-list ~'scan-one (reader->list r#) (make-position 1 0)))))
      (println))))

(defn reverse-list->string
  [rlis]
  (let [sb (StringBuilder. (count rlis))]
    (loop [rlis rlis]
      (if (empty? rlis)
        (do
          (.reverse sb)
          (.toString sb))
        (do
          (.appendCodePoint sb ^int (first rlis))
          (recur (rest rlis)))))))

(define-record-type ScanResult
  (make-scan-result data input input-position)
  scan-result?
  [data scan-result-data ; holds the return in the end either scan-error or empty list
   input scan-result-input              ; the rest of input
   input-position scan-result-input-position])

(defn make-scan-one
  [scanner]

  (let [^ints states (scanner-states scanner)
	bot-state? (scanner-bot-state? scanner)
	^objects final (scanner-final scanner)
	partition-size (scanner-partition-size scanner)
	bits (scanner-partition-bits scanner)
	^ints indices (scanner-indices scanner)
	^ints encodings (scanner-encodings scanner)
	eof-action (scanner-eof-action scanner)]

    (let [mask (- (bit-shift-left 1 bits) 1)
	  state-size (+ 1 partition-size)
          scalar-value->class (fn [sc]
                                (aget encodings
                                      (+ (aget indices
                                               (bit-shift-right sc bits))
                                         (bit-and sc mask))))
          state-next (fn [state-index sc]
                       (let [class (scalar-value->class sc)]
                         (if (= class -1)
                           -1
                           (loop [state-index state-index]
                             (let [base (* state-index state-size)
                                   next-index (aget states (+ base class))]
                               (if (= next-index -1)
                                 (let [tunnel-index (aget states (+ base partition-size))]
                                   (if (= tunnel-index -1)
                                     -1
                                     (recur tunnel-index)))
                                 next-index))))))]

      (fn [start-input start-position]
	(let [position (copy-position start-position) ; updated
              ;; lexeme read so far
              lexeme-builder (StringBuilder.)]
	  (loop [state (if (and bot-state?
                                (zero? (position-column position)))
                         1
                         0)
                 ;; to be prepended to port
                 input start-input
                 ;; these are the values for the last final state
                 last-action nil
                 last-lexeme ""
                 last-input '()
                 last-position nil]
	    ;; (write (list 'loop state input (reverse rev-lexeme) last-action (reverse last-rev-lexeme) last-input)) (newline)
	    (cond
             (not-empty input)
             (let [c (int (first input))
                   input (rest input)]
               (update-position! position c)
               (let [new-state (long (state-next state c))]
                 (cond
                  (not= new-state -1)
                  ;; successful transition
                  (do
                    (.appendCodePoint lexeme-builder c)
                    (if-let [action (aget final new-state)]
                      ;; final state
                      (if (eol-action? action) ; EOL action
                        (recur new-state input
                               (if (or (empty? input)
                                       (= linefeed (first input)))
                                 (eol-action-at-eol action)
                                 (eol-action-vanilla action))
                               (.toString lexeme-builder)
                               input (copy-position position))
                        (recur new-state input action (.toString lexeme-builder)
                               input (copy-position position)))
                      ;; non-final state
                      (recur new-state input
                             last-action last-lexeme last-input last-position)))

                  last-action
                  ;; stuck
                  (last-action last-lexeme
                               start-position
                               last-input last-position)
                  :else
                  ;; stuck, no action
                  (make-scan-result stuck-scan-error start-input start-position))))

	     ;; eof
	     last-action
             (last-action last-lexeme
                          start-position
                          last-input last-position)

	     ;; eof at the beginning
	     (zero? (.length lexeme-builder))
             ;; call either the default or user specified eof handler.
             (eof-action "" start-position '() last-position)

             ;; eof, no action
	     :else
             (make-scan-result eof-scan-error start-input start-position)))))))) ;the end position

(defn scan-to-list
  [scan-one input input-position]
  (loop [rev-result '()
         input input
         input-position input-position]
    (if (empty? input)
      [(reverse rev-result) input input-position]
      (let [scan-result (scan-one input input-position)]
        (if-let [data (scan-result-data scan-result)]
          (if (scan-error? data)
            [data (scan-result-input scan-result) (scan-result-input-position scan-result)]
            (recur (cons data rev-result)
                   (scan-result-input scan-result) (scan-result-input-position scan-result)))
          [(reverse rev-result) input input-position])))))

(defn string->list
  [^String str]
  (let [sz (.length str)]
  (loop [i 0
         lis '()]
    (if (< i sz)
      (let [sv (.codePointAt str i)]
        (recur (+ i (Character/charCount sv))
               (cons sv lis)))
      (reverse lis)))))

(defn read-scalar-value
  ^long [^Reader r]
  (let [high (.read r)]
    (if (= -1 high)
     -1
     (let [highc (char high)]
       (if (Character/isHighSurrogate highc)
         (let [next (.read r)]
           (when (= -1 next)
             (throw (IOException. "malformed Unicode encoding")))
           (let [lowc (char next)]
             (when-not (Character/isLowSurrogate lowc)
               (throw (IOException. "malformed Unicode encoding")))
             (Character/toCodePoint highc lowc)))
         high)))))


(defn reader->list
  [^Reader r]
  (loop [rev '()]
    (let [sc (read-scalar-value r)]
      (if (= sc -1)
        (reverse rev)
        (recur (cons sc rev))))))
