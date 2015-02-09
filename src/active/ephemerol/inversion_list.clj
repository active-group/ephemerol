(ns active.ephemerol.inversion-list
  (:require [active.clojure.record :refer :all]
            [active.clojure.condition :refer (guard raise)]
            [active.clojure.condition :as c]))

; This was taken from Chapter 13 of Richard Gillam: Unicode Demystified.
; It is based on similar code in Scheme 48
; Mike doesn't know what the original source is.

(define-record-type InversionList
  (make-inversion-list min max
		       range-vector)
  inversion-list?
  ;; minimum element, needed for complement & difference
  [min inversion-list-min
   ;; maximum element, needed size
   ;; we pretty much assume consistency for union / intersection for MIN and MAX
   max inversion-list-max
   ;; consecutive elements are paired to form ranges of the form
   ;; [ (aget v i) (aget v (+ 1 i)) )
   ;; (except the last one, possibly)
   range-array inversion-list-range-array])

(defn make-empty-inversion-list
  [min max]
  (InversionList. min max (int-array 0)))

(defn inversion-list-member?
  [n i-list]
  (let [^ints ranges (inversion-list-range-array i-list)]
    (loop [low 0
           high (alength ranges)]
      (if (< low high)
        (let [mid (quot (+ low high) 2)]
          (if (>= n (aget ranges mid))
            (recur (+ 1 mid) high)
            (recur low mid)))
        (odd? high)))))

;; Utilities

(definline array-copy!
  [source source-start dest dest-start count]
  `(System/arraycopy ~source ~source-start ~dest ~dest-start ~count))

(defn inversion-list-complement
  [i-list]
  (let [^ints ranges (inversion-list-range-array i-list)
        min (inversion-list-min i-list)
	max (inversion-list-max i-list)
        size (alength ranges)]
    (InversionList.
     min max
     (cond
      (zero? size)
      (int-array 1 min)

      (not (= min (aget ranges 0)))
      (if (and (even? size)
               (= max (aget ranges (- size 1))))
        (let [result (int-array size)]
          (aset-int result 0 min)
          (array-copy! ranges 0 result 1 (- size 1))
          result)
        (let [result (int-array (+ 1 size))]
          (aset-int result 0 min)
          (array-copy! ranges 0 result 1 size)
          result))

      (and (even? size)
           (= max (aget ranges (- size 1))))
      (let [result (int-array (- size 2))]
        (array-copy! ranges 1 result 0 (- size 2))
        result)

      :else
      (let [result (int-array (- size 1))]
        (array-copy! ranges 1 result 0 (- size 1))
        result)))))

(defn make-inversion-list-union-intersection
  [proc-thunk                           ; for CALL-ERROR
   write-increment-count write-decrement-count
   process-first? decrement-count?
   middle-increment
   copy-extra-count]

  (fn [i-list-1 i-list-2]
    (when (or (not= (inversion-list-min i-list-1)
                    (inversion-list-min i-list-2))
              (not= (inversion-list-max i-list-1)
                    (inversion-list-max i-list-2)))
      (c/assertion-violation `make-inversion-list-union-intersection
			     "min/max mismatch" (proc-thunk) i-list-1 i-list-2))
    (let [^ints ranges-1 (inversion-list-range-array i-list-1)
	  ^ints ranges-2 (inversion-list-range-array i-list-2)
	  min (inversion-list-min i-list-1)
	  max (inversion-list-max i-list-1)

          size-1 (alength ranges-1)
          size-2 (alength ranges-2)
          
          temp (int-array (+ size-1 size-2))]

      (loop [index-1 0
             index-2 0
             count 0
             index-result 0]

        (if (and (< index-1 size-1)
                 (< index-2 size-2))
          (let [el-1 (aget ranges-1 index-1)
                el-2 (aget ranges-2 index-2)

                [index el index-1 index-2]
                (if (or (< el-1 el-2)
                        (and (= el-1 el-2)
                             (process-first? index-1)))
                  [index-1 el-1 (+ 1 index-1) index-2]
                  [index-2 el-2 index-1 (+ 1 index-2)])]

            (if (even? index)
              (if (= write-increment-count count)
                (do
                  (aset-int temp index-result el)
                  (recur index-1 index-2 (+ 1 count) (+ 1 index-result)))
                (recur index-1 index-2 (+ 1 count) index-result))
              (if (= write-decrement-count count)
                (do
                  (aset-int temp index-result el)
                  (recur index-1 index-2 (- count 1) (+ 1 index-result)))
                (recur index-1 index-2 (- count 1) index-result))))
          (let [count
                (if (or (and (not (= index-1  size-1))
                             (decrement-count? index-1))
                        (and (not (= index-2 size-2))
                             (decrement-count? index-2)))
                  (+ count middle-increment)
                  count)
                result-size
                (if (= copy-extra-count count)
                  (+ index-result
                     (- size-1 index-1)
                     (- size-2 index-2))
                  index-result)
                result (int-array result-size)]
            (array-copy! temp 0 result 0 index-result)
            (if (= copy-extra-count count)
              (do
                (array-copy! ranges-1 index-1 result index-result
                             (- size-1 index-1))
                (array-copy! ranges-2 index-2 result index-result
                             (- size-2 index-2))))
            (InversionList. min max result)))))))

; for associative procedures only
(defn- binary->n-ary
  [proc2]
  (fn [arg-1 & args]
    (if (and (seq args)
	     (not (empty? (rest args))))
      (proc2 arg-1 (first args))
      (loop [args args
             result arg-1]
        (if (empty? args)
          result
          (recur (rest args) (proc2 result (first args))))))))

(def inversion-list-union
  (binary->n-ary
   (make-inversion-list-union-intersection (fn [] inversion-list-union)
					   0 1 even? odd? -1 0)))
   

(def inversion-list-intersection
  (binary->n-ary
   (make-inversion-list-union-intersection (fn [] inversion-list-intersection)
					   1 2 odd? even? +1 2)))

(def inversion-list-difference
  (binary->n-ary
   (fn [i-list-1 i-list-2]
     (inversion-list-intersection i-list-1
				  (inversion-list-complement i-list-2)))))

(defn number->inversion-list
  [min max n]
  (when (or (< n min)
            (>= n max))
    (c/assertion-violation `number->inversion-list "invalid number"
                         min max n))
  (InversionList. min max
                  (if (= n (- max 1))
                    (int-array [n])
                    (int-array [n (+ n 1)]))))

(defn numbers->inversion-list
  [min max & numbers]
  (cond
   (empty? numbers) (make-empty-inversion-list min max)
   (empty? (rest numbers)) (number->inversion-list min max (first numbers))
   :else
   (loop [numbers (rest numbers)
          i-list (number->inversion-list min max (first numbers))]
     (if (empty? numbers)
       i-list
       (recur (rest numbers)
              (inversion-list-union
               i-list
               (number->inversion-list min max (first numbers))))))))

(defn range->inversion-list
  [min max left right]
  (when (or (> min max)
            (> left right)
            (< left min)
            (> right max))
    (c/assertion-violation `range->inversion-list "invalid range"
                           min max left right))
  (InversionList. min max
                  (if (= right max)
                    (int-array [left])
                    (int-array [left right]))))

(defn ranges->inversion-list
  [min max & ranges]
  (loop [ranges ranges
         result (make-empty-inversion-list min max)]
    (if (empty? ranges)
      result
      (let [range-pair (first ranges)
            left (first range-pair)
            right (second range-pair)]
        (when (not (and (number? left)
                        (number? right)))
          (c/assertion-violation `ranges->inversion-list "invalid range"
                                 min max (cons left right)))
        (recur (rest ranges)
               (inversion-list-union result
                                     (range->inversion-list min max left right)))))))

(defn inversion-list-adjoin
  [i-list & numbers]
  (inversion-list-union i-list
			(apply
			 numbers->inversion-list
			 (inversion-list-min i-list)
			 (inversion-list-max i-list)
			 numbers)))

(defn inversion-list-remove
  [i-list & numbers]
  (inversion-list-difference i-list
			     (apply
			      numbers->inversion-list
			      (inversion-list-min i-list)
			      (inversion-list-max i-list)
			      numbers)))

(defn inversion-list-size
  [i-list]
  (let [ranges (inversion-list-range-array i-list)
        size (alength ranges)]
    (loop [index 0
           count 0]
      (cond
       (>= index size) count
       (= (+ 1 index) size)
       (+ count (- (inversion-list-max i-list)
                   (aget ranges index)))
       :else
       (recur (+ 2 index)
	      (+ count
		 (- (aget ranges (+ 1 index))
		    (aget ranges index))))))))

(defn inversion-list=?
  [i-list-1 i-list-2]
  (and (= (inversion-list-min i-list-1)
	  (inversion-list-min i-list-2))
       (= (inversion-list-max i-list-1)
	  (inversion-list-max i-list-2))
       (java.util.Arrays/equals ^ints (inversion-list-range-array i-list-1)
                                ^ints (inversion-list-range-array i-list-2))))

; bah, no nested loops in clojure
(define-record-type NextFold
  (make-next-fold v i)
  next-fold?
  [v next-fold-v
   i next-fold-i])

; Iterate over the elements until DONE? (applied to the accumulator)
; returns #t
(defn inversion-list-fold-done? 
  [kons knil done? i-list]
  (let [ranges (inversion-list-range-array i-list)
        size (alength ranges)]
    (loop [v knil
           i 0]
      (if (>= i size)
        v
        (let [left (aget ranges i)
              right (if (< i (- size 1))
                      (aget ranges (+ 1 i))
                      (inversion-list-max i-list))]
          (let [res
                ;; argh, no nested loops in Clojure
                (loop [v v
                       n left]
                  (if (>= n right)
                    (NextFold. v (+ 2 i))
                    (let [v (kons n v)]
                      (if (done? v)
                        v
                        (recur v (+ 1 n))))))]
            (if (next-fold? res)
              (recur (next-fold-v res) (next-fold-i res))
              res)))))))

; Uses the same method as Olin's reference implementation for SRFI 14.

(defn inversion-list-hash
  [i-list bound]
  (let [mask (loop [i 0x10000] ; skip first 16 iterations
		(if (>= i bound)
		    (- i 1)
		    (recur (+ i i))))]
    (let [^ints range-array (inversion-list-range-array i-list)
          size (alength range-array)]
      (loop [i 0
             ans 0]
	(if (>= i size)
          (mod ans bound)
          (recur (+ 1 i)
            (bit-and mask
                     (+ (* 37 ans)
                        (aget range-array i)))))))))

