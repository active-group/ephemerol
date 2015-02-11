(ns active.ephemerol.char-set-test
  (:require [active.ephemerol.char-set :refer :all]
            [clojure.test :refer :all]))

;; adapted from Olin Shivers's test suite for SRFI 14

(def vowel? (set (map scalar-value [\a \e \i \o \u])))

(deftest t-char-set?
  (is (not (char-set? 5)))
  (is (char-set? (char-set \a \e \i \o \u))))

(deftest t-char-set=?
  (is (char-set=?))
  (is (char-set=? (char-set)))
  (is (char-set=? (char-set \a \e \i \o \u)
                 (string->char-set "ioeauaiii")))
  (is (not (char-set=? (char-set \e \i \o \u)
                      (string->char-set "ioeauaiii")))))

(deftest t-char-set<=
  (is (char-set<=))
  (is (char-set<= (char-set)))
  (is (char-set<= (char-set \a \e \i \o \u)
                  (string->char-set "ioeauaiii")))
  (is (char-set<= (char-set \e \i \o \u)
                  (string->char-set "ioeauaiii"))))

(deftest t-char-set-hash
  (let [h (char-set-hash char-set:graphic 100)]
    (is (>= h 0))
    (is (<= h 99))))

(deftest t-char-set-fold
  (is (= 4
         (char-set-fold (fn [c i] (+ i 1)) 0
			(char-set \e \i \o \u \e \e)))))

(deftest t-char-set-for-each
  (let [cs (atom (string->char-set "0123456789"))]
    (char-set-for-each (fn [c] (swap! cs char-set-delete c))
		       (string->char-set "02468000"))
    (is (char-set=? (string->char-set "97531")
                   @cs)))

  (let [cs (atom (string->char-set "0123456789"))]
    (char-set-for-each (fn [c] (swap! cs char-set-delete c))
		       (string->char-set "02468"))
    (is (not (char-set=? (string->char-set "7531") @cs)))))

(deftest t-char-set
  (is (char-set=? (string->char-set "xy") (char-set \x \y)))
  (is (not (char-set=? (string->char-set "xy") (char-set \x \y \z)))))

(deftest t-coll->char-set
  (is (char-set=? (string->char-set "xy") (coll->char-set [\x \y])))
  (is (not (char-set=? (string->char-set "axy") (coll->char-set [\x \y])))))

(deftest t-char-set-filter
  (is (char-set=? (string->char-set "aeiou")
                 (char-set-filter vowel? char-set:ascii)))
  (is (not (char-set=? (string->char-set "aeou")
                      (char-set-filter vowel? char-set:ascii)))))

(deftest t-range->char-set
  (is (char-set=? (string->char-set "abcdef")
                 (range->char-set 97 103)))
  (is (not (char-set=? (string->char-set "abcef")
                      (range->char-set 97 103)))))

(deftest t-->char-set
  (is (char-set=? (->char-set \x) (->char-set "x")))
  (is (char-set=? (->char-set \x)  (->char-set (char-set \x))))
  (is (not (char-set=? (->char-set \x) (->char-set "y")))))

(deftest t-char-set-size
  (is (= 10
         (char-set-size (char-set-intersection char-set:ascii char-set:digit)))))

(deftest t-char-set-count
  (is (= 5
         (char-set-count vowel? char-set:ascii))))

(deftest t-char-set->list
  (is (= (list (scalar-value \x)) (char-set->list (char-set \x))))
  (is (not (= (list (scalar-value \X)) (char-set->list (char-set \x))))))

(deftest t-char-set-contains?
  (is (char-set-contains? (->char-set "xyz") \x))
  (is (not (char-set-contains? (->char-set "xyz") \a))))

(defn char-lower-case?
  [c]
  (Character/isLowerCase c))

(deftest t-char-set-every
  (is (char-set-every char-lower-case? (->char-set "abcd")))
  (is (not (char-set-every char-lower-case? (->char-set "abcD")))))

(deftest t-char-set-any
  (is (char-set-any char-lower-case? (->char-set "abcd")))
  (is (not (char-set-any char-lower-case? (->char-set "ABCD")))))

(deftest t-char-set-adjoin
  (is (char-set=? (->char-set "123xa")
                 (char-set-adjoin (->char-set "123") \x \a)))
  (is (not (char-set=? (->char-set "123x")
                      (char-set-adjoin (->char-set "123") \x \a)))))

(deftest t-char-set-delete
  (is (char-set=? (->char-set "13")
                 (char-set-delete (->char-set "123") \2 \a \2)))
  (is (not (char-set=? (->char-set "13a")
                      (char-set-delete (->char-set "123") \2 \a \2)))))

(deftest t-char-set-intersection
  (is
   (char-set=? (->char-set "abcdefABCDEF")
              (char-set-intersection char-set:hex-digit (char-set-complement char-set:digit)))))

(deftest t-char-set-union
  (is
   (char-set=? 
    (->char-set "abcdefABCDEFghijkl0123456789")
    (char-set-union char-set:hex-digit
                    (->char-set "abcdefghijkl")))))

(deftest t-char-set-difference
  (is
   (char-set=?
    (->char-set "ghijklmn")
    (char-set-difference (->char-set "abcdefghijklmn")
                         char-set:hex-digit))))

(deftest t-char-set-xor
  (is
   (char-set=?
   (->char-set "abcdefABCDEF")
   (char-set-xor (->char-set "0123456789")
		 char-set:hex-digit))))

(deftest t-char-set-diff+intersection
  (let [[d i] (char-set-diff+intersection char-set:hex-digit
                                          char-set:letter)]
      (is (char-set=? (->char-set "0123456789") d))
      (is (char-set=? (->char-set "abcdefABCDEF") i)))
  (let [[d i] (char-set-diff+intersection (char-set-union char-set:letter
                                                          char-set:digit)
                                          char-set:letter)]
    (is (char-set=? char-set:digit d))
    (is (char-set=? char-set:letter i))))

; The following stuff was adapted from the suite Matthew Flatt wrote
; for PLT Scheme

(deftest t-char-set:lower-case
  (is (char-set-contains? char-set:lower-case \a))
  (is (not (char-set-contains? char-set:lower-case \A)))
  (is (char-set-contains? char-set:lower-case (char 0x00E0)))
  (is (not (char-set-contains? char-set:lower-case (char 0x00C2))))
  (is (char-set-contains? char-set:lower-case (char 0x00B5))))

(deftest t-char-set:upper-case
  (is (char-set-contains? char-set:upper-case \A))
  (is (not (char-set-contains? char-set:upper-case \a)))
  (is (char-set-contains? char-set:upper-case (char 0x00C2)))
  (is (not (char-set-contains? char-set:upper-case (char 0x00E0)))))

(deftest t-char-set:title-case
  (is (char-set-contains? char-set:title-case (char 0x01C5)))
  (is (char-set-contains? char-set:title-case (char 0x1FA8)))
  (is (not (char-set-contains? char-set:title-case \a)))
  (is (not (char-set-contains? char-set:title-case \A))))

(deftest t-char-set:letter
  (is (char-set-contains? char-set:letter \a))
  (is (char-set-contains? char-set:letter \A))
  (is (not (char-set-contains? char-set:letter \1)))
  (is (char-set-contains? char-set:letter (char 0x00AA)))
  (is (char-set-contains? char-set:letter (char 0x00BA))))

(deftest t-char-set:lower-case-2
  (is (not (char-set-every (fn [c] (char-set-contains? char-set:lower-case c)) char-set:letter)))
  (is (char-set-any (fn [c] (char-set-contains? char-set:lower-case c)) char-set:letter)))

(deftest t-char-set:upper-case-2
  (is (not (char-set-every (fn [c] (char-set-contains? char-set:upper-case c)) char-set:letter)))
  (is (char-set-any (fn [c] (char-set-contains? char-set:upper-case c)) char-set:letter)))

;; Not true?
;; (test #t char-set<= char-set:letter (char-set-union char-set:lower-case char-set:upper-case char-set:title-case))

(deftest t-char-set:digit
  (is (char-set-contains? char-set:digit \1))
  (is (not (char-set-contains? char-set:digit \a))))

(deftest t-char-set:hex-digit
  (is (char-set-contains? char-set:hex-digit \1))
  (is (char-set-contains? char-set:hex-digit \a))
  (is (char-set-contains? char-set:hex-digit \A))
  (is (not (char-set-contains? char-set:hex-digit \g))))

(deftest t-char-set:letter+digit
  (is (char-set-contains? char-set:letter+digit \1))
  (is (char-set-contains? char-set:letter+digit \a))
  (is (char-set-contains? char-set:letter+digit \z))
  (is (char-set-contains? char-set:letter+digit \A))
  (is (char-set-contains? char-set:letter+digit \Z)))

(deftest t-char-set:letter-size 
  (is (= 92496 (char-set-size char-set:letter))))

(deftest t-char-set:letter-2
  (is (char-set=? char-set:letter+digit 
                 (char-set-union char-set:letter char-set:digit)))
  (is (not (char-set-every (fn [c] (char-set-contains? char-set:letter c)) char-set:letter+digit)))
  (is (not (char-set-every (fn [c] (char-set-contains? char-set:digit c)) char-set:letter+digit)))
  (is (char-set-any (fn [c] (char-set-contains? char-set:letter c)) char-set:letter+digit)))

(deftest t-char-set:letter+digit-2
  (is (char-set-every (fn [c] (char-set-contains? char-set:letter+digit c)) char-set:letter))
  (is (char-set-every (fn [c] (char-set-contains? char-set:letter+digit c)) char-set:digit)))

(def char-set:latin-1 (range->char-set 0 256))

(deftest t-char-set:latin-1 
  (is
   (char-set=?
    (char-set-intersection char-set:graphic char-set:latin-1)
    (char-set-intersection (char-set-union char-set:letter char-set:digit char-set:punctuation char-set:symbol)
                           char-set:latin-1))))

(deftest t-char-set:printing
  (is
   (char-set=? char-set:printing
              (char-set-union char-set:graphic char-set:whitespace))))

(deftest t-char-set:whitespace
  (is (char-set-contains? char-set:whitespace (char 0x0009)))
  (is (char-set-contains? char-set:whitespace (char 0x000D)))
  (is (not (char-set-contains? char-set:whitespace \a))))

(deftest t-char-set:iso-control
  (is (char-set=? char-set:iso-control 
                 (char-set-union (range->char-set 0x0000 0x0020)
			      (range->char-set 0x007F 0x00A0)))))

(deftest t-char-set:punctuation 
  (is (char-set-contains? char-set:punctuation \!))
  (is (char-set-contains? char-set:punctuation (char 0x00A1)))
  (is (not (char-set-contains? char-set:punctuation \a))))

(deftest t-char-set:symbol
  (is (char-set-contains? char-set:symbol \$))
  (is (char-set-contains? char-set:symbol (char 0x00A2)))
  (is (not (char-set-contains? char-set:symbol \a))))

(deftest t-char-set:blank
  (is (char-set-contains?  char-set:blank \space))
  (is (char-set-contains?  char-set:blank (char 0x3000)))
  (is (not (char-set-contains?  char-set:blank \a))))

;; General procedures ----------------------------------------

(deftest t-char-set=?-2
  (is (char-set=? char-set:letter char-set:letter char-set:letter))
  (is (not (char-set=? char-set:letter char-set:digit)))
  (is (not (char-set=? char-set:letter char-set:letter char-set:digit)))
  (is (not (char-set=? char-set:letter char-set:digit char-set:letter))))

(deftest t-char-set<=-2
  (is (char-set<= char-set:graphic char-set:printing))
  (is (not (char-set<= char-set:printing char-set:graphic)))
  (is (char-set<= char-set:graphic char-set:printing char-set:full))
  (is (not (char-set<= char-set:graphic char-set:full char-set:printing))))

(deftest t-char-set-hash-2
  (is (= (char-set-hash char-set:graphic)
	 (char-set-hash char-set:graphic))))

;; Iterating over character sets ----------------------------------------

;; The number 290 comes from "grep Nd UnicodeData.txt | wc -l"
(deftest t-char-set-size-2
  (is (= 290 (char-set-size char-set:digit))))

(deftest t-char-set-for-each-2
  (is
   (char-set=? char-set:digit
              (let [cs (atom char-set:empty)]
                (char-set-for-each 
                 (fn [c]
                   (swap! cs char-set-adjoin c))
                 char-set:digit)
                @cs))))

;; Creating character sets ----------------------------------------

(deftest t-abc
  (let [abc (char-set \a \b \c)]
    (is (char-set=? abc
                   (char-set \c \a \b)))
    (is (char-set=? abc (string->char-set "cba")))
    (is (char-set=? (char-set \b) (char-set-filter (fn [c] (= c (scalar-value \b))) abc)))
    (is (char-set=? abc (->char-set "abc")))
    (is (char-set=? abc (->char-set abc)))
    (is (char-set=? (char-set \a) (->char-set \a)))))

(deftest t-range->char-2
  (is
   (char-set=? (range->char-set 0 0x20000)
              (char-set-union (range->char-set 0 0xD800)
                              (range->char-set 0xE000 0x20000))))
  (is
   (char-set=?
    (range->char-set 0 0xD801)
    (range->char-set 0 0xD800)))
  (is
   (char-set=?
    (range->char-set 0 0xDFFF)
    (range->char-set 0 0xD800)))
  (is
   (char-set=?
    char-set:empty
    (range->char-set 0xD800 0xD810)))
  (is
   (char-set=?
    (range->char-set 0xD810 0xE000)
    char-set:empty))
  (is
   (char-set=?
    (range->char-set 0xE000 0xE001)
    (range->char-set 0xD810 0xE001)))
  (is
   (char-set=?
    (char-set (char 0xD7FF) (char 0xE000))
    (range->char-set 0xD7FF 0xE001))))

;; Querying character sets ------------------------------

(deftest t-char-set-count-2
  (is
   (= 3
      (char-set-count (fn [x]
                        (and (<= (scalar-value \0) x)
                             (<= x (scalar-value \2))))
                      char-set:digit))))

(deftest t-coll->char-set-2
  (is (char-set=?
       char-set:digit
       (coll->char-set (char-set->list char-set:digit)))))


