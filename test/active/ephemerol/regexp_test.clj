(ns active.ephemerol.regexp-test
  (:require [active.ephemerol.char-set :refer :all]
            [active.ephemerol.regexp :refer :all]
            [clojure.test :refer :all]))

(deftest t-partition
  (is
   (= (set (partition-char-sets (list (char-set \A)
                                      (char-set \B))))
      #{(char-set \A) (char-set \B)}))
  (is
   (= (set (partition-char-sets (list (char-set \A \B)
                                      (char-set \B))))
      #{(char-set \A) (char-set \B)}))

  (is
   (= (set (partition-char-sets (list (char-set \B)
                                      (char-set \A \B))))
      #{(char-set \A) (char-set \B)}))

  (is
   (= (set (partition-char-sets (list (char-set \A \B \C)
                                      (char-set \B))))
      #{(char-set \A \C) (char-set \B)}))

  (is
   (= (set (partition-char-sets (list (char-set \A \B \C)
                                      (char-set \A \B \D))))
      #{(char-set \A \B) (char-set \C) (char-set \D)}))

  (is
   (= (set (partition-char-sets (list (char-set \A \B \C)
                                      (char-set \A \B \D)
                                      (char-set \X \Y)))) 
      #{(char-set \A \B)
        (char-set \C)
        (char-set \D)
        (char-set \X \Y)}))

  (is
   (= (set (partition-char-sets (list (char-set \A \B \C)
                                      (char-set \A \B \D)
                                      (char-set \B \E))))
      #{(char-set \A)
        (char-set \B)
        (char-set \C)
        (char-set \D)
        (char-set \E)}))

  (is
   (= 1
      (count (partition-char-sets (list (char-set \A) (char-set \A)))))))

(deftest t-=
  (is (= the-epsilon the-epsilon))
  (is (= #{1 2 3} #{3 2 1}))
  (is (not (= #{1 2} #{1})))
  (is (= (repetition 0 false \A)
         (repetition 0 false \A)))
  (is (not (= (repetition 0 false \A)
              (repetition 0 false \B))))
  (is (not (= (repetition 0 1 \A)
              (repetition 0 2 \A))))
  (is (= (repetition 0 5 (char-set \A))
         (repetition 0 5 (char-set \A))))
  (is (= (union (char-set \A)
                "ABC")
         (union (char-set \A)
                (concatenation \A \B \C))))
  (is (not (= (union (char-set \A)
                     "ABCD")
              (union (char-set \A)
                     (concatenation \A \B \C))))))

(deftest t-regexp-accepts-empty?
  (is (not (regexp-accepts-empty? (char-set \A))))
  (is (not (regexp-accepts-empty? #{0})))
  (is (regexp-accepts-empty? (concatenation)))
  (is (regexp-accepts-empty? the-epsilon))
  (is (regexp-accepts-empty? (union (concatenation) \A)))
  (is (regexp-accepts-empty? (concatenation the-epsilon
                                            (union the-epsilon
                                                   \A))))
  (is (not (regexp-accepts-empty? (concatenation \B \A))))
  (is (regexp-accepts-empty? (repetition 0 false (union \B \A))))
  (is (not (regexp-accepts-empty? (repetition 1 false (union \B \A))))))

(deftest t-regexp-after
  (is
   (= the-empty-set
      (regexp-after the-epsilon \A)))
  (is
   (= the-epsilon
      (regexp-after (regexp \A) \A)))
  (is
   (= the-empty-set
      (regexp-after (regexp \B) \A)))

  (is
   (= (regexp \B)
      (regexp-after (concatenation \A \B) \A)))
  
  (is
   (= (repetition 0 false \A)
      (regexp-after (repetition 0 false \A) \A)))

  (is
   (= the-empty-set
      (regexp-after (repetition 0 false \A) \B)))
  
  (is
   (= the-epsilon
      (regexp-after (union \A \B) \A)))
  
  (is
   (=
    (regexp \C)
    (regexp-after (union "AC" \B) \A)))
  
  (is
   (= (union \B \C)
      (regexp-after (union "AB" "AC") \A))))

(deftest t-regexp-next
  (is
   (= char-set:empty
      (char-regexp-next the-epsilon)))
  (is
   (= (char-set \A)
      (char-regexp-next (regexp \A))))

  (is
   (= (char-set \A)
      (char-regexp-next (concatenation \A \B))))

  (is
   (= (char-set \A)
      (char-regexp-next (repetition 0 false \A))))

  (is
   (= (char-set \A \B)
      (char-regexp-next (union \A \B))))

  (is
   (= (char-set \A \B)
      (char-regexp-next (union "AC" \B))))

  (is
   (= (char-set \A \B)
      (char-regexp-next (concatenation (union the-epsilon \A)
                                       \B))))

  (is
   (= (char-set \A)
      (char-regexp-next (union "AB" "AC")))))


(deftest t-re
  (is
   (= (concatenation \A \B \C)
      (re (/ \A \B \C))))
  (is
   (= (re (* "ABC"))
      (repetition 0 false "ABC")))
  (is
   (= (repetition 0 1 "ABC")
      (re (? "ABC"))))
  (is
   (= (repetition 1 false "ABC")
      (re (+ "ABC"))))
  (is
   (= (union "ABC" "CDE")
      (re (or "ABC" "CDE"))))
  (let [foo (re "ABC")]
    (is (= foo (re ,foo))))
  (is
   (= (regexp "ABC") (re "ABC")))
  (is
   (= (concatenation (repetition 0 false
                                 (repetition 0 1 "ABC"))
                     (union (repetition 0 1 (repetition 0 false "ABC"))
                            (repetition 1 false (repetition 0 false "ABC"))))
      (re (/ (* (? "ABC"))
             (or (? (* "ABC"))
                 (+ (* "ABC"))))))))

(deftest t-bol
  (is
   (= char-set:empty
      (char-regexp-next (re (% "foo")))))
  (is
   (= (re "foo")
      (regexp-after (re (% "foo")) nil)))
  (is
   (= the-empty-set
      (regexp-after (re (% "foo")) \f)))
  (is
   (= (re "foo")
      (regexp-after (re "foo") nil))))
