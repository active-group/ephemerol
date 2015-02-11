(ns active.ephemerol.inversion-list-test
  (:require [active.ephemerol.inversion-list :refer :all]
            [clojure.test :refer :all]))

(deftest creation-membership
  (is (not (inversion-list-member? 5 (make-empty-inversion-list 0 1000))))
  (is (inversion-list-member? 5 (number->inversion-list 0 1000 5)))
  (is (not (inversion-list-member? 4 (number->inversion-list 0 1000 5))))
  (is (not (inversion-list-member? 6 (number->inversion-list 0 1000 5))))
  (is (not (inversion-list-member? 6 (range->inversion-list 0 1000 500 1000))))
  (is (not (inversion-list-member? 499 (range->inversion-list 0 1000 500 1000))))
  (is (inversion-list-member? 500 (range->inversion-list 0 1000 500 1000)))
  (is (inversion-list-member? 1000 (range->inversion-list 0 1000 500 1000))))

(deftest equality
  (is (= (range->inversion-list 0 1000 5 10)
         (range->inversion-list 0 1000 5 10)))
  (is (not (= (range->inversion-list 0 1000 5 10)
              (range->inversion-list 0 1001 5 10))))
  (is (not (= (range->inversion-list 0 1000 5 10)
              (range->inversion-list 1 1000 5 10))))
  (is (not (= (range->inversion-list 0 1000 5 10)
              (range->inversion-list 0 1000 6 10))))
  (is (not (= (range->inversion-list 0 1000 5 10)
              (range->inversion-list 0 1000 5 11)))))

(deftest complement-1
  (is
   (inversion-list=?
   (range->inversion-list 0 1000 5 10)
   (inversion-list-complement
    (inversion-list-complement
     (range->inversion-list 0 1000 5 10))))))

(deftest complement-2
  (is
   (inversion-list=?
    (range->inversion-list 0 1000 0 1000)
    (inversion-list-complement
     (inversion-list-complement
      (range->inversion-list 0 1000 0 1000))))))

(deftest union-1
  (is
   (inversion-list=?
    (ranges->inversion-list 0 1000 [5 10] [20 30])
    (inversion-list-union (range->inversion-list 0 1000 5 10)
                          (range->inversion-list 0 1000 20 30)))))

(deftest union-2
  (is
   (inversion-list=?
   (range->inversion-list 0 1000 5 10)
   (inversion-list-union (range->inversion-list 0 1000 5 10)
			 (range->inversion-list 0 1000 7 8)))))
  
(deftest union-3
   (is
    (inversion-list=?
     (range->inversion-list 0 1000 5 15)
     (inversion-list-union (range->inversion-list 0 1000 5 10)
                           (range->inversion-list 0 1000 7 15)))))

(deftest union-4
  (is
   (inversion-list=?
    (range->inversion-list 0 1000 0 1000)
    (inversion-list-union (range->inversion-list 0 1000 500 1000)
                          (range->inversion-list 0 1000 0 500)))))

(deftest intersection-1
  (is
   (inversion-list=?
    (make-empty-inversion-list 0 1000)
    (inversion-list-intersection (range->inversion-list 0 1000 5 10)
                                 (range->inversion-list 0 1000 20 30)))))
  
(deftest intersection-2
   (is
    (inversion-list=?
     (range->inversion-list 0 1000 7 8)
     (inversion-list-intersection (range->inversion-list 0 1000 5 10)
                                  (range->inversion-list 0 1000 7 8)))))

(deftest intersection-3
  (is
   (inversion-list=?
    (range->inversion-list 0 1000 7 10)
    (inversion-list-intersection (range->inversion-list 0 1000 5 10)
                                 (range->inversion-list 0 1000 7 15)))))

(deftest intersection-4
  (is
   (inversion-list=?
    (range->inversion-list 0 1000 500 501)
    (inversion-list-intersection (range->inversion-list 0 1000 500 1000)
                                 (range->inversion-list 0 1000 0 501)))))

(deftest intersection-5
   (is
    (inversion-list=?
     (inversion-list-intersection (range->inversion-list 0 1000 500 1000)
                                  (range->inversion-list 0 1000 501 505))
     (range->inversion-list 0 1000 501 505))))

(deftest adjoin
  (is
   (inversion-list=?
    (range->inversion-list 0 1000 0 1000)
    (inversion-list-adjoin (range->inversion-list 0 1000 0 999) 999))))

(deftest iremove
  (is
   (inversion-list=?
    (range->inversion-list 0 1000 0 999)
    (inversion-list-remove (range->inversion-list 0 1000 0 1000) 999))))

(deftest size
  (is
   (= 510
      (inversion-list-size
       (ranges->inversion-list 0 1000 [5 10] [15 20] [500 1000])))))

(deftest fold-done?
  (is
   (= 250781
      (inversion-list-fold-done?
       (fn [n sum]
         (+ n sum))
       0
       (fn [sum]
         (> sum 250000))
       (ranges->inversion-list 0 1000 [5 10] [15 20] [500 1000])))))

(deftest ihash
  (is
   (not=
    (inversion-list-hash (ranges->inversion-list 0 1000 [5 10] [500 1000]) 1031)
    (inversion-list-hash (ranges->inversion-list 0 1000 [5 10] [15 20] [500 1000]) 1031)))
  (is
   (not=
    (hash (ranges->inversion-list 0 1000 [5 10] [500 1000]))
    (hash (ranges->inversion-list 0 1000 [5 10] [15 20] [500 1000])))))

