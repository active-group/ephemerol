(ns active.ephemerol.scanner-test
  (:require [active.ephemerol.char-set :refer :all]
            [active.ephemerol.regexp :refer :all]
            [active.ephemerol.scanner :refer :all]
            [active.ephemerol.scanner-run :refer :all]
            [clojure.test :refer :all]))

(defn compact-partition-index
  [indices encodings bits c]
  (let [mask (- (bit-shift-left 1 bits) 1)
	scalar-value (int c)]
    (get encodings
         (+ (aget indices
                  (bit-shift-right scalar-value bits))
            (bit-and scalar-value mask)))))

(deftest compact-table
  (let [part
        (list
         (char-set \A)
         (char-set \E)
         (char-set \a)
         (char-set \B \D)
         (char-set \C \b))
        [indices encodings] (partition->compact-table part 8)]
    (is (= 0 (compact-partition-index indices encodings 8 \A)))
    (is (= 1 (compact-partition-index indices encodings 8 \E)))
    (is (= 2 (compact-partition-index indices encodings 8 \a)))
    (is (= 3 (compact-partition-index indices encodings 8 \B)))
    (is (= 3 (compact-partition-index indices encodings 8 \D)))
    (is (= 4 (compact-partition-index indices encodings 8 \C)))
    (is (= 4 (compact-partition-index indices encodings 8 \b)))
    (is (= -1 (compact-partition-index indices encodings 8 \F)))
    (is (= -1 (compact-partition-index indices encodings 8 \G)))
    (is (= -1 (compact-partition-index indices encodings 8 \0)))
    (is (= -1 (compact-partition-index indices encodings 8 \c)))))

; running example from Grosch's paper

(def grosch-scanner-spec
  (scanner-spec
   ((/ char-set:letter
       (* (or char-set:letter char-set:digit)))
    (fn [lexeme position input input-position]
      [[:ident-symbol lexeme]
       input input-position]))
   ((+ char-set:digit)
    (fn [lexeme position input input-position]
      [[:decimal-symbol (Integer/parseInt lexeme)]
       input input-position]))
   ((/ (+ (char-set \0 \1 \2 \3 \4 \5 \6 \7))
       \Q)
    (fn [lexeme position input input-position]
      [[:octal-symbol
        (Integer/parseInt
         (.substring lexeme 0 (- (count lexeme) 1))
         8)]
       input input-position]))
   ("BEGIN"
    (fn [lexeme position input input-position]
      [[:begin-symbol nil]
       input input-position]))
   ("END"
    (fn [lexeme position input input-position]
      [[:end-symbol nil]
       input input-position]))
   (":="
    (fn [lexeme position input input-position]
      [[:assign-symbol nil]
       input input-position]))))

(def grosch-scanner (compute-scanner grosch-scanner-spec))

(def grosch-scan-one (make-scan-one (eval (scanner->expression grosch-scanner))))

(deftest grosch-one
  
  (let [check-one
        (fn [input enc rest row col]
          (let [[the-enc the-rest the-pos] (grosch-scan-one (string->list input)
                                                            (make-position 1 0))]
            (is (= enc the-enc))
            (is (= the-rest (string->list rest)))
            (is (= the-pos (make-position row col)))))]

    (check-one "foo A" [:ident-symbol "foo"] " A" 1 3)
    (check-one "BEGIN A" [:begin-symbol nil] " A" 1 5)
    (check-one "END A" [:end-symbol nil] " A" 1 3)
    (check-one ":= A" [:assign-symbol nil] " A" 1 2)
    (check-one "999 A" [:decimal-symbol 999] " A" 1 3)
    (check-one "777Q A" [:octal-symbol 511] " A" 1 4)))


(def test-scanner-spec
  (scanner-spec
   ("A"
    (fn [lexeme position input input-position]
      [[:a nil]
       input input-position]))
   ("ARF"
    (fn [lexeme position input input-position]
      [[:arf nil]
       input input-position]))
   ("ARFL"
    (fn [lexeme position input input-position]
      [[:arfl nil]
       input input-position]))
   ((/ (+ char-set:letter))
    (fn [lexeme position input input-position]
      [[:ident lexeme]
       input input-position]))))

(deftest test-1
  (let [scanner (compute-scanner test-scanner-spec)
        scan-one (make-scan-one (eval (scanner->expression scanner)))
        check-one
        (fn [input enc]
          (let [[the-enc the-rest the-pos] (scan-one (string->list input)
                                                     (make-position 1 0))]
            (is (= enc the-enc))))]

    (check-one "A" [:a nil])
    (check-one "ARF" [:arf nil])
    (check-one "ARFL" [:arfl nil])
    (check-one "ARFLG" [:ident "ARFLG"])
    (check-one "ARG" [:ident "ARG"])
    (check-one "AR" [:ident "AR"])
    (check-one "" nil)))

(def bol-scanner-spec
  (scanner-spec
   ((% "A")
    (fn [lexeme position input input-position]
      [:%a input input-position]))
   ("A"
    (fn [lexeme position input input-position]
      [:a input input-position]))
   (char-set:whitespace 
    (fn [lexeme position input input-position]
      [:whitespace
       input input-position]))))

(deftest bol-1
  (let [scanner (compute-scanner bol-scanner-spec)
        scan-one (make-scan-one (eval (scanner->expression scanner)))
        check-one
        (fn [input enc]
          (let [[output input input-position]
                (scan-to-list scan-one (string->list input) (make-position 1 0))]
            (is (= enc output))))]

    (check-one "AAAA" '(:%a :a :a :a))
    (check-one (str \A \A \newline \A \A \A) '(:%a :a :whitespace :%a :a :a))))

(def eol-scanner-spec
  (scanner-spec
   (($ (% "A"))
    (fn [lexeme position input input-position]
      [:$%a
       input input-position]))
   ((% "A")
    (fn [lexeme position input input-position]
      [:%a
       input input-position]))
   (($ "A")
    (fn [lexeme position input input-position]
      [:$a
       input input-position]))
   ("A"
    (fn [lexeme position input input-position]
      [:a
       input input-position]))
   (char-set:whitespace 
    (fn [lexeme position input input-position]
      [:whitespace
       input input-position]))))

(deftest eol-1
  (let [scanner (compute-scanner eol-scanner-spec)
        scan-one (make-scan-one (eval (scanner->expression scanner)))
        check-one
        (fn [input enc]
          (let [[output input input-position]
                (scan-to-list scan-one (string->list input) (make-position 1 0))]
            (is (= enc output))))]

    (check-one "AAAA" '(:%a :a :a :$a))
    (check-one (str \A \A \newline \A \newline \A \A \A) '(:%a :$a :whitespace :$%a :whitespace :%a :a :$a))))

(def eof-test-scanner-spec
  (scanner-spec
   ("A"
    (fn [lexeme position input input-position]
      [[:a nil]
       input input-position]))
   ("ARF"
    (fn [lexeme position input input-position]
      [[:arf nil]
       input input-position]))
   ("ARFL"
    (fn [lexeme position input input-position]
      [[:arfl nil]
       input input-position]))
   ((/ (+ char-set:letter))
    (fn [lexeme position input input-position]
      [[:ident lexeme]
       input input-position]))
   (<<eof>>
    (fn [lexeme position input input-position]
      [[:eof lexeme]
       input input-position]))))
	 	

(deftest eof-test-1
  (let [scanner (compute-scanner eof-test-scanner-spec)
        scan-one (make-scan-one (eval (scanner->expression scanner)))
        check-one
        (fn [input enc]
          (let [[the-enc the-rest the-pos] (scan-one (string->list input) (make-position 1 0))]
            (is (= enc the-enc))))]

      (check-one "A" [:a nil])
      (check-one "ARF" [:arf nil])
      (check-one "ARFL" [:arfl nil])
      (check-one "ARFLG" [:ident "ARFLG"])
      (check-one "ARG" [:ident "ARG"])
      (check-one "AR" [:ident "AR"])
      (check-one "" [:eof ""])))

; (define type-list '())
; (define stream-test-scanner-spec
;   (scanner-spec
;    ((+ char-set:whitespace)
;     (fn [lexeme position input input-position)
;       (values (cons 'blank #f)
; 	      input input-position)))
;    ("A"
;     (fn [lexeme position input input-position)
;       (values (cons 'a #f)
; 	      input input-position)))
;    ("ARF"
;     (fn [lexeme position input input-position)
;       (values (cons 'arf #f)
; 	      input input-position)))
;    ("ARFL"
;     (fn [lexeme position input input-position)
;       (values (cons 'arfl #f)
; 	      input input-position)))
;    ((: (+ char-set:letter))
;     (fn [lexeme position input input-position)
;       (if (member lexeme type-list)
; 	  (values (cons 'tdefed lexeme)
; 		  input input-position)
; 	  (values (cons 'ident lexeme)
; 		  input input-position))))
;    ("type"
;     (fn [lexeme position input input-position)
;       (values (cons 'tdef "ast")
; 	      input input-position)))))
; 
; (define input-car car)
; 
; (define input-cdr 
;   (fn [in) 
;     (force (cdr in))))
; 
; (define input-null? 
;   (fn [in)
;     (scan-result? in)))
; 
;  (define do-the-scan
;    (fn [in)
;      (if (input-null? in) 
; 	 '()
; 	 (let* ((data (input-car in))
; 		(cmd (car data))
; 		(lex (cdr data)))
; 	   (if (eq? cmd 'tdef)
; 	       (set! type-list (cons lex type-list)))
; 	   (cons data (do-the-scan (input-cdr in)))))))
; 
; (deftest stream-test-1
;   (check
;    (let* ((scanner (compute-scanner stream-test-scanner-spec))
; 	  (scan-one (make-scan-one (eval (scanner->expression scanner)))))
;      (define (scan-stream input)
;        (scan-to-stream scan-one (string->list input) (make-position 1 0)))
;     
;     
;      (let ((result (do-the-scan (scan-stream "ast id type ast test"))))
;        (list (cond ((assoc 'ident result) => cdr)
; 		   (else #f))
; 	     (cond ((assoc 'tdefed result) => cdr)
; 		   (else #f)))))  => '("ast" "ast")))

