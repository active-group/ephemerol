(ns active.ephemerol.char-set
  (:require [active.clojure.condition :refer (guard raise)]
            [active.clojure.condition :as c]
            [active.ephemerol.inversion-list :refer :all]))

(declare char-set? char-set=? char-set-hash)

(deftype CharSet [simple i-list]
  Object
  (equals
    [this other]
    (and (char-set? other)
         (char-set=? this other)))
  (hashCode
    [this]
    (char-set-hash this)))

(defn char-set?
  [thing]
  (instance? CharSet thing))

(defn char-set-simple
  [^CharSet cs]
  (.simple cs))

(defn char-set-i-list
  [^CharSet cs]
  (.i-list cs))

(defn make-char-set
  [simple i-list]
  (CharSet. simple i-list))

(defn scalar-value
  [thing]
  (cond
   (integer? thing) thing
   (char? thing) (int thing)
   :else
   (c/assertion-violation `scalar-value
                          "does not represent a scalar value"
                          thing)))
   
;;; "Simple Csets"---we use mutable byte vectors for the Latin-1 part

(def ^:private simple-cset-boundary 256)

(defn- simple-char?
  [c]
  (< (scalar-value c) simple-cset-boundary))

(defn- make-empty-simple-cset
  []
  (boolean-array simple-cset-boundary false))

(defn- make-full-simple-cset
  []
  (boolean-array simple-cset-boundary true))

(defn- copy-simple-cset
  [^booleans s]
  (aclone s))

; don't mistake these for abstractions
(defn- simple-cset-code-not-member?
  [^booleans s i]
  (not (aget s i)))

(defn- simple-cset-code-member?
  [^booleans s i]
  (not (simple-cset-code-not-member? s i)))

(defn- simple-cset-ref
  [^booleans s i]
  (aget s i))

(defn- simple-cset-set!
  [^booleans s i v]
  (aset-boolean s i v))

(defn- simple-cset-remove-code!
  [^booleans s i]
  (aset-boolean s i false))

(defn- simple-cset-adjoin-code!
  [^booleans s i]
  (aset-boolean s i true))

(defn- simple-cset-contains?
  [s char]
  (simple-cset-code-member? s (scalar-value char)))

(defn- simple-cset=?
  [^booleans s1 ^booleans s2]
  (java.util.Arrays/equals s1 s2))

(defn- simple-cset<=?
  [^booleans s1 ^booleans s2]
  (or (= s1 s2)
      (loop [i 0]
	(if (>= i simple-cset-boundary)
	    true
	    (and (or (not (simple-cset-ref s1 i)) (simple-cset-ref s2 i))
		 (recur (+ 1 i)))))))

(defn- simple-cset-size
  [^booleans s]
  (loop [i 0
         size 0]
    (if (>= i simple-cset-boundary)
	size
	(recur (+ 1 i) (+ size (if (simple-cset-ref s i) 1 0))))))

(defn- simple-cset-count
  [pred ^booleans s]
  (loop [i 0
         count 0]
    (if (>= i simple-cset-boundary)
	count
	(recur (+ 1 i)
               (if (and (simple-cset-code-member? s i) (pred i))
                 (+ count 1)
                 count)))))

(defn- simple-cset-modify!
  [set ^booleans s chars]
  (doseq [c chars]
    (set s (scalar-value c)))
  s)

(defn- simple-cset-modify
  [set ^booleans s chars]
  (simple-cset-modify! set (copy-simple-cset s) chars))

(defn- simple-cset-adjoin
  [^booleans s & chars]
  (simple-cset-modify simple-cset-adjoin-code! s chars))

(defn- simple-cset-delete 
  [^booleans s & chars]
  (simple-cset-modify simple-cset-remove-code! s chars))

(defn- simple-cset-delete!
  [^booleans s & chars]
  (simple-cset-modify! simple-cset-remove-code! s chars))

(defn- simple-cset-for-each
  [proc ^booleans s]
  (loop [i 0]
    (if (< i simple-cset-boundary)
	(do
	  (when (simple-cset-code-member? s i)
            (proc i))
	  (recur (+ 1 i))))))

(defn- simple-cset-fold
  [kons knil s]
  (loop [i 0
         ans knil]
    (if (>= i simple-cset-boundary)
      ans
      (recur (+ 1 i)
             (if (simple-cset-code-not-member? s i)
               ans
               (kons i ans))))))

(defn- simple-cset-every?
  [pred s]
  (loop [i 0]
    (cond
     (>= i simple-cset-boundary) true

     (or (simple-cset-code-not-member? s i)
         (pred i))
     (recur (+ 1 i))

     :else false)))

(defn- simple-cset-any
  [pred s]
  (loop [i 0]
    (cond
     (>= i simple-cset-boundary) false
     
     (and (simple-cset-code-member? s i)
          (pred (char i)))
     true
     
     :else (recur (+ 1 i)))))

(defn- range->simple-cset
  [lower upper]
  (let [s (make-empty-simple-cset)]
    (loop [i lower]
      (if (< i upper)
        (do
          (simple-cset-adjoin-code! s i)
          (recur (+ 1 i)))))
    s))

; Algebra

; These do various "s[i] := s[i] op val" operations

(defn- simple-cset-invert-code!
  [^booleans s i v]
  (simple-cset-set! s i (not v)))

(defn- simple-cset-and-code!
  [^booleans s i v]
  (when (not v)
    (simple-cset-remove-code! s i)))
(defn- simple-cset-or-code!
  [^booleans s i v]
  (when v
    (simple-cset-adjoin-code! s i)))
(defn- simple-cset-minus-code!
  [^booleans s i v]
  (when v
    (simple-cset-remove-code! s i)))
(defn- simple-cset-xor-code!
  [^booleans s i v]
  (when v
    (simple-cset-set! s i (not (simple-cset-ref s i)))))

;; Boolean array utilities

;;; Apply P to each index and its char code in S: (P I VAL).
;;; Used by the set-algebra ops.

(defn- boolean-array-iter
  [p ^booleans s]
  (loop [i (- (alength s) 1)]
    (if (>= i 0)
	(do
	  (p i (aget s i))
	  (recur (- i 1))))))

(defn- simple-cset-complement!
  [^booleans s]
  (boolean-array-iter (fn [i v] (simple-cset-invert-code! s i v)) s)
  s)

(defn- simple-cset-complement
  [s]
  (simple-cset-complement! (copy-simple-cset s)))

(defn- simple-cset-op!
  [^booleans s simple-csets code-op!]
  (doseq [s2 simple-csets]
    (loop [i 0]
      (if (< i simple-cset-boundary)
        (do
         (code-op! s i (simple-cset-ref s2 i))
         (recur (+ 1 i))))))
  s)

(defn- simple-cset-union!
  [^booleans s1 & ss]
  (simple-cset-op! s1 ss simple-cset-or-code!))

(defn- simple-cset-union
  [& ss]
  (if (empty? ss)
    (make-empty-simple-cset)
    (apply simple-cset-union!
           (copy-simple-cset (first ss))
           (rest ss))))

(defn- simple-cset-intersection!
  [^booleans s1 & ss]
  (simple-cset-op! s1 ss simple-cset-and-code!))

(defn- simple-cset-intersection
  [& ss]
  (if (empty? ss)
    (make-full-simple-cset)
    (apply simple-cset-intersection!
           (aclone (first ss))
           (rest ss))))

(defn- simple-cset-difference!
  [^booleans s1 & ss]
  (simple-cset-op! s1 ss simple-cset-minus-code!))

(defn- simple-cset-difference
  [^booleans s1 & ss]
  (if (empty? ss)
      s1
      (apply simple-cset-difference! (copy-simple-cset s1) ss)))

(defn- simple-cset-xor!
  [s1 & ss]
  (simple-cset-op! s1 ss simple-cset-xor-code!))

(defn- simple-cset-xor
  [& ss]
  (if (empty? ss)
    (make-empty-simple-cset)
    (apply simple-cset-xor!
           (copy-simple-cset (first ss))
           (rest ss))))

(defn- simple-cset-diff+intersection!
  [^booleans s1 ^booleans s2 & ss]
  (boolean-array-iter (fn [i v]
                        (cond
                         (not v) (simple-cset-remove-code! s2 i)
                         
                         (simple-cset-code-member? s2 i) (simple-cset-remove-code! s1 i)))
                      s1)

  (doseq [s ss]
    (boolean-array-iter (fn [i v]
                          (when (and v
                                     (simple-cset-code-member? s1 i))
                            (simple-cset-remove-code! s1 i)
                            (simple-cset-adjoin-code! s2 i)))
                        s))

  [s1 s2])

; Compute (c + 37 c + 37^2 c + ...) modulo BOUND, with sleaze thrown
; in to keep the intermediate values small. (We do the calculation
; with just enough bits to represent BOUND, masking off high bits at
; each step in calculation. If this screws up any important properties
; of the hash function I'd like to hear about it. -Olin)

(defn- simple-cset-hash
  [^booleans s bound]
  ;; The mask that will cover BOUND-1:
  (let [mask (loop [i 0x10000]   ; Let's skip first 16 iterations, eh?
               (if (>= i bound) (- i 1) (recur (+ i i))))]
    (loop [i (- simple-cset-boundary 1)
           ans 0]
      (if (< i 0)
        (mod ans bound)
        (recur (- i 1)
               (if (simple-cset-code-not-member? s i)
                 ans
                 (bit-and mask (+ (* 37 ans) i))))))))

;;; Now for the real character sets

(defn make-empty-char-set
  []
  (make-char-set (make-empty-simple-cset)
		 (make-empty-inversion-list simple-cset-boundary (+ 1 0x10ffff))))
(defn make-full-char-set
  []
  (make-char-set (make-full-simple-cset)
		 (range->inversion-list simple-cset-boundary (+ 1 0x10ffff)
					simple-cset-boundary (+ 1 0x10ffff))))

; binary version
(defn char-set=?-2
  [cs-1 cs-2]
  (and (simple-cset=? (char-set-simple cs-1) (char-set-simple cs-2))
       (inversion-list=? (char-set-i-list cs-1)
			 (char-set-i-list cs-2))))
; n-ary version
(defn char-set=?
  [& css]
  (or (empty? css)
      (let [cs1  (first css)
	    rst (rest css)]
	(loop [rst rst]
	  (or (empty? rst)
              (and (char-set=?-2 cs1 (first rst))
                   (recur (rest rst))))))))

; binary
(defn char-set<=-2
  [cs-1 cs-2]
  (and (simple-cset<=? (char-set-simple cs-1) (char-set-simple cs-2))
       (inversion-list<=? (char-set-i-list cs-1)
			  (char-set-i-list cs-2))))
; n-ary
(defn char-set<=
  [& css]
  (or (empty? css)
      (let [cs1  (first css)
	    rst (rest css)]
	(loop [cs1 cs1 rst rst]
	  (or (empty? rst)
              (and (char-set<=-2 cs1 (first rst))
                   (recur (first rst) (rest rst))))))))


;;; Hash

; We follow Olin's reference implementation:
;
; If you keep BOUND small enough, the intermediate calculations will 
; always be fixnums. How small is dependent on the underlying Scheme system; 
; we use a default BOUND of 2^22 = 4194304, which should hack it in
; Schemes that give you at least 29 signed bits for fixnums. The core 
; calculation that you don't want to overflow is, worst case,
;     (+ 65535 (* 37 (- bound 1)))
; where 65535 is the max character code. Choose the default BOUND to be the
; biggest power of two that won't cause this expression to fixnum overflow, 
; and everything will be copacetic.

(defn char-set-hash
  [cs & [maybe-bound]]
  (let [bound (or maybe-bound  4194304)]
    (when-not (and (integer? bound)
                   (<= 0 bound))
      (c/assertion-violation `char-set-hash "invalid bound" bound))
    (mod (+ (simple-cset-hash (char-set-simple cs) bound)
            (* 37 (inversion-list-hash (char-set-i-list cs) bound)))
         bound)))

(defn char-set-contains?
  [cs char]
  (if (simple-char? char)
      (simple-cset-contains? (char-set-simple cs) char)
      (inversion-list-member? (scalar-value char)
			      (char-set-i-list cs))))

(defn char-set-size
  [cs]
  (+ (simple-cset-size (char-set-simple cs))
     (inversion-list-size (char-set-i-list cs))))

(defn char-set-count
  [pred cset]
  (+ (simple-cset-count pred (char-set-simple cset))
     (inversion-list-count pred (char-set-i-list cset))))

(defn- partition-coll
  [pred lis]
  (let [mp (group-by pred lis)]
    [(mp true) (mp false)]))

(defn make-char-set-char-op
  [simple-cset-op inversion-list-op]
  (fn [cs & chars]
    (let [[simple-chars non-simple-chars] (partition-coll simple-char? chars)]
      (make-char-set (apply simple-cset-op (char-set-simple cs) simple-chars)
                     (apply inversion-list-op (char-set-i-list cs)
                            (map scalar-value non-simple-chars))))))

(def char-set-adjoin
  (make-char-set-char-op simple-cset-adjoin inversion-list-adjoin))

(def char-set-delete
  (make-char-set-char-op simple-cset-delete inversion-list-remove))

(defn char-set-for-each
  [proc cs]
  (simple-cset-for-each proc (char-set-simple cs))
  (inversion-list-fold-done? (fn [n _]
			       (proc n)
			       nil)
			     false
			     (fn [_] false)
                             (char-set-i-list cs)))

(defn char-set-fold
  [kons knil cs]
  (inversion-list-fold-done? kons
			     (simple-cset-fold kons knil (char-set-simple cs))
			     (fn [_] false)
			     (char-set-i-list cs)))

(defn char-set-every
  [pred cs]
  (and (simple-cset-every? pred (char-set-simple cs))
       (inversion-list-fold-done? (fn [n v]
				    (and v (pred n)))
				  true
				  not
				  (char-set-i-list cs))))

(defn char-set-any
  [pred cs]
  (or (simple-cset-any pred (char-set-simple cs))
      (inversion-list-fold-done? (fn [n v]
				   (or v (pred n)))
				 false
				 identity
				 (char-set-i-list cs))))

; converting from and to lists

(defn coll->char-set
  [chars]
  (if (empty? chars)
    (make-empty-char-set)
    (reduce char-set-adjoin (make-empty-char-set) chars)))

(defn char-set 
  [& chars]
  (coll->char-set chars))

(defn char-set->list
  [cs]
  (char-set-fold cons '() cs))

; converting to and from strings

(defn string->char-set
  [str]
  (reduce char-set-adjoin
          (make-empty-char-set)
          str))

(defn range->char-set
  [lower upper]
  (let [lower (scalar-value lower)
        upper (scalar-value upper)]
    (when (neg? lower)
      (c/assertion-violation `range->char-set! "negative lower bound" lower))
    (when (> lower 0x10ffff)
      (c/assertion-violation `range->char-set! "invalid lower bound" lower))
    (when (neg? upper)
      (c/assertion-violation `range->char-set! "negative upper bound" upper))
    (when (> upper 0x110000)
      (c/assertion-violation `range->char-set! "invalid lower bound" upper))
    (when (not (<= lower upper))
      (c/assertion-violation `range->char-set! "decreasing bounds" lower upper))

    (letfn [(create-inversion-list
              [lower upper]
              (cond
               (and (>= lower 0xD800)
                    (>= 0xe000 upper))
               (make-empty-inversion-list simple-cset-boundary (+ 1 0x10ffff))

               (<= upper 0xe000)
               (range->inversion-list simple-cset-boundary (+ 1 0x10ffff)
                                      lower (min 0xd800 upper))

               (>= lower 0xd800)
               (range->inversion-list simple-cset-boundary (+ 1 0x10ffff)
                                      (max 0xe000 lower) upper)
               :else
               ;; hole
               (ranges->inversion-list simple-cset-boundary (+ 1 0x10ffff)
                                       [lower 0xd800]
                                       [0xe000 upper])))]
      (cond
       (>= lower simple-cset-boundary)
       (make-char-set (make-empty-simple-cset)
		      (create-inversion-list lower upper))

       (< upper simple-cset-boundary)
       (make-char-set (range->simple-cset lower upper)
		      (make-empty-inversion-list simple-cset-boundary (+ 1 0x10ffff)))
       :else
       (make-char-set (range->simple-cset lower simple-cset-boundary)
                       (create-inversion-list simple-cset-boundary upper))))))

(defn char-set-filter
  [predicate domain]
  (char-set-fold (fn [ch v]
		   (if (predicate ch)
		       (char-set-adjoin v ch)
                       v))
                 (make-empty-char-set)
		 domain))

; {string, char, char-set, char predicate} -> char-set

(defn ->char-set
  [x]
  (cond 
   (char-set? x) x
   (string? x) (string->char-set x)
   (char? x) (char-set x)
   (integer? x) (char-set x)
   :else (c/assertion-violation `->char-set "Not a charset, string or char." x)))


; Set algebra

(def ^:private surrogate-complement-i-list
  (inversion-list-complement
   (range->inversion-list simple-cset-boundary (+ 1 0x10ffff)
			  0xd800 0xe000)))

(defn char-set-complement
  [cs]
  (make-char-set (simple-cset-complement (char-set-simple cs))
		 (inversion-list-intersection
		  (inversion-list-complement (char-set-i-list cs))
		  surrogate-complement-i-list)))

(defn make-char-set-op
  [simple-cset-op! inversion-list-op]
  (fn [cset1 & csets]
    (let [simple (apply simple-cset-op!
                        (copy-simple-cset (char-set-simple cset1))
                        (map char-set-simple csets))
          ilist (apply inversion-list-op
                       (char-set-i-list cset1)
                       (map char-set-i-list csets))]
      (make-char-set simple ilist))))

(def char-set-union
  (make-char-set-op simple-cset-union! inversion-list-union))

(def char-set-intersection
  (make-char-set-op simple-cset-intersection! inversion-list-intersection))

(def char-set-difference
  (make-char-set-op simple-cset-difference inversion-list-difference))


; Really inefficient for things outside Latin-1
; WHO NEEDS THIS NONSENSE, ANYWAY?
(def char-set-xor
  (make-char-set-op simple-cset-xor! inversion-list-xor))

(defn char-set-diff+intersection 
  [cs1 cs2 & csets]
  (let [s1 (copy-simple-cset (char-set-simple cs1))
        s2 (copy-simple-cset (char-set-simple cs2))
        [simple-diff simple-intersection]
        (apply simple-cset-diff+intersection!
               s1 s2
               (map char-set-simple csets))]
      (let [i-list-1 (char-set-i-list cs1)
            i-list-2 (char-set-i-list cs2)
	    i-list-rest (map char-set-i-list csets)]
        (let [i1 (apply inversion-list-difference
                              i-list-1 i-list-2
                              i-list-rest)
              i2 (inversion-list-intersection
                        i-list-1
                        (apply inversion-list-union
                               i-list-2
                               i-list-rest))]
          [(make-char-set s1 i1)
           (make-char-set s2 i2)]))))

;; Utility for srfi-14-base-char-sets.scm, which follows

; The range vector is an even-sized vector with [lower, upper)
; pairs.

(defn range-vector->char-set
  [range-vector]
  (let [size (count range-vector)
        simple-cset (make-empty-simple-cset)]

    (loop [index 0 
           ranges '()]
      (if (>= index size)
        (make-char-set simple-cset
                       (apply ranges->inversion-list
                              simple-cset-boundary (+ 1 0x10ffff)
                              ranges))
        (let [lower (get range-vector index)
              upper (get range-vector (+ 1 index))
              fill-simple-cset! (fn [lower upper]
                                  (loop [scalar-value lower]
                                    (if (< scalar-value upper)
                                      (do
                                        (simple-cset-adjoin-code! simple-cset scalar-value)
                                        (recur (+ 1 scalar-value))))))]
	    
          (cond
           (>= lower simple-cset-boundary)
           (recur (+ 2 index) (cons [lower upper] ranges))
             
           (< upper simple-cset-boundary)
           (do
             (fill-simple-cset! lower upper)
             (recur (+ 2 index) ranges))
           
           :else
           (do
             (fill-simple-cset! lower simple-cset-boundary)
             (recur (+ 2 index)
                    (cons [simple-cset-boundary upper] ranges)))))))))


; Automatically generated in Scheme 48, then hand-converted here.

(def char-set:lower-case (range-vector->char-set [97 123 181 182 223 247 248 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288 289 290 291 292 293 294 295 296 297 298 299 300 301 302 303 304 305 306 307 308 309 310 311 313 314 315 316 317 318 319 320 321 322 323 324 325 326 327 328 330 331 332 333 334 335 336 337 338 339 340 341 342 343 344 345 346 347 348 349 350 351 352 353 354 355 356 357 358 359 360 361 362 363 364 365 366 367 368 369 370 371 372 373 374 375 376 378 379 380 381 382 385 387 388 389 390 392 393 396 398 402 403 405 406 409 412 414 415 417 418 419 420 421 422 424 425 427 428 429 430 432 433 436 437 438 439 441 443 445 446 447 448 454 455 457 458 460 461 462 463 464 465 466 467 468 469 470 471 472 473 474 475 476 478 479 480 481 482 483 484 485 486 487 488 489 490 491 492 493 494 495 497 499 500 501 502 505 506 507 508 509 510 511 512 513 514 515 516 517 518 519 520 521 522 523 524 525 526 527 528 529 530 531 532 533 534 535 536 537 538 539 540 541 542 543 544 545 546 547 548 549 550 551 552 553 554 555 556 557 558 559 560 561 562 563 570 572 573 575 577 578 579 583 584 585 586 587 588 589 590 591 610 611 618 619 628 629 630 631 641 642 655 656 660 666 667 669 671 672 673 675 684 686 688 837 838 867 880 891 894 912 913 940 975 976 978 981 983 985 986 987 988 989 990 991 992 993 994 995 996 997 998 999 1000 1001 1002 1003 1004 1005 1006 1007 1011 1013 1014 1016 1017 1019 1020 1072 1120 1121 1122 1123 1124 1125 1126 1127 1128 1129 1130 1131 1132 1133 1134 1135 1136 1137 1138 1139 1140 1141 1142 1143 1144 1145 1146 1147 1148 1149 1150 1151 1152 1153 1154 1163 1164 1165 1166 1167 1168 1169 1170 1171 1172 1173 1174 1175 1176 1177 1178 1179 1180 1181 1182 1183 1184 1185 1186 1187 1188 1189 1190 1191 1192 1193 1194 1195 1196 1197 1198 1199 1200 1201 1202 1203 1204 1205 1206 1207 1208 1209 1210 1211 1212 1213 1214 1215 1216 1218 1219 1220 1221 1222 1223 1224 1225 1226 1227 1228 1229 1230 1232 1233 1234 1235 1236 1237 1238 1239 1240 1241 1242 1243 1244 1245 1246 1247 1248 1249 1250 1251 1252 1253 1254 1255 1256 1257 1258 1259 1260 1261 1262 1263 1264 1265 1266 1267 1268 1269 1270 1271 1272 1273 1274 1275 1276 1277 1278 1279 1280 1281 1282 1283 1284 1285 1286 1287 1288 1289 1290 1291 1292 1293 1294 1295 1296 1297 1298 1299 1300 1377 1416 6448 6457 7426 7427 7432 7434 7441 7445 7446 7448 7453 7456 7522 7544 7545 7547 7548 7550 7551 7579 7626 7627 7681 7682 7683 7684 7685 7686 7687 7688 7689 7690 7691 7692 7693 7694 7695 7696 7697 7698 7699 7700 7701 7702 7703 7704 7705 7706 7707 7708 7709 7710 7711 7712 7713 7714 7715 7716 7717 7718 7719 7720 7721 7722 7723 7724 7725 7726 7727 7728 7729 7730 7731 7732 7733 7734 7735 7736 7737 7738 7739 7740 7741 7742 7743 7744 7745 7746 7747 7748 7749 7750 7751 7752 7753 7754 7755 7756 7757 7758 7759 7760 7761 7762 7763 7764 7765 7766 7767 7768 7769 7770 7771 7772 7773 7774 7775 7776 7777 7778 7779 7780 7781 7782 7783 7784 7785 7786 7787 7788 7789 7790 7791 7792 7793 7794 7795 7796 7797 7798 7799 7800 7801 7802 7803 7804 7805 7806 7807 7808 7809 7810 7811 7812 7813 7814 7815 7816 7817 7818 7819 7820 7821 7822 7823 7824 7825 7826 7827 7828 7829 7836 7841 7842 7843 7844 7845 7846 7847 7848 7849 7850 7851 7852 7853 7854 7855 7856 7857 7858 7859 7860 7861 7862 7863 7864 7865 7866 7867 7868 7869 7870 7871 7872 7873 7874 7875 7876 7877 7878 7879 7880 7881 7882 7883 7884 7885 7886 7887 7888 7889 7890 7891 7892 7893 7894 7895 7896 7897 7898 7899 7900 7901 7902 7903 7904 7905 7906 7907 7908 7909 7910 7911 7912 7913 7914 7915 7916 7917 7918 7919 7920 7921 7922 7923 7924 7925 7926 7927 7928 7929 7930 7936 7944 7952 7958 7968 7976 7984 7992 8000 8006 8016 8024 8032 8040 8048 8062 8064 8072 8080 8088 8096 8104 8112 8117 8118 8120 8126 8127 8130 8133 8134 8136 8144 8148 8150 8152 8160 8168 8178 8181 8182 8184 64256 64263 64275 64280 65345 65371 66600 66640 917601 917627]))
(def char-set:upper-case (range-vector->char-set [65 91 192 215 216 223 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288 289 290 291 292 293 294 295 296 297 298 299 300 301 302 303 304 305 306 307 308 309 310 311 313 314 315 316 317 318 319 320 321 322 323 324 325 326 327 328 330 331 332 333 334 335 336 337 338 339 340 341 342 343 344 345 346 347 348 349 350 351 352 353 354 355 356 357 358 359 360 361 362 363 364 365 366 367 368 369 370 371 372 373 374 375 376 378 379 380 381 382 385 387 388 389 390 392 393 396 398 402 403 405 406 409 412 414 415 417 418 419 420 421 422 424 425 426 428 429 430 432 433 436 437 438 439 441 444 445 452 453 455 456 458 459 461 462 463 464 465 466 467 468 469 470 471 472 473 474 475 476 478 479 480 481 482 483 484 485 486 487 488 489 490 491 492 493 494 495 497 498 500 501 502 505 506 507 508 509 510 511 512 513 514 515 516 517 518 519 520 521 522 523 524 525 526 527 528 529 530 531 532 533 534 535 536 537 538 539 540 541 542 543 544 545 546 547 548 549 550 551 552 553 554 555 556 557 558 559 560 561 562 563 570 572 573 575 577 578 579 583 584 585 586 587 588 589 590 591 902 903 904 907 908 909 910 912 913 930 931 940 984 985 986 987 988 989 990 991 992 993 994 995 996 997 998 999 1000 1001 1002 1003 1004 1005 1006 1007 1012 1013 1015 1016 1017 1019 1021 1072 1120 1121 1122 1123 1124 1125 1126 1127 1128 1129 1130 1131 1132 1133 1134 1135 1136 1137 1138 1139 1140 1141 1142 1143 1144 1145 1146 1147 1148 1149 1150 1151 1152 1153 1162 1163 1164 1165 1166 1167 1168 1169 1170 1171 1172 1173 1174 1175 1176 1177 1178 1179 1180 1181 1182 1183 1184 1185 1186 1187 1188 1189 1190 1191 1192 1193 1194 1195 1196 1197 1198 1199 1200 1201 1202 1203 1204 1205 1206 1207 1208 1209 1210 1211 1212 1213 1214 1215 1216 1218 1219 1220 1221 1222 1223 1224 1225 1226 1227 1228 1229 1230 1232 1233 1234 1235 1236 1237 1238 1239 1240 1241 1242 1243 1244 1245 1246 1247 1248 1249 1250 1251 1252 1253 1254 1255 1256 1257 1258 1259 1260 1261 1262 1263 1264 1265 1266 1267 1268 1269 1270 1271 1272 1273 1274 1275 1276 1277 1278 1279 1280 1281 1282 1283 1284 1285 1286 1287 1288 1289 1290 1291 1292 1293 1294 1295 1296 1297 1298 1299 1329 1367 4256 4294 7547 7548 7550 7551 7680 7681 7682 7683 7684 7685 7686 7687 7688 7689 7690 7691 7692 7693 7694 7695 7696 7697 7698 7699 7700 7701 7702 7703 7704 7705 7706 7707 7708 7709 7710 7711 7712 7713 7714 7715 7716 7717 7718 7719 7720 7721 7722 7723 7724 7725 7726 7727 7728 7729 7730 7731 7732 7733 7734 7735 7736 7737 7738 7739 7740 7741 7742 7743 7744 7745 7746 7747 7748 7749 7750 7751 7752 7753 7754 7755 7756 7757 7758 7759 7760 7761 7762 7763 7764 7765 7766 7767 7768 7769 7770 7771 7772 7773 7774 7775 7776 7777 7778 7779 7780 7781 7782 7783 7784 7785 7786 7787 7788 7789 7790 7791 7792 7793 7794 7795 7796 7797 7798 7799 7800 7801 7802 7803 7804 7805 7806 7807 7808 7809 7810 7811 7812 7813 7814 7815 7816 7817 7818 7819 7820 7821 7822 7823 7824 7825 7826 7827 7828 7829 7840 7841 7842 7843 7844 7845 7846 7847 7848 7849 7850 7851 7852 7853 7854 7855 7856 7857 7858 7859 7860 7861 7862 7863 7864 7865 7866 7867 7868 7869 7870 7871 7872 7873 7874 7875 7876 7877 7878 7879 7880 7881 7882 7883 7884 7885 7886 7887 7888 7889 7890 7891 7892 7893 7894 7895 7896 7897 7898 7899 7900 7901 7902 7903 7904 7905 7906 7907 7908 7909 7910 7911 7912 7913 7914 7915 7916 7917 7918 7919 7920 7921 7922 7923 7924 7925 7926 7927 7928 7929 7944 7952 7960 7966 7976 7984 7992 8000 8008 8014 8025 8026 8027 8028 8029 8030 8031 8032 8040 8048 8072 8080 8088 8096 8104 8112 8120 8125 8136 8141 8152 8156 8168 8173 8184 8189 65313 65339 66560 66600 917569 917595]))
(def char-set:title-case (range-vector->char-set [453 454 456 457 459 460 498 499 8072 8080 8088 8096 8104 8112 8124 8125 8140 8141 8188 8189]))
(def char-set:letter (range-vector->char-set [65 91 97 123 170 171 181 182 186 187 192 215 216 247 248 706 710 722 736 741 750 751 890 894 902 903 904 907 908 909 910 930 931 975 976 1014 1015 1154 1162 1300 1329 1367 1369 1370 1377 1416 1488 1515 1520 1523 1569 1595 1600 1611 1646 1648 1649 1748 1749 1750 1765 1767 1774 1776 1786 1789 1791 1792 1808 1809 1810 1840 1869 1902 1920 1958 1969 1970 1994 2027 2036 2038 2042 2043 2308 2362 2365 2366 2384 2385 2392 2402 2427 2432 2437 2445 2447 2449 2451 2473 2474 2481 2482 2483 2486 2490 2493 2494 2510 2511 2524 2526 2527 2530 2544 2546 2565 2571 2575 2577 2579 2601 2602 2609 2610 2612 2613 2615 2616 2618 2649 2653 2654 2655 2674 2677 2693 2702 2703 2706 2707 2729 2730 2737 2738 2740 2741 2746 2749 2750 2768 2769 2784 2786 2821 2829 2831 2833 2835 2857 2858 2865 2866 2868 2869 2874 2877 2878 2908 2910 2911 2914 2929 2930 2947 2948 2949 2955 2958 2961 2962 2966 2969 2971 2972 2973 2974 2976 2979 2981 2984 2987 2990 3002 3077 3085 3086 3089 3090 3113 3114 3124 3125 3130 3168 3170 3205 3213 3214 3217 3218 3241 3242 3252 3253 3258 3261 3262 3294 3295 3296 3298 3333 3341 3342 3345 3346 3369 3370 3386 3424 3426 3461 3479 3482 3506 3507 3516 3517 3518 3520 3527 3585 3633 3634 3636 3648 3655 3713 3715 3716 3717 3719 3721 3722 3723 3725 3726 3732 3736 3737 3744 3745 3748 3749 3750 3751 3752 3754 3756 3757 3761 3762 3764 3773 3774 3776 3781 3782 3783 3804 3806 3840 3841 3904 3912 3913 3947 3976 3980 4096 4130 4131 4136 4137 4139 4176 4182 4256 4294 4304 4347 4348 4349 4352 4442 4447 4515 4520 4602 4608 4681 4682 4686 4688 4695 4696 4697 4698 4702 4704 4745 4746 4750 4752 4785 4786 4790 4792 4799 4800 4801 4802 4806 4808 4823 4824 4881 4882 4886 4888 4955 4992 5008 5024 5109 5121 5741 5743 5751 5761 5787 5792 5867 5888 5901 5902 5906 5920 5938 5952 5970 5984 5997 5998 6001 6016 6068 6103 6104 6108 6109 6176 6264 6272 6313 6400 6429 6480 6510 6512 6517 6528 6570 6593 6600 6656 6679 6917 6964 6981 6988 7424 7616 7680 7836 7840 7930 7936 7958 7960 7966 7968 8006 8008 8014 8016 8024 8025 8026 8027 8028 8029 8030 8031 8062 8064 8117 8118 8125 8126 8127 8130 8133 8134 8141 8144 8148 8150 8156 8160 8173 8178 8181 8182 8189 8305 8306 8319 8320 8336 8341 8450 8451 8455 8456 8458 8468 8469 8470 8473 8478 8484 8485 8486 8487 8488 8489 8490 8494 8495 8506 8508 8512 8517 8522 8526 8527 8579 8581 11264 11311 11312 11359 11360 11373 11380 11384 11392 11493 11520 11558 11568 11622 11631 11632 11648 11671 11680 11687 11688 11695 11696 11703 11704 11711 11712 11719 11720 11727 11728 11735 11736 11743 12293 12295 12337 12342 12347 12349 12353 12439 12445 12448 12449 12539 12540 12544 12549 12589 12593 12687 12704 12728 12784 12800 13312 19894 19968 40892 40960 42125 42775 42779 43008 43010 43011 43014 43015 43019 43020 43043 43072 43124 44032 55204 63744 64046 64048 64107 64112 64218 64256 64263 64275 64280 64285 64286 64287 64297 64298 64311 64312 64317 64318 64319 64320 64322 64323 64325 64326 64434 64467 64830 64848 64912 64914 64968 65008 65020 65136 65141 65142 65277 65313 65339 65345 65371 65382 65471 65474 65480 65482 65488 65490 65496 65498 65501 65536 65548 65549 65575 65576 65595 65596 65598 65599 65614 65616 65630 65664 65787 66304 66335 66352 66369 66370 66378 66432 66462 66464 66500 66504 66512 66560 66718 67584 67590 67592 67593 67594 67638 67639 67641 67644 67645 67647 67648 67840 67862 68096 68097 68112 68116 68117 68120 68121 68148 73728 74607 119808 119893 119894 119965 119966 119968 119970 119971 119973 119975 119977 119981 119982 119994 119995 119996 119997 120004 120005 120070 120071 120075 120077 120085 120086 120093 120094 120122 120123 120127 120128 120133 120134 120135 120138 120145 120146 120486 120488 120513 120514 120539 120540 120571 120572 120597 120598 120629 120630 120655 120656 120687 120688 120713 120714 120745 120746 120771 120772 120780 131072 173783 194560 195102]))
(def char-set:digit (range-vector->char-set [48 58 1632 1642 1776 1786 1984 1994 2406 2416 2534 2544 2662 2672 2790 2800 2918 2928 3046 3056 3174 3184 3302 3312 3430 3440 3664 3674 3792 3802 3872 3882 4160 4170 6112 6122 6160 6170 6470 6480 6608 6618 6992 7002 65296 65306 66720 66730 120782 120832]))
(def char-set:mark (range-vector->char-set [768 880 1155 1159 1160 1162 1425 1470 1471 1472 1473 1475 1476 1478 1479 1480 1552 1558 1611 1631 1648 1649 1750 1757 1758 1765 1767 1769 1770 1774 1809 1810 1840 1867 1958 1969 2027 2036 2305 2308 2364 2365 2366 2382 2385 2389 2402 2404 2433 2436 2492 2493 2494 2501 2503 2505 2507 2510 2519 2520 2530 2532 2561 2564 2620 2621 2622 2627 2631 2633 2635 2638 2672 2674 2689 2692 2748 2749 2750 2758 2759 2762 2763 2766 2786 2788 2817 2820 2876 2877 2878 2884 2887 2889 2891 2894 2902 2904 2946 2947 3006 3011 3014 3017 3018 3022 3031 3032 3073 3076 3134 3141 3142 3145 3146 3150 3157 3159 3202 3204 3260 3261 3262 3269 3270 3273 3274 3278 3285 3287 3298 3300 3330 3332 3390 3396 3398 3401 3402 3406 3415 3416 3458 3460 3530 3531 3535 3541 3542 3543 3544 3552 3570 3572 3633 3634 3636 3643 3655 3663 3761 3762 3764 3770 3771 3773 3784 3790 3864 3866 3893 3894 3895 3896 3897 3898 3902 3904 3953 3973 3974 3976 3984 3992 3993 4029 4038 4039 4140 4147 4150 4154 4182 4186 4959 4960 5906 5909 5938 5941 5970 5972 6002 6004 6070 6100 6109 6110 6155 6158 6313 6314 6432 6444 6448 6460 6576 6593 6600 6602 6679 6684 6912 6917 6964 6981 7019 7028 7616 7627 7678 7680 8400 8432 12330 12336 12441 12443 43010 43011 43014 43015 43019 43020 43043 43048 64286 64287 65024 65040 65056 65060 68097 68100 68101 68103 68108 68112 68152 68155 68159 68160 119141 119146 119149 119155 119163 119171 119173 119180 119210 119214 119362 119365 917760 918000]))
(def char-set:separator (range-vector->char-set [32 33 160 161 5760 5761 6158 6159 8192 8203 8232 8234 8239 8240 8287 8288 12288 12289]))
(def char-set:punctuation (range-vector->char-set [33 36 37 43 44 48 58 60 63 65 91 94 95 96 123 124 125 126 161 162 171 172 183 184 187 188 191 192 894 895 903 904 1370 1376 1417 1419 1470 1471 1472 1473 1475 1476 1478 1479 1523 1525 1548 1550 1563 1564 1566 1568 1642 1646 1748 1749 1792 1806 2039 2042 2404 2406 2416 2417 3572 3573 3663 3664 3674 3676 3844 3859 3898 3902 3973 3974 4048 4050 4170 4176 4347 4348 4961 4969 5741 5743 5787 5789 5867 5870 5941 5943 6100 6103 6104 6107 6144 6155 6468 6470 6622 6624 6686 6688 7002 7009 8208 8232 8240 8260 8261 8274 8275 8287 8317 8319 8333 8335 9001 9003 10088 10102 10181 10183 10214 10220 10627 10649 10712 10716 10748 10750 11513 11517 11518 11520 11776 11800 11804 11806 12289 12292 12296 12306 12308 12320 12336 12337 12349 12350 12448 12449 12539 12540 43124 43128 64830 64832 65040 65050 65072 65107 65108 65122 65123 65124 65128 65129 65130 65132 65281 65284 65285 65291 65292 65296 65306 65308 65311 65313 65339 65342 65343 65344 65371 65372 65373 65374 65375 65382 65792 65794 66463 66464 66512 66513 67871 67872 68176 68185 74864 74868]))
(def char-set:symbol (range-vector->char-set [36 37 43 44 60 63 94 95 96 97 124 125 126 127 162 170 172 173 174 178 180 181 182 183 184 185 215 216 247 248 706 710 722 736 741 750 751 768 884 886 900 902 1014 1015 1154 1155 1547 1548 1550 1552 1769 1770 1789 1791 2038 2039 2546 2548 2554 2555 2801 2802 2928 2929 3059 3067 3313 3315 3647 3648 3841 3844 3859 3864 3866 3872 3892 3893 3894 3895 3896 3897 4030 4038 4039 4045 4047 4048 4960 4961 5008 5018 6107 6108 6464 6465 6624 6656 7009 7019 7028 7037 8125 8126 8127 8130 8141 8144 8157 8160 8173 8176 8189 8191 8260 8261 8274 8275 8314 8317 8330 8333 8352 8374 8448 8450 8451 8455 8456 8458 8468 8469 8470 8473 8478 8484 8485 8486 8487 8488 8489 8490 8494 8495 8506 8508 8512 8517 8522 8526 8592 9001 9003 9192 9216 9255 9280 9291 9372 9450 9472 9885 9888 9907 9985 9989 9990 9994 9996 10024 10025 10060 10061 10062 10063 10067 10070 10071 10072 10079 10081 10088 10132 10133 10136 10160 10161 10175 10176 10181 10183 10187 10192 10214 10224 10627 10649 10712 10716 10748 10750 11035 11040 11044 11493 11499 11904 11930 11931 12020 12032 12246 12272 12284 12292 12293 12306 12308 12320 12321 12342 12344 12350 12352 12443 12445 12688 12690 12694 12704 12736 12752 12800 12831 12842 12868 12880 12881 12896 12928 12938 12977 12992 13055 13056 13312 19904 19968 42128 42183 42752 42775 42784 42786 43048 43052 64297 64298 65020 65022 65122 65123 65124 65127 65129 65130 65284 65285 65291 65292 65308 65311 65342 65343 65344 65345 65372 65373 65374 65375 65504 65511 65512 65519 65532 65534 65794 65795 65847 65856 65913 65930 118784 119030 119040 119079 119082 119141 119146 119149 119171 119173 119180 119210 119214 119262 119296 119362 119365 119366 119552 119639 120513 120514 120539 120540 120571 120572 120597 120598 120629 120630 120655 120656 120687 120688 120713 120714 120745 120746 120771 120772]))
(def char-set:space-separator (range-vector->char-set [32 33 160 161 5760 5761 6158 6159 8192 8203 8239 8240 8287 8288 12288 12289]))

(def char-set:empty (char-set))
(def char-set:full (char-set-complement char-set:empty))

(def char-set:letter+digit
  (char-set-union char-set:letter char-set:digit))

(def char-set:graphic
  (char-set-union char-set:mark
		  char-set:letter
		  char-set:digit
		  char-set:symbol
		  char-set:punctuation))

(def char-set:whitespace
  (char-set-union char-set:separator
		  (coll->char-set (map char
				       [9 ; tab
                                        10 ; newline
                                        11 ; vtab
                                        12 ; page
                                        13 ; return
                                        ]))))


(def char-set:printing
  (char-set-union char-set:whitespace char-set:graphic))

(def char-set:iso-control
  (char-set-union (range->char-set 0 0x20)
		  (range->char-set 0x7f 0xa0)))

(def char-set:blank
  (char-set-union char-set:space-separator
		  (char-set (char 9)))) ; tab

(def char-set:ascii (range->char-set 0 128))
(def char-set:hex-digit (string->char-set "0123456789abcdefABCDEF"))
