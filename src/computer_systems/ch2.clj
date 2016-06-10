(ns computer-systems.ch2
  (:use [clojure.java.shell :only [sh]])
  (:require [clojure.string :as clj-str]))


(def ex2-2 [[1 0 1 1] [1 1 0 1 1] [1 1 1 0 1 1]])

(def bits-to-unsigned twos-complement-pos)
(defn twos-complement-pos [bit-vector]
;Will convert a bit vector of anysize
  (let [rev-vec (reverse bit-vector)]
  (loop [power 0
         rv    0]
    (if (= power (count bit-vector))
     (int rv) 
     (recur
      ;power
      (+ power 1)

      ;rv
      (+ rv (*
             (nth rev-vec power) ;is the bit positive
             (Math/pow 2  power))))))))

(defn twos-complement-neg [bit-vector]
  (let [neg-val (Math/pow 2 (- (count bit-vector) 1))]
    (- 
      (bits-to-unsigned
        ;invert the vector
        ;(map #(if (= 1 %1) 0 1) (rest bit-vector)))
        (rest bit-vector))
      neg-val)))

(defn twos-complement [bit-vector]
  (if (= (first bit-vector) 1)
    (int (twos-complement-neg bit-vector))
    (int (twos-complement-pos bit-vector))))


;practice problem 2.24
;Suppose we truncate a 4-bit value (represented by hex digits 0 through F) to a 3- bit value (represented as hex digits 0 through 7). Fill in the table below showing the effect of this truncation for some cases, in terms of the unsigned and two’s- complement interpretations of those bit patterns.

(def unsigned-nums
  [[0 0 0 0] ;0
  [0 0 1 0] ;2
  [1 0 0 1] ;9
  [1 0 1 1] ;11
  [1 1 1 1]]) ;15

(def twos-complement-nums
  [[0 0 0 0] ;0
   [0 0 1 0] ;2
   [1 0 0 1] ;-7
   [1 0 1 1] ;-5
   [1 1 1 1]];-1
  )
  

(defn truncate-unsigned [bit-vec]
  (twos-complement-pos (rest bit-vec)))

(defn truncate-twos-complement [bit-vec]
  (twos-complement (rest bit-vec)))

(defn run-2-24 []
  (println (str "Unsigned ints: "
    (pr-str (map
      #(truncate-unsigned %1)
      unsigned-nums))))
  (println (str "Twos complement: "
    (pr-str (map
      #(truncate-twos-complement %1)
      twos-complement-nums)))))


;=> Unsigned ints: (0.0 2.0 1.0 3.0 7.0) ;satisfies the condition mod 2^k
;=> Twos complement:  (0 2 1 3 -1)
; Note, the twos complement is not what the formula asked for. It asked for the transition from twos complement to unsigned to truncated. This would result in the same values as the Unsigned ints: My way is certianly more interesting!




;Practice problem 2.25.
;The reason this results in an error is that length is unsigned. Casting a -1 to unsigned yields unsigned max, therefore it will iterate unsigned max times. The following code should iterate many many many times

(def buggy-c-code
  "#include <stdio.h>

   float sum_elements(float a[], unsigned length){
      int i;
      int sum;
      for(i=0; i<=length-1; i++){
        printf(\"%d\", i);
      }
      return 0.0;
    }
   int main(){
     float elements[0];
     sum_elements(elements ,0);
   }
  ")

;to fix use i<length

(defmacro with-timeout [millis & body]
  `(let [future# (future ~@body)]
    (try
      (.get future# ~millis java.util.concurrent.TimeUnit/MILLISECONDS)
      (catch java.util.concurrent.TimeoutException x# 
        (do
          (future-cancel future#)
          "function timed out")))))

(defn compile-and-run-buggy-c-code []
  (spit "buggy_c.cpp" buggy-c-code)
  (sh "gcc" "-o" "buggy_c" "buggy_c.cpp")
  (with-timeout 1000 (sh "./buggy_c")))

;And I was right!

;2.28 Hex to decimal to negative unsigned complement (way to do subtraction under unsigned integers)

(def hex-to-bin-encoding
                 {"0" [0 0 0 0]
                  "1" [0 0 0 1]
                  "2" [0 0 1 0]
                  "3" [0 0 1 1]
                  "4" [0 1 0 0]
                  "5" [0 1 0 1]
                  "6" [0 1 1 0]
                  "7" [0 1 1 1]
                  "8" [1 0 0 0]
                  "9" [1 0 0 1]
                  "A" [1 0 1 0]
                  "B" [1 0 1 1]
                  "C" [1 1 0 0]
                  "D" [1 1 0 1]
                  "E" [1 1 1 0]
                  "F" [1 1 1 1]})

(defn hex-to-binary [hex-str]
  (let [encoding {"0" [0 0 0 0]
                  "1" [0 0 0 1]
                  "2" [0 0 1 0]
                  "3" [0 0 1 1]
                  "4" [0 1 0 0]
                  "5" [0 1 0 1]
                  "6" [0 1 1 0]
                  "7" [0 1 1 1]
                  "8" [1 0 0 0]
                  "9" [1 0 0 1]
                  "A" [1 0 1 0]
                  "B" [1 0 1 1]
                  "C" [1 1 0 0]
                  "D" [1 1 0 1]
                  "E" [1 1 1 0]
                  "F" [1 1 1 1]}]
     (flatten (map #(get encoding (str %1)) hex-str))))

(defn decimal-to-binary [dec word-size]
  (loop [decimal-part dec
         binary-part  []]
      (if (= word-size (count binary-part))
        (reverse binary-part)
        (recur (Math/floor (/ decimal-part 2)) (conj binary-part (int (mod decimal-part 2)))))))

(defn binary-to-decimal [bin]
  (let [col (reverse bin)]
    (loop [power 0
         sum   0
         ]
      (if (= power (count col))
        (int sum)
        (recur (+ 1 power) (+ sum (* (nth col power) (Math/pow 2 power))))))))

(defn binary-to-hex [bin]
  (if (= 0 (mod (count bin) 4))
    (let [bin-to-hex (clojure.set/map-invert hex-to-bin-encoding)
          bin (reverse bin)]
      (loop [remaining-bin bin
             result ""]
        (if (not (= 0 (count remaining-bin)))
          (recur (drop 4 remaining-bin) (str result (get  bin-to-hex (reverse (take 4 remaining-bin)))))
          (apply str (reverse result)))))
   (throw (Exception. "Bin must be a multiple of 4"))))

(def input-for-2-28 ["0" "5" "8" "D" "F"])

(defn unsigned-sub [dec word-size]
  (if (= 0 dec)
    0
    (int (- (Math/pow 2 word-size) dec))))

(defn solve-2-28 []
  (map
   #(-> %1
        hex-to-binary
        binary-to-decimal
        ((fn [item] do (println (str %1 " as dec: " item)) item))
        (unsigned-sub 4)
        ((fn [item] do (println (str "unsigned-complement of " %1 " as dec: " item)) item))
        (decimal-to-binary 4)
        binary-to-hex
        ((fn [item] do (println (str "unsigned-complement of " %1 " as hex: " item)) item)))
   input-for-2-28))



;Practice 2.29 Fill in the following table in the style of Figure 2.24. Give the integer values of the 5-bit arguments, the values of both their integer and two’s-complement sums, the bit-level representation of the two’s-complement sum, and the case from the derivation of Equation 2.14.

(defn pad-twos-complement [x word-size]
  (if (< (count x) word-size)
    (let [padding-type (first x)
          padding (repeat (- word-size (count x)) padding-type)]
      (concat padding x))
    x))

(defn non-overflow-sum [x y]
  (let [x (reverse (pad-twos-complement x (count y)))
        y (reverse (pad-twos-complement y (count x)))]
    (loop [index 0
           carry  0
           result []]
      (if (and (>= index (count x)) (= carry 0))
        (reverse result)
        (recur
          (+ index 1)
          (if (> (+ carry (nth x index 0) (nth y index 0)) 1) 1 0) ;These could be done bitwise with &
          (conj result (mod (+ carry (nth x index 0) (nth y index 0)) 2)))))))

;This is broken.

(defn overflow-sum [x y]
  (let [x (reverse (pad-twos-complement x (count y)))
        y (reverse (pad-twos-complement y (count x)))]
    (loop [index 0
           carry 0
           result []]
      (if (>= index (count x))
        (reverse result)
        (recur
          (+ index 1)
          (if (> (+ carry (nth x index 0) (nth y index 0)) 1) 1 0) ;These could be done bitwise with &
          (conj result (mod (+ carry (nth x index 0) (nth y index 0)) 2)))))))


(def data-2-29 [
  [[1 0 1 0 0][1 0 0 0 1]]
  [[1 1 0 0 0][1 1 0 0 0]]
  [[1 0 1 1 1][0 1 0 0 0]]
  [[0 0 0 1 0][0 0 1 0 1]]
  [[0 1 1 0 0][0 0 1 0 0]]])

(defn solve-2-29 []
  (map
    #(do
      ((fn [input] (println (str (first input) "+" (second input) " non-overflow =: "
          (non-overflow-sum (first input) (second input))))) %1)
       ((fn [input] (println (str (first input) "+" (second input) " overflow =: "
          (overflow-sum (first input) (second input))))) %1 )
      (println ""))
    data-2-29))

 
;Practice 2.30 Write a function with the following prototype:
;/* Determine whether arguments can be added without overflow */
;int tadd_ok(int x, int y);

(defn tadd_ok [x y]
  (let [sum (overflow-sum x y)]
    (if (and (= (first x) (first y)) (not (= (first sum) (first x))))
      0
      1)))

;(tadd_ok [0 1 1 0] [0 0 0 1]) => 1
;(tadd_ok [0 1 1 1] [0 0 0 1]) => 0
;(tadd_ok [1 0 0 1] [1 0 1 0]) => 0
;(tadd_ok [1 1 1 1] [1 1 1 0]) => 1
;(tadd_ok [1 1 1 1] [0 1 1 0]) => 1

;2.31
;Good, I am on the right track. Addition is reversable. Even if x + y overflows, sum - x will just reverse the overflow. You always end up with x

;2.32
(defn tsub-ok [x y]
  (tadd_ok x y))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn get-min-word-size [num]
  (+ 2 (Math/floor (log2 num))))

(defn test-tsub-ok []
  (doseq [x (map (fn [item] [(decimal-to-binary (* -1 item) 4) item]) (range -8 8))]
    (doseq [y (map (fn [item] [(decimal-to-binary (* -1 item) 4) item]) (range -8 8))]
        (println "(" x "," y "): " (tsub-ok (first x) (first y))
                 (<= (Math/abs (- (second x) (second y))) 8)))))


;Practice Problem 2.33
;We can represent a bit pattern of length w = 4 with a single hex digit. For a two’s- complement interpretation of these digits, fill in the following table to determine the additive inverses of the digits shown:

(def data-2-33 
  [{:hex "0"}
   {:hex "5"}
   {:hex "8"}
   {:hex "D"}
   {:hex "F"}])

(defn twos-complement-additive-inverse [item size]
  (cond   
    (or (> item (Math/pow 2 (- size 1))) (< item (* -1 (Math/pow 2 (- size 1)))))
    (throw (Exception. (str
                        item
                        " exceeds size representable by 2s complement with word size " 
                        size)))

    (== item (* -1 (Math/pow 2 (- size 1))))
    (* -1 (Math/pow 2 (- size 1)))


    :else
    (* -1 item)))



(defn solve-2-33 []
  (map
   #(let
        [dec        (twos-complement (hex-to-binary (:hex %1)))
         neg-x-dec  (twos-complement-additive-inverse dec 4)]
       (assoc
       %1
       :dec    dec 
       :-x-dec neg-x-dec
       :-x-hex (binary-to-hex (decimal-to-binary neg-x-dec 4))))
       data-2-33))

   
;unsigned and signed additive inverses have the same bit representation. This now seems obvious. The same bit addition should always produce [0 0 0 0] reglardless of how we interpret those bit strings
              


;Practice 2.34
;Fill in the following table showing the results of multiplying different 3-bit num- bers, in the style of Figure 2.26:

(def data-2-34
  [[[1 0 0] [1 0 1]]
   [[0 1 0] [1 1 1]]
   [[1 1 0] [1 1 0]]])


(defn solve-2-34 []
  (map
   #(let [s-x (twos-complement (first %1))
          s-y (twos-complement (second %1))
          u-x (binary-to-decimal (first %1))
          u-y (binary-to-decimal (second %1))]
      
       {:s-x              s-x
       :s-y               s-y 
       :s-x*y             (* s-x s-y)
       :s-trunc-x*y-bin   (decimal-to-binary (* s-x s-y) 3)
       :s-trunc-x*y-dec   (twos-complement (decimal-to-binary (* s-x s-y) 3))
       :u-x               u-x
       :u-y               u-y
       :u-x*y             (* u-x u-y)
       :u-trunc-x*y-bin   (decimal-to-binary (* s-x s-y) 3)
       :u-trunc-x*y-dec   (binary-to-decimal (decimal-to-binary (* s-x s-y) 3))})
   data-2-34))

;opperations on unsigned are isomorphic with operations on signed



;Practice Problem 2.45
;Fill in the missing information in the following table
;Fraction, binary, decimal
(def data-2-45
  [[1/8 "0.001" 0.125]
   [3/4 nil nil]
   [25/16 nil nil]
   [nil "10.1011" nil]
   [nil "1.001" nil]
   [nil nil 5.875]
   [nil nil 3.1875]])

(defn fraction-to-decimal [fraction]
 (float fraction))

(defn decimal-to-fraction [decimal]
  (rationalize decimal))

(defn binary-to-decimal [binary]
  (let [split-binary (clj-str/split binary #"\.")]
       (+ (loop [power 0 ;the positive exponent loop
                 sum 0]
            (let [digit  (nth (reverse (first split-binary)) power nil)]
              (if (nil? digit)
                sum
                (recur (+ power 1) (+ sum (* (read-string (str digit)) (Math/pow 2 power)))))))
          (loop [power -1 ;the negative exponent loop
                 sum 0]
            (let [digit (nth (second split-binary) (- (Math/abs power) 1) nil)]
              (if (nil? digit)
                sum
                (recur (- power 1) (+ sum (* (read-string (str digit)) (Math/pow 2 power))))))))))

(defn str-insert
  "Insert c in string s at index i."
  [s c i]
  (str (subs s 0 i) c (subs s i)))

(defn fraction-to-binary [fraction]
  (let [min-pow (int (log2 (denominator fraction)))]
    (reverse (str-insert
              (let [r-str
                    (loop [pow 0 
                           num (int (* (float fraction) (Math/pow 2 min-pow)))
                           rv ""]
                      (if (= num 0)
                        rv
                        (recur (+ pow 1) (int (/ num 2)) (str rv (mod num 2)))))

                    padding
                    (- min-pow (count r-str))]
                (if (> padding 0)
                  (str r-str (apply str (take (+ 1 padding) (repeat "0"))))
                  r-str))
              "."
              min-pow))))

(defn get-number-type [number-array]
  (let [types [:fraction :binary :decimal]]
    (loop [index 0]
      (if (or (not (nil? (get number-array index))) (> (+ index 1) (count number-array)))
        {:type (get types index nil) :val (get number-array index nil)}
        (recur (+ index 1))))))

(defmulti to-fraction (fn [input] (:type input)))

(defmethod to-fraction :fraction [input] (input :val))
(defmethod to-fraction :decimal  [input] (rationalize (input :val)))
(defmethod to-fraction :binary  [input] (rationalize (binary-to-decimal (input :val))))


(defmulti to-binary (fn [input] (:type input)))

(defmethod to-binary :fraction [input] (fraction-to-binary (input :val)))
(defmethod to-binary :decimal  [input] (fraction-to-binary (to-fraction input)))
(defmethod to-binary :binary   [input] (input :val))


(defmulti to-decimal (fn [input] (:type input)))

(defmethod to-decimal :fraction [input] (float (input :val)))
(defmethod to-decimal :decimal  [input] (input :val))
(defmethod to-decimal :binary   [input] (binary-to-decimal (input :val)))

(defn solve-2-45 []
  (map
   #(let [input (get-number-type %1)]
      [(to-fraction input)
       (to-binary   input)
       (to-decimal  input)])
   data-2-45))

;Practice Problem 2-46
;The imprecision of floating-point arithmetic can have disastrous effects. On Febru- ary 25, 1991, during the first Gulf War, an American Patriot Missile battery in Dharan, Saudi Arabia, failed to intercept an incoming Iraqi Scud missile. The Scud struck an American Army barracks and killed 28 soldiers. The U.S. General Accounting Office (GAO) conducted a detailed analysis of the failure [72] and de- termined that the underlying cause was an imprecision in a numeric calculation. In this exercise, you will reproduce part of the GAO’s analysis.
;The Patriot system contains an internal clock, implemented as a counter
;that is incremented every 0.1 seconds. To determine the time in seconds, the
;program would multiply the value of this counter by a 24-bit quantity that was
;a fractional binary approximation to 1 . In particular, the binary representation 1 10
;of 10 is the nonterminating sequence 0.000110011[0011] . . .2, where the portion in brackets is repeated indefinitely. The program approximated 0.1, as a value x, by considering just the first 23 bits of the sequence to the right of the binary point: x = 0.00011001100110011001100. (See Problem 2.51 for a discussion of how they could have approximated 0.1 more precisely.)

;A?) What is the binary representation of 0.1 − x?
;A!) .1 - x = .0000000000000000000000011001100110011001100110011001100110011001100

;B?) What is the approximate decimal value of 0.1 − x?
(defn solve-2-46-b []
  (to-decimal {:type :binary :val "0.0000000000000000000000011001100110011001100110011001100110011001100"}))
;B!) 9.536743164061958E-8

;C?) The clock starts at 0 when the system is first powered up and keep scounting up from there. In this case, the system had been running for around 100 hours. What was the difference between the actual time and the time computed by the software?

(defn solve-2-46-c []
  (* 60 60 100 9.536743164061958E-8))
;C!) .034 seconds


;D?)The system predicts where an incoming missile will appear based on its velocity and the time of the last radar detection. Given that a Scud travels at around 2000 meters per second, how far off was its prediction?
(defn solve-2-46-d []
  ;page 137
  ;
  (* 2000 (solve-2-46-c)))

;D!) 68.66455 meters!



;Problem 2.47


(def vars-2-47
  [:e     "The value represented by considering the exponent field to be an unsigned integer"
   :E     "The value of the exponent after biasing"
   :2E    "The numeric weight of the exponent"
   :f     "The value of the fraction"
   :M     "The value of the significand"
   :2ExM  "The (unreduced) fractional value of the number"])


(defn enumerate-bits [n]
  (if  (= n 1)
    ;base case, n = 1
    [[0] [1]]

    ;normal-case, n > 1
    ;foreach bit string in enumerate-bits (- n 1) create 2 new bitstrings conjing - and 1
    (reduce
     (fn [strings assembled-string]
       (conj strings
             (conj assembled-string 0)
             (conj assembled-string 1)))
     [] 
     (enumerate-bits (- n 1)))))

;Defaults bit counts for floats in this problem
(def float-configuration {:s 1 :e 4 :f 3})

(defn grab-sign
  "Will grab the sign portion of a float bitstring"
  [bit-string]
  (if (= (first bit-string) 0)
    1
    -1))

(defn grab-exponent
  "Will grab the exponent portion of a float bitstring"
  [bit-string]
  (subvec
   bit-string
   (float-configuration :s)
   (+ (float-configuration :s) (float-configuration :e))))

(defn grab-fraction
  "Will grab the exponent portion of a float bitstring"
  [bit-string]
  (subvec
   bit-string
   (+ (float-configuration :s) (float-configuration :e))))

(defn get-float-category [bit-string]
  (let
      
      [all-1s-exponent
       (= 1 (reduce bit-and (grab-exponent bit-string)))

       all-0s-exponent
       (= 0 (reduce bit-or (grab-exponent bit-string)))

       all-0s-fraction
       (= 0 (reduce bit-or (grab-fraction bit-string)))
       ]

    (if (not (or all-1s-exponent all-0s-exponent))
      :normalized-values
      (if all-0s-exponent
        :denormalized-values
        (if all-0s-fraction
          :infinity
          :nan)))))


(defn bias-exponent [exp cat]
  (let [bias (- (Math/pow 2 (-  (float-configuration :e) 1)) 1)]
    (if (= cat :denormalized-values)
      (- 1 bias)
      (- exp bias))))

(defn compute-significand [frac cat]
  (if (= cat :denormalized-values)
    frac
    (+ 1 frac)))

(defn compute-v [twoExM cat s]
  (if (contains? #{:normalized-values :denormalized-values} cat)
    (* s twoExM)
    cat))
  


(defn compute-float-table-row [bit-string]
  (let
      [cat    (get-float-category bit-string)
       s      (grab-sign bit-string)
       e      (bits-to-unsigned (grab-exponent bit-string))
       E      (bias-exponent e cat)
       twoE   (Math/pow 2 E)
       f      (to-decimal {:type :binary :val  (str "0." (apply str (grab-fraction bit-string)))})
       M      (compute-significand f cat)
       twoExM (* twoE M)
       V      (compute-v twoExM cat s)]
   (assoc
    {}
    :vec   bit-string
    :cat   cat
    :s     s
    :e     e
    :E     E
    :2E    twoE
    :f     f
    :M     M
    :2ExM  twoExM
    :V     V)))


(defn complete-whole-float-table
  [config]
  (def float-configuration config)
  (map
   #(compute-float-table-row %1)
   (enumerate-bits (+ (:s config) (:e config) (:f config)))))

;(complete-whole-float-table {:s 1 :e 2 :f 2}) ;solves the problem!


;Practice Problem 2.48
;As mentioned in Problem 2.6, the integer 3,510,593 has hexadecimal representa- tion 0x00359141, while the single-precision, floating-point number 3510593.0 has hexadecimal representation 0x4A564504. Derive this floating-point representa- tion and explain the correlation between the bits of the integer and floating-point representations.

(defn get-significant-bits [bit-vec]
  (subvec (vec bit-vec) (.indexOf (apply str bit-vec) "1")))

(defn right-pad [bit-vec size]
  (concat bit-vec (take (- size (count bit-vec)) (repeat 0))))



(defn get-float-bits-from-number [n]
  (let
      [non-padded-f
       (rest ;drop the first bit (it is a 1), this is because it is always a 1 and can be implied
        (get-significant-bits 
         (decimal-to-binary n 32)))
       
       pow (count non-padded-f)]

    (concat
     (if (< n 0) [1] [0])
     (decimal-to-binary (+ 127 pow) 8)
     (right-pad non-padded-f 23))))


;(get-float-bits-from-number 3510593)
;=>(0 1 0 0 1 0 1 0 0 1 0 1 0 1 1 0 0 1 0 0 0 1 0 1 0 0 0 0 0 1 0 0)
;(hex-to-binary "00359141")
;=>(0 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 1 0 0 1 0 0 0 1 0 1 0 0 0 0 0 1)

; 101011001000101000001 is the float portion and it is the same the first 1 on the hex representation is dropped due to its implicitness
