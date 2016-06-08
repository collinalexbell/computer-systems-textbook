(ns computer-systems.core)

(def ex2-2 [[1 0 1 1] [1 1 0 1 1] [1 1 1 0 1 1]])

(defn twos-complement [bit-vector]
  (if (= (first bit-vector) 1)
    (twos-complement-neg bit-vector)
    (twos-complement-pos bit-vector)))

(defn twos-complement-pos [bit-vector]
  (let [rev-vec (reverse bit-vector)]
  (loop [power 0
         rv    0]
    (if (= power (count bit-vector))
     rv 
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
      (twos-complement-pos
        ;invert the vector
        ;(map #(if (= 1 %1) 0 1) (rest bit-vector)))
        (rest bit-vector))
      neg-val)))
