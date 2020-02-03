;;fastrecord comparison with record...
spork.util.record> (let [r (assoc (fr. 10 10) :a 2 :b :3 :c 4)]  (c/quick-bench (get r :x)))
Evaluation count : 53604480 in 6 samples of 8934080 calls.
             Execution time mean : 9.381978 ns
    Execution time std-deviation : 0.057688 ns
   Execution time lower quantile : 9.321729 ns ( 2.5%)
   Execution time upper quantile : 9.436063 ns (97.5%)
                   Overhead used : 1.860554 ns
nil
spork.util.record> (let [r (assoc (fr. 10 10) :a 2 :b :3 :c 4)]  (c/quick-bench (get r :c)))
Evaluation count : 34729932 in 6 samples of 5788322 calls.
             Execution time mean : 15.436899 ns
    Execution time std-deviation : 0.237012 ns
   Execution time lower quantile : 15.263644 ns ( 2.5%)
   Execution time upper quantile : 15.810472 ns (97.5%)
                   Overhead used : 1.860554 ns

Found 1 outliers in 6 samples (16.6667 %)
	low-severe	 1 (16.6667 %)
 Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
nil
spork.util.record> (let [r (assoc (r. 10 10) :a 2 :b :3 :c 4)]  (c/quick-bench (get r :x)))
Evaluation count : 31582332 in 6 samples of 5263722 calls.
             Execution time mean : 17.620178 ns
    Execution time std-deviation : 0.734314 ns
   Execution time lower quantile : 17.137678 ns ( 2.5%)
   Execution time upper quantile : 18.777557 ns (97.5%)
                   Overhead used : 1.860554 ns

Found 1 outliers in 6 samples (16.6667 %)
	low-severe	 1 (16.6667 %)
 Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
nil
spork.util.record> (let [r (assoc (r. 10 10) :a 2 :b :3 :c 4)]  (c/quick-bench (get r :c)))
Evaluation count : 22709778 in 6 samples of 3784963 calls.
             Execution time mean : 24.598666 ns
    Execution time std-deviation : 0.030144 ns
   Execution time lower quantile : 24.567735 ns ( 2.5%)
   Execution time upper quantile : 24.637346 ns (97.5%)
                   Overhead used : 1.860554 ns
nil
;;fn invoke lookup (fast).

spork.util.record> (let [r (assoc (fr. 10 10) :a 2 :b :3 :c 4)]  (c/quick-bench (r :x)))
Evaluation count : 73261614 in 6 samples of 12210269 calls.
Execution time mean : 6.439670 ns
Execution time std-deviation : 0.116191 ns
Execution time lower quantile : 6.333676 ns ( 2.5%)
Execution time upper quantile : 6.581136 ns (97.5%)
Overhead used : 1.860554 ns
nil
spork.util.record> (let [r (assoc (fr. 10 10) :a 2 :b :3 :c 4)]  (c/quick-bench (r :c)))
Execution time mean : 10.482522 ns


spork.util.record> (let [r {:x  10 :y 10 :a 2 :b :3 :c 4}]  (c/quick-bench (get r :x)))
Evaluation count : 42221196 in 6 samples of 7036866 calls.
Execution time mean : 12.339204 ns
Execution time std-deviation : 0.067425 ns
Execution time lower quantile : 12.281303 ns ( 2.5%)
Execution time upper quantile : 12.416245 ns (97.5%)
Overhead used : 1.860554 ns

spork.util.record> (let [r {:x  10 :y 10 :a 2 :b :3 :c 4}]  (c/quick-bench (get r :c)))
Evaluation count : 38292000 in 6 samples of 6382000 calls.
Execution time mean : 14.084213 ns
Execution time std-deviation : 0.402300 ns
Execution time lower quantile : 13.815076 ns ( 2.5%)
Execution time upper quantile : 14.770936 ns (97.5%)
Overhead used : 1.860554 ns

Found 1 outliers in 6 samples (16.6667 %)
low-severe	 1 (16.6667 %)
Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
nil

spork.util.record> (let [r {:x  10 :y 10 :a 2 :b :3 :c 4}]  (c/quick-bench (r :x)))
Evaluation count : 57558102 in 6 samples of 9593017 calls.
Execution time mean : 8.650793 ns
Execution time std-deviation : 0.067726 ns
Execution time lower quantile : 8.563923 ns ( 2.5%)
Execution time upper quantile : 8.709615 ns (97.5%)
Overhead used : 1.860554 ns
nil

spork.util.record> (let [r {:x  10 :y 10 :a 2 :b :3 :c 4}]  (c/quick-bench (r :c)))
Evaluation count : 48846870 in 6 samples of 8141145 calls.
Execution time mean : 10.598616 ns
Execution time std-deviation : 0.186617 ns
Execution time lower quantile : 10.432756 ns ( 2.5%)
Execution time upper quantile : 10.879601 ns (97.5%)
Overhead used : 1.860554 ns
nil
