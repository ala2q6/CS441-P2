(ns tutorial.core)
(import '(java.util.concurrent Executors))


(defn open [numberCount threadCount]
  " open and divide work "

  ;; read file
  (def iter (atom 1))
  (def seqVariableA (seq []))
  (def seqVariableB (seq []))
  (def size (int (/ numberCount (* threadCount 2))))
  (with-open [file (clojure.java.io/reader "src/tutorial/numbers.txt")]

    ;; iterate
    (doseq [line (line-seq file)]

      ;; while under limit
      (if (> numberCount @iter) (do

                                  (cond

                                    ;; start new list
                                    (= (mod @iter size) 0) (do

                                                             (def seqVariableB (concat seqVariableB [(bigdec line)]))
                                                             (def seqVariableA (concat seqVariableA [seqVariableB]))

                                                             (def seqVariableB (seq []))

                                                             )

                                    )

                                  ;; add work to list
                                  (def seqVariableB (concat seqVariableB [(bigdec line)]))
                                  (swap! iter inc)

                                  )

                                )

      )

    )

  ;; return divided
  (seq [seqVariableA])

  )


(defn group [seqParameter]
  " pair sequences together "

  (println "before " (count (nth seqParameter 0)))

  ;; iterate
  (def iter (atom 1))
  (def seqVariableA (seq []))
  (doseq [ele (nth seqParameter 0)]

    (if (= (mod @iter 2) 0) (do

                              (println (count (nth (nth (nth seqParameter 0) 0) 0)))

                              )

                            )

    (swap! iter inc)

    )


  (doseq [ele (nth (nth seqParameter 0) 0)]


    ;; pair
    (if (= (mod @iter 2) 0) (do


                              (def seqVariableB (nth seqParameter (- @iter 1)))
                              (def seqVariableC (nth seqParameter (- @iter 2)))

                              (def seqVariableB (concat seqVariableB seqVariableC))
                              (def seqVariableA (conj seqVariableA seqVariableB))

                              )

                            )

    ;; increment
    (swap! iter inc)

    )

  ;; return
  (seq [seqVariableA])

  )


(defn quickSort [seqParameter]
  " recursive, divide-and-conquer sort "

  (cond

    (<= (count seqParameter) 1) seqParameter
    :else (do

            (let [pivot (first seqParameter) other (rest seqParameter)]

              (concat

                (quickSort (filter #(>= pivot %) other))
                [pivot]
                (quickSort (filter #(< pivot %) other))

                )

              )

            )

    )

  )


(defn run [numberCount threadCount]
  " function to determine how long it takes numberCount
    to sort, dependent upon threadCount "

  ;; ;; ;; <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <-

  ;; good
  (def s (open numberCount threadCount))
  ;;(println (count (nth s 0)))

  ;; on
  (def a (group s))
  ;(println (count (nth (nth a 0) 0)))

  ;;(def b (group a))
  ;(println (count (nth (nth b 0) 0)))

  (println "\n")


  ;; ;; ;;

  (def iter (atom threadCount))
  (while (not= @iter 0)

    (println @iter)
    (reset! iter (int (/ @iter 2)))

    )

  ;; ;; ;;

  )


(defn -main []

  (run 50000 32)

  )


;; cd /Users/adrienhighlander/Desktop/tutorial/src/tutorial
;; lein run

;;(time (open 1000000 64))


(defn -main []

  ;; 64 thread instance
  (def openVariable (open 50000 64))
  (def groupVariable (group openVariable))

  (def seqVariable (nth groupVariable 0))
  (let [pool (Executors/newFixedThreadPool 64) work (map (fn [p] #(quickSort p)) seqVariable)]

    (doseq [future (.invokeAll pool work)]

      (println (deref future))

      )

    (.shutdown pool)

    )

  )