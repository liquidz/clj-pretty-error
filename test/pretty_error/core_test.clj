(ns pretty-error.core-test
  (:use clojure.test
        pretty-error.core)
  (:import [com.uo.liquidz ClassUtil])
  )

;(deftest sample-output
;  (try
;    (.foo nil)
;    (catch Exception ex
;      (print-pretty-stack-trace ex))))

;(defn get-null-pointer-exception []
;  (let [ex (atom nil)]
;    (try (.foo nil)
;      (catch NullPointerException e
;        (reset! ex e)))
;    @ex))

(defmacro assertion-error? [body]
  `(is (~'thrown? AssertionError ~body)))


(deftest throwable->map-test
  (testing "Simple exception"
    (let [m (throwable->map (Exception. "hello"))]
      (are [x y] (= x y)
        false   (nil? m)
        "hello" (:message m)
        false   (empty? (:stack-trace m))
        () (:causes m))))
  (testing "invalid param"
    (assertion-error? (throwable->map "hello"))))


(deftest set-stack-trace-element-test
  (let [base (Exception. "hello")
        ex   (set-stack-trace-element
               base {:class "foo" :method "bar" :filename "baz" :line 123})
        st   (:stack-trace (throwable->map ex))]
    (testing "no side-effect"
      (is (not= base ex))
      (is (not= (throwable->map base) (throwable->map ex))))

    (testing "overwrited stack trace"
      (are [x y] (= x y)
        1     (count st)
        "foo" (-> st first :class)
        "bar" (-> st first :method)
        "baz" (-> st first :filename)
        123   (-> st first :line)))
    )

  (testing "set multiple stack traces"
    (let [base (Exception. "hello")
          ex   (set-stack-trace-element
                 base {:class "a" :method "b" :filename "c" :line 1}
                      {:class "d" :method "e" :filename "f" :line 2})
          st   (:stack-trace (throwable->map ex))]
      (are [x y] (= x y)
        2   (count st)
        "a" (-> st first :class)
        "d" (-> st second :class))))

  (testing "empty stacktrace is valid"
    (let [base (Exception. "hello")
          ex   (set-stack-trace-element base)]
      (is ex)
      (is (-> ex throwable->map :stack-trace empty?))))

  (testing "invalid param"
    (assertion-error? (set-stack-trace-element "hello"))
    (assertion-error? (set-stack-trace-element (Exception. "hello") 'foo))))

(deftest clone-exception-test
  (testing "empty exception"
    (let [ex1 (Exception.)
          ex2 (clone-exception ex1)]
      (is (not (= ex1 ex2)))
      (is (= (throwable->map ex1) (throwable->map ex2)))))

  (testing "exception with message"
    (let [ex1 (Exception. "hello")
          ex2 (clone-exception ex1)]
      (is (not (= ex1 ex2)))
      (is (= (throwable->map ex1) (throwable->map ex2)))))

  (testing "exception with cause"
    (let [cause (Throwable. "cause")
          ex1   (Exception. cause)
          ex2   (clone-exception ex1)]
      (is (not (= ex1 ex2)))
      (is (= (throwable->map ex1) (throwable->map ex2)))))

  (testing "exception with message and cause"
    (let [cause (Throwable. "cause")
          ex1   (Exception. "hello" cause)
          ex2   (clone-exception ex1)]
      (is (not (= ex1 ex2)))
      (is (= (throwable->map ex1) (throwable->map ex2)))))

  (testing "other exception class"
    (let [ex1 (NullPointerException. "hello")
          ex2 (clone-exception ex1)]
      (is (not (= ex1 ex2)))
      (is (= (throwable->map ex1) (throwable->map ex2)))))

  (testing "invalid param"
    (assertion-error? (clone-exception "hello"))))

(deftest filter-stack-trace-test
  (let [ex (set-stack-trace-element
             (Exception. "hello")
             {:class "a" :method "b" :filename "d" :line 1}
             {:class "a" :method "b" :filename "e" :line 2}
             {:class "a" :method "c" :filename "f" :line 3})]
    (testing "no match"
      (let [ex (filter-stack-trace #(nil? (:class %)) ex)]
        (is (empty? (:stack-trace (throwable->map ex))))))

    (testing "all match"
      (let [ex (filter-stack-trace #(= "a" (:class %)) ex)]
        (is (= 3 (count (:stack-trace (throwable->map ex)))))))

    (testing "match one"
      (let [ex (filter-stack-trace #(= "d" (:filename %)) ex)
            st (:stack-trace (throwable->map ex))]
        (are [x y] (= x y)
          1   (count st)
          "a" (-> st first :class)
          "b" (-> st first :method)
          "d" (-> st first :filename)
          1   (-> st first :line)))))

  (testing "invalid param"
    (assertion-error? (filter-stack-trace (constantly true) "hello"))
    (assertion-error? (filter-stack-trace "hello" (Exception.)))))



