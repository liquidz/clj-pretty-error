(ns pretty-error.core-test
  (:use clojure.test
        pretty-error.core))

(defmacro assertion-error? [body]
  `(is (~'thrown? AssertionError ~body)))

(deftest get-stack-trace-test
  (testing "simple exception stack trace"
    (let [st (get-stack-trace (Exception.))
          st1 (first st)]
      (are [x] (true? x)
        (> (count st) 0)
        (contains? st1 :class)
        (contains? st1 :method)
        (contains? st1 :filename)
        (contains? st1 :line)
        (contains? st1 :native?)
        (contains? st1 :str))))
  (testing "invalid param"
    (assertion-error? (get-stack-trace "hello"))))

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


(deftest set-stack-trace-element!-test
  (testing "side-effect"
    (let [base (Exception. "hello")
          ex   (set-stack-trace-element! base {:class "foo" :method "bar" :filename "baz" :line 1})]
      (is (= base ex))
      (is (= (throwable->map base) (throwable->map ex)))))

  (testing "overwrited stack trace"
    (let [base (Exception. "hello")
          ex   (set-stack-trace-element! base {:class "foo" :method "bar" :filename "baz" :line 123})
          st   (:stack-trace (throwable->map ex))]
      (are [x y] (= x y)
        1     (count st)
        "foo" (-> st first :class)
        "bar" (-> st first :method)
        "baz" (-> st first :filename)
        123   (-> st first :line))))

  (testing "set multiple stack traces"
    (let [base (Exception. "hello")
          ex   (set-stack-trace-element!
                 base {:class "a" :method "b" :filename "c" :line 1}
                      {:class "d" :method "e" :filename "f" :line 2})
          st   (:stack-trace (throwable->map ex))]
      (are [x y] (= x y)
        2   (count st)
        "a" (-> st first :class)
        "d" (-> st second :class))))

  (testing "empty stacktrace is valid"
    (let [base (Exception. "hello")
          ex   (set-stack-trace-element! base)]
      (is ex)
      (is (-> ex throwable->map :stack-trace empty?))))

  (testing "StackTraceElement instances"
    (let [base (Exception. "hello")
          ex   (set-stack-trace-element!
                 base
                 (StackTraceElement. "a" "b" "c" 1)
                 (StackTraceElement. "d" "e" "f" 2))
          sts  (:stack-trace (throwable->map ex))]
      (is ex)
      (are [x y] (= x y)
        2   (count sts)
        "a" (-> sts first :class)
        "b" (-> sts first :method)
        "c" (-> sts first :filename)
        1   (-> sts first :line))))

  (testing "invalid param"
    (assertion-error? (set-stack-trace-element! "hello"))
    (assertion-error? (set-stack-trace-element! (Exception. "hello") 'foo))))



(deftest sample-output
  (try (.foo nil)
    (catch Exception e
      (println "======= TEST OUTPUT START =======")
      (print-pretty-stack-trace e)
      (println "-----------------")
      (print-pretty-stack-trace e :filter #(not= -1 (.indexOf (:str %) "user")))
      (println "======= TEST OUTPUT END ======="))))

