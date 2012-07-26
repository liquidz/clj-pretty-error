(ns pretty-error.core
  (:use
    [text-decoration.core :only [red bold underline assoc-color]]
    [clostache.parser     :only [render]]))

(declare throwable->map)

(defn- str-contains? [s target]
  (not= -1 (.indexOf s target)))

(defn- get-causes [#^Throwable ex]
  (map throwable->map
       (take-while (comp not nil?) (iterate #(.getCause %) (.getCause ex)))))

(defn- stack-trace->map [#^StackTraceElement st]
  {:obj      st
   :class    (.getClassName st)
   :filename (.getFileName st)
   :line     (.getLineNumber st)
   :method   (.getMethodName st)
   :native?  (.isNativeMethod st)
   :str      (.toString st)})

(defn- get-stack-trace [#^Throwable ex]
  (let [traces (seq (.getStackTrace ex))]
    (map stack-trace->map traces)))

(defn throwable->map [ex]
  {:message           (.getMessage ex)
   :stack-trace       (get-stack-trace ex)
   :causes            (get-causes ex)
   :str               (.toString ex)
   :localized-message (.getLocalizedMessage ex)})

(defn clone-exception [#^Exception ex]
  (let [klass  (class ex)
        msg    (.getMessage ex)
        cause  (.getCause ex)
        arg    (into-array Class (map class (remove nil? (list msg cause))))
        c      (.getConstructor klass arg)
        param  (into-array Object (remove nil? (list msg cause)))
        new-ex (.newInstance c param)]
    (.setStackTrace new-ex (.getStackTrace ex))
    new-ex))

(defn set-stack-trace-element
  [base-exception & elems]
  (let [ex (clone-exception base-exception)
        sts (map (fn [{:keys [class method filename line]}]
                   (StackTraceElement. class method filename line))
                 elems)]
    (.setStackTrace ex (into-array StackTraceElement sts))
    ex))

(defn filter-stack-trace [pred ex]
  (apply set-stack-trace-element ex (filter pred (get-stack-trace ex))))

(defn- print-cause [cause & {:keys [caused?] :or {caused? false}}]
  (let [label  (str (if caused? "Caused by " "") (:str cause))]
    (flush)
    (println "")
    (println (underline label))
    (doseq [st (:stack-trace cause)]
      (let [st (assoc-color st :method bold :filename red :line (comp red bold))]
        (println
          (render "   at {{class}} / {{method}} ({{filename}}:{{line}})" st))))))

(defn print-pretty-stack-trace
  [#^Exception ex]
  (let [[f & r] (:causes (throwable->map ex))]
    (print-cause f)
    (doseq [x r] (print-cause x :caused? true))))

