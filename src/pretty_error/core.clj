(ns pretty-error.core
  (:use
    [text-decoration.core :only [red bold underline assoc-color]]
    [clostache.parser     :only [render]]))

(declare throwable->map)

(def ^:dynamic *pretty-label-format*
  "\n{{label}}")
(def ^:dynamic *pretty-error-format*
  "   at {{class}} / {{method}} ({{filename}}:{{line}})")
(def ^:dynamic *pretty-cause-text*
  "Caused by ")
(def ^:dynamic *pretty-error-color*
  {:label underline :method bold :filename red :line (comp red bold)})

;; Private functions

(defn- str-contains? [s target]
  (not= -1 (.indexOf s target)))

(defn- get-causes [#^Throwable ex]
  {:pre [(instance? Throwable ex)]}
  (map throwable->map
       (take-while (comp not nil?) (iterate #(.getCause %) (.getCause ex)))))

(defn- stack-trace->map [#^StackTraceElement st]
  {:pre [(instance? StackTraceElement st)]}
  {:obj      st
   :class    (.getClassName st)
   :filename (.getFileName st)
   :line     (.getLineNumber st)
   :method   (.getMethodName st)
   :native?  (.isNativeMethod st)
   :str      (.toString st)})

;; Public functions

; =get-stack-trace
(defn get-stack-trace
  "Get stack trace list which is converted to map."
  [#^Throwable ex]
  {:pre [(instance? Throwable ex)]}
  (let [traces (seq (.getStackTrace ex))]
    (map stack-trace->map traces)))

; =throwable->map
(defn throwable->map
  "Convert java.lang.Throwable to map."
  [ex]
  {:pre [(instance? Throwable ex)]}
  {:message           (.getMessage ex)
   :stack-trace       (get-stack-trace ex)
   :causes            (get-causes ex)
   :str               (.toString ex)
   :localized-message (.getLocalizedMessage ex)})

; =clone-exception
(defn clone-exception
  "Clone java.lang.Exception instance."
  [#^Exception ex]
  {:pre [(instance? Exception ex)]}
  (let [klass  (class ex)
        msg    (.getMessage ex)
        arg    (if msg (list msg) ())
        c      (.getConstructor klass (into-array Class (map class arg)))
        new-ex (.newInstance c (into-array Object arg))]
    (.setStackTrace new-ex (.getStackTrace ex))
    (.initCause new-ex (.getCause ex))
    new-ex))

; =set-stack-trace-element
(defn set-stack-trace-element
  "Set stack trace element list to specified java.lang.Exception."
  [base-exception & elems]
  {:pre [(instance? Exception base-exception)
         (or (empty? elems)
             (every? #(or (map? %) (instance? StackTraceElement %)) elems))]}
  (let [ex  (clone-exception base-exception)
        sts (map #(if (instance? StackTraceElement %) %
                    (StackTraceElement. (:class %) (:method %) (:filename %) (:line %)))
                 elems)]
    (.setStackTrace ex (into-array StackTraceElement sts))
    ex))

; =filter-stack-trace
(defn filter-stack-trace
  "Filter stack trace elements in specified java.lang.Exception."
  [pred ex]
  {:pre [(fn? pred)
         (instance? Exception ex)]}
  (apply set-stack-trace-element ex (map :obj (filter pred (get-stack-trace ex)))))

; =print-error
(defn- print-error
  "Print one pretty error."
  [ex & {:keys [caused?] :or {caused? false}}]
  (let [label (str (if caused? *pretty-cause-text* "") (:str ex))
        label ((:label *pretty-error-color*) label)]
    (flush)
    (println (render *pretty-label-format* {:label label}))
    (doseq [st (:stack-trace ex)]
      (let [st   (apply assoc-color st (flatten (seq *pretty-error-color*)))
            text (render *pretty-error-format* st)]
        (println text)))))

; =print-pretty-stack-trace
(defn print-pretty-stack-trace
  "Print pretty stack trace."
  [#^Exception ex]
  {:pre [(instance? Exception ex)]}
  (let [ex (throwable->map ex)]
    (print-error ex)
    (doseq [x (:causes ex)] (print-error x :caused? true))))

