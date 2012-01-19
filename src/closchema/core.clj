(ns closchema.core
  "This is JSON Schema in Clojure. See
   http://tools.ietf.org/html/draft-zyp-json-schema-02 Main purposed
   is to allow object validation, but schema metadata can be used for
   exposing contracts as well."
  (:use clojure.walk clojure.template)
  (:require [clojure.set :as set]
            [clojure.data.json :as json]))


;; This could cause memory problems if there are many (say, hundreds)
;; of schemae that are being read from files
(def ^{:doc "Cache for schemas read from files" :dynamic true}
      *schema-cache* (atom {}))

(def ^{:doc "Allow validation errors to be captured." :dynamic true}
     *validation-context* nil)

(def ^{:doc "When walking an object, we keep a binding to current parent."
       :dynamic true}
     *parent* nil)


(def ^{:doc "Default processing just outputs a boolean return." :dynamic true}
  process-errors
  (fn [errors] (= (count errors) 0)))


(defmacro with-validation-context
  "Defines a binding to allow access to the root object and to enable
   invalidations to be captured. This strategy removes the need of
   raising exceptions at every single invalid point, and allows
   context information to be used when reporting about errors. Nested
   contexts are just ignored."
  [& body]
  `(let [body# #(do ~@body
                    (process-errors @(:errors *validation-context*)))]
     (if-not *validation-context*
       (binding [*validation-context* {:errors (ref '())
                                       :path (ref [])}]
         (body#))
       (body#))))


(defmacro walk-in
  "Step inside a relative path, from a previous object. This
   information is useful for reporting."
  [parent rel-path & body]
  `(binding [*parent* ~parent]
     (if-let [{path# :path} *validation-context*]
       (do
         (dosync alter path# conj ~rel-path)
         ~@body
         (dosync alter path# pop))
       (do ~@body))))


(defmacro invalid
  "Register an invalid piece of data according to schema."
  [& args]
  (let [[path args] (if (keyword? (first args))
                      [nil args] [(first args) (rest args)])
        key (first args)
        data (second args)]
    `(let [error# {:ref ~path :key ~key :data ~data}]
       (if-let [{errors# :errors path# :path} *validation-context*]
         (dosync (alter errors# conj
                        (merge {:path (conj @path# ~path)} error#))))
       (process-errors (list error#)))))


(defmacro report-errors
  "Returns all errors, instead of simple boolean."
  [& args]
  `(binding [process-errors (fn [errors#] errors#)]
     (with-validation-context
       (do ~@args))))

(defn- read-schema [loc]
  (when-not (contains? *schema-cache* loc)
    (swap! *schema-cache* assoc loc (json/read-json (slurp loc))))
  (get @*schema-cache* loc))

(defmulti validate*
  "Dispatch on object type for validation. If not implemented,
   performs only basic type validation. Users can extend which types
   are supported by implementing validation for new types."
  (fn [schema instance]
    (cond
      (vector? (:type schema)) "union"
      (:$ref schema) "ref"
      (:enum schema) "enum"
      :else (:type schema))))


(defn validate
  "Entry point. A validation context is created and validation is
   dispatched to the appropriated multimethod."
  [schema instance]
  (with-validation-context
    (validate* schema instance)))


(def default-type "object")

(defmethod validate* nil [schema instance]
  (validate (merge schema {:type default-type}) instance))

(defmethod validate* "ref" [schema instance]
  (validate (read-schema (:$ref schema)) instance))

;; Basically, try all the types with binding to a "fresh"
;; error queue.  If any of the resulting queues are empty at the
;; end, the instance validated and there's no reason to do anything.
;; If not, we pick one of the types (the first one, because why not?)
;; and put it through validation again to populate the error queue
(defmethod validate* "union" [schema instance]
  (let [current-errors #(count (deref (:errors *validation-context*)))
        error-counts (map #(binding [*validation-context* {:errors (ref '())
                                                           :path (ref [])}]
                             (validate % instance)
                             {:errors (current-errors) :schema %})
                          (:type schema))]
    (when-not (some #(= 0 (:errors %)) error-counts)
      (validate (first (:type schema)) instance))))


(def ^{:doc "Known basic types."}
     basic-type-validations
     { "object" #(map? %)
       "array" #(or (seq? %) (and (coll? %) (not (map? %))))
       "string" #(string? %)
       "number" #(number? %)
       "integer" #(integer? %)
       "boolean" #(instance? Boolean %)
       "null" #(nil? %)
       "any" (fn [] true)})


(defn check-basic-type
  "Validate basic type definition for known types."
  [{t :type :as schema} instance]


  (or (and (nil? instance) (:optional schema))
      (let [t (or t default-type)
            types (if (coll? t) t (vector t))]
        (or (reduce #(or %1 %2)
                    (map (fn [t] ((basic-type-validations t) instance))
                         types))
            (invalid :type {:expected (map str types)
                            :actual (str (type instance))})))))


(defn common-validate [schema instance]
  (check-basic-type schema instance)
  (comment TODO
           disallow
           extends))


(defmethod validate* :default [schema instance]
  (common-validate schema instance))


(defmethod validate* "object"
  [{properties-schema :properties
    additional-schema :additionalProperties
    :as schema} instance]

  (common-validate schema instance)

  #_ "validate properties defined in schema"
  (doseq [[property-name
           {optional :optional :as property-schema}] properties-schema]
    (let [property (get instance property-name)]
      (when-not (or (not (nil? property)) optional)
        (invalid property-name :required))))


  #_ "validate instance properties (using invidivual or addicional schema)"
  (if (map? instance)
    (doseq [[property-name property] instance]
      (if-let [{requires :requires :as property-schema}
               (or (and (map? properties-schema) (properties-schema property-name))
                   (and (map? additional-schema) additional-schema))]
        (do
          (when (and requires property
                     (nil? (get instance (keyword requires))))
            (invalid requires :required {:required-by property-name}))


          (when-not (and (:optional :property-schema) (nil? instance))
            (walk-in instance property-name
                     (validate property-schema property))))))
    (invalid :objects-must-be-maps {:properties properties-schema}))


  #_ "check additional properties"
  (when (false? additional-schema)
    (if-let [additionals (set/difference (set (keys instance))
                                      (set (keys properties-schema)))]
      (when (> (count additionals) 0)
        (invalid :addicional-properties-not-allowed
                 {:properties additionals})))))




(defmethod validate* "array"
  [{items-schema :items
    unique? :uniqueItems :as schema} instance]

  (common-validate schema instance)

  #_ "specific array validation"
  (let [total (count instance)]
    (do-template [key op]
                 (if-let [expected (key schema)]
                   (when (op total expected)
                     (invalid key {:expected expected :actual total})))
                 :minItems <
                 :maxItems >))

  (if-let [unique? (:uniqueItems schema)]
    (reduce (fn [l r] (when-not (= l r)
                        (invalid :uniqueItems {:l l :r r}))
              r) instance))


  #_ "treat array as object for further common validation"
  (when items-schema
    (let [obj-array (zipmap (range (count instance)) instance)
          obj-schema (cond (and (map? items-schema)
                                (:type items-schema))
                           {:type "object"
                            :additionalProperties items-schema}

                           (coll? items-schema)
                           (merge schema
                                  {:type "object"
                                   :properties (zipmap (range (count items-schema))
                                                       items-schema)}))]
      (validate obj-schema obj-array))))




(defmethod validate* "string"
  [schema instance]
  (common-validate schema instance)

  (when (schema :maxLength)
    (if-not (>= (schema :maxLength) (count instance))
      (invalid :max-length-exceeded
               {:maxLength (schema :maxLength) :actual (count instance) })))

  (when (schema :minLength)
    (if-not (<= (schema :minLength) (count instance))
      (invalid :min-length-not-reached
               {:minLength (schema :minLength) :actual (count instance) })))

  (when (schema :pattern)
    (if-not (.matches instance (schema :pattern))
      (invalid :pattern-not-matched
               {:pattern (schema :pattern) :actual instance}))))

(defmethod validate* "enum"
  [schema instance]
  (if-not (true? (some #(= % instance) (schema :enum)))
    (invalid :value-not-in-enum {:enum (schema :enum) :value instance })))


(defmethod validate* "number"
  [schema instance]
  (common-validate schema instance)
  (when (schema :maximum)
    (if-not (>= (schema :maximum) instance)
      (invalid :value-greater-then-maximum
               {:maximum (schema :maximum) :value instance })))

  (when (schema :minimum)
    (if-not (<= (schema :minimum) instance)
      (invalid :value-lower-then-minimum
               {:minimum (schema :minimum) :value instance })))

  (when (schema :exclusiveMaximum)
    (if-not (> (schema :exclusiveMaximum) instance)
      (invalid :value-greater-or-equal-then-maximum
               {:exclusiveMaximum (schema :exclusiveMaximum) :value instance })))

  (when (schema :exclusiveMinimum)
    (if-not (< (schema :minimumCanEqual) instance)
      (invalid :value-lower-or-equal-then-minimum
               {:exclusiveMinimum (schema :exclusiveMinimum) :value instance })))

  (when (schema :divisibleBy)
    (if-not (= 0 (mod instance (schema :divisibleBy)))
      (invalid :value-not-divisible-by
               {:divisibleBy (schema :divisibleBy) :value instance}))))
