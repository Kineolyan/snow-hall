(ns snow-hall.rest.core)

(defn resolved
  "Marks the value as resolved in the context of a `with`"
  [value]
  [::ok value])
(defn rejected
  "Marks the value as an error in the context of a `with`."
  [error]
  [::ko error])
(defn resolved?
  [[status _ :as all]]
  (case status
    ::ok true
    ::ko false
    (throw (IllegalStateException. (str all)))))
(defn map-resolved
  [f [_ value :as r]]
  (if (resolved? r)
    (resolved (f value))
    r))

(defn- resolve-component
  [[_ result] [k resolver]]
  (let [r (resolver result)]
    (map-resolved (partial assoc result k) r)))

(defn- check-guards
  [guards values]
  (->> (map #(% values) guards)
       (filter (comp not nil?))
       first
       #(if (nil? %) (rejected %) (resolved values))))

(defn checked-with
  ([components guards]
   (checked-with components guards identity))
  ([components guards consumer]
   (->> (reduce resolve-component [::ok {}] components)
        (map-resolved (partial check-guards guards))
        (map-resolved consumer)
        second)))

(defn with
  ([components]
   (checked-with components []))
  ([components consumer]
   (checked-with components [] consumer)))

(comment
  (def ctx {:a [1 2 3] :b {:b1 10 :b2 20}})
  (def req {:body {:info "abc"}})
  (defn get-a [_] ((comp resolved first :a) ctx))
  (defn get-b [_] (resolved (get-in ctx [:b :b2])))
  (defn get-c [_] (resolved (get-in req [:body :info])))

  (resolve-component [nil {}] [:a get-a])

  (def components {:a get-a :b get-b :c get-c})
  (with components)
  (defn get-fail [_] (rejected "Oops"))
  (with {:d get-fail})

  (def s (ref nil))
  (with components #(dosync (ref-set s %)))
  @s

  (checked-with components [(comp neg? :a)]))