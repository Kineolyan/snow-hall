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
       (#(if (nil? %) (resolved values) (rejected %)))))

(defn- resolve-and-reduce
  [acc entry]
  (let [r (resolve-component acc entry)]
    (if (resolved? r)
      r
      (reduced r))))

(defn checked-with
  "Resolves components and runs checks on them until the first failure.
  This returns etheir all components or the first error.
  If f is provided, the components are passed to f. Otherwise, it returns
  the result wrapped in `resolved` or the first error wrapped in `rejected`."
  ([components guards]
   (checked-with components guards identity))
  ([components guards f]
   (->> (reduce 
         resolve-and-reduce
         [::ok {}] 
         components)
        (map-resolved (partial check-guards guards))
        (map-resolved f)
        second)))

(defn with
  "Resolves components in order until the first failure.
  This returns etheir all components or the first error.
  If f is provided, the components are passed to f. Otherwise, it returns
  the result wrapped in `resolved `or the first error wrapped in `rejected `."
  ([components]
   (checked-with components []))
  ([components f]
   (checked-with components [] f)))
