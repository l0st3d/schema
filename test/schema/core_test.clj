(ns schema.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as st]
            [l0st3d.util.schema :as s])
  (:import [java.util Date]
           [clojure.lang ExceptionInfo]))

(deftest should-validate-some-data
  (testing "data structures"
    (let [person (s/with-modifications
                   (s/with-defaults
                     (s/map-of :id (s/all-of (s/is-integer) (s/unique))
                               :company-id (s/all-of (s/one-of (s/is-integer)
                                                               (s/is-nil))
                                                     (s/foreign-key :company))
                               :active (s/is-boolean)
                               :name (s/trimmed (s/is-string))
                               :date-of-birth (s/is-date)
                               :favourite-foods (s/all-of (s/list-of (s/is-string))
                                                          (s/count-is "at least one" >= 1))
                               :some-numbers (s/one-of (s/is-nil)
                                                       (s/list-of (s/one-of (s/is-decimal)
                                                                            (s/is-integer))))
                               :roles (s/one-of (s/equals :normal-user)
                                                (s/equals :admin-user)
                                                (s/equals :read-only)))
                     :favourite-foods ["dougnuts"]
                     :roles :read-only
                     :active true)
                   (fn [{:keys [favourite-foods] :as p}]
                     (assoc p :favourite-foods (map st/lower-case favourite-foods))))
          company (s/map-of :id (s/all-of (s/is-integer) (s/unique))
                            :name (s/is-string))
          company-map (s/map-of-values :id company)
          new-p (s/get-constructor person)
          new-c (s/get-constructor company)
          
          expected-person {:id 1
                           :active true
                           :name "Fred Bloggs"
                           :date-of-birth (Date. 0)
                           :roles :read-only
                           :favourite-foods ["apples"]}
          test-data {:id "1"
                     :name " Fred Bloggs "
                     :date-of-birth "1970-01-01T00:00:00.000Z"}]
      (testing "keywords"
        (is (= (assoc expected-person :roles :admin-user :favourite-foods ["dougnuts"])
               (-> test-data
                 (assoc :roles :admin-user)
                 (s/validate person)))))
      (testing "defaults can be overridden"
        (is (= expected-person
               (-> test-data
                 (assoc :favourite-foods ["apples"])
                 (s/validate person)))))
      (testing "modifications are run"
        (is (= (assoc expected-person :favourite-foods ["apples" "oranges"])
               (-> test-data
                 (assoc :favourite-foods ["APPLES" "Oranges"])
                 (s/validate person)))))
      (let [test-data (assoc test-data :roles :super-user :id "not a number")]
        (testing "get errors"
          (is (= #{"[:roles] Should be ':normal-user' or Should be ':admin-user' or Should be ':read-only'"
                   "[:id] should be a number"}
                 (into #{} (s/get-errors test-data person)))))
        (testing "validation throws exceptions"
          (is (thrown? ExceptionInfo (s/validate test-data person)))))
      (testing "get data ignoring errors"
        (is (= (-> expected-person
                 (assoc :roles :super-user :id "not a number" :favourite-foods ["dougnuts"]))
               (-> test-data
                 (assoc :roles :super-user :id "not a number")
                 (s/get-data-ignoring-errors person)))))
      (testing "merge and validate"
        (is (= (assoc expected-person :favourite-foods ["apples" "oranges"])
               (s/merge-and-validate {:favourite-foods ["APPLES" "Oranges"]} test-data person))))
      (testing "constructor"
        (let [c (new-c 1 "ACME Ltd")]
          (is (= (assoc expected-person :date-of-birth #inst "2000-01-01T12:34:56.789Z" :some-numbers [1M 2.5M] :favourite-foods ["dougnuts"] :company-id 1)
                 (new-p 1 c "Fred Bloggs" "2000-01-01T12:34:56.789Z" [1 2.5])))))
      (testing "metadata"
        (is (= {::s/unique-paths [[:id]] ::s/valid true ::s/foreign-key-paths {:company-id :company}} (meta (s/validate test-data person)))))
      (testing "validators"
        (let [people (atom [] :validator (s/validate (s/list-of person)))
              c (new-c 1 "ACME Ltd")
              p (new-p 1 c "Fred Bloggs" "2000-01-01T12:34:56.789Z" [1 2.5])]
          (is (= [p] (swap! people conj p)))
          (is (thrown? ExceptionInfo (swap! people conj c)))
          (is (= [p] @people))))
      (testing "map of values"
        (let [cs {1 (new-c 1 "ACME Ltd")
                  "2" (new-c 2 "Bizniss Ltd")}]
          (is (= {1 {:name "ACME Ltd" :id 1}
                  2 {:name "Bizniss Ltd" :id 2}}
                 (s/validate cs company-map)))
          (is (= [] (s/get-errors cs company-map))))))))

