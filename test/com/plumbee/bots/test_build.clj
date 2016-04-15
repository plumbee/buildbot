(ns com.plumbee.bots.test-build
  (:require [clojure.test :refer :all]
            [clojure.data.json :as json]
            [com.plumbee.bots.build :refer [handler slurp-authed config millis-since-epoch]]
            [com.plumbee.hamcrest.assert :refer [assert-that]]
            [com.plumbee.hamcrest.matchers :refer [equal-to is-not
                                                   all-of any-of
                                                   matches-re
                                                   has-item has-entry has-count has-key empty-sequence
                                                   has-str string-contains]]))


(def test-buildbot handler)

(def test-config {:jenkins-api-url     "mock-url"
                  :jenkins-api-id      "jenkins-user"
                  :jenkins-api-token   "jenkins-token"})

(def has-empty-outbox (any-of
                        (is-not (has-key :outbox))
                        (has-entry :outbox nil)
                        (has-entry :outbox (has-count 0))))

(def views
  (json/write-str
    {:views [{:name "Test View" :description "#channel-five"}]}))

(def builds
  (json/write-str
    {:jobs
     [{:name "Test-Job-1"
       :url "https://Jenkins/Test-Job-1/"
       :buildable true
       :lastCompletedBuild
       {:actions
                [{:causes
                  [{:shortDescription "Started by user Will Testmore"
                    :userId "will.testmore@plumbee.co.uk"}]}]
        :number 42
        :result "FAILURE"
        :url "https://Jenkins/Test-Job-1/42"
        :culprits
                [{:id "no-one"
                  :property [{:address "no-one@plumbee.co.uk"}]}]}}]}))

(def builds-with-upstream
  (json/write-str
    {:jobs
     [{:name "Test-Job-1"
       :url "https://Jenkins/Test-Job-1/"
       :buildable true
       :lastCompletedBuild
       {:actions
                [{:causes
                  [{:shortDescription "Started by user Will Testmore"
                    :userId "will.testmore@plumbee.co.uk"}
                   {:upstreamBuild 453
                    :upstreamUrl "job/Test-Job-2/"}]}]
        :number 46
        :result "FAILURE"
        :url "https://Jenkins/Test-Job-1/46"
        :culprits
                [{:id "no-one"
                  :property [{:address "no-one@plumbee.co.uk"}]}]}}]}))

(def builds-passed
  (json/write-str
    {:jobs
     [{:name "Test-Job-1"
       :url "https://Jenkins/Test-Job-1/"
       :buildable true
       :lastCompletedBuild
       {:actions
                [{:causes
                  [{:shortDescription "Started by user Will Testmore"
                    :userId "will.testmore@plumbee.co.uk"}]}]
        :number 42
        :result "SUCCESS"
        :url "https://Jenkins/Test-Job-1/42"
        :culprits
                [{:id "no-one"
                  :property [{:address "no-one@plumbee.co.uk"}]}]}}]}))

(def upstream-build
  (json/write-str
    {:actions
     [{:causes
       [{:shortDescription "Started by user John Smith"
         :userId           "john.smith@plumbee.co.uk"}
        ]}]
     :culprits
     [{:id       "barbara"
       :property [{:address "babs@plumbee.co.uk"}]}]}))

(defn make-mock [results]
  (let [result-stream (atom results)
        args-capture (atom [])]
    [(fn [& args]
       (swap! args-capture conj args)
       (let [result (first @result-stream)]
         (swap! result-stream rest)
         result))
     args-capture]))


(deftest buildbot-test

  (testing "new build failure augments known-failures and messages the correct channel"
    (let [[mock-slurp capture] (make-mock [views builds])]
      (binding [slurp-authed mock-slurp
                config test-config]
        (let [result (test-buildbot {} {:type :poll})]

          (assert-that result (has-entry :outbox (has-count 1)))
          (assert-that result (has-entry :outbox (has-item (has-entry :channel-id "#channel-five"))))
          (assert-that result (has-entry :outbox (has-item (has-str (all-of
                                                                      (string-contains "FAILURE")
                                                                      (string-contains "Test-Job-1")
                                                                      (string-contains "42")
                                                                      (string-contains "will.testmore")
                                                                      (string-contains "retry")
                                                                      (string-contains "console"))))))
          (assert-that @capture (is-not (empty-sequence)))
          (assert-that result (has-entry :known-failures
                                         (has-entry "Test-Job-1"
                                                    (has-entry :channels
                                                               (has-item "#channel-five")))))))))

  (testing "culprits are extracted from upstream builds"
    (let [[mock-slurp capture] (make-mock [views builds-with-upstream upstream-build])]
      (binding [slurp-authed mock-slurp
                config test-config]
        (let [result (test-buildbot {} {:type :poll})]

          (assert-that result (has-entry :outbox (has-count 1)))
          (assert-that result (has-entry :outbox (has-item (has-str (all-of
                                                                      (string-contains "FAILURE")
                                                                      (string-contains "Test-Job-1")
                                                                      (string-contains "46")
                                                                      (string-contains "will.testmore")
                                                                      (string-contains "john.smith")
                                                                      (any-of
                                                                        (string-contains "barbara")
                                                                        (string-contains "babs")))))))
          (assert-that @capture (is-not (empty-sequence)))
          (assert-that result (has-entry :known-failures
                                         (has-entry "Test-Job-1"
                                                    (has-entry :channels
                                                               (has-item "#channel-five")))))))))


  (testing "reminders for unclaimed builds"
    (let [[mock-slurp _] (make-mock [views builds])
          some-point-in-time 1449235075476
          just-over-half-an-hour (inc (* 30 60 1000))
          a-later-point-in-time (+ some-point-in-time just-over-half-an-hour)]
      (binding [slurp-authed mock-slurp
                config test-config
                millis-since-epoch #(do a-later-point-in-time)]
        (let [result (test-buildbot {:known-failures {"Test-Job-1" {:timestamp some-point-in-time
                                                                    :url "https://Jenkins/job/Test-Job-1/"
                                                                    :channels #{"#channel-five"}}}}
                                    {:type :poll})]

          (assert-that result (has-entry :outbox (has-count 1)))
          (assert-that result (has-entry :outbox (has-item (has-entry :channel-id "#channel-five"))))
          (assert-that result (has-entry :outbox (has-item (has-str (all-of
                                                                      (string-contains "<!here>")
                                                                      (string-contains "Test-Job-1")
                                                                      (string-contains "still broken!"))))))
          (assert-that result (has-entry :known-failures
                                         (has-entry "Test-Job-1"
                                                    (has-entry :reminded a-later-point-in-time))))))))

  (testing "reminders for claimed builds"
    (let [[mock-slurp _] (make-mock [views builds])
          some-point-in-time 1449235075476
          just-over-four-hours (inc (* 4 60 60 1000))
          a-later-point-in-time (+ some-point-in-time just-over-four-hours)]
      (binding [slurp-authed mock-slurp
                config test-config
                millis-since-epoch #(do a-later-point-in-time)]
        (let [result (test-buildbot {:known-failures {"Test-Job-1" {:timestamp some-point-in-time
                                                                    :url "https://Jenkins/job/Test-Job-1/"
                                                                    :channels #{"#channel-five"}
                                                                    :owners #{"Bob"}}}}
                                    {:type :poll})]

          (assert-that result (has-entry :outbox (has-count 1)))
          (assert-that result (has-entry :outbox (has-item (has-entry :user-id "Bob"))))
          (assert-that result (has-entry :outbox (has-item (has-str (all-of
                                                                      (string-contains "Don't forget")
                                                                      (string-contains "Test-Job-1"))))))
          (assert-that result (has-entry :known-failures
                                         (has-entry "Test-Job-1"
                                                    (has-entry :reminded a-later-point-in-time))))))))

  (testing "notification that build has passed"
    (let [[mock-slurp _] (make-mock [views builds-passed])]
      (binding [slurp-authed mock-slurp
                config test-config]
        (let [result (test-buildbot {:known-failures {"Test-Job-1" {:timestamp 0
                                                                    :url "https://Jenkins/job/Test-Job-1/"
                                                                    :channels #{"#channel-five"}
                                                                    :owners #{"Bob"}}}}
                                    {:type :poll})]

          (assert-that result (has-entry :outbox (has-count 1)))

          (assert-that result (has-entry :outbox (has-item (has-entry :channel-id "#channel-five"))))
          (assert-that result (has-entry :outbox (has-item (has-str (all-of
                                                                      (string-contains "Bob")
                                                                      (string-contains "Test-Job-1")
                                                                      (string-contains "passed"))))))

          (assert-that result (has-entry :known-failures (is-not (has-key "Test-Job-1"))))))))

  (testing "adding a reaction claims a build"
    (let [[mock-slurp _] (make-mock [views builds-passed])]
      (binding [slurp-authed mock-slurp
                config test-config]
        (let [result (test-buildbot {:known-failures {"Test-Job-1" {:timestamp 0
                                                                    :url "https://Jenkins/job/Test-Job-1/"
                                                                    :channels #{"#channel-five"}
                                                                    :owners #{"Bob"}}}}
                                    {:type "reaction_added"
                                     :user "Cleopatra"
                                     :reacted {:username "BuildBot"
                                               :attachments [{:fallback "Test-Job-1"}]}})]

          (assert-that result has-empty-outbox)
          (assert-that result (has-entry :known-failures (has-entry "Test-Job-1"
                                                                    (has-entry :owners #{"Bob" "Cleopatra"}))))))))

  (testing "deleting a reaction unclaims a build"
    (let [[mock-slurp _] (make-mock [views builds-passed])]
      (binding [slurp-authed mock-slurp
                config test-config]
        (let [result (test-buildbot {:known-failures {"Test-Job-1" {:timestamp 0
                                                                    :url "https://Jenkins/job/Test-Job-1/"
                                                                    :channels #{"#channel-five"}
                                                                    :owners #{"Bob" "Frances"}}}}
                                    {:type "reaction_removed"
                                     :user "Frances"
                                     :reacted {:username "BuildBot"
                                               :attachments [{:fallback "Test-Job-1"}]}})]

          (assert-that result has-empty-outbox)
          (assert-that result (has-entry :known-failures (has-entry "Test-Job-1"
                                                                    (has-entry :owners #{"Bob"})))))))))