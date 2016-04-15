(defproject buildbot "0.1"
  :description "A Clojure Slack Bot to report on Jenkins jobs."
  :url "http://plumbee.com"
  :license {:name "Apache License"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :signing {:gpg-key "tech-external@plumbee.co.uk"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [com.plumbee/plumbot "0.0.2"]
                 [com.plumbee/clojure-hamcrest "0.2"]])
