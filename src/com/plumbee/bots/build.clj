(ns com.plumbee.bots.build
  (:require [ring.util.codec :refer [url-encode base64-encode]]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [com.plumbee.plumbot.support.persistence :refer [persistent-atom load-data]]
            [com.plumbee.plumbot.support.logging :as log]
            [com.plumbee.plumbot.support.message :refer [skin-tone join-seq de-link]]
            [com.plumbee.plumbot.components.timing :refer [seconds minutes hours]])
  (:import (java.net URL)))


(def state (persistent-atom :buildbot-state
                            (constantly {:cron {:poll "* 8-17 * * 1-5" ; every minute from 08:00 until 17:59 Mon-Fri
                                                :stats "0 13 * * 1-5"}}))) ; 13:00 Mon-Fri

(def ^:dynamic config
  "This is dynamically bound to facilitate mocking."
  (load-data :buildbot-config (constantly {:jenkins-api-url     "https://<your cloudbees url goes here>"
                                           :jenkins-api-id      "<your jenkins api id goes here>"
                                           :jenkins-api-token   "<your jenkins api token goes here>"
                                           :override-channel-id nil ; useful for testing
                                           :stats-channel-id    "<a slack channel for daily stats goes here>"})))

(defn ^:dynamic slurp-authed
  "This is dynamically bound to facilitate mocking."
  [url user-agent user-id user-token]
  (-> url
      (URL.)
      (.openConnection)
      (doto (.setRequestProperty "User-Agent" user-agent))
      (doto (.setRequestProperty "Authorization"
                                 (str "Basic " (base64-encode (.getBytes (str user-id ":" user-token))))))
      (.getInputStream)
      slurp))

(defn ^:dynamic millis-since-epoch
  "This is dynamically bound to facilitate mocking."
  []
  (System/currentTimeMillis))


(defn poll-jenkins-views [{:keys [jenkins-api-url
                                  jenkins-api-id
                                  jenkins-api-token]}]
  (slurp-authed
    (str jenkins-api-url "/api/json?pretty=true&tree=views[name,description]")
    "Slackbot"
    jenkins-api-id
    jenkins-api-token))


(def culprits-query "culprits[id,property[address]]")
(def actions-query "actions[causes[shortDescription,upstreamUrl,upstreamBuild,userId]]")

(defn get-jobs [{:keys [jenkins-api-url
                        jenkins-api-id
                        jenkins-api-token
                        override-channel-id]}
                view]
  (map #(assoc % :channel (or override-channel-id (:channel view)))
       (-> (slurp-authed
             (str jenkins-api-url
                  "/view/"
                  (url-encode (:name view))
                  (str "/api/json?tree=jobs[name,url,buildable,lastCompletedBuild[number,result,url," culprits-query "," actions-query "]]"))
             "Slackbot"
             jenkins-api-id
             jenkins-api-token)
           (json/read-str :key-fn keyword)
           :jobs)))

(defn with-channel [{:keys [description] :as view}]
  (if-let [groups (re-find #"(#[A-Za-z\-]+)" (or description ""))]
    (assoc view :channel (groups 1))))

(defn poll-jenkins []
  (let [all-views (:views (json/read-str (poll-jenkins-views config) :key-fn keyword))
        views-with-channel (filter identity (map with-channel all-views))
        jobs (apply concat (map #(get-jobs config %) views-with-channel))]
    jobs))

(defn get-jenkins-job-info [{:keys [jenkins-api-url jenkins-api-id jenkins-api-token]} job-url build-number]
  (json/read-str (slurp-authed
                   (str jenkins-api-url "/" job-url build-number "/api/json?pretty=true&tree=" culprits-query "," actions-query)
                   "Slackbot"
                   jenkins-api-id
                   jenkins-api-token) :key-fn keyword))

(defn job-status [job]
  (:result (:lastCompletedBuild job)))

(defn success? [x] (= "SUCCESS" (job-status x)))
(def failed? (complement success?))

(defn known-to [known-failures]
  (fn [job]
    (get known-failures (:name job))))

(defn failure-message [job user-lookup]
  (let [culprits (:all-culprits job)
        named-culprits (apply str (interpose ", " (map #(get user-lookup % %) culprits)))
        unaliased-culprits (filter #(not (get user-lookup %)) culprits)
        help-me (if (empty? unaliased-culprits)
                  ""
                  (str ":question: Please tell me who these people are: " (join-seq ", " unaliased-culprits)))
        causes (string/join "\n"
                            (map :shortDescription
                                 (apply concat (map :causes (:actions (:lastCompletedBuild job))))))
        build-description (str "<" (:url job) "|" (:name job) ">, "
                               "build <" (:url (:lastCompletedBuild job)) "|number " (:number (:lastCompletedBuild job)) ">"
                               " is in state '" (job-status job) "' "
                               "(<" (:url (:lastCompletedBuild job)) "retry|retry> | "
                               "<" (:url (:lastCompletedBuild job)) "consoleFull|console>)")
        text (join-seq "\n" [build-description named-culprits causes])
        fallback-text (:name job)]
    {:type       :message
     :channel-id (:channel job)
     :text       help-me
     :params     {:icon_emoji  ":aperture:"
                  :attachments [{:color "#ff9c00"
                                 :text  text
                                 :fallback fallback-text}]}}))

(defn success-message [job known-failures]
  (let [job-name (:name job)
        user-names (:owners (get known-failures job-name))
        user-msg (if (empty? user-names) "" (str (join-seq " " (map #(str "<@" % ">") user-names)) ": "))
        text (str user-msg job-name " has passed! " (skin-tone ":thumbsup:"))
        fallback-text (str job-name " has passed! ")]
    {:type       :message
     :channel-id (:channel job)
     :text       ""
     :params     {:attachments [{:color "#22FF44"
                                 :text  text
                                 :fallback fallback-text}]}}))

(defn unclaimed-reminder-message [text fallback-text channel]
  {:type       :message
   :channel-id channel
   :text       ":exclamation: Please add a reaction to this message if you're looking into the failure."
   :params     {:icon_emoji  ":aperturered:"
                :attachments [{:color "#ee0000" :text text :fallback fallback-text}]}})

(defn thank-you-message [channel]
  {:type       :message
   :channel-id channel
   :text       "Thanks!"})

(defn unclaimed-reminder-messages [[name metadata]]
  (let [text (str "<!here> <" (:url metadata) "|" name "> is still broken!")
        fallback-text name]
    (map #(unclaimed-reminder-message text fallback-text %) (:channels metadata))))

(defn claimed-reminder-message [text fallback-text user]
  {:type    :direct
   :user-id user
   :text    ""
   :params  {:icon_emoji  ":apertureblue:"
             :attachments [{:color "#5cbeed" :text text :fallback fallback-text}]}})

(defn claimed-reminder-messages [[name metadata]]
  (let [text (str "Don't forget about <" (:url metadata) "|" name ">...")
        fallback-text name]
    (map #(claimed-reminder-message text fallback-text %) (:owners metadata))))

(defn existing-metadata-or-new [failed-job known-failures]
  (let [state-for-job (get known-failures (:name failed-job) {})
        all-channels (into #{} (filter identity (cons (:channel failed-job) (:channels state-for-job))))]
    (merge
      {:timestamp (millis-since-epoch)}
      state-for-job
      {:url      (:url failed-job)
       :channels all-channels})))

(defn make-known-failures [known-failures current-failures]
  (zipmap (map :name current-failures)
          (map #(existing-metadata-or-new % known-failures) current-failures)))

(def unclaimed? (comp empty? :owners))
(def claimed? (comp not unclaimed?))

(def claimed-interval (* 4 hours))
(def initial-unclaimed-interval (* 15 minutes))
(defn backoff [x] (* 2 x))

(defn normalise-interval [[name build-state]]
  (let [unclaimed-interval (or (:interval build-state) initial-unclaimed-interval)
        new-interval (if (claimed? build-state)
                       claimed-interval
                       unclaimed-interval)]
    [name (assoc build-state :interval new-interval)]))

(defn due? [[_ build-state]]
  (let [now (millis-since-epoch)
        last-message (or (:reminded build-state) (:timestamp build-state))
        reminder-interval (:interval build-state)]
    (< reminder-interval (- now last-message))))

(defn backoff-interval [[name build-state]]
  (let [interval (backoff (:interval build-state))]
    [name (assoc build-state :interval interval)]))

(defn update-timestamp [[name build-state]]
  (let [now (millis-since-epoch)]
    [name (assoc build-state :reminded now)]))

(defn consider-reminders [{:keys [known-failures] :as state}]
  (let [normalised (into {} (map normalise-interval known-failures))
        to-remind (filter due? normalised)
        unclaimed (filter (comp unclaimed? second) to-remind)
        claimed (filter (comp claimed? second) to-remind)
        updated-unclaimed (into {} (map (comp update-timestamp backoff-interval) unclaimed))
        updated-claimed (into {} (map update-timestamp claimed))]
    (-> state
        (update-in [:outbox] concat (mapcat unclaimed-reminder-messages unclaimed))
        (update-in [:outbox] concat (mapcat claimed-reminder-messages claimed))
        (update-in [:known-failures] merge normalised updated-claimed updated-unclaimed))))

(defn process-reaction [state reacted reaction-user update-owners-function]
  (let [we-care (= (:username reacted) "BuildBot")
        build-name (-> reacted :attachments first :fallback)]
    (if we-care
      (update-in state [:known-failures build-name :owners] update-owners-function reaction-user)
      state)))

(defn get-more-culprits [build limit]
  (if (<= limit 0)
    #{}
    (let [actions (:actions build)
          causes (mapcat :causes actions)
          user-causes (filter identity (map :userId causes))
          upstream-causes (filter :upstreamBuild causes)
          get-build #(get-jenkins-job-info config (:upstreamUrl %) (:upstreamBuild %))
          upstream-builds (map get-build upstream-causes)]
      (set (concat user-causes
                   (map :id (:culprits build))
                   (mapcat get-more-culprits upstream-builds (repeat (dec limit))))))))

(defn enrich-new-failure [new-failure]
  (assoc new-failure :all-culprits
                     (get-more-culprits (:lastCompletedBuild new-failure) 10)))

(defn enrich-new-failures [new-failures]
  (map enrich-new-failure new-failures))


(defn decay [scores]
  (into {} (for [[k v] scores :when (< 0.5 v)] [k (* 0.995 v)])))

(defn merge-stats [{:keys [scores streaks]} failures]
  (let [fails (into {} (map #(vector (str "<" (:url %) "|" (:name %) ">") 1) failures))
        new-streaks (into {} (for [[k v] fails] [k (+ v (get streaks k 0))] ))
        new-scores (merge-with + (decay scores) fails)]
    {:scores new-scores :streaks new-streaks}))


(defn stats-message [text]
  {:type       :message
   :channel-id (or (:override-channel-id config) (:stats-channel-id config))
   :text       ""
   :params     {:icon_emoji  ":aperture:"
                :attachments [{:color    "#ff3399"
                               :text     text
                               :fallback "Statistics"
                               :mrkdwn_in ["text"]}]}})

(defn make-stats-messages [{:keys [scores streaks]}]
  (let [least-loved (map (fn [rank [name _]] (str rank ". " name "\n"))
                         (iterate inc 1)
                         (take 3 (sort-by second > scores)))
        least-loved-message (when (seq least-loved)
                              (str "*Least loved builds:* :cry:\n" (apply str least-loved)))
        longest-streaks (map (fn [[name streak]] (str name " (" streak " minutes)" "\n"))
                             (take 3 (sort-by second > streaks)))
        longest-streaks-message (when (seq longest-streaks)
                                  (str "*Longest ongoing failures:* :fire:\n" (apply str longest-streaks)))
        messages (map stats-message (filter identity [least-loved-message longest-streaks-message]))]
    messages))

(defn handler [{:keys [known-failures user-lookup] :as state}
               {type :type event-channel :channel reacted :reacted reaction-user :user text :text}]
  (cond
    (= type :poll) (let [jobs (filter :buildable (poll-jenkins))
                         current-failures (filter failed? jobs)
                         current-successes (filter success? jobs)
                         new-failures (enrich-new-failures (filter (complement (known-to known-failures)) current-failures))
                         new-successes (filter (known-to known-failures) current-successes)]
                     (log/info "polled!")
                     (-> state
                         (update-in [:outbox] concat (map #(failure-message % user-lookup) new-failures))
                         (update-in [:outbox] concat (map #(success-message % known-failures) new-successes))
                         (assoc :known-failures (make-known-failures known-failures current-failures))
                         consider-reminders
                         (update-in [:stats] merge-stats current-failures)))

    (= type :stats) (update-in state [:outbox] concat (make-stats-messages (:stats state)))

    (= type "reaction_added") (process-reaction state reacted reaction-user (comp set conj))

    (= type "reaction_removed") (process-reaction state reacted reaction-user disj)

    (= type "message") (if-let [groups (and text (re-find #"[Bb]uild[Bb]ot:?\s+(\S+)\s+is\s+(\S+)" text))]
                         (let [jenkins-alias (de-link (nth groups 1))
                               slack-alias (nth groups 2)]
                           (-> state
                               (assoc-in [:user-lookup jenkins-alias] slack-alias)
                               (update-in [:outbox] conj (thank-you-message event-channel))))
                         state)

    :else state))
