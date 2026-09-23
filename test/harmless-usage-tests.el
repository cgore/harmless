;;; harmless-usage-tests.el --- Tests for Harmless usage -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless-http)
(require 'harmless-openai)
(require 'harmless-usage)
(require 'harmless-xai)

(ert-deftest harmless-http-rate-limit-headers ()
  (let ((limits (harmless-http-rate-limits
                 (concat "HTTP/1.1 200 OK\n"
                         "content-type: text/event-stream\n"
                         "x-ratelimit-remaining-tokens: 80\n"
                         "x-ratelimit-limit-tokens: 100\n"
                         "x-ratelimit-remaining-requests: 9\n"
                         "anthropic-ratelimit-input-tokens-remaining: 1000\n"
                         "anthropic-ratelimit-output-tokens-remaining: 200\n"))))
    (should (equal "80" (plist-get limits :tokens-remaining)))
    (should (equal "100" (plist-get limits :tokens-limit)))
    (should (equal "9" (plist-get limits :requests-remaining)))
    (should (equal "1000" (plist-get limits :input-tokens-remaining)))
    (should (equal "200" (plist-get limits :output-tokens-remaining)))
    (should-not (plist-get limits :content-type))))

(ert-deftest harmless-usage-note-turn-sets-last-and-total ()
  (let* ((harmless-directory (make-temp-file "harmless-usage-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-make-openai-compat
                    "local" :host "127.0.0.1:9" :protocol "http"
                    :key "none" :models '("m")))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd harmless-directory
                                        :provider provider
                                        :model "m")))
    (harmless-usage-note-turn session 10 4)
    (harmless-usage-note-turn session 3 1)
    (should (= 13 (harmless-session-prompt-tokens session)))
    (should (= 5 (harmless-session-completion-tokens session)))
    (should (= 3 (harmless-session-last-prompt-tokens session)))
    (should (= 1 (harmless-session-last-completion-tokens session)))))

(ert-deftest harmless-usage-report-splits-sessions-and-accounts ()
  (let* ((harmless-directory (make-temp-file "harmless-usage-report-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (xai (harmless-make-openai-compat
               "xAI" :host "api.x.ai" :protocol "https"
               :key "none" :models '("grok-4.6")))
         (local (harmless-make-openai-compat
                 "local" :host "127.0.0.1:9" :protocol "http"
                 :key "none" :models '("m")))
         (harmless-providers (list xai local))
         (a (harmless-session-new :cwd "/proj/a" :provider xai :model "grok-4.6"))
         (b (harmless-session-new :cwd "/proj/b" :provider local :model "m")))
    (harmless-usage-note-turn a 100 20)
    (harmless-usage-note-turn b 5 1)
    (harmless-usage-record-limits
     "xAI" '(:tokens-remaining "80" :tokens-limit "100" :requests-remaining "9"))
    (let ((report (harmless-usage-report a)))
      (should (string-match-p "xAI    grok-4.6" report))
      (should (string-match-p "window        500,000" report))
      (should (string-match-p "last prompt   100" report))
      (should (string-match-p "remaining     499,900" report))
      (should (string-match-p "prompt        100" report))
      (should (string-match-p "completion    20" report))
      (should-not (string-match-p "prompt        5" report))
      (should-not (string-match-p "All sessions" report))
      (should (string-match-p "tokens remaining 80 of 100" report))
      (should (string-match-p "requests remaining 9" report))
      (should (string-match-p "local\n    no allowance report yet" report)))))

(ert-deftest harmless-usage-xai-allowance-bar ()
  (let* ((info (harmless-xai--allowance-plist
                (harmless-json-decode
                 "{\"config\":{\"creditUsagePercent\":46.2,\"currentPeriod\":{\"type\":\"USAGE_PERIOD_TYPE_WEEKLY\",\"end\":\"2026-09-27T16:59:00Z\"},\"productUsage\":[{\"product\":\"Build\",\"usagePercent\":26}]}}")
                (harmless-json-decode "{\"subscriptionTier\":\"SuperGrok\"}")))
         (lines (harmless-usage--allowance-lines info)))
    (should (equal 46.2 (plist-get info :used-percent)))
    (should (equal "WEEKLY" (plist-get info :period-type)))
    (should (string-match-p "plan SuperGrok" lines))
    (should (string-match-p "used 46% of the weekly limit" lines))
    (should (string-match-p "\\[###########-------------\\]" lines))
    (should (string-match-p "resets in " lines))
    (should (string-match-p "Build 26%" lines))))
