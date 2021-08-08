[![Build Status](https://github.com/atgreen/didit/actions/workflows/build.yml/badge.svg)](https://github.com/atgreen/didit/actions)

# didit

This is a WIP.

Didit waits for things to happen, and if they don't happen on
schedule, it will send an alert or trigger automation.

Users define the alert mechanisms and 'didit' schedules in git hosted
repos.  These repos must contain a `didit.ini` file that has two major
sections: `alerts` and `didit`.  Here's what one might look like:

    [alerts]

    [alerts.slack]
    type = "slack"
    webhook-url = "https://hooks.slack.com/services/MY_WEBHOOK_URL"

    [didit]

    [didit.alive]
    name = "My every-5-minute check"
    cron = "(0-59)/5 * * * *"
    alert = "slack"
    token = "zsdaadfgzdszedfgzs89y9"

This defines one slack alert mechanism and one didit.  The slack alert
mechanism uses slack's webhook feature to post messages to a channel.
The didit defines a cron-style schedule that checks every 5 minutes.
But what does it check?  It simply checks that the
`didit.example.com/didit/zsdaadfgzdszedfgzs89y9` endpoint was hit at
least once during that 5 minute period.  Once it has checked, it
clears the state associated with that endpoint and checks again in 5
minutes.
