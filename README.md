DartsCLJiraAPI
==============

This will become eventually a simple client for Common Lisp
to talk to a Jira server via its REST API. Right now, a few
read-only requests are already implemented:

  - search-issues QUERY &key SESSION OBJECT-CACHE LIMIT OFFSET FIELDS EXPAND
  - find-issue KEY &key SESSION OBJECT-CACHE FIELDS EXPAND IF-DOES-NOT-EXIST DEFAULT
  - find-user NAME &key SESSION OBJECT-CACHE IF-DOES-NOT-EXIST DEFAULT
  - list-priorities &key SESSION
  - list-states &key SESSION
  - list-resolutions &key SESSION
  - list-issue-types &key SESSION

More to come.
