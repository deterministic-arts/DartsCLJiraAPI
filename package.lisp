

(defpackage "DARTS.LIB.JIRA-API"
  (:use 
     "COMMON-LISP" "DRAKMA" "DARTS.LIB.TRIVIA" "CL-BASE64" "PURI" 
     "TRIVIAL-UTF-8" "DARTS.LIB.WBTREE" "LOCAL-TIME" "CL-PPCRE"
     "STRING-CASE")
  (:export 
     "JIRA-ERROR" "TRANSPORT-ERROR-BODY" "TRANSPORT-ERROR-STATUS" 
     "TRANSPORT-ERROR-SESSION" "TRANSPORT-ERROR" "PARSER-ERROR" 
     "PARSER-ERROR-EXPECTED-TYPE" "PARSER-ERROR-DATUM" "PARSER-ERROR-PATH"
     "SKIP-OFFENDING-VALUE"

     "CONNECTOR" "CREDENTIALS" "SESSION" "USERNAME-PASSWORD" 
     "ATTRIBUTE-TREE" "CREDENTIALS-USERNAME" "CREDENTIALS-PASSWORD" 
     "CONNECTOR-BASE-URI" "SESSION-CREDENTIALS" "SESSION-CONNECTOR" 
     "*DEFAULT-SESSION*" 

     "RESOURCE" "RESOURCE-URI" "AVATAR" "AVATAR-URI" "AVATAR-WIDTH" 
     "AVATAR-HEIGHT" "AVATAR-URI" "USER" "USER-NAME" "USER-DISPLAY-NAME" 
     "USER-EMAIL-ADDRESS" "USER-AVATARS" "DESCRIPTOR" "DESCRIPTOR-NAME" 
     "DESCRIPTOR-ID" "DESCRIPTOR-ICON-URI" "DESCRIPTOR-DESCRIPTION" 
     "ISSUE" "ISSUE-ID" "ISSUE-KEY" "ISSUE-FIELDS"

     "SEARCH-ISSUES" "FIND-ISSUE" "INTERN-SESSION-OBJECT" "FIND-USER"))