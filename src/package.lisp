#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- JIRA REST API Client
  Copyright (c) 2013 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#


(defpackage "DARTS.LIB.JIRA-API"
  (:use 
     "COMMON-LISP" "DRAKMA" "DARTS.LIB.TRIVIA" "CL-BASE64" "PURI" 
     "TRIVIAL-UTF-8" "DARTS.LIB.WBTREE" "LOCAL-TIME" "CL-PPCRE"
     "STRING-CASE" "DARTS.LIB.MIME-TYPE")
  (:export 
     "JIRA-ERROR" "TRANSPORT-ERROR-BODY" "TRANSPORT-ERROR-STATUS" 
     "TRANSPORT-ERROR-SESSION" "TRANSPORT-ERROR" "PARSER-ERROR" 
     "PARSER-ERROR-EXPECTED-TYPE" "PARSER-ERROR-DATUM" "PARSER-ERROR-PATH"
     "SKIP-OFFENDING-VALUE" "SKIPPING-OFFENDING-VALUES"

     "CONNECTOR" "CREDENTIALS" "SESSION" "USERNAME-PASSWORD" 
     "ATTRIBUTE-TREE" "CREDENTIALS-USERNAME" "CREDENTIALS-PASSWORD" 
     "CONNECTOR-BASE-URI" "SESSION-CREDENTIALS" "SESSION-CONNECTOR" 
     "SESSION-COOKIE-JAR" "*DEFAULT-SESSION*" "RESET-SESSION" 
     "SESSION-ISSUE-FIELD-INDEX"

     "RESOURCE" "RESOURCE-URI" "AVATAR" "AVATAR-URI" "AVATAR-WIDTH" 
     "AVATAR-HEIGHT" "AVATAR-URI" "USER" "USER-NAME" "USER-DISPLAY-NAME" 
     "USER-EMAIL-ADDRESS" "USER-AVATARS" "DESCRIPTOR" "DESCRIPTOR-NAME" 
     "DESCRIPTOR-ID" "DESCRIPTOR-ICON-URI" "DESCRIPTOR-DESCRIPTION" 
     "ISSUE" "ISSUE-ID" "ISSUE-KEY" "ISSUE-FIELDS" "USER-URI" "ISSUE-URI" 
     "PRIORITY-URI" "STATUS-URI" "RESOLUTION-URI" "ISSUE-TYPE-URI" "PRIORITY-ID" 
     "STATUS-ID" "RESOLUTION-ID" "ISSUE-TYPE-ID" "PRIORITY-NAME" "STATUS-NAME" 
     "RESOLUTION-NAME" "ISSUE-TYPE-NAME" "PRIORITY-DESCRIPTION" 
     "STATUS-DESCRIPTION" "RESOLUTION-DESCRIPTION" "ISSUE-TYPE-DESCRIPTION" 
     "PRIORITY-ICON-URI" "STATUS-ICON-URI" "RESOLUTION-ICON-URI" 
     "ISSUE-TYPE-ICON-URI" "ISSUE-FIELD" "MAP-ISSUE-FIELDS" "COMPONENT"
     "COMPONENT-URI" "COMPONENT-ID" "COMPONENT-NAME" "COMPONENT-DESCRIPTION"
     "COMPONENT-ICON-URI" "ISSUE-TYPE" "STATUS" "PRIORITY" "RESOLUTION"
     "PROJECT" "PROJECT-URI" "PROJECT-NAME" "PROJECT-KEY" "PROJECT-ID"
     "PROJECT-DESCRIPTION" "PROJECT-LEAD" "PROJECT-ISSUE-TYPES" "PROJECT-AVATARS"
     "PROJECT-COMPONENTS" "SUBSET" "SUBSET-OFFSET" "SUBSET-LIMIT" 
     "SUBSET-TOTAL" "SUBSET-ELEMENTS" "COMMENT" "COMMENT-ID" "COMMENT-BODY" 
     "COMMENT-TEXT" "COMMENT-AUTHOR" "COMMENT-EDITED" "COMMENT-EDITOR" 
     "COMMENT-CREATED" "COMMENT-URI" "MAP-CONTAINER-ELEMENTS" "ISSUE-LINK-TYPE" 
     "ISSUE-LINK-TYPE-ID" "ISSUE-LINK-TYPE-NAME" "ISSUE-LINK-TYPE-INWARDS"
     "ISSUE-LINK-TYPE-OUTWARDS" "ISSUE-LINK-TYPE-URI" "ISSUE-LINK" 
     "ISSUE-LINK-ID" "ISSUE-LINK-URI" "ISSUE-LINK-INWARD-ISSUE" 
     "ISSUE-LINK-OUTWARD-ISSUE" "SCHEMA-FIELD" "SCHEMA-FIELD-ID" 
     "SCHEMA-FIELD-NAME" "SCHEMA-FIELD-TYPE" "SCHEMA-FIELD-CUSTOM-ID"
     "SCHEMA-FIELD-PLUGIN" "SCHEMA-FIELD-PARSER" "SCHEMA-FIELD-NAVIGABLE-P" 
     "SCHEMA-FIELD-CUSTOM-P" "SCHEMA-FIELD-SEARCHABLE-P" "SCHEMA-FIELD-ORDERABLE-P" 
     "WORKLOG" "WORKLOG-ID" "WORKLOG-COMMENT" "WORKLOG-STARTED"
     "WORKLOG-DURATION" "WORKLOG-AUTHOR" "WORKLOG-EDITOR" "WORKLOG-CREATED"
     "WORKLOG-EDITED" "WORKLOG-URI" "ATTACHMENT" "ATTACHMENT-URI" 
     "ATTACHMENT-FILENAME" "ATTACHMENT-SIZE" "ATTACHMENT-MIME-TYPE" 
     "ATTACHMENT-CONTENT-URI" "ATTACHMENT-THUMBNAIL-URI" "ATTACHMENT-AUTHOR" 
     "ATTACHMENT-CREATED" "ISSUE-FILTER" "ISSUE-FILTER-URI" "ISSUE-FILTER-ID" 
     "ISSUE-FILTER-OWNER" "ISSUE-FILTER-NAME" "ISSUE-FILTER-DESCRIPTION" 
     "ISSUE-FILTER-QUERY" "ISSUE-FILTER-FAVOURITE-P"

     "VALUE-TYPE" "VALUE-TYPE-LISP-TYPE" "VALUE-TYPE-DISPLAY-NAME"
     "VALUE-TYPE-IDENTIFIER" "PARSE-JSON-VALUE*" "PARSE-JSON-VALUE"
     "VALUE-TYPE-DOCUMENTATION"
     "REGISTER-SESSION-OBJECT" "MAKE-ARRAY-VALUE-TYPE" "MAKE-SUBSET-VALUE-TYPE"

     "MAPPED-FIELD" "MAPPED-FIELD-KEY" "MAPPED-FIELD-DEFINITION"
     "MAPPED-FIELD-VALUE-TYPE" "MAPPED-FIELD-SEARCHABLE-P" "MAPPED-FIELD-ORDERABLE-P"
     "MAPPED-FIELD-NAVIGABLE-P" "MAPPED-FIELD-NAME" "MAPPED-FIELD-ID"
     "MAPPED-FIELD-CUSTOM-ID" "MAKE-CUSTOM-FIELD" "MAKE-MAPPED-FIELD"

     "+STRING+" "+INTEGER+" "+NUMBER+" "+BOOLEAN+" "+URI+" "+TIMESTAMP+" 
     "+DATE+" "+AVATAR+" "+USER+" "+RESOLUTION+" "+STATUS+" "+PRIORITY+" 
     "+RESOLUTION+" "+COMPONENT+" "+ISSUE-LINK-TYPE+" "+COMMENT+" 
     "+PROJECT+" "+JSON+" "+WORKLOG+" "+ATTACHMENT+" "+ISSUE-TYPE+"
     "+ISSUE-FILTER+"
     
     "FIND-ISSUE" "SEARCH-ISSUES" "FIND-PROJECT" "FIND-USER" "LIST-STATES"
     "LIST-PRIORITIES" "LIST-RESOLUTIONS" "LIST-ISSUE-TYPES" "LIST-PROJECTS"
     "LIST-ISSUE-FILTERS" "FIND-ISSUE-FILTER" "ADD-COMMENT" "UPDATE-COMMENT"
     "DELETE-COMMENT" "FIND-COMMENT" "LIST-COMMENTS" "QUERY-COMMAND"
     "FIND-WORKLOG" "LIST-WORKLOGS" "ADD-WORKLOG"))
