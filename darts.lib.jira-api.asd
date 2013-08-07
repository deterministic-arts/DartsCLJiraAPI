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

(asdf:defsystem :darts.lib.jira-api
  :name "darts.lib.jira-api"
  :author "Dirk Esser"
  :version "0.1"
  :maintainer "Dirk Eßer"
  :licence "MIT"
  :description "API to access a JIRA instance via its REST API"
  :long-description ""
  :depends-on ("drakma" "darts.lib.trivia" "cl-base64" "puri" "trivial-utf-8"
               "local-time" "darts.lib.hashtree" "cl-ppcre" "string-case")
  :serial t
  :components
  ((:file "package")
   (:file "conditions" :depends-on ("package"))
   (:file "session" :depends-on ("conditions"))
   (:file "resources" :depends-on ("session"))
   (:file "parser" :depends-on ("resources"))))