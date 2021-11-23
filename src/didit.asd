;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DIDIT; Base: 10 -*-

#|

Copyright (C) 2021  Anthony Green <green@moxielogic.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with this program.  If not, see
<http://www.gnu.org/licenses/>.

|#

(asdf:defsystem #:didit
  :description "A 'did-it' tracker"
  :author "Anthony Green <green@moxielogic.com>"
  :version "0"
  :serial t
  :components ((:file "package")
               (:file "alert")
	       (:file "didit")
               (:file "load-config")
               (:file "pull-repo")
               (:file "server"))
  :depends-on (:async-process
               :bt-semaphore
               :cl-etcd
               :cl-fad
               :cl-json
               :cl-ppcre
               :cl-toml
               :drakma
               :flexi-streams
               :easy-routes
               :hunchentoot
               :inferior-shell
               :ironclad
               :log4cl
               :markup
               :prometheus
               :prometheus.collectors.process
               :prometheus.collectors.sbcl
               :prometheus.exposers.hunchentoot
               :prometheus.formats.text
               :scheduler
               :str))
