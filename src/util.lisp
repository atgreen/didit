;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DIDIT-SERVER; Base: 10 -*-
;;;
;;; Copyright (C) 2021  Anthony Green <green@moxielogic.com>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(defpackage #:didit.util
  (:use #:cl)
  (:shadow #:package)
  (:export #:valid-url?
	   #:make-absolute-pathname))

(in-package #:didit.util)

(defparameter +root-path+ (asdf:component-pathname (asdf:find-system "didit")))

(defun make-absolute-pathname (pathname)
  "Return an absolute pathname.  If PATHNAME is relative, make it
   relative to the rlgl-server system (provided by asdf)."
  (if (cl-fad:pathname-absolute-p pathname)
      pathname
      (merge-pathnames +root-path+ pathname)))

(defun valid-url? (string)
  "Returns T if STRING is a valid http or https url."
  (and string
       (quri:uri-http-p (quri:uri string))))
