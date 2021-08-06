;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DIDIT; Base: 10 -*-

;;; Copyright (C) 2021  Anthony Green <green@moxielogic.com>

;;; Didit is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your
;;; option) any later version.
;;;
;;; Didit is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with didit; see the file COPYING3.  If not see
;;; <http://www.gnu.org/licenses/>.

;;;; package.lisp

(defpackage #:didit
  (:use #:hunchentoot #:cl)
  (:shadow #:package)
  (:export #:start-didit #:stop-didit #:check-didit #:alert/slack))

(in-package #:didit)
