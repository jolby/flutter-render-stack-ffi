;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; example.lisp - Example usage of flutter-render-stack bindings
;;;
;;; Demonstrates creating a layer tree with display lists and compositing.

(defpackage :flutter-render-stack.example
  (:use :cl)
  (:export #:run-example))

(in-package :flutter-render-stack.example)

;;; TODO: Add example demonstrating:
;;; 1. Creating a display list with Impeller
;;; 2. Wrapping it in a Flow layer tree
;;; 3. Using damage tracking for efficient repaints
;;; 4. Leveraging raster cache for expensive subtrees

(defun run-example ()
  "Run the flutter-render-stack example."
  (format t "~&Flutter Render Stack Example~%")
  (format t "~&Impeller version: ~A~%" (%impeller:get-version))
  (format t "~&Flow version: ~A~%" (%flow:get-version))
  ;; TODO: Actual rendering example
  )
