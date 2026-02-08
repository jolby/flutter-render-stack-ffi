;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; flutter-render-stack-ffi.asd - Thin Flutter rendering stack bindings for Common Lisp
;;;
;;; Provides Lisp bindings to Flutter's rendering stack:
;;; - Impeller: GPU rendering, display lists, paint, paths, textures
;;; - Flow: Compositor, layer tree, damage tracking, raster cache
;;;
;;; These bindings enable non-Dart UI frameworks (like McCLIM) to leverage
;;; Flutter's retained-mode rendering and GPU-accelerated compositing.

(asdf:defsystem :flutter-render-stack-ffi
  :description "Common Lisp bindings to Flutter's rendering stack (Impeller + Flow)"
  :version "0.2.0"
  :author "Joel Boehland"
  :license "BSD-3-Clause"
  :depends-on (:flutter-render-stack-ffi-impeller-bindings
               :flutter-render-stack-ffi-flow-bindings))


;;;-----------------------------------------------------------------------------
;;; Impeller wrapper (generates bindings to src/impeller/bindings/)
;;;-----------------------------------------------------------------------------

(asdf:defsystem :flutter-render-stack-ffi/gen-impeller
  :description "CLAW wrapper generator for Impeller bindings"
  :version "0.2.0"
  :author "Joel Boehland"
  :license "BSD-3-Clause"
  :depends-on (:alexandria :claw-utils :claw)
  :pathname "src/impeller/"
  :serial t
  :components ((:file "claw-impeller")
               (:module :impeller-includes :pathname "lib")))


;;;-----------------------------------------------------------------------------
;;; Flow wrapper (generates bindings to src/flow/bindings/)
;;;-----------------------------------------------------------------------------

(asdf:defsystem :flutter-render-stack-ffi/gen-flow
  :description "CLAW wrapper generator for Flow bindings"
  :version "0.2.0"
  :author "Joel Boehland"
  :license "BSD-3-Clause"
  :depends-on (:alexandria :claw-utils :claw)
  :pathname "src/flow/"
  :serial t
  :components ((:file "claw-flow")
               (:module :flow-includes :pathname "lib")))


;;;-----------------------------------------------------------------------------
;;; Example
;;;-----------------------------------------------------------------------------

(asdf:defsystem :flutter-render-stack-ffi/example
  :description "Example usage of flutter-render-stack bindings"
  :version "0.2.0"
  :author "Joel Boehland"
  :license "BSD-3-Clause"
  :depends-on (:alexandria
               :flutter-render-stack
               :cffi-c-ref
               :static-vectors
               :trivial-main-thread)
  :pathname "example/"
  :components ((:file "example")))

;;;-----------------------------------------------------------------------------
;;; Generating/Regenerating the Bindings
;;;-----------------------------------------------------------------------------

;; Delete the generated adapters and run:
;;
;; (pushnew :claw-regen-adapter *features*)
;; (cffi:load-foreign-library (merge-pathnames ".local/lib/libresect.so" (user-homedir-pathname)))
;; (ql:quickload :flutter-render-stack-ffi/gen-impeller :force t)
;; (ql:quickload :flutter-render-stack-ffi/gen-flow :force t)
;; (claw:load-wrapper :flutter-render-stack-ffi/impeller)
;; (claw:load-wrapper :flutter-render-stack-ffi/flow)
