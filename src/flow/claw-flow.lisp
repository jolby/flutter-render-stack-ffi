;;; CLAW wrapper definition for Flow
;;; Compositor, layer tree, damage tracking, raster cache

(claw:defwrapper (:flutter-render-stack-ffi/flow
                  (:system :flutter-render-stack-ffi/gen-flow)
                  (:headers "flow.h")
                  (:includes :flow-includes)
                  (:include-definitions "^Flow\\w+" "^FLOW_\\w+")
                  (:targets ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
                            ((:and :x86-64 :windows) "x86_64-w64-mingw32")
                            ((:and :x86-64 :darwin) "x86_64-apple-darwin-gnu")
                            ((:and :aarch64 :android) "aarch64-linux-android"))
                  (:persistent t
                   :depends-on (:claw-utils)
                   :asd-path #.(merge-pathnames "flutter-render-stack-ffi-flow-bindings.asd" (asdf:component-pathname (asdf:find-system '#:flutter-render-stack-ffi)))
                   :bindings-path #.(merge-pathnames "flow-bindings/" (asdf:component-pathname (asdf:find-system '#:flutter-render-stack-ffi)))))
  :in-package :%flow
  :trim-enum-prefix t
  :recognize-bitfields t
  :recognize-strings t
  :override-types ((:string claw-utils:claw-string)
                   (:pointer claw-utils:claw-pointer))
  :symbolicate-names (:in-pipeline
                      (:by-removing-prefixes "Flow" "FLOW_")))
