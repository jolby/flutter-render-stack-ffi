;;; CLAW wrapper definition for Impeller
;;; GPU rendering, display lists, paint, paths, textures

(claw:defwrapper (:flutter-render-stack-ffi/impeller
                  (:system :flutter-render-stack-ffi/gen-impeller)
                  (:headers "impeller.h")
                  (:includes :impeller-includes)
                  (:include-definitions "^Impeller\\w+" "^IMPELLER_\\w+")
                  (:targets ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
                            ((:and :x86-64 :windows) "x86_64-w64-mingw32")
                            ((:and :x86-64 :darwin) "x86_64-apple-darwin-gnu")
                            ((:and :aarch64 :android) "aarch64-linux-android"))
                  (:persistent t
                   :depends-on (:claw-utils)
                   :asd-path #.(merge-pathnames "flutter-render-stack-impeller-bindings.asd" (asdf:component-pathname (asdf:find-system '#:flutter-render-stack-ffi)))
                   :bindings-path #.(merge-pathnames "src/impeller/bindings/" (asdf:component-pathname (asdf:find-system '#:flutter-render-stack-ffi)))))
  :in-package :%impeller
  :trim-enum-prefix t
  :recognize-bitfields t
  :recognize-strings t
  :override-types ((:string claw-utils:claw-string)
                   (:pointer claw-utils:claw-pointer))
  :symbolicate-names (:in-pipeline
                      (:by-removing-prefixes "Impeller" "IMPELLER_")))
