# flutter-render-stack

Common Lisp bindings to Flutter's rendering stack, enabling non-Dart UI frameworks to leverage Flutter's GPU-accelerated rendering and compositing.

## Components

This package provides bindings to two Flutter modules:

### Impeller (`%impeller` package)

GPU rendering engine providing:
- Display lists (recording and playback of drawing commands)
- Paint operations (colors, gradients, shaders)
- Path operations (lines, curves, shapes)
- Text rendering (typography, paragraphs)
- Texture management
- Vulkan/Metal/OpenGL backend abstraction

### Flow (`%flow` package)

Compositor and layer tree providing:
- Layer tree construction (hierarchical scene graph)
- Layer types: Container, Transform, Clip, Opacity, Filter, DisplayList
- **Damage tracking** (GPU-side partial repaint)
- **Raster cache** (automatic GPU texture caching for expensive subtrees)
- Frame lifecycle management

## Why This Matters

Traditional immediate-mode rendering rebuilds the entire scene every frame. Flutter's retained-mode approach:

1. **Reuses unchanged subtrees** - Skip rebuilding clean branches
2. **GPU-side damage tracking** - Only repaint changed pixels
3. **Automatic caching** - Expensive subtrees become GPU textures

This dramatically reduces CPU/GPU work for complex UIs.

## Directory Structure

```
flutter-render-stack/
├── flutter-render-stack.asd                    # Main system definition
├── flutter-render-stack-impeller-bindings.asd  # Symlink to generated
├── flutter-render-stack-flow-bindings.asd      # Symlink to generated
├── lib/                                        # Native libraries (runtime)
│   ├── libimpeller.so
│   └── libflow.so
├── src/
│   ├── impeller/               # Impeller wrapper system
│   │   ├── claw-impeller.lisp  # CLAW config
│   │   ├── lib/impeller.h      # C API header
│   │   └── bindings/           # Generated bindings
│   └── flow/                   # Flow wrapper system
│       ├── claw-flow.lisp      # CLAW config
│       ├── lib/flow.h          # C API header
│       └── bindings/           # Generated bindings
└── example/
```

## Generating Bindings

Bindings must be generated separately for each component:

```lisp
;; Generate Impeller bindings (writes to src/impeller/bindings/)
(ql:quickload :flutter-render-stack/impeller-wrapper)
(claw:load-wrapper :flutter-render-stack/impeller)

;; Generate Flow bindings (writes to src/flow/bindings/)
(ql:quickload :flutter-render-stack/flow-wrapper)
(claw:load-wrapper :flutter-render-stack/flow)
```

## Usage

After generating bindings:

```lisp
;; Load both Impeller and Flow bindings
(ql:quickload :flutter-render-stack)

;; Create a display list with Impeller
(let ((dl-builder (%impeller:display-list-builder-new nil)))
  (unwind-protect
       (progn
         ;; Draw operations...
         (%impeller:display-list-builder-draw-rect dl-builder rect paint)
         (let ((dl (%impeller:display-list-builder-build dl-builder)))

           ;; Wrap in Flow layer tree
           (let* ((offset (cffi:foreign-alloc '(:struct %flow:point)))
                  (layer (%flow:display-list-layer-new offset dl)))
             ;; Add to layer tree for compositing...
             )))
    (%impeller:display-list-builder-release dl-builder)))
```

## Requirements

- SBCL or other Common Lisp with CFFI support
- CLAW (for binding generation)
- `libimpeller.so` and `libflow.so` (from Flutter engine build)

## Building the Native Libraries

The native libraries must be built from the Flutter engine. See the parent project's documentation for build instructions.

## Version

This package tracks Flutter engine version via tags:
- `flow-interop-v0.2.0` - Phase 1b complete (all layer types, damage tracking, raster cache)

## License

BSD-3-Clause (matching Flutter engine license)
