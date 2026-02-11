// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef FLUTTER_FLOW_INTEROP_FLOW_H_
#define FLUTTER_FLOW_INTEROP_FLOW_H_

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

///-----------------------------------------------------------------------------
///        _____ _                    _    ____ ___
///       |  ___| | _____      __    / \  |  _ \_ _|
///       | |_  | |/ _ \ \ /\ / /   / _ \ | |_) | |
///       |  _| | | (_) \ V  V /   / ___ \|  __/| |
///       |_|   |_|\___/ \_/\_/   /_/   \_\_|  |___|
///
///-----------------------------------------------------------------------------
///
/// This file describes a C API for Flutter's flow module (compositor/layer
/// tree). It enables non-Dart UI frameworks to leverage Flutter's retained
/// rendering and damage tracking capabilities.
///
/// The API follows the same patterns as libimpeller for consistency.
///

#if defined(__cplusplus)
#define FLOW_EXTERN_C extern "C"
#define FLOW_EXTERN_C_BEGIN FLOW_EXTERN_C {
#define FLOW_EXTERN_C_END }
#else
#define FLOW_EXTERN_C
#define FLOW_EXTERN_C_BEGIN
#define FLOW_EXTERN_C_END
#endif

#ifdef _WIN32
#define FLOW_EXPORT_DECORATION __declspec(dllexport)
#else
#define FLOW_EXPORT_DECORATION __attribute__((visibility("default")))
#endif

#ifndef FLOW_NO_EXPORT
#define FLOW_EXPORT FLOW_EXPORT_DECORATION
#else
#define FLOW_EXPORT
#endif

#ifdef __clang__
#define FLOW_NULLABLE _Nullable
#define FLOW_NONNULL _Nonnull
#else
#define FLOW_NULLABLE
#define FLOW_NONNULL
#endif

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 202000L)
#define FLOW_NODISCARD [[nodiscard]]
#else
#define FLOW_NODISCARD
#endif

FLOW_EXTERN_C_BEGIN

//------------------------------------------------------------------------------
// Version
//------------------------------------------------------------------------------

#define FLOW_MAKE_VERSION(variant, major, minor, patch)            \
  ((((uint32_t)(variant)) << 29U) | (((uint32_t)(major)) << 22U) | \
   (((uint32_t)(minor)) << 12U) | ((uint32_t)(patch)))

#define FLOW_VERSION_VARIANT 1
#define FLOW_VERSION_MAJOR 0
#define FLOW_VERSION_MINOR 1
#define FLOW_VERSION_PATCH 0

#define FLOW_VERSION                                          \
  FLOW_MAKE_VERSION(FLOW_VERSION_VARIANT, FLOW_VERSION_MAJOR, \
                    FLOW_VERSION_MINOR, FLOW_VERSION_PATCH)

#define FLOW_VERSION_GET_VARIANT(version) ((uint32_t)(version) >> 29U)
#define FLOW_VERSION_GET_MAJOR(version) (((uint32_t)(version) >> 22U) & 0x7FU)
#define FLOW_VERSION_GET_MINOR(version) (((uint32_t)(version) >> 12U) & 0x3FFU)
#define FLOW_VERSION_GET_PATCH(version) ((uint32_t)(version) & 0xFFFU)

//------------------------------------------------------------------------------
/// @brief      Get the version of the Flow interop API.
///
/// @return     The version of the API.
///
FLOW_EXPORT
uint32_t FlowGetVersion(void);

//------------------------------------------------------------------------------
// Handle Definitions
//------------------------------------------------------------------------------

#define FLOW_INTERNAL_HANDLE_NAME(handle) handle##_
#define FLOW_DEFINE_HANDLE(handle) \
  typedef struct FLOW_INTERNAL_HANDLE_NAME(handle) * handle;

//------------------------------------------------------------------------------
/// A layer tree represents the complete scene graph for a frame.
///
/// Layer trees are mutable during construction, then become effectively
/// immutable once passed to the compositor for rasterization.
///
FLOW_DEFINE_HANDLE(FlowLayerTree);

//------------------------------------------------------------------------------
/// A container layer that can hold child layers.
///
/// This is the base for building hierarchical scene graphs.
///
FLOW_DEFINE_HANDLE(FlowContainerLayer);

//------------------------------------------------------------------------------
/// A layer that renders a display list.
///
/// Display lists contain the actual drawing commands.
///
FLOW_DEFINE_HANDLE(FlowDisplayListLayer);

//------------------------------------------------------------------------------
/// A layer that applies a transformation to its children.
///
FLOW_DEFINE_HANDLE(FlowTransformLayer);

//------------------------------------------------------------------------------
/// A layer that clips its children to a rectangle.
///
FLOW_DEFINE_HANDLE(FlowClipRectLayer);

//------------------------------------------------------------------------------
/// A layer that clips its children to a rounded rectangle.
///
FLOW_DEFINE_HANDLE(FlowClipRRectLayer);

//------------------------------------------------------------------------------
/// A layer that clips its children to an arbitrary path.
///
FLOW_DEFINE_HANDLE(FlowClipPathLayer);

//------------------------------------------------------------------------------
/// A layer that applies opacity (alpha blending) to its children.
///
FLOW_DEFINE_HANDLE(FlowOpacityLayer);

//------------------------------------------------------------------------------
/// A layer that applies a color filter to its children.
///
FLOW_DEFINE_HANDLE(FlowColorFilterLayer);

//------------------------------------------------------------------------------
/// A layer that applies an image filter (e.g., blur) to its children.
///
FLOW_DEFINE_HANDLE(FlowImageFilterLayer);

//------------------------------------------------------------------------------
/// The compositor context manages state for rasterization.
///
/// This includes the raster cache and timing information.
///
FLOW_DEFINE_HANDLE(FlowCompositorContext);

//------------------------------------------------------------------------------
/// A scoped frame represents a single frame being rasterized.
///
/// Frames are created from a compositor context and manage the per-frame
/// rendering state.
///
FLOW_DEFINE_HANDLE(FlowScopedFrame);

//------------------------------------------------------------------------------
/// Tracks damage (changed regions) between frames for partial repaint.
///
/// Used to compute the minimal bounding region that needs repainting,
/// enabling differential rendering and improving performance.
///
FLOW_DEFINE_HANDLE(FlowFrameDamage);

//------------------------------------------------------------------------------
/// A raster cache for caching rasterized layers and display lists.
///
/// The cache improves performance by storing pre-rasterized content that
/// can be reused across frames. Owned by CompositorContext.
///
FLOW_DEFINE_HANDLE(FlowRasterCache);

//------------------------------------------------------------------------------
// Forward declarations for Impeller C API handle types.
// These match IMPELLER_DEFINE_HANDLE expansion exactly so that flow.h can be
// included without requiring impeller.h.
//------------------------------------------------------------------------------
#ifndef IMPELLER_DEFINE_HANDLE
typedef struct ImpellerContext_* ImpellerContext;
typedef struct ImpellerDisplayList_* ImpellerDisplayList;
#endif

//------------------------------------------------------------------------------
// Structures
//------------------------------------------------------------------------------

typedef struct FlowISize {
  int32_t width;
  int32_t height;
} FlowISize;

typedef struct FlowIRect {
  int32_t x;
  int32_t y;
  int32_t width;
  int32_t height;
} FlowIRect;

typedef struct FlowRect {
  float x;
  float y;
  float width;
  float height;
} FlowRect;

typedef struct FlowPoint {
  float x;
  float y;
} FlowPoint;

typedef struct FlowMatrix {
  float m[16];
} FlowMatrix;

//------------------------------------------------------------------------------
/// Metrics about raster cache usage for a frame.
///
/// These metrics provide insight into cache efficiency and memory usage.
///
typedef struct FlowRasterCacheMetrics {
  /// The number of cache entries evicted this frame.
  size_t eviction_count;
  /// The total bytes of evicted cache entries this frame.
  size_t eviction_bytes;
  /// The number of cache entries currently in use.
  size_t in_use_count;
  /// The total bytes of cache entries currently in use.
  size_t in_use_bytes;
} FlowRasterCacheMetrics;

//------------------------------------------------------------------------------
/// Enum for clip behavior (how clip paths are applied).
///
typedef enum {
  /// Clip with antialiasing (smooth edges).
  FlowClipBehavior_AntiAlias = 0,
  /// Clip without antialiasing (hard edges).
  FlowClipBehavior_HardEdge = 1,
  /// No clipping (draw outside bounds).
  FlowClipBehavior_None = 2,
} FlowClipBehavior;

//------------------------------------------------------------------------------
/// Enum for raster cache status.
///
typedef enum {
  /// Cache entry was successfully rasterized and stored.
  FlowRasterStatus_Success = 0,
  /// Cache entry was not found and needs rasterization.
  FlowRasterStatus_NotFound = 1,
  /// Rasterization failed or cache entry is invalid.
  FlowRasterStatus_Failed = 2,
} FlowRasterStatus;

//------------------------------------------------------------------------------
// Geometry Helper Functions
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
/// @brief      Compute the intersection of two rectangles.
///
/// @param[in]  rect1       The first rectangle.
/// @param[in]  rect2       The second rectangle.
/// @param[out] out_rect    The intersection rectangle (may be empty).
///
/// @return     true if rectangles intersect, false otherwise.
///
/// @example
/// @code
///   FlowRect r1 = {0, 0, 100, 100};
///   FlowRect r2 = {50, 50, 100, 100};
///   FlowRect result;
///   if (FlowRectIntersection(&r1, &r2, &result)) {
///     printf("Intersection: (%f, %f, %f, %f)\n",
///            result.x, result.y, result.width, result.height);
///   }
/// @endcode
///
FLOW_EXPORT
bool FlowRectIntersection(const FlowRect* FLOW_NONNULL rect1,
                          const FlowRect* FLOW_NONNULL rect2,
                          FlowRect* FLOW_NONNULL out_rect);

//------------------------------------------------------------------------------
/// @brief      Compute the union (bounding box) of two rectangles.
///
/// @param[in]  rect1       The first rectangle.
/// @param[in]  rect2       The second rectangle.
/// @param[out] out_rect    The union rectangle.
///
/// @example
/// @code
///   FlowRect r1 = {0, 0, 50, 50};
///   FlowRect r2 = {75, 75, 50, 50};
///   FlowRect result;
///   FlowRectUnion(&r1, &r2, &result);
///   printf("Union bounds: (%f, %f, %f, %f)\n",
///          result.x, result.y, result.width, result.height);
/// @endcode
///
FLOW_EXPORT
void FlowRectUnion(const FlowRect* FLOW_NONNULL rect1,
                   const FlowRect* FLOW_NONNULL rect2,
                   FlowRect* FLOW_NONNULL out_rect);

//------------------------------------------------------------------------------
/// @brief      Test if a point is contained within a rectangle.
///
/// @param[in]  rect        The rectangle.
/// @param[in]  point       The point.
///
/// @return     true if point is inside rectangle, false otherwise.
///
/// @example
/// @code
///   FlowRect bounds = {10, 10, 100, 100};
///   FlowPoint p1 = {50, 50};
///   FlowPoint p2 = {150, 150};
///   assert(FlowRectContainsPoint(&bounds, &p1));
///   assert(!FlowRectContainsPoint(&bounds, &p2));
/// @endcode
///
FLOW_EXPORT
bool FlowRectContainsPoint(const FlowRect* FLOW_NONNULL rect,
                           const FlowPoint* FLOW_NONNULL point);

//------------------------------------------------------------------------------
/// @brief      Transform a point by a 4x4 matrix.
///
/// Applies the transformation matrix to the point, returning the result.
/// This is useful for converting points between coordinate spaces in
/// transformed layer trees.
///
/// @param[in]  point       The input point.
/// @param[in]  matrix      The transformation matrix.
/// @param[out] out_point   The transformed point.
///
/// @example
/// @code
///   FlowPoint p = {10, 20};
///   FlowMatrix transform = {...};  // Identity, scale, rotation, etc.
///   FlowPoint transformed;
///   FlowPointTransform(&p, &transform, &transformed);
/// @endcode
///
FLOW_EXPORT
void FlowPointTransform(const FlowPoint* FLOW_NONNULL point,
                        const FlowMatrix* FLOW_NONNULL matrix,
                        FlowPoint* FLOW_NONNULL out_point);

//------------------------------------------------------------------------------
/// @brief      Compute the inverse of a 4x4 transformation matrix.
///
/// @param[in]  matrix          The input matrix.
/// @param[out] out_inverted    The inverted matrix.
///
/// @return     true if matrix is invertible, false if singular.
///
/// @example
/// @code
///   FlowMatrix transform = {...};
///   FlowMatrix inverse;
///   if (FlowMatrixInvert(&transform, &inverse)) {
///     // Use inverse to transform points back
///   }
/// @endcode
///
FLOW_EXPORT
bool FlowMatrixInvert(const FlowMatrix* FLOW_NONNULL matrix,
                      FlowMatrix* FLOW_NONNULL out_inverted);

//------------------------------------------------------------------------------
/// @brief      Multiply two 4x4 transformation matrices.
///
/// Computes `result = a * b` (matrix product, not element-wise).
/// Useful for composing transformations.
///
/// @param[in]  a           The left-hand matrix.
/// @param[in]  b           The right-hand matrix.
/// @param[out] out_result  The product matrix.
///
/// @example
/// @code
///   FlowMatrix scale = {...};
///   FlowMatrix translate = {...};
///   FlowMatrix combined;
///   FlowMatrixMultiply(&scale, &translate, &combined);
/// @endcode
///
FLOW_EXPORT
void FlowMatrixMultiply(const FlowMatrix* FLOW_NONNULL a,
                        const FlowMatrix* FLOW_NONNULL b,
                        FlowMatrix* FLOW_NONNULL out_result);

//------------------------------------------------------------------------------
// LayerTree API
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
/// @brief      Create a new layer tree with the given frame size.
///
/// @param[in]  version      The API version (use FLOW_VERSION).
/// @param[in]  frame_size   The size of the frame in physical pixels.
///
/// @return     The layer tree or NULL on failure.
///
FLOW_EXPORT FLOW_NODISCARD FlowLayerTree FLOW_NULLABLE
FlowLayerTreeNew(uint32_t version, const FlowISize* FLOW_NONNULL frame_size);

//------------------------------------------------------------------------------
/// @brief      Retain a reference to the layer tree.
///
/// @param[in]  tree  The layer tree.
///
FLOW_EXPORT
void FlowLayerTreeRetain(FlowLayerTree FLOW_NULLABLE tree);

//------------------------------------------------------------------------------
/// @brief      Release a reference to the layer tree.
///
/// @param[in]  tree  The layer tree.
///
FLOW_EXPORT
void FlowLayerTreeRelease(FlowLayerTree FLOW_NULLABLE tree);

//------------------------------------------------------------------------------
/// @brief      Set the root layer of the layer tree.
///
/// @param[in]  tree   The layer tree.
/// @param[in]  layer  The root container layer.
///
FLOW_EXPORT
void FlowLayerTreeSetRootLayer(FlowLayerTree FLOW_NONNULL tree,
                               FlowContainerLayer FLOW_NONNULL layer);

//------------------------------------------------------------------------------
/// @brief      Get the frame size of the layer tree.
///
/// @param[in]  tree       The layer tree.
/// @param[out] out_size   The frame size.
///
FLOW_EXPORT
void FlowLayerTreeGetFrameSize(FlowLayerTree FLOW_NONNULL tree,
                               FlowISize* FLOW_NONNULL out_size);

//------------------------------------------------------------------------------
// ContainerLayer API
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
/// @brief      Retain a reference to the container layer.
///
/// @param[in]  layer  The container layer.
///
FLOW_EXPORT
void FlowContainerLayerRetain(FlowContainerLayer FLOW_NULLABLE layer);

//------------------------------------------------------------------------------
/// @brief      Release a reference to the container layer.
///
/// @param[in]  layer  The container layer.
///
FLOW_EXPORT
void FlowContainerLayerRelease(FlowContainerLayer FLOW_NULLABLE layer);

//------------------------------------------------------------------------------
// TransformLayer API
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
/// @brief      Create a new transform layer with the given matrix.
///
/// @param[in]  matrix  The 4x4 transformation matrix.
///
/// @return     The transform layer or NULL on failure.
///
FLOW_EXPORT FLOW_NODISCARD FlowTransformLayer FLOW_NULLABLE
FlowTransformLayerNew(const FlowMatrix* FLOW_NONNULL matrix);

//------------------------------------------------------------------------------
/// @brief      Retain a reference to the transform layer.
///
/// @param[in]  layer  The transform layer.
///
FLOW_EXPORT
void FlowTransformLayerRetain(FlowTransformLayer FLOW_NULLABLE layer);

//------------------------------------------------------------------------------
/// @brief      Release a reference to the transform layer.
///
/// @param[in]  layer  The transform layer.
///
FLOW_EXPORT
void FlowTransformLayerRelease(FlowTransformLayer FLOW_NULLABLE layer);

//------------------------------------------------------------------------------
/// @brief      Add a child layer to a transform layer.
///
/// @param[in]  parent  The parent transform layer.
/// @param[in]  child   The child layer (any layer type cast to container).
///
FLOW_EXPORT
void FlowTransformLayerAddChild(FlowTransformLayer FLOW_NONNULL parent,
                                FlowContainerLayer FLOW_NONNULL child);

//------------------------------------------------------------------------------
/// @brief      Get the transform layer as a container layer handle.
///
/// @param[in]  layer  The transform layer.
///
/// @return     The container layer handle (no ref count change).
///
FLOW_EXPORT FlowContainerLayer FLOW_NONNULL
FlowTransformLayerAsContainer(FlowTransformLayer FLOW_NONNULL layer);

//------------------------------------------------------------------------------
// DisplayListLayer API
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
/// @brief      Create a new display list layer.
///
///             Note: Requires ImpellerDisplayList from libimpeller.
///
/// @param[in]  offset        The offset for the display list.
/// @param[in]  display_list  The impeller display list handle (void* for now).
///
/// @return     The display list layer or NULL on failure.
///
FLOW_EXPORT FLOW_NODISCARD FlowDisplayListLayer FLOW_NULLABLE
FlowDisplayListLayerNew(const FlowPoint* FLOW_NONNULL offset,
                        void* FLOW_NONNULL display_list);

//------------------------------------------------------------------------------
/// @brief      Retain a reference to the display list layer.
///
/// @param[in]  layer  The display list layer.
///
FLOW_EXPORT
void FlowDisplayListLayerRetain(FlowDisplayListLayer FLOW_NULLABLE layer);

//------------------------------------------------------------------------------
/// @brief      Release a reference to the display list layer.
///
/// @param[in]  layer  The display list layer.
///
FLOW_EXPORT
void FlowDisplayListLayerRelease(FlowDisplayListLayer FLOW_NULLABLE layer);

//------------------------------------------------------------------------------
/// @brief      Get the display list layer as a container layer handle.
///
/// @param[in]  layer  The display list layer.
///
/// @return     The container layer handle (no ref count change).
///
FLOW_EXPORT FlowContainerLayer FLOW_NONNULL
FlowDisplayListLayerAsContainer(FlowDisplayListLayer FLOW_NONNULL layer);

//------------------------------------------------------------------------------
// ClipRectLayer API
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
/// @brief      Create a new clip rect layer.
///
/// @param[in]  clip_rect      The rectangle to clip to.
/// @param[in]  clip_behavior  How to apply the clip (antialiased, hard edge,
/// etc).
///
/// @return     The clip rect layer or NULL on failure.
///
FLOW_EXPORT FLOW_NODISCARD FlowClipRectLayer FLOW_NULLABLE
FlowClipRectLayerNew(const FlowRect* FLOW_NONNULL clip_rect,
                     FlowClipBehavior clip_behavior);

FLOW_EXPORT
void FlowClipRectLayerRetain(FlowClipRectLayer FLOW_NULLABLE layer);

FLOW_EXPORT
void FlowClipRectLayerRelease(FlowClipRectLayer FLOW_NULLABLE layer);

FLOW_EXPORT
void FlowClipRectLayerAddChild(FlowClipRectLayer FLOW_NONNULL parent,
                               FlowContainerLayer FLOW_NONNULL child);

FLOW_EXPORT FlowContainerLayer FLOW_NONNULL
FlowClipRectLayerAsContainer(FlowClipRectLayer FLOW_NONNULL layer);

//------------------------------------------------------------------------------
// ClipRRectLayer API
//------------------------------------------------------------------------------

FLOW_EXPORT FLOW_NODISCARD FlowClipRRectLayer FLOW_NULLABLE
FlowClipRRectLayerNew(const FlowRect* FLOW_NONNULL bounds,
                      float corner_radius,
                      FlowClipBehavior clip_behavior);

FLOW_EXPORT
void FlowClipRRectLayerRetain(FlowClipRRectLayer FLOW_NULLABLE layer);

FLOW_EXPORT
void FlowClipRRectLayerRelease(FlowClipRRectLayer FLOW_NULLABLE layer);

FLOW_EXPORT
void FlowClipRRectLayerAddChild(FlowClipRRectLayer FLOW_NONNULL parent,
                                FlowContainerLayer FLOW_NONNULL child);

FLOW_EXPORT FlowContainerLayer FLOW_NONNULL
FlowClipRRectLayerAsContainer(FlowClipRRectLayer FLOW_NONNULL layer);

//------------------------------------------------------------------------------
// ClipPathLayer API
//------------------------------------------------------------------------------

FLOW_EXPORT FLOW_NODISCARD FlowClipPathLayer FLOW_NULLABLE
FlowClipPathLayerNew(FlowClipBehavior clip_behavior);

FLOW_EXPORT
void FlowClipPathLayerRetain(FlowClipPathLayer FLOW_NULLABLE layer);

FLOW_EXPORT
void FlowClipPathLayerRelease(FlowClipPathLayer FLOW_NULLABLE layer);

FLOW_EXPORT
void FlowClipPathLayerAddChild(FlowClipPathLayer FLOW_NONNULL parent,
                               FlowContainerLayer FLOW_NONNULL child);

FLOW_EXPORT FlowContainerLayer FLOW_NONNULL
FlowClipPathLayerAsContainer(FlowClipPathLayer FLOW_NONNULL layer);

//------------------------------------------------------------------------------
// OpacityLayer API
//------------------------------------------------------------------------------

FLOW_EXPORT FLOW_NODISCARD FlowOpacityLayer FLOW_NULLABLE
FlowOpacityLayerNew(int alpha, const FlowPoint* FLOW_NONNULL offset);

FLOW_EXPORT
void FlowOpacityLayerRetain(FlowOpacityLayer FLOW_NULLABLE layer);

FLOW_EXPORT
void FlowOpacityLayerRelease(FlowOpacityLayer FLOW_NULLABLE layer);

FLOW_EXPORT
void FlowOpacityLayerAddChild(FlowOpacityLayer FLOW_NONNULL parent,
                              FlowContainerLayer FLOW_NONNULL child);

FLOW_EXPORT FlowContainerLayer FLOW_NONNULL
FlowOpacityLayerAsContainer(FlowOpacityLayer FLOW_NONNULL layer);

//------------------------------------------------------------------------------
// ColorFilterLayer API
//------------------------------------------------------------------------------

FLOW_EXPORT FLOW_NODISCARD FlowColorFilterLayer FLOW_NULLABLE
FlowColorFilterLayerNew(void* FLOW_NONNULL color_filter);

FLOW_EXPORT
void FlowColorFilterLayerRetain(FlowColorFilterLayer FLOW_NULLABLE layer);

FLOW_EXPORT
void FlowColorFilterLayerRelease(FlowColorFilterLayer FLOW_NULLABLE layer);

FLOW_EXPORT
void FlowColorFilterLayerAddChild(FlowColorFilterLayer FLOW_NONNULL parent,
                                  FlowContainerLayer FLOW_NONNULL child);

FLOW_EXPORT FlowContainerLayer FLOW_NONNULL
FlowColorFilterLayerAsContainer(FlowColorFilterLayer FLOW_NONNULL layer);

//------------------------------------------------------------------------------
// ImageFilterLayer API
//------------------------------------------------------------------------------

FLOW_EXPORT FLOW_NODISCARD FlowImageFilterLayer FLOW_NULLABLE
FlowImageFilterLayerNew(void* FLOW_NONNULL image_filter);

FLOW_EXPORT
void FlowImageFilterLayerRetain(FlowImageFilterLayer FLOW_NULLABLE layer);

FLOW_EXPORT
void FlowImageFilterLayerRelease(FlowImageFilterLayer FLOW_NULLABLE layer);

FLOW_EXPORT
void FlowImageFilterLayerAddChild(FlowImageFilterLayer FLOW_NONNULL parent,
                                  FlowContainerLayer FLOW_NONNULL child);

FLOW_EXPORT FlowContainerLayer FLOW_NONNULL
FlowImageFilterLayerAsContainer(FlowImageFilterLayer FLOW_NONNULL layer);

//------------------------------------------------------------------------------
// CompositorContext API
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
/// @brief      Create a new compositor context.
///
/// @param[in]  version  The API version (use FLOW_VERSION).
///
/// @return     The compositor context or NULL on failure.
///
FLOW_EXPORT FLOW_NODISCARD FlowCompositorContext FLOW_NULLABLE
FlowCompositorContextNew(uint32_t version);

//------------------------------------------------------------------------------
/// @brief      Retain a reference to the compositor context.
///
/// @param[in]  context  The compositor context.
///
FLOW_EXPORT
void FlowCompositorContextRetain(FlowCompositorContext FLOW_NULLABLE context);

//------------------------------------------------------------------------------
/// @brief      Release a reference to the compositor context.
///
/// @param[in]  context  The compositor context.
///
FLOW_EXPORT
void FlowCompositorContextRelease(FlowCompositorContext FLOW_NULLABLE context);

//------------------------------------------------------------------------------
/// @brief      Get the raster cache from a compositor context.
///
///             The returned cache is owned by the compositor context. The
///             caller receives a new reference that must be released.
///
/// @param[in]  context  The compositor context.
///
/// @return     The raster cache, or NULL if not available.
///
FLOW_EXPORT FLOW_NODISCARD FlowRasterCache FLOW_NULLABLE
FlowCompositorContextGetRasterCache(FlowCompositorContext FLOW_NONNULL context);

//------------------------------------------------------------------------------
// ScopedFrame API
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
/// @brief      Acquire a scoped frame for rasterization from the compositor
///             context.
///
///             The frame creates an internal DisplayListBuilder as its canvas.
///             After rasterization, call FlowScopedFrameBuildDisplayListNew
///             to extract the recorded DisplayList for rendering to a surface.
///
/// @param[in]  context           The compositor context.
/// @param[in]  impeller_context  The Impeller context (provides rendering
///                               backend). May be NULL for CPU-only raster
///                               cache (limited functionality).
/// @param[in]  frame_size        The frame dimensions in physical pixels.
///
/// @return     A scoped frame handle, or NULL on failure.
///
/// @see        FlowScopedFrameRaster
/// @see        FlowScopedFrameBuildDisplayListNew
///
FLOW_EXPORT FLOW_NODISCARD FlowScopedFrame FLOW_NULLABLE
FlowCompositorContextAcquireFrame(FlowCompositorContext FLOW_NONNULL context,
                                  ImpellerContext FLOW_NULLABLE
                                      impeller_context,
                                  const FlowISize* FLOW_NONNULL frame_size);

//------------------------------------------------------------------------------
/// @brief      Rasterize a layer tree to the scoped frame.
///
///             Runs the full Flutter rasterization pipeline:
///             1. Preroll pass (compute bounds, prepare raster cache)
///             2. Paint pass (recursive layer painting to internal canvas)
///
///             After a successful raster, call
///             FlowScopedFrameBuildDisplayListNew to extract the recorded
///             DisplayList.
///
/// @param[in]  frame              The scoped frame.
/// @param[in]  layer_tree         The layer tree to rasterize.
/// @param[in]  frame_damage       Optional damage tracker for partial repaint.
///                                Pass NULL for full repaint.
/// @param[in]  ignore_raster_cache  If true, bypass the raster cache.
///
/// @return     FlowRasterStatus_Success on success.
///
FLOW_EXPORT
FlowRasterStatus FlowScopedFrameRaster(FlowScopedFrame FLOW_NONNULL frame,
                                       FlowLayerTree FLOW_NONNULL layer_tree,
                                       FlowFrameDamage FLOW_NULLABLE
                                           frame_damage,
                                       bool ignore_raster_cache);

//------------------------------------------------------------------------------
/// @brief      Build the immutable DisplayList from the frame's recorded
///             drawing commands.
///
///             Must be called after a successful FlowScopedFrameRaster().
///             Returns a new ImpellerDisplayList handle owned by the caller.
///             The caller should render it to a surface using
///             ImpellerSurfaceDrawDisplayList() and then release it.
///
///             Can only be called once per frame — subsequent calls return
///             NULL.
///
/// @param[in]  frame  The scoped frame (must have been rasterized).
///
/// @return     A new ImpellerDisplayList handle, or NULL on failure.
///             Caller owns the returned handle and must release it with
///             ImpellerDisplayListRelease().
///
FLOW_EXPORT FLOW_NODISCARD ImpellerDisplayList FLOW_NULLABLE
FlowScopedFrameBuildDisplayListNew(FlowScopedFrame FLOW_NONNULL frame);

//------------------------------------------------------------------------------
/// @brief      Retain a reference to a scoped frame.
///
/// @param[in]  frame  The scoped frame.
///
FLOW_EXPORT
void FlowScopedFrameRetain(FlowScopedFrame FLOW_NULLABLE frame);

//------------------------------------------------------------------------------
/// @brief      Release a reference to a scoped frame.
///
/// @param[in]  frame  The scoped frame.
///
FLOW_EXPORT
void FlowScopedFrameRelease(FlowScopedFrame FLOW_NULLABLE frame);

//------------------------------------------------------------------------------
// RasterCache API
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
/// @brief      Retain a reference to the raster cache.
///
/// @param[in]  cache  The raster cache.
///
FLOW_EXPORT
void FlowRasterCacheRetain(FlowRasterCache FLOW_NULLABLE cache);

//------------------------------------------------------------------------------
/// @brief      Release a reference to the raster cache.
///
/// @param[in]  cache  The raster cache.
///
FLOW_EXPORT
void FlowRasterCacheRelease(FlowRasterCache FLOW_NULLABLE cache);

//------------------------------------------------------------------------------
/// @brief      Clear all cached entries from the raster cache.
///
/// @param[in]  cache  The raster cache.
///
FLOW_EXPORT
void FlowRasterCacheClear(FlowRasterCache FLOW_NONNULL cache);

//------------------------------------------------------------------------------
/// @brief      Get the access threshold for the raster cache.
///
///             The access threshold is the number of frames a layer or picture
///             must be visible before it gets cached.
///
/// @param[in]  cache  The raster cache.
///
/// @return     The access threshold count.
///
FLOW_EXPORT
size_t FlowRasterCacheGetAccessThreshold(FlowRasterCache FLOW_NONNULL cache);

//------------------------------------------------------------------------------
/// @brief      Get the current picture (display list) cache metrics.
///
/// @param[in]  cache        The raster cache.
/// @param[out] out_metrics  The metrics structure to fill.
///
FLOW_EXPORT
void FlowRasterCacheGetPictureMetrics(FlowRasterCache FLOW_NONNULL cache,
                                      FlowRasterCacheMetrics* FLOW_NONNULL
                                          out_metrics);

//------------------------------------------------------------------------------
/// @brief      Get the current layer cache metrics.
///
/// @param[in]  cache        The raster cache.
/// @param[out] out_metrics  The metrics structure to fill.
///
FLOW_EXPORT
void FlowRasterCacheGetLayerMetrics(FlowRasterCache FLOW_NONNULL cache,
                                    FlowRasterCacheMetrics* FLOW_NONNULL
                                        out_metrics);

//------------------------------------------------------------------------------
/// @brief      Get the total number of cached entries (layers + pictures).
///
/// @param[in]  cache  The raster cache.
///
/// @return     The total count of cached entries.
///
FLOW_EXPORT
size_t FlowRasterCacheGetCachedEntriesCount(FlowRasterCache FLOW_NONNULL cache);

//------------------------------------------------------------------------------
/// @brief      Get the number of cached layer entries.
///
/// @param[in]  cache  The raster cache.
///
/// @return     The count of cached layer entries.
///
FLOW_EXPORT
size_t FlowRasterCacheGetLayerCachedEntriesCount(
    FlowRasterCache FLOW_NONNULL cache);

//------------------------------------------------------------------------------
/// @brief      Get the number of cached picture (display list) entries.
///
/// @param[in]  cache  The raster cache.
///
/// @return     The count of cached picture entries.
///
FLOW_EXPORT
size_t FlowRasterCacheGetPictureCachedEntriesCount(
    FlowRasterCache FLOW_NONNULL cache);

//------------------------------------------------------------------------------
/// @brief      Estimate the memory used by cached pictures in bytes.
///
/// @param[in]  cache  The raster cache.
///
/// @return     Estimated bytes used by picture cache.
///
FLOW_EXPORT
size_t FlowRasterCacheEstimatePictureByteSize(
    FlowRasterCache FLOW_NONNULL cache);

//------------------------------------------------------------------------------
/// @brief      Estimate the memory used by cached layers in bytes.
///
/// @param[in]  cache  The raster cache.
///
/// @return     Estimated bytes used by layer cache.
///
FLOW_EXPORT
size_t FlowRasterCacheEstimateLayerByteSize(FlowRasterCache FLOW_NONNULL cache);

//------------------------------------------------------------------------------
// FrameDamage API
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
/// @brief      Create a new frame damage tracker.
///
/// @param[in]  version      The API version (use FLOW_VERSION).
/// @param[in]  frame_size   The frame size in physical pixels.
///
/// @return     The frame damage tracker or NULL on failure.
///
/// @example
/// @code
///   FlowISize size = {800, 600};
///   FlowFrameDamage damage = FlowFrameDamageNew(FLOW_VERSION, &size);
///   if (damage) {
///     // Use damage tracker...
///     FlowFrameDamageRelease(damage);
///   }
/// @endcode
///
FLOW_EXPORT FLOW_NODISCARD FlowFrameDamage FLOW_NULLABLE
FlowFrameDamageNew(uint32_t version, const FlowISize* FLOW_NONNULL frame_size);

//------------------------------------------------------------------------------
/// @brief      Retain a reference to the frame damage tracker.
///
/// @param[in]  damage  The frame damage tracker.
///
FLOW_EXPORT
void FlowFrameDamageRetain(FlowFrameDamage FLOW_NULLABLE damage);

//------------------------------------------------------------------------------
/// @brief      Release a reference to the frame damage tracker.
///
/// @param[in]  damage  The frame damage tracker.
///
FLOW_EXPORT
void FlowFrameDamageRelease(FlowFrameDamage FLOW_NULLABLE damage);

//------------------------------------------------------------------------------
/// @brief      Set the previous frame's layer tree for comparison.
///
/// Used to compute damage between frames by comparing the previous and current
/// layer trees. The damage tracker stores a reference to the layer tree.
///
/// @param[in]  damage      The frame damage tracker.
/// @param[in]  tree        The previous frame's layer tree (may be NULL).
///
/// @example
/// @code
///   FlowFrameDamage damage = FlowFrameDamageNew(FLOW_VERSION, &size);
///   FlowLayerTree prev_tree = ...;
///   FlowFrameDamageSetPreviousLayerTree(damage, prev_tree);
/// @endcode
///
FLOW_EXPORT
void FlowFrameDamageSetPreviousLayerTree(FlowFrameDamage FLOW_NONNULL damage,
                                         FlowLayerTree FLOW_NULLABLE tree);

//------------------------------------------------------------------------------
/// @brief      Add an explicit damage rectangle.
///
/// Used to mark regions that need repainting, beyond what automatic damage
/// computation would identify. Rectangles are in screen/device coordinates.
///
/// @param[in]  damage  The frame damage tracker.
/// @param[in]  rect    The damage rectangle to add.
///
/// @example
/// @code
///   FlowIRect damage_rect = {100, 100, 200, 150};
///   FlowFrameDamageAddAdditionalDamage(damage, &damage_rect);
/// @endcode
///
FLOW_EXPORT
void FlowFrameDamageAddAdditionalDamage(FlowFrameDamage FLOW_NONNULL damage,
                                        const FlowIRect* FLOW_NONNULL rect);

//------------------------------------------------------------------------------
/// @brief      Compute the minimal clip rectangle that covers all damage.
///
/// Calculates the bounding rectangle of all damaged regions, including:
/// - Regions changed between previous and current layer trees
/// - Explicitly added damage rectangles (via AddAdditionalDamage)
///
/// @param[in]  damage           The frame damage tracker.
/// @param[in]  tree             The current frame's layer tree.
/// @param[out] out_clip_rect    The minimal bounding rectangle (device coords).
///
/// @return     true if damage computation succeeded, false otherwise.
///
/// @example
/// @code
///   FlowIRect clip_rect;
///   if (FlowFrameDamageComputeClipRect(damage, tree, &clip_rect)) {
///     printf("Repaint region: (%d, %d, %d, %d)\n",
///            clip_rect.x, clip_rect.y, clip_rect.width, clip_rect.height);
///   }
/// @endcode
///
FLOW_EXPORT
bool FlowFrameDamageComputeClipRect(FlowFrameDamage FLOW_NONNULL damage,
                                    FlowLayerTree FLOW_NONNULL tree,
                                    FlowIRect* FLOW_NONNULL out_clip_rect);

FLOW_EXTERN_C_END

#endif  // FLUTTER_FLOW_INTEROP_FLOW_H_
