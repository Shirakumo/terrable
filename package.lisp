(defpackage #:terrable
  (:nicknames #:org.shirakumo.fraf.terrable)
  (:use #:cl)
  (:export
   #:terrable-condition
   #:stream-position
   #:invalid-header
   #:header
   #:unknown-chunk-type
   #:chunk-type
   #:unknown-curvature-type
   #:curvature-type
   #:terrain
   #:width
   #:height
   #:scale
   #:curve-radius
   #:curve-mode
   #:height-base
   #:height-scale
   #:data
   #:read-terrain
   #:free-terrain
   #:write-terrain))
