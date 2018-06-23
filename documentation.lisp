#|
 This file is a part of Terrable
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.terrable)

(docs:define-docs
  (type terrable-condition
    "Base condition for all problems related to Terragen files.

See STREAM-POSITION")
  
  (function stream-position
    "Returns the fast-io buffer/stream's position at which the problem occurred.

See TERRABLE-CONDITION")
  
  (type invalid-header
    "Error signalled when the file header is invalid.

See HEADER
See TERRABLE-CONDITION")
  
  (function header
    "Returns the offending header that did not match.

See INVALID-HEADER")
  
  (type unknown-chunk-type
    "Warning signalled when an unknown chunk type is encountered.

Since an unknown chunk type cannot be read, it is very likely that the
rest of the file will be read as garbage as well and the read won't
succeed.

See CHUNK-TYPE
See TERRABLE-CONDITION")
  
  (function chunk-type
    "Returns the unknown 4-letter string chunk-type that was encountered.

See UNKNOWN-CHUNK-TYPE")
  
  (type unknown-curvature-type
    "Warning signalled when an unknown curvature type is encountered.

The curvature type is stored in its integer form in the TERRAIN instance's
CURVE-MODE slot.

See CURVATURE-TYPE
See TERRABLE-CONDITION")
  
  (function curvature-type
    "Returns the unknown curvature type.

See UNKNOWN-CURVATURE-TYPE")
  
  (type terrain
    "Container for all data read from a Terragen terrain file.

If some data is not explicitly provided by the terrain file, the slots
will contain default values.

WARNING: When a TERRAIN instance is garbage-collected, its DATA vector
is automatically deallocated with it. If you keep foreign references to
the data, or reference it somewhere else, you will need to keep a
reference to the TERRAIN instance around as well, copy the contents
of the data vector elsewhere, or call TG:CANCEL-FINALIZATION on the
TERRAIN instance to get rid of its automatic deallocation behaviour. If
you would like to manually free the terrain instantly, use FREE-TERRAIN.

See WIDTH
See HEIGHT
See SCALE
See CURVE-RADIUS
See CURVE-MODE
See HEIGHT-BASE
See HEIGHT-SCALE
See DATA
See READ-TERRAGEN
See FREE-TERRAIN")
  
  (function width
    "Returns the width (in pixels) of the terrain.

See TERRAIN")
  
  (function height
    "Returns the height (in pixels) of the terrain.

See TERRAIN")
  
  (function scale
    "Returns the scaling factors to turn pixel coordinates into real-world units.

This is a list of three floats (X, Y, Z), each of which represent
the number of pixel units in that direction represent a metre.
The default is (30 30 30), meaning 30 pixels in any direction for a
metre.

See TERRAIN")
  
  (function curve-radius
    "Returns the curve radius for spherical terrains.

This value is a float in the unit of kilometres. The default value
is 6370, an approximation of Earth's radius. Note that this value is
only relevant if CURVE-MODE is :DRAPED.

See TERRAIN")
  
  (function curve-mode
    "Returns the curvature mode of the terrain.

This value is a keyword for known curve modes, or an integer for
unknown ones. The following known modes exist:

  :FLAT   --- The terrain is a flat map.
  :DRAPED --- The terrain is draped over a sphere with a radius of
                (/ (* CURVE-RADIUS 1000) (third SCALE))
              with its centre point being at
                X: (/ WIDTH 2)
                Y: (/ HEIGHT 2)
                Z: (/ (* CURVE-RADIUS -1000) (third SCALE))

See TERRAIN")
  
  (function height-base
    "Returns the base height of the terrain.

This height should be added to all points in the data terrain.

See TERRAIN")
  
  (function height-scale
    "Returns the height scaling factor of the terrain.

This factor should be multiplied with all points in the data terrain
after HEIGHT-BASE has been added to receive a height that is properly
scaled.

See TERRAIN
See HEIGHT-BASE")
  
  (function data
    "Returns a static-vector of the pixel data that makes up the terrain.

This pixel-data is a sequence of 16 bit signed integers, stored in little-
endian encoding in an unsigned-byte 8 array. If you don't immediately load
this into another software like OpenGL, you will have to stitch the
integers back together manually.

Also note that these units are not directly in the scale of SCALE, but
must first be normalised by adding HEIGHT-BASE and then multiplying by
HEIGHT-SCALE.

Finally, note that this is a static-vector and is thus directly passable
to C by using STATIC-VECTORS:STATIC-VECTOR-POINTER.

See HEIGHT-BASE
See HEIGHT-SCALE
See STATIC-VECTORS:STATIC-VECTOR-POINTER
See TERRAIN")
  
  (function read-terragen
    "Parses the given thing as a Terragen terrain.

Returns the complete TERRAIN instance if successful. This function will
signal conditions of type TERRABLE-CONDITION should problems occur while
parsing the format.

The input can be either a FAST-IO:INPUT-BUFFER, a STREAM, a PATHNAME, a
STRING, or a VECTOR with (UNSIGNED-BYTE 8) element-type.

See TERRAIN
See TERRABLE-CONDITION
See INVALID-HEADER
See UNKNOWN-CHUNK-TYPE
See UNKNOWN-CURVATURE-TYPE")

  (function free-terrain
    "Instantly frees the data array in the terrain instance.

After calling this function on a TERRAIN instance, its data slot will
contain NIL and all references to the former data array will be invalid.

See TERRAIN"))

