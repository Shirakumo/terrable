## About Terrable
Terrable implements a parser for the Terragen terrain file format (`.TER`). You can find a format reference [here](https://planetside.co.uk/wiki/index.php?title=Terragen_.TER_Format).

## How To
To parse a file, you can pass a file, vector, or fast-io buffer to `read-terragen`.

    (terrable:read-terragen #p"~/heightmap.ter")

If everything goes well, you should have a fresh `terrain` instance with `width`, `height`, `scale`, `curve-radius`, `curve-mode`, `height-base`, `height-scale`, and `data` slots all filled out for you. The data is parsed as a static-vector, meaning it is directly passable to C libraries like OpenGL for further processing.

    (gl:tex-image-2d :texture-2d 0 :R16I (terrable:width *) (terrable:height *) 0 :RED :SHORT
                     (static-vectors:static-vector-pointer (terrable:data *)))

And that's it. Note that finalizers are used to automatically deallocate the data array when the terrain instance is garbage collected. If you want to manually free the terrain data instantly, you can use `free-terrain`.

    (terrable:free-terrain **)
