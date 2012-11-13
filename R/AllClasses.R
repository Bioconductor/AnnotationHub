## Some internal value are used for knowing what this object "points" to...
## curPath = holds the path that the user has chosen "so far".

## paths = holds character vector of possible paths to choose from.

## pattern = what tab completion has guess so far based on the paths and the
## user interaction.

## curPathExtendedYet = has the curPath been successfully extended yet?  IOW,
## has the user chosed a viable paths value, once this is true, we can reset
## curPath to something bigger and move on to the next set of paths.

setClass("AnnotationHub", representation(curPath = "character",
                                         paths="character",
                                         pattern="character",
                                         curPathExtendedYet="logical"))


