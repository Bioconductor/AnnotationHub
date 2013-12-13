

setMethod("display", signature(object="AnnotationHub"),
          function(object, ...) {
    md <- metadata(object)
    d <- display(as.data.frame(md)) # cast as dataframe to display in browser

    ## f <- apply(d,2, unique) 

    # drop the element named "Tags"
    ## f2 <- f[-which(names(f) %in% c("Tags"))]

    filters(object) <- as.list(d['RDataPath'])

    return(object)
})
