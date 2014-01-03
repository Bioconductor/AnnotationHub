setMethod("display", signature(object="AnnotationHub"),
    function(object, ...) 
{
    md <- metadata(object)
    drops <-c("Title", "TaxonomyId")
    md <- md[,!(names(md) %in% drops)]
    #Concatenate a string with show(object) and pass to the next line..
    d <- display(as.data.frame(md)) 
    filters(object) <- as.list(d['RDataPath'])
    return(object)
})
