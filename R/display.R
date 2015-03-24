.display <- function(object, ...) { 
    #metadata- do not touch
    md <- mcols(object)
        
    #create a data frame copy for edits called df
    df <- as.data.frame(md)
        
    #dropping column names
    drops <-c("title", "taxonomyid", "sourceurl")
    df <- df[,!(names(df) %in% drops), drop=FALSE]
    
    summaryMessage = capture.output(show(object))
    serverOptions= list(
        bSortClasses=TRUE,
        aLengthMenu = c(1000, 5000, "All"),
        iDisplayLength = 1000,
        "sDom" = '<"top"i>rt<"top"f>lt<"bottom"p><"clear">')
        
    d <- display(object =df, summaryMessage = summaryMessage, 
                 serverOptions = serverOptions)
    idx <- rownames(d)
    object[idx]
}

setMethod("display", signature(object="AnnotationHub"),
          function(object){.display(object)})


## library(AnnotationHub); mh = AnnotationHub(); debug(AnnotationHub:::.display)
## foo <- display(mh)
