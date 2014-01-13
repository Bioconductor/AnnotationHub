setMethod("display", signature(object="AnnotationHub"),
    function(object, ...) 
{ 
    #metadata- do not touch
    md <- metadata(object)
    md[["Tags"]] <- unstrsplit(md[["Tags"]], sep=", ")
        
    #create a data frame copy for edits called df
    df <- as.data.frame(md)
    
    #Adding a unique identifier
    Id <- seq_len(nrow(df))
    df <- cbind(Id, df)
    
    #dropping column names
    drops <-c("Title", "TaxonomyId")
    df <- df[,!(names(df) %in% drops), drop=FALSE]
    
    #Truncating extra large columns
    df[["Tags"]] <- substr(df[["Tags"]], 1, 10)
    df[["RDataPath"]] <- substr(df[["RDataPath"]], 1, 10)
    
    summaryMessage = capture.output(show(object))
    serverOptions= list(
        bSortClasses=TRUE,
        aLengthMenu = c(1000, 5000, "All"),
        iDisplayLength = 1000,
        "sDom" = '<"top"i>rt<"top"f>lt<"bottom"p><"clear">')
        
        
    d <- display(object =df, summaryMessage = summaryMessage, 
                 serverOptions = serverOptions)
    filterString <- md[gsub(" ","",as.vector(d[,1])),"RDataPath"]
    filters(object) <- list(RDataPath = filterString)
    object
})

