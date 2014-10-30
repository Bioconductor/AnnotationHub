.display <- function(object, ...) { 
    #metadata- do not touch
    md <- .resource_table(object)
        
    #create a data frame copy for edits called df
    df <- as.data.frame(md)
    
    #Adding a unique identifier
    Id <- seq_len(nrow(df))
    df <- cbind(Id, df)
    
    #dropping column names
    drops <-c("Title", "TaxonomyId")
    df <- df[,!(names(df) %in% drops), drop=FALSE]
    
    #Truncating extra large columns
#     df[["Tags"]] <- substr(df[["Tags"]], 1, 10)
#     df[["RDataPath"]] <- substr(df[["RDataPath"]], 1, 10)
    
    summaryMessage = capture.output(show(object))
    serverOptions= list(
        bSortClasses=TRUE,
        aLengthMenu = c(1000, 5000, "All"),
        iDisplayLength = 1000,
        "sDom" = '<"top"i>rt<"top"f>lt<"bottom"p><"clear">')
        
        
    d <- display(object =df, summaryMessage = summaryMessage, 
                 serverOptions = serverOptions)

    idx <- gsub(" ","",as.vector(d[,'Id']))

    ## trap the initial rownames like this:
    finalMd <- data.frame(md, db_uid= rownames(md))
    ## remove rownames from finalMd
    rownames(finalMd) <- NULL
    ## now subset out the rows and then modify the object
    finalMd <- finalMd[idx,]
    .db_uid(object) <- as.integer(as.character(finalMd[,'db_uid']))
    object
}

setMethod("display", signature(object="AnnotationHub"),
          function(object){.display(object)})


## library(AnnotationHub); mh = AnnotationHub(); debug(AnnotationHub:::.display)
## foo <- display(mh)
