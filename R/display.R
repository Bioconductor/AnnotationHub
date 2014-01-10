setMethod("display", signature(object="AnnotationHub"),
    function(object, ...) 
{ 
    md <- metadata(ah)
    drops <-c("Title", "TaxonomyId")
    md <- md[,!(names(md) %in% drops)]
    summaryMessage = capture.output(show(ah))
    serverOptions= list(
        bSortClasses=TRUE,
        aLengthMenu = c(1000, 5000, "All"),
        iDisplayLength = 1000,
        "sDom" = '<"top"i>rt<"top"f>lt<"bottom"p><"clear">')
    ch <- list( 
            '<script type="text/javascript"
                     class="shiny-html-output"
                     language="javascript" 
                     src="/js/editable.min.js" ></script>', 
            '<script class="jsbin" 
                     src="/js/jquery.dataTables.nightly.js" ></script>', 
            '<script class="shiny-html-output" 
                     type="text/javascript" 
                     src="/js/demo.js" ></script>',
            '<script src = "/js/shiny2.js" ></script>' )
    d <- display(object =as.data.frame(md), summaryMessage = summaryMessage, 
        serverOptions = serverOptions, style=ch )
    filters(object) <- as.list(d['RDataPath'])
    return(object)
})

# ch <- c(
# 'tags$script(type="text/javascript",class="shiny-html-output", language="javascript", src="/js/editable.min.js")',  
# 'tags$script(class="jsbin", class="shiny-html-output", src="/js/jquery.dataTables.nightly.js")',  
# 'tags$script(class="shiny-html-output", src="/js/demo.js")', 
# 'tags$script(class="shiny-html-output", src = "/js/shiny2.js")') #NEW 

