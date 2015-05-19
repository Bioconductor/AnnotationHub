library(shiny)


#####################################################
## the original demo as a function:

# display2 <- 
# function(df, ...)
# {    
#     app <- list(
#                 ui = fluidPage(
#                     title = 'Row selection in DataTables',
#                     sidebarLayout(
#                         sidebarPanel(textOutput('rows_out')),
#                         mainPanel(dataTableOutput('tbl')),
#                         position = 'right'
#                     )
#                 )                
#                 ,
#                 server = function(input, output) {
#                     output$rows_out <- renderText({
#                         paste(c('You selected these rows on the page:', 
#                                 input$rows),
#                                     collapse = ' ')
#                     })                    
#                     output$tbl <- renderDataTable(
#                         df,
#                         options = list(pageLength = 10),
#                         callback = "function(table) {
#                         table.on('click.dt', 'tr', function() {
#                         $(this).toggleClass('selected');
#                         Shiny.onInputChange('rows',
#                         table.rows('.selected').indexes().toArray());
#                         }); }")
#                 }
#     )
# 
#     ## selectively use the RStudio viewer pane (if available)
#     viewer <- getOption("viewer")
#     if (!is.null(viewer)){
#         runApp(app, launch.browser = rstudio::viewer, ...)
#     }else{
#         runApp(app, ...)
#     }
# }


#####################################################
## my function that tries to just use indexes:
display2 <- 
    function(df, ...)
    {
        rowNames <- rownames(df)
        dt <- data.frame(rownames=rowNames,df)
        ## define the app
        app <- list(
            ui = fluidPage(
                title = 'The data from your data.frame',
                sidebarLayout(
                    sidebarPanel(textOutput('rows_out'),
                                 br(),
                                 actionButton("btnSend", "Send Rows")),
                    mainPanel(dataTableOutput('tbl')),
                    position = 'left'
                )
            )                
            ,
            server = function(input, output) {
                output$rows_out <- renderText({
                    paste(c('You selected these rows on the page:', 
                            input$rows),
                          collapse = ' ')
                })                    
                output$tbl <- renderDataTable(
                    dt,
                    options = list(pageLength = 20),
                    callback = "function(table) {
                    table.on('click.dt', 'tr', function() {
                    $(this).toggleClass('selected');
                    Shiny.onInputChange('rows',
                    table.rows('.selected').indexes().toArray());
                    }); }")
                observe({
                    if(input$btnSend > 0)
                        isolate({
                            #print(input$rows)
                            idx <- as.integer(input$rows) + 1
                            stopApp(returnValue = df[idx,])
                        })
                })                            
        })
        ## selectively use the RStudio viewer pane (if available)
        viewer <- getOption("viewer")
        if (!is.null(viewer)){
            runApp(app, launch.browser = rstudio::viewer, ...)
        }else{
            runApp(app, ...)
        }
}
## usage: 
## display2(mtcars)

## original:
## table.rows('.selected').indexes().toArray());

## This is kind of the same thing
##     table.rows( $('.selected').closest('tr') ).indexes().toArray());
## And this gets just the 1st one
##     table.rows( $('.selected').closest('tr')[0] ).indexes().toArray());






#####################################################
## my function that just get the row data (like before)
display2 <- 
    function(df, ...)
    {
        rowNames <- rownames(df)
        dt <- data.frame(rownames=rowNames,df)
        ## define the app
        app <- list(
            ui = fluidPage(
                title = 'The data from your data.frame',
                sidebarLayout(
                    sidebarPanel(textOutput('rows_out'),
                                 br(),
                                 actionButton("btnSend", "Send Rows")),
                    mainPanel(dataTableOutput('tbl')),
                    position = 'left'
                )
            )                
            ,
            server = function(input, output) {
                output$rows_out <- renderText({
                    paste(c('You selected these rows on the page:', 
                            input$rows),
                          collapse = ' ')
                })                    
                output$tbl <- renderDataTable(
                    dt,
                    options = list(pageLength = 50),
                    callback = "function(table) {
                    table.on('click.dt', 'tr', function() {
                    $(this).toggleClass('selected');
                    Shiny.onInputChange('rows',                    
            table.rows('.selected').indexes().toArray());
                    Shiny.onInputChange('tbl',                    
            table.rows('.selected').indexes().toArray());
                    }); }")
## TODO: change the above callback so that it returns rowRanges (not just indexes)
                observe({
                    if(input$btnSend > 0)
                        isolate({
                            #print(input$rows)
#                            idx <- as.integer(input$rows) + 1
#                           stopApp(returnValue = df[idx,])
                            
#                             dfVec <- input$myTable
#                             df <- as.data.frame(matrix(data=dfVec, ncol=dim(df)[2],
#                                                        byrow=TRUE))
#                             names(df) <- colNames
                             stopApp(returnValue = input$tbl)                            
                        })
    })                            
    })
    ## selectively use the RStudio viewer pane (if available)
    viewer <- getOption("viewer")
    if (!is.null(viewer)){
        runApp(app, launch.browser = rstudio::viewer, ...)
    }else{
        runApp(app, ...)
    }
    }
## usage: 
## display2(mtcars)


