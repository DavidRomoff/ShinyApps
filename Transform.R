
library(shiny)
library(shinythemes)
library(stringr)

str = as.character(Sys.time())
str = str_replace_all(str,'-','_')
str = str_replace_all(str,' ','_')
str = str_replace_all(str,':','_')
savename = str
savename_script = paste0(str,'_script.R')
savename1 = paste0(str,'_percentreturns.csv')
savename2 = paste0(str,'_dollarreturns.csv')

ui <- fluidPage(theme = shinytheme("cyborg"),
                shiny::HTML(text = '<br>'),
  tabsetPanel(
    tabPanel('Upload',
             shiny::HTML(text = '<br>'),
             shiny::HTML(text = '<br>'),
             sidebarLayout(
               sidebarPanel(
                 fileInput(inputId = "file1",
                       label =  "Choose CSV File",
                       multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
                       ),
                 checkboxInput("header", "Header", TRUE),
                 numericInput(inputId = 'numin_rownames',label = 'Row names column',value = 1),
                 
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ","),
                 radioButtons("quote", "Quote",
                              choices = c(None = "",
                                          "Double Quote" = '"',
                                          "Single Quote" = "'"),
                              selected = '"'),
                 
                 radioButtons("disp", "Display",
                              choices = c(Head = "head",
                                          All = "all"),
                              selected = "head")
                 ),
               mainPanel(
                 tableOutput(outputId = 'tabout')
               )
             ),
             ),
    tabPanel('Transform',
             shiny::HTML(text = '<br>'),
             shiny::HTML(text = '<br>'),
                 tabsetPanel(
                   tabPanel('Create returns',
                            shiny::HTML(text = '<br>'),
                            shiny::HTML(text = '<br>'),
                            sidebarLayout(
                              sidebarPanel(
                                numericInput(inputId = 'numin_lag',label = 'Lag',value = 1),
                                actionButton(inputId = 'action_makepercentreturns',label = 'Create returns data'),
                                shiny::HTML(text = '<br>'),
                                shiny::HTML(text = '<br>'),
                                conditionalPanel(
                                  condition = "output.show1",
                                  shiny::HTML(text = '<br>'),
                                  shiny::HTML(text = '<br>'),
                                  wellPanel(
                                    h4('Save'),
                                    radioButtons(inputId = 'radio_csv_xts',label = 'csv/xts',choices = c('csv','xts'),selected = 'csv',inline = TRUE),
                                    textInput(inputId = 'savename1',label = 'File Name',value = savename1),
                                    downloadButton(outputId = "downloadData1",label =  "Download")
                                  )
                                )
                              ),
                              mainPanel(
                                tableOutput(outputId = 'tabout3')
                              )
                            )
                   ),
                   tabPanel('Create dollar returns',
                            shiny::HTML(text = '<br>'),
                            shiny::HTML(text = '<br>'),
                            sidebarLayout(
                              sidebarPanel(
                                numericInput(inputId = 'numin_lagdollar',label = 'Lag',value = 1),
                                textInput(inputId = 'numin_dollars',label = 'Enter dollars',value = 1),
                                actionButton(inputId = 'action_makedollarreturns',label = 'Create returns data'),
                                shiny::HTML(text = '<br>'),
                                shiny::HTML(text = '<br>'),
                                conditionalPanel(
                                  condition = "output.show2",
                                  shiny::HTML(text = '<br>'),
                                  shiny::HTML(text = '<br>'),
                                  wellPanel(
                                    h4('Save'),
                                    radioButtons(inputId = 'radio_csv_xts2',label = 'csv/xts',choices = c('csv','xts'),selected = 'csv',inline = TRUE),
                                    textInput(inputId = 'savename2',label = 'File Name',value = savename2),
                                    downloadButton(outputId = "downloadData2",label =  "Download")
                                  )
                                )
                                
                                
                              ),
                              mainPanel(
                                tableOutput(outputId = 'tabout4')
                                )
                            )
                   )
                 )
               ),

    tabPanel('Code',
             shiny::HTML(text = '<br>'),
             shiny::HTML(text = '<br>'),
             verbatimTextOutput(outputId = 'code'),
             textInput(inputId = 'savename',label = 'Save name',value = savename_script),
             downloadButton("download_script")
    )
  )
)

server <- function(input, output, session) {

  # Data objects ####
  Obj_data_dollarreturns = eventReactive(input$action_makedollarreturns,{
    df = Obj_data()
    nlag = input$numin_lagdollar
    n = dim(df)[1]
    R = df[(nlag+1):n,]/df[1:(n-nlag),]-1
    
    dollars = as.numeric(strsplit(input$numin_dollars,',')[[1]])
    ncols = dim(R)[2]
    for (i in 1:ncols){
      R[,i] = R[,i]*dollars[i]
    }
    R
  })
  
  Obj_data_returns = eventReactive(input$action_makepercentreturns,{
    df = Obj_data()
    
    nlag = input$numin_lag
    n = dim(df)[1]
    R = df[(nlag+1):n,]/df[1:(n-nlag),]-1
    R
  })

  Obj_data = eventReactive(input$file1,{
    read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote,
             row.names = input$numin_rownames)
  })
  
  observeEvent(input$file1,{
    df = Obj_data()
    ncols = dim(df)[2]
    str_dollars = paste0(rep('100',ncols),collapse=',')
    updateTextInput(session = session,inputId = 'numin_dollars',value = str_dollars)
  })
  
  Obj_filename = eventReactive(input$file1,{
    input$file1$name
  })
  
  
  # Tables ####
  output$tabout = renderTable({
    Obj_data()
  },rownames = TRUE)
  output$tabout2 = renderTable({
    Obj_data_date()
  },rownames = TRUE)
  output$tabout3 = renderTable({
    Obj_data_returns()
  },rownames = TRUE)
  output$tabout4 = renderTable({
    Obj_data_dollarreturns()
  },rownames = TRUE)
  
  
  # Printing  ####
  output$code = renderPrint(cat(Obj_code()))
  
  Obj_code = reactive({
    str = paste0(strsplit(input$numin_dollars,',')[[1]],collapse = ',')
    (
paste0("
prices = read.csv(file = '",Obj_filename(),"',
         header = ",input$header,",
         row.names = ",input$numin_rownames,")
         
nlag = ",input$numin_lag,"
n = dim(prices)[1]
percentreturns = prices[(nlag+1):n,]/prices[1:(n-nlag),]-1

nlag = ",input$numin_lagdollar,"
dollarreturns = prices[(nlag+1):n,]/prices[1:(n-nlag),]-1

dollars = c(",str,")
ncols = dim(dollarreturns)[2]
for (i in 1:ncols){
  dollarreturns[,i] = dollarreturns[,i]*dollars[i]
}

")
    )
    })
  
  
  # Downloading ####
  output$download_script <- downloadHandler(
    filename = function() {
      input$savename2
    },
    content = function(file) {
      capture.output(cat(Obj_code()),file = file)
    }
  )
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      fname = input$savename1
      if (input$radio_csv_xts == 'csv'){
        fname = ifelse(str_detect(fname,'.csv'),fname,paste0(fname,'.csv'))
      }
      if (input$radio_csv_xts == 'xts'){
        fname = ifelse(str_detect(fname,'.R'),fname,paste0(fname,'.RData'))
      }
      fname
    },
    content = function(file) {
      x = Obj_data_returns()
      if (input$radio_csv_xts == 'csv'){ write.csv(x = as.data.frame(x), file = file, row.names = TRUE)}
      if (input$radio_csv_xts == 'xts'){ save(x,file = file)}
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      fname = input$savename2
      if (input$radio_csv_xts == 'csv'){
        fname = ifelse(str_detect(fname,'.csv'),fname,paste0(fname,'.csv'))
      }
      if (input$radio_csv_xts == 'xts'){
        fname = ifelse(str_detect(fname,'.R'),fname,paste0(fname,'.RData'))
      }
      fname
    },
    content = function(file) {
      x = Obj_data_dollarreturns()
      if (input$radio_csv_xts == 'csv'){ write.csv(x = as.data.frame(x), file = file, row.names = TRUE)}
      if (input$radio_csv_xts == 'xts'){ save(x,file = file)}
    }
  )
  
  
  # ShowHide ####
  output$show1 <- eventReactive(input$action_makepercentreturns,{
    TRUE
  })
  output$show2 <- eventReactive(input$action_makedollarreturns,{
    TRUE
  })
  outputOptions(output, 'show1', suspendWhenHidden = FALSE)
  outputOptions(output, 'show2', suspendWhenHidden = FALSE)
  
}

shinyApp(ui, server)