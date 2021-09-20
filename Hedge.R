
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
    tabPanel('Hedge',
             shiny::HTML(text = '<br>'),
             shiny::HTML(text = '<br>'),
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = 'asset_returns',label = 'Select asset returns',choices = '',selected = '',multiple = F),
                 selectInput(inputId = 'hedge_returns',label = 'Select hedge returns',choices = '',selected = '',multiple = T),
                 textInput(inputId = 'text_nWindowVec',label = 'Enter window sizes',value = '20,120,252'),
                 numericInput(inputId = 'num_nTestAhead',label = 'Enter periods ahead to test',value = 1),
                 actionButton(inputId = 'action_fit',label = 'Fit!')),
               mainPanel(
                 tabsetPanel(
                   tabPanel('Plot',
                            plotOutput(outputId = 'plot'))
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

  Obj_data = eventReactive(input$file1,{
    read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote,
             row.names = input$numin_rownames)
  })
  
  observeEvent(input$file1,{
    df = Obj_data()
    colnamesx = colnames(df)
    ncols = length(colnamesx)
    n = dim(df)[1]
    x = paste0(round(quantile(x = 1:n,probs = (1:9 * 10/100)),0),collapse = ',')
    updateSelectInput(session = session,inputId = 'asset_returns',choices = colnamesx,selected = colnamesx[1])
    updateSelectInput(session = session,inputId = 'hedge_returns',choices = colnamesx,selected = colnamesx[2:ncols])
    updateTextInput(session = session, inputId = 'text_nWindowVec',value = x)
  })
  
  # Tables ####
  output$tabout = renderTable({
    Obj_data()
  },rownames = TRUE)

  # Code ####
  Obj_filename = eventReactive(input$file1,{
    input$file1$name
  })
  
  # Downloading ####
  output$download_script <- downloadHandler(
    filename = function() {
      input$savename
    },
    content = function(file) {
      capture.output(cat(Obj_code()),file = file)
    }
  )

  Obj_nWindowVec = eventReactive(input$action_fit,{
    as.numeric(strsplit(x = input$text_nWindowVec,split = ',')[[1]])
  })

  # Fit ####
  Obj_ErrorSDs = eventReactive(input$action_fit,{
    
    R = Obj_data()
    asset_returns = as.matrix(R[,input$asset_returns])
    hedge_returns = as.matrix(R[,input$hedge_returns])
    
    n = length(asset_returns)
    nTestAhead = input$num_nTestAhead
    i_window = 1
    nWindowVec = Obj_nWindowVec()
    errorvols = vector(mode = 'numeric',length = length(nWindowVec))
    for (nWindow in nWindowVec){
      
      idx_window = 1:nWindow
      nTests = n-nWindow - nTestAhead + 1

      testvec = vector(mode = 'numeric',length = nTests)
      idx_test = nWindow+nTestAhead

      for (i in 1:nTests){
        x = as.matrix(hedge_returns[idx_window,])
        y = asset_returns[idx_window]
        fit = lm.fit(x = x, y = y)
        h = fit$coefficients
        testvec[i] =  asset_returns[idx_test] - h %*% hedge_returns[idx_test,]

        idx_test = idx_test + 1
        idx_window = idx_window + 1
      }
      errorvols[i_window] = sd(testvec)
      i_window = i_window + 1
    }
    errorvols
  })

  output$plot = renderPlot({
    plot(Obj_nWindowVec(),Obj_ErrorSDs(),xlab= 'Window Sizes',ylab = 'Error Vol')
  })

  
  # Printing  ####
  
  output$code = renderPrint(cat(Obj_code()))

  Obj_code = reactive({
    str_windowvec = as.character(Obj_nWindowVec())
    str_windowvec = paste(str_windowvec,collapse = ',')
    str_windowvec = paste0('c(',str_windowvec,')')
    hedge_returns = paste0(input$hedge_returns,collapse ="','")
    
    paste0(
"
R = read.csv(file = '",Obj_filename(),"',
         header = ",input$header,",
         row.names = ",input$numin_rownames,")
         
asset_returns = as.matrix(R[,'",input$asset_returns,"'])
hedge_returns = as.matrix(R[,c('",hedge_returns,"')])

n = length(asset_returns)
nTestAhead = ",input$num_nTestAhead,"
i_window = 1
nWindowVec = ",str_windowvec,"
errorvols = vector(mode = 'numeric',length = length(nWindowVec))
for (nWindow in nWindowVec){

  idx_window = (n-nWindow+1):n
  nTests = n-nWindow - nTestAhead + 1

  testvec = vector(mode = 'numeric',length = nTests)
  idx_test = nTests

  for (i in 1:nTests){
    x = hedge_returns[idx_window,]
    y = asset_returns[idx_window]
    fit = lm.fit(x = x, y = y)
    h = fit$coefficients
    testvec[idx_test] =  asset_returns[idx_test] - h %*% hedge_returns[idx_test,]

    idx_test = idx_test - 1
    idx_window = idx_window - 1
  }
  errorvols[i_window] = sd(testvec)
  i_window = i_window + 1
}
",sep='')
 })

}

shinyApp(ui, server)