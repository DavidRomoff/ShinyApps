
library(shiny)
library(shinythemes)
library(quantmod)
library(ggplot2)
library(xts)
library(stringr)

tickers = c('AAPL','GOOG','GS','FB','IBM','TSLA','PLTR','MSFT','ETH-USD','BTC-USD','DOGE-USD','^GSPC')
sampletickers = paste(sample(tickers,2),collapse = ',')
str = as.character(Sys.time())
str = str_replace_all(str,'-','_')
str = str_replace_all(str,' ','_')
str = str_replace_all(str,':','_')
savename = str
savename_script = paste0(str,'_script.R')

ui <- fluidPage(theme = shinytheme("cyborg"),
                shiny::HTML(text = '<br>'),
  tabsetPanel(
    tabPanel('Query',
             shiny::HTML(text = '<br>'),
             shiny::HTML(text = '<br>'),
             sidebarLayout(
               sidebarPanel(
                 textInput(inputId = 'tickers',label = 'Enter Ticker Symbols',value = sampletickers),
                 dateRangeInput(inputId = "daterange1",
                                label = "Date Range",
                                start = as.character(Sys.Date()-30),
                                end = as.character(Sys.Date())),
                 checkboxGroupInput(inputId = 'checkbox',label = 'Select fields',selected = 6,inline = TRUE,
                                    choiceNames = c('open','high','low','close','volume','adjusted'),
                                    choiceValues = 1:6),
                 br(),
                 actionButton(inputId = 'action_Download',label = 'Query!')
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel('Table',
                            tableOutput(outputId = 'tabout')
                   )
                 ),
               )
             )
    ),
    tabPanel('Save',
             shiny::HTML(text = '<br>'),
             shiny::HTML(text = '<br>'),
             radioButtons(inputId = 'radio_csv_xts',label = 'csv/xts',choices = c('csv','xts'),selected = 'csv',inline = TRUE),
             textInput(inputId = 'savename1',label = 'File Name',value = savename),
             downloadButton(outputId = "downloadData",label =  "Download")
    ),
    tabPanel('Code',
             shiny::HTML(text = '<br>'),
             shiny::HTML(text = '<br>'),
             verbatimTextOutput(outputId = 'code'),
             textInput(inputId = 'savename2',label = 'Save name',value = savename_script),
             downloadButton("download_script")
             )
  )
)

server <- function(input, output, session) {
  
  output$downloadData <- downloadHandler(
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
      x = Obj_download()
      if (input$radio_csv_xts == 'csv'){ write.csv(x = as.data.frame(x), file = file, row.names = TRUE)}
      if (input$radio_csv_xts == 'xts'){ save(x,file = file)}
    }
  )
  
  
  output$download_script <- downloadHandler(
    filename = function() {
      input$savename2
    },
    content = function(file) {
      capture.output(cat(Obj_code()),file = file)
    }
  )
  
  
  
  Obj_download = eventReactive(input$action_Download,{
    tickers = input$tickers
    tickers = str_replace_all(input$tickers,pattern = '\t',replacement = ',')
    tickers = str_replace_all(input$tickers,pattern = ' ',replacement = ',')
    tickers = unique(c(strsplit(tickers,',')[[1]]))
    namevec = quantmod::getSymbols(tickers,auto.assign = T,from = input$daterange1[1],to = input$daterange1[2])
    namevec = str_replace_all(namevec,'\\^','')
    idx_fields = as.numeric(input$checkbox)
    x = get(namevec[1])[,idx_fields]
    n = length(namevec)
    if (n > 1){
      for (i in 2:n){
        x = merge.xts(x,get(namevec[i])[,idx_fields])
      }
    }
    x
  })
  
  output$tabout <- renderTable({
    Obj_download()
  },rownames = TRUE)

  
  Obj_code = reactive({
paste0("
tickers = c('",paste0(strsplit(input$tickers,',')[[1]],collapse ="','"),"')
namevec = quantmod::getSymbols(tickers,auto.assign = T,from = '",input$daterange1[1],"',to = '",input$daterange1[2],"')
namevec = str_replace_all(namevec,'\\\\^','')
idx_fields = c(",paste(as.numeric(input$checkbox),collapse = ","),")
x = get(namevec[1])[,idx_fields]
n = length(namevec)
if (n > 1){
  for (i in 2:n){
    x = merge.xts(x,get(namevec[i])[,idx_fields])
  }
}
print(x)
",sep='')
  })
    
  output$code = renderPrint(cat(Obj_code()))
  

}

shinyApp(ui, server)