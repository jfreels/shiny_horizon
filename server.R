### Created by Justin Freels
### email: jfreels@gmail.com
### twitter: https://twitter.com/jfreels4
### github: https://github.com/jfreels

# Load libraries
libs<-c("lubridate","plyr","reshape2","ggplot2","xts","PerformanceAnalytics","shiny","devtools")
lapply(libs,require,character.only=TRUE)
install_github(repo="r_jfreels",username="jfreels")
library(jfreels)

# load example dataset
example<-read.csv("example.csv")
example$date<-ymd(example$date)
example<-dcast(example,date~fund,value.var="return")

##### SHINY SERVER
shinyServer(function(input, output) {
  
# reactive: upload_dataset
  upload_dataset <- reactive({
    if (is.null(input$csv)) { return(NULL) }
    d<-read.csv(input$csv$datapath,check.names=FALSE)
    d$date<-ymd(d$date)
    d
  })

# reactive: dataset_original
  dataset_original <- reactive({
    dat<-if (input$upload=="Yes") { 
      upload_dataset()
    }
    else { 
      example
    }
    dat
  })
    
# reactive: dataset
  dataset <- reactive({
    dat<-if (input$upload=="Yes") { 
        droplevels(upload_dataset()[,c("date",input$upload_choose_fund)])
      }
      else { 
        droplevels(example[,c("date",input$example_choose_fund)])
      }
  })
  
# reactive: choice
  choices <- reactive({
    if(input$upload=="No") { input$example_choose_fund }
    else { input$upload_choose_fund }
  })

# reactive: data_export
  data_export<-reactive({
    dat<-dataset()
    dat_subset<-if(input$data_subset=="Common") { na.omit(dat) }
    else { 
      dat               
    }
    dat_format<-if(input$data_format=="Wide") { dat_subset }
    else { na.omit(melt(dat_subset,id.vars="date")) }
    dat_format
  })

# reactive: dataset_final
  dataset_final<-reactive({
    subset(data_export(),date>=ymd(input$data_start_date_input)&date<=ymd(input$data_end_date_input))
    #data_export()[data_export()$date>=input$data_end_date_input,]
  })

# reactive: melted_dataset
  melted_dataset<-reactive({
    dat<-if(input$data_format=="Wide") { melt(dataset_final(),id="date") }
    else { dataset_final() }
    dat
  })
  
### sideBarPanel reactive UIs
  output$example_choose_fund<-renderUI({
    if (input$upload=="No") { return(NULL) }
    conditionalPanel(
      condition="input.upload=='Yes'",
      selectInput(inputId="upload_choose_fund",label="Choose Funds:",choices=names(upload_dataset()[-1]),multiple=TRUE)
    )
  })
  
  output$upload_choose_fund<-renderUI({
    if (input$upload=="Yes") { return(NULL) }
    conditionalPanel(
      condition="input.upload=='No'",
      selectInput(inputId="example_choose_fund",label="Choose Funds:",choices=names(example[-1]),multiple=TRUE)
    )    
  })
  
  output$data_start_date<-renderUI({
    selectInput(inputId="data_start_date_input",label="Start Date:",choices=unique(as.character(data_export()$date)))
  })
  
  output$data_end_date<-renderUI({
    selectInput(inputId="data_end_date_input",label="End Date:",choices=rev(unique(as.character(data_export()$date))))
  })

### Tab: "Horizon"
  output$rolling_plot12<-renderPlot({
    dat<-ddply(na.omit(melted_dataset()),.(variable),transform,roll=rollapply(value,12,FUN=function (x) {tail(cumprod(na.omit(x) + 1), 1) - 1},fill=NA,align="right"))
    dat<-dat[,-3]    
    print(horizon.panel.ggplot(dat,"Horizon Plot: 12 Month Rolling Returns"))
  })
  
  output$rollign_plot36<-renderPlot({
    dat<-ddply(na.omit(melted_dataset()),.(variable),transform,roll=rollapply(value,36,FUN=function (x) {tail(cumprod(na.omit(x) + 1), 1) - 1},fill=NA,align="right"))
    dat<-dat[,-3]  
    print(horizon.panel.ggplot(dat,"Horizon Plot: 36 Month Rolling Returns"))
  })
  
### Tab: "Drawdown"
  output$drawdown_plot<-renderPlot({
    dat<-melted_dataset()
    dat<-ddply(dat,.(variable),transform,dd=dd(value))
    dat<-dat[,-3]
    print(horizon.panel.ggplot(dat,"Horizon Plot: Drawdown"))
  })
  
### Tab: "Data Preview"
  output$data_choices<-renderPrint({
    choices()
  })
  
  output$data_export_str<-renderPrint({
    str(dataset_final())
  })
  
  output$data_export_summary<-renderPrint({
    head(dataset_final(),5)
  })
  
### Tab: "Example"
  output$example<-renderTable({
    example$date<-as.character(example$date)
    head(na.omit(example[,1:3]),10)
  },digits=4)

### Export Data
  output$exportData<-downloadHandler(
    filename=function() { paste0(input$exportName,".csv") },
    content = function(file) {
      write.csv(data_export(),file,row.names=FALSE)
    }
  )
  
})