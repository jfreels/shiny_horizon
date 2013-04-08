### Created by Justin Freels
### email: jfreels@gmail.com
### twitter: https://twitter.com/jfreels4
### github: https://github.com/jfreels

# Load libraries
libs<-c("lubridate","plyr","reshape2","ggplot2","xts","PerformanceAnalytics","shiny")
lapply(libs,require,character.only=TRUE)

# load functions
longToXts<-function (longDataFrame) { xts(longDataFrame[,-1],longDataFrame[,1]) }
xtsToLong<-function (Xts) { 
  df<-data.frame(date=index(Xts),coredata(Xts)) 
  names(df)<-c("date",names(Xts))
  df<-melt(df,id.vars="date")
  names(df)<-c("date","fund","return")
  df
}

# http://timelyportfolio.blogspot.com/2012/08/horizon-on-ggplot2.html
horizon.panel.ggplot <- function(df, title) {
  #df parameter should be in form of date (x), grouping, and a value (y)
  colnames(df) <- c("date","grouping","y")
  # numbers above the origin will be colored differently than numbers below the origin
  origin = 0
  #get number of bands for the loop
  #limit to 3 so it will be much more manageable
  nbands = 3
  # make this value equal to 1/nbands * range of the dataset
  upperLimit <- max(df[,3],na.rm=TRUE)
  lowerLimit <- min(df[,3],na.rm=TRUE)
  limit<- max(upperLimit,abs(lowerLimit))
  horizonscale <- (1/nbands)*limit
  #get some decent colors from RColorBrewer
  #we will use colors on the edges so 2:4 for red and 7:9 for blue
  require(RColorBrewer)
  col.brew <- brewer.pal(name="RdBu",n=10)
  #loop through nbands to add a column for each of the positive and negative bands
  for (i in 1:nbands) {
    #do positive
    df[,paste("ypos",i,sep="")] <- ifelse(df$y > origin,
                                          # true
                                          ifelse(abs(df$y) > horizonscale * i,
                                                 # true
                                                 horizonscale,
                                                 # false
                                                 ifelse(abs(df$y) - (horizonscale * (i - 1) - origin) > origin, abs(df$y) - (horizonscale * (i - 1) - origin), origin)),
                                          # false
                                          origin)
    #do negative
    df[,paste("yneg",i,sep="")] <- ifelse(df$y < origin,
                                          # true
                                          ifelse(abs(df$y) > horizonscale * i,
                                                 # true
                                                 horizonscale,
                                                 # false
                                                 ifelse(abs(df$y) - (horizonscale * (i - 1) - origin) > origin, abs(df$y) - (horizonscale * (i - 1) - origin), origin)),
                                          # false
                                          origin)
  }
  #melt data frame now that we have added a column for each band
  #this will fit ggplot2 expectations and make it much easier
  df.melt <- melt(df[,c(1:2,4:9)],id.vars=1:2)    
  #name the columns for reference
  #try to be generic
  colnames(df.melt) <- c("date","grouping","band","value")
  
  #use ggplot to produce an area plot
  p <- ggplot(data=df.melt) +
    geom_area(aes(x = date, y = value, fill=band),
              #alpha=0.25,
              position="identity") +  #this means not stacked
    scale_fill_manual(values=c("ypos1"=col.brew[7],  #assign the colors to each of the bands; colors get darker as values increase
                               "ypos2"=col.brew[8],
                               "ypos3"=col.brew[9],
                               "yneg1"=col.brew[4],
                               "yneg2"=col.brew[3],
                               "yneg3"=col.brew[2])) +
    ylim(origin,horizonscale) +   #limit plot to origin and horizonscale
    facet_grid(grouping ~ .) +    #do new subplot for each group
    theme_bw() +                  #this is optional, but I prefer to default
    theme(legend.position = "none",    #remove legend
          strip.text.y=element_text(angle=0, hjust=1),#rotate strip text to horizontal 
          axis.text.y = element_blank(),#remove y axis labels
          axis.ticks = element_blank(), #remove tick marks
          axis.title.y = element_blank(),#remove title for the y axis
          axis.title.x = element_blank(),#remove title for the x axis
          plot.title = element_text(size=16, face="bold", hjust=0))+ #format title
    labs(title=title)
  
  return(p)
}

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
  output$horizon_plot<-renderPlot({
    dat<-ddply(na.omit(melted_dataset()),.(variable),transform,roll=rollapply(value,12,FUN=function (x) {tail(cumprod(na.omit(x) + 1), 1) - 1},fill=NA,align="right"))
    dat<-dat[,-3]    
    print(horizon.panel.ggplot(dat,"Horizon Plot: 12 Month Rolling Returns"))
  })
  
  output$horizon_plot2<-renderPlot({
    dat<-ddply(na.omit(melted_dataset()),.(variable),transform,roll=rollapply(value,36,FUN=function (x) {tail(cumprod(na.omit(x) + 1), 1) - 1},fill=NA,align="right"))
    dat<-dat[,-3]  
    print(horizon.panel.ggplot(dat,"Horizon Plot: 36 Month Rolling Returns"))
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