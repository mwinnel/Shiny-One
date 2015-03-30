setwd("C:/Users/s2783343/Documents/ARC/Shiny-One");
library(shiny)
source("functions2.R")

library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='localhost', port='5432', dbname='WQIAS',
                 user='postgres', password='griffith24')

## Submit and execute the query
event10 <- dbGetQuery(con, "SELECT * FROM events.event10_fingerprint")
event1 <- dbGetQuery(con, "SELECT * FROM events.event1_fingerprint")
event2 <- dbGetQuery(con, "SELECT * FROM events.event2")
event9 <- dbGetQuery(con, "SELECT * FROM events.event9")
event4 <- dbGetQuery(con, "SELECT * FROM events.event4")
event5 <- dbGetQuery(con, "SELECT * FROM events.event8")



event.info <- dbGetQuery(con, "SELECT * FROM events.event_fingerprint_info")
event.info[event.info$event_id == "Event 10",]


#Cond1 <- read.table("data/dataset_Cond1.dat",col.names=c("Date", "Time", "Cond", "MINUTES"))
#Cond2 <- read.table("data/dataset_Cond2.dat",col.names=c("Date", "Time", "Cond", "MINUTES"))
#pH1 <- read.table("data/dataset_pH1.dat",col.names=c("Date", "Time", "pH", "MINUTES"))
#pH2 <- read.table("data/dataset_pH2.dat",col.names=c("Date", "Time", "pH", "MINUTES"))
#TempA <- read.table("data/dataset_TempA1.dat",col.names=c("Date", "Time", "TempA", "MINUTES"))
#TempC1 <- read.table("data/dataset_TempC1.dat",col.names=c("Date", "Time", "TempC", "MINUTES"))
#TempC2 <- read.table("data/dataset_TempC2.dat",col.names=c("Date", "Time", "TempC", "MINUTES"))
#TurbA1 <- read.table("data/dataset_TurbA1.dat",col.names=c("Date", "Time", "TurbA", "MINUTES"))
#TurbA2 <- read.table("data/dataset_TurbA2.dat",col.names=c("Date", "Time", "TurbA", "MINUTES"))
#TurbS2 <- read.table("data/dataset_TurbS2.dat",col.names=c("Date", "Time", "TurbS", "MINUTES"))
#TurbS1 <- read.table("data/dataset_TurbS1.dat",col.names=c("Date", "Time", "TurbS", "MINUTES"))

shinyServer(
  
  function(input, output, session) {
    
    nodelist <- read.table(file="Nodelist",col.names=c("nodename","iP"),sep="\t")
    nodelist <- droplevels(nodelist)
    temp <- ""
    
    for(i in 1:dim(nodelist)[1]){
      temp <- paste(temp,'"', nodelist$nodename[i], '"', " =", i)
      if(i != dim(nodelist)[1])
        temp <- paste(temp, ",")
    }
    
    list <- list()
    
    temp <- paste("nodeList <- list(",temp,")")
   
    eval(parse(text=temp))

    Cond1 <- reactive({
      Cond <- reactiveFileReader(as.numeric(input$select), session, paste("data/",input$node,"/dataset_Cond1.csv",sep =""), read.csv)
      Cond1 <- Cond()
      })
    
    Cond2 <- reactive({
      Cond <- reactiveFileReader(as.numeric(input$select), session, paste("data/",input$node,"/dataset_Cond2.csv",sep =""), read.csv)
      Cond2 <- Cond()
    })
    
    pH1 <- reactive({
      pH <- reactiveFileReader(as.numeric(input$select), session, paste("data/",input$node,"/dataset_pH1.csv",sep =""), read.csv)
      pH1 <- pH()
    })
    
    pH2 <- reactive({
      pH <- reactiveFileReader(as.numeric(input$select), session, paste("data/",input$node,"/dataset_pH2.csv",sep =""), read.csv)
      pH2 <- pH()
    })
    
    TempC1 <- reactive({
      TempC <- reactiveFileReader(as.numeric(input$select), session, paste("data/",input$node,"/dataset_TempC1.csv",sep =""), read.csv)
      TempC1 <- TempC()
    })
    
    TempC2 <- reactive({
      TempC <- reactiveFileReader(as.numeric(input$select), session, paste("data/",input$node,"/dataset_TempC2.csv",sep =""), read.csv)
      TempC2 <- TempC()
    })
    
    TurbA1 <- reactive({
      TurbA <- reactiveFileReader(as.numeric(input$select), session, paste("data/",input$node,"/dataset_TurbA1.csv",sep =""), read.csv)
      TurbA1 <- TurbA()
    })
    
    TurbA2 <- reactive({
      TurbA <- reactiveFileReader(as.numeric(input$select), session, paste("data/",input$node,"/dataset_TurbA2.csv",sep =""), read.csv)
      TurbA2 <- TurbA()
    })
    
    TurbS1 <- reactive({
      TurbS <- reactiveFileReader(as.numeric(input$select), session, paste("data/",input$node,"/dataset_TurbS1.csv",sep =""), read.csv)
      TurbS1 <- TurbS()
    })
    
    TurbS2 <- reactive({
      TurbS <- reactiveFileReader(as.numeric(input$select), session, paste("data/",input$node,"/dataset_TurbS2.csv",sep =""), read.csv)
      TurbS2 <- TurbS()
    })
    
    TempA1 <- reactive({
      TempA <- reactiveFileReader(as.numeric(input$select), session, paste("data/",input$node,"/dataset_TempA1.csv",sep =""), read.csv)
      TempA1 <- TempA()
    })
    
  
  ### START SERVER. R  
    
  output$plotpH <- renderPlot({
    if(!("Dual" %in% input$Parameters)){
      s_plot(pH1(), pH2(), "pH1", "pH2")
    }
    else 
      d_plot(pH1(), pH2(), "pH1&pH2")
  })
  
  output$plotTempC <- renderPlot({
    if(!("Dual" %in% input$Parameters)){
      s_plot(TempC1(), TempC2(), "TempC1", "TempC2")
    }
    else 
      d_plot(TempC1(), TempC2(), "TempC1&TempC2")
  })
  
  output$plotCond <- renderPlot({
    if(!("Dual" %in% input$Parameters)){
      s_plot(Cond1(), Cond2(), "Cond1", "Cond2")
    }
    else 
      d_plot(Cond1(), Cond2(), "Cond1&Cond2")
  })
  
  output$plotTurbA <- renderPlot({
    if(!("Dual" %in% input$Parameters)){
      s_plot(TurbA1(), TurbA2(), "TurbA1", "TurbA2")
    }
    else 
      d_plot(TurbA1(), TurbA2(), "TurbA1&TurbA2")
  })
  
  output$plotTurbS <- renderPlot({
    if(!("Dual" %in% input$Parameters)){
      s_plot(TurbS1(), TurbS2(), "TurbS1", "TurbS2")
    }
    else 
      d_plot(TurbS1(), TurbS2(), "TurbS1&TurbS2")
  })
  
  ### END TAB 1
  
  
  ### START TAB 2
  
  
  output$plotEvent1 <- renderPlot({

    par(mfcol = c(2,2))
    plot(event1$MINUTES,event1$pH, type="l",ylab="pH")
    LabelTimeAxis()
    plot(event1$MINUTES,event1$Turb, type="l",ylab="Turb")
    LabelTimeAxis()
    plot(event1$MINUTES,event1$Temp, type="l",ylab="Temp")
    LabelTimeAxis()
    plot(event1$MINUTES,event1$Cond, type="l",ylab="Cond")
    LabelTimeAxis()


  })
  
  
  output$plotEvent10 <- renderPlot({
    
    par(mfcol = c(2,2))
    plot(event10$MINUTES,event10$pH, type="l",ylab="pH")
    LabelTimeAxis()
    plot(event10$MINUTES,event10$Turb, type="l",ylab="Turb")
    LabelTimeAxis()
    plot(event10$MINUTES,event10$Temp, type="l",ylab="Temp")
    LabelTimeAxis()
    plot(event10$MINUTES,event10$Cond, type="l",ylab="Cond")
    LabelTimeAxis()
    
    
  })
  
  output$plotEvent3 <- renderPlot({
    
    par(mfcol = c(2,2))
    plot(event9$MINUTES,event9$pH, type="l",ylab="pH")
    LabelTimeAxis()
    plot(event9$MINUTES,event9$Turb, type="l",ylab="Turb")
    LabelTimeAxis()
    plot(event9$MINUTES,event9$Temp, type="l",ylab="Temp")
    LabelTimeAxis()
    plot(event9$MINUTES,event9$Cond, type="l",ylab="Cond")
    LabelTimeAxis()
    
    
  })
  
  output$plotEvent4 <- renderPlot({
    
    par(mfcol = c(2,2))
    plot(event4$MINUTES,event4$pH, type="l",ylab="pH")
    LabelTimeAxis()
    plot(event4$MINUTES,event4$Turb, type="l",ylab="Turb")
    LabelTimeAxis()
    plot(event4$MINUTES,event4$Temp, type="l",ylab="Temp")
    LabelTimeAxis()
    plot(event4$MINUTES,event4$Cond, type="l",ylab="Cond")
    LabelTimeAxis()
    
    
  })
  
  output$plotEvent5 <- renderPlot({
    
    par(mfcol = c(2,2))
    plot(event2$MINUTES,event2$pH, type="l",ylab="pH")
    LabelTimeAxis()
    plot(event2$MINUTES,event2$Turb, type="l",ylab="Turb")
    LabelTimeAxis()
    plot(event2$MINUTES,event2$Temp, type="l",ylab="Temp")
    LabelTimeAxis()
    plot(event2$MINUTES,event2$Cond, type="l",ylab="Cond")
    LabelTimeAxis()
    
    
  })
  
  output$summary <- renderPrint({
    summary(event10)
  })
  
  output$table <- renderDataTable({
    event.info
  }, 
  
  options=list(pageLength=10))

  
}) #END FUNCTION # END SHINY

