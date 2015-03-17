setwd("C:/rcode/Shiny-One");
library(shiny)
source("functions2.R")
#source("readNodeList.r")

Cond1 <- read.table("data/dataset_Cond1.dat",col.names=c("Date", "Time", "Cond", "MINUTES"))
Cond2 <- read.table("data/dataset_Cond2.dat",col.names=c("Date", "Time", "Cond", "MINUTES"))
pH1 <- read.table("data/dataset_pH1.dat",col.names=c("Date", "Time", "pH", "MINUTES"))
pH2 <- read.table("data/dataset_pH2.dat",col.names=c("Date", "Time", "pH", "MINUTES"))
TempA <- read.table("data/dataset_TempA1.dat",col.names=c("Date", "Time", "TempA", "MINUTES"))
TempC1 <- read.table("data/dataset_TempC1.dat",col.names=c("Date", "Time", "TempC", "MINUTES"))
TempC2 <- read.table("data/dataset_TempC2.dat",col.names=c("Date", "Time", "TempC", "MINUTES"))
TurbA1 <- read.table("data/dataset_TurbA1.dat",col.names=c("Date", "Time", "TurbA", "MINUTES"))
TurbA2 <- read.table("data/dataset_TurbA2.dat",col.names=c("Date", "Time", "TurbA", "MINUTES"))
TurbS2 <- read.table("data/dataset_TurbS2.dat",col.names=c("Date", "Time", "TurbS", "MINUTES"))
TurbS1 <- read.table("data/dataset_TurbS1.dat",col.names=c("Date", "Time", "TurbS", "MINUTES"))

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
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- renderDataTable({
    cars
  }, options=list(pageLength=10))
})