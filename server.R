setwd("/Users/linhao/GitHub/Shiny-One");
library(shiny)
source("functions2.R")

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

    
  output$plotpH <- renderPlot({
    #plot(cars, type=input$Parameters)
    if(!("Dual" %in% input$Parameters)){
      s_plot(pH1, pH2, "pH1", "pH2")
    }
    else 
      d_plot(pH1, pH2, "pH1&pH2")
  })
  
  output$plotTempC <- renderPlot({
    #plot(cars, type=input$Parameters)
    if(!("Dual" %in% input$Parameters)){
      s_plot(TempC1, TempC2, "TempC1", "TempC2")
    }
    else 
      d_plot(TempC1, TempC2, "TempC1&TempC2")
  })
  
  output$plotCond <- renderPlot({
    #plot(cars, type=input$Parameters)
    if(!("Dual" %in% input$Parameters)){
      s_plot(Cond1, Cond2, "Cond1", "Cond2")
    }
    else 
      d_plot(Cond1, Cond2, "Cond1&Cond2")
  })
  
  output$plotTurbA <- renderPlot({
    #plot(cars, type=input$Parameters)
    if(!("Dual" %in% input$Parameters)){
      s_plot(TurbA1, TurbA2, "TurbA1", "TurbA2")
    }
    else 
      d_plot(TurbA1, TurbA2, "TurbA1&TurbA2")
  })
  
  output$plotTurbS <- renderPlot({
    #plot(cars, type=input$Parameters)
    if(!("Dual" %in% input$Parameters)){
      s_plot(TurbS1, TurbS2, "TurbS1", "TurbS2")
    }
    else 
      d_plot(TurbS1, TurbS2, "TurbS1&TurbS2")
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- renderDataTable({
    cars
  }, options=list(pageLength=10))
})