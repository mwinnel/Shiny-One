
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
