library(shiny)
library(markdown)
###
shinyUI(navbarPage("Linkage!",
                   tabPanel("Plot",
                            sidebarLayout(
                              sidebarPanel(
                                #radioButtons("Parameters", "Parameters",
                                #             c("pH"="p", "TempA"="l", "TempC"="p", "Cond"="p", "TurbA"="l", "TurbS"="p", "Dual-Sensor"="p")
                                #)
                                checkboxGroupInput("Parameters", label = h3("Parameters"), 
                                                   choices = list("pH" = "pH", "TempA" = "TempA", "TempC" = "TempC", "Cond" = "Cond", "TurbA" = "TurbA", "TurbS" = "TurbS", "Dual-Sensor" = "Dual"),
                                                   selected = "pH")
                              ),
                              mainPanel(
                                plotOutput("plot")
                              )
                            )
                   ),
                   tabPanel("History",
                            verbatimTextOutput("history")
                   ),
                   tabPanel("Events",
                            verbatimTextOutput("events")
                   ),
                   tabPanel("Alarms",
                            verbatimTextOutput("alarms")
                   ),
                   navbarMenu("More",
                              tabPanel("Table",
                                       dataTableOutput("table")
                              ),
                              tabPanel("About",
                                       fluidRow(
                                         
                                         
                                       )
                              )
                   )
))
