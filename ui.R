library(shiny)
library(markdown)
###
shinyUI(navbarPage("Linkage!",
                  tabPanel("Plot",       
                      fluidRow(
                              column(12,
                                checkboxGroupInput("Parameters", label = h3("Real Time Plotting"), 
                                choices = list("Dual-Sensor" = "Dual"),selected = ""),
                                selectInput("select", label = h3(""), 
                                            choices = list("10 Seconds" = 10000, "30 Seconds" = 30000, "60 Seconds" = 60000), 
                                            selected = 10000)
                              )
                            ),
                      fluidRow(
                        column(12,
                               wellPanel(plotOutput("plotpH"))
                        )
                      ),
                      fluidRow(
                        column(12,
                               wellPanel(plotOutput("plotTempC"))
                        )
                      ),
                      fluidRow(
                        column(12,
                               wellPanel(plotOutput("plotCond"))
                        )
                      ),
                      fluidRow(
                        column(12,
                               wellPanel(plotOutput("plotTurbA"))
                        )
                      ),
                      fluidRow(
                        column(12,
                               wellPanel(plotOutput("plotTurbS"))
                        )
                      )
                              
                              

                                #plotOutput("plotpH"),
                                #plotOutput("plotTempA"),
                                #plotOutput("plotTempC"),
                                #plotOutput("plotCond"),
                                #plotOutput("plotTurbA"),
                                #plotOutput("plotTurbS")   
                              
                            
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
