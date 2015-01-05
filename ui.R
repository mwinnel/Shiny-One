library(shiny)
library(markdown)

shinyUI(navbarPage("Linkage!",
                   tabPanel("Plot",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("plotType", "Plot type",
                                             c("Scatter"="p", "Line"="l")
                                )
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
                                         column(6,
                                                includeMarkdown("about.md")
                                         )
                                         
                                       )
                              )
                   )
))
