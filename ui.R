library(shiny)
library(markdown)
source("readNodeList.r")
###
shinyUI(navbarPage("WQAIS",
                  tabPanel("Plot",
                           sidebarLayout(
                             
                             sidebarPanel(
                                      
                               
                                checkboxGroupInput("Parameters", label = h3("Ploting Setting"), 
                                                         choices = list("Dual-Sensor" = "Dual"),selected = ""),
                                      selectInput("select", label = h4("Refresh Intervals"), 
                                                  choices = list("10 Seconds" = 10000, "30 Seconds" = 30000, "60 Seconds" = 60000), 
                                                  selected = 10000),
                                      selectInput("node", label = h4("Unit Selection"), 
                                                  choices = nodeList, 
                                                  selected = 1),
                                      width = 3
                               ),
                             
                             
                             
                             mainPanel(
                               
                               titlePanel("Real Time Plotting"),
                               
                               
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
                               ),
                               width = 9
                               )
                             )
                              
                   ),
                  
                  
                  
                  
                   tabPanel("History",
                            fluidRow(
                              
                              column(4,
                                     h3("Buttons"),
                                     actionButton("action", label = "Action"),
                                     br(),
                                     br(), 
                                     submitButton("Submit")),
                              
                              column(4, 
                                     dateInput("date", 
                                               label = h3("Date Start"), 
                                               value = "2014-01-01"))   
                            ,
                              
                              column(4, 
                                     dateInput("date", 
                                               label = h3("Date End"), 
                                               value = "2014-01-01"))   
                            ),
                            
                            
                            verbatimTextOutput("history")
                            
                            
                            
                   ),
                  
                  
                  
                  
                   tabPanel("Alarms",
                            
                            titlePanel("Recent Events"),
                            fluidRow(
                              column(12,
                                     wellPanel(plotOutput("plotEvent1"))
                              )
                            ),
          
                            fluidRow(
                              column(10,
                                     wellPanel(plotOutput("plotEvent10"))
                              )
                              
                            ),
                            
                            fluidRow(
                              column(12,
                                     wellPanel(plotOutput("plotEvent3"))
                              )
                            ),
                            fluidRow(
                              column(12,
                                     wellPanel(plotOutput("plotEvent4"))
                              )
                            ),
                            fluidRow(
                              column(12,
                                     wellPanel(plotOutput("plotEvent5"))
                              )
                            ),
                            
                            verbatimTextOutput("events")
                   ),
                  
                  
                  
                   tabPanel("Events Summary",
                            dataTableOutput("table")
                   ),
                  
                  
    
                   navbarMenu("More",
                              tabPanel("Table",verbatimTextOutput("alarms")
                                       
                              ),
                              tabPanel("About",
                                       fluidRow(
                                         
                                         
                                       )
                              )
                   )
))
