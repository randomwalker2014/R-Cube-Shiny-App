
# Load libraries ---------------------------------------------------------------

library(shiny)
library(markdown)
library(devtools)
library(shinyTree)


shinyUI(
  
  navbarPage(id = "navBar",title=div(class="div-logo",img(src="logo.png"),'R Cube'),inverse = TRUE,
             collapsable = TRUE,windowTitle = "R Cube",
                   
                   # First tab --------------------------------------------------------------------                   
                   
                   tabPanel('Data Explorer', icon = icon("search"),
                            
                            includeCSS('style.css'), 
                            
                            
                            absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = FALSE,
                                          top = "auto", left = "auto", right = 230, bottom = "auto",
                                          width = 250, height = "auto",
 
                                          h4(icon("table"),strong("Cubes")),
                                          
                                          
                                          tags$style(type="text/css", "label { color: #CC7722; display:inline-block; }"),
                                          selectInput("cubeChoice", label=strong("OLAP Cube:"),
                                                      c("Select","Retail Store" = "store"),
                                                      selected="Select"),
                                          
                                          h4(strong(icon("gears"),"Dimensions")),
                                          wellPanel(conditionalPanel(
                                            condition = "input.cubeChoice != 'Select'",
                                            shinyTree(outputId = "tree_dimension",selected = "dNodeSelect", checkbox = TRUE)
                                          )),
                                          
                                          h4(icon("tachometer"),strong("Measures")),
                                          wellPanel(conditionalPanel(
                                            condition = "input.cubeChoice != 'Select'",
                                            shinyTree(outputId = "tree_measure", selected = "mNodeSelect", checkbox = TRUE)
                                          )
                                                          
                            )),
                            absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = FALSE,
                                          top = "auto", left = "auto", right = 10, bottom = "auto",
                                          width = 170, height = "auto",
                                          
                                          h4(icon("filter"),strong("Filters")),
                                         
                                          wellPanel(
                                                     uiOutput("region"),
                                                     uiOutput("state"),
                                                     uiOutput("city"),
                                                     uiOutput("productName"),
                                                     uiOutput("productCategory"),
                                                     uiOutput("storeId"),
                                                     uiOutput("storeSize"),
                                                     uiOutput("year"),
                                                     uiOutput("month"),
                                                     uiOutput("quarter")

                                          )     
                            ),
                            
                            # Main panel on first Tab------------------------------------------------------------------- 
                            mainPanel(

                              conditionalPanel(
                                condition =   "input.dNodeSelect == null || input.mNodeSelect == null",
                                absolutePanel(id = "intro-controls", class = "modal",fixed = TRUE, draggable = FALSE,
                                              top = "auto", left = 300, right = "auto", bottom = "auto",
                                              width = 925, height = 625,
                                              HTML('<p><img src="big_logo.png"/></p>'),
                                              includeMarkdown('intro.md')
                                )),
                              conditionalPanel(
                                condition = "input.cubeChoice != 'Select' 
                                             && input.dNodeSelect != null 
                                             && input.mNodeSelect != null",
                                absolutePanel(id = "controls", class = "modal",fixed = TRUE, draggable = FALSE,
                                              top = "auto", left = 350, right = "auto", bottom = "auto",
                                              width = 925, height = 375,
                                              style = "background-color: #ffffff;border: 1px grey;",

                                  plotOutput("plot",height=375)
                                )),
                              
                              conditionalPanel(
                                condition = "input.cubeChoice != 'Select' 
                                             && input.dNodeSelect != null 
                                             && input.mNodeSelect != null",
                              absolutePanel(id = "controls", class = "modal",fixed = TRUE, draggable = FALSE,
                                            top = 480, left = 350, right = "auto", bottom = "auto",
                                            width = 925, height = 300,
                               dataTableOutput('table'),
                               style = "background-color: #ffffff; border: 1px grey;font-size:14px;font-color:black;font-weight:500"
                               
                              ))                             
                              
                            )
                         ),
             
                   
                   # Second tab -------------------------------------------------------------------  
                   
                   tabPanel('Info', icon = icon("info-circle"),
                            mainPanel(
                                     absolutePanel(id = "intro-controls", class = "modal",fixed = TRUE, draggable = FALSE,
                                                   top = "auto", left = 300, right = "auto", bottom = "auto",
                                                   width = 925, height = 625,
                                                   HTML('<p><img src="big_logo.png"/></p>'),
                                                   includeMarkdown('info.md'))
                                                   
                              
                            )
                   ),
                  # Third tab ------------------------------------------------------------------- 
                  tabPanel('About', icon = icon("list-alt"),
                            mainPanel(
                               absolutePanel(id = "intro-controls", class = "modal",fixed = TRUE, draggable = FALSE,
                                             top = "auto", left = 300, right = "auto", bottom = "auto",
                                             width = 925, height = 625,
                                             HTML('<p><img src="ddp.png"/></p>'),
                                             includeMarkdown('about.md'))
                        
                        
                            )
                  )
             
           ))
