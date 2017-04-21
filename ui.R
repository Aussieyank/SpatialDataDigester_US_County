
navbarPage(div(style='font-size: 20px;', "Geospatial Data Digester -- US County v0.8"),
           id="nav",
           collapsible = TRUE, # small-screen friendly menu bar (good for mobile devices)
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),     # had to make change in styles.css -- zoom: from 0.9 to 1, otherwise slider is bugged   CS 04/2017
                          includeScript("gomap.js")
                        ),
                        
                        tags$style(type = "text/css",
                                   ".radio label {font-size: 12px;}
                                   "),
             
                        leafletOutput("map", width="100%", height="100%"),
                        verbatimTextOutput("out"),
                        

                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 500, height = "auto",
                                      
                                      h2("Pick and Choose"),
                                      
                                      # filter_slider("test_filter"),
                                      
                                      # ==================================
                                      fluidRow(
                                        column(5, checkboxInput("ck1", "Air Quality", value = FALSE, width = NULL)),
                                        conditionalPanel(condition = "input.ck1",
                                                         column(3,numericInput("w1", div(style='font-size: 12px;', "wt"), 20, min = 1, max = 100)),
                                                         div(style="height: 1px;",
                                                             column(4,sliderInput("sl1", div(style='font-size: 10px;', "PM 2.5 µg/m³:"), min = MINs[1], max = MAXs[1], value = c(MINs[1],MAXs[1]), 
                                                                                  step = 0.1, ticks=FALSE)))
                                        ) # end of conditionalPanel
                                      ),  # end of fluidRow 1
                                      
                                      fluidRow(
                                        column(3, checkboxInput("ck2", "VoteDiff", value = FALSE, width = NULL)),
                                        conditionalPanel(condition = "input.ck2",
                                                         column(2, radioButtons("radio2", label = NULL,
                                                                                choices = list("prefer dem" = 1, "prefer gop" = 2), 
                                                                                selected = 1)),
                                                         column(3,numericInput("w2", div(style='font-size: 12px;', "wt"), 20, min = 1, max = 100)),
                                                         column(4,sliderInput("sl2", div(style='font-size: 10px;', "dem% - gop%"), min = MINs[2], max = MAXs[2], value = c(MINs[2],MAXs[2]), ticks=FALSE))
                                        ) # end of conditionalPanel
                                      ),  # end of fluidRow 2
                                      
                                      
                                      fluidRow(
                                        column(5, checkboxInput("ck3", "Income", value = FALSE, width = NULL)),
                                        conditionalPanel(condition = "input.ck3",
                                                         column(3,numericInput("w3", div(style='font-size: 12px;', "wt"), 20, min = 1, max = 100)),
                                                         column(4,sliderInput("sl3", div(style='font-size: 10px;', "$/year"), min = MINs[3], max = MAXs[3], value = c(MINs[3],MAXs[3]),ticks=FALSE))
                                        ) # end of conditionalPanel
                                      ),  # end of fluidRow 3
                                      
                                      fluidRow(
                                        column(5, checkboxInput("ck4", "Cost of Living", value = FALSE, width = NULL)),
                                        conditionalPanel(condition = "input.ck4",
                                                         column(3,numericInput("w4", div(style='font-size: 12px;', "wt"), 20, min = 1, max = 100)),
                                                         column(4,sliderInput("sl4", div(style='font-size: 10px;', "$/year"), min = MINs[4], max = MAXs[4], value = c(MINs[4],MAXs[4]),ticks=FALSE))
                                        ) # end of conditionalPanel
                                      ),  # end of fluidRow 4
                                      
                                      fluidRow(
                                        column(5, checkboxInput("ck5", "Income Cost Ratio", value = FALSE, width = NULL)),
                                        conditionalPanel(condition = "input.ck5",
                                                         column(3,numericInput("w5", div(style='font-size: 12px;', "wt"), 20, min = 1, max = 100)),
                                                         column(4,sliderInput("sl5", div(style='font-size: 10px;', "$/year"), min = MINs[5], max = MAXs[5], value = c(MINs[5],MAXs[5]),ticks=FALSE))
                                        ) # end of conditionalPanel
                                      ),  # end of fluidRow 5
                                      
                                      fluidRow(
                                        column(5, checkboxInput("ck6", "Crime Rate", value = FALSE, width = NULL)),
                                        conditionalPanel(condition = "input.ck6",
                                                         column(3,numericInput("w6", div(style='font-size: 12px;', "wt"), 20, min = 1, max = 100)),
                                                         column(4,sliderInput("sl6", div(style='font-size: 10px;', "per 100k people"), min = MINs[6], max = MAXs[6], value = c(MINs[6],MAXs[6]),ticks=FALSE))
                                        ) # end of conditionalPanel
                                      ),  # end of fluidRow 6
                                      
                                      fluidRow(
                                        column(3, checkboxInput("ck7", "Population Density", value = FALSE, width = NULL)),
                                        conditionalPanel(condition = "input.ck7",
                                                         column(2, radioButtons("radio7", label = NULL,
                                                                                choices = list("love the crowd" = 1, "gimme some space" = 2), 
                                                                                selected = 1)),
                                                         column(3,numericInput("w7", div(style='font-size: 12px;', "wt"), 20, min = 1, max = 100)),
                                                         column(4,sliderInput("sl7", div(style='font-size: 10px;', ""), min = MINs[7], max = MAXs[7], value = c(MINs[7],MAXs[7]),ticks=FALSE))
                                        ) # end of conditionalPanel
                                      ),  # end of fluidRow 7
                                      
                                      
                                      
                                      
                                      
                                      
                                      fluidRow(
                                        column(3, checkboxInput("ck100", "Dist to Mouse Click", value = FALSE, width = NULL)),
                                        conditionalPanel(condition = "input.ck100",
                                                         column(2, radioButtons("radio100", label = NULL,
                                                                                choices = list("close to" = 1, "away from" = 2), 
                                                                                selected = 1)),
                                                         column(3,numericInput("w100", div(style='font-size: 12px;', "wt"), 20, min = 1, max = 100)),
                                                         column(4, verbatimTextOutput('click_lat_lon'))

                                                         
                                                         # column(4,sliderInput("sl100", div(style='font-size: 10px;', ""), min = MINs[100], max = MAXs[100], value = c(MINs[100],MAXs[100]),ticks=FALSE))

                                        ) # end of conditionalPanel
                                      ),  # end of fluidRow 
                                      
                                      
                                      
                                      
                                      # action button -- user triggers a plot refresh when all changes are done
                                      actionButton("do", "Update Plots",icon("refresh"), width="100%")
                                      
                                      # # This is where the pie chart comes in -- below the action button
                                      # htmlOutput("pie")
                                      
                                      # uiOutput("radar")


                                      

                        ),  # end of absolutePanel "controls"
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 705, left = "auto", right = 1038, bottom = "auto",
                                      width = 500, height = "auto",
                                      
                                      # This is where the pie chart comes in -- below the action button
                                      # htmlOutput("pie"),
                                      
                                      
                                      # fluidRow(
                                        
                                        # chartJSRadarOutput("radar", height = "300")
                                        # column(7,uiOutput("radar")),
                                      uiOutput("radar")
                                        # This is where the pie chart comes in -- below the action button
                                        # column(5,htmlOutput("pie"))
                                      # )

                        ),  # end of absolutePanel "keyplots"
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 720, left = "auto", right = 650, bottom = "auto",
                                      width = 300, height = "auto",

                                      # This is where the pie chart comes in -- below the action button
                                      htmlOutput("pie")

                                      # chartJSRadarOutput("radar", height = "300")

                        ),  # end of absolutePanel "keyplots"
                                      
                        tags$div(id="cite",
                                 'AAA', tags$em('BBB'), ' CCC'
                        )
                        
                    ) # end of div
           ),
           
           tabPanel("Data explorer",
                    DT::dataTableOutput("leafmaptable")
           ), # end of tabPanel
           
           conditionalPanel("false", icon("crosshair"))
)
