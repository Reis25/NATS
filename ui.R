if(!require(shiny)) install.packages("shiny")
if(!require(shinyWidgets)) install.packages("shinyWidgets")
if(!require(shinythemes)) install.packages("shinythemes")
if(!require("DT")) install.packages('DT')
if(!require(htmltools)) install.packages("htmltools")
if(!require(bsplus)) install.packages("bsplus")
#if(!require("rsconnect")) install.packages('rsconnect')
#deployApp()

#series = array(list(), 9)
#series[1] <- read.csv("Data/Series fk and logistics maps/series_fk_0.csv", stringsAsFactors = FALSE)
#series[2] <- read.csv("Data/Series fk and logistics maps/series_fk_0_5.csv", stringsAsFactors = FALSE)
#series[3] <- read.csv("Data/Series fk and logistics maps/series_fk_1_5.csv", stringsAsFactors = FALSE)
#series[4] <- read.csv("Data/Series fk and logistics maps/series_fk_1.csv", stringsAsFactors = FALSE)
#series[5] <- read.csv("Data/Series fk and logistics maps/series_fk_2_5.csv", stringsAsFactors = FALSE)
#series[6] <- read.csv("Data/Series fk and logistics maps/series_fk_2.csv", stringsAsFactors = FALSE)
#series[7] <- read.csv("Data/Series fk and logistics maps/series_fk_3.csv", stringsAsFactors = FALSE)
#series[8] <- read.csv("Data/Series fk and logistics maps/series_map_3_6.csv", stringsAsFactors = FALSE)
#series[9] <- read.csv("Data/Series fk and logistics maps/series_map_4.csv", stringsAsFactors = FALSE)



# tweaks, a list object to set up multicols for checkboxGroupInput
tweaks <- list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 150px;
                                   -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 5;    /* Firefox */ 
                                   column-count: 5; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
  ))

shinyUI(
  fluidPage(theme = shinytheme("flatly"),
            
            tags$head(tags$title("NATS"),
                      tags$link(rel = "stylesheet", type = "text/css", href = "www/bootstrap.css")),
            
            navbarPage("NATS",
                       
                tabPanel("Data Analysis",
                     fluidPage(
                                  
                        fluidRow(
                        column(12, 
                                           
                           fluidRow(
                           column(4,
                                  
                              #fluidRow(
                              #column(12, 
                              #       h5(strong("SERIES")))),
                                  
                              #fluidRow(
                              #column(12, 
                              #       h5(strong("")),
                              #       pickerInput("series_example", "",
                              #                   choices = list('Noises with f-k power spectrum' = c("Series fk: k = 0" = "k0", "Series fk: k = 0.5" = "k05", "Series fk: k = 1" = "k1", "Series fk: k = 1.5" = "k15", "Series fk: k = 2" = "k2", "Series fk: k = 2.5" = "k25", "Series fk: k = 3" = "k3"),
                              #                                  'Logistic Map' = list("Logistic map: r = 3.6, x_0 = 0.1" = "lm36", "Logistic map: r = 4, x_0 = 0.1" = "lm4")),
                              #                   options = list('actions-box' = TRUE, 'none-selected-text' = "Please make a selection!"),
                              #                   selected = c("k0"),
                              #                   multiple = TRUE,
                              #                   width = '100%'))),
                                                    
                              fluidRow(
                              column(12, 
                                     h5(strong("CHOOSE CSV FILE")))),
                                                    
                              fluidRow(
                              column(6,
                                     fileInput("file1", " ", accept = c("text/csv", "text/comma-separated-values","text/plain", ".csv"), multiple = T)),
                                                      
                              column(6, 
                                    selectInput(inputId = 'sep', label = 'Separator', c(Point = '.', Comma=',',Semicolon=';',Tab='\t',Space=' '),' '))),
                              fluidRow(
                                column(12,
                                       actionButton(inputId = "reload", label = "Reload data", width = '100%'))),
                                                    
                              tags$hr(),
                              h5(strong("PROBABILITY DISTRIBUTION")),
                                 selectInput(inputId = "distribution", "",
                                             list('Ordinal Patterns' = list("Bandt-Pompe", "Bandt-Pompe Weight"),
                                                  'Transition Graphs' = list("Ordinal patterns transition graphs", "Weighted Ordinal Patterns Transition Graph")),
                                                   width = '100%'),
                                                  
                              tags$hr(),
                              fluidRow(
                              column(12,
                                     h5(strong("PARAMETERS OF ORDINAL PATTERNS")))),
                              fluidRow(
                              column(6, "", br(),
                                    sliderInput("dimension", "Dimension:", min = 3, max = 6, value = 3)),
                              column(6, "", br(),
                                    sliderInput("delay", "Delay:", min = 1, max = 6, value = 1))),
                           
                           tags$hr(), 
                           br(), br(),  
                           style = "background-color:#EBF1F1;"),
                                             
                           column(8, 
                                                    
                               tabsetPanel(id = "resultsTab", type = "tabs",
                                                              
                                           tabPanel(title = HTML("Time Series and <br/> Ordinal Patterns"), id = "plotTab",
                                                    fluidRow(
                                                      column(3,
                                                             br(), br(), 
                                                             textInput(inputId = "pattern", "Select Pattern:",
                                                                         list("0"),
                                                                         width = '125px') %>%
                                                               shinyInput_label_embed(
                                                                 shiny_iconlink() %>%
                                                                   bs_embed_popover(
                                                                     title = "0 for no pattern or the chronological index of the ordinal pattern chosen", content = "Number of pattern for analysis", placement = "left"
                                                                   )
                                                               )),
                                                      column(9,
                                                             br(), br(), 
                                                             
                                                             selectInput(inputId = "chosen.series", label = "Series to be analyzed:", choices = list(empty = ""), multiple = FALSE, width = '165px')
                                                             #%>% shinyInput_label_embed(
                                                            #     shiny_iconlink() %>%
                                                            #       bs_embed_popover(
                                                            #         title = "Choose the series to be analyzed by its index", content = "Index of time series", placement = "left"
                                                            #       ))
                                                             )
                                                      
                                                      ),
                                                    fluidRow(
                                                      column(6,
                                                             downloadButton(outputId = "downloadTimeSeries", label = "Save figure"),
                                                             plotOutput("timeSeriesPlot", dblclick = "plot1_dblclick",
                                                                        brush = brushOpts(
                                                                          id = "plot1_brush",
                                                                          resetOnNew = TRUE
                                                                        ))),
                                                      column(6,
                                                             downloadButton(outputId = "downloadHistogram", label = "Save figure"),
                                                             plotOutput("HistogramPlot")))),
                                                                
                                           tabPanel(tweaks,title = HTML("Entropy and <br/> Statistical Complexity"), id = "tableTab",
                                                    br(),
                                                    
                                                     tags$div(align = 'left', 
                                                              class = 'multicol', 
                                                              checkboxGroupInput(inputId = "EntropyForm",
                                                                           label = "Select the descriptors:",
                                                                           choices = c("Shannon Entropy" = "Shannon", "Tsallis Entropy" = "Tsallis", "Renyi Entropy" = "Renyi", 
                                                                                       "Fisher Entropy" = "Fisher", "Permutation Min Entropy (PmE)" = "PmE", "Statistical Complexity (SC)" = "SC"),
                                                                           selected = c("Shannon"),
                                                                           inline = F)),
                                                    
                                                    textInput("q1", "q parameter:", "0.01", width = '100px'),
                                                    tags$hr(),
                                                    
                                                    downloadButton(outputId = "downloadEntropy", label = "Download table"),
                                                    
                                                    br(), br(), br(), 
                                                    dataTableOutput("chooseEntropy"),
                                                    br(), br()),
                                                                
                                           tabPanel(title = HTML("Stochastic Distances <br/> and Divergences"), id = "tableTab1",
                                                    br(),
                                                    
                                                    tags$div(align = 'left', 
                                                             class = 'multicol', 
                                                             checkboxGroupInput(inputId = "DistanceForm",
                                                                                label = "Select the descriptors:",
                                                                                choices = c("Euclidian Distance" = "Euclidian", "Quadratic Euclidian Distance" = "Quadratic", "Manhattan Distance" = "Manhattan",
                                                                                            "Chebyshev Distance" = "Chebyshev", "Hellinger Distance" = "Hellinger", "Jensen Divergence" = "Jensen", 
                                                                                            "Wootter Distance" = "Wootter", "Kullback Leibler Divergence" = "Kullback"),
                                                                                selected = c("Euclidian"),
                                                                                inline = F)),
                                                    tags$hr(),
                                                    
                                                    downloadButton(outputId = "downloadDistance", label = "Download table"),
                                                    
                                                    br(), br(), br(),
                                                    dataTableOutput("chooseDistance"),
                                                    br(), br()),
                                                                
                                           tabPanel(title = HTML("Plane <br/> Analysis"), id = "plotTab1",
                                                    fluidRow(
                                                      column(6, br(),
                                                             selectInput(inputId = "plotId", "Select plane:",list("Entropy-Complexity" = "HC", "Fisher-Shannon" = "FS"), width = '400px')),
                                                      column(6,
                                                             br(), br(), 
                                                             downloadButton(outputId = "downloadPlane", label = "Save figure", width = '400px'),
                                                             br(), br()),
                                                      
                                                      plotOutput("HC", dblclick = "plot2_dblclick",
                                                                 brush = brushOpts(
                                                                   id = "plot2_brush",
                                                                   resetOnNew = TRUE
                                                                 ))
                                                    ))))
                                  
                        ))))),
                       
                       tabPanel("About",
                                navlistPanel(widths=c(2,10),
                                             tabPanel("What does NATS do?",
                                                      wellPanel(includeHTML("www/nats_do.html"))),
                                             tabPanel("What does NATS need?",
                                                      wellPanel(includeHTML("www/nats_need.html"))),
                                             tabPanel("What does NATS provide?",
                                                      wellPanel(includeHTML("www/nats_provide.html")))
                                )
                       )
              )
      )
)



