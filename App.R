# Shiny App #

if (!require("shiny")) install.packages("shiny")
if (!require("ggplot2")) install.packages("ggplot2")

library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  tabsetPanel(
  tabPanel(title = "Genereate plot",
  tags$h3("Cost-effectiveness acceptability curve app", align = "center", tags$hr()),
  
  fileInput("inputdata", label = "Input CSV file", accept = c(".csv")),
  
 wellPanel(fluidRow(
    column(3, numericInput("thresholdvalue", "Cost-effectiveness threshold", value = 0)),
    column(3, offset=1, numericInput("thresholdvalue2", "Second threshold (if range)", value = 0)),
    column(4, offset=1, textInput("currency", label = "Currency (for X-axis label)"))
  )),
  
  wellPanel(fluidRow(
    column(4, radioButtons(inputId = "linestyle", label = "Linestyle", choices = list("Linetype" = 1, "Colour" = 2),
                                                                                selected = 1)),
    column(4, checkboxInput("legendoption", "Include Legend", value=FALSE))
  )),
  
  #downloadButton('downloadplot', 'Inactive Download Plot Button'),
  #tags$hr(),
  fluidRow(column(12, plotOutput(outputId = "graph"), align = 'center'))
  
),

tabPanel(title = "Input instructions", 
         tags$h3("Description of input data"),tags$hr(),
         fluidRow(column(8,wellPanel(
         tags$h5("The CSV input data file should have three columns; 
                 the comparator name, the willingness to pay threshold (without currency characters), 
                 and the probability of cost-effectiveness"),tags$br(),
         tags$h5("The input data should be structured similar to the table below, 
                 but without the column headings in the CSV file (this is shown for demonstration only)."),tags$br()))),
        tableOutput("exampletable")
 )
)
)

# CODE

gen_ceac = function(psa, label, legend, threshold, threshold2, line) {
  
  colnames(psa) = c("variable", "WTP", "value")
  wtp.label <- paste("Willingness to pay (", label, ")", sep="")

    z = ggplot(psa) + 
    labs(x = wtp.label, text = element_text(size=15)) + labs (y = "Probability cost-effective", text = element_text(size=10)) + theme_classic() +
    theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
          axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.key.width=unit(3.8,"line"), text = element_text(size=15), 
          plot.margin=unit(c(1,1,1,1),"cm")) + 
    scale_x_continuous(labels = scales::comma, expand = c(0, 0.1)) + 
    scale_y_continuous(limits = c(0,1), breaks=seq(0,1,0.1), expand = c(0, 0)) + 
    geom_vline(xintercept = threshold, linetype="dotted", size=0.5) + 
    geom_vline(xintercept = threshold2, linetype="dotted", size=0.5)
  
  if(line==1) z = z + geom_line(aes(x=WTP, y=value, linetype=variable), size=1.1)  
  if(line==2) z = z + geom_line(aes(x=WTP, y=value, colour=variable), size=1.1)
    
  if(legend==FALSE) z <- z + theme(legend.position = "none")
    
  z
  
}

matrix <- matrix(c("Treatment 1","Treatment 1","Treatment 1","Treatment 1",
                   0,1000,2000,3000,
                   0,0.01,0.04,0.09),
                 nrow = 4,
                 ncol = 3)
colnames(matrix) <- c("Comparator","Willingness to pay","Probability cost-effective")


## END 


server <- function(input, output) {
  output$graph <- renderPlot({
    
    d <- input$inputdata
    
    if (is.null(d))
      return(NULL)
    
    z <- read.csv(d$datapath)
    
    threshold = 0
    
    gen_ceac(z, input$currency, input$legendoption, input$thresholdvalue, input$thresholdvalue2, input$linestyle) 

    
}, height = 400, width = 700)
  
  output$exampletable <- renderTable(matrix)
    
}

shinyApp(ui = ui, server = server) 


