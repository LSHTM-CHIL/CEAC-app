# Shiny App #

if (!require("shiny")) install.packages("shiny")
if (!require("ggplot2")) install.packages("ggplot2")

library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  tags$h3("Cost-effectiveness acceptability curve app", align = "center", tags$hr()),
  
  fileInput("inputdata", label = "Input CSV file", accept = c(".csv")),
  
  fluidRow(
    column(3, numericInput("thresholdvalue", "Cost-effectiveness threshold", value = 0)),
    column(3, offset=1, numericInput("thresholdvalue2", "Second threshold (if range)", value = 0)),
    column(4, offset=1, textInput("currency", label = "Currency (for X-axis label)"))
  ),
  tags$br(),
  
  fluidRow(
    column(4, radioButtons(inputId = "linestyle", label = "Linestyle", choices = list("Linetype" = 1, "Colour" = 2),
                                                                                selected = 1)),
    column(4, checkboxInput("legendoption", "Include Legend", value=FALSE))
  ),
  
  #downloadButton('downloadplot', 'Inactive Download Plot Button'),
  tags$hr(),
  tags$br(),
  fluidRow(column(12, plotOutput(outputId = "graph"), align = 'center'))
  
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
          plot.margin=unit(c(1.5,1.5,1,1),"cm")) + 
    scale_x_continuous(labels = scales::comma, expand = c(0, 0.1)) + 
    scale_y_continuous(limits = c(0,1), breaks=seq(0,1,0.1), expand = c(0, 0)) + 
    geom_vline(xintercept = threshold, linetype="dotted", size=0.5) + 
    geom_vline(xintercept = threshold2, linetype="dotted", size=0.5)
  
  if(line==1) z = z + geom_line(aes(x=WTP, y=value, linetype=variable), size=1.2)  
  if(line==2) z = z + geom_line(aes(x=WTP, y=value, colour=variable), size=1.2)
    
  if(legend==FALSE) z <- z + theme(legend.position = "none")
    
  z
  
}

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
  
}

shinyApp(ui = ui, server = server) 


