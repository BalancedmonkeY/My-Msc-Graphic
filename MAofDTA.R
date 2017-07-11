#### -- First R Shiny App -- ####
### Creating basic ROC curve ###
# Going to have a first tab which is just a standard ROC curve and you toggle what features/stats one wants #

library(mada)
library(shiny)
library(shinyWidgets)

#------------------------------------------------------------#

ui <- fluidPage(
  fluidRow(titlePanel("Msc Project!")), #title
  
  fluidRow(column(3, wellPanel( fluidRow(p("Plot options")),
                                fluidRow(dropdownButton(label = "Choose options", circle=F, status = "primary", width = 80,
                                                        checkboxGroupInput(inputId = "plotcheck", label = "Choose", 
                                                        choices = list("Point estimate"=1, "Confidence region"=2, "Predictive region"=3, "Extrapolate"=4, "Data points"=5),
                                                        selected=list(1,2,3)))) #selected shows the default ticked boxes
                  )), #gives options to alter the aesthetics of the plot
           
           column(6, plotOutput(outputId="SROC"))), #graph
  
  fluidRow(column(6, offset=3, wellPanel( fluidRow(column(6, p("Bivariate Meta-Analysis Statistics")), 
                                                   column(6, dropdownButton(label="Choose statistics", circle=F, status="primary", width=80,
                                                                            checkboxGroupInput(inputId="statscheck", label="Choose", #drop down menu for the statistics
                                                                            choices=list("Sensitivity"=1, "Specificity"=2, "AUC"=3, "FPR"=4), selected=list(1,2))) )),
                                          conditionalPanel( condition="output.senscheck", textOutput("sens")), #conditional statement is a reactive R equality (when true the panel will run)
                                          conditionalPanel( condition="output.speccheck", textOutput("spec")),
                                          conditionalPanel( condition="output.AUCcheck", textOutput("AUC")),
                                          conditionalPanel( condition="output.FPRcheck", textOutput("FPR")) ))) #statistics conditional on being ticked
 
  #fluidRow( textOutput(outputId="logical")) #print results from logical vector
)

#-------------------------------------------------------------------------#

server <- function(input,output) {
  
  #Data
  data("AuditC") #basic data to have a demo with
  
  #Analyis
  fit.reitsma <- reitsma(AuditC) #fits a bivariate model
  stats<-summary(fit.reitsma) #output statistics from the fit 
  
  # Basic ROC curve without anything else, add the other parts on top using an interactive vector (object orientation)
  #toggle pieces: data, summary estimate, conf region, pred region, extrapolate
  plotticks<-logical(length=5) #default of six Falses
  leglabticks <- matrix(nrow=5, ncol=1)
  legendticks <- matrix(nrow=5, ncol=2) #empty matrix for legend arguments
  leglabticks[1] <- "SROC curve"
  legendticks[1,2] <- 1 #legend options for SROC curve
    output$SROC <- renderPlot({ if ('1' %in% input$plotcheck) {plotticks[1] <- T
                                                                leglabticks[2]<-"Summary estimate"
                                                                legendticks[2,1]<-1} #change plotticks and legendticks if option 1 selected
                                if ('2' %in% input$plotcheck) {plotticks[2] <- T
                                                                leglabticks[3]<-"Confidence region"
                                                                legendticks[3,2]<-1} #creates interactive vector
                                if ('3' %in% input$plotcheck) {plotticks[3] <- T
                                                                leglabticks[4]<-"Predictive region"
                                                                legendticks[4,2]<-2}
                                if ('4' %in% input$plotcheck) {plotticks[4] <- T} #Extrapolate
                                if ('5' %in% input$plotcheck) {plotticks[5] <- T
                                                                leglabticks[5]<-"Data"
                                                                legendticks[5,1]<-2}
                                  plot(fit.reitsma, main = "SROC curve (bivariate model) for AUDIT-C data",
                                       extrapolate=plotticks[4], plotsumm=plotticks[2], predict=plotticks[3], pch="") #plot where options are dependent on interactive vector plotticks
                                  if (plotticks[5]==T) {points(fpr(AuditC), sens(AuditC), pch=2)} #add data points
                                  if (plotticks[1]==T) {points(stats$coefficients[4,1], stats$coefficients[3,1])} #adding summary estimate
                                  legend("bottomright", leglabticks, pch = legendticks[,1], lty=legendticks[,2], lwd=c(2,NA,1,1,NA))
                                  }) 
  
 
  #Add some stats
  output$sens <- renderText(print(sprintf("Sensitivity: %4.3f", stats$coefficients[3,1]), quote=F)) #prints sensitivity
  output$spec <- renderText(print(sprintf("Specificity: %4.3f", 1-stats$coefficients[4,1]), quote=F)) #prints specificity
  output$AUC <- renderText(print(sprintf("AUC: %4.3f", stats$AUC[1]), quote=F)) #prints AUC
  output$FPR <- renderText(print(sprintf("False-positive rate: %4.3f", stats$coefficients[4,1]), quote=F)) #prints FPrate
  #Making the print statistics interactive with the pulldown menu:
  output$senscheck <- reactive({'1' %in% input$statscheck}) #reactive boolean to see whether '1' (sensitivity has been checked) is in the checklist
  output$speccheck <- reactive({'2' %in% input$statscheck}) #needs to be reactive as can't do the correct condition in JavaScript
  output$AUCcheck <- reactive({'3' %in% input$statscheck})
  output$FPRcheck <- reactive({'4' %in% input$statscheck})
outputOptions(output, "senscheck", suspendWhenHidden = FALSE) #option needed for conditional to function properly
outputOptions(output, "speccheck", suspendWhenHidden = FALSE)
outputOptions(output, "AUCcheck", suspendWhenHidden = FALSE)
outputOptions(output, "FPRcheck", suspendWhenHidden = FALSE)
}

#-----------------------------------------------------------#

shinyApp(ui = ui, server = server)




