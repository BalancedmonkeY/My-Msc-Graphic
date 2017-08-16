#### -- First R Shiny App -- ####
### Creating basic ROC curve ###
# Going to have a first tab which is just a standard ROC curve and you toggle what features/stats one wants #

#install.packages("madaedit.tar.gz", repos = NULL, type = "source") #install amended mada package (needs to be done only once), gotta instal dependencies manually
library(mada)
library(shiny)
library(shinyWidgets)
library(DT)

#Made necessary fixes
#Said '95%', changes 'HRSOC', made HSROC parameters Greek symbols, created footnote of what model used, made plot square and centered, Added study authors and year, made sens and spec 3dp, added total ptps column,removed border on legend 

#------------------------------------------------------------#

ui <- fluidPage(
  titlePanel("Msc Project!"), #title
  
    column(5, wellPanel(fluidRow(h4("Plot options")), #plot options
                  fluidRow(checkboxInput(inputId="dataptscheck", label="Data points")),
                 fluidRow(checkboxGroupInput(inputId = "bivcheck", label = "Bivariate model options", 
                                            choices = list("Point estimate"=1, "Confidence region"=2, "Predictive region"=3),
                                            selected=list(1,2,3))),
                 fluidRow(checkboxGroupInput(inputId = "HSROCcheck", label = "HSROC options",
                                             choices = list("SROC curve"=1, "Extrapolate"=2)))),
                 br(),
                 wellPanel(fluidRow(p("Bivariate Meta-Analysis Statistics")), 
                 fluidRow(column(6, checkboxInput("intervals", label="95% Confidence intervals")),#checkbox for option to show confidence intervals
                          column(6, dropdownButton(label="Choose statistics", circle=F, status="primary", width=80, size="sm",
                                                   checkboxGroupInput(inputId="statscheck", label="Choose", #drop down menu for the statistics
                                                                      choices=list("Sensitivity"=1, "Specificity"=2, "AUC"=3, "FPR"=4, "DOR"=5, "Likelihood Ratios"=6, "HSROC parameters"=7), selected=list(1,2))) )),
                 conditionalPanel( condition="output.senscheck", fluidRow(column(5, "Sensitivity"), column(2, textOutput("sens")), column(5, conditionalPanel(condition="input.intervals", textOutput("sensCI"))))), #conditional statement is a reactive R equality (when true the panel will run)
                 conditionalPanel( condition="output.speccheck", fluidRow(column(5, "Specificity"), column(2, textOutput("spec")), column(5, conditionalPanel(condition="input.intervals", textOutput("specCI"))))), #conditional confidence intervals
                 conditionalPanel( condition="output.AUCcheck", fluidRow(column(5, "AUC"), column(2, textOutput("AUC")))),
                 conditionalPanel( condition="output.FPRcheck", fluidRow(column(5, "False-positive rate"), column(2, textOutput("FPR")), column(5, conditionalPanel(condition="input.intervals", textOutput("FPRCI"))))),
                 conditionalPanel( condition="output.DORcheck", fluidRow(column(5, "Diagnostic Odds Ratio"), column(2, textOutput("DOR")))),
                 conditionalPanel( condition="output.LRcheck", fluidRow(strong("Likelihood Ratios")),
                                   fluidRow(column(5, "Positive"), column(2, textOutput("LRp"))),
                                   fluidRow(column(5, "Negative"), column(2, textOutput("LRn")))),
                 conditionalPanel( condition="output.HSROCcheck", fluidRow(strong("HSROC parameters")),
                                                                  fluidRow(column(5, HTML("&theta;")), column(2, textOutput("Theta"))),
                                                                  fluidRow(column(5, HTML("&lambda;")), column(2, textOutput("Lambda"))),
                                                                  fluidRow(column(5, HTML("&beta;")), column(2, textOutput("Beta"))),
                                                                  fluidRow(column(5, HTML("&sigma;<sub>&theta;</sub>")), column(2, textOutput("Sigth"))),
                                                                  fluidRow(column(5, HTML("&sigma;<sub>&alpha;</sub>")), column(2, textOutput("Sigal"))))
                 )),
    
    column(7, fluidRow(align="center", plotOutput(outputId="SROC", width="450px", height="450px"), #fixed width to keep ROC space square
           h6("Note: Bivariate random-effects model fitted")),   
           br(),
           h4("Data"),
              DT::dataTableOutput("sumdata"))
                 )

#-------------------------------------------------------------------------#

server <- function(input,output) {
  
  #Data
  data("AuditC") #basic data to have a demo with
  #Add Study authors and year
  AuditC$Author <- c("Aalto","Aertgeert", "Aertgeert", "Bradley", "Bradley", "Bush", "Gomez", "Gordon", "Gual", "Rumpf", "Seale", "Selin", "Tsai", "Tuunanen")
  AuditC$Year <- c(2006, 2001, 2002, 2003, 2007, 1998, 2006, 2001, 2002, 2002, 2006, 2006, 2005, 2007)
  #Add sens and spec (to 3 dp)
  AuditC$sensitivity <- round(sens(AuditC), digits=3)
  AuditC$FalsePositiveRate <- round(fpr(AuditC), digits=3)
  #Add total number of participants
  AuditC$No_of_participants <- AuditC$TP+AuditC$FN+AuditC$FP+AuditC$TN
  #Reorder columns ready for output in datatable
  AuditC<-AuditC[,c(5,6,1,2,3,4,9,7,8)]
  
  
  #Analyis
  fit.reitsma <- reitsma(AuditC) #fits a bivariate model
  sum.fit<-summary(fit.reitsma) #output statistics from the fit 
  pts.fit <- SummaryPts(fit.reitsma) #outputs posLR, negLR, invnegLR and DOR (as samples, have to mean to extract output)
  
  # Basic ROC curve without anything else, add the other parts on top using an interactive vector (object orientation)
  #toggle pieces: data, summary estimate, conf region, pred region, extrapolate
  plotticks<-logical(length=6) #default of six Falses
  leglabticks <- matrix(nrow=5, ncol=1)
  legendticks <- matrix(nrow=5, ncol=2) #empty matrix for legend arguments
    output$SROC <- renderPlot({ if ('1' %in% input$bivcheck) {plotticks[1] <- T
                                                                leglabticks[2]<-"Summary estimate"
                                                                legendticks[2,1]<-1} #change plotticks and legendticks if option 1 selected
                                if ('2' %in% input$bivcheck) {plotticks[2] <- T
                                                                leglabticks[3]<-"Confidence region"
                                                                legendticks[3,2]<-1} #creates interactive vector
                                if ('3' %in% input$bivcheck) {plotticks[3] <- T
                                                                leglabticks[4]<-"Predictive region"
                                                                legendticks[4,2]<-3}
                                if ('2' %in% input$HSROCcheck) {plotticks[4] <- T} #Extrapolate
                                if (input$dataptscheck==T) {plotticks[5] <- T
                                                                leglabticks[5]<-"Data"
                                                                legendticks[5,1]<-2}
                                if ('1' %in% input$HSROCcheck) {plotticks[6] <- T
                                                                leglabticks[1]<-"HSROC curve"
                                                                legendticks[1,2]<-1}
                                  plot(fit.reitsma, main = "Bivariate model for AUDIT-C data",
                                       HSROC=plotticks[6], extrapolate=plotticks[4], plotsumm=plotticks[2], predict=plotticks[3], pch="", sroclwd=2) #plot where options are dependent on interactive vector plotticks
                                  if (plotticks[5]==T) {points(fpr(AuditC), sens(AuditC), pch=2)} #add data points
                                  if (plotticks[1]==T) {points(sum.fit$coefficients[4,1], sum.fit$coefficients[3,1])} #adding summary estimate
                                  legend("bottomright", bty="n", leglabticks, pch = legendticks[,1], lty=legendticks[,2], lwd=c(2,NA,1,1,NA))
                                  }) 
  
 
  #Add some stats
  output$sens <- renderText(print(sprintf("%4.3f", sum.fit$coefficients[3,1]), quote=F)) #prints sensitivity
  output$spec <- renderText(print(sprintf("%4.3f", 1-sum.fit$coefficients[4,1]))) #prints specificity
  output$AUC <- renderText(print(sprintf("%4.3f", sum.fit$AUC[1]))) #prints AUC
  output$FPR <- renderText(print(sprintf("%4.3f", sum.fit$coefficients[4,1]))) #prints FPrate
  output$DOR <- renderText(print(sprintf("%6.3f", mean(pts.fit$DOR)))) #prints summary DOR
  output$LRp <- renderText(print(sprintf("%6.3f", mean(pts.fit$posLR)))) #LR+
  output$LRn <- renderText(print(sprintf("%6.3f", mean(pts.fit$negLR)))) #LR-
  output$Theta <- renderText(print(sprintf("%4.3f", sum.fit$coef_hsroc[1])))#theta
  output$Lambda <- renderText(print(sprintf("%4.3f", sum.fit$coef_hsroc[2]))) #lambda
  output$Beta <- renderText(print(sprintf("%4.3f", sum.fit$coef_hsroc[3]))) #beta
  output$Sigth <- renderText(print(sprintf("%4.3f", sum.fit$coef_hsroc[4]))) #sigma_theta
  output$Sigal <- renderText(print(sprintf("%4.3f", sum.fit$coef_hsroc[5]))) #sigma_alpha
  #Making the print statistics interactive with the pulldown menu:
  output$senscheck <- reactive({'1' %in% input$statscheck}) #reactive boolean to see whether '1' (sensitivity has been checked) is in the checklist
  output$speccheck <- reactive({'2' %in% input$statscheck}) #needs to be reactive as can't do the correct condition in JavaScript
  output$AUCcheck <- reactive({'3' %in% input$statscheck})
  output$FPRcheck <- reactive({'4' %in% input$statscheck})
  output$DORcheck <- reactive({'5' %in% input$statscheck})
  output$LRcheck <- reactive({'6' %in% input$statscheck})
  output$HSROCcheck <- reactive({'7' %in% input$statscheck})
outputOptions(output, "senscheck", suspendWhenHidden = FALSE) #option needed for conditional to function properly
outputOptions(output, "speccheck", suspendWhenHidden = FALSE)
outputOptions(output, "AUCcheck", suspendWhenHidden = FALSE)
outputOptions(output, "FPRcheck", suspendWhenHidden = FALSE)
outputOptions(output, "DORcheck", suspendWhenHidden = FALSE)
outputOptions(output, "LRcheck", suspendWhenHidden = FALSE)
outputOptions(output, "HSROCcheck", suspendWhenHidden = FALSE)
  #Adding confidence intervals
  output$sensCI <- renderText(print(sprintf("(%4.3f, %4.3f)", sum.fit$coefficients[3,5], sum.fit$coefficients[3,6]), quote=F))
  output$specCI <- renderText(print(sprintf("(%4.3f, %4.3f)", 1-sum.fit$coefficients[4,6], 1-sum.fit$coefficients[4,5]), quote=F))
  output$FPRCI <- renderText(print(sprintf("(%4.3f, %4.3f)", sum.fit$coefficients[4,5], sum.fit$coefficients[4,6]), quote=F))

  
  #Add table of studies
  output$sumdata <- DT::renderDataTable({ datatable(AuditC, colnames=c("Author","Year", "TP","FN", "FP", "TN", "No. of participants", "Sensitivity", "False-positive rate"))  })
  
  }

#-----------------------------------------------------------#

shinyApp(ui = ui, server = server)




