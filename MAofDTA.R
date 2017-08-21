#### -- First R Shiny App -- ####
### Creating basic ROC curve ###
# Going to have a first tab which is just a standard ROC curve and you toggle what features/stats one wants #

#install.packages("madaedit.tar.gz", repos = NULL, type = "source") #install amended mada package (needs to be done only once), gotta instal dependencies manually
library(mada)
library(shiny)
library(shinyWidgets)
library(DT)


#------------------------------------------------------------#

ui <- fluidPage(
  titlePanel("Msc Project!"), #title
  
  tabsetPanel( #enables tabs
  tabPanel("Summary Analysis", #basic tab
  
    column(4, wellPanel(fluidRow(h4("Plot options")), #plot options
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
    
    column(8, fluidRow(align="center", plotOutput(outputId="SROC", width="450px", height="450px"), #fixed width to keep ROC space square
           h6("Note: Bivariate random-effects model fitted")),   
           br(),
           h4("Data"),
              DT::dataTableOutput("sumdata"))
                 ),

  tabPanel("Sensitivity Analysis", 
           column(3, wellPanel(checkboxGroupInput(inputId="studies", label="Included studies",
                                                  choices=list("Aalto (2006)"=1, "Aertgeerts (2001)"=2, "Aertgeerts (2002)"=3, "Bradley (2003)"=4, "Bradley (2007)"=5, "Bush (1998)"=6, "Gomez (2006)"=7, "Gordon (2001)"=8, "Gual (2002)"=9, "Rumpf (2002)"=10, "Seale (2006)"=11, "Selin (2006)"=12, "Tsai (2005)"=13, "Tuunanen (2007)"=14),
                                                  selected=list(1,2,3,4,5,6,7,8,9,10,11,12,13,14)), #checkboxes for studies
                               actionButton(inputId="rerun", label="RUN ANALYSIS") #Button to initiate re-running of sensitivity analysis
                               )),
           column(9, fluidRow(column(7, align="center", plotOutput(outputId="SROCb", width="550px", height="550px")), #plot
                              column(5, wellPanel(fluidRow(h4("Plot options")), #plot options
                                                  fluidRow(checkboxInput(inputId="dataptscheck2", label="Data points", value=T)),
                                                  fluidRow(checkboxGroupInput(inputId = "bivcheck2", label = "Bivariate model options", 
                                                                              choices = list("Point estimate"=1, "Confidence region"=2, "Predictive region"=3),
                                                                              selected=list(1,2))),
                                                  fluidRow(checkboxGroupInput(inputId = "HSROCcheck2", label = "HSROC options",
                                                                              choices = list("SROC curve"=1, "Extrapolate"=2)))),
                                     h6("Note: Bivariate random-effects model fitted"),  
                      wellPanel(fluidRow(column(7, h5("Original Analysis"), #basic stats
                                                   h6(textOutput("origsens")),
                                                   h6(textOutput("origspec"))), #original results
                                         column(5, h5("Current Analysis"),
                                                   h6(textOutput("cursens")),
                                                   h6(textOutput("curspec")))))   ))#current results 
           )
  )
)
)

#-------------------------------------------------------------------------#

server <- function(input,output) {
  
  #Data
  data("AuditC") #basic data to have a demo with
  #Add Study authors and year
  AuditC$Author <- c("Aalto","Aertgeerts", "Aertgeerts", "Bradley", "Bradley", "Bush", "Gomez", "Gordon", "Gual", "Rumpf", "Seale", "Selin", "Tsai", "Tuunanen")
  AuditC$Year <- c(2006, 2001, 2002, 2003, 2007, 1998, 2006, 2001, 2002, 2002, 2006, 2006, 2005, 2007)
  #Add sens and spec (to 3 dp)
  AuditC$sensitivity <- round(sens(AuditC), digits=3)
  AuditC$FalsePositiveRate <- round(fpr(AuditC), digits=3)
  #Add total number of participants
  AuditC$No_of_participants <- AuditC$TP+AuditC$FN+AuditC$FP+AuditC$TN
  #Reorder columns ready for output in datatable
  AuditC<-AuditC[,c(5,6,1,2,3,4,9,7,8)]
  
  #Reactive dataset
  datacopy<-AuditC #copy original dataset
  makeReactiveBinding("datacopy")
  curdata <- reactive({
    datacopy <- datacopy[input$studies, ] #updates from checkboxes
  })
  
  #Analyis
  fit.reitsma <- reitsma(AuditC) #fits a bivariate model
  sum.fit<-summary(fit.reitsma) #output statistics from the fit 
  pts.fit <- SummaryPts(fit.reitsma) #outputs posLR, negLR, invnegLR and DOR (as samples, have to mean to extract output)
  
  #Reactive analysis
  fitting <- reactive({
    sensdata <- curdata()
    fit <- reitsma(sensdata)
  }) #fitting() is a reactive reitsma fit
  summm <- reactive({
    fitb <- fitting()
    sum <- summary(fitb)
  }) #summary() is a reactive summary object
  
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
  
  
    #SENSITIVITY PLOT
  plotticks2<-logical(length=6) #default of six Falses
  leglabticks2 <- matrix(nrow=5, ncol=1)
  legendticks2 <- matrix(nrow=5, ncol=2)
  
output$SROCb <- renderPlot ({
  
  sensdata <- curdata() #bring in the reactive dataset and summaries needed
  fitb <- fitting()
  sum.fitb <- summm()
  
  #plot with options
  if ('1' %in% input$bivcheck2) {plotticks2[1] <- T
                                leglabticks2[2]<-"Summary estimate"
                                legendticks2[2,1]<-1} #change plotticks and legendticks if option 1 selected
  if ('2' %in% input$bivcheck2) {plotticks2[2] <- T
                                leglabticks2[3]<-"Confidence region"
                                legendticks2[3,2]<-1} #creates interactive vector
  if ('3' %in% input$bivcheck2) {plotticks2[3] <- T
                                leglabticks2[4]<-"Predictive region"
                                legendticks2[4,2]<-3}
  if ('2' %in% input$HSROCcheck2) {plotticks2[4] <- T} #Extrapolate
  if (input$dataptscheck2==T) {plotticks2[5] <- T
                              leglabticks2[5]<-"Data"
                              legendticks2[5,1]<-2}
  if ('1' %in% input$HSROCcheck2) {plotticks2[6] <- T
                                  leglabticks2[1]<-"HSROC curve"
                                  legendticks2[1,2]<-1}
  plot(fit.reitsma, main="Bivariate model for AUDIT-C data", #original dataset
       HSROC=plotticks2[6], extrapolate=plotticks2[4], plotsumm=plotticks2[2], predict=plotticks2[3], pch="", sroclwd=1, col="gray")
  par(new=TRUE) #allows next plot to be on same graph
  plot(fitb,
       HSROC=plotticks2[6], extrapolate=plotticks2[4], plotsumm=plotticks2[2], predict=plotticks2[3], pch="", sroclwd=2) #plot where options are dependent on interactive vector plotticks
  if (plotticks2[5]==T) {points(fpr(AuditC), sens(AuditC), pch=2, col="gray")} #add data points
  if (plotticks2[5]==T) {points(fpr(sensdata), sens(sensdata), pch=2)}
  if (plotticks2[1]==T) {points(sum.fit$coefficients[4,1], sum.fit$coefficients[3,1], col="gray")} #adding summary estimate
  if (plotticks2[1]==T) {points(sum.fitb$coefficients[4,1], sum.fitb$coefficients[3,1])}
  legend("bottomright", bty="n", leglabticks2, pch = legendticks2[,1], lty=legendticks2[,2], lwd=c(2,NA,1,1,NA))
  })

#Sensitivity summary values
output$origsens <- renderText(print(sprintf("Sensitivity: %4.3f (%4.3f, %4.3f)", sum.fit$coefficients[3,1], sum.fit$coefficients[3,5], sum.fit$coefficients[3,6])))
output$cursens <- renderText({
  sum.fitb <- summm() #bring in reactive summary object
  print(sprintf("%4.3f (%4.3f, %4.3f)", sum.fitb$coefficients[3,1], sum.fitb$coefficients[3,5], sum.fitb$coefficients[3,6])) })
output$origspec <- renderText(print(sprintf("FP-Rate: %4.3f (%4.3f, %4.3f)", sum.fit$coefficients[4,1], sum.fit$coefficients[4,5], sum.fit$coefficients[4,6])))
output$curspec <- renderText({
 sum.fitb <- summm()
  print(sprintf("%4.3f (%4.3f, %4.3f)", sum.fitb$coefficients[4,1], sum.fitb$coefficients[4,5], sum.fitb$coefficients[4,6])) })

  
}

#-----------------------------------------------------------#

shinyApp(ui = ui, server = server)




