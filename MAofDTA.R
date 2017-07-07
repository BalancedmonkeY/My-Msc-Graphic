#### -- First R Shiny App -- ####
### Creating basic ROC curve ###
# Going to have a first tab which is just a standard ROC curve and you toggle what features/stats one wants #

library(mada)
library(shiny)

#---------- Dropdownbutton function code -----------------#
dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}

#----------------------------------------------------------------------#

ui <- fluidPage(
  fluidRow(titlePanel("Msc Project!")), #title
  fluidRow(column(3, wellPanel( fluidRow(p("Plot options")),
                                fluidRow(dropdownButton(
                                  label = "Choose options", status = "primary", width = 80,
                                  checkboxGroupInput(inputId = "plotcheck", label = "Choose", 
                                                     choices = list("Point estimate"=1, "Data points"=2, "Confidence region"=3, "Predictive region"=4, "Individual confidence regions"=5),
                                                     selected=list(1,2,3))
                                ))
                                )), #gives options to alter the aesthetics of the plot
           column(6, plotOutput(outputId="SROC"))), #graph
  fluidRow(column(6, offset=3, wellPanel( fluidRow(column(6, p("Bivariate Meta-Analysis Statistics")), column(6, dropdownButton(
    label="Choose statistics", status="primary", width=80,
    checkboxGroupInput(inputId="statscheck", label="Choose",
                       choices=list("Sensitivity"=1, "Specificity"=2, "AUC"=3), selected=list(1,2)))
  )),
  conditionalPanel( condition="output.senscheck", textOutput("sens")),
  conditionalPanel( condition="output.speccheck", textOutput("spec")),
  conditionalPanel( condition="output.AUCcheck", textOutput("AUC")) )))#stats in a gray box together


)

#-------------------------------------------------------------------------#

server <- function(input,output) {
  data("AuditC") #basic data to have a demo with
  

output$senscheck <- reactive({'1' %in% input$statscheck})
output$speccheck <- reactive({'2' %in% input$statscheck})
output$AUCcheck <- reactive({'3' %in% input$statscheck})
outputOptions(output, "senscheck", suspendWhenHidden = FALSE)
outputOptions(output, "speccheck", suspendWhenHidden = FALSE)
outputOptions(output, "AUCcheck", suspendWhenHidden = FALSE)

#Bivariate SROC curve
  output$SROC <- renderPlot( {
fit.reitsma <- reitsma(AuditC) #fixed effect coefficients (intercepts), loglik, AIC BIC
plot(fit.reitsma, sroclwd = 2, main = "SROC curve (bivariate model) for AUDIT-C data") #resulting SROC curve, thick curve for SROC, summary estimate with conf.region
points(fpr(AuditC), sens(AuditC), pch = 2) #add study sens/spec
legend("bottomright", c("data", "summary estimate"), pch = c(2,1))
legend("bottomleft", c("SROC", "conf. region"), lwd = c(2,1))
} )
  
  #Need a basic ROC curve without anything else, to then be able to add the other parts on top
  #going to need a "middle man (plot) similar to the middle data example#
  plot(fit.reitsma)
  
  
  #Add some stats
  stats<-summary(fit.reitsma) 
  output$sens <- renderText(print(sprintf("Sensitivity: %4.3f", stats$coefficients[3,1]), quote=F))
  output$spec <- renderText(print(sprintf("Specificity: %4.3f", 1-stats$coefficients[4,1]), quote=F))
  output$AUC <- renderText(print(sprintf("AUC: %4.3f", stats$AUC[1]), quote=F))
  
}

shinyApp(ui = ui, server = server)




