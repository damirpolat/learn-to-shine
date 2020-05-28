library(shiny)
library(mlr)
library(llama)
library(aslib)

# Define UI ----
ui = fluidPage(
  titlePanel(strong("Comparing Selectors")),
  sidebarLayout(
    sidebarPanel(
      p("Compare algorithm selectors with ASlib scenarios"),
      textInput("scenario", label = h3(strong("Type ASlib scenario")),
                  placeholder = "ex. SAT11-INDU", value = "SAT11-INDU"),
      plotOutput("plot1") 
    ), 
    mainPanel()
  )
)

# Define server logic ----
server = function(input, output) {
  
  learner1 = makeImputeWrapper(learner = setHyperPars(makeLearner("regr.randomForest")),
                classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(),
                factor = imputeConstant("NA"), character = imputeConstant("NA")))
  
  learner2 = makeImputeWrapper(learner = setHyperPars(makeLearner("regr.featureless")),
                               classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(),
                                              factor = imputeConstant("NA"), character = imputeConstant("NA")))
  
  temp_vals = reactiveValues()
  observe({
    temp_vals$scenario_name = input$scenario
    temp_vals$scenario = getCosealASScenario(temp_vals$scenario_name)
    temp_vals$llama.cv = trainTest(convertToLlama(temp_vals$scenario))
    temp_vals$selector1 = regression(learner1, temp_vals$llama.cv)
    temp_vals$selector2 = regression(learner2, temp_vals$llama.cv)
    temp_vals$penalties1 = misclassificationPenalties(temp_vals$llama.cv, temp_vals$selector1)
    temp_vals$penalties2 = misclassificationPenalties(temp_vals$llama.cv, temp_vals$selector2)
    
    temp_vals$data = data.frame("mis1" = temp_vals$penalties1, "mis2" = temp_vals$penalties2)
  })
  
  #scenario = reactive({ getCosealASScenario(input$scenario) })
  
  output$plot1 = renderPlot({ 
    ggplot(temp_vals$data, aes(x = mis1, y = mis2)) + geom_point(color = 'red') + 
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
      ggtitle("Misclassification Penalties of Two Selectors") + 
      xlab("Random Forest") + ylab("Featureless Model") +
      theme(plot.title = element_text(size = 15, hjust = 0.5))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)