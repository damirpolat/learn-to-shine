library(shiny)
library(mlr)
library(llama)
library(aslib)
library(ggplot2)

library(plotly)

# Define UI ----
ui = fluidPage(
  titlePanel(strong("Comparing Selectors")),
  p("Compare algorithm selectors with ASlib scenarios"),
  fluidRow(
    column(3,
      textInput("scenario", label = h3(strong("Type ASlib scenario")),
                  placeholder = "ex. SAT11-INDU", value = "SAT11-INDU"),
      textInput("selector1", label = h3(strong("Type selector name")),
                placeholder = "ex. Random Forest", value = "regr.featureless"),
      textInput("selector2", label = h3(strong("Type selector name")),
                placeholder = "ex. Random Forest", value = "regr.featureless"),
      actionButton("run", "Run!")
    ), 
    column(6, plotlyOutput("plot1")), 
    mainPanel()
  )
)

# Define server logic ----
server = function(input, output) {
  
  learner1 = eventReactive(input$run, {
    makeImputeWrapper(learner = setHyperPars(makeLearner(input$selector1)),
                classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(),
                factor = imputeConstant("NA"), character = imputeConstant("NA")))
  })
  
  learner2 = eventReactive(input$run, {
    makeImputeWrapper(learner = setHyperPars(makeLearner(input$selector2)),
                classes = list(numeric = imputeMean(), integer = imputeMean(), logical = imputeMode(),
                factor = imputeConstant("NA"), character = imputeConstant("NA")))
  })
  
  # function to load ASlib scenario
  load_scenario = eventReactive(input$run, {
    getCosealASScenario(input$scenario)
  })
  
  # convert data into llama format
  get_data = reactive(trainTest(convertToLlama(load_scenario())))
  
  # build selectors 
  #selector1 = reactive(regression(learner1, get_data()))
  #selector2 = reactive(regression(learner2, get_data()))
  
  # compute misclassification penalties
  penalties1 = reactive(misclassificationPenalties(get_data(), temp_vals$selector1))
  penalties2 = reactive(misclassificationPenalties(get_data(), temp_vals$selector2))
  
  data = reactive(data.frame(mis1 = penalties1(), mis2 = penalties2()))
  temp_vals = reactiveValues()
  observe({
    temp_vals$selector1 = regression(learner1(), get_data())
    temp_vals$selector2 = regression(learner2(), get_data())
  })
  
  output$plot1 = renderPlotly({ 
    
    ggplot(data = data(), aes(x = mis1, y = mis2)) + geom_point(color = 'red') + 
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
      ggtitle("Misclassification Penalties") + 
      xlab(input$selector1) + ylab(input$selector2) +
      theme(plot.title = element_text(size = 15, hjust = 0.5))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)