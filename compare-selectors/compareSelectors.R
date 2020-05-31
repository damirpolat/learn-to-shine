# compareSelectors.R
# Damir Pulatov

library(shiny)
library(mlr)
library(llama)
library(aslib)
library(ggplot2)
library(scatterD3)
source("./helpers.R")

set.seed(1L)

# reference lines for scatter plot
default_lines = data.frame(slope = c(0, Inf, 1), intercept = c(0, 0, 0), 
                stroke_width = 1, stroke_dasharray = 5)

# Define UI 
ui = fluidPage(
  titlePanel(strong("Comparing Selectors")),
  p("Compare algorithm selectors with ASlib scenarios"),
  fluidRow(
    column(2,
           textInput("scenario", label = h3(strong("Type ASlib scenario")),
                     placeholder = "ex. SAT11-INDU", value = "SAT11-INDU"),
           textInput("selector1", label = h3(strong("Type learner name")),
                     placeholder = "ex. Random Forest", value = "regr.featureless"),
           textInput("selector2", label = h3(strong("Type learner name")),
                     placeholder = "ex. Random Forest", value = "regr.featureless"),
           actionButton("run", "Run!")
    ), 
    column(7, offset = 1, scatterD3Output("plot1")), 
    column(2,
           checkboxInput("mcp", "Misclassification Penalties", value = TRUE),
           checkboxInput("par", "PAR10 Scores")),
    mainPanel()
  )
)

# Define server logic 
server = function(input, output) {
  
  lines = reactive({ default_lines })
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
  get_ids = reactive(get_data()$data[get_data()$test[[1]], get_data()$ids])
  
  # compute metrics of interest
  penalties1 = reactive(misclassificationPenalties(get_data(), temp_vals$selector1))
  penalties2 = reactive(misclassificationPenalties(get_data(), temp_vals$selector2))
  par1 = reactive(parscores(get_data(), temp_vals$selector1))
  par2 = reactive(parscores(get_data(), temp_vals$selector2))
  
  build_mcp = reactive(build_data(get_ids(), penalties1(), penalties2(), par1 = NULL, par2 = NULL))
  build_par = reactive(build_data(get_ids(), penalties1 = NULL, penalties2 = NULL, par1(), par2()))
  build_both = reactive(build_data(get_ids(), penalties1(), penalties2(), par1(), par2()))
  # create data for plot
  data = reactive(
    if(input$mcp && input$par) {
      build_both()
    } else if (input$mcp) {
      build_mcp()
      #data.frame(instance_id = get_ids(), x = penalties1(), y = penalties2(), 
      #           method = "mcp")
    } else if (input$par) {
      build_par()
      #data.frame(instance_id = get_ids(), x = par1(), y = par2(), method = "par")
    }
  )
  
  # might need to rewrite this
  temp_vals = reactiveValues()
  observe({
    temp_vals$selector1 = regression(learner1(), get_data())
    temp_vals$selector2 = regression(learner2(), get_data())
  })
  
  tooltip = reactive(paste("instance_id = ", data()$instance_id, "<br>x = ", 
                           data()$x, "<br>y = ", data()$y))
  # make scatterplot with misclassification penalties
  output$plot1 = renderScatterD3({
    scatterD3(data = data(), x = x, y = y, tooltip_text = tooltip(),
      tooltip_position = "top right",
      xlab = input$selector1, ylab = input$selector2,
      point_size = 100, point_opacity = 0.5,
      hover_size = 3, hover_opacity = 1,
      lines = lines(),
      caption = list(text = paste("Misclassification Penalties for ", input$selector1, " vs. ", input$selector2),
                     title = "Misclassification Penalties"))
  })
}

# Run the app 
shinyApp(ui = ui, server = server)