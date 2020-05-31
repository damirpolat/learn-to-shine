library(shiny)
#Lesson exercise from RShiny tutorial

ui = fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(
      h3(strong("Installation")),
      br(),
      p("Shiny is available on CRAN, so you can"),
      p("install it in the usual way from your R"),
      p("console:"),
      br(),
      code("install.packages(\"shiny\")"),
      br(),
      br(),
      img(src = "rstudio.png", height = 72, width = 220),
      br(),
      "Shiny is a product of ",
      a("RStudio", href = "https://rstudio.com/")
    ),
    mainPanel(
      h1("Introducting Shiny"),
      p("Shiny is a new package from RStudio that makes it ",
        em("incredibly"),
        "easy to build interactive web applications with R"),
      br(),
      p("For an introduction and live examples, visit the ",
        a("Shiny homepage", href = "https://shiny.rstudio.com/")),
      br(),
      br(),
      br(),
      h1(strong("Features")),
      p("- Build useful web applications with only a few lines of code -- no JavaScript required."),
      p("- Shiny applications are automatically 'live' in the same way that ",
        strong("spreadsheets"),
        " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser.")
    )
  )
)

server = function(input, output) {
  
}

shinyApp(ui = ui, server = server)