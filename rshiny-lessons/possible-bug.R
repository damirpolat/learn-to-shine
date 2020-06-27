# possible-bug.R
library(shiny)
library(shinydashboard)

# line break
linebreak = function(n) {
  HTML(strrep(br(), n))
}

ui = dashboardPage(
  dashboardHeader(title = "Testing a bug", 
                  titleWidth = 440),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Ex", tabName = "ex", icon = icon("file-import"))
    )
  ),
  dashboardBody(
    tabItems(
      # input tab
      tabItem(tabName = "ex",
              fluidRow(
                column(width = 2,
                       selectInput("a1", label = h4(strong("Text")),
                                   choices = c("c1", "c2")),
                       selectInput("a2", label = h4(strong("Text")),
                                   choices = c("c1", "c2")),
                       selectInput("a3", label = h4(strong("Text")),
                                   choices = c("c1", "c2"))
                )
              ),
              linebreak(2),
              # summary
              column(width = 8, offset = 4,
                     htmlOutput("title"),
                     verbatimTextOutput("perf"),
                     tags$head(tags$style("#clickGene{overflow-y:scroll; max-height: 50px;}"))
              )
      )
    )
  )
)

server = function(input, output) {
  data = reactive({
    d = data.frame(matrix(ncol = 11, nrow = 20))
    d[1] = rep.int("very long titles here", 20)
    d[2] = rep.int("more text", 20)
    for(i in 3:20) {
      d[i] = round(runif(20), 3)
    }
    return(d)
  })
  
  output$perf = renderPrint({
    print(data())
  })
}

shinyApp(ui, server)