# exploring possible bug in scatterD3
library(scatterD3)
library(shiny)

x1 = data.frame(x = 1:5)
y1 = data.frame(y = 5:10, m = "m")
m1 = data.frame(x = 6:11)
n1 = data.frame(y = 4:9, m = "a'")

x = rbind(x1, m1)
y = rbind(y1, n1)

data = data.frame(ids = letters[1:10])
data = cbind.data.frame(data, x, y)

ui = fluidPage(
  scatterD3Output("plot")
  
)

server = function(input, output) {

}


shinyApp(ui = ui, server = server)