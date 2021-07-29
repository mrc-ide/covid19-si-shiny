library(ggplot2)
library(shiny)
models_grid <- function(ids) {
  fluidRow(
    column(4, plotOutput(ids[1])),
    column(4, plotOutput(ids[2])),
    column(4, plotOutput(ids[3]))
  )
}
ui <- navbarPage(
  titlePanel("Robust estimates of the infectiousness profile and serial interval distributions of SARS-CoV-2"),
  fluidRow(
    column(
      12, selectInput("view", "Choose outputs to view", choices = c("across models", "across datasets"))
    )
  ),
  models_grid(c("s3", "s3mix", "s3recall")),
  models_grid(c("s3mixrecall", "s4", "s4mix")),
  models_grid(c("s4recall", "s4mixrecall", "s3s4mix"))

)
server <- function(input, output, session) {
  output$s3 <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$s3mix <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$s3recall <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })
  output$s4 <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$s4mix <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$s4recall <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })
  output$s4mixrecall <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$s3mixrecall <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$s3s4mix <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })
}
shinyApp(ui, server)
