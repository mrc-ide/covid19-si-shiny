library(ggplot2)
library(shiny)
models_grid <- function(ids) {
  fluidRow(
    column(4, plotOutput(ids[1])),
    column(4, plotOutput(ids[2])),
    column(4, plotOutput(ids[3]))
  )
}
ui <- fluidPage(
  titlePanel("Robust estimates of the infectiousness profile and serial interval distributions of SARS-CoV-2"),
  fluidRow(
    column(
      12, selectInput("view", "Choose outputs to view", choices = c("across models", "across datasets"))
    )
  ),
  tabsetPanel(
    tabPanel(
      "NF",
      models_grid(c("s3", "s3mix", "s3recall")),
      models_grid(c("s3mixrecall", "s4", "s4mix")),
      models_grid(c("s4recall", "s4mixrecall", "s3s4mix"))
    ),
    tabPanel(
      "skew normal",
        models_grid(c("sns3", "sns3mix", "sns3recall")),
      models_grid(c("sns3mixrecall", "sns4", "sns4mix")),
      models_grid(c("sns4recall", "sns4mixrecall", "sns3s4mix"))
    ),
    tabPanel(
      "gamma",
      models_grid(c("gs3", "gs3mix", "gs3recall")),
      models_grid(c("gs3mixrecall", "gs4", "gs4mix")),
      models_grid(c("gs4recall", "gs4mixrecall", "gs3s4mix"))
    )
  )
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
  ## Skew normal
  output$sns3 <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$sns3mix <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$sns3recall <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })
  output$sns4 <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$sns4mix <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$sns4recall <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })
  output$sns4mixrecall <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$sns3mixrecall <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$sns3s4mix <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$gs3 <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$gs3mix <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$gs3recall <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })
  output$gs4 <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$gs4mix <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$gs4recall <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })
  output$gs4mixrecall <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$gs3mixrecall <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })

  output$gs3s4mix <- renderPlot({
    ggplot(mtcars, aes(mpg, cyl)) + geom_point()
  })
}
shinyApp(ui, server)
