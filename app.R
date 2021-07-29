library(ggplot2)
library(shiny)
source("R/utils.R")
cowling <- readRDS("data/cowling_data_cleaned.rds")
pairs <- readRDS("data/cowling_data_pairs.rds")
snfull <- readRDS("processed_stanfits/skew_normal/release/best_si_skew_normal.rds")
snpairs <- readRDS("processed_stanfits/skew_normal/discrete_pairs/best_si_skew_normal.rds")

models_grid <- function(model, ids) {
  fluidRow(
    column(2, div(HTML(model))),
    column(5, plotOutput(ids[1])),
    column(5, plotOutput(ids[2]))
  )
}
ui <- fluidPage(
  titlePanel("Robust estimates of the infectiousness profile and serial interval distributions of SARS-CoV-2"),
  fluidRow(
    column(
      6,
      selectInput(
        "tost", "Choose TOST to view",
        choices = c("NF", "skew_normal", "gamma", "alldistrs")
      )
    )
  ),
  tabsetPanel(
    id = "tosttab",
    type = "hidden",
    tabPanelBody(
      "NF",
      models_grid("Baseline", c("s3full", "s3pairs")),
      models_grid("Baseline2", c("s3mixfull", "s3mixpairs")),
      models_grid("Baseline3", c("s3recallfull", "s3recallpairs")),
      models_grid("Baseline4", c("s3mixrecallfull", "s3mixrecallpairs")),
      models_grid("Baseline5", c("s4full", "s4pairs")),
      models_grid("Baseline6", c("s4mixfull", "s4mixpairs")),
      models_grid("Baseline7", c("s4recallfull", "s4recallpairs")),
      models_grid("Baseline8", c("s4mixrecallfull", "s4mixrecallpairs")),
      models_grid("Baseline9", c("s3s4full", "s3s4pairs"))
    ),
    tabPanelBody(
      "skew_normal",
      models_grid("Baseline10", c("sns3full", "sns3pairs")),
      models_grid("Baseline11", c("sns3mixfull", "sns3mixpairs")),
      models_grid("Baseline12", c("sns3recallfull", "sns3recallpairs")),
      models_grid("Baseline13", c("sns3mixrecallfull", "sns3mixrecallpairs")),
      models_grid("Baseline14", c("sns4full", "sns4pairs")),
      models_grid("Baseline15", c("sns4mixfull", "sns4mixpairs")),
      models_grid("Baseline16", c("sns4recallfull", "sns4recallpairs")),
      models_grid("Baseline17", c("sns4mixrecallfull", "sns4mixrecallpairs")),
      models_grid("Baseline18", c("sns3s4full", "sns3s4pairs"))
    ),
    tabPanelBody(
      "gamma",
      models_grid("Baseline19", c("gs3full", "gs3pairs")),
      models_grid("Baseline20", c("gs3mixfull", "gs3mixpairs")),
      models_grid("Baseline21", c("gs3recallfull", "gs3recallpairs")),
      models_grid("Baseline22", c("gs3mixrecallfull", "gs3mixrecallpairs")),
      models_grid("Baseline23", c("gs4full", "gs4pairs")),
      models_grid("Baseline24", c("gs4mixfull", "gs4mixpairs")),
      models_grid("Baseline25", c("gs4recallfull", "gs4recallpairs")),
      models_grid("Baseline26", c("gs4mixrecallfull", "gs4mixrecallpairs")),
      models_grid("Baseline27", c("gs3s4full", "gs3s4pairs"))
    ),
    tabPanelBody(
      "alldistrs",
      models_grid("Baseline28", c("alls3full", "alls3pairs")),
      models_grid("Baseline29", c("alls3mixfull", "alls3mixpairs")),
      models_grid("Baseline30", c("alls3recallfull", "alls3recallpairs")),
      models_grid("Baseline31", c("alls3mixrecallfull", "alls3mixrecallpairs")),
      models_grid("Baseline32", c("alls4full", "alls4pairs")),
      models_grid("Baseline33", c("alls4mixfull", "alls4mixpairs")),
      models_grid("Baseline34", c("alls4recallfull", "alls4recallpairs")),
      models_grid("Baseline35", c("alls4mixrecallfull", "alls4mixrecallpairs")),
      models_grid("Baseline36", c("alls3s4full", "alls3s4pairs"))
    )
  )
)
server <- function(input, output, session) {
  observeEvent(input$tost, {
    updateTabsetPanel(inputId = "tosttab", selected = input$tost)
  })
  ## Skew normal
  ##output$Baseline10 <- renderText({"Baseline"})
  output$sns3full <- renderPlot({
    plot_fitted_si(obs = cowling, fitted = snfull[[8]], FALSE, FALSE, FALSE)
  })
  output$sns3pairs <- renderPlot({
    plot_fitted_si(obs = pairs, fitted = snpairs[[8]], FALSE, FALSE, FALSE)
  })


  output$sns3mixfull <- renderPlot({
    plot_fitted_si(obs = cowling, fitted = snfull[[7]], TRUE, FALSE, FALSE)
  })
  output$sns3mixpairs <- renderPlot({
    plot_fitted_si(obs = pairs, fitted = snpairs[[7]], TRUE, FALSE, FALSE)
  })


  output$sns3recallfull <- renderPlot({
    plot_fitted_si(obs = cowling, fitted = snfull[[6]], FALSE, TRUE, FALSE)
  })
  output$sns3recallpairs <- renderPlot({
    plot_fitted_si(obs = pairs, fitted = snpairs[[6]], FALSE, TRUE, FALSE)
  })


  output$sns3mixrecallfull <- renderPlot({
    plot_fitted_si(obs = cowling, fitted = snfull[[5]], TRUE, TRUE, FALSE)
  })
  output$sns3mixrecallpairs <- renderPlot({
    plot_fitted_si(obs = pairs, fitted = snpairs[[5]], TRUE, TRUE, FALSE)
  })


  output$sns4full <- renderPlot({
    plot_fitted_si(obs = cowling, fitted = snfull[[4]], FALSE, FALSE, TRUE)
  })
  output$sns4pairs <- renderPlot({
    plot_fitted_si(obs = pairs, fitted = snpairs[[4]], FALSE, FALSE, TRUE)
  })


  output$sns4mixfull <- renderPlot({
    plot_fitted_si(obs = cowling, fitted = snfull[[3]], TRUE, FALSE, TRUE)
  })
  output$sns4mixpairs <- renderPlot({
    plot_fitted_si(obs = pairs, fitted = snpairs[[3]], TRUE, FALSE, TRUE)
  })


  output$sns4recallfull <- renderPlot({
    plot_fitted_si(obs = cowling, fitted = snfull[[2]], FALSE, TRUE, TRUE)
  })
  output$sns4recallpairs <- renderPlot({
    plot_fitted_si(obs = pairs, fitted = snpairs[[2]], FALSE, TRUE, TRUE)
  })


  output$sns4mixrecallfull <- renderPlot({
    plot_fitted_si(obs = cowling, fitted = snfull[[1]], TRUE, TRUE, TRUE)
  })
  output$sns4mixrecallpairs <- renderPlot({
    plot_fitted_si(obs = pairs, fitted = snpairs[[1]], TRUE, TRUE, TRUE)
  })

}
shinyApp(ui, server)
