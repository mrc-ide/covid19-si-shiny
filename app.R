library(ggplot2)
library(shiny)
source("R/utils.R")
cowling <- readRDS("data/cowling_data_cleaned.rds")
pairs <- readRDS("data/cowling_data_pairs.rds")
s3s4pairs_obs <- readRDS("data/cowling_data_s3s4_pairs.rds")
snfull <- readRDS("processed_stanfits/skew_normal/release/best_si_skew_normal.rds")
snpairs <- readRDS("processed_stanfits/skew_normal/discrete_pairs/best_si_skew_normal.rds")
sns3s4 <-readRDS("processed_stanfits/skew_normal/s3s4/best_si_skew_normal.rds")
sns3s4pairs <-readRDS("processed_stanfits/skew_normal/s3s4pairs/best_si_skew_normal.rds")

model_features <- list(
  "mixture" = c(TRUE, FALSE), "recall"  = c(TRUE, FALSE),
  "right_bias" = c(TRUE, FALSE)
)

model_features <- expand.grid(model_features)

models_grid <- function(ids) {
  fluidRow(
    column(6, plotOutput(ids[1])),
    column(6, plotOutput(ids[2]))
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
      fluidRow(
        column(6, HTML("<h2>Model fitted to full data-set</h2>")),
        column(6, HTML("<h2>Model fitted to  transmission pairs</h2>"))
      ),
      models_grid(c("s3full", "s3pairs")),
      models_grid(c("s3mixfull", "s3mixpairs")),
      models_grid(c("s3recallfull", "s3recallpairs")),
      models_grid(c("s3mixrecallfull", "s3mixrecallpairs")),
      models_grid(c("s4full", "s4pairs")),
      models_grid(c("s4mixfull", "s4mixpairs")),
      models_grid(c("s4recallfull", "s4recallpairs")),
      models_grid(c("s4mixrecallfull", "s4mixrecallpairs")),
      models_grid(c("s3s4full", "s3s4pairs"))
    ),
    tabPanelBody(
      "skew_normal",
      fluidRow(
        column(6, HTML("<h2>Model fitted to full data-set</h2>")),
        column(6, HTML("<h2>Model fitted to  transmission pairs</h2>"))
      ),
      models_grid(c("sns3full", "sns3pairs")),
      models_grid(c("sns3mixfull", "sns3mixpairs")),
      models_grid(c("sns3recallfull", "sns3recallpairs")),
      models_grid(c("sns3mixrecallfull", "sns3mixrecallpairs")),
      models_grid(c("sns4full", "sns4pairs")),
      models_grid(c("sns4mixfull", "sns4mixpairs")),
      models_grid(c("sns4recallfull", "sns4recallpairs")),
      models_grid(c("sns4mixrecallfull", "sns4mixrecallpairs")),
      models_grid(c("sns3s4full", "sns3s4pairs"))
    ),
    tabPanelBody(
      "gamma",
      models_grid(c("gs3full", "gs3pairs")),
      models_grid(c("gs3mixfull", "gs3mixpairs")),
      models_grid(c("gs3recallfull", "gs3recallpairs")),
      models_grid(c("gs3mixrecallfull", "gs3mixrecallpairs")),
      models_grid(c("gs4full", "gs4pairs")),
      models_grid(c("gs4mixfull", "gs4mixpairs")),
      models_grid(c("gs4recallfull", "gs4recallpairs")),
      models_grid(c("gs4mixrecallfull", "gs4mixrecallpairs")),
      models_grid(c("gs3s4full", "gs3s4pairs"))
    ),
    tabPanelBody(
      "alldistrs",
      models_grid(c("alls3full", "alls3pairs")),
      models_grid(c("alls3mixfull", "alls3mixpairs")),
      models_grid(c("alls3recallfull", "alls3recallpairs")),
      models_grid(c("alls3mixrecallfull", "alls3mixrecallpairs")),
      models_grid(c("alls4full", "alls4pairs")),
      models_grid(c("alls4mixfull", "alls4mixpairs")),
      models_grid(c("alls4recallfull", "alls4recallpairs")),
      models_grid(c("alls4mixrecallfull", "alls4mixrecallpairs")),
      models_grid(c("alls3s4full", "alls3s4pairs"))
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

  output$sns3s4pairs <- renderPlot({
    plot_fitted_si(obs = s3s4pairs_obs, fitted = sns3s4pairs[[1]], FALSE, FALSE, TRUE, "S3-S4 + ")
  })

}
shinyApp(ui, server)
