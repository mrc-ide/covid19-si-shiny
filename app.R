library(ggplot2)
library(purrr)
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
        choices = c("skew_normal")
      )
    ),
    column(
      4,
      actionButton(
        inputId = "submit",
        label = "Submit",
        style = "margin:40px;"
      )
    )
  ),
  fluidRow(
    div(
      id = "plot-container",
      uiOutput(outputId = "plots")
    )
  )
)

server <- shinyServer(
  function(input, output, session){
    session$onSessionEnded(stopApp)

    # query data from USGS API
    wq_data <- eventReactive(input$submit, {
      req(input$tost)
      if (input$tost == "skew_normal") {
        out <- list(
          full = snfull, pairs = snpairs,
          s3s4 = sns3s4, s3s4pairs = sns3s4pairs
        )
      } else if (input$tost == "NF") {
        out <- list(
          full = nffull, pairs = nfpairs,
          s3s4 = nfs3s4, s3s4pairs = nfs3s4pairs
        )
      } else if (input$tost == "gamma") {
        out <- list(
          full = gfull, pairs = gpairs,
          s3s4 = gs3s4, s3s4pairs = gs3s4pairs
        )
      }
      out
    })

    # create a list of graphs - with one for each parameter selected
    full_graphs <- eventReactive(input$submit, {
      req(wq_data())

      pmap(
        list(
          fitted = wq_data()[[1]],
          mixture = model_features$mixture,
          recall = model_features$recall,
          right_bias= model_features$right_bias
        ), function(fitted, mixture, recall, right_bias) {
          plot_fitted_si(
            cowling, fitted, mixture, recall, right_bias
          )
        }
      )
    })

    pairs_graphs <- eventReactive(input$submit, {
      req(wq_data())

      pmap(
        list(
          fitted = wq_data()[[2]],
          mixture = model_features$mixture,
          recall = model_features$recall,
          right_bias= model_features$right_bias
        ), function(fitted, mixture, recall, right_bias) {
          plot_fitted_si(
            cowling, fitted, mixture, recall, right_bias
          )
        }
      )
    })

    # use purrr::iwalk to create a dynamic number of outputs
    observeEvent(input$submit, {
      req(full_graphs())
      req(pairs_graphs())
      pwalk(
        list(
          full = full_graphs(), pairs = pairs_graphs(),
          index = seq_len(8)
        ), function(full, pairs, index) {
          output_name <- paste0("plot_full_", index)
          output[[output_name]] <- renderPlot(full)

          output_name <- paste0("plot_pairs_", index)
          output[[output_name]] <- renderPlot(pairs)

      })
    })

    # use renderUI to create a dynamic number of output ui elements
    output$plots <- renderUI({
      req(full_graphs())
      req(pairs_graphs())
      full <- full_graphs()
      pairs <- pairs_graphs()
      index <- seq_along(full)
      plots_list <- pmap(
        list(f = full, p = pairs, i = index),
        function(f, p, i) {
        tagList(
          plotOutput(
            outputId = paste0("plot_full_", i)
          ),
          br(),
          plotOutput(
            outputId = paste0("plot_pairs_", i)
          ),
          br()
        )
      })

      tagList(plots_list)
    })

  }
)
shinyApp(ui, server)
