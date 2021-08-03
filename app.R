library(ggplot2)
library(purrr)
library(shiny)
source("R/utils.R")
cowling <- readRDS("data/cowling_data_cleaned.rds")
pairs <- readRDS("data/cowling_data_pairs.rds")
s3s4pairs_obs <- readRDS("data/cowling_data_s3s4_pairs.rds")
s3s4_obs <- readRDS("data/cowling_data_s3s4.rds")

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
    column(6, HTML("<h2>Model fitted to full data-set</h2>")),
    column(6, HTML("<h2>Model fitted to  transmission pairs</h2>"))
  ),
  fluidRow(
    column(
      6,
      div(
        id = "full-container",
        uiOutput(outputId = "plots")
      )
    ),
    column(
      6,
      div(
        id = "pairs-container",
        uiOutput(outputId = "pairplots")
      )
    )
  ),
  fluidRow(
    column(
      6,
      div(
        id = "full-s3s4",
        uiOutput(outputId = "s3s4")
      )
    ),
    column(
      6,
      div(
        id = "pairs-s3s4",
        uiOutput(outputId = "s3s4pairs")
      )
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

    s3s4pairs_graphs <- eventReactive(input$submit, {
      req(wq_data())

      pmap(
        list(
          fitted = wq_data()[[4]],
          mixture = model_features$mixture[model_features$right_bias],
          recall = model_features$recall[model_features$right_bias]
        ), function(fitted, mixture, recall) {
          title <- "S3S4"
          if (mixture) title <- paste(title, " + MIXTURE")
          if (recall) title <- paste(title, " + RECALL")
          plot_fitted_si(
            s3s4pairs_obs, fitted, mixture, recall, TRUE, title
          )
        }
      )
    })

    s3s4_graphs <- eventReactive(input$submit, {
      req(wq_data())

      pmap(
        list(
          fitted = wq_data()[[3]],
          mixture = model_features$mixture[model_features$right_bias],
          recall = model_features$recall[model_features$right_bias]
        ), function(fitted, mixture, recall) {
          title <- "S3S4"
          if (mixture) title <- paste(title, " + MIXTURE")
          if (recall) title <- paste(title, " + RECALL")
          plot_fitted_si(
            s3s4_obs, fitted, mixture, recall, TRUE, title
          )
        }
      )
    })

    # use purrr::iwalk to create a dynamic number of outputs
    observeEvent(input$submit, {
      req(full_graphs())
      req(pairs_graphs())
      req(s3s4_graphs())
      req(s3s4pairs_graphs())

      iwalk(full_graphs(), function(f, index) {
        output_name <- paste0("plot_full_", index)
        output[[output_name]] <- renderPlot(f)
      })
      iwalk(pairs_graphs(), function(p, index) {
        output_name <- paste0("plot_pairs_", index)
        output[[output_name]] <- renderPlot(p)
      })
      iwalk(s3s4_graphs(), function(p, index) {
        output_name <- paste0("plot_s3s4_", index)
        output[[output_name]] <- renderPlot(p)
      })
      iwalk(s3s4pairs_graphs(), function(p, index) {
        output_name <- paste0("plot_s3s4pairs_", index)
        output[[output_name]] <- renderPlot(p)
      })
    })

    # use renderUI to create a dynamic number of output ui elements
    output$plots <- renderUI({
      req(full_graphs())
      plots_list <- imap(
        full_graphs(),
        function(f, i) {
        tagList(
          plotOutput(
            outputId = paste0("plot_full_", i)
          ),
          br()
        )
      })
      tagList(plots_list)
    })

    output$pairplots <- renderUI({
      req(pairs_graphs())
      plots_list <- imap(
        pairs_graphs(),
        function(f, i) {
        tagList(
          plotOutput(
            outputId = paste0("plot_pairs_", i)
          ),
          br()
        )
      })
      tagList(plots_list)
    })

    output$s3s4 <- renderUI({
      req(s3s4_graphs())
      plots_list <- imap(
        s3s4_graphs(),
        function(f, i) {
        tagList(
          plotOutput(
            outputId = paste0("plot_s3s4_", i)
          ),
          br()
        )
      })
      tagList(plots_list)
    })

    output$s3s4pairs <- renderUI({
      req(s3s4pairs_graphs())
      plots_list <- imap(
        s3s4pairs_graphs(), function(f, i) {
        tagList(
          plotOutput(
            outputId = paste0("plot_s3s4pairs_", i)
          ),
          br()
        )
      })
      tagList(plots_list)
    })


  }
)
shinyApp(ui, server)
