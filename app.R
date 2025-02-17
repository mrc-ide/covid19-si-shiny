## https://tbradley1013.github.io/2018/08/10/create-a-dynamic-number-of-ui-elements-in-shiny-with-purrr/
library(ggplot2)
library(purrr)
library(reactable)
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


nffull <- readRDS("processed_stanfits/nf/release/best_si_nf.rds")
nfpairs <- readRDS("processed_stanfits/nf/discrete_pairs/best_si_nf.rds")
nfs3s4 <-readRDS("processed_stanfits/nf/s3s4/best_si_nf.rds")
##nfs3s4pairs <- readRDS("processed_stanfits/nf/s3s4pairs/best_si_nf.rds")
nfs3s4pairs <- nfs3s4

gfull <- readRDS("processed_stanfits/gamma/release/best_si_gamma.rds")
gpairs <- readRDS("processed_stanfits/gamma/discrete_pairs/best_si_gamma.rds")
gs3s4 <-readRDS("processed_stanfits/gamma/s3s4/best_si_gamma.rds")
gs3s4pairs <-readRDS("processed_stanfits/gamma/s3s4pairs/best_si_gamma.rds")

overall_t2 <- readRDS(
  "processed_stanfits/compare_all/s3s4/table2_all_distrs.rds"
)

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
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  h2(id = "title-panel", "Robust estimates of the infectiousness profile and serial interval distributions of SARS-CoV-2"),
  tabsetPanel(
    tabPanel(
      title = "Model fits to observed data",
      fluidRow(
        column(
          6,
        selectInput(
          "tost", "Choose TOST to view",
          choices = c(`Skew normal` = "skew_normal", gamma = "gamma", NF = "NF"),
          selected = "NF"
        )
      )
    ),
    fluidRow(
      column(6, HTML("<h2>Model fitted to full data-set</h2>")),
      column(6, HTML("<h2>Model fitted to  transmission pairs</h2>"))
    ),
    fluidRow(
      column(6, div(id = "full-container", uiOutput(outputId = "plots"))
    ),
    column(6, div(id = "pairs-container", uiOutput(outputId = "pairplots")))
  ),
  fluidRow(
    column(6, div(id = "full-s3s4", uiOutput(outputId = "s3s4"))),
    column(6, div(id = "pairs-s3s4", uiOutput(outputId = "s3s4pairs")))
  )
  ),
  tabPanel(title = "Summary Tables", reactableOutput("summary")),
  type = "pills"
  )

)

server <- shinyServer(
  function(input, output, session){
    session$onSessionEnded(stopApp)

    # query data from USGS API
    wq_data <- reactive({
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
    full_graphs <- reactive({
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

    pairs_graphs <- reactive({
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

    s3s4pairs_graphs <- reactive({
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

    s3s4_graphs <- reactive({
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

    # use renderUI to create a dynamic number of output ui elements
    output$plots <- renderUI({
      req(full_graphs())
      plots_list <- imap(
        full_graphs(),
        function(f, i) {
        output_name <- paste0("plot_full_", i)
        output[[output_name]] <- renderPlot(f)
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
        output_name <- paste0("plot_pairs_", i)
        output[[output_name]] <- renderPlot(f)
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
        output_name <- paste0("plot_s3s4_", i)
        output[[output_name]] <- renderPlot(f)
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
        output_name <- paste0("plot_s3s4pairs_", i)
        output[[output_name]] <- renderPlot(f)
        tagList(
          plotOutput(
            outputId = paste0("plot_s3s4pairs_", i)
          ),
          br()
        )
      })
      tagList(plots_list)
    })

    output$summary <- renderReactable({
      reactable(overall_t2)
    })
  }
)
shinyApp(ui, server)
