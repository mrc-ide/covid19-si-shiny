pretty_mu <- function(x) {
  paste0(" Mean:\n", format(round(x, 1), nsmall = 1), " days")
}

## Obs: observed data
## fitted: output of processed stanfit
## has 2 named components:
## conditional and unconditional
plot_fitted_si <- function(obs, fitted, mixture, recall, right_bias) {

  obs_mu <- mean(obs$si)
  cndtnl_mu <- mean(fitted[["conditional"]])
  uncndtnl_mu <- mean(fitted[["unconditional"]])
  p <- ggplot() +
    geom_histogram(
      data = obs, aes(si, y = ..density.., fill = "gray77"),
      alpha = 0.8,
      binwidth = 1
    ) +
    geom_density(
      data = NULL,
      aes(fitted[["unconditional"]], fill = "red"),
      alpha = 0.3, colour = NA
    ) +
    geom_vline(
      xintercept = obs_mu, col = "gray77", linetype = "dashed"
    ) +
    geom_vline(
      xintercept = uncndtnl_mu, col = "red", linetype = "dashed"
    ) +
    annotate(
      geom = "text", x = uncndtnl_mu, y = 0.055, color = "red",
      label = pretty_mu(uncndtnl_mu),
      hjust = -0.1
    ) +
    scale_fill_identity(
      guide = "legend",
      labels = c("Data", "Posterior - True", "Posterior - Expected"),
      breaks = c("gray77", "red", "blue")
    ) +
    theme_minimal() +
    xlab("Serial Interval (days)") +
    theme(legend.title = element_blank()) +
    guides(
      fill = guide_legend(override.aes = list(alpha = c(0.8, 0.3)))
    ) +
    theme(legend.position = "top")


  if (mixture | recall | right_bias) {
    p <- p +
      geom_density(
        data = NULL,
        aes(fitted[["conditional"]], fill = "blue"),
        alpha = 0.3, colour = NA
    ) +
    geom_vline(
      xintercept = cndtnl_mu, col = "blue", linetype = "dashed"
    ) +
    annotate(
      geom = "text", x = cndtnl_mu, y = 0.095, color = "blue",
      label = pretty_mu(cndtnl_mu),
      hjust = -0.1
    ) +
    guides(
      fill = guide_legend(override.aes = list(alpha = c(0.8, 0.3, 0.3)))
    )
  }
  p
}
