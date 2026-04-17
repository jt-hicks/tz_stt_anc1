create_diag_figs <- function(result, name, country, district, burnin = 0) {
  # Required plotting packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")
  if (!requireNamespace("bayesplot", quietly = TRUE)) stop("bayesplot required")
  if (!requireNamespace("patchwork", quietly = TRUE)) stop("patchwork required")

  chain_len <- nrow(result$mcmc)
  start_chain <- round(burnin * chain_len) + 1
  if (start_chain < 1) start_chain <- 1
  if (start_chain > chain_len) start_chain <- chain_len
  mcmc <- result$mcmc[start_chain:chain_len, , drop = FALSE]

  print('acceptance rate')
  ar <- 1 - coda::rejectionRate(coda::as.mcmc(mcmc))
  print(ar)
  print('effective size')
  ess <- coda::effectiveSize(coda::as.mcmc(mcmc))
  print(ess)

  title <- paste0('Diagnostic plots for seasonal model - ', district, ', ', country)
  pars_list <- colnames(mcmc)

  trace_plots <- lapply(pars_list, function(x) {
    p <- bayesplot::mcmc_trace(mcmc, pars = x)
    p <- p + ggplot2::ggtitle(paste0(x, ' / AR: ', round(ar[x], 3), ' / ESS: ', round(ess[x], 1))) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 6), axis.title.y = ggplot2::element_blank())
    p
  })

  dense_plots <- lapply(pars_list, function(x) {
    p <- bayesplot::mcmc_dens(mcmc, pars = x)
    p <- p + ggplot2::ggtitle(paste0(x, ' / AR: ', round(ar[x], 2), ' / ESS: ', round(ess[x], 1))) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 6), axis.title.x = ggplot2::element_blank())
    p
  })

  # Combine plots safely
  npars <- length(pars_list)
  if (npars == 0) {
    warning('No parameters to plot')
    return(NULL)
  }

  pairs <- lapply(seq_len(npars), function(i) trace_plots[[i]] + dense_plots[[i]])

  # Arrange rows of pairs
  row_plots <- list()
  i <- 1
  while (i <= length(pairs)) {
    if (i == length(pairs)) {
      row_plots <- c(row_plots, pairs[i])
      i <- i + 1
    } else {
      row_plots <- c(row_plots, list(pairs[[i]] + pairs[[i + 1]]))
      i <- i + 2
    }
  }

  combined <- Reduce('/', row_plots)
  combined <- combined + patchwork::plot_layout(guides = 'collect') + patchwork::plot_annotation(title = title)

  out_file <- paste0(name, '-', gsub(' ', '_', district), '-', gsub(' ', '_', country), '.png')
  ggplot2::ggsave(filename = out_file, plot = combined, dpi = 300, height = 11, width = 8, units = 'in')
  return(combined)
}
