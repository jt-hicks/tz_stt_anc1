## Prepare diagnostic dashboard data (council level)

library(magrittr)
library(dplyr)
library(coda)
library(ggplot2)
library(bayesplot)
library(patchwork)

orderly::orderly_shared_resource('create_diag_figs.R')

params <- orderly::orderly_parameters(length = 1000,
                                     burnin_prop = 0.5,
                                     max_councils = NULL,
                                     sample_seed = NULL,
                                     make_plots = TRUE)

run_length <- params$length
burnin_prop <- params$burnin_prop
max_councils <- params$max_councils
sample_seed <- params$sample_seed
make_plots <- isTRUE(params$make_plots)

if (is.numeric(max_councils) && max_councils <= 0) {
  max_councils <- NULL
}
if (is.numeric(sample_seed) && sample_seed <= 0) {
  sample_seed <- NULL
}

orderly::orderly_dependency("compile_results", quote(latest()),
                            c('global_results.rds'))
orderly::orderly_dependency("02_data_quality", quote(latest()),
                            c('dqa_council_monthly_nested.rds'))

source('create_diag_figs.R')

global_results <- readRDS('global_results.rds')
data_list <- readRDS('dqa_council_monthly_nested.rds')

# Build full list of councils (including those without results)
all_councils <- list()
for (zone_name in names(data_list)) {
  for (region_name in names(data_list[[zone_name]])) {
    council_names <- names(data_list[[zone_name]][[region_name]])
    if (length(council_names) == 0) {
      next
    }
    all_councils[[paste(zone_name, region_name, sep = '|')]] <- data.frame(
      zone = zone_name,
      region = region_name,
      council = council_names,
      stringsAsFactors = FALSE
    )
  }
}
all_councils_df <- dplyr::bind_rows(all_councils)
if (!is.null(max_councils) && is.numeric(max_councils) && max_councils > 0) {
  if (!is.null(sample_seed)) {
    set.seed(sample_seed)
  }
  if (nrow(all_councils_df) > max_councils) {
    all_councils_df <- all_councils_df[sample(seq_len(nrow(all_councils_df)), max_councils), ]
  }
}

all_councils_df <- all_councils_df %>%
  dplyr::arrange(zone, region, council)

council_filter <- all_councils_df %>%
  dplyr::distinct(council, zone, region)

# Extract observed prevalence by council
observed_list <- list()
for (i in seq_len(nrow(all_councils_df))) {
  zone <- all_councils_df$zone[i]
  region <- all_councils_df$region[i]
  council <- all_councils_df$council[i]
  obs_data <- data_list[[zone]][[region]][[council]]

  if (is.null(obs_data)) {
    next
  }

  obs_df <- obs_data %>%
    dplyr::select(month, positive, tested) %>%
    dplyr::mutate(
      obs_prevalence = ifelse(tested > 0, positive / tested, NA_real_),
      council = council,
      zone = zone,
      region = region
    )

  observed_list[[council]] <- obs_df
}

observed_all <- dplyr::bind_rows(observed_list)

# Fitted prevalence from model results
fitted_prev <- global_results %>%
  dplyr::filter(measure == "prev_anc_all") %>%
  dplyr::semi_join(council_filter, by = c("council", "zone", "region")) %>%
  dplyr::select(council, zone, region, month, median, lower, upper) %>%
  dplyr::rename(
    fitted_prevalence = median,
    fitted_lower = lower,
    fitted_upper = upper
  )

# Merge observed and fitted for fit metrics
prevalence_comparison <- observed_all %>%
  dplyr::left_join(fitted_prev, by = c("council", "zone", "region", "month"))

fit_metrics <- prevalence_comparison %>%
  dplyr::group_by(council, zone, region) %>%
  dplyr::summarise(
    rmse = sqrt(mean((obs_prevalence - fitted_prevalence)^2, na.rm = TRUE)),
    mae = mean(abs(obs_prevalence - fitted_prevalence), na.rm = TRUE),
    correlation = {
      complete_pairs <- stats::complete.cases(obs_prevalence, fitted_prevalence)
      if (sum(complete_pairs) > 1) {
        stats::cor(obs_prevalence[complete_pairs], fitted_prevalence[complete_pairs])
      } else {
        NA_real_
      }
    },
    mean_obs_prevalence = mean(obs_prevalence, na.rm = TRUE),
    mean_fitted_prevalence = mean(fitted_prevalence, na.rm = TRUE),
    n_months = n_distinct(month),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(rmse, mae, correlation, mean_obs_prevalence, mean_fitted_prevalence),
      ~ ifelse(is.nan(.x), NA_real_, .x)
    )
  )

# Time series data for dashboard
fitted_series <- global_results %>%
  dplyr::filter(measure %in% c("prev_anc_all", "clininc_all", "EIR",
                              "betaa", "eff_moz_pop", "moz2human_ratio", "spz_rate")) %>%
  dplyr::semi_join(council_filter, by = c("council", "zone", "region")) %>%
  dplyr::mutate(series = "fitted") %>%
  dplyr::select(council, zone, region, month, measure, series,
                value = median, lower, upper)

observed_series <- observed_all %>%
  dplyr::mutate(series = "observed",
                measure = "prev_anc_all",
                value = obs_prevalence,
                lower = NA_real_,
                upper = NA_real_) %>%
  dplyr::select(council, zone, region, month, measure, series, value, lower, upper,
                positive, tested)

council_timeseries <- dplyr::bind_rows(
  fitted_series,
  observed_series
)

# Extract MCMC diagnostics and generate plots
dir.create('diagnostic_plots', showWarnings = FALSE)

mcmc_diag_list <- list()
plot_index <- data.frame(
  council = character(),
  plot_file = character(),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(all_councils_df))) {
  zone <- all_councils_df$zone[i]
  region <- all_councils_df$region[i]
  council <- all_councils_df$council[i]

  search_query <- paste0(
    'latest(parameter:name == "', council, '" && ',
    'parameter:zone == "', zone, '" && ',
    'parameter:region == "', region, '" && ',
    'parameter:length == ', run_length, ')'
  )

  result_ids <- tryCatch(orderly::orderly_search(search_query, name = 'run_pmcmc'),
                         error = function(e) character(0))
  if (length(result_ids) == 0) {
    next
  }

  dependency_ok <- tryCatch({
    orderly::orderly_dependency("run_pmcmc", search_query,
                               setNames("result.rds", paste0("data/", council, ".rds")))
    TRUE
  }, error = function(e) {
    FALSE
  })

  if (!dependency_ok) {
    next
  }

  data_file <- paste0('data/', council, '.rds')
  if (!file.exists(data_file)) {
    next
  }

  result <- readRDS(data_file)

  if (is.null(result$mcmc) || nrow(result$mcmc) == 0) {
    next
  }

  chain_len <- nrow(result$mcmc)
  start_chain <- floor(burnin_prop * chain_len) + 1
  if (start_chain < 1) start_chain <- 1
  if (start_chain > chain_len) start_chain <- chain_len

  mcmc_post <- result$mcmc[start_chain:chain_len, , drop = FALSE]

  ar <- 1 - coda::rejectionRate(coda::as.mcmc(mcmc_post))
  ess <- coda::effectiveSize(coda::as.mcmc(mcmc_post))

  mcmc_diag_list[[council]] <- data.frame(
    council = council,
    zone = zone,
    region = region,
    acceptance_min = min(ar, na.rm = TRUE),
    ess_min = min(ess, na.rm = TRUE),
    n_iterations = chain_len,
    burnin_start = start_chain,
    stringsAsFactors = FALSE
  )

  # Generate diagnostic plots
  if (make_plots) {
    tryCatch({
      create_diag_figs(result,
                       country = region,
                       district = council,
                       name = council,
                       burnin = burnin_prop)

      plot_file <- paste0(council, '-', gsub(' ', '_', council), '-',
                          gsub(' ', '_', region), '.png')
      target_file <- file.path('diagnostic_plots', plot_file)
      if (file.exists(plot_file)) {
        file.rename(plot_file, target_file)
        plot_index <- dplyr::bind_rows(plot_index,
                                       data.frame(council = council,
                                                  plot_file = target_file,
                                                  stringsAsFactors = FALSE))
      }
    }, error = function(e) {
      message(paste('Warning creating diag figs for', council, ':', e$message))
    })
  }
}

mcmc_diagnostics <- dplyr::bind_rows(mcmc_diag_list)

council_summary <- all_councils_df %>%
  dplyr::left_join(fit_metrics, by = c("council", "zone", "region")) %>%
  dplyr::left_join(mcmc_diagnostics, by = c("council", "zone", "region")) %>%
  dplyr::mutate(has_results = !is.na(rmse))

# Save outputs
saveRDS(council_summary, 'council_summary.rds')
saveRDS(council_timeseries, 'council_timeseries.rds')
saveRDS(prevalence_comparison, 'prevalence_comparison.rds')
saveRDS(mcmc_diagnostics, 'council_mcmc_diagnostics.rds')
saveRDS(plot_index, 'diagnostic_plot_index.rds')

cat("\n========================================\n")
cat("DIAGNOSTIC DASHBOARD DATA PREP SUMMARY\n")
cat("========================================\n")
cat(paste("Total councils:", nrow(all_councils_df), "\n"))
cat(paste("Councils with results:", sum(council_summary$has_results, na.rm = TRUE), "\n"))
cat(paste("Councils with diagnostics:", nrow(mcmc_diagnostics), "\n"))
cat(paste("Plots generated:", make_plots, "\n"))
cat("========================================\n\n")
