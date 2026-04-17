## Summarise short-run PMCMC results
## Extracts posterior estimates (median and 95% CIs) for key measures
## across retained MCMC samples (post-burnin)
## Parameterized by zone and region to match run_diagnostics pattern

library(magrittr)

orderly::orderly_dependency("02_data_quality", quote(latest()),
                            c('dqa_council_monthly_nested.rds'))
''
params <- orderly::orderly_parameters(zone = NULL,
                                     region = NULL,
                                     length = 1000,
                                     burnin_prop = 0.5,
                                     posterior_sample_n = 100)

zone <- params$zone
region <- params$region
length <- params$length
burnin_prop <- params$burnin_prop
posterior_sample_n <- params$posterior_sample_n

data_list <- readRDS('dqa_council_monthly_nested.rds')

# Get months vector
months <- data_list[[zone]][[region]][[1]]$month
if (inherits(months, "Date")) {
  months <- as.Date(format(months, "%Y-%m-01"))
} else {
  month_numeric <- as.numeric(months)
  year_part <- floor(month_numeric)
  month_part <- round((month_numeric - year_part) * 12) + 1
  month_part <- pmin(pmax(month_part, 1), 12)
  months <- as.Date(sprintf("%04d-%02d-01", year_part, month_part))
}

# Define measures to extract
measure_names <- c("prev_anc_all", "prev_05", "clininc_all", "clininc_05", "EIR", 
                   "betaa", "eff_moz_pop", "moz2human_ratio", "spz_rate")

# Get all council names for this zone/region
names_list <- names(data_list[[zone]][[region]])

# Extract results for each council
results_list <- list()
posterior_samples_list <- list()
prop_df <- data.frame(name = character(),
                      prop = numeric(),
                      stringsAsFactors = FALSE)
processed_councils <- character()
missing_councils <- data.frame(council = character(),
                                reason = character(),
                                stringsAsFactors = FALSE)

for (name in names_list) {
    # Find the latest run_pmcmc result for this council/zone/region in the archive
    search_query <- paste0('latest(parameter:name == "', name, '" && parameter:zone == "', zone, '" && parameter:region == "', region, '" && parameter:length == ', length, ')')

    result_ids <- tryCatch(orderly::orderly_search(search_query, name = 'run_pmcmc'), error = function(e) character(0))
    if (length(result_ids) == 0) {
      missing_councils <- dplyr::bind_rows(missing_councils,
                                           data.frame(council = name,
                                                      reason = 'No run_pmcmc result found in archive',
                                                      stringsAsFactors = FALSE))
      next
    } else {
      # Try to load the result; if dependency fails, skip this council
      dependency_ok <- tryCatch({
        orderly::orderly_dependency("run_pmcmc", search_query,
                                   c('data/${name}.rds' = 'result.rds'))
        TRUE
      }, error = function(e) {
        missing_councils <<- dplyr::bind_rows(missing_councils,
                                              data.frame(council = name,
                                                         reason = paste('orderly_dependency error:', e$message),
                                                         stringsAsFactors = FALSE))
        FALSE
      })
      
      if (!dependency_ok) {
        next
      }
      
      data_file_path <- paste0('data/', name, '.rds')
      if (!file.exists(data_file_path)) {
        missing_councils <- dplyr::bind_rows(missing_councils,
                                             data.frame(council = name,
                                                        reason = 'run_pmcmc result file missing in archive',
                                                        stringsAsFactors = FALSE))
        next
      }
    }    

    print(name)

    tryCatch({
      result <- readRDS(data_file_path)

      # Get dimensions
      num_months <- length(result$history[1, 1, ])
      start_month_index <- (num_months - length(months) + 1)
      chain_length <- length(result$history[1, , 1])
      burnin_end_index <- floor((chain_length * burnin_prop)) + 1

      # Create summary and posterior sample draws for each measure
      history_outputs <- lapply(measure_names, function(y) {
        history_long <- as.data.frame(t(result$history[y, burnin_end_index:chain_length,
                                                     start_month_index:num_months])) %>%
          dplyr::mutate(month = months) %>%
          reshape2::melt(id = "month")

        summary_df <- history_long %>%
          dplyr::group_by(month) %>%
          dplyr::summarise(
            median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            measure = y,
            council = name
          )

        sample_df <- history_long %>%
          dplyr::group_by(month) %>%
          dplyr::group_modify(~ {
            sampled_idx <- sample(seq_len(nrow(.x)),
                                  size = posterior_sample_n,
                                  replace = nrow(.x) < posterior_sample_n)
            data.frame(draw = seq_len(posterior_sample_n),
                       value = .x$value[sampled_idx],
                       stringsAsFactors = FALSE)
          }) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            measure = y,
            council = name
          )

        list(summary = summary_df, samples = sample_df)
      })

      df <- dplyr::bind_rows(lapply(history_outputs, function(x) x$summary))
      posterior_df <- dplyr::bind_rows(lapply(history_outputs, function(x) x$samples))

      if (!is.null(df) && nrow(df) > 0) {
        results_list[[name]] <- df
        posterior_samples_list[[name]] <- posterior_df

        # Store parameter variance for diagnostics
        temp_df <- data.frame(
          name = name,
          prop = as.numeric(var(result$pars)),
          stringsAsFactors = FALSE
        )
        prop_df <- dplyr::bind_rows(prop_df, temp_df)
        processed_councils <- c(processed_councils, name)
      }

    }, error = function(e) {
      print(paste("Error processing", name, ":", e$message))
      # Record missing council with error reason
      missing_councils <<- dplyr::bind_rows(missing_councils,
                                            data.frame(council = name,
                                                       reason = e$message,
                                                       stringsAsFactors = FALSE))
    })
}

# Combine all results
results_all <- dplyr::bind_rows(results_list)
posterior_samples_all <- dplyr::bind_rows(posterior_samples_list)

# Check for councils with no results
councils_not_processed <- setdiff(names_list, processed_councils)
if (length(councils_not_processed) > 0) {
  for (council in councils_not_processed) {
    # Check if it's already in missing_councils (from error)
    if (!council %in% missing_councils$council) {
      missing_councils <- dplyr::bind_rows(missing_councils,
                                           data.frame(council = council,
                                                      reason = "No result file found",
                                                      stringsAsFactors = FALSE))
    }
  }
}

# Print summary report
cat("\n========================================\n")
cat("EXTRACTION SUMMARY\n")
cat("========================================\n")
cat(paste("Zone:", zone, "\n"))
cat(paste("Region:", region, "\n"))
cat(paste("Run length:", length, "\n"))
cat(paste("Burnin proportion:", burnin_prop, "\n"))
cat(paste("Posterior sample size per council-month-measure:", posterior_sample_n, "\n"))
cat("----------------------------------------\n")
cat(paste("Total councils expected:", length(names_list), "\n"))
cat(paste("Successfully processed:", nrow(prop_df), "\n"))
cat(paste("Missing/Failed:", nrow(missing_councils), "\n"))
cat("----------------------------------------\n")

if (nrow(missing_councils) > 0) {
  cat("WARNING: The following councils have missing results:\n")
  for (i in seq_len(nrow(missing_councils))) {
    cat(paste("  •", missing_councils$council[i], 
              "- Reason:", missing_councils$reason[i], "\n"))
  }
  cat("\nThese councils require attention - rerun MCMC or check for errors.\n")
} else {
  cat("✓ All councils successfully processed!\n")
}
cat("========================================\n\n")

# Save outputs
saveRDS(results_all, 'results_summary.rds')
saveRDS(posterior_samples_all, 'results_posterior_sample.rds')
saveRDS(prop_df, 'props.rds')
saveRDS(missing_councils, 'missing_councils.rds')

print(paste("Summary complete. Processed", nrow(prop_df), "councils."))
