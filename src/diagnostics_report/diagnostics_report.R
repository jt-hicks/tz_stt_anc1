## Create diagnostics report from run_diagnostics tasks
## Analyzes parameter variance and model convergence
## Also compares observed vs predicted prevalence

library(dplyr)
library(ggplot2)

# Declare dependencies
orderly::orderly_dependency("02_data_quality", quote(latest()),
                             c('dqa_council_monthly_nested.rds'))

# Define all zone/region combinations
zone_region_combos <- list(
  Lake = c('Kagera Region', 'Mwanza Region', 'Geita Region', 'Mara Region', 'Simiyu Region', 'Shinyanga Region'),
  Western = c('Tabora Region', 'Kigoma Region'),
  Northern = c('Kilimanjaro Region', 'Tanga Region', 'Arusha Region'),
  Central = c('Dodoma Region', 'Singida Region', 'Manyara Region'),
  `Southern Highlands` = c('Iringa Region', 'Njombe Region', 'Ruvuma Region'),
  Southern = c('Lindi Region', 'Mtwara Region'),
  `Southwest Highlands` = c('Mbeya Region', 'Rukwa Region', 'Katavi Region', 'Songwe Region'),
  Eastern = c('Dar Es Salaam Region', 'Pwani Region', 'Morogoro Region')
)

# Collect all diagnostics
all_props <- list()
diag_log <- data.frame(
  zone = character(),
  region = character(),
  status = character(),
  councils = numeric(),
  error = character(),
  stringsAsFactors = FALSE
)

cat("Aggregating diagnostics from all zones and regions...\n\n")

for (zone in names(zone_region_combos)) {
  for (region in zone_region_combos[[zone]]) {
    
    cat(paste("Processing diagnostics:", zone, "-", region, "..."))
    
    tryCatch({
      # Search for latest run_diagnostics for this zone/region
      search_query <- paste0('name == "run_diagnostics" && ',
                            'parameter:zone == "', zone, '" && ',
                            'parameter:region == "', region, '"')
      
      result_ids <- orderly::orderly_search(search_query)
      
      if (length(result_ids) == 0) {
        cat(" SKIPPED (no diagnostics found)\n")
        diag_log <- bind_rows(diag_log,
                             data.frame(zone = zone,
                                       region = region,
                                       status = "skipped",
                                       councils = 0,
                                       error = "No run_diagnostics found",
                                       stringsAsFactors = FALSE))
        next
      }
      
        # Use most recent
        result_id <- result_ids[1]
      
      # Load the props
      file_path <- paste0('./archive/run_diagnostics/', result_id, '/props.RDS')
      
      if (!file.exists(file_path)) {
        cat(" ERROR (file not found)\n")
        diag_log <- bind_rows(diag_log,
                             data.frame(zone = zone,
                                       region = region,
                                       status = "error",
                                       councils = 0,
                                       error = "props.RDS not found",
                                       stringsAsFactors = FALSE))
        next
      }
      
      props <- readRDS(file_path)
      
      # Add zone/region columns
      props <- props %>%
        mutate(zone = zone, 
               region = region,
               .before = name)
      
      # Store
      all_props[[paste0(zone, "_", region)]] <- props
      
      cat(paste(" OK (", nrow(props), " councils)\n", sep = ""))
      
      diag_log <- bind_rows(diag_log,
                           data.frame(zone = zone,
                                     region = region,
                                     status = "success",
                                     councils = nrow(props),
                                     error = NA_character_,
                                     stringsAsFactors = FALSE))
      
    }, error = function(e) {
      cat(paste(" ERROR -", e$message, "\n"))
      diag_log <<- bind_rows(diag_log,
                            data.frame(zone = zone,
                                      region = region,
                                      status = "error",
                                      councils = 0,
                                      error = e$message,
                                      stringsAsFactors = FALSE))
    })
  }
}

# Combine all diagnostics
global_diagnostics <- bind_rows(all_props)

cat("\n========================================\n")
cat("DIAGNOSTICS REPORT\n")
cat("========================================\n")
cat(paste("Total zone/region combinations:", nrow(diag_log), "\n"))
cat(paste("Successfully processed:", sum(diag_log$status == "success"), "\n"))
cat(paste("Total councils analyzed:", nrow(global_diagnostics), "\n"))
cat("----------------------------------------\n\n")

# Parameter variance summary
cat("PARAMETER VARIANCE SUMMARY\n")
cat("(Higher variance = worse convergence)\n")
cat("----------------------------------------\n")

if (nrow(global_diagnostics) == 0) {
  cat("\nNo diagnostics data found. Run run_diagnostics for your zones/regions first.\n")
  summary_stats <- data.frame()
} else {
  summary_stats <- global_diagnostics %>%
    group_by(zone, region) %>%
  summarise(
    n_councils = n(),
    min_var = min(prop),
    max_var = max(prop),
    mean_var = mean(prop),
    median_var = median(prop),
    sd_var = sd(prop),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_var))
  
  print(summary_stats, n = Inf)
}

cat("\n========================================\n")
cat("COUNCILS WITH HIGHEST PARAMETER VARIANCE\n")
cat("(These may need longer runs or better proposal matrices)\n")
cat("----------------------------------------\n")

if (nrow(global_diagnostics) > 0) {
  top_problematic <- global_diagnostics %>%
    arrange(desc(prop)) %>%
    head(20) %>%
    select(zone, region, name, prop)
  print(top_problematic, n = Inf)
} else {
  cat("No data available\n")
}

cat("\n========================================\n")
cat("BEST-CONVERGING COUNCILS\n")
cat("(Low variance = good convergence)\n")
cat("----------------------------------------\n")

if (nrow(global_diagnostics) > 0) {

best_converging <- global_diagnostics %>%
    arrange(prop) %>%
    head(20) %>%
    select(zone, region, name, prop)
  print(best_converging, n = Inf)
} else {
  cat("No data available\n")
}

cat("\n========================================\n\n")

# ========================================
# PREVALENCE FIT ANALYSIS
# Compare observed vs predicted prevalence
# ========================================

cat("PREVALENCE FIT ANALYSIS\n")
cat("Comparing observed vs model-predicted prevalence by council\n")
cat("----------------------------------------\n")

prevalence_comparison <- NULL

tryCatch({
  # Load observed prevalence from 02_data_quality (via dependency)
  obs_data <- readRDS('dqa_council_monthly_nested.rds')
  
  # Collect predicted prevalence from summarise_results tasks
  pred_data_list <- list()
  pred_log <- data.frame(
    zone = character(),
    region = character(),
    status = character(),
    error = character(),
    stringsAsFactors = FALSE
  )
  
  for (zone in names(zone_region_combos)) {
    for (region in zone_region_combos[[zone]]) {
      
      tryCatch({
        # Search for latest summarise_results for this zone/region
        search_query <- paste0('name == "summarise_results" && ',
                              'parameter:zone == "', zone, '" && ',
                              'parameter:region == "', region, '"')
        
        result_ids <- orderly::orderly_search(search_query)
        
        if (length(result_ids) == 0) {
          pred_log <- bind_rows(pred_log,
                               data.frame(zone = zone,
                                         region = region,
                                         status = "skipped",
                                         error = "No summarise_results found",
                                         stringsAsFactors = FALSE))
          next
        }
        
        result_id <- result_ids[1]
        file_path <- paste0('./archive/summarise_results/', result_id, '/results_summary.rds')
        
        if (!file.exists(file_path)) {
          pred_log <- bind_rows(pred_log,
                               data.frame(zone = zone,
                                         region = region,
                                         status = "error",
                                         error = "results_summary.rds not found",
                                         stringsAsFactors = FALSE))
          next
        }
        
        pred_results <- readRDS(file_path)
        
        # Filter to just prevalence measure (prev_anc_all)
        if (!is.null(pred_results) && nrow(pred_results) > 0) {
          pred_prev <- pred_results %>%
            filter(measure == "prev_anc_all") %>%
            select(council, month, median, lower, upper) %>%
            rename(pred_median = median, pred_lower = lower, pred_upper = upper)
          
          pred_data_list[[paste0(zone, "_", region)]] <- pred_prev
          
          pred_log <- bind_rows(pred_log,
                               data.frame(zone = zone,
                                         region = region,
                                         status = "success",
                                         error = NA_character_,
                                         stringsAsFactors = FALSE))
        }
        
      }, error = function(e) {
        pred_log <<- bind_rows(pred_log,
                              data.frame(zone = zone,
                                        region = region,
                                        status = "error",
                                        error = e$message,
                                        stringsAsFactors = FALSE))
      })
    }
  }
  
  if (length(pred_data_list) > 0) {
    pred_all <- bind_rows(pred_data_list, .id = "zone_region")
    pred_all <- pred_all %>%
      separate(zone_region, into = c("zone", "region"), sep = "_", extra = "merge")
    
    # Now compare observed vs predicted
    obs_comparison <- NULL
    
    for (zone in names(obs_data)) {
      for (region in names(obs_data[[zone]])) {
        for (council in names(obs_data[[zone]][[region]])) {
          
          council_data <- obs_data[[zone]][[region]][[council]]
          
          if (!is.null(council_data$month) && !is.null(council_data$prev_anc_all)) {
            obs_df <- data.frame(
              zone = zone,
              region = region,
              council = council,
              month = council_data$month,
              observed = council_data$prev_anc_all,
              stringsAsFactors = FALSE
            )
            
            obs_comparison <- bind_rows(obs_comparison, obs_df)
          }
        }
      }
    }
    
    # Merge observed and predicted
    if (!is.null(obs_comparison)) {
      prevalence_comparison <- obs_comparison %>%
        left_join(pred_all, by = c("zone", "region", "council", "month")) %>%
        mutate(
          residual = observed - pred_median,
          abs_error = abs(residual),
          se_pred = (pred_upper - pred_lower) / (2 * 1.96)  # Standard error from 95% CI
        )
      
      cat(paste("✓ Merged", n_distinct(prevalence_comparison$council), 
                "councils across", n_distinct(prevalence_comparison$zone), "zones\n\n"))
      
      # Calculate fit metrics by council
      fit_metrics <- prevalence_comparison %>%
        group_by(zone, region, council) %>%
        summarise(
          n_obs = n(),
          obs_mean = mean(observed, na.rm = TRUE),
          pred_mean = mean(pred_median, na.rm = TRUE),
          mae = mean(abs_error, na.rm = TRUE),  # Mean absolute error
          rmse = sqrt(mean(residual^2, na.rm = TRUE)),
          cor = cor(observed, pred_median, use = "complete.obs"),
          .groups = "drop"
        ) %>%
        arrange(mae)
      
      # Show best and worst fitting councils
      cat("BEST-FITTING COUNCILS (lowest Mean Absolute Error):\n")
      print(head(fit_metrics, 10), n = Inf)
      
      cat("\nWORST-FITTING COUNCILS (highest Mean Absolute Error):\n")
      print(tail(fit_metrics, 10), n = Inf)
      
    } else {
      cat("WARNING: Could not load observed prevalence data\n")
    }
  } else {
    cat("WARNING: Could not find predicted prevalence results\n")
  }
  
}, error = function(e) {
  cat(paste("Could not perform prevalence comparison:", e$message, "\n"))
})

cat("\n========================================\n\n")

# Create visualization
if (nrow(global_diagnostics) > 0) {
  cat("Creating visualization...\n")
  
  # Box plot by zone
  p1 <- ggplot2::ggplot(global_diagnostics, 
                        aes(x = reorder(zone, prop, FUN = median), y = prop, fill = zone)) +
    geom_boxplot() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Parameter Variance by Zone",
         x = "Zone",
         y = "Parameter Variance",
         subtitle = "Higher variance indicates convergence issues") +
    theme(legend.position = "none")
  
  ggplot2::ggsave('variance_by_zone.png', p1, width = 10, height = 6, dpi = 150)
  cat("  ✓ variance_by_zone.png\n")
  
  # Scatter plot: zone vs region coloring
  p2 <- ggplot2::ggplot(global_diagnostics, 
                        aes(x = reorder(region, prop, FUN = median), y = prop, color = zone)) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Parameter Variance by Region",
         x = "Region",
         y = "Parameter Variance",
         color = "Zone") +
    theme(legend.position = "right")
  
  ggplot2::ggsave('variance_by_region.png', p2, width = 12, height = 6, dpi = 150)
  cat("  ✓ variance_by_region.png\n")
}

# Create prevalence comparison visualization
if (!is.null(prevalence_comparison) && nrow(prevalence_comparison) > 0) {
  cat("Creating prevalence fit visualization...\n")
  
  # Scatter plot: observed vs predicted prevalence (sample of councils)
  sample_councils <- prevalence_comparison %>%
    distinct(council, .keep_all = TRUE) %>%
    slice_sample(n = min(20, n_distinct(prevalence_comparison$council)))
  
  sample_data <- prevalence_comparison %>%
    filter(council %in% sample_councils$council)
  
  p3 <- ggplot2::ggplot(sample_data, 
                        aes(x = observed, y = pred_median, color = council)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    theme_minimal() +
    labs(title = "Model Fit: Observed vs Predicted Prevalence (Sample)",
         x = "Observed Prevalence",
         y = "Predicted Prevalence",
         subtitle = "Points on diagonal indicate perfect fit") +
    theme(legend.position = "none") +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1))
  
  tryCatch({
    ggplot2::ggsave('prevalence_fit_scatter.png', p3, width = 10, height = 8, dpi = 150)
    cat("  ✓ prevalence_fit_scatter.png\n")
  }, error = function(e) {
    cat(paste("  ! Warning creating scatter plot:", e$message, "\n"))
  })
  
  # Distribution of errors by zone
  if (nrow(prevalence_comparison) > 0) {
    p4 <- ggplot2::ggplot(prevalence_comparison, 
                          aes(x = zone, y = residual, fill = zone)) +
      geom_boxplot() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Prediction Residuals by Zone",
           x = "Zone",
           y = "Residual (Observed - Predicted)",
           subtitle = "0 line indicates perfect prediction") +
      theme(legend.position = "none")
    
    tryCatch({
      ggplot2::ggsave('residuals_by_zone.png', p4, width = 10, height = 6, dpi = 150)
      cat("  ✓ residuals_by_zone.png\n")
    }, error = function(e) {
      cat(paste("  ! Warning creating residuals plot:", e$message, "\n"))
    })
  }
}

# Save outputs
saveRDS(global_diagnostics, 'global_diagnostics.rds')
write.csv(global_diagnostics, 'global_diagnostics.csv', row.names = FALSE)

if (!is.null(prevalence_comparison) && nrow(prevalence_comparison) > 0) {
  saveRDS(prevalence_comparison, 'prevalence_comparison.rds')
  write.csv(prevalence_comparison, 'prevalence_comparison.csv', row.names = FALSE)
  
  fit_metrics <- prevalence_comparison %>%
    group_by(zone, region, council) %>%
    summarise(
      n_obs = n(),
      obs_mean = mean(observed, na.rm = TRUE),
      pred_mean = mean(pred_median, na.rm = TRUE),
      mae = mean(abs_error, na.rm = TRUE),
      rmse = sqrt(mean(residual^2, na.rm = TRUE)),
      cor = cor(observed, pred_median, use = "complete.obs"),
      .groups = "drop"
    ) %>%
    arrange(mae)
  
  saveRDS(fit_metrics, 'fit_metrics.rds')
  write.csv(fit_metrics, 'fit_metrics.csv', row.names = FALSE)
}

if (nrow(global_diagnostics) > 0) {
  saveRDS(summary_stats, 'summary_statistics.rds')
  write.csv(summary_stats, 'summary_statistics.csv', row.names = FALSE)
}

cat("\n✓ Diagnostics report complete!\n")
cat("  - global_diagnostics.rds (full data)\n")
cat("  - global_diagnostics.csv (for spreadsheet viewing)\n")
if (!is.null(prevalence_comparison) && nrow(prevalence_comparison) > 0) {
  cat("  - prevalence_comparison.rds\n")
  cat("  - prevalence_comparison.csv\n")
  cat("  - fit_metrics.rds (by council)\n")
  cat("  - fit_metrics.csv (by council)\n")
  cat("  - prevalence_fit_scatter.png\n")
  cat("  - residuals_by_zone.png\n")
}
if (nrow(global_diagnostics) > 0) {
  cat("  - summary_statistics.rds\n")
  cat("  - summary_statistics.csv\n")
  cat("  - variance_by_zone.png\n")
  cat("  - variance_by_region.png\n")
} else {
  cat("  [No convergence diagnostic data available]\n")
}
