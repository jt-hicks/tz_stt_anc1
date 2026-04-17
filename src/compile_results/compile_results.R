## Compile results from all summarise_results tasks
## Creates a single global dataframe with results from all zones and regions

library(magrittr)
library(dplyr)

normalize_month_to_date <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(format(x, "%Y-%m-01")))
  }

  month_numeric <- as.numeric(x)
  year_part <- floor(month_numeric)
  month_part <- round((month_numeric - year_part) * 12) + 1
  month_part <- pmin(pmax(month_part, 1), 12)
  as.Date(sprintf("%04d-%02d-01", year_part, month_part))
}

# Define all zone/region combinations based on available data
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

# Collect all results
all_results <- list()
all_posterior_samples <- list()
compilation_log <- data.frame(
  zone = character(),
  region = character(),
  status = character(),
  records = numeric(),
  posterior_sample_records = numeric(),
  error = character(),
  stringsAsFactors = FALSE
)

cat("Aggregating results from all zones and regions...\n\n")

for (zone in names(zone_region_combos)) {
  for (region in zone_region_combos[[zone]]) {
    
    cat(paste("Processing:", zone, "-", region, "..."))
    
    tryCatch({
      # Search for latest summarise_results for this zone/region
      search_query <- paste0('latest(parameter:zone == "', zone, '" && ',
                            'parameter:region == "', region, '")')
      
      result_ids <- orderly::orderly_search(search_query, name = 'summarise_results')
      
      if (length(result_ids) == 0) {
        cat(" SKIPPED (no results found)\n")
        compilation_log <- dplyr::bind_rows(compilation_log,
                                           data.frame(zone = zone,
                                                      region = region,
                                                      status = "skipped",
                                                      records = 0,
                                                      posterior_sample_records = 0,
                                                      error = "No summarise_results found",
                                                      stringsAsFactors = FALSE))
        next
      }
      
      # Load the results
      orderly::orderly_dependency("summarise_results", search_query,
                             c('data/${region}.rds'='results_summary.rds'))
      file_path <- paste0('data/', region, '.rds')

      posterior_file_path <- paste0('data/', region, '_posterior_sample.rds')
      posterior_dependency_ok <- tryCatch({
        orderly::orderly_dependency("summarise_results", search_query,
                                    c('data/${region}_posterior_sample.rds'='results_posterior_sample.rds'))
        TRUE
      }, error = function(e) {
        FALSE
      })
      
      if (!file.exists(file_path)) {
        cat(" ERROR (file not found)\n")
        compilation_log <- dplyr::bind_rows(compilation_log,
                                           data.frame(zone = zone,
                                                      region = region,
                                                      status = "error",
                                                      records = 0,
                                                      posterior_sample_records = 0,
                                                      error = "results_summary.rds not found",
                                                      stringsAsFactors = FALSE))
        next
      }
      
      results <- readRDS(file_path)

      if ("month" %in% names(results)) {
        results$month <- normalize_month_to_date(results$month)
      }
      
      if (nrow(results) == 0) {
        cat(" ERROR (no records in file)\n")
        compilation_log <- dplyr::bind_rows(compilation_log,
                                           data.frame(zone = zone,
                                                      region = region,
                                                      status = "error",
                                                      records = 0,
                                                      posterior_sample_records = 0,
                                                      error = "results_summary.rds has no records",
                                                      stringsAsFactors = FALSE))
        next
      }

      # Add zone/region columns if not present
      if (!"zone" %in% names(results)) {
        results <- results %>%
          dplyr::mutate(zone = zone, region = region)
      }
      if (!"region" %in% names(results)) {
        results <- results %>%
          dplyr::mutate(region = region)
      }
      
      # Store results
      all_results[[paste0(zone, "_", region)]] <- results

      posterior_records <- 0
      if (posterior_dependency_ok && file.exists(posterior_file_path)) {
        posterior_results <- readRDS(posterior_file_path)
        if ("month" %in% names(posterior_results)) {
          posterior_results$month <- normalize_month_to_date(posterior_results$month)
        }
        if (nrow(posterior_results) > 0) {
          if (!"zone" %in% names(posterior_results)) {
            posterior_results <- posterior_results %>%
              dplyr::mutate(zone = zone, region = region)
          }
          if (!"region" %in% names(posterior_results)) {
            posterior_results <- posterior_results %>%
              dplyr::mutate(region = region)
          }
          all_posterior_samples[[paste0(zone, "_", region)]] <- posterior_results
          posterior_records <- nrow(posterior_results)
        }
      }
      
      cat(paste(" OK (", nrow(results), " records)\n", sep = ""))
      
      compilation_log <- dplyr::bind_rows(compilation_log,
                                         data.frame(zone = zone,
                                                    region = region,
                                                    status = "success",
                                                    records = nrow(results),
                                                    posterior_sample_records = posterior_records,
                                                    error = NA_character_,
                                                    stringsAsFactors = FALSE))
      
    }, error = function(e) {
      cat(paste(" ERROR -", e$message, "\n"))
      compilation_log <<- dplyr::bind_rows(compilation_log,
                                          data.frame(zone = zone,
                                                     region = region,
                                                     status = "error",
                                                     records = 0,
                                                     posterior_sample_records = 0,
                                                     error = e$message,
                                                     stringsAsFactors = FALSE))
    })
  }
}

# Combine all results
global_results <- dplyr::bind_rows(all_results)
global_posterior_samples <- dplyr::bind_rows(all_posterior_samples)

cat("\n========================================\n")
cat("COMPILATION SUMMARY\n")
cat("========================================\n")
cat(paste("Total zone/region combinations:", nrow(compilation_log), "\n"))
cat(paste("Successfully processed:", sum(compilation_log$status == "success"), "\n"))
cat(paste("Skipped:", sum(compilation_log$status == "skipped"), "\n"))
cat(paste("Errors:", sum(compilation_log$status == "error"), "\n"))
cat(paste("Total records compiled:", nrow(global_results), "\n"))
cat(paste("Total posterior sample records compiled:", nrow(global_posterior_samples), "\n"))
cat("----------------------------------------\n")

successful <- compilation_log %>% dplyr::filter(status == "success")
if (nrow(successful) > 0) {
  cat("Successfully compiled from:\n")
  for (i in seq_len(nrow(successful))) {
    cat(paste("  •", successful$zone[i], "-", successful$region[i],
              "(", successful$records[i], "records)\n"))
  }
}

errors <- compilation_log %>% dplyr::filter(status == "error")
if (nrow(errors) > 0) {
  cat("\nErrors encountered:\n")
  for (i in seq_len(nrow(errors))) {
    cat(paste("  •", errors$zone[i], "-", errors$region[i],
              "-", errors$error[i], "\n"))
  }
}

cat("========================================\n\n")

# Summary statistics
cat("Global Results Summary:\n")
cat(paste("Unique councils:", n_distinct(global_results$council), "\n"))
cat(paste("Unique measures:", n_distinct(global_results$measure), "\n"))
cat(paste("Time period:", min(global_results$month), "to", max(global_results$month), "\n"))
cat(paste("Measures included:", paste(unique(global_results$measure), collapse = ", "), "\n\n"))

# Save outputs
saveRDS(global_results, 'global_results.rds')
saveRDS(global_posterior_samples, 'global_results_posterior_sample.rds')
saveRDS(compilation_log, 'compilation_log.rds')

# Also save as CSV for easy viewing
write.csv(global_results, 'global_results.csv', row.names = FALSE)

cat("✓ Compilation complete!\n")
cat("  - global_results.rds (full data)\n")
cat("  - global_results_posterior_sample.rds (posterior sample draws)\n")
cat("  - global_results.csv (for spreadsheet viewing)\n")
cat("  - compilation_log.rds (processing log)\n")
