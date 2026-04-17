#!/usr/bin/env Rscript
## Test batch runner for 3-4 regions
## Process a small sample to evaluate output and timing before full batch run

library(orderly)
library(dplyr)

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║           TEST BATCH: Extract Results for Sample Regions      ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# Define test zone/region combinations (4 regions)
test_zones_regions <- list(
  Lake = c('Geita Region', 'Mwanza Region'),
  Western = c('Tabora Region'),
  Northern = c('Kilimanjaro Region')
)

cat("Testing with the following regions:\n")
for (zone in names(test_zones_regions)) {
  for (region in test_zones_regions[[zone]]) {
    cat(paste("  •", zone, "-", region, "\n"))
  }
}
cat("\n")

# Track progress
test_log <- data.frame(
  zone = character(),
  region = character(),
  task = character(),
  status = character(),
  result_id = character(),
  duration_sec = numeric(),
  error = character(),
  timestamp = character(),
  stringsAsFactors = FALSE
)

total_combos <- sum(sapply(test_zones_regions, length))
completed <- 0

cat(paste("Total zone/region combinations to process:", total_combos, "\n\n"))

# Run summaries and diagnostics for each zone/region
for (zone in names(test_zones_regions)) {
  for (region in test_zones_regions[[zone]]) {
    
    completed <- completed + 1
    cat(paste("[", completed, "/", total_combos, "] Processing: ", zone, " - ", region, "\n", sep = ""))
    
    # ========== summarise_results ==========
    cat("  → Running summarise_results...\n")
    start_time <- Sys.time()
    
    tryCatch({
      result_id <- orderly_run('summarise_results',
                              parameters = list(zone = zone,
                                               region = region,
                                               length = 1000),
                              echo = FALSE)
      
      duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      cat(paste("    ✓ Complete (", round(duration, 1), "s)\n", sep = ""))
      
      test_log <- dplyr::bind_rows(test_log,
                                   data.frame(zone = zone,
                                             region = region,
                                             task = "summarise_results",
                                             status = "success",
                                             result_id = result_id,
                                             duration_sec = duration,
                                             error = NA_character_,
                                             timestamp = as.character(start_time),
                                             stringsAsFactors = FALSE))
      
    }, error = function(e) {
      duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      cat(paste("    ✗ Failed:", e$message, "\n"))
      test_log <<- dplyr::bind_rows(test_log,
                                    data.frame(zone = zone,
                                              region = region,
                                              task = "summarise_results",
                                              status = "error",
                                              result_id = NA_character_,
                                              duration_sec = duration,
                                              error = e$message,
                                              timestamp = as.character(start_time),
                                              stringsAsFactors = FALSE))
    })
    
    # ========== run_diagnostics ==========
    cat("  → Running run_diagnostics...\n")
    start_time <- Sys.time()
    
    tryCatch({
      diag_id <- orderly_run('run_diagnostics',
                             parameters = list(zone = zone,
                                              region = region,
                                              length = 1000),
                             echo = FALSE)
      
      duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      cat(paste("    ✓ Complete (", round(duration, 1), "s)\n\n", sep = ""))
      
      test_log <- dplyr::bind_rows(test_log,
                                   data.frame(zone = zone,
                                             region = region,
                                             task = "run_diagnostics",
                                             status = "success",
                                             result_id = diag_id,
                                             duration_sec = duration,
                                             error = NA_character_,
                                             timestamp = as.character(start_time),
                                             stringsAsFactors = FALSE))
      
    }, error = function(e) {
      duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      cat(paste("    ✗ Failed:", e$message, "\n\n"))
      test_log <<- dplyr::bind_rows(test_log,
                                    data.frame(zone = zone,
                                              region = region,
                                              task = "run_diagnostics",
                                              status = "error",
                                              result_id = NA_character_,
                                              duration_sec = duration,
                                              error = e$message,
                                              timestamp = as.character(start_time),
                                              stringsAsFactors = FALSE))
    })
  }
}

# Summary statistics
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║                    TEST BATCH COMPLETE                        ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("Execution Summary:\n")
cat("────────────────────────────────────────────────────────────────\n")

summarise_results <- test_log %>% filter(task == "summarise_results")
run_diagnostics <- test_log %>% filter(task == "run_diagnostics")

cat(paste("summarise_results task:\n"))
cat(paste("  ✓ Successful:", sum(summarise_results$status == "success"), "\n"))
cat(paste("  ✗ Failed:", sum(summarise_results$status == "error"), "\n"))
if (sum(summarise_results$status == "success") > 0) {
  avg_time <- mean(summarise_results$duration_sec[summarise_results$status == "success"])
  cat(paste("  Average time per region:", round(avg_time, 1), "seconds\n"))
}
cat("\n")

cat(paste("run_diagnostics task:\n"))
cat(paste("  ✓ Successful:", sum(run_diagnostics$status == "success"), "\n"))
cat(paste("  ✗ Failed:", sum(run_diagnostics$status == "error"), "\n"))
if (sum(run_diagnostics$status == "success") > 0) {
  avg_time <- mean(run_diagnostics$duration_sec[run_diagnostics$status == "success"])
  cat(paste("  Average time per region:", round(avg_time, 1), "seconds\n"))
}
cat("\n")

# Show detailed timing by region
cat("Detailed timing:\n")
cat("────────────────────────────────────────────────────────────────\n")
detailed <- test_log %>%
  filter(status == "success") %>%
  group_by(zone, region) %>%
  summarise(
    summarise_time = sum(duration_sec[task == "summarise_results"]),
    diag_time = sum(duration_sec[task == "run_diagnostics"]),
    total_time = sum(duration_sec),
    .groups = "drop"
  ) %>%
  arrange(desc(total_time))

print(detailed, n = Inf)

# Grand total
total_time <- sum(test_log$duration_sec[test_log$status == "success"])
cat(paste("\n\nGrand total execution time:", round(total_time, 1), "seconds\n"))
cat(paste("Projected time for 26 regions:", round(total_time / nrow(detailed) * 26, 1), "seconds (~", 
          round(total_time / nrow(detailed) * 26 / 60, 1), " minutes)\n\n"))

# Save test log
saveRDS(test_log, 'batch_test_log.rds')
write.csv(test_log, 'batch_test_log.csv', row.names = FALSE)

cat("════════════════════════════════════════════════════════════════\n")
cat("Test logs saved:\n")
cat("  - batch_test_log.rds (R format)\n")
cat("  - batch_test_log.csv (spreadsheet format)\n")
cat("════════════════════════════════════════════════════════════════\n\n")

# If successful, show sample output
successful_summarise <- sum(summarise_results$status == "success")
if (successful_summarise > 0) {
  cat("✓ Test completed successfully!\n\n")
  
  # Load and display sample from first successful summarise_results
  first_success <- summarise_results %>% 
    filter(status == "success") %>%
    arrange(timestamp) %>%
    slice(1)
  
  result_path <- paste0('./archive/summarise_results/', first_success$result_id[1])
  
  tryCatch({
    results <- readRDS(paste0(result_path, '/results_summary.rds'))
    
    cat("SAMPLE OUTPUT FROM summarise_results:\n")
    cat("────────────────────────────────────────────────────────────────\n")
    cat(paste("Zone:", first_success$zone[1], "\n"))
    cat(paste("Region:", first_success$region[1], "\n"))
    cat(paste("Total records:", nrow(results), "\n"))
    cat(paste("Unique councils:", n_distinct(results$council), "\n"))
    cat(paste("Unique measures:", n_distinct(results$measure), "\n"))
    cat(paste("Time range:", min(results$month), "to", max(results$month), "\n\n"))
    
    cat("First 10 rows:\n")
    print(head(results, 10))
    
  }, error = function(e) {
    cat(paste("Could not load sample output:", e$message, "\n"))
  })
}

cat("\n════════════════════════════════════════════════════════════════\n")
cat("Next steps:\n")
cat("1. Review the sample output above\n")
cat("2. Check batch_test_log.csv for timing information\n")
cat("3. If satisfied, run batch_summarise_all.ps1 for full 186-council batch\n")
cat("════════════════════════════════════════════════════════════════\n\n")

