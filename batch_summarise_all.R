#!/usr/bin/env Rscript
## Batch runner for summarise_results across all zones and regions
## Extracts posterior estimates for all 186 councils

library(orderly)
library(dplyr)

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║     BATCH RUNNER: Extract Results for All 186 Councils        ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

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

# Track progress
batch_log <- data.frame(
  zone = character(),
  region = character(),
  status = character(),
  result_id = character(),
  error = character(),
  timestamp = character(),
  stringsAsFactors = FALSE
)

# Count total combinations
total_combos <- sum(sapply(zone_region_combos, length))
completed <- 0

cat(paste("Total zone/region combinations to process:", total_combos, "\n\n"))

# Run summaries for each zone/region
for (zone in names(zone_region_combos)) {
  for (region in zone_region_combos[[zone]]) {
    
    completed <- completed + 1
    cat(paste("[", completed, "/", total_combos, "] Processing: ", zone, " - ", region, "...", sep = ""))
    
    tryCatch({
      
      # Run the summarise_results task
      result_id <- orderly_run('summarise_results',
                              parameters = list(zone = zone,
                                               region = region,
                                               length = 1000),
                              echo = FALSE)
      
      # Also run run_diagnostics for this zone/region
      diag_id <- orderly_run('run_diagnostics',
                             parameters = list(zone = zone,
                                              region = region,
                                              length = 1000),
                             echo = FALSE)
      
      cat(" ✓ DONE\n")
      
      batch_log <- dplyr::bind_rows(batch_log,
                                   data.frame(zone = zone,
                                             region = region,
                                             status = "success",
                                             result_id = result_id,
                                             error = NA_character_,
                                             timestamp = as.character(Sys.time()),
                                             stringsAsFactors = FALSE))
      
    }, error = function(e) {
      cat(paste(" ✗ FAILED:", e$message, "\n"))
      batch_log <<- dplyr::bind_rows(batch_log,
                                    data.frame(zone = zone,
                                              region = region,
                                              status = "error",
                                              result_id = NA_character_,
                                              error = e$message,
                                              timestamp = as.character(Sys.time()),
                                              stringsAsFactors = FALSE))
    })
  }
}

# Summary statistics
cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║                       BATCH COMPLETE                           ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

successful <- sum(batch_log$status == "success")
failed <- sum(batch_log$status == "error")

cat(paste("Total processed:", nrow(batch_log), "\n"))
cat(paste("✓ Successful:", successful, "\n"))
cat(paste("✗ Failed:", failed, "\n\n"))

# Show any failures
if (failed > 0) {
  cat("Failures encountered:\n")
  failed_runs <- batch_log %>% filter(status == "error")
  for (i in 1:nrow(failed_runs)) {
    cat(paste("  •", failed_runs$zone[i], "-", failed_runs$region[i],
              "\n    Error:", failed_runs$error[i], "\n"))
  }
  cat("\n")
}

# Show successful runs
cat("Successfully processed zones/regions:\n")
successful_runs <- batch_log %>% filter(status == "success")
for (z in unique(successful_runs$zone)) {
  regions <- successful_runs %>% filter(zone == z)
  cat(paste("  ", z, " (", nrow(regions), ")\n", sep = ""))
  for (r in regions$region) {
    cat(paste("    - ", r, "\n", sep = ""))
  }
}

# Save batch log
saveRDS(batch_log, 'batch_summarise_log.rds')
write.csv(batch_log, 'batch_summarise_log.csv', row.names = FALSE)

cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("NEXT STEP: Compile all results globally\n")
cat("════════════════════════════════════════════════════════════════\n")

# Ask if user wants to compile now
if (successful > 0) {
  cat(paste("\n", successful, " zones/regions successfully processed.\n", sep = ""))
  cat("Running compile_results to aggregate all data...\n\n")
  
  tryCatch({
    compile_id <- orderly_run('compile_results', echo = FALSE)
    cat(paste("✓ Compilation successful! ID:", compile_id, "\n\n"))
    
    # Load and display summary
    compile_path <- paste0('./archive/compile_results/', compile_id)
    global_results <- readRDS(paste0(compile_path, '/global_results.rds'))
    
    cat("════════════════════════════════════════════════════════════════\n")
    cat("COMPILATION RESULTS\n")
    cat("════════════════════════════════════════════════════════════════\n")
    cat(paste("Total records:", nrow(global_results), "\n"))
    cat(paste("Unique councils:", n_distinct(global_results$council), "\n"))
    cat(paste("Unique zones:", n_distinct(global_results$zone), "\n"))
    cat(paste("Unique regions:", n_distinct(global_results$region), "\n"))
    cat(paste("Unique measures:", n_distinct(global_results$measure), "\n"))
    cat(paste("Time range:", min(global_results$month), "to", max(global_results$month), "\n"))
    
    cat("\nSample data (first 10 rows):\n")
    print(head(global_results, 10))
    
    cat("\n✓ All results successfully compiled and saved!\n")
    cat("  Files: global_results.rds, global_results.csv, compilation_log.rds\n\n")
    
  }, error = function(e) {
    cat(paste("⚠ Compilation encountered an error:", e$message, "\n"))
    cat("You can run compile_results manually later once all zones are ready.\n\n")
  })
}

cat("════════════════════════════════════════════════════════════════\n")
cat("Batch logs saved:\n")
cat("  - batch_summarise_log.rds (R format)\n")
cat("  - batch_summarise_log.csv (spreadsheet format)\n")
cat("════════════════════════════════════════════════════════════════\n\n")
