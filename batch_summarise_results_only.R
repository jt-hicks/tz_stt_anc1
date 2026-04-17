#!/usr/bin/env Rscript
## Batch runner for summarise_results across all zones and regions
## Does NOT run run_diagnostics or compile_results

library(orderly)
library(dplyr)

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║     BATCH RUNNER: summarise_results Only (All Regions)        ║\n")
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

batch_log <- data.frame(
  zone = character(),
  region = character(),
  status = character(),
  result_id = character(),
  error = character(),
  timestamp = character(),
  stringsAsFactors = FALSE
)

total_combos <- sum(sapply(zone_region_combos, length))
completed <- 0

cat(paste("Total zone/region combinations to process:", total_combos, "\n\n"))

for (zone in names(zone_region_combos)) {
  for (region in zone_region_combos[[zone]]) {
    completed <- completed + 1
    cat(paste("[", completed, "/", total_combos, "] Processing: ", zone, " - ", region, "...", sep = ""))

    tryCatch({
      result_id <- orderly_run('summarise_results',
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

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║                       BATCH COMPLETE                           ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

successful <- sum(batch_log$status == "success")
failed <- sum(batch_log$status == "error")

cat(paste("Total processed:", nrow(batch_log), "\n"))
cat(paste("✓ Successful:", successful, "\n"))
cat(paste("✗ Failed:", failed, "\n\n"))

if (failed > 0) {
  cat("Failures encountered:\n")
  failed_runs <- batch_log %>% filter(status == "error")
  for (i in 1:nrow(failed_runs)) {
    cat(paste("  •", failed_runs$zone[i], "-", failed_runs$region[i],
              "\n    Error:", failed_runs$error[i], "\n"))
  }
  cat("\n")
}

cat("Successfully processed zones/regions:\n")
successful_runs <- batch_log %>% filter(status == "success")
for (z in unique(successful_runs$zone)) {
  regions <- successful_runs %>% filter(zone == z)
  cat(paste("  ", z, " (", nrow(regions), ")\n", sep = ""))
  for (r in regions$region) {
    cat(paste("    - ", r, "\n", sep = ""))
  }
}

saveRDS(batch_log, 'batch_summarise_results_only_log.rds')
write.csv(batch_log, 'batch_summarise_results_only_log.csv', row.names = FALSE)

cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("Batch logs saved:\n")
cat("  - batch_summarise_results_only_log.rds (R format)\n")
cat("  - batch_summarise_results_only_log.csv (spreadsheet format)\n")
cat("════════════════════════════════════════════════════════════════\n\n")
