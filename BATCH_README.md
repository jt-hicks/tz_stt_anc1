# Batch Summarise Runner

## Overview

This batch runner automatically extracts posterior estimates for all **186 councils** across **8 zones** and **26 regions** from your completed short MCMC runs. It processes results systematically and compiles them into a single global dataframe.

## What It Does

1. **Runs `summarise_results` for each zone/region** (26 combinations)
2. **Extracts posterior estimates** (median & 95% CIs) for 8 epidemiological measures
3. **Creates tracking logs** of successes and failures
4. **Automatically compiles results** into a global dataset
5. **Produces outputs** in both R and CSV formats

## Prerequisites

✓ All short MCMC runs completed (`run_pmcmc` with `length=1000`)
✓ Data quality task completed (`02_data_quality`)
✓ R 4.5+ installed with `orderly` package

## How to Run

### Option 1: PowerShell (Recommended)
```powershell
# From VS Code Terminal or PowerShell
.\batch_summarise_all.ps1
```

### Option 2: Direct R Script
```r
# From R console or RStudio
source('batch_summarise_all.R')
```

### Option 3: From R Command Line
```bash
Rscript batch_summarise_all.R
```

## Expected Output

The script will display progress like this:

```
╔════════════════════════════════════════════════════════════════╗
║     BATCH RUNNER: Extract Results for All 186 Councils        ║
╚════════════════════════════════════════════════════════════════╝

Total zone/region combinations to process: 26

[1/26] Processing: Lake - Kagera Region...✓ DONE
[2/26] Processing: Lake - Mwanza Region...✓ DONE
[3/26] Processing: Lake - Geita Region...✓ DONE
...
[26/26] Processing: Eastern - Morogoro Region...✓ DONE

╔════════════════════════════════════════════════════════════════╗
║                      BATCH COMPLETE                           ║
╚════════════════════════════════════════════════════════════════╝

Total processed: 26
✓ Successful: 26
✗ Failed: 0

Successfully processed zones/regions:
   Lake (6)
    - Kagera Region
    - Mwanza Region
    ... etc
```

## Output Files

### Batch Logs
- **`batch_summarise_log.rds`** - R data frame with execution log
  - Columns: zone, region, status, result_id, error, timestamp
- **`batch_summarise_log.csv`** - Spreadsheet-friendly version

### Compiled Results
Located in `archive/compile_results/[timestamp]/`:
- **`global_results.rds`** - Full compiled dataframe (186 councils × 8 measures)
- **`global_results.csv`** - Spreadsheet-friendly version
- **`compilation_log.rds`** - Processing log from compilation

## Examining Results

```r
# Load the global results
compile_id <- orderly_search('latest(name == "compile_results")')
global_results <- readRDS(paste0(
  './archive/compile_results/', compile_id, '/global_results.rds'
))

# Summary
nrow(global_results)  # Total records
n_distinct(global_results$council)  # 186 councils
n_distinct(global_results$zone)     # 8 zones
n_distinct(global_results$measure)  # 8 measures

# View a sample
head(global_results, 20)

# Filter by council
kagera_results <- filter(global_results, zone == 'Lake', region == 'Kagera Region')

# Filter by measure
prevalence <- filter(global_results, measure == 'prev_anc_all')

# Plot trend for a council
library(ggplot2)
one_council <- filter(global_results, 
                      council == 'Mufindi District Council',
                      measure == 'prev_anc_all')
ggplot(one_council, aes(x = month, y = median)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = 'Prevalence: Mufindi District')
```

## Zones and Regions Covered

### Lake (6 regions)
- Kagera Region
- Mwanza Region
- Geita Region
- Mara Region
- Simiyu Region
- Shinyanga Region

### Western (2 regions)
- Tabora Region
- Kigoma Region

### Northern (3 regions)
- Kilimanjaro Region
- Tanga Region
- Arusha Region

### Central (3 regions)
- Dodoma Region
- Singida Region
- Manyara Region

### Southern Highlands (3 regions)
- Iringa Region
- Njombe Region
- Ruvuma Region

### Southern (2 regions)
- Lindi Region
- Mtwara Region

### Southwest Highlands (4 regions)
- Mbeya Region
- Rukwa Region
- Katavi Region
- Songwe Region

### Eastern (3 regions)
- Dar Es Salaam Region
- Pwani Region
- Morogoro Region

## Troubleshooting

### Script Fails Partway Through
The batch runner is designed to continue even if individual zones fail. Check `batch_summarise_log.csv` to see which zones had errors and address those specifically:

```r
# Check failures
batch_log <- read.csv('batch_summarise_log.csv')
failures <- batch_log[batch_log$status == 'error',]
print(failures)

# Retry a specific zone
orderly_run('summarise_results', 
            parameters=list(zone='Western', region='Tabora Region', length=1000))
```

### Missing MCMC Results
If a zone/region fails because `run_pmcmc` results don't exist:
1. Check that all short runs (`length=1000`) completed for that zone
2. Use `orderly_search()` to verify results exist
3. Rerun any failed MCMC jobs

### Compilation Fails
If `compile_results` fails during the batch:
1. Wait for all `summarise_results` to complete
2. Run manually: `orderly_run('compile_results')`
3. Check `compilation_log.rds` for detailed error info

## Monitoring Progress

You can monitor progress in real-time while the script runs by checking the archive:

```r
# Check how many summarise_results have been created
existing <- orderly_search('name == "summarise_results"')
cat("Progress:", length(unique(existing)), "/ 26 zone/region combos\n")

# List what's been done
for (result_id in existing[1:5]) {  # Show first 5
  meta <- readRDS(paste0('./archive/summarise_results/', result_id, '/orderly.yml'))
  cat(result_id, "\n")
}
```

## Performance Tips

- **Run on a quiet system** - MCMC extraction is CPU-intensive
- **Estimated time**: 5-10 minutes for all 26 zones (varies by system)
- **Disk space**: ~50 MB for all compiled results
- **RAM usage**: Peaks during compilation step

## Next Steps

Once batch processing is complete:

1. **Run diagnostics report**:
   ```r
   orderly_run('diagnostics_report')
   ```

2. **Check convergence**:
   ```r
   diag_id <- orderly_search('latest(name == "diagnostics_report")')
   diag <- readRDS(paste0('./archive/diagnostics_report/', diag_id, '/global_diagnostics.rds'))
   ```

3. **Plan long runs** - Use variance estimates to decide which councils need longer chains

## Files Modified/Created

- ✓ `batch_summarise_all.R` - Main batch runner script
- ✓ `batch_summarise_all.ps1` - PowerShell wrapper
- ✓ `batch_summarise_log.csv` - Execution log (generated on run)
- ✓ Archive outputs from `compile_results`

See [USAGE_GUIDE.R](USAGE_GUIDE.R) for additional examples.
