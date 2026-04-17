# QUICK START: Extract & Compile All Results

## TL;DR - Three Commands

```powershell
# 1. Extract results for all 186 councils
.\batch_summarise_all.ps1

# (It will then automatically compile everything)

# 2. View the compiled global results
R
compile_id <- orderly_search('latest(name == "compile_results")')
global_results <- readRDS(paste0('./archive/compile_results/', compile_id, '/global_results.rds'))
head(global_results)
```

## What You Now Have

### Three New Orderly Tasks
✓ **`summarise_results`** - Extracts posterior estimates from MCMC runs
✓ **`compile_results`** - Aggregates all zone/region results into one global dataframe  
✓ **`diagnostics_report`** - Analyzes MCMC convergence metrics

### Three New Batch Scripts
✓ **`batch_summarise_all.R`** - Main R script that processes all 26 zone/region combos
✓ **`batch_summarise_all.ps1`** - PowerShell wrapper (easiest to use)
✓ **`BATCH_README.md`** - Full documentation with examples

### Reference Files
✓ **`USAGE_GUIDE.R`** - Usage examples for each task
✓ **`QUICK_START.md`** - This file

## Step-by-Step

### Step 1: Run the Batch Processor

```powershell
# In VS Code Terminal (PowerShell)
cd y:\jth\tz_stt_anc1
.\batch_summarise_all.ps1
```

Expected duration: **5-10 minutes**

The script will:
1. Run `summarise_results` for Lake (6 regions) → ~1-2 min
2. Run `summarise_results` for Western (2 regions) → ~30 sec
3. ... (continue for all 8 zones)
4. Automatically run `compile_results` at the end → ~30 sec
5. Show you a summary of what was extracted

### Step 2: Check Your Results

```r
# In R console
library(orderly)
library(dplyr)

# Get the compiled results
compile_id <- orderly_search('latest(name == "compile_results")')
global_results <- readRDS(paste0('./archive/compile_results/', compile_id, '/global_results.rds'))

# Quick summary
cat("Total records:", nrow(global_results), "\n")
cat("Unique councils:", n_distinct(global_results$council), "\n")
cat("Expected: 186 councils\n")

# View a sample
head(global_results, 10)
```

### Step 3: View by Zone or Council

```r
# Councils in a specific region
lake_kagera <- filter(global_results, zone == 'Lake', region == 'Kagera Region')
unique(lake_kagera$council)

# View one measure over time
prev_data <- filter(global_results, 
                    council == 'Mufindi District Council',
                    measure == 'prev_anc_all')
head(prev_data)
```

### Step 4: Run Diagnostics (Optional)

```r
# Analyze MCMC convergence
orderly::orderly_run('diagnostics_report')

# View convergence metrics
diag_id <- orderly_search('latest(name == "diagnostics_report")')
global_diag <- readRDS(paste0(
  './archive/diagnostics_report/', diag_id, '/global_diagnostics.rds'
))

# Councils with best/worst convergence
best <- arrange(global_diag, prop) %>% head(10)
worst <- arrange(global_diag, desc(prop)) %>% head(10)
print(worst)  # Consider longer runs for these
```

## Key Outputs

### In Working Directory
- `batch_summarise_log.csv` - Log of which zones succeeded/failed
- `batch_summarise_log.rds` - R version of the log

### In Archive
```
archive/compile_results/[latest]/
├── global_results.rds          # Full compiled dataframe
├── global_results.csv          # Spreadsheet version
└── compilation_log.rds         # Processing details

archive/diagnostics_report/[latest]/   (if you run it)
├── global_diagnostics.rds
├── summary_statistics.rds
└── variance_by_*.png           # Convergence plots
```

## What Gets Extracted

For each of 186 councils and 8 measures, we extract:
- **Month** (time series)
- **Median** (posterior median estimate)
- **Lower** (2.5th percentile - 95% CI lower bound)
- **Upper** (97.5th percentile - 95% CI upper bound)

### The 8 Measures
1. **prev_anc_all** - Prevalence from ANC data
2. **prev_05** - Prevalence in children <5
3. **clininc_all** - Clinical incidence
4. **EIR** - Entomological Inoculation Rate
5. **betaa** - Transmission parameter
6. **eff_moz_pop** - Effective mosquito population
7. **moz2human_ratio** - Mosquito to human ratio
8. **spz_rate** - Sporozoite rate

## Troubleshooting

### "Some zones failed"
Check `batch_summarise_log.csv` to identify which ones failed. You can rerun individual zones:

```r
orderly_run('summarise_results', 
            parameters=list(zone='Western', region='Tabora Region', length=1000))
```

### "Results seem incomplete"
Double-check that all short MCMC runs completed:

```r
# Check how many run_pmcmc results exist for length=1000
all_pmcmc <- orderly_search('name == "run_pmcmc" && parameter:length == 1000')
length(all_pmcmc)  # Should be >= 186
```

### Memory/Performance Issues
- Run on a quiet system
- Close other applications
- Batch runner can be paused and resumed

## Next Steps After Batch

1. **Create visualizations** from global_results.rds
2. **Perform statistical analysis** on compiled estimates
3. **Plan longer runs** based on diagnostics
4. **Generate reports** comparing zones/regions

See `BATCH_README.md` for complete documentation and examples.

## Questions?

Refer to:
- `BATCH_README.md` - Detailed documentation
- `USAGE_GUIDE.R` - Code examples
- Individual task scripts under `src/summarise_results/`, `src/compile_results/`, etc.
