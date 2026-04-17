## Quick reference: Running summarise_results for multiple regions
## Then compiling global results

# After your short MCMC runs complete for each zone/region, run:

# Lake Zone
orderly_run('summarise_results', parameters=list(zone='Lake', region='Kagera Region', length=1000))
orderly_run('summarise_results', parameters=list(zone='Lake', region='Mwanza Region', length=1000))
orderly_run('summarise_results', parameters=list(zone='Lake', region='Geita Region', length=1000))
orderly_run('summarise_results', parameters=list(zone='Lake', region='Mara Region', length=1000))
orderly_run('summarise_results', parameters=list(zone='Lake', region='Simiyu Region', length=1000))
orderly_run('summarise_results', parameters=list(zone='Lake', region='Shinyanga Region', length=1000))

# Western Zone
orderly_run('summarise_results', parameters=list(zone='Western', region='Tabora Region', length=1000))
orderly_run('summarise_results', parameters=list(zone='Western', region='Kigoma Region', length=1000))

# etc. for other zones...

# Once you have at least a few zone/region combinations with summarise_results done:
orderly_run('compile_results')

# Retrieve and view the compiled results
compile_id <- orderly_search('latest(name == "compile_results")')
global_results <- readRDS(paste0('./archive/compile_results/', compile_id, '/global_results.rds'))
compile_log <- readRDS(paste0('./archive/compile_results/', compile_id, '/compilation_log.rds'))

print(compile_log)  # See status of each zone/region
head(global_results, 20)  # View first rows of compiled results

# For diagnostics report
orderly_run('diagnostics_report')
diag_id <- orderly_search('latest(name == "diagnostics_report")')
global_diag <- readRDS(paste0('./archive/diagnostics_report/', diag_id, '/global_diagnostics.rds'))
summary_stats <- readRDS(paste0('./archive/diagnostics_report/', diag_id, '/summary_statistics.rds'))

print(summary_stats)  # See convergence by zone/region
