library(dplyr)

orderly::orderly_shared_resource('create_diag_figs.R')

# Use orderly v1 parameter API
params <- orderly::orderly_parameters(zone = NULL,
                                     region = NULL,
                                     length = NULL,
                                     workers = 1,
                                     chain = 1)

zone <- params$zone
region <- params$region
length <- params$length
workers <- params$workers
chain <- params$chain

# Depend on the data quality output (will be copied into working dir)
orderly::orderly_dependency("02_data_quality", quote(latest()),
                            c('dqa_council_monthly_nested.rds'))

source('create_diag_figs.R')

data_list <- readRDS('dqa_council_monthly_nested.rds')

names_list <- names(data_list[[zone]][[region]])

results_list <- list()
prop_df <- data.frame(name=character(),
                      prop=numeric())

for (name in names_list) {
  tryCatch({
    # Request the most recent run_pmcmc for this council
    orderly::orderly_dependency("run_pmcmc",
                                quote(latest(parameter:name == environment:name && parameter:zone == environment:zone && parameter:region == environment:region && parameter:length == environment:length)),
                                c("data/${name}.rds" = "result.rds"))

    data_file <- paste0('data/', name, '.rds')
    if (!file.exists(data_file)) {
      message(paste("Skipping", name, "- result file not found"))
      next
    }

    print(name)
    result <- readRDS(data_file)

    temp_df <- data.frame(name = name,
                          prop = as.numeric(var(result$pars)),
                          stringsAsFactors = FALSE)
    prop_df <- dplyr::bind_rows(prop_df, temp_df)

    temp_list <- list(result)
    names(temp_list) <- name
    results_list <- append(results_list, temp_list)

    # Create diagnostic figures but protect against plotting errors
    tryCatch({
      create_diag_figs(result,
                       country = region,
                       district = name,
                       name = name)
    }, error = function(e_plot) {
      message(paste('Warning creating diag figs for', name, ':', e_plot$message))
    })

  }, error = function(e) {
    message(paste('Error processing', name, ':', e$message))
    next
  })
}

saveRDS(prop_df,'props.RDS')
saveRDS(results_list,'results_list.RDS')
