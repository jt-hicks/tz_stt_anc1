library(orderly)
# remotes::install_github("mrc-ide/orderly.sharedfile")
# orderly_init(force=TRUE)

# orderly_cleanup('01_read_data')
# orderly_cleanup('02_data_quality')
# orderly_cleanup('03_attendance_rate')
# orderly_cleanup('04_correlation')
# orderly_cleanup('times_series_analysis')

library(readxl)
library(zoo)
library(plyr)
library(dplyr)
library(ggpubr)
library(binom)
library(gridExtra)
library(ggplot2)
library(viridis)
library(lubridate)
library(terra)
library(stringr)
library(geofacet)
library(sf)
library(forcats)
library(patchwork)
library(R2OpenBUGS)
library(loo)
library(mcmcplots)
library(htmltools)
library(bayesplot)
library(rstanarm)
library(rstan)
library(paletteer)
library(tidyterra)
library(hipercow)
library(reclin2)


##Read in data files, process column names, and combine rows
orderly_run('01_read_data')

##Calculate and plot data quality metrics
##Remove illogical data points for downstream analysis
orderly_run('02_data_quality')

##Calculate rate of ANC1 attendance according to expected pregnancy/fertility rates
##Geographic distribution, time series
orderly_run('03_attendance_rate')

##Measure correlation between ANC1 prevalence and DHS prevalence in children < 5yo
orderly_run('04_correlation')

##Estimate
orderly_run('05_treatment_cost')

orderly_run('times_series_analysis')

Western_zone<- c('Tabora Region', 'Kigoma Region')
Northern_zone<- c('Kilimanjaro Region', 'Tanga Region', 'Arusha Region')
Central_zone<- c('Dodoma Region', 'Singida Region', 'Manyara Region')
SouthernHighlands_zone<- c('Iringa Region', 'Njombe Region', 'Ruvuma Region')
Southern_zone<- c('Lindi Region', 'Mtwara Region')
SouthwestHighlands_zone<- c('Mbeya Region', 'Rukwa Region', 'Katavi Region', 'Songwe Region')
Lake_zone<- c('Kagera Region', 'Mwanza Region', 'Geita Region', 'Mara Region', 'Simiyu Region', 'Shinyanga Region')
Eastern_zone<- c('Dar Es Salaam Region', 'Pwani Region', 'Morogoro Region')

latest_data <- orderly::orderly_search('latest(name == "02_data_quality")')
file_path <- paste0('./archive/02_data_quality/',latest_data,'/dqa_council_monthly_nested.rds')
final_data_list<- readRDS(file_path)
months <- final_data_list[[1]][[1]][[1]]$month

latest_data_raw <- orderly::orderly_search('latest(name == "01_read_data")')
file_path <- paste0('./archive/01_read_data/',latest_data_raw,'/Full_data.csv')
final_data_raw<- read.csv(file_path)
xlsx::write.xlsx(final_data_raw,file='C:/Users/jthicks/Desktop/Full_data_11Aug2025.xlsx',password='ANC4U')

hipercow_init()
hipercow_configure('dide-windows')
hipercow_provision()
hipercow_environment_create(packages=c('dplyr','ggplot2'))
resources <- hipercow_resources(cores=32)
lake_kagera_names <- data.frame(name=names(final_data_list[['Lake']][['Kagera Region']]))
lake_kagera_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Lake',region='Kagera Region',name=name,length=1000)),data=lake_kagera_names,
                                  resources=resources)
lake_kagera_short_id <- hipercow::hipercow_bundle_load('zealous_illadopsis')
task_status(lake_kagera_short_id$ids)#zealous_illadopsis
task_log_show(lake_kagera_short_id$ids[2])

lake_geita_names <- data.frame(name=names(final_data_list[['Lake']][['Geita Region']]))
lake_geita_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Lake',region="Geita Region",name=name,length=1000)),
                                  data=lake_geita_names,
                                  resources=resources)
lake_geita_short_id <- hipercow::hipercow_bundle_load('waiting_xiaosaurus')
task_status(lake_geita_short_id$ids)#waiting_xiaosaurus
task_log_show(lake_geita_short_id$ids[3])
lake_geita_short_id_3 <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Lake',region="Geita Region",name=name,length=1000,seed=1989)),
                                             data=data.frame(name=lake_geita_names[3,]),
                                             resources=resources)
lake_geita_short_id_3 <- hipercow::hipercow_bundle_load('dermatic_shrimp')
hipercow::hipercow_bundle_list()
task_status(lake_geita_short_id_3$ids)#dermatic_shrimp
task_log_show(lake_geita_short_id_3$ids[1])

lake_geita_short_results <- orderly::orderly_run('run_diagnostics',parameters=list(zone='Lake',region="Geita Region",
                                                                length=1000))
lake_geita_short_results_id <-orderly_search('latest(name == "run_diagnostics" && parameter:zone == "Lake" && parameter:region == "Geita Region")')
lake_geita_props <- readRDS(paste0('./archive/run_diagnostics/',lake_geita_short_results_id,'/props.RDS'))

lake_geita_long_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Lake',region="Geita Region",name=name,proposal_matrix=prop,length=5000)),
                                             data=lake_geita_props,
                                             resources=resources)
lake_geita_long_id <- hipercow::hipercow_bundle_load('locustal_slothbear')
task_status(lake_geita_long_id$ids)#locustal_slothbear
# "ebc5e84c8f9c87a3219e9ac54c9a4f6d" "3095c637efda7073670cc5a993a5be67"
# "e7e83cc1ac2164f18d4987f063ecea18" "f79f86e7ef45ee69cf529a2eb4d6247b"
# "76ac23b317fc0b37dc7c7442c458b81d" "47204cf1742a474d179f9001d010d424"
task_log_show(lake_geita_long_id$ids[1])

lake_geita_long_id_3 <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Lake',region="Geita Region",name=name,proposal_matrix=prop,length=5000,seed=501)),
                                            data=lake_geita_props[3,],
                                            resources=resources)
lake_geita_long_id_3 <- hipercow::hipercow_bundle_load() #'6bcdbe5270410de1608d8af44e3cd5ea'
task_status(lake_geita_long_id_3$ids)

lake_regions <- names(final_data_list[['Lake']])
lake_mara_names <- data.frame(name=names(final_data_list[['Lake']][['Mara Region']]))
lake_mara_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Lake',region="Mara Region",name=name,length=1000)),
                                             data=lake_mara_names,
                                             resources=resources)
lake_mara_short_id <- hipercow::hipercow_bundle_load('preagricultural_icterinewarbler')
task_status(lake_mara_short_id$ids)#preagricultural_icterinewarbler
task_log_show(lake_mara_short_id$ids[1])

lake_mara_short_results <- orderly::orderly_run('run_diagnostics',parameters=list(zone='Lake',region="Mara Region",
                                                                                   length=1000))
lake_mara_short_results_id <-orderly_search('latest(name == "run_diagnostics" && parameter:zone == "Lake" && parameter:region == "Mara Region")')
lake_mara_props <- readRDS(paste0('./archive/run_diagnostics/',lake_mara_short_results_id,'/props.RDS'))

lake_mara_long_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Lake',region="Mara Region",name=name,proposal_matrix=prop,length=5000)),
                                            data=lake_mara_props,
                                            resources=resources)

lake_mara_long_id <- hipercow::hipercow_bundle_load()
task_status(lake_mara_long_id$ids)
# "0efaa9df38523ee39f2fb25ef9b2d130" "f22c709f4069f6e614dd8f80d1e258dd"
# "3b625a0193c13a71f960da971c5818b0" "75a639e4d598c42bd2cf07a35cb7eda5"
# "1b6302c7485857897d4054a6e5be8da1" "1ebd8aad968907f9f0a8ec5afd09fda9"
# "e3b9545496cc1bfab94421c42c334ae9" "10865fa175b803d82371c1ca71dea459"
# "1df9d18b1dde74f257b0da82fbd3960a"
lake_mwanza_names <- data.frame(name=names(final_data_list[['Lake']][['Mwanza Region']]))
lake_mwanza_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Lake',region="Mwanza Region",name=name,length=1000)),
                                            data=lake_mwanza_names,
                                            resources=resources)
lake_mwanza_short_id <- hipercow::hipercow_bundle_load('rhombohedral_walrus')
task_status(lake_mwanza_short_id$ids)#rhombohedral_walrus
task_log_show(lake_mwanza_short_id$ids[1])

lake_mwanza_short_results <- orderly::orderly_run('run_diagnostics',parameters=list(zone='Lake',region="Mwanza Region",
                                                                                  length=1000))
lake_mwanza_short_results_id <-orderly_search('latest(name == "run_diagnostics" && parameter:zone == "Lake" && parameter:region == "Mwanza Region")')
lake_mwanza_props <- readRDS(paste0('./archive/run_diagnostics/',lake_mwanza_short_results_id,'/props.RDS'))

lake_mwanza_long_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Lake',region="Mwanza Region",name=name,proposal_matrix=prop,length=5000)),
                                           data=lake_mwanza_props,
                                           resources=resources)
lake_mwanza_long_id <- hipercow::hipercow_bundle_load('misleading_cranefly')
task_status(lake_mwanza_long_id$ids)#misleading_cranefly
# "6235a34574e456ce1822a801f0985bb2" "6f44b6fa2001fe2be59aae119947376c" "0ead61d644b2ef228d7ff4618b1c8a43"
# "db8fd9fc7a46317e244a8d6a81ed123d" "976c461e371cbd3e205af47495034c30" "db0755bd8f51be30bfcefd7ff86158c4"
# "9282a4e0605cbd155ceae59d7fde2fd3" "f260d8e1e123b741b9673482f449348d"

lake_shinyanga_names <- data.frame(name=names(final_data_list[['Lake']][['Shinyanga Region']]))
lake_shinyanga_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Lake',region="Shinyanga Region",name=name,length=1000)),
                                            data=lake_shinyanga_names,
                                            resources=resources)
lake_shinyanga_short_id <- hipercow::hipercow_bundle_load('necessary_hen')
task_status(lake_shinyanga_short_id$ids)#necessary_hen
task_log_show(lake_shinyanga_short_id$ids[1])

lake_shinyanga_short_results <- orderly::orderly_run('run_diagnostics',parameters=list(zone='Lake',region="Shinyanga Region",
                                                                                  length=1000))
lake_shinyanga_short_results_id <-orderly_search('latest(name == "run_diagnostics" && parameter:zone == "Lake" && parameter:region == "Shinyanga Region")')
lake_shinyanga_props <- readRDS(paste0('./archive/run_diagnostics/',lake_shinyanga_short_results_id,'/props.RDS'))

lake_shinyanga_long_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Lake',region="Shinyanga Region",name=name,proposal_matrix=prop,length=5000)),
                                           data=lake_shinyanga_props,
                                           resources=resources)
lake_shinyanga_long_id <- hipercow::hipercow_bundle_load('unaroused_octopus')
task_status(lake_shinyanga_long_id$ids)#unaroused_octopus
# "a79e7b4ed0824cdf75a35747a4feae07" "81d20978f05591b202744f16ee0694b2" "70838185c5b43df38ac25ca89ac9a9d1"
# "9612947de2f4f46c5f3bc53006a8bab5" "9019870a5c4a6f8d7010f7f41b9a2ddc" "bf3d15529006481f20be2e56b1553080"

lake_simiyu_names <- data.frame(name=names(final_data_list[['Lake']][['Simiyu Region']]))
lake_simiyu_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Lake',region="Simiyu Region",name=name,length=1000)),
                                            data=lake_simiyu_names,
                                            resources=resources)
lake_simiyu_short_id <- hipercow::hipercow_bundle_load('ethnological_izuthrush')
task_status(lake_simiyu_short_id$ids)#ethnological_izuthrush
task_log_show(lake_simiyu_short_id$ids[5])

lake_simiyu_short_results <- orderly::orderly_run('run_diagnostics',parameters=list(zone='Lake',region="Simiyu Region",
                                                                                  length=1000))
lake_simiyu_short_results_id <-orderly_search('latest(name == "run_diagnostics" && parameter:zone == "Lake" && parameter:region == "Simiyu Region")')
lake_simiyu_props <- readRDS(paste0('./archive/run_diagnostics/',lake_simiyu_short_results_id,'/props.RDS'))

lake_simiyu_long_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Lake',region="Simiyu Region",name=name,proposal_matrix=prop,length=5000)),
                                           data=lake_simiyu_props,
                                           resources=resources)
lake_simiyu_long_id <- hipercow::hipercow_bundle_load('highfaluting_mussel')
task_status(lake_simiyu_long_id$ids)#highfaluting_mussel
# "eeb8d34a4e59f366c9852565be35fe41" "68358e214773ff2729a9a26ae88a6e39" "3216e74d6e1691c5ff7b58cad0fb1c68"
# "3cfd691cf6e0e8a730538066da2d3385" "a342a007ac7745a1bf1a229a35ef3d57" "5a60e9dbf583aa26ddc205fa4db6a486"

params <- c(lake_kagera_names$name,lake_geita_names$name,lake_mara_names$name,
            lake_mwanza_names$name,lake_shinyanga_names$name,lake_simiyu_names$name)
results_list <- list()
x<-1
measure_names <- c("prev_anc_all","prev_05", "clininc_all", "EIR","betaa","eff_moz_pop","moz2human_ratio","spz_rate")

for(x in 1:length(params)){
  name <- params[x]
  latest_result <- orderly::orderly_search('latest(parameter:name == environment:name)')
  file_path <- paste0('./archive/run_pmcmc/',latest_result,'/result.rds')
  results <- readRDS(file_path)
  num_months <- length(results$history[1, 1, ])

  start_month_index <- (num_months - length(months) + 1)
  chain_length <- length(results$history[1, , 1])
  burnin_end_index <- floor((chain_length*0.5))+1
  history.dfs <- lapply(measure_names, function(y) {
    as.data.frame(t(results$history[y, burnin_end_index:chain_length, start_month_index:num_months])) %>%
      mutate(month = months) %>%
      reshape2::melt(id = "month") %>%
      group_by(month) %>%
      summarise(
        median = median(value),
        lower = quantile(value, 0.025),
        upper = quantile(value, 0.975),
        .groups = "drop"
      ) %>%
      mutate(measure = y,
             Council = name)
  })
  df <- bind_rows(history.dfs)
  if (!is.null(df)) results_list[[paste0(name)]] <- df
}
results_all <- bind_rows(results_list)

saveRDS(results_all,'lake_zone_short_runs_history.rds')

western_kigoma_names <- data.frame(name=names(final_data_list[['Western']][['Kigoma Region']]))
western_kigoma_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Western',region='Kigoma Region',name=name,length=1000)),data=western_kigoma_names,
                                              resources=resources)
task_status(western_kigoma_short_id$ids)
western_kigoma_short_id <- hipercow::hipercow_bundle_load('dermatic_shrimp')
kigoma_check <- lapply(1:nrow(western_kigoma_names), function(i){
  orderly_search(paste0('parameter:zone == "Western" && parameter:region == "Kigoma Region" && parameter:name == "',western_kigoma_names$name[i],'"'))
})
kigoma_failed <- which(sapply(kigoma_check,length)==0)

western_tabora_names <- data.frame(name=names(final_data_list[['Western']][['Tabora Region']]))
western_tabora_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Western',region='Tabora Region',name=name,length=1000)),data=western_tabora_names,
                                                 resources=resources)
task_status(western_tabora_short_id$ids)
western_tabora_short_id <- hipercow::hipercow_bundle_load('locustal_slothbear')
tabora_check <- lapply(1:nrow(western_tabora_names), function(i){
  orderly_search(paste0('parameter:zone == "Western" && parameter:region == "Tabora Region" && parameter:name == "',western_tabora_names$name[i],'"'))
})
tabora_failed <- which(sapply(tabora_check,length)==0)


northern_kilimanjaro_names <- data.frame(name=names(final_data_list[['Northern']][['Kilimanjaro Region']]))
northern_kilimanjaro_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Northern',region='Kilimanjaro Region',name=name,length=1000)),data=northern_kilimanjaro_names,
                                                 resources=resources)
task_status(northern_kilimanjaro_short_id$ids)
northern_kilimanjaro_short_id <- hipercow::hipercow_bundle_load('cynophobic_drafthorse')
kilimanjaro_check <- lapply(1:nrow(northern_kilimanjaro_names), function(i){
  orderly_search(paste0('parameter:zone == "Northern" && parameter:region == "Kilimanjaro Region" && parameter:name == "',northern_kilimanjaro_names$name[i],'"'))
})
kilimanjaro_failed <- which(sapply(kilimanjaro_check,length)==0)
northern_kilimanjaro_short_id_2 <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Northern',region='Kilimanjaro Region',name=name,length=1000,seed=1234)),data=data.frame(name=northern_kilimanjaro_names[kilimanjaro_failed,]),
                                                  resources=resources)
northern_kilimanjaro_short_id_2 <- hipercow::hipercow_bundle_load('overcultured_degu')
task_status(northern_kilimanjaro_short_id_2$ids)
task_log_show(northern_kilimanjaro_short_id_2$ids[1])

northern_tanga_names <- data.frame(name=names(final_data_list[['Northern']][['Tanga Region']]))
northern_tanga_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Northern',region='Tanga Region',name=name,length=1000)),data=northern_tanga_names,
                                                 resources=resources)
task_status(northern_tanga_short_id$ids)
northern_tanga_short_id <- hipercow::hipercow_bundle_load('concretionary_earwig')
tanga_check <- lapply(1:nrow(northern_tanga_names), function(i){
  orderly_search(paste0('parameter:zone == "Northern" && parameter:region == "Tanga Region" && parameter:name == "',northern_tanga_names$name[i],'"'))
})
tanga_failed <- which(sapply(tanga_check,length)==0)


northern_arusha_names <- data.frame(name=names(final_data_list[['Northern']][['Arusha Region']]))
northern_arusha_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Northern',region='Arusha Region',name=name,length=1000)),data=northern_arusha_names,
                                                 resources=resources)
task_status(northern_arusha_short_id$ids)
task_log_show(northern_arusha_short_id$ids[1])
northern_arusha_short_id <- hipercow::hipercow_bundle_load('sticky_agouti')
arusha_check <- lapply(1:nrow(northern_arusha_names), function(i){
  char <- orderly_search(paste0('latest(parameter:length==1000 && parameter:zone == "Northern" && parameter:region == "Arusha Region" && parameter:name == "',northern_arusha_names$name[i],'")'))
  print(char)
  if(is.na(char)) return('failed')
  else return(char)
})

data.frame(council = northern_arusha_names$name, id = unlist(arusha_check))

arusha_failed <- which(sapply(arusha_check,length)==0)
northern_arusha_short_id_2 <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Northern',region='Arusha Region',name=name,length=1000,seed=4321)),data=data.frame(name=northern_arusha_names[arusha_failed,]),
                                                  resources=resources)
northern_arusha_short_id_2 <- hipercow::hipercow_bundle_load('poisonous_asiaticwildass')
task_status(northern_arusha_short_id_2$ids)
task_log_show(northern_arusha_short_id_2$ids[1])

central_dodoma_names <- data.frame(name=names(final_data_list[['Central']][['Dodoma Region']]))
central_dodoma_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Central',region='Dodoma Region',name=name,length=1000)),data=central_dodoma_names,
                                                  resources=resources)
central_dodoma_short_id <- hipercow::hipercow_bundle_load('literalminded_canary')
task_status(central_dodoma_short_id$ids) #literalminded_canary
task_log_show(central_dodoma_short_id$ids[1])
dodoma_check <- lapply(1:nrow(central_dodoma_names), function(i){
  orderly_search(paste0('parameter:zone == "Central" && parameter:region == "Dodoma Region" && parameter:name == "',central_dodoma_names$name[i],'"'))
})
dodoma_failed <- which(sapply(dodoma_check,length)==0)
central_dodoma_short_id_2 <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Central',region='Dodoma Region',name=name,length=1000,seed=5678)),data=data.frame(name=central_dodoma_names[dodoma_failed,]),
                                                  resources=resources)
central_dodoma_short_id_2 <- hipercow::hipercow_bundle_load('uniform_zethusspinipes')
task_status(central_dodoma_short_id_2$ids)
task_log_show(central_dodoma_short_id_2$ids[1])


central_singida_names <- data.frame(name=names(final_data_list[['Central']][['Singida Region']]))
central_singida_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Central',region='Singida Region',name=name,length=1000)),data=central_singida_names,
                                                 resources=resources)
central_singida_short_id <- hipercow::hipercow_bundle_load('locustal_slothbear')
task_status(central_singida_short_id$ids)
task_log_show(central_singida_short_id$ids[1])
singida_check <- lapply(1:nrow(central_singida_names), function(i){
  orderly_search(paste0('parameter:zone == "Central" && parameter:region == "Singida Region" && parameter:name == "',central_singida_names$name[i],'"'))
})
singida_failed <- which(sapply(singida_check,length)==0)
central_singida_short_id_2 <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Central',region='Singida Region',name=name,length=1000,seed=1234)),data=data.frame(name=central_singida_names[singida_failed,]),
                                                  resources=resources)
central_singida_short_id_2 <- hipercow::hipercow_bundle_load('cayenned_shrew')
task_status(central_singida_short_id_2$ids)
task_log_show(central_singida_short_id_2$ids[1])


central_manyara_names <- data.frame(name=names(final_data_list[['Central']][['Manyara Region']]))
central_manyara_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Central',region='Manyara Region',name=name,length=1000)),data=central_manyara_names,
                                                 resources=resources)
central_manyara_short_id <- hipercow::hipercow_bundle_load('dermatic_shrimp')
central_manyara_ids <- orderly_search('parameter:zone == "Central" && parameter:region == "Manyara Region"')
manyara_check <- lapply(1:nrow(central_manyara_names), function(i){
  orderly_search(paste0('parameter:zone == "Central" && parameter:region == "Manyara Region" && parameter:name == "',central_manyara_names$name[i],'"'))
})
manyara_failed <- which(sapply(manyara_check,length)==0)
central_manyara_short_id_236 <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Central',region='Manyara Region',name=name,length=1000,seed=236)),data=data.frame(name=central_manyara_names[manyara_failed,]),
                                                  resources=resources)
central_manyara_short_id_236 <- hipercow::hipercow_bundle_load('astromantic_fowl')
task_status(central_manyara_short_id_236$ids)
task_log_show(central_manyara_short_id_236$ids[2])


task_status(central_manyara_short_id$ids)
task_log_show(central_manyara_short_id$ids[1])

SouthernHighlands_zone
southerhighlands_iringa_names <- data.frame(name=names(final_data_list[['Southern Highlands']][['Iringa Region']]))
southerhighlands_iringa_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Southern Highlands',region='Iringa Region',name=name,length=1000)),data=southerhighlands_iringa_names,
                                                 resources=resources)
southerhighlands_iringa_short_id <- hipercow::hipercow_bundle_load('cynophobic_drafthorse')
task_status(southerhighlands_iringa_short_id$ids) #cynophobic_drafthorse
southern_iringa_ids <- orderly_search('parameter:zone == "Southern Highlands" && parameter:region == "Iringa Region"')
orderly_search('parameter:zone == "Southern Highlands" && parameter:region == "Iringa Region" && parameter:name == "Mufindi District Council"')
task_log_show(southerhighlands_iringa_short_id$ids[1])

southern_iringa_failed <- which(as.vector(southerhighlands_iringa_names)$name %in% c('Iringa District Council','Mafinga Town Council'))
southerhighlands_iringa_short_id_1_4 <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Southern Highlands',region='Iringa Region',name=name,length=1000,seed=7802)),data=data.frame(name=southerhighlands_iringa_names[southern_iringa_failed,]),
                                                          resources=resources)
southerhighlands_iringa_short_id_1_4 <- hipercow::hipercow_bundle_load('olericultural_xenopus')
task_status(southerhighlands_iringa_short_id_1_4$ids) #olericultural_xenopus
task_log_show(southerhighlands_iringa_short_id_1_4$ids[1])

southerhighlands_njombe_names <- data.frame(name=names(final_data_list[['Southern Highlands']][['Njombe Region']]))
southerhighlands_njombe_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Southern Highlands',region='Njombe Region',name=name,length=1000)),data=southerhighlands_njombe_names,
                                                          resources=resources)
southerhighlands_njombe_short_id <- hipercow::hipercow_bundle_load('concretionary_earwig')
task_status(southerhighlands_njombe_short_id$ids) #concretionary_earwig
southern_njombe_ids <- orderly_search('parameter:zone == "Southern Highlands" && parameter:region == "Njombe Region"')
task_log_show(southerhighlands_njombe_short_id$ids[1])

southerhighlands_ruvuma_names <- data.frame(name=names(final_data_list[['Southern Highlands']][['Ruvuma Region']]))
southerhighlands_ruvuma_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Southern Highlands',region='Ruvuma Region',name=name,length=1000)),data=southerhighlands_ruvuma_names,
                                                          resources=resources)
southerhighlands_ruvuma_short_id <- hipercow::hipercow_bundle_load('sticky_agouti')
task_status(southerhighlands_ruvuma_short_id$ids) #sticky_agouti
southern_ruvuma_ids <- orderly_search('parameter:zone == "Southern Highlands" && parameter:region == "Ruvuma Region"')
task_log_show(southerhighlands_ruvuma_short_id$ids[1])

southern_lindi_names <- data.frame(name=names(final_data_list[['Southern']][['Lindi Region']]))
southern_lindi_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Southern',region='Lindi Region',name=name,length=1000)),data=southern_lindi_names,
                                                          resources=resources)
southern_lindi_short_id <- hipercow::hipercow_bundle_load('literalminded_canary')
task_status(southern_lindi_short_id$ids) #literalminded_canary
task_log_show(southern_lindi_short_id$ids[1])
southern_lindi_ids <- orderly_search('parameter:zone == "Southern"&& parameter:region == "Lindi Region"')
task_status(southern_lindi_ids) #literalminded_canary

southern_mtwara_names <- data.frame(name=names(final_data_list[['Southern']][['Mtwara Region']]))
southern_mtwara_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Southern',region='Mtwara Region',name=name,length=1000)),data=southern_mtwara_names,
                                                 resources=resources)
southern_mtwara_short_id <- hipercow::hipercow_bundle_load('iridescent_oyster')
task_status(southern_mtwara_short_id$ids) #iridescent_oyster
task_log_show(southern_mtwara_short_id$ids[1])

southwesthighlands_mbeya_names <- data.frame(name=names(final_data_list[['Southwest Highlands']][['Mbeya Region']]))
southwesthighlands_mbeya_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Southwest Highlands',region='Mbeya Region',name=name,length=1000)),data=southwesthighlands_mbeya_names,
                                                  resources=resources)
southwesthighlands_mbeya_short_id <- hipercow::hipercow_bundle_load('locustal_slothbear')
task_status(southwesthighlands_mbeya_short_id$ids) #locustal_slothbear
task_log_show(southwesthighlands_mbeya_short_id$ids[1])

##Get names of councils in Mbeya region that failed to complete runs
which(task_status(southwesthighlands_mbeya_short_id$ids)!='success')
orderly_search('parameter:zone == "Southwest Highlands" && parameter:region == "Mbeya Region" && parameter:name == "Kyela District Council"')
southwesthighlands_mbeya_short_id_6 <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Southwest Highlands',region='Mbeya Region',name=name,length=1000,seed=11225)),data=data.frame(name=southwesthighlands_mbeya_names[6,]),
                                                           resources=resources)
#equine_seriema
southwesthighlands_mbeya_short_id_6 <- hipercow::hipercow_bundle_load('equine_seriema')
task_status(southwesthighlands_mbeya_short_id_6$ids) #equine_seriema
task_log_show(southwesthighlands_mbeya_short_id_6$ids[1])

southwesthighlands_rukwa_names <- data.frame(name=names(final_data_list[['Southwest Highlands']][['Rukwa Region']]))
southwesthighlands_rukwa_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Southwest Highlands',region='Rukwa Region',name=name,length=1000)),data=southwesthighlands_rukwa_names,
                                                           resources=resources)
southwesthighlands_rukwa_short_id <- hipercow::hipercow_bundle_load('dermatic_shrimp')
task_status(southwesthighlands_rukwa_short_id$ids) #dermatic_shrimp
task_log_show(southwesthighlands_rukwa_short_id$ids[1])

southwesthighlands_katavi_names <- data.frame(name=names(final_data_list[['Southwest Highlands']][['Katavi Region']]))
southwesthighlands_katavi_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Southwest Highlands',region='Katavi Region',name=name,length=1000)),data=southwesthighlands_katavi_names,
                                                           resources=resources)
southwesthighlands_katavi_short_id <- hipercow::hipercow_bundle_load('cynophobic_drafthorse')
task_status(southwesthighlands_katavi_short_id$ids) #cynophobic_drafthorse
task_log_show(southwesthighlands_katavi_short_id$ids[1])

southwesthighlands_songwe_names <- data.frame(name=names(final_data_list[['Southwest Highlands']][['Songwe Region']]))
southwesthighlands_songwe_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Southwest Highlands',region='Songwe Region',name=name,length=1000)),data=southwesthighlands_songwe_names,
                                                           resources=resources)
southwesthighlands_songwe_short_id <- hipercow::hipercow_bundle_load('concretionary_earwig')
task_status(southwesthighlands_songwe_short_id$ids) #concretionary_earwig
task_log_show(southwesthighlands_songwe_short_id$ids[1])

eastern_dar_names <- data.frame(name=names(final_data_list[['Eastern']][['Dar Es Salaam Region']]))
eastern_dar_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Eastern',region='Dar Es Salaam Region',name=name,length=1000)),data=eastern_dar_names,
                                                           resources=resources)
eastern_dar_short_id <- hipercow::hipercow_bundle_load('sticky_agouti')
task_status(eastern_dar_short_id$ids) #sticky_agouti
task_log_show(eastern_dar_short_id$ids[1])

eastern_pwani_names <- data.frame(name=names(final_data_list[['Eastern']][['Pwani Region']]))
eastern_pwani_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Eastern',region='Pwani Region',name=name,length=1000)),data=eastern_pwani_names,
                                                 resources=resources)
eastern_pwani_short_id <- hipercow::hipercow_bundle_load('literalminded_canary')
task_status(eastern_pwani_short_id$ids) #literalminded_canary
task_log_show(eastern_pwani_short_id$ids[1])

eastern_morogoro_names <- data.frame(name=names(final_data_list[['Eastern']][['Morogoro Region']]))
eastern_morogoro_short_id <- task_create_bulk_expr(orderly::orderly_run('run_pmcmc',parameters=list(zone='Eastern',region='Morogoro Region',name=name,length=1000)),data=eastern_morogoro_names,
                                                 resources=resources)
eastern_morogoro_short_id <- hipercow::hipercow_bundle_load('iridescent_oyster')
task_status(eastern_morogoro_short_id$ids) #iridescent_oyster
task_log_show(eastern_morogoro_short_id$ids[1])

search_query <- paste0('latest(parameter:zone == "', zone, '" && ',
                            'parameter:region == "', region, '")')

result_ids <- orderly::orderly_search(search_query,name='summarise_results')
View(readRDS(paste0('./archive/summarise_results/',result_ids,'/results_summary.rds')))

##Batch summarise all councils:
source("./batch_summarise_all.R")

##Compile all results
orderly_run('compile_results')

orderly::orderly_run(
  "create_diagnostic_dashboard",
  parameters = list(make_plots = FALSE, max_councils = 0, sample_seed = 0)
)
# Then retrieve results
result_id <- orderly_search('latest(name == "compile_results")')
global_results <- readRDS(paste0('./archive/compile_results/', result_id, '/global_results.rds'))
compilation_log <- readRDS(paste0('./archive/compile_results/', result_id, '/compilation_log.rds'))


orderly_run('diagnostics_report')

orderly_run('diag_dash')
shiny::runApp("Y:/jth/tz_stt_anc1/archive/diag_dash/20260207-161059-cc0f1b6b/diag_dash_app.R", launch.browser = TRUE)

# View results
result_id <- orderly_search('latest(name == "diagnostics_report")')
global_diag <- readRDS(paste0('./archive/diagnostics_report/', result_id, '/global_diagnostics.rds'))
summary_stats <- readRDS(paste0('./archive/diagnostics_report/', result_id, '/summary_statistics.rds'))

##Compare with SMPS
orderly::orderly_run('08_smps_comp')

orderly::orderly_run("09_program_story_figure")
