library(orderly2)
# remotes::install_github("mrc-ide/orderly.sharedfile")
# orderly_init(force=TRUE)

orderly_cleanup('01_read_data')
orderly_cleanup('02_data_quality')
orderly_cleanup('03_attendance_rate')
orderly_cleanup('04_correlation')

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
library(ggplot2)
library(mcmcplots)
library(htmltools)
library(bayesplot)
library(rstanarm)


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
