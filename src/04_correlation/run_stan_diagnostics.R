## Run Stan Model with Full Diagnostics for 04_correlation
## This script runs the correlation model and produces diagnostic outputs

library(rstan)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(patchwork)
library(viridis)
library(bayesplot)
library(loo)

# Set Stan options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Load helper functions
source('addCIs.R')
source('theme_base.R')

cat("Loading data...\n")
# Load data files
dqa_hf_2017 <- readRDS('dqa_hf_remove_duplicates_2017.rds')
dqa_hf_2022 <- readRDS('dqa_hf_remove_duplicates_2022.rds')
van_eijk <- read_excel('data/paper_data_strat_G.xlsx')

u5yo_prev <- read_xlsx('data/u5yo_prev_dhs_tanz_2017-2022.xlsx',sheet = 'Regions')%>%
  mutate(Region = ifelse(Regions=='Dar es Salaam','Dar Es Salaam',Regions),
         positive = round(tested * prev/100))
u5yo_prev <- addCIs(df=u5yo_prev,Ys=u5yo_prev$positive,Ns=u5yo_prev$tested)

# Summarize 2017 data
dqa_hf_2017_sum <- dqa_hf_2017 %>%
  group_by(Region)%>%
  summarise(ANC_test = sum(ANC_test,na.rm = TRUE),
            ANC_pos = sum(ANC_pos,na.rm = TRUE),
            ANC_test_lt20 = sum(ANC_test_lt20,na.rm = TRUE),
            ANC_pos_lt20 = sum(ANC_pos_lt20,na.rm = TRUE),
            ANC_test_ge20 = sum(ANC_test_ge20,na.rm = TRUE),
            ANC_pos_ge20 = sum(ANC_pos_ge20,na.rm = TRUE))
dqa_hf_2017_sum <- addCIs(df=dqa_hf_2017_sum,Ys=dqa_hf_2017_sum$ANC_pos,Ns=dqa_hf_2017_sum$ANC_test)%>%
  rename(mean_total = mean, lower_total = lower, upper_total = upper)
dqa_hf_2017_sum <- addCIs(df=dqa_hf_2017_sum,Ys=dqa_hf_2017_sum$ANC_pos_lt20,Ns=dqa_hf_2017_sum$ANC_test_lt20)%>%
  rename(mean_lt20 = mean, lower_lt20 = lower, upper_lt20 = upper)
dqa_hf_2017_sum <- addCIs(df=dqa_hf_2017_sum,Ys=dqa_hf_2017_sum$ANC_pos_ge20,Ns=dqa_hf_2017_sum$ANC_test_ge20)%>%
  rename(mean_ge20 = mean, lower_ge20 = lower, upper_ge20 = upper)
dqa_hf_2017_sum$year <- 2017
dqa_hf_2017_sum$Region <- gsub(' Region','',dqa_hf_2017_sum$Region)

# Summarize 2022 data
dqa_hf_2022_sum <- dqa_hf_2022 %>%
  group_by(Region)%>%
  summarise(ANC_test = sum(ANC_test,na.rm = TRUE),
            ANC_pos = sum(ANC_pos,na.rm = TRUE),
            ANC_test_10_14 = sum(ANC_test_10_14,na.rm = TRUE),
            ANC_pos_10_14 = sum(ANC_pos_10_14,na.rm = TRUE),
            ANC_test_15_19 = sum(ANC_test_15_19,na.rm = TRUE),
            ANC_pos_15_19 = sum(ANC_pos_15_19,na.rm = TRUE),
            ANC_test_20_24 = sum(ANC_test_20_24,na.rm = TRUE),
            ANC_pos_20_24 = sum(ANC_pos_20_24,na.rm = TRUE),
            ANC_test_25_29 = sum(ANC_test_25_29,na.rm = TRUE),
            ANC_pos_25_29 = sum(ANC_pos_25_29,na.rm = TRUE),
            ANC_test_30_34 = sum(ANC_test_30_34,na.rm = TRUE),
            ANC_pos_30_34 = sum(ANC_pos_30_34,na.rm = TRUE),
            ANC_test_ge35 = sum(ANC_test_ge35,na.rm = TRUE),
            ANC_pos_ge35 = sum(ANC_pos_ge35,na.rm = TRUE))%>%
  mutate(ANC_test_lt20 = ANC_test_10_14+ANC_test_15_19,
         ANC_pos_lt20 = ANC_pos_10_14+ANC_pos_15_19,
         ANC_test_ge20 = ANC_test_20_24 + ANC_test_25_29 + ANC_test_30_34 + ANC_test_ge35,
         ANC_pos_ge20 = ANC_pos_20_24 + ANC_pos_25_29 + ANC_pos_30_34 + ANC_pos_ge35)
dqa_hf_2022_sum <- addCIs(df=dqa_hf_2022_sum,Ys=dqa_hf_2022_sum$ANC_pos,Ns=dqa_hf_2022_sum$ANC_test)%>%
  rename(mean_total = mean, lower_total = lower, upper_total = upper)
dqa_hf_2022_sum <- addCIs(df=dqa_hf_2022_sum,Ys=dqa_hf_2022_sum$ANC_pos_lt20,Ns=dqa_hf_2022_sum$ANC_test_lt20)%>%
  rename(mean_lt20 = mean, lower_lt20 = lower, upper_lt20 = upper)
dqa_hf_2022_sum <- addCIs(df=dqa_hf_2022_sum,Ys=dqa_hf_2022_sum$ANC_pos_ge20,Ns=dqa_hf_2022_sum$ANC_test_ge20)%>%
  rename(mean_ge20 = mean, lower_ge20 = lower, upper_ge20 = upper)
dqa_hf_2022_sum$year <- 2022
dqa_hf_2022_sum$Region <- gsub(' Region','',dqa_hf_2022_sum$Region)

# Merge DHS and ANC data
dhs_anc_merged <- bind_rows(dqa_hf_2022_sum,dqa_hf_2017_sum)%>%
  left_join(u5yo_prev,by=c('Region','year'))%>%
  mutate(year=factor(year,levels=c('2017','2022')))

# Add Van Eijk et al data
all_ve <- van_eijk %>%
  subset(subG %in% c('P','M')) %>%
  group_by(Site)%>%
  dplyr::summarise(inf_prev_n=mean(child_Y),
                   inf_N=mean(child_N),
                   ANC_prev_n=sum(preg_Y),
                   ANC_N=sum(preg_N))%>%
  dplyr::select(-Site) %>%
  dplyr::rename(positive=inf_prev_n,
                tested=inf_N,
                ANC_pos=ANC_prev_n,
                ANC_test=ANC_N)
all_ve <- addCIs(df=all_ve,Ys=all_ve$ANC_pos,Ns=all_ve$ANC_test)%>%
  rename(mean_total = mean, lower_total = lower, upper_total = upper)
all_ve <- addCIs(df=all_ve,Ys=all_ve$positive,Ns=all_ve$tested)
all_ve$Region <- 'Van Eijk, et al.'
all_ve$year <- as.factor('Van Eijk, et al.')

dhs_anc_merged <- bind_rows(dhs_anc_merged,all_ve)

cat("Data loaded. N sites =", nrow(dhs_anc_merged), "\n")

# Prepare Stan data
stan_data <- list(
  N             = nrow(dhs_anc_merged),
  pos_child     = as.integer(dhs_anc_merged$positive),
  total_child   = as.integer(dhs_anc_merged$tested),
  pos_preg_pg   = as.integer(dhs_anc_merged$ANC_pos),
  total_preg_pg = as.integer(dhs_anc_merged$ANC_test)
)

cat("\nCompiling Stan model...\n")
# Compile Stan model
sm <- stan_model("anc_allages.stan")

cat("\nRunning Stan model with NUTS sampler...\n")
cat("This may take several minutes...\n")
# Sample with NUTS
fit_stan <- sampling(
  sm,
  data       = stan_data,
  chains     = 4,
  iter       = 2000,
  warmup     = 500,
  control    = list(adapt_delta = 0.95, max_treedepth = 12),
  seed       = 123
)

cat("\n========================================\n")
cat("STAN MODEL SUMMARY\n")
cat("========================================\n")
print(fit_stan, pars = c(
  "av_lo_child","intercept_pg","gradient_pg",
  "sigma_c","sigma_int"
))

cat("\n========================================\n")
cat("CHECKING CONVERGENCE DIAGNOSTICS\n")
cat("========================================\n")

# Check Rhat and n_eff
summ <- summary(fit_stan, pars = c("av_lo_child","intercept_pg","gradient_pg","sigma_c","sigma_int"))$summary
cat("\nRhat values (should be < 1.01):\n")
print(summ[, "Rhat"])
cat("\nEffective sample sizes:\n")
print(summ[, "n_eff"])

# Check for divergent transitions
sampler_params <- get_sampler_params(fit_stan, inc_warmup = FALSE)
divergent <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))
cat("\nNumber of divergent transitions:", divergent, "\n")
if(divergent > 0) {
  cat("WARNING: Model has divergent transitions. Consider increasing adapt_delta.\n")
} else {
  cat("Good: No divergent transitions detected.\n")
}

# Check tree depth
max_treedepth <- sum(sapply(sampler_params, function(x) sum(x[, "treedepth__"] >= 12)))
cat("Number of iterations hitting max treedepth:", max_treedepth, "\n")
if(max_treedepth > 0) {
  cat("WARNING: Some iterations hit max treedepth. Consider increasing max_treedepth.\n")
}

cat("\n========================================\n")
cat("SAVING DIAGNOSTIC PLOTS\n")
cat("========================================\n")

# Traceplots
cat("Saving traceplots...\n")
png("stan_traceplots.png", width = 10, height = 8, units = "in", res = 300)
traceplot(fit_stan, pars = c("av_lo_child","intercept_pg","gradient_pg","sigma_c","sigma_int"))
dev.off()

# Pairs plot
cat("Saving pairs plot...\n")
png("stan_pairs.png", width = 10, height = 10, units = "in", res = 300)
pairs(fit_stan, pars = c("av_lo_child","intercept_pg","gradient_pg","sigma_c","sigma_int"))
dev.off()

# Autocorrelation
cat("Saving autocorrelation plot...\n")
png("stan_autocorr.png", width = 10, height = 6, units = "in", res = 300)
stan_ac(fit_stan, pars = c("av_lo_child","intercept_pg","gradient_pg","sigma_c","sigma_int"))
dev.off()

# Additional bayesplot diagnostics
cat("Saving additional diagnostic plots...\n")
posterior <- as.array(fit_stan)

png("stan_mcmc_areas.png", width = 8, height = 6, units = "in", res = 300)
mcmc_areas(posterior, pars = c("av_lo_child","intercept_pg","gradient_pg","sigma_c","sigma_int"))
dev.off()

png("stan_mcmc_dens_overlay.png", width = 10, height = 6, units = "in", res = 300)
mcmc_dens_overlay(posterior, pars = c("av_lo_child","intercept_pg","gradient_pg","sigma_c","sigma_int"))
dev.off()

# Save the fitted model
cat("\nSaving fitted model...\n")
saveRDS(fit_stan, "fit_stan_allages_NEW.rds")

cat("\n========================================\n")
cat("DIAGNOSTICS COMPLETE!\n")
cat("========================================\n")
cat("\nResults saved:\n")
cat("  - fit_stan_allages_NEW.rds (fitted model object)\n")
cat("  - stan_traceplots.png\n")
cat("  - stan_pairs.png\n")
cat("  - stan_autocorr.png\n")
cat("  - stan_mcmc_areas.png\n")
cat("  - stan_mcmc_dens_overlay.png\n")
cat("\nModel is ready for generating predictions!\n")
