## Check existing model outputs and create diagnostics
## Working with BUGS outputs since rstan is not installed

library(dplyr)
library(ggplot2)
library(patchwork)
library(viridis)
library(R2OpenBUGS)
library(coda)

cat("Loading existing model output...\n")

# Load the all-ages model (this is the one matching the Stan model intent)
run_full <- readRDS("run_full_allagesfixedANCprev.rds")

cat("\n========================================\n")
cat("MODEL SUMMARY\n")
cat("========================================\n")
print(run_full, digits = 3)

cat("\n========================================\n")
cat("CHECKING KEY PARAMETERS\n")
cat("========================================\n")

# Extract summary for key parameters
params_of_interest <- c("av_lo_child", "intercept_pg", "gradient_pg", "sigma_c", "sigma_int")

# Check which parameters are available
available_params <- rownames(run_full$summary)
params_to_check <- intersect(params_of_interest, available_params)

if(length(params_to_check) > 0) {
  cat("\nParameter estimates:\n")
  print(run_full$summary[params_to_check, ])
} else {
  cat("\nKey parameters not found. Available parameters:\n")
  print(head(available_params, 20))
}

# Check DIC
cat("\nDIC:", run_full$DIC, "\n")

# Convert to MCMC object for diagnostics
mcmc_obj <- as.mcmc.list(run_full)

cat("\n========================================\n")
cat("CREATING DIAGNOSTIC PLOTS\n")
cat("========================================\n")

# Traceplots
cat("Creating traceplots...\n")
png("bugs_traceplots.png", width = 12, height = 10, units = "in", res = 300)
par(mfrow = c(5, 1), mar = c(3, 4, 2, 1))
if("av_lo_child" %in% available_params) {
  traceplot(mcmc_obj[, "av_lo_child"], main = "av_lo_child", col = 1:3)
}
if("intercept_pg" %in% available_params) {
  traceplot(mcmc_obj[, "intercept_pg"], main = "intercept_pg", col = 1:3)
}
if("gradient_pg" %in% available_params) {
  traceplot(mcmc_obj[, "gradient_pg"], main = "gradient_pg", col = 1:3)
}
if("sigma_c" %in% available_params) {
  traceplot(mcmc_obj[, "sigma_c"], main = "sigma_c", col = 1:3)
}
if("sigma_int" %in% available_params) {
  traceplot(mcmc_obj[, "sigma_int"], main = "sigma_int", col = 1:3)
}
dev.off()

# Density plots
cat("Creating density plots...\n")
png("bugs_densities.png", width = 12, height = 10, units = "in", res = 300)
par(mfrow = c(5, 1), mar = c(3, 4, 2, 1))
if("av_lo_child" %in% available_params) {
  densplot(mcmc_obj[, "av_lo_child"], main = "av_lo_child")
}
if("intercept_pg" %in% available_params) {
  densplot(mcmc_obj[, "intercept_pg"], main = "intercept_pg")
}
if("gradient_pg" %in% available_params) {
  densplot(mcmc_obj[, "gradient_pg"], main = "gradient_pg")
}
if("sigma_c" %in% available_params) {
  densplot(mcmc_obj[, "sigma_c"], main = "sigma_c")
}
if("sigma_int" %in% available_params) {
  densplot(mcmc_obj[, "sigma_int"], main = "sigma_int")
}
dev.off()

# Autocorrelation
cat("Creating autocorrelation plots...\n")
png("bugs_autocorr.png", width = 12, height = 10, units = "in", res = 300)
par(mfrow = c(5, 1), mar = c(3, 4, 2, 1))
if("av_lo_child" %in% available_params) {
  autocorr.plot(mcmc_obj[, "av_lo_child"], main = "av_lo_child")
}
if("intercept_pg" %in% available_params) {
  autocorr.plot(mcmc_obj[, "intercept_pg"], main = "intercept_pg")
}
if("gradient_pg" %in% available_params) {
  autocorr.plot(mcmc_obj[, "gradient_pg"], main = "gradient_pg")
}
if("sigma_c" %in% available_params) {
  autocorr.plot(mcmc_obj[, "sigma_c"], main = "sigma_c")
}
if("sigma_int" %in% available_params) {
  autocorr.plot(mcmc_obj[, "sigma_int"], main = "sigma_int")
}
dev.off()

# Gelman-Rubin diagnostic
cat("\nGelman-Rubin diagnostic (should be < 1.1):\n")
if(length(params_to_check) > 0) {
  gr <- gelman.diag(mcmc_obj[, params_to_check])
  print(gr)
}

cat("\n========================================\n")
cat("DIAGNOSTICS COMPLETE!\n")
cat("========================================\n")
cat("\nDiagnostic plots saved:\n")
cat("  - bugs_traceplots.png\n")
cat("  - bugs_densities.png\n")
cat("  - bugs_autocorr.png\n")
cat("\nModel appears to be from BUGS, not Stan.\n")
cat("Model fit object: run_full_allagesfixedANCprev.rds\n")
cat("Posterior summary: mcmc_sim_summary_stan.rds (already exists)\n")
