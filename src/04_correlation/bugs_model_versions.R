###Fixed ANC prevalence
#----------------------------------------------------------
# 1. Define the BUGS model
#----------------------------------------------------------
preg_child_predictive_model <- function(){
  for (i in 1:N) {
    # 1) CHILDREN UNDER 5:
    pos_child[i] ~ dbin(p_child[i], total_child[i])

    # 2) “STABILISED” PREGNANT WOMEN PROPORTIONS (to avoid exactly 0 or 1):
    p_preg_pg[i] <- (pos_preg_pg[i] + 0.5) / (total_preg_pg[i] + 1)
    p_preg_mg[i] <- (pos_preg_mg[i] + 0.5) / (total_preg_mg[i] + 1)

    # 3) LOGIT‐TRANSFORM THOSE:
    logit_pg[i] <- logit(p_preg_pg[i])
    logit_mg[i] <- logit(p_preg_mg[i])

    # 4) LINEAR PREDICTOR FOR CHILD:
    logit(p_child[i]) <- alpha +
      beta_pg * logit_pg[i] +
      beta_mg * logit_mg[i] +
      RE[i]

    # 5) RANDOM EFFECT:
    RE[i] ~ dnorm(0, tau_RE)
  }

  # PRIORS:
  alpha   ~ dnorm(0, 0.001)
  beta_pg ~ dnorm(0, 0.001)
  beta_mg ~ dnorm(0, 0.001)
  tau_RE  ~ dgamma(0.001, 0.001)
  sigma_RE <- 1 / sqrt(tau_RE)
}

#----------------------------------------------------------
# 2. Write model to file
#----------------------------------------------------------
model.file <- "model_predictive.txt"
write.model(preg_child_predictive_model, model.file)

#----------------------------------------------------------
# 3. Prepare data
#----------------------------------------------------------
N_sites <- nrow(dhs_anc_merged)

data_list <- list(
  pos_child     = as.integer(dhs_anc_merged$positive),
  total_child   = as.integer(dhs_anc_merged$tested),
  pos_preg_pg   = as.integer(dhs_anc_merged$ANC_pos_lt20),
  total_preg_pg = as.integer(dhs_anc_merged$ANC_test_lt20),
  pos_preg_mg   = as.integer(dhs_anc_merged$ANC_pos_ge20),
  total_preg_mg = as.integer(dhs_anc_merged$ANC_test_ge20),
  N             = N_sites
)

#----------------------------------------------------------
# 4. Initial values
#----------------------------------------------------------
inits <- function() {
  list(
    alpha   = rnorm(1, 0, 1),
    beta_pg = rnorm(1, 0, 1),
    beta_mg = rnorm(1, 0, 1),
    tau_RE  = rgamma(1, 1, 1)
  )
}


#----------------------------------------------------------
# 5. Parameters to monitor
#----------------------------------------------------------
params <- c("alpha", "beta_pg", "beta_mg", "sigma_RE", "p_child")

#----------------------------------------------------------
# 6. Run full model
#----------------------------------------------------------
run_full <- bugs(
  data            = data_list,
  inits           = inits,
  parameters.to.save = params,
  model.file      = "model_predictive.txt",
  n.chains        = 3,
  n.iter          = 50000,
  n.burnin        = 10000,
  n.thin          = 10,
  debug           = TRUE,
  DIC             = TRUE,
  working.directory = getwd()
)
saveRDS(run_full,file = 'run_full_fixedANCprev.rds')
#----------------------------------------------------------
# 7. Compute LOOIC
#----------------------------------------------------------
# 1) Pull out all the saved p_child[1: N_sites] from all chains:
p_child_draws <- run_full$sims.list$p_child
#   – this is an array (n_draws_total × N_sites).
#   n_draws_total = (#chains)*(#iterations saved per chain).

# 2) Create a matrix (n_draws_total × N_sites) of log‐likelihoods:
y_obs <- data_list$pos_child
n_obs <- data_list$total_child

# A little helper: for each site i, we take all draws of p_child_draws[ , i ],
# then compute dbinom( y_obs[i], size = n_obs[i], prob = that_draw, log=TRUE ).

log_lik_mat <- matrix(NA,
                      nrow = nrow(p_child_draws),
                      ncol = N_sites)

for (i in seq_len(N_sites)) {
  log_lik_mat[, i] <- dbinom(
    x    = y_obs[i],
    size = n_obs[i],
    prob = p_child_draws[, i],
    log  = TRUE
  )
}

# 3) Now feed that to loo::loo():
loo_result <- loo(log_lik_mat)
saveRDS(loo_result,file='loo_result_fixedANCprev.rds')
print(loo_result)


#----------------------------------------------------------
# 8. Leave-One-Out Cross Validation for Plotting
#----------------------------------------------------------
# 1) Preallocate vectors to hold each site’s held‐out posterior summaries:
predicted_means  <- numeric(N_sites)
predicted_lowers <- numeric(N_sites)
predicted_uppers <- numeric(N_sites)
observed_props   <- y_obs / n_obs    # “observed” child prevalence at each site

# 2) Loop over sites:
for (i in seq_len(N_sites)) {
  cat("Leaving out site", i, "of", N_sites, "...\n")

  # 2a) Which rows remain?
  loo_idx <- setdiff(seq_len(N_sites), i)
  n_loo   <- length(loo_idx)

  # 2b) Build the LOO data list:
  loo_data <- list(
    pos_child     = as.integer(dhs_anc_merged$positive[loo_idx]),
    total_child   = as.integer(dhs_anc_merged$tested[loo_idx]),
    pos_preg_pg   = as.integer(dhs_anc_merged$ANC_pos_lt20[loo_idx]),
    total_preg_pg = as.integer(dhs_anc_merged$ANC_test_lt20[loo_idx]),
    pos_preg_mg   = as.integer(dhs_anc_merged$ANC_pos_ge20[loo_idx]),
    total_preg_mg = as.integer(dhs_anc_merged$ANC_test_ge20[loo_idx]),
    N             = n_loo
  )

  # 2c) Run BUGS on the “leave‐i‐out” data:
  loo_inits <- function() {
    list(
      alpha   = rnorm(1, 0, 1),
      beta_pg = rnorm(1, 0, 1),
      beta_mg = rnorm(1, 0, 1),
      tau_RE  = rgamma(1, 1, 1)
    )
  }

  loo_fit <- bugs(
    data               = loo_data,
    inits              = loo_inits,
    parameters.to.save = c("alpha", "beta_pg", "beta_mg"),
    model.file         = "model_predictive.txt",
    n.chains           = 2,
    n.iter             = 10000,
    n.burnin           = 2000,
    n.thin             = 5,
    DIC                = FALSE,
    debug              = FALSE,
    working.directory  = getwd()
  )

  # 2d) Extract posterior draws for alpha, beta_pg, beta_mg:
  alpha_samps  <- loo_fit$sims.list$alpha
  beta_pg_samps <- loo_fit$sims.list$beta_pg
  beta_mg_samps <- loo_fit$sims.list$beta_mg

  # 2e) Compute the “observed” pregnant‐woman proportions at site i (stabilized):
  pg_obs <- (dhs_anc_merged$ANC_pos_lt20[i] + 0.5) / (dhs_anc_merged$ANC_test_lt20[i] + 1)
  mg_obs <- (dhs_anc_merged$ANC_pos_ge20[i] + 0.5) / (dhs_anc_merged$ANC_test_ge20[i] + 1)
  logit_pg_i <- qlogis(pg_obs)
  logit_mg_i <- qlogis(mg_obs)

  # 2f) For each posterior draw, predict log‐odds → p_child at held‐out site:
  #     logit(p_pred) = alpha + beta_pg*logit_pg_i + beta_mg*logit_mg_i.
  n_draws <- length(alpha_samps)
  log_odds_pred_draws <- alpha_samps +
    beta_pg_samps * logit_pg_i +
    beta_mg_samps * logit_mg_i
  p_pred_draws <- plogis(log_odds_pred_draws)

  # 2g) Summarize:
  predicted_means[i]  <- mean(p_pred_draws)
  predicted_lowers[i] <- quantile(p_pred_draws, 0.025)
  predicted_uppers[i] <- quantile(p_pred_draws, 0.975)
}

# 3) Build a data‐frame for plotting:
plot_df <- data.frame(
  site      = seq_len(N_sites),
  observed  = observed_props,
  predicted = predicted_means,
  lower     = predicted_lowers,
  upper     = predicted_uppers
)
saveRDS(plot_df,file='df_4_looplot_fixedANCprev.rds'
)
plot_df <- readRDS('df_4_looplot_fixedANCprev.rds')
# 4) Finally, plot observed vs. predicted with 95% CIs:
obs_v_pred_allages <- ggplot(plot_df, aes(x = observed, y = predicted)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  scale_x_continuous(limits=c(0,0.3),expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.3),expand=c(0,0))+
  labs(
    x = "Observed under 5 malaria prevalence",
    y = "Predicted under 5 malaria prevalence",
    title = "Under and Over 20 year old ANC1 model performance"
  )
ggsave(plot=obs_v_pred_allages,filename = 'obs_v_pred_plot_fixedANCprev.png',width=5,height=5,units = 'in')


###all age Fixed ANC prevalence
#----------------------------------------------------------
# 1. Define the BUGS model
#----------------------------------------------------------
preg_child_predictive_model <- function(){
  for (i in 1:N) {
    # 1) CHILDREN UNDER 5:
    pos_child[i] ~ dbin(p_child[i], total_child[i])

    # 2) “STABILISED” PREGNANT WOMEN PROPORTIONS (to avoid exactly 0 or 1):
    p_preg[i] <- (pos_preg[i] + 0.5) / (total_preg[i] + 1)

    # 3) LOGIT‐TRANSFORM THOSE:
    logit_preg[i] <- logit(p_preg[i])

    # 4) LINEAR PREDICTOR FOR CHILD:
    logit(p_child[i]) <- alpha +
      beta * logit_preg[i] +
      RE[i]

    # 5) RANDOM EFFECT:
    RE[i] ~ dnorm(0, tau_RE)
  }

  # PRIORS:
  alpha   ~ dnorm(0, 0.001)
  beta ~ dnorm(0, 0.001)
  tau_RE  ~ dgamma(0.001, 0.001)
  sigma_RE <- 1 / sqrt(tau_RE)
}

#----------------------------------------------------------
# 2. Write model to file
#----------------------------------------------------------
model.file <- "model_predictive.txt"
write.model(preg_child_predictive_model, model.file)

#----------------------------------------------------------
# 3. Prepare data
#----------------------------------------------------------
N_sites <- nrow(dhs_anc_merged)

data_list <- list(
  pos_child     = as.integer(dhs_anc_merged$positive),
  total_child   = as.integer(dhs_anc_merged$tested),
  pos_preg   = as.integer(dhs_anc_merged$ANC_pos),
  total_preg = as.integer(dhs_anc_merged$ANC_test),
  N             = N_sites
)

#----------------------------------------------------------
# 4. Initial values
#----------------------------------------------------------
inits <- function() {
  list(
    alpha   = rnorm(1, 0, 1),
    beta = rnorm(1, 0, 1),
    tau_RE  = rgamma(1, 1, 1)
  )
}


#----------------------------------------------------------
# 5. Parameters to monitor
#----------------------------------------------------------
params <- c("alpha", "beta", "sigma_RE", "p_child")

#----------------------------------------------------------
# 6. Run full model
#----------------------------------------------------------
run_full <- bugs(
  data            = data_list,
  inits           = inits,
  parameters.to.save = params,
  model.file      = "model_predictive.txt",
  n.chains        = 3,
  n.iter          = 50000,
  n.burnin        = 10000,
  n.thin          = 10,
  debug           = TRUE,
  DIC             = TRUE,
  working.directory = getwd()
)
saveRDS(run_full,file = 'run_full_allagesfixedANCprev.rds')
#----------------------------------------------------------
# 7. Compute LOOIC
#----------------------------------------------------------
# 1) Pull out all the saved p_child[1: N_sites] from all chains:
p_child_draws <- run_full$sims.list$p_child
#   – this is an array (n_draws_total × N_sites).
#   n_draws_total = (#chains)*(#iterations saved per chain).

# 2) Create a matrix (n_draws_total × N_sites) of log‐likelihoods:
y_obs <- data_list$pos_child
n_obs <- data_list$total_child

# A little helper: for each site i, we take all draws of p_child_draws[ , i ],
# then compute dbinom( y_obs[i], size = n_obs[i], prob = that_draw, log=TRUE ).

log_lik_mat <- matrix(NA,
                      nrow = nrow(p_child_draws),
                      ncol = N_sites)

for (i in seq_len(N_sites)) {
  log_lik_mat[, i] <- dbinom(
    x    = y_obs[i],
    size = n_obs[i],
    prob = p_child_draws[, i],
    log  = TRUE
  )
}

# 3) Now feed that to loo::loo():
loo_result <- loo(log_lik_mat)
saveRDS(loo_result,file='loo_result_fixedANCprev.rds')
print(loo_result)


#----------------------------------------------------------
# 8. Leave-One-Out Cross Validation for Plotting
#----------------------------------------------------------
# 1) Preallocate vectors to hold each site’s held‐out posterior summaries:
predicted_means  <- numeric(N_sites)
predicted_lowers <- numeric(N_sites)
predicted_uppers <- numeric(N_sites)
observed_props   <- y_obs / n_obs    # “observed” child prevalence at each site

# 2) Loop over sites:
for (i in seq_len(N_sites)) {
  cat("Leaving out site", i, "of", N_sites, "...\n")

  # 2a) Which rows remain?
  loo_idx <- setdiff(seq_len(N_sites), i)
  n_loo   <- length(loo_idx)

  # 2b) Build the LOO data list:
  loo_data <- list(
    pos_child     = as.integer(dhs_anc_merged$positive[loo_idx]),
    total_child   = as.integer(dhs_anc_merged$tested[loo_idx]),
    pos_preg   = as.integer(dhs_anc_merged$ANC_pos[loo_idx]),
    total_preg = as.integer(dhs_anc_merged$ANC_test[loo_idx]),
    N             = n_loo
  )

  # 2c) Run BUGS on the “leave‐i‐out” data:
  loo_inits <- function() {
    list(
      alpha   = rnorm(1, 0, 1),
      beta = rnorm(1, 0, 1),
      tau_RE  = rgamma(1, 1, 1)
    )
  }

  loo_fit <- bugs(
    data               = loo_data,
    inits              = loo_inits,
    parameters.to.save = c("alpha", "beta"),
    model.file         = "model_predictive.txt",
    n.chains           = 2,
    n.iter             = 10000,
    n.burnin           = 2000,
    n.thin             = 5,
    DIC                = FALSE,
    debug              = FALSE,
    working.directory  = getwd()
  )

  # 2d) Extract posterior draws for alpha, beta_pg, beta_mg:
  alpha_samps  <- loo_fit$sims.list$alpha
  beta_samps <- loo_fit$sims.list$beta

  # 2e) Compute the “observed” pregnant‐woman proportions at site i (stabilized):
  preg_obs <- (dhs_anc_merged$ANC_pos[i] + 0.5) / (dhs_anc_merged$ANC_test[i] + 1)
  logit_preg_i <- qlogis(preg_obs)

  # 2f) For each posterior draw, predict log‐odds → p_child at held‐out site:
  #     logit(p_pred) = alpha + beta_pg*logit_pg_i + beta_mg*logit_mg_i.
  n_draws <- length(alpha_samps)
  log_odds_pred_draws <- alpha_samps +
    beta_samps * logit_preg_i
  p_pred_draws <- plogis(log_odds_pred_draws)

  # 2g) Summarize:
  predicted_means[i]  <- mean(p_pred_draws)
  predicted_lowers[i] <- quantile(p_pred_draws, 0.025)
  predicted_uppers[i] <- quantile(p_pred_draws, 0.975)
}

# 3) Build a data‐frame for plotting:
plot_df <- data.frame(
  site      = seq_len(N_sites),
  observed  = observed_props,
  predicted = predicted_means,
  lower     = predicted_lowers,
  upper     = predicted_uppers
)
saveRDS(plot_df,file='df_4_looplot_allagesfixedANCprev.rds')
plot_df <- readRDS('df_4_looplot_allagesfixedANCprev.rds')
# 4) Finally, plot observed vs. predicted with 95% CIs:
obs_v_pred <- ggplot(plot_df, aes(x = observed, y = predicted)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  scale_x_continuous(limits=c(0,0.3),expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.3),expand=c(0,0))+
  labs(
    x = "Observed under 5 malaria prevalence",
    y = "Predicted under 5 malaria prevalence",
    title = "All ages ANC1 model performance"
  )
ggsave(plot=obs_v_pred,filename = 'obs_v_pred_plot_allagesfixedANCprev.png',width=5,height=5,units = 'in')
loo_composit <- obs_v_pred + obs_v_pred_allages + plot_layout(ncol=2)
ggsave(plot=loo_composit,filename = 'obs_v_pred_plot_bothfixedANCprev.png',width=10,height=5,units = 'in')

###Fixed ANC prevalence with interaction
#----------------------------------------------------------
# 1. Define the BUGS model
#----------------------------------------------------------
preg_child_predictive_model <- function(){
  for (i in 1:N) {
    # 1) CHILDREN UNDER 5:
    pos_child[i] ~ dbin(p_child[i], total_child[i])

    # 2) “STABILISED” PREGNANT WOMEN PROPORTIONS (to avoid exactly 0 or 1):
    p_preg_pg[i] <- (pos_preg_pg[i] + 0.5) / (total_preg_pg[i] + 1)
    p_preg_mg[i] <- (pos_preg_mg[i] + 0.5) / (total_preg_mg[i] + 1)

    # 3) LOGIT‐TRANSFORM THOSE:
    logit_pg[i] <- logit(p_preg_pg[i])
    logit_mg[i] <- logit(p_preg_mg[i])

    # 4) LINEAR PREDICTOR FOR CHILD:
    logit(p_child[i]) <- alpha +
      beta_pg * logit_pg[i] +
      beta_mg * logit_mg[i] +
      beta_int * logit_pg[i] * logit_mg[i] +
      RE[i]

    # 5) RANDOM EFFECT:
    RE[i] ~ dnorm(0, tau_RE)
  }

  # PRIORS:
  alpha   ~ dnorm(0, 0.001)
  beta_pg ~ dnorm(0, 0.001)
  beta_mg ~ dnorm(0, 0.001)
  beta_int ~ dnorm(0, 0.001)
  tau_RE  ~ dgamma(0.001, 0.001)
  sigma_RE <- 1 / sqrt(tau_RE)
}

#----------------------------------------------------------
# 2. Write model to file
#----------------------------------------------------------
model.file <- "model_predictive.txt"
write.model(preg_child_predictive_model, model.file)

#----------------------------------------------------------
# 3. Prepare data
#----------------------------------------------------------
N_sites <- nrow(dhs_anc_merged)

data_list <- list(
  pos_child     = as.integer(dhs_anc_merged$positive),
  total_child   = as.integer(dhs_anc_merged$tested),
  pos_preg_pg   = as.integer(dhs_anc_merged$ANC_pos_lt20),
  total_preg_pg = as.integer(dhs_anc_merged$ANC_test_lt20),
  pos_preg_mg   = as.integer(dhs_anc_merged$ANC_pos_ge20),
  total_preg_mg = as.integer(dhs_anc_merged$ANC_test_ge20),
  N             = N_sites
)

#----------------------------------------------------------
# 4. Initial values
#----------------------------------------------------------
inits <- function() {
  list(
    alpha   = rnorm(1, 0, 1),
    beta_pg = rnorm(1, 0, 1),
    beta_mg = rnorm(1, 0, 1),
    beta_int = rnorm(1, 0, 1),
    tau_RE  = rgamma(1, 1, 1)
  )
}


#----------------------------------------------------------
# 5. Parameters to monitor
#----------------------------------------------------------
params <- c("alpha", "beta_pg", "beta_mg","beta_int", "sigma_RE", "p_child")

#----------------------------------------------------------
# 6. Run full model
#----------------------------------------------------------
run_full <- bugs(
  data            = data_list,
  inits           = inits,
  parameters.to.save = params,
  model.file      = "model_predictive.txt",
  n.chains        = 3,
  n.iter          = 50000,
  n.burnin        = 10000,
  n.thin          = 10,
  debug           = TRUE,
  DIC             = TRUE,
  working.directory = getwd()
)
saveRDS(run_full,file = 'run_full_interactionfixedANCprev.rds')
#----------------------------------------------------------
# 7. Compute LOOIC
#----------------------------------------------------------
# 1) Pull out all the saved p_child[1: N_sites] from all chains:
p_child_draws <- run_full$sims.list$p_child
#   – this is an array (n_draws_total × N_sites).
#   n_draws_total = (#chains)*(#iterations saved per chain).

# 2) Create a matrix (n_draws_total × N_sites) of log‐likelihoods:
y_obs <- data_list$pos_child
n_obs <- data_list$total_child

# A little helper: for each site i, we take all draws of p_child_draws[ , i ],
# then compute dbinom( y_obs[i], size = n_obs[i], prob = that_draw, log=TRUE ).

log_lik_mat <- matrix(NA,
                      nrow = nrow(p_child_draws),
                      ncol = N_sites)

for (i in seq_len(N_sites)) {
  log_lik_mat[, i] <- dbinom(
    x    = y_obs[i],
    size = n_obs[i],
    prob = p_child_draws[, i],
    log  = TRUE
  )
}

# 3) Now feed that to loo::loo():
loo_result <- loo(log_lik_mat)
saveRDS(loo_result,file='loo_result_fixedANCprev.rds')
print(loo_result)


#----------------------------------------------------------
# 8. Leave-One-Out Cross Validation for Plotting
#----------------------------------------------------------
# 1) Preallocate vectors to hold each site’s held‐out posterior summaries:
predicted_means  <- numeric(N_sites)
predicted_lowers <- numeric(N_sites)
predicted_uppers <- numeric(N_sites)
observed_props   <- y_obs / n_obs    # “observed” child prevalence at each site

# 2) Loop over sites:
for (i in seq_len(N_sites)) {
  cat("Leaving out site", i, "of", N_sites, "...\n")

  # 2a) Which rows remain?
  loo_idx <- setdiff(seq_len(N_sites), i)
  n_loo   <- length(loo_idx)

  # 2b) Build the LOO data list:
  loo_data <- list(
    pos_child     = as.integer(dhs_anc_merged$positive[loo_idx]),
    total_child   = as.integer(dhs_anc_merged$tested[loo_idx]),
    pos_preg_pg   = as.integer(dhs_anc_merged$ANC_pos_lt20[loo_idx]),
    total_preg_pg = as.integer(dhs_anc_merged$ANC_test_lt20[loo_idx]),
    pos_preg_mg   = as.integer(dhs_anc_merged$ANC_pos_ge20[loo_idx]),
    total_preg_mg = as.integer(dhs_anc_merged$ANC_test_ge20[loo_idx]),
    N             = n_loo
  )

  # 2c) Run BUGS on the “leave‐i‐out” data:
  loo_inits <- function() {
    list(
      alpha   = rnorm(1, 0, 1),
      beta_pg = rnorm(1, 0, 1),
      beta_mg = rnorm(1, 0, 1),
      beta_int = rnorm(1, 0, 1),
      tau_RE  = rgamma(1, 1, 1)
    )
  }

  loo_fit <- bugs(
    data               = loo_data,
    inits              = loo_inits,
    parameters.to.save = c("alpha", "beta_pg", "beta_mg","beta_int"),
    model.file         = "model_predictive.txt",
    n.chains           = 2,
    n.iter             = 10000,
    n.burnin           = 2000,
    n.thin             = 5,
    DIC                = FALSE,
    debug              = FALSE,
    working.directory  = getwd()
  )

  # 2d) Extract posterior draws for alpha, beta_pg, beta_mg:
  alpha_samps  <- loo_fit$sims.list$alpha
  beta_pg_samps <- loo_fit$sims.list$beta_pg
  beta_mg_samps <- loo_fit$sims.list$beta_mg
  beta_int_samps <- loo_fit$sims.list$beta_int

  # 2e) Compute the “observed” pregnant‐woman proportions at site i (stabilized):
  pg_obs <- (dhs_anc_merged$ANC_pos_lt20[i] + 0.5) / (dhs_anc_merged$ANC_test_lt20[i] + 1)
  mg_obs <- (dhs_anc_merged$ANC_pos_ge20[i] + 0.5) / (dhs_anc_merged$ANC_test_ge20[i] + 1)
  logit_pg_i <- qlogis(pg_obs)
  logit_mg_i <- qlogis(mg_obs)

  # 2f) For each posterior draw, predict log‐odds → p_child at held‐out site:
  #     logit(p_pred) = alpha + beta_pg*logit_pg_i + beta_mg*logit_mg_i.
  n_draws <- length(alpha_samps)
  log_odds_pred_draws <- alpha_samps +
    beta_pg_samps * logit_pg_i +
    beta_mg_samps * logit_mg_i +
    beta_int_samps * logit_pg_i * logit_mg_i
  p_pred_draws <- plogis(log_odds_pred_draws)

  # 2g) Summarize:
  predicted_means[i]  <- mean(p_pred_draws)
  predicted_lowers[i] <- quantile(p_pred_draws, 0.025)
  predicted_uppers[i] <- quantile(p_pred_draws, 0.975)
}

# 3) Build a data‐frame for plotting:
plot_df <- data.frame(
  site      = seq_len(N_sites),
  observed  = observed_props,
  predicted = predicted_means,
  lower     = predicted_lowers,
  upper     = predicted_uppers
)
saveRDS(plot_df,file='df_4_looplot_interactionfixedANCprev.rds'
)
plot_df <- readRDS('df_4_looplot_interactionfixedANCprev.rds')
# 4) Finally, plot observed vs. predicted with 95% CIs:
obs_v_pred_interaction <- ggplot(plot_df, aes(x = observed, y = predicted)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  scale_x_continuous(limits=c(0,0.3),expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.3),expand=c(0,0))+
  labs(
    x = "Observed under 5 malaria prevalence",
    y = "Predicted under 5 malaria prevalence",
    title = "Under and Over 20 year old with interaction ANC1 model performance"
  )
ggsave(plot=obs_v_pred_interaction,filename = 'obs_v_pred_plot_interactionfixedANCprev.png',width=5,height=5,units = 'in')
loo_composit <- obs_v_pred + obs_v_pred_allages +obs_v_pred_interaction + plot_layout(ncol=3)
ggsave(plot=loo_composit,filename = 'obs_v_pred_plot_allfixedANCprev.png',width=15,height=5,units = 'in')

###<20 only Fixed ANC prevalence
#----------------------------------------------------------
# 1. Define the BUGS model
#----------------------------------------------------------
preg_child_predictive_model <- function(){
  for (i in 1:N) {
    # 1) CHILDREN UNDER 5:
    pos_child[i] ~ dbin(p_child[i], total_child[i])

    # 2) “STABILISED” PREGNANT WOMEN PROPORTIONS (to avoid exactly 0 or 1):
    p_preg[i] <- (pos_preg[i] + 0.5) / (total_preg[i] + 1)

    # 3) LOGIT‐TRANSFORM THOSE:
    logit_preg[i] <- logit(p_preg[i])

    # 4) LINEAR PREDICTOR FOR CHILD:
    logit(p_child[i]) <- alpha +
      beta * logit_preg[i] +
      RE[i]

    # 5) RANDOM EFFECT:
    RE[i] ~ dnorm(0, tau_RE)
  }

  # PRIORS:
  alpha   ~ dnorm(0, 0.001)
  beta ~ dnorm(0, 0.001)
  tau_RE  ~ dgamma(0.001, 0.001)
  sigma_RE <- 1 / sqrt(tau_RE)
}

#----------------------------------------------------------
# 2. Write model to file
#----------------------------------------------------------
model.file <- "model_predictive.txt"
write.model(preg_child_predictive_model, model.file)

#----------------------------------------------------------
# 3. Prepare data
#----------------------------------------------------------
N_sites <- nrow(dhs_anc_merged)

data_list <- list(
  pos_child     = as.integer(dhs_anc_merged$positive),
  total_child   = as.integer(dhs_anc_merged$tested),
  pos_preg   = as.integer(dhs_anc_merged$ANC_pos_lt20),
  total_preg = as.integer(dhs_anc_merged$ANC_test_lt20),
  N             = N_sites
)

#----------------------------------------------------------
# 4. Initial values
#----------------------------------------------------------
inits <- function() {
  list(
    alpha   = rnorm(1, 0, 1),
    beta = rnorm(1, 0, 1),
    tau_RE  = rgamma(1, 1, 1)
  )
}


#----------------------------------------------------------
# 5. Parameters to monitor
#----------------------------------------------------------
params <- c("alpha", "beta", "sigma_RE", "p_child")

#----------------------------------------------------------
# 6. Run full model
#----------------------------------------------------------
run_full <- bugs(
  data            = data_list,
  inits           = inits,
  parameters.to.save = params,
  model.file      = "model_predictive.txt",
  n.chains        = 3,
  n.iter          = 50000,
  n.burnin        = 10000,
  n.thin          = 10,
  debug           = TRUE,
  DIC             = TRUE,
  working.directory = getwd()
)
saveRDS(run_full,file = 'run_full_lt20fixedANCprev.rds')
#----------------------------------------------------------
# 7. Compute LOOIC
#----------------------------------------------------------
# 1) Pull out all the saved p_child[1: N_sites] from all chains:
p_child_draws <- run_full$sims.list$p_child
#   – this is an array (n_draws_total × N_sites).
#   n_draws_total = (#chains)*(#iterations saved per chain).

# 2) Create a matrix (n_draws_total × N_sites) of log‐likelihoods:
y_obs <- data_list$pos_child
n_obs <- data_list$total_child

# A little helper: for each site i, we take all draws of p_child_draws[ , i ],
# then compute dbinom( y_obs[i], size = n_obs[i], prob = that_draw, log=TRUE ).

log_lik_mat <- matrix(NA,
                      nrow = nrow(p_child_draws),
                      ncol = N_sites)

for (i in seq_len(N_sites)) {
  log_lik_mat[, i] <- dbinom(
    x    = y_obs[i],
    size = n_obs[i],
    prob = p_child_draws[, i],
    log  = TRUE
  )
}

# 3) Now feed that to loo::loo():
loo_result <- loo(log_lik_mat)
saveRDS(loo_result,file='loo_result_fixedANCprev.rds')
print(loo_result)


#----------------------------------------------------------
# 8. Leave-One-Out Cross Validation for Plotting
#----------------------------------------------------------
# 1) Preallocate vectors to hold each site’s held‐out posterior summaries:
predicted_means  <- numeric(N_sites)
predicted_lowers <- numeric(N_sites)
predicted_uppers <- numeric(N_sites)
observed_props   <- y_obs / n_obs    # “observed” child prevalence at each site

# 2) Loop over sites:
for (i in seq_len(N_sites)) {
  cat("Leaving out site", i, "of", N_sites, "...\n")

  # 2a) Which rows remain?
  loo_idx <- setdiff(seq_len(N_sites), i)
  n_loo   <- length(loo_idx)

  # 2b) Build the LOO data list:
  loo_data <- list(
    pos_child     = as.integer(dhs_anc_merged$positive[loo_idx]),
    total_child   = as.integer(dhs_anc_merged$tested[loo_idx]),
    pos_preg   = as.integer(dhs_anc_merged$ANC_pos_lt20[loo_idx]),
    total_preg = as.integer(dhs_anc_merged$ANC_test_lt20[loo_idx]),
    N             = n_loo
  )

  # 2c) Run BUGS on the “leave‐i‐out” data:
  loo_inits <- function() {
    list(
      alpha   = rnorm(1, 0, 1),
      beta = rnorm(1, 0, 1),
      tau_RE  = rgamma(1, 1, 1)
    )
  }

  loo_fit <- bugs(
    data               = loo_data,
    inits              = loo_inits,
    parameters.to.save = c("alpha", "beta"),
    model.file         = "model_predictive.txt",
    n.chains           = 2,
    n.iter             = 10000,
    n.burnin           = 2000,
    n.thin             = 5,
    DIC                = FALSE,
    debug              = FALSE,
    working.directory  = getwd()
  )

  # 2d) Extract posterior draws for alpha, beta_pg, beta_mg:
  alpha_samps  <- loo_fit$sims.list$alpha
  beta_samps <- loo_fit$sims.list$beta

  # 2e) Compute the “observed” pregnant‐woman proportions at site i (stabilized):
  preg_obs <- (dhs_anc_merged$ANC_pos_lt20[i] + 0.5) / (dhs_anc_merged$ANC_test_lt20[i] + 1)
  logit_preg_i <- qlogis(preg_obs)

  # 2f) For each posterior draw, predict log‐odds → p_child at held‐out site:
  #     logit(p_pred) = alpha + beta_pg*logit_pg_i + beta_mg*logit_mg_i.
  n_draws <- length(alpha_samps)
  log_odds_pred_draws <- alpha_samps +
    beta_samps * logit_preg_i
  p_pred_draws <- plogis(log_odds_pred_draws)

  # 2g) Summarize:
  predicted_means[i]  <- mean(p_pred_draws)
  predicted_lowers[i] <- quantile(p_pred_draws, 0.025)
  predicted_uppers[i] <- quantile(p_pred_draws, 0.975)
}

# 3) Build a data‐frame for plotting:
plot_df <- data.frame(
  site      = seq_len(N_sites),
  observed  = observed_props,
  predicted = predicted_means,
  lower     = predicted_lowers,
  upper     = predicted_uppers
)
saveRDS(plot_df,file='df_4_looplot_lt20fixedANCprev.rds')
plot_df <- readRDS('df_4_looplot_lt20fixedANCprev.rds')
# 4) Finally, plot observed vs. predicted with 95% CIs:
obs_v_pred_lt20 <- ggplot(plot_df, aes(x = observed, y = predicted)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  scale_x_continuous(limits=c(0,0.3),expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.3),expand=c(0,0))+
  labs(
    x = "Observed under 5 malaria prevalence",
    y = "Predicted under 5 malaria prevalence",
    title = "Less than 20 years ANC1 model performance"
  )
ggsave(plot=obs_v_pred_lt20,filename = 'obs_v_pred_plot_lt20fixedANCprev.png',width=5,height=5,units = 'in')
loo_composit <- obs_v_pred_lt20 + obs_v_pred + obs_v_pred_allages + obs_v_pred_interaction + plot_layout(ncol=2)
ggsave(plot=loo_composit,filename = 'obs_v_pred_plot_lt20fixedANCprevcomp.png',width=10,height=10,units = 'in')

###Article method
#----------------------------------------------------------
# 1. Define the BUGS model
#----------------------------------------------------------
preg_child_predictive_model <- function(){
  ##For each site (N)

  ##For no secundigrav
  for (i in 1:N) {
    #Probability distribution of prevalence measures
    pos_child[i] ~ dbin(p_child[i],total_child[i])
    pos_preg_pg[i] ~ dbin(p_preg_pg[i],total_preg_pg[i])
    # pos_preg_mg[i] ~ dbin(p_preg_mg[i],total_preg_mg[i])

    #Log odds conversions from probability
    logit(p_child[i]) <- log_odds_child[i]
    logit(p_preg_pg[i]) <- log_odds_child[i]+log_OR_pp_v_c[i]
    # logit(p_preg_mg[i]) <- logit(p_preg_pg[i])+log_OR_pm_v_pp[i]


    log_odds_child[i]~dnorm(av_lo_child,sigma_c_inv)

    #Primigrav-specific gradient
    log_OR_pp_v_c[i]<-RE_intercept_pg[i]+gradient_pg*(log_odds_child[i]-av_lo_child)
    #Multigrav-specific gradient
    # log_OR_pm_v_pp[i]<-RE_intercept_mg[i]+gradient_mg*(log_odds_child[i]-av_lo_child)

    #Random-effects intercept for each gravidity type
    RE_intercept_pg[i]~dnorm(intercept_pg,sigma_int_inv)
    # RE_intercept_mg[i]~dnorm(intercept_mg,sigma_int_inv)

    ##Add secundi
    #Prior on total number of secundigrav - based on unknown probability of a multigrav woman being a secundigrav
    # total_preg_sg[i] ~ dbin(prob_sg[i],total_preg_mg[i])
    # logit(prob_sg[i]) <- log_odds_sg[i]
    # log_odds_sg[i]~dnorm(av_lo_sg,sigma_sg_inv)

  }
  intercept_pg~dnorm(0,0.001)
  # intercept_mg~dnorm(0,0.001)
  gradient_pg ~ dnorm(0,0.001)
  # gradient_mg ~ dnorm(0,0.001)
  # av_lo_child~dnorm(0,0.001)
  # av_lo_sg~dnorm(0,0.001)
  av_lo_child~dunif(-3.663562,3.663562)
  # av_lo_sg~dunif(-3.663562,3.663562)
  sigma_c_inv~dgamma(0.001,0.001)
  sigma_int_inv~dgamma(0.001,0.001)
  # sigma_sg_inv~dgamma(0.001,0.001)
  sigma_child<-1/sqrt(sigma_c_inv)
  sigma_intercept<-1/sqrt(sigma_int_inv)
  # sigma_sg<-1/sqrt(sigma_sg_inv)
}

##Non-centred
preg_child_predictive_model <- function(){
  ##For each site (N)

  ##For no secundigrav
  for (i in 1:N) {
    #Probability distribution of prevalence measures
    pos_child[i] ~ dbin(p_child[i],total_child[i])
    pos_preg_pg[i] ~ dbin(p_preg_pg[i],total_preg_pg[i])

    #Log odds conversions from probability
    logit(p_child[i]) <- log_odds_child[i]
    logit(p_preg_pg[i]) <- log_odds_child[i]+log_OR_pp_v_c[i]


    # z_child[i]        ~ dnorm(0, 1)
    # log_odds_child[i] <- av_lo_child + z_child[i] * sigma_c
    log_odds_child[i]~dnorm(av_lo_child,sigma_c)

    #Primigrav-specific gradient
    log_OR_pp_v_c[i]<-RE_intercept_pg[i]+gradient_pg*(log_odds_child[i]-av_lo_child)
    #Multigrav-specific gradient

    #Random-effects intercept for each gravidity type
    z_int_pg[i]           ~ dnorm(0, 1)
    RE_intercept_pg[i]    <- intercept_pg + z_int_pg[i] * sigma_int
    # RE_intercept_mg[i]~dnorm(intercept_mg,sigma_int_inv)

  }
  intercept_pg~dnorm(0,0.16)
  gradient_pg ~ dnorm(0,0.16)
  av_lo_child~dunif(-3.663562,3.663562)
  sigma_c ~ dunif(0,2)
  tau_c <- 1/(sigma_c*sigma_c)
  sigma_int ~ dunif(0,5)
  tau_int <- 1/(sigma_int*sigma_int)
}

#----------------------------------------------------------
# 2. Write model to file
#----------------------------------------------------------
model.file <- "model_predictive.txt"
write.model(preg_child_predictive_model, model.file)

#----------------------------------------------------------
# 3. Prepare data
#----------------------------------------------------------
N_sites <- nrow(dhs_anc_merged)

data_list <- list(
  pos_child     = as.integer(dhs_anc_merged$positive),
  total_child   = as.integer(dhs_anc_merged$tested),
  pos_preg_pg   = as.integer(dhs_anc_merged$ANC_pos),
  total_preg_pg = as.integer(dhs_anc_merged$ANC_test),
  N             = N_sites
)

#----------------------------------------------------------
# 4. Initial values
#----------------------------------------------------------
inits <- function() {
  list(
    av_lo_child=rnorm(1),
    intercept_pg=rnorm(1),
    gradient_pg=rnorm(1),
    sigma_c=runif(1),
    sigma_int=runif(1),
    z_int_pg = rnorm(N_sites, 0, 1))
}


#----------------------------------------------------------
# 5. Parameters to monitor
#----------------------------------------------------------
params<-c("av_lo_child",
          "intercept_pg",
          "gradient_pg",
          "sigma_c",
          "sigma_int",
          "z_int_pg",
          "p_child")

#----------------------------------------------------------
# 6. Run full model
#----------------------------------------------------------
run_full <- bugs(
  data            = data_list,
  inits           = inits,
  parameters.to.save = params,
  model.file      = "model_predictive.txt",
  n.chains        = 3,
  # n.iter          = 50000,
  # n.burnin        = 10000,
  n.iter          = 5000,
  n.burnin        = 1000,
  n.thin          = 10,
  debug           = TRUE,
  DIC             = TRUE,
  working.directory = getwd()
)
saveRDS(run_full,file = 'run_full_articlemethod_withVE.rds')
saveRDS(run_full,file = 'run_full_gradientallagesANCprev.rds')
str(run_full)
run_full$summary
run_full_coda <- as.mcmc(run_full)
summary(run_full_coda)
run_full_coda_mainpars <- run_full_coda[1:2,c('av_lo_child','intercept_pg','gradient_pg','sigma_child','sigma_intercept')]
run_full_coda_mainpars <- run_full_coda[[-1]][,c('av_lo_child','intercept_pg')]
plot(run_full_coda_mainpars)
pars_of_int <- c('av_lo_child','intercept_pg','gradient_pg','sigma_c','sigma_int')
acf(as.mcmc(run_full_coda[[1]])[, pars_of_int])
pairs(as.data.frame(as.matrix(run_full_coda[, pars_of_int])))
library(GGally)

run_full_coda_goodchains <- run_full_coda[c(2,3)]
str(run_full_coda_goodchains)
pairs(as.data.frame(as.matrix(run_full_coda_goodchains[, pars_of_int])))

###Stoch ANC prevalence
#----------------------------------------------------------
# 1. Define the BUGS model
#----------------------------------------------------------
preg_child_predictive_model <- function(){
  for (i in 1:N) {
    # CHILDREN:
    pos_child[i]   ~ dbin(p_child[i], total_child[i])

    # PREGNANT WOMEN <20:
    pos_preg_pg[i] ~ dbin(p_preg_pg[i], total_preg_pg[i])
    logit( p_preg_pg[i] ) <- gamma_pg

    # PREGNANT WOMEN ≥20:
    pos_preg_mg[i] ~ dbin(p_preg_mg[i], total_preg_mg[i])
    logit( p_preg_mg[i] ) <- gamma_mg

    # CHILD PREDICTION:
    logit(p_child[i]) <- alpha
    + beta_pg * logit(p_preg_pg[i])
    + beta_mg * logit(p_preg_mg[i])
    + RE[i]
    RE[i] ~ dnorm(0, tau_RE)
  }

  # PRIORS:
  alpha   ~ dnorm(0, 0.001)
  beta_pg ~ dnorm(0, 0.001)
  beta_mg ~ dnorm(0, 0.001)
  gamma_pg ~ dnorm(-2, 1.5)
  gamma_mg ~ dnorm(-2, 1.5)
  tau_RE  ~ dgamma(0.001, 0.001)
  sigma_RE <- 1 / sqrt(tau_RE)
}
logit <- function(p) {log(p/(1-p))}
p_from_logit <- function(log_odds) {exp(log_odds) / (1 + exp(log_odds))}
#----------------------------------------------------------
# 2. Write model to file
#----------------------------------------------------------
model.file <- "model_predictive.txt"
write.model(preg_child_predictive_model, model.file)

#----------------------------------------------------------
# 3. Prepare data
#----------------------------------------------------------
N_sites <- nrow(dhs_anc_merged)

data_list <- list(
  pos_child     = as.integer(dhs_anc_merged$positive),
  total_child   = as.integer(dhs_anc_merged$tested),
  pos_preg_pg   = as.integer(dhs_anc_merged$ANC_pos_lt20),
  total_preg_pg = as.integer(dhs_anc_merged$ANC_test_lt20),
  pos_preg_mg   = as.integer(dhs_anc_merged$ANC_pos_ge20),
  total_preg_mg = as.integer(dhs_anc_merged$ANC_test_ge20),
  N             = N_sites
)

#----------------------------------------------------------
# 4. Initial values
#----------------------------------------------------------
inits <- function() {
  list(
    alpha   = rnorm(1, 0, 1),
    beta_pg = rnorm(1, 0, 1),
    beta_mg = rnorm(1, 0, 1),
    gamma_pg = rnorm(1, 0, 1),
    gamma_mg = rnorm(1, 0, 1),
    tau_RE  = rgamma(1, 1, 1)
  )
}


#----------------------------------------------------------
# 5. Parameters to monitor
#----------------------------------------------------------
params <- c("alpha", "beta_pg", "beta_mg", "gamma_pg", "gamma_mg","sigma_RE", "p_child",'RE')

#----------------------------------------------------------
# 6. Run full model
#----------------------------------------------------------
run_full_stochprev <- bugs(
  data            = data_list,
  inits           = inits,
  parameters.to.save = params,
  model.file      = "model_predictive.txt",
  n.chains        = 3,
  # n.iter          = 50000,
  # n.burnin        = 10000,
  n.iter          = 5000,
  n.burnin        = 1000,
  n.thin          = 10,
  # debug           = TRUE,
  DIC             = TRUE,
  working.directory = getwd()
)

trace_plots <- mcmcplot(run_full_stochprev,parms=c("alpha", "beta_pg", "beta_mg", "gamma_pg", "gamma_mg","sigma_RE"))
save_html(trace_plots,'run_full_stochANCprev_trace.html')
saveRDS(run_full_stochprev,file = 'run_full_stochANCprev.rds')
run_full_stochprev$summary
#----------------------------------------------------------
# 7. Compute LOOIC
#----------------------------------------------------------
# 1) Pull out all the saved p_child[1: N_sites] from all chains:
p_child_draws <- run_full_stochprev$sims.list$p_child
#   – this is an array (n_draws_total × N_sites).
#   n_draws_total = (#chains)*(#iterations saved per chain).

# 2) Create a matrix (n_draws_total × N_sites) of log‐likelihoods:
y_obs <- data_list$pos_child
n_obs <- data_list$total_child

# A little helper: for each site i, we take all draws of p_child_draws[ , i ],
# then compute dbinom( y_obs[i], size = n_obs[i], prob = that_draw, log=TRUE ).

log_lik_mat <- matrix(NA,
                      nrow = nrow(p_child_draws),
                      ncol = N_sites)

for (i in seq_len(N_sites)) {
  log_lik_mat[, i] <- dbinom(
    x    = y_obs[i],
    size = n_obs[i],
    prob = p_child_draws[, i],
    log  = TRUE
  )
}

# 3) Now feed that to loo::loo():
loo_result <- loo(log_lik_mat)
plot(loo_result)
saveRDS(loo_result,file='loo_result_stochANCprev.rds')
print(loo_result)
#kcvpr <- kfold(run_full_stochprev, K=10)
ppc_pit_ecdf(pit=pit(y = y, yrep = posterior_predict(run_full_stochprev)))

# -----------------------------------------------------------------------------
# generate_quantities_bugs:
#   Given one posterior draw of (alpha, beta_pg, beta_mg, gamma_pg, gamma_mg,
#   RE[1:N], sigma_RE) plus the data, compute for each site i:
#     * log_lik[i]:  log ∫ Binomial(pos_child[i]|total_child[i], inv_logit(lp_i))
#                         ⋅ Normal(RE|0, sigma_RE) dRE
#     * y_rep[i]:    posterior‐predictive replicate using the *drawn* RE[i]
#     * y_loorep[i]: LOO style replicate: draw fresh RE* ~ N(0,sigma_RE)
#                   then y ~ Binomial(total_child[i], inv_logit(lp_i + RE*))
# -----------------------------------------------------------------------------
library(matrixStats)

approx_log_marginals <- function(pos_child,
                                 total_child,
                                 mu,        # length‐N
                                 tau_RE,
                                 M = 2000) {
  N <- length(pos_child)
  stopifnot(length(total_child) == N,
            length(mu)           == N)

  # 1) draw M random‐effects
  re_draws <- rnorm(M, 0, tau_RE)                 # length‐M

  # 2) log prior density of each draw
  log_dnorm <- dnorm(re_draws, 0, tau_RE, log = TRUE)  # length‐M

  # 3) replicate mu into an M×N matrix
  mu_mat <- matrix(mu, nrow = M, ncol = N, byrow = TRUE)

  # 4) add the random effects to get the linear‐predictor matrix
  #    each row m is mu + re_draws[m]
  linpred <- mu_mat + re_draws                        # M×N

  # 5) compute probabilities
  p_mat <- plogis(linpred)                            # M×N

  # 6) replicate obs into M×N matrices
  y_mat <- matrix(pos_child,  nrow = M, ncol = N, byrow = TRUE)
  n_mat <- matrix(total_child, nrow = M, ncol = N, byrow = TRUE)

  # 7) compute the binomial log‐likelihood at all M×N entries
  log_binom <- dbinom(y_mat, size = n_mat, prob = p_mat, log = TRUE)  # M×N

  # 8) add the prior‐log to each row of log_binom
  #    since log_dnorm is length M, we sweep it over rows
  log_joint <- log_binom + log_dnorm                   # M×N

  # 9) collapse each column via log‐sum‐exp minus log(M)
  #    so log_marginal[i] = log( (1/M) * sum_m exp(log_joint[m,i]) )
  log_marginal <- colLogSumExps(log_joint) - log(M)    # length‐N

  return(log_marginal)
}




generate_quantities_bugs <- function(alpha,
                                     beta_pg,
                                     beta_mg,
                                     gamma_pg,
                                     gamma_mg,
                                     RE,         # vector of length N: the *posterior* draw of each site RE[i]
                                     tau_RE,
                                     total_child,
                                     total_preg_pg,    # not used below, but you can pass it if needed
                                     total_preg_mg,    # ditto
                                     pos_child) {
  N <- length(pos_child)

  log_lik   <- numeric(N)
  y_rep     <- integer(N)
  y_loorep  <- integer(N)

  for (i in seq_len(N)) {
    # 1) linear predictor for children (log‐odds):
    #    logit(p_child[i]) = alpha + beta_pg*gamma_pg + beta_mg*gamma_mg + RE[i]
    lp_i <- alpha +
      beta_pg * gamma_pg +
      beta_mg * gamma_mg +
      RE[i]

    # 2) posterior‐predictive draw conditional on the *sampled* RE[i]:
    p_child_i <- plogis(lp_i)
    y_rep[i]  <- rbinom(1, size = total_child[i], prob = p_child_i)

    # # 3) marginal log‐likelihood: integrate out RE
    # integrand <- function(re_val) {
    #   p <- plogis(alpha +
    #                 beta_pg * gamma_pg +
    #                 beta_mg * gamma_mg +
    #                 re_val)
    #   dnorm(re_val, mean = 0, sd = sigma_RE) *
    #     dbinom(pos_child[i], size = total_child[i], prob = p)
    # }
    # marg <- integrate(integrand, lower = -Inf, upper = +Inf,
    #                   rel.tol = 1e-8, abs.tol = 1e-10)$value
    # log_lik[i] <- log(marg)

    # 1) compute the “mu” for each site
    mu <- alpha +
      beta_pg * gamma_pg +
      beta_mg * gamma_mg

    # 2) approximate all N marginal log‐likelihoods at once:
    log_lik_vec <- approx_log_marginals(
      pos_child   = data_list$pos_child,
      total_child = data_list$total_child,
      mu           = mu,
      tau_RE     = tau_RE,
      M            = 2000    # you can increase/decrease as needed
    )

    # 4) LOO‐style replicate: draw a fresh random effect, then a binomial
    re_star      <- rnorm(1, mean = 0, sd = tau_RE)
    p_loorep_i   <- plogis((alpha +
                              beta_pg * gamma_pg +
                              beta_mg * gamma_mg) +
                             re_star)
    y_loorep[i]  <- rbinom(1, size = total_child[i], prob = p_loorep_i)
  }

  list(
    log_lik  = log_lik_vec,
    y_rep    = y_rep,
    y_loorep = y_loorep
  )
}

#S        <- length(run_full_stochprev$sims.list$alpha)
S=100
#Pre‐allocate lists to store all S draws:
all_log_lik  <- vector("list", S)
all_y_rep    <- vector("list", S)
all_y_loorep <- vector("list", S)

for (s in seq_len(S)) {
  gen <- generate_quantities_bugs(
    alpha         = run_full_stochprev$sims.list$alpha[s],
    beta_pg       = run_full_stochprev$sims.list$beta_pg[s],
    beta_mg       = run_full_stochprev$sims.list$beta_mg[s],
    gamma_pg      = run_full_stochprev$sims.list$gamma_pg[s],
    gamma_mg      = run_full_stochprev$sims.list$gamma_mg[s],
    RE            = run_full_stochprev$sims.list$RE[s, ],   # vector length N
    tau_RE      = 1/(run_full_stochprev$sims.list$sigma_RE[s])^2,
    total_child   = data_list$total_child,
    total_preg_pg = data_list$total_preg_pg,
    total_preg_mg = data_list$total_preg_mg,
    pos_child     = data_list$pos_child
  )
  all_log_lik[[s]]  <- gen$log_lik
  all_y_rep[[s]]    <- gen$y_rep
  all_y_loorep[[s]] <- gen$y_loorep
}
# let N <- length(data_list$pos_child)
# let S <- number of draws
N <- length(data_list$pos_child)
log_lik_matrix <- matrix(NA, nrow = S, ncol = N)

for (s in seq_len(S)) {
  # compute the site‐level linear predictor without the RE:
  mu <- run_full_stochprev$sims.list$alpha[s] +
    run_full_stochprev$sims.list$beta_pg[s] * run_full_stochprev$sims.list$gamma_pg[s] +
    run_full_stochprev$sims.list$beta_mg[s] * run_full_stochprev$sims.list$gamma_mg[s]

  tau_RE <- 1 / (run_full_stochprev$sims.list$sigma_RE[s])^2

  # vector of pos_child and total_child:
  y  <- data_list$pos_child
  n  <- data_list$total_child

  log_lik_matrix[s, ] <- approx_log_marginals(
    pos_child   = y,
    total_child = n,
    mu          = mu,
    tau_RE    = tau_RE,
    M           = 2000
  )
}

# now feed log_lik_matrix to loo():
library(loo)
loo_res <- loo(log_lik_matrix)

# Turn the list of log‐lik vectors into an SxN matrix for loo:
log_lik_mat <- do.call(rbind, all_log_lik)

library(loo)
loo_res <- loo(log_lik_mat)

print(loo_res)

# And you can extract y_rep/y_loorep across draws however you like for posterior‐predictive checks.

#posterior predictive checking
y <- data_list$pos_child/data_list$total_child
dim(p_child_draws)
length(y)
sample <- round(seq.int(from=100,to=12000,length.out = 100 ))
yrep <- p_child_draws[sample,]
str(yrep)
dim(yrep)
pp_check(y,yrep, fun=ppc_dens_overlay)
pp_check(y,yrep, fun=ppc_pit_ecdf)
pp_check(y,yrep, fun=ppc_intervals)
pp_check(y,yrep, fun=ppc_scatter_avg)
ggplot()
prop_zero <- function(y) mean(y == 0)
prop_zero_test1 <- pp_check(y,yrep, plotfun = "stat", stat = "prop_zero")
#----------------------------------------------------------
# 8. Leave-One-Out Cross Validation for Plotting
#----------------------------------------------------------
# 1) Preallocate vectors to hold each site’s held‐out posterior summaries:
predicted_means  <- numeric(N_sites)
predicted_lowers <- numeric(N_sites)
predicted_uppers <- numeric(N_sites)
observed_props   <- y_obs / n_obs    # “observed” child prevalence at each site

# 2) Loop over sites:
for (i in seq_len(N_sites)) {
  cat("Leaving out site", i, "of", N_sites, "...\n")

  # 2a) Which rows remain?
  loo_idx <- setdiff(seq_len(N_sites), i)
  n_loo   <- length(loo_idx)

  # 2b) Build the LOO data list:
  loo_data <- list(
    pos_child     = as.integer(dhs_anc_merged$positive[loo_idx]),
    total_child   = as.integer(dhs_anc_merged$tested[loo_idx]),
    pos_preg_pg   = as.integer(dhs_anc_merged$ANC_pos_lt20[loo_idx]),
    total_preg_pg = as.integer(dhs_anc_merged$ANC_test_lt20[loo_idx]),
    pos_preg_mg   = as.integer(dhs_anc_merged$ANC_pos_ge20[loo_idx]),
    total_preg_mg = as.integer(dhs_anc_merged$ANC_test_ge20[loo_idx]),
    N             = n_loo
  )

  # 2c) Run BUGS on the “leave‐i‐out” data:
  loo_inits <- function() {
    list(
      alpha   = rnorm(1, 0, 1),
      beta_pg = rnorm(1, 0, 1),
      beta_mg = rnorm(1, 0, 1),
      gamma_pg = rnorm(1, 0, 1),
      gamma_mg = rnorm(1, 0, 1),
      tau_RE  = rgamma(1, 1, 1)
    )
  }

  loo_fit <- bugs(
    data               = loo_data,
    inits              = loo_inits,
    parameters.to.save = c("alpha", "beta_pg", "beta_mg","tau_RE"),
    model.file         = "model_predictive.txt",
    n.chains           = 2,
    # n.iter             = 10000,
    # n.burnin           = 2000,
    n.iter             = 1000,
    n.burnin           = 200,
    n.thin             = 5,
    DIC                = FALSE,
    debug              = FALSE,
    working.directory  = getwd()
  )

  # 2d) Extract posterior draws for alpha, beta_pg, beta_mg:
  alpha_samps  <- loo_fit$sims.list$alpha
  beta_pg_samps <- loo_fit$sims.list$beta_pg
  beta_mg_samps <- loo_fit$sims.list$beta_mg
  tau_RE_samps <- loo_fit$sims.list$tau_RE

  # 2e) Compute the “observed” pregnant‐woman proportions at site i (stabilized):
  pg_obs <- (dhs_anc_merged$ANC_pos_lt20[i] + 0.5) / (dhs_anc_merged$ANC_test_lt20[i] + 1)
  mg_obs <- (dhs_anc_merged$ANC_pos_ge20[i] + 0.5) / (dhs_anc_merged$ANC_test_ge20[i] + 1)
  logit_pg_i <- qlogis(pg_obs)
  logit_mg_i <- qlogis(mg_obs)

  # 2f) For each posterior draw, predict log‐odds → p_child at held‐out site:
  #For each draw s, marginally sample a RE* and compute p_pred:
  #     logit(p_pred) = alpha + beta_pg*logit_pg_i + beta_mg*logit_mg_i.
  n_draws <- length(alpha_samps)
  p_preds <- mapply(function(a, bpg, bmg, tRE) {
    mu    <- a + bpg * logit_pg_i + bmg * logit_mg_i
    re_st <- rnorm(1, 0, tRE)
    plogis(mu + re_st)
  },
  a   = alpha_samps,
  bpg = beta_pg_samps,
  bmg = beta_mg_samps,
  tRE = tau_RE_samps)

  # 2g) Summarize:
  predicted_means[i]  <- mean(p_preds)
  predicted_lowers[i] <- quantile(p_preds, 0.025)
  predicted_uppers[i] <- quantile(p_preds, 0.975)
}

# 3) Build a data‐frame for plotting:
plot_df <- data.frame(
  site      = seq_len(N_sites),
  observed  = observed_props,
  predicted = predicted_means,
  lower     = predicted_lowers,
  upper     = predicted_uppers
)
saveRDS(plot_df,file='df_4_looplot_stochANCprev.rds')
# 4) Finally, plot observed vs. predicted with 95% CIs:
obs_v_pred <- ggplot(plot_df, aes(x = observed, y = predicted)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.01) +
  labs(
    x = "Observed child malaria prevalence (p̂_obs)",
    y = "Predicted child prevalence from LOO model (p_pred)",
    title = "LOO‐CV: Predicted vs. Observed child prevalence"
  )
ggsave(plot=obs_v_pred,filename = 'obs_v_pred_plot_fixedANCprev.png',width=5,height=5,units = 'in')

###No age disaggregation ANC prevalence
#----------------------------------------------------------
# 1. Define the BUGS model
#----------------------------------------------------------
preg_child_predictive_model <- function(){
  for (i in 1:N) {
    # CHILDREN:
    pos_child[i]   ~ dbin(p_child[i], total_child[i])

    # All ages pregant women:
    pos_preg[i] ~ dbin(p_preg[i], total_preg[i])
    logit( p_preg[i] ) <- gamma

    # CHILD PREDICTION:
    logit(p_child[i]) <- alpha
    + beta * logit(p_preg[i])
    + RE[i]
    RE[i] ~ dnorm(0, tau_RE)
  }

  # PRIORS:
  alpha   ~ dnorm(0, 0.001)
  beta ~ dnorm(0, 0.001)
  gamma ~ dnorm(-2, 1.5)
  tau_RE  ~ dgamma(0.001, 0.001)
  sigma_RE <- 1 / sqrt(tau_RE)
}

#----------------------------------------------------------
# 2. Write model to file
#----------------------------------------------------------
model.file <- "model_predictive.txt"
write.model(preg_child_predictive_model, model.file)

#----------------------------------------------------------
# 3. Prepare data
#----------------------------------------------------------
N_sites <- nrow(dhs_anc_merged)

data_list <- list(
  pos_child     = as.integer(dhs_anc_merged$positive),
  total_child   = as.integer(dhs_anc_merged$tested),
  pos_preg   = as.integer(dhs_anc_merged$ANC_pos),
  total_preg = as.integer(dhs_anc_merged$ANC_test),
  N             = N_sites
)

#----------------------------------------------------------
# 4. Initial values
#----------------------------------------------------------
inits <- function() {
  list(
    alpha   = rnorm(1, 0, 1),
    beta = rnorm(1, 0, 1),
    gamma = rnorm(1, 0, 1),
    tau_RE  = rgamma(1, 1, 1)
  )
}


#----------------------------------------------------------
# 5. Parameters to monitor
#----------------------------------------------------------
params <- c("alpha", "beta", "gamma","sigma_RE", "p_child")

#----------------------------------------------------------
# 6. Run full model
#----------------------------------------------------------
run_full_allageANC <- bugs(
  data            = data_list,
  inits           = inits,
  parameters.to.save = params,
  model.file      = "model_predictive.txt",
  n.chains        = 3,
  n.iter          = 50000,
  n.burnin        = 10000,
  # n.iter          = 5000,
  # n.burnin        = 1000,
  n.thin          = 10,
  # debug           = TRUE,
  DIC             = TRUE,
  working.directory = getwd()
)
trace_plots <- mcmcplot(run_full_allageANC,parms=c("alpha", "beta", "gamma","sigma_RE"))
save_html(trace_plots,'run_full_allageANC.html')
saveRDS(run_full_allageANC,file = 'run_full_allageANCprev.rds')
#----------------------------------------------------------
# 7. Compute LOOIC
#----------------------------------------------------------
# 1) Pull out all the saved p_child[1: N_sites] from all chains:
p_child_draws <- run_full_allageANC$sims.list$p_child
#   – this is an array (n_draws_total × N_sites).
#   n_draws_total = (#chains)*(#iterations saved per chain).

# 2) Create a matrix (n_draws_total × N_sites) of log‐likelihoods:
y_obs <- data_list$pos_child
n_obs <- data_list$total_child

# A little helper: for each site i, we take all draws of p_child_draws[ , i ],
# then compute dbinom( y_obs[i], size = n_obs[i], prob = that_draw, log=TRUE ).

log_lik_mat <- matrix(NA,
                      nrow = nrow(p_child_draws),
                      ncol = N_sites)

for (i in seq_len(N_sites)) {
  log_lik_mat[, i] <- dbinom(
    x    = y_obs[i],
    size = n_obs[i],
    prob = p_child_draws[, i],
    log  = TRUE
  )
}

# 3) Now feed that to loo::loo():
loo_result <- loo(log_lik_mat)
plot(loo_result)
saveRDS(loo_result,file='loo_result_stochANCprev.rds')
print(loo_result)


#----------------------------------------------------------
# 8. Leave-One-Out Cross Validation for Plotting
#----------------------------------------------------------
# 1) Preallocate vectors to hold each site’s held‐out posterior summaries:
predicted_means  <- numeric(N_sites)
predicted_lowers <- numeric(N_sites)
predicted_uppers <- numeric(N_sites)
observed_props   <- y_obs / n_obs    # “observed” child prevalence at each site

# 2) Loop over sites:
for (i in seq_len(N_sites)) {
  cat("Leaving out site", i, "of", N_sites, "...\n")

  # 2a) Which rows remain?
  loo_idx <- setdiff(seq_len(N_sites), i)
  n_loo   <- length(loo_idx)

  # 2b) Build the LOO data list:
  loo_data <- list(
    pos_child     = as.integer(dhs_anc_merged$positive[loo_idx]),
    total_child   = as.integer(dhs_anc_merged$tested[loo_idx]),
    pos_preg   = as.integer(dhs_anc_merged$ANC_pos[loo_idx]),
    total_preg = as.integer(dhs_anc_merged$ANC_test[loo_idx]),
    N             = n_loo
  )

  # 2c) Run BUGS on the “leave‐i‐out” data:
  loo_inits <- function() {
    list(
      alpha   = rnorm(1, 0, 1),
      beta = rnorm(1, 0, 1),
      gamma = rnorm(1, 0, 1),
      tau_RE  = rgamma(1, 1, 1)
    )
  }

  loo_fit <- bugs(
    data               = loo_data,
    inits              = loo_inits,
    parameters.to.save = c("alpha", "beta", "gamma"),
    model.file         = "model_predictive.txt",
    n.chains           = 2,
    n.iter             = 10000,
    n.burnin           = 2000,
    n.thin             = 5,
    DIC                = FALSE,
    debug              = FALSE,
    working.directory  = getwd()
  )

  # 2d) Extract posterior draws for alpha, beta_pg, beta_mg:
  alpha_samps  <- loo_fit$sims.list$alpha
  beta_samps <- loo_fit$sims.list$beta

  # 2e) Compute the “observed” pregnant‐woman proportions at site i (stabilized):
  anc_obs <- (dhs_anc_merged$ANC_pos[i] + 0.5) / (dhs_anc_merged$ANC_test[i] + 1)
  logit_anc_i <- qlogis(anc_obs)

  # 2f) For each posterior draw, predict log‐odds → p_child at held‐out site:
  #     logit(p_pred) = alpha + beta_pg*logit_pg_i + beta_mg*logit_mg_i.
  n_draws <- length(alpha_samps)
  log_odds_pred_draws <- alpha_samps +
    beta_samps * logit_anc_i
  p_pred_draws <- plogis(log_odds_pred_draws)

  # 2g) Summarize:
  predicted_means[i]  <- mean(p_pred_draws)
  predicted_lowers[i] <- quantile(p_pred_draws, 0.025)
  predicted_uppers[i] <- quantile(p_pred_draws, 0.975)
}

# 3) Build a data‐frame for plotting:
plot_df <- data.frame(
  site      = seq_len(N_sites),
  observed  = observed_props,
  predicted = predicted_means,
  lower     = predicted_lowers,
  upper     = predicted_uppers
)
saveRDS(plot_df,file='df_4_looplot_fixedANCprev.rds')
# 4) Finally, plot observed vs. predicted with 95% CIs:
obs_v_pred <- ggplot(plot_df, aes(x = observed, y = predicted)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.01) +
  labs(
    x = "Observed child malaria prevalence (p̂_obs)",
    y = "Predicted child prevalence from LOO model (p_pred)",
    title = "LOO‐CV: Predicted vs. Observed child prevalence"
  )
ggsave(plot=obs_v_pred,filename = 'obs_v_pred_plot_fixedANCprev.png',width=5,height=5,units = 'in')


###Only <20 ANC prevalence
#----------------------------------------------------------
# 1. Define the BUGS model
#----------------------------------------------------------
preg_child_predictive_model <- function(){
  for (i in 1:N) {
    # CHILDREN:
    pos_child[i]   ~ dbin(p_child[i], total_child[i])

    # PREGNANT WOMEN <20:
    pos_preg_pg[i] ~ dbin(p_preg_pg[i], total_preg_pg[i])
    logit( p_preg_pg[i] ) <- gamma_pg

    # CHILD PREDICTION:
    logit(p_child[i]) <- alpha
    + beta_pg * logit(p_preg_pg[i])
    + RE[i]
    RE[i] ~ dnorm(0, tau_RE)
  }

  # PRIORS:
  alpha   ~ dnorm(0, 0.001)
  beta_pg ~ dnorm(0, 0.001)
  gamma_pg ~ dnorm(-2, 1.5)
  tau_RE  ~ dgamma(0.001, 0.001)
  sigma_RE <- 1 / sqrt(tau_RE)
}

#----------------------------------------------------------
# 2. Write model to file
#----------------------------------------------------------
model.file <- "model_predictive.txt"
write.model(preg_child_predictive_model, model.file)

#----------------------------------------------------------
# 3. Prepare data
#----------------------------------------------------------
N_sites <- nrow(dhs_anc_merged)

data_list <- list(
  pos_child     = as.integer(dhs_anc_merged$positive),
  total_child   = as.integer(dhs_anc_merged$tested),
  pos_preg_pg   = as.integer(dhs_anc_merged$ANC_pos_lt20),
  total_preg_pg = as.integer(dhs_anc_merged$ANC_test_lt20),
  N             = N_sites
)

#----------------------------------------------------------
# 4. Initial values
#----------------------------------------------------------
inits <- function() {
  list(
    alpha   = rnorm(1, 0, 1),
    beta_pg = rnorm(1, 0, 1),
    gamma_pg = rnorm(1, 0, 1),
    tau_RE  = rgamma(1, 1, 1)
  )
}


#----------------------------------------------------------
# 5. Parameters to monitor
#----------------------------------------------------------
params <- c("alpha", "beta_pg", "gamma_pg","sigma_RE", "p_child")

#----------------------------------------------------------
# 6. Run full model
#----------------------------------------------------------
run_full_lt20anc_prev <- bugs(
  data            = data_list,
  inits           = inits,
  parameters.to.save = params,
  model.file      = "model_predictive.txt",
  n.chains        = 3,
  n.iter          = 50000,
  n.burnin        = 10000,
  # n.iter          = 5000,
  # n.burnin        = 1000,
  n.thin          = 10,
  # debug           = TRUE,
  DIC             = TRUE,
  working.directory = getwd()
)
trace_plots <- mcmcplot(run_full_lt20anc_prev,parms=c("alpha", "beta_pg", "gamma_pg","sigma_RE"))
save_html(trace_plots,'run_full_allageANC.html')

saveRDS(run_full_lt20anc_prev,file = 'run_full_lt20anc_prev.rds')
#----------------------------------------------------------
# 7. Compute LOOIC
#----------------------------------------------------------
# 1) Pull out all the saved p_child[1: N_sites] from all chains:
p_child_draws <- run_full_lt20anc_prev$sims.list$p_child
#   – this is an array (n_draws_total × N_sites).
#   n_draws_total = (#chains)*(#iterations saved per chain).

# 2) Create a matrix (n_draws_total × N_sites) of log‐likelihoods:
y_obs <- data_list$pos_child
n_obs <- data_list$total_child

# A little helper: for each site i, we take all draws of p_child_draws[ , i ],
# then compute dbinom( y_obs[i], size = n_obs[i], prob = that_draw, log=TRUE ).

log_lik_mat <- matrix(NA,
                      nrow = nrow(p_child_draws),
                      ncol = N_sites)

for (i in seq_len(N_sites)) {
  log_lik_mat[, i] <- dbinom(
    x    = y_obs[i],
    size = n_obs[i],
    prob = p_child_draws[, i],
    log  = TRUE
  )
}

# 3) Now feed that to loo::loo():
loo_result <- loo(log_lik_mat)
plot(loo_result)
saveRDS(loo_result,file='loo_result_lt20ANCprev.rds')
print(loo_result)


#----------------------------------------------------------
# 8. Leave-One-Out Cross Validation for Plotting
#----------------------------------------------------------
# 1) Preallocate vectors to hold each site’s held‐out posterior summaries:
predicted_means  <- numeric(N_sites)
predicted_lowers <- numeric(N_sites)
predicted_uppers <- numeric(N_sites)
observed_props   <- y_obs / n_obs    # “observed” child prevalence at each site

# 2) Loop over sites:
for (i in seq_len(N_sites)) {
  cat("Leaving out site", i, "of", N_sites, "...\n")

  # 2a) Which rows remain?
  loo_idx <- setdiff(seq_len(N_sites), i)
  n_loo   <- length(loo_idx)

  # 2b) Build the LOO data list:
  loo_data <- list(
    pos_child     = as.integer(dhs_anc_merged$positive[loo_idx]),
    total_child   = as.integer(dhs_anc_merged$tested[loo_idx]),
    pos_preg_pg   = as.integer(dhs_anc_merged$ANC_pos_lt20[loo_idx]),
    total_preg_pg = as.integer(dhs_anc_merged$ANC_test_lt20[loo_idx]),
    N             = n_loo
  )

  # 2c) Run BUGS on the “leave‐i‐out” data:
  loo_inits <- function() {
    list(
      alpha   = rnorm(1, 0, 1),
      beta_pg = rnorm(1, 0, 1),
      tau_RE  = rgamma(1, 1, 1)
    )
  }

  loo_fit <- bugs(
    data               = loo_data,
    inits              = loo_inits,
    parameters.to.save = c("alpha", "beta_pg", "beta_mg"),
    model.file         = "model_predictive.txt",
    n.chains           = 2,
    n.iter             = 10000,
    n.burnin           = 2000,
    n.thin             = 5,
    DIC                = FALSE,
    debug              = FALSE,
    working.directory  = getwd()
  )

  # 2d) Extract posterior draws for alpha, beta_pg, beta_mg:
  alpha_samps  <- loo_fit$sims.list$alpha
  beta_pg_samps <- loo_fit$sims.list$beta_pg

  # 2e) Compute the “observed” pregnant‐woman proportions at site i (stabilized):
  pg_obs <- (dhs_anc_merged$ANC_pos_lt20[i] + 0.5) / (dhs_anc_merged$ANC_test_lt20[i] + 1)
  logit_pg_i <- qlogis(pg_obs)

  # 2f) For each posterior draw, predict log‐odds → p_child at held‐out site:
  #     logit(p_pred) = alpha + beta_pg*logit_pg_i + beta_mg*logit_mg_i.
  n_draws <- length(alpha_samps)
  log_odds_pred_draws <- alpha_samps +
    beta_pg_samps * logit_pg_i
  p_pred_draws <- plogis(log_odds_pred_draws)

  # 2g) Summarize:
  predicted_means[i]  <- mean(p_pred_draws)
  predicted_lowers[i] <- quantile(p_pred_draws, 0.025)
  predicted_uppers[i] <- quantile(p_pred_draws, 0.975)
}

# 3) Build a data‐frame for plotting:
plot_df <- data.frame(
  site      = seq_len(N_sites),
  observed  = observed_props,
  predicted = predicted_means,
  lower     = predicted_lowers,
  upper     = predicted_uppers
)
saveRDS(plot_df,file='df_4_looplot_fixedANCprev.rds')
# 4) Finally, plot observed vs. predicted with 95% CIs:
obs_v_pred <- ggplot(plot_df, aes(x = observed, y = predicted)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.01) +
  labs(
    x = "Observed child malaria prevalence (p̂_obs)",
    y = "Predicted child prevalence from LOO model (p_pred)",
    title = "LOO‐CV: Predicted vs. Observed child prevalence"
  )
ggsave(plot=obs_v_pred,filename = 'obs_v_pred_plot_fixedANCprev.png',width=5,height=5,units = 'in')
