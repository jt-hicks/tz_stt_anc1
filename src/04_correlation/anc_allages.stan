data {
  int<lower=1> N;                 // number of sites
  int<lower=0> pos_child[N];      // malaria‐positive children counts
  int<lower=0> total_child[N];    // number of children tested
  int<lower=0> pos_preg_pg[N];    // malaria‐positive primigrav counts
  int<lower=0> total_preg_pg[N];  // number of primigrav tested
}

parameters {
  real av_lo_child;               // population mean log‐odds of child prevalence
  real intercept_pg;              // population mean log‐OR primigrav vs child
  real gradient_pg;               // slope of log‐OR vs child log‐odds

  real<lower=0> sigma_c;          // sd of child log‐odds random effect
  real<lower=0> sigma_int;        // sd of intercept‐PG random effect

  vector[N] z_child;              // non‑centered child random effects
  vector[N] z_int_pg;             // non‑centered primigrav random intercepts

}

transformed parameters {
  // 1) Build the random intercepts vectorized
  vector[N] RE_int_pg= intercept_pg + sigma_int * z_int_pg;   // primigrav random intercepts

  // 2) Build the child log‑odds vectorized
  vector[N] log_odds_child= av_lo_child + sigma_c   * z_child;      // child log‑odds

  // 3) Compute the two logits
  vector[N] logit_p_child = log_odds_child;       // child logit(p)
  vector[N] logit_p_preg_pg= log_odds_child
                       + RE_int_pg
                       + gradient_pg * (log_odds_child - av_lo_child);     // primigrav logit(p)
  // 4) Inverse‐logit to get probabilities
  vector[N] p_child= inv_logit(logit_p_child);             // child probabilities
  vector[N] p_preg_pg= inv_logit(logit_p_preg_pg);           // primigrav probabilities

}


model {
  // 1) Priors
  av_lo_child  ~ normal(logit(0.15), 1);
  intercept_pg ~ normal(0, 1);
  gradient_pg  ~ normal(0, 1);
  sigma_c      ~ student_t(3, 0, 1) T[0, ];
  sigma_int    ~ student_t(3, 0, 1) T[0, ];

  // 2) Non‑centered random effects
  z_child   ~ normal(0, 1);
  z_int_pg  ~ normal(0, 1);

  // 3) Build all N linear predictors at once
  {
    // 4) Vectorized likelihoods
    target += binomial_logit_lpmf(pos_child   | total_child,   log_odds_child);
    target += binomial_logit_lpmf(pos_preg_pg | total_preg_pg, logit_p_preg_pg);
  }
}


generated quantities {
  // (optional) leave blank or compute derived quantities
}
