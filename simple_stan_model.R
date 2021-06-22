library(rstan)
library(readr)

scode <- "

// generated with brms 2.13.5
functions {
  /* zero-one-inflated beta log-PDF of a single response 
   * Args: 
   *   y: response value 
   *   mu: mean parameter of the beta part
   *   phi: precision parameter of the beta part
   *   zoi: zero-one-inflation probability
   *   coi: conditional one-inflation probability
   * Returns:  
   *   a scalar to be added to the log posterior 
   */ 
   real zero_one_inflated_beta_lpdf(real y, real mu, real phi,
                                    real zoi, real coi) {
     row_vector[2] shape = [mu * phi, (1 - mu) * phi]; 
     if (y == 0) { 
       return bernoulli_lpmf(1 | zoi) + bernoulli_lpmf(0 | coi); 
     } else if (y == 1) {
       return bernoulli_lpmf(1 | zoi) + bernoulli_lpmf(1 | coi);
     } else { 
       return bernoulli_lpmf(0 | zoi) + beta_lpdf(y | shape[1], shape[2]);
     } 
   }
}
data {
  int<lower=1> N;  // number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  vector[N] Z_1_2;
  vector[N] Z_1_3;
  vector[N] Z_1_4;
  int<lower=1> NC_1;  // number of group-level correlations
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> phi;  // precision parameter
  real<lower=0,upper=1> zoi;  // zero-one-inflation probability
  real<lower=0,upper=1> coi;  // conditional one-inflation probability
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  matrix[M_1, N_1] z_1;  // standardized group-level effects
  cholesky_factor_corr[M_1] L_1;  // cholesky factor of correlation matrix
}
transformed parameters {
  matrix[N_1, M_1] r_1;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_1;
  vector[N_1] r_1_2;
  vector[N_1] r_1_3;
  vector[N_1] r_1_4;
  // compute actual group-level effects
  r_1 = (diag_pre_multiply(sd_1, L_1) * z_1)';
  r_1_1 = r_1[, 1];
  r_1_2 = r_1[, 2];
  r_1_3 = r_1[, 3];
  r_1_4 = r_1[, 4];
}
model {
  // initialize linear predictor term
  vector[N] mu = Intercept + Xc * b;
  for (n in 1:N) {
    // add more terms to the linear predictor
    mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_1_2[J_1[n]] * Z_1_2[n] + r_1_3[J_1[n]] * Z_1_3[n] + r_1_4[J_1[n]] * Z_1_4[n];
  }
  for (n in 1:N) {
    // apply the inverse link function
    mu[n] = inv_logit(mu[n]);
  }
  // priors including all constants
  target += student_t_lpdf(Intercept | 3, 0, 2.5);
  target += gamma_lpdf(phi | 0.01, 0.01);
  target += beta_lpdf(zoi | 1, 1);
  target += beta_lpdf(coi | 1, 1);
  target += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 4 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(to_vector(z_1));
  target += lkj_corr_cholesky_lpdf(L_1 | 1);
  // likelihood including all constants
  if (!prior_only) {
    for (n in 1:N) {
      target += zero_one_inflated_beta_lpdf(Y[n] | mu[n], phi, zoi, coi);
    }
  }
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  // compute group-level correlations
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1,upper=1>[NC_1] cor_1;
  // extract upper diagonal of correlation matrix
  for (k in 1:M_1) {
    for (j in 1:(k - 1)) {
      cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
    }
  }
}

"

sdata <- read_rds("standata_simple_dem.rds")

fit <- stan(model_code = scode, 
            data = sdata, 
            chains = 3, 
            iter = 4000, 
            warmup = 1000)

saveRDS(fit, 
        "simple_stan_model.rds")
