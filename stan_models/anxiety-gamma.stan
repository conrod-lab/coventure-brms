functions {
  /* hurdle gamma log-PDF of a single response
   * Args:
   *   y: the response value
   *   alpha: shape parameter of the gamma distribution
   *   beta: rate parameter of the gamma distribution
   *   hu: hurdle probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real hurdle_gamma_lpdf(real y, real alpha, real beta, real hu) {
    if (y == 0) {
      return bernoulli_lpmf(1 | hu);
    } else {
      return bernoulli_lpmf(0 | hu) +
             gamma_lpdf(y | alpha, beta);
    }
  }
  /* hurdle gamma log-PDF of a single response
   * logit parameterization of the hurdle part
   * Args:
   *   y: the response value
   *   alpha: shape parameter of the gamma distribution
   *   beta: rate parameter of the gamma distribution
   *   hu: linear predictor for the hurdle part
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real hurdle_gamma_logit_lpdf(real y, real alpha, real beta, real hu) {
    if (y == 0) {
      return bernoulli_logit_lpmf(1 | hu);
    } else {
      return bernoulli_logit_lpmf(0 | hu) +
             gamma_lpdf(y | alpha, beta);
    }
  }
  // hurdle gamma log-CCDF and log-CDF functions
  real hurdle_gamma_lccdf(real y, real alpha, real beta, real hu) {
    return bernoulli_lpmf(0 | hu) + gamma_lccdf(y | alpha, beta);
  }
  real hurdle_gamma_lcdf(real y, real alpha, real beta, real hu) {
    return log1m_exp(hurdle_gamma_lccdf(y | alpha, beta, hu));
  }
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N] int<lower=1> J_1;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  array[N] int<lower=1> J_2;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // regression coefficients
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> shape;  // shape parameter
  real<lower=0,upper=1> hu;  // hurdle probability
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  array[M_1] vector[N_1] z_1;  // standardized group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  array[M_2] vector[N_2] z_2;  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_1;  // actual group-level effects
  vector[N_2] r_2_1;  // actual group-level effects
  real lprior = 0;  // prior contributions to the log posterior
  r_1_1 = (sd_1[1] * (z_1[1]));
  r_2_1 = (sd_2[1] * (z_2[1]));
  lprior += student_t_lpdf(Intercept | 3, 0.7, 2.5);
  lprior += gamma_lpdf(shape | 0.01, 0.01);
  lprior += beta_lpdf(hu | 1, 1);
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_2 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept + Xc * b;
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n];
    }
    mu = exp(mu);
    for (n in 1:N) {
      target += hurdle_gamma_lpdf(Y[n] | shape, shape / mu[n], hu);
    }
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
  target += std_normal_lpdf(z_2[1]);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}