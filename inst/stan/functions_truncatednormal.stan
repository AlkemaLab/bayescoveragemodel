// for truncated normals, not used currently
// From the Stan User's Guide 2.28: https://mc-stan.org/docs/2_28/stan-users-guide/truncated-random-number-generation.html.
// Released under CC-BY 4.0 license (https://creativecommons.org/licenses/by/4.0/legalcode)
real normal_lub_cdf(real x, real mu, real sigma, real lb, real ub) {
  real p_x  = normal_cdf(x  | mu, sigma);
  real p_lb = normal_cdf(lb | mu, sigma);
  real p_ub = normal_cdf(ub | mu, sigma);
  return (p_x - p_lb) / p_ub;
}
real normal_lub_rng(real mu, real sigma, real lb, real ub) {
  real p_lb = normal_cdf(lb | mu, sigma);
  real p_ub = normal_cdf(ub | mu, sigma);
  real u = uniform_rng(p_lb, p_ub);
  real y = mu + sigma * inv_Phi(u);
  return y;
}
