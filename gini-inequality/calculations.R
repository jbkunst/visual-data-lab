# Population Gini; no n / (n - 1) sample correction.
gini <- function(x) {
  stopifnot(length(x) > 0, all(is.finite(x)), all(x >= 0))
  if (max(x) == 0) return(NA_real_)
  x <- sort(x / max(x))
  n <- length(x)
  sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
}

lorenz <- function(x) {
  if (max(x) == 0) return(data.frame(p = numeric(), L = numeric()))
  x <- sort(x / max(x))
  data.frame(p = c(0, seq_along(x) / length(x)), L = c(0, cumsum(x) / sum(x)))
}

# beta is a rate for Gamma; mu/sigma describe log income for Lognormal.
income_distribution <- function(family, par) {
  p <- seq(0, 1, length.out = 301)
  switch(family,
    beta = list(
      gini = exp(log(2 / par$alpha) + lbeta(par$alpha + par$beta, par$alpha + par$beta) -
        lbeta(par$alpha, par$alpha) - lbeta(par$beta, par$beta)),
      mean = par$alpha / (par$alpha + par$beta),
      density = function(x) dbeta(x, par$alpha, par$beta),
      quantile = function(u) qbeta(u, par$alpha, par$beta),
      curve = data.frame(p, L = pbeta(qbeta(p, par$alpha, par$beta), par$alpha + 1, par$beta))
    ),
    lognormal = list(
      gini = 2 * pnorm(par$sigma / sqrt(2)) - 1,
      mean = exp(par$mu + par$sigma^2 / 2),
      density = function(x) dlnorm(x, par$mu, par$sigma),
      quantile = function(u) qlnorm(u, par$mu, par$sigma),
      curve = data.frame(p, L = pnorm(qnorm(p) - par$sigma))
    ),
    gamma = list(
      gini = exp(lgamma(par$alpha + 0.5) - lgamma(par$alpha + 1)) / sqrt(pi),
      mean = par$alpha / par$beta,
      density = function(x) dgamma(x, par$alpha, rate = par$beta),
      quantile = function(u) qgamma(u, par$alpha, rate = par$beta),
      curve = data.frame(p, L = pgamma(qgamma(p, par$alpha), par$alpha + 1))
    ),
    exponential = list(
      gini = 0.5,
      mean = 1 / par$lam,
      density = function(x) dexp(x, rate = par$lam),
      quantile = function(u) qexp(u, rate = par$lam),
      curve = data.frame(p, L = c(p[-length(p)] + (1 - p[-length(p)]) * log1p(-p[-length(p)]), 1))
    )
  )
}

# Equal-probability quantile cells approximate the continuous pair integral.
# This grid is illustrative; the reported Gini never uses this approximation.
quantile_pairs <- function(x) {
  n <- length(x)
  ij <- combn(seq_len(n), 2)
  difference <- abs(x[ij[1, ]] - x[ij[2, ]])
  contribution <- (difference / max(x)) / (n * sum(x / max(x)))
  lapply(seq_along(difference), function(k) list(
    x = 100 * (ij[1, k] - 0.5) / n,
    y = 100 * (ij[2, k] - 0.5) / n,
    group1 = sprintf("%g-%g%%", 100 * (ij[1, k] - 1) / n, 100 * ij[1, k] / n),
    group2 = sprintf("%g-%g%%", 100 * (ij[2, k] - 1) / n, 100 * ij[2, k] / n),
    income1 = x[ij[1, k]], income2 = x[ij[2, k]],
    value = difference[k], contribution = contribution[k]
  ))
}
