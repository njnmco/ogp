

# https://www.lacountyarts.org/funding/organizational-grant-program/ogp-grantseekers/apply/ogp-grantseekers-calculator-explained

OGP_CATEGORIES <- c(0, 17500, 100000, 1500000, 40000000, Inf)

OGP.2013 <- function(B) {

  category <- cut(B, OGP_CATEGORIES, FALSE)

  alpha <- c(7500, 606, 36250, 84415.58, 300000)
  beta  <- c(0, .3939, .0375, .0054, 0)

  alpha[category] + beta[category] * B
}
