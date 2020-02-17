

# https://www.lacountyarts.org/funding/organizational-grant-program/ogp-grantseekers/apply/ogp-grantseekers-calculator-explained

OGP_CATEGORIES <- c(0, 17500, 100000, 1500000, 40000000, Inf)

OGP.2013 <- function(B) {

  category <- cut(B, OGP_CATEGORIES, FALSE)

  alpha <- c(7500, 606, 36250, 84415.58, 300000)
  beta  <- c(0, .3939, .0375, .0054, 0)

  alpha[category] + beta[category] * B
}

OGP.2013_forward <- function(B) {

  categories <- c(0,        1,     7500, 17500, 50000, 100000, 1500000, 10000000, 40000000)
  beta <-       c(7500,     0,        0, .3939, .3939,  .0375,   .0054,    .0054,        0)


  b <- outer(B, categories, `-`); b

  b[] <- pmax(b, 0); b
  b <- sweep(b, MARGIN=2, STATS = c(diff(categories), Inf), FUN=pmin)

  cbind(B, round(b), new=b %*% beta, old=OGP.2013(B))
}


Award <- function(Budget, Grant100Formula, Scores, TotalBudget=4500000, ...) {

  Grant100 <- Grant100Formula(Budget)
  Awards <-  Grant100 * Scores
  TotalBudget * Awards / sum(Awards)
}

