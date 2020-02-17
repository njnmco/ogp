

# https://www.lacountyarts.org/funding/organizational-grant-program/ogp-grantseekers/apply/ogp-grantseekers-calculator-explained

OGP_CATEGORIES <- c(0, 17500, 100000, 1500000, 40000000, Inf)

OGP.2013 <- function(B) {

  Vectorize(function(budgetSize){
    if(budgetSize < 17500) {
      grant = 7500;
    }
    else if (budgetSize >= 17500 && budgetSize <= 100000) {
      grant = 606.0606 + 0.3939394 * budgetSize;
    }
    else if (budgetSize >= 100000 && budgetSize <= 1500000) {
      grant = 36250 + 0.0375 * budgetSize;
    }
    else if (budgetSize >= 1500000 && budgetSize <= 40000000) {
      grant = 84415.5844 + 0.0053896 * budgetSize;
    }
    else {
      grant = 300000;
    }

    grant
  })(B)
  #category <- cut(B, OGP_CATEGORIES, FALSE)

  #alpha <- c(7500, 606, 36250, 84415.58, 300000)
  #beta  <- c(0, .3939, .0375, .0054, 0)

  #alpha[category] + beta[category] * B
}

OGP.2013_forward <- function(B) {

  c1 <- 0.3939394
  c2 <- 0.0053896
  categories <- c(0,        1,     7500, 17500, 50000, 100000, 1500000, 10000000, 40000000)
  beta <-       c(7500,     0,        0,    c1,    c1,  .0375,      c2,       c2,        0)


  b <- outer(B, categories, `-`); b

  b[] <- pmax(b, 0); b
  b <- sweep(b, MARGIN=2, STATS = c(diff(categories), Inf), FUN=pmin)

  drop(b %*% beta)
}


Award <- function(Budget, Grant100Formula, Scores, TotalBudget=4500000, ...) {

  Grant100 <- Grant100Formula(Budget)
  Awards   <-  Grant100 * Scores
  mod      <- TotalBudget / sum(Awards)

  Final <- Awards * mod

  structure(data.frame(..., Grant100, Awards, Final), mod=mod)
}

