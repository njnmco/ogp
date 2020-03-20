

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

basis <- function(B, categories=NULL) {

  if(is.null(categories)) {
    categories <-   c(0,        1,     7500, 17500, 50000, 100000, 1500000, 10000000, 40000000)
  }
  b <- outer(B, categories, `-`); b
  b[] <- pmax(b, 0); b
  b <- sweep(b, MARGIN=2, STATS = c(diff(categories), Inf), FUN=pmin)

  b

}

OGP.2013_forward <- function(B, beta=NULL) {

  if(is.null(beta)) {
    c1 <- 0.3939394
    c2 <- 0.0053896
    categories <-   c(0,        1,     7500, 17500, 50000, 100000, 1500000, 10000000, 40000000)
    beta       <-   c(7500,     0,        0,    c1,    c1,  .0375,      c2,       c2,        0)
  }

  drop(basis(B) %*% beta)
}


Award <- function(Budget, Grant100Formula, Scores, TotalBudget=4500000, Year=1, ...) {

  Grant100 <- Grant100Formula(Budget)
  Awards   <-  Grant100 * Scores
  mod      <- TotalBudget / ave(Awards, Year, FUN=sum)

  Final <- Awards * mod

  structure(data.frame(..., Grant100, Awards, Final), mod=mod)
}

make_group_constraint <- function(B, group, b_0=B, dropZeros=TRUE) {

  g <- fac2sparse(as.factor(group))
  Amat <- g %*% basis(B)
  b_0 <- g %*% b_0

  if(dropZeros) {
    i <- Amat[,2] > 0
    Amat <- Amat[i,]
    b_0 <- b_0[i]
  }

  list(Amat=Amat, b_0=b_0)
}


make_coef_constraints <- function(k) {

  Amat_beta_gt_0 <- diag(k)[-1,]
  b_0_beta_gt_0  <- rep(0, k)[-1]

  Amat_beta_lt_50p <- -diag(k)[-1,]
  b_0_beta_lt_50p  <- -rep(.5, k)[-1]


  return(list(
    Amat=rbind(Amat_beta_gt_0, Amat_beta_lt_50p),
    b_0 =c(b_0_beta_gt_0, b_0_beta_lt_50p)
    ))

}

scenario <- function(data,
                       B = data$Budget_Size,
                       Y = data$Current.Final,
                       Y_low = .95 * data$Current.Grant100,
                       groups_const = c("City", "District_Most_Activity", "Discipline", "OGP_Budget_Category"),
                       mod=NULL, verbose=FALSE, niter=100
                     ) {

  stopifnot(all(groups_const %in% colnames(data)))

  if(is.character(B)) B <- data[[B]]
  if(is.character(Y)) Y <- data[[Y]]

  X <- basis(B)
  Y <- Y

  g_consts_l <- lapply(data[groups_const], make_group_constraint, B=B, b_0=Y_low)
  Amat <- as.matrix(do.call(rbind, lapply(g_consts_l, `[[`, "Amat")))
  b_0 <- Reduce(c, lapply(g_consts_l, `[[`, "b_0"))

  # Remove Constraints from one org
  i <- Amat[,1] > 1
  Amat <- Amat[i, ]
  b_0 <- b_0[i]

  sigma <- basis(100*1000000)[1,]

  ### Rescale to 0:1
  X    <- X %*% diag(1/sigma)
  Amat <- Amat %*% diag(1/sigma)



  # Add slack variables
  slack <- -diag(nrow(Amat))

  X2 <- cbind(X, matrix(0, nrow=nrow(X), ncol=ncol(slack)))
  Amat2 <- cbind(Amat, slack)
  b_02 <- b_0
  u2 <- c(7700, sigma[-1], rep(1000000, nrow(slack)) ) # 7700 max + scaled box constraints [$0,$1]
  s <- sqrt(max(sum(b_02, Y)))

  if(is.list(mod)){
    if("s" %in% names(mod)) s <- s * mod[["s"]]

    if("base" %in% names(mod)) u2[1] <- mod[["base"]]
  }

  # coef_consts_l <- make_coef_constraints(ncol(X))
  #
  # Amat <- rbind(Amat, coef_consts_l$Amat)
  # b_0 <- c(b_0, coef_consts_l$b_0)

  # solve.QP(crossprod(X), crossprod(X,Y), t(Amat), b_0)
  sol <- LowRankQP(crossprod(X2) /s, crossprod(X2,Y) / s,
                   as.matrix(Amat2) /s, b_02 /s,
                   u=u2,
                   verbose=verbose, niter=niter); #print(round(.Last.value$alpha[1:9] / sigma, 4));
  round(sol$alpha[1:9] / sigma, 4)
}



##############################################################################################

ogp_summary_table <- function(data, g, caption=NULL, Year=19) {
  g <- enquo(g)
  caption = caption %||% as.character(get_expr(g))
  data %>% filter(Year %in% !!Year) %>% group_by(!!g) %>% summarise(
    n=n(),
    `Total Budget`=sum(Budget_Size),
    `Total Max Request`=sum(Current.Grant100),
    `Total HPA`=sum(Current.Awards),
    `Total Awarded` = sum(Current.Final),
    ) %>%
      ungroup() %>%
      mutate(`Percent Awarded` = 100* `Total Awarded` / sum(`Total Awarded`)) %>%
      arrange(-`Percent Awarded`) %>%
    structure(., class=c("ogp_table", class(.)), caption=caption)
}

print.ogp_table <- function(x, ...) {
  if( length(knitr::opts_current$get()) > 0) {
    knitr::kable(x,  digits = 2, format.args=list(big.mark=','), caption = attr(x, "caption")) %>%
      kableExtra::column_spec(1, width="10em") %>%
      knitr::knit_print()
  } else {
    print.data.frame(x)
  }
}

`%||%` <- function(x,y) if(is.null(x)) y else x

