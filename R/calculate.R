

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

OGP.2020 <- function(B) {

  # from https://www.lacountyarts.org/sites/default/files/js/js_pa6EhFUALtJQlWo5Qj-7Gs4y5d9vTSerRl4vDyFWoVE.js

  Vectorize(function(budgetSize){
    if (budgetSize < 7500) {
      grant =      -0 + 1.000000 * budgetSize;
    }
    else if (budgetSize < 100000) {
      grant =    5080 + 0.323000 * budgetSize;
    }
    else if (budgetSize < 1500000) {
      grant =   33190 + 0.041900 * budgetSize;
    }
    else if (budgetSize < 40000000) {
      grant =   90040 + 0.003999 * budgetSize;
    }
    else if (budgetSize < 999999999) {
      grant =  250000 + 0.000000 * budgetSize;
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

  drop(basis(B, categories = categories) %*% beta)
}

OGP.2020_forward <- function(B, beta=NULL, categories=NULL) {

  if(is.null(beta)) {
    c1 <- 1
    c2 <- 0.323000
    c3 <- 0.041900
    c4 <- 0.003999
    beta       <-   c(c1,        c1,      c2,    c2,    c2,     c3,      c4,       c4,        0)
  }

  if(is.null(categories)) {
    categories <-   c(0,        1,     7500, 17500, 50000, 100000, 1500000, 10000000, 40000000)
  }

  drop(basis(B, categories = categories) %*% beta)
}

### Main Award calc ####
Award <- function(Budget, Grant100Formula, Scores, TotalBudget=4500000, Year=1, beta=NULL, categories=NULL, ...) {

  # TotalBudget should either be year:chr or, if scalar will be elongated
  if(length(TotalBudget) == 1) {
    uy <- unique(Year)
    TotalBudget <- setNames(rep(TotalBudget, length(uy)), uy)
  }

  Grant100 <- Grant100Formula(Budget, beta=beta, categories=categories)
  Awards   <-  Grant100 * Scores
  mod      <- tapply(Awards, Year, sum)

  mod <- TotalBudget[names(mod)] / mod

  Final <- Awards * mod[Year]

  structure(data.frame(..., Grant100, Awards, Final), mod=mod)
}

make_group_constraint <- function(B, group, b_0=B, dropZeros=TRUE, categories=NULL) {

  g <- fac2sparse(as.factor(group))
  Amat <- g %*% basis(B, categories=categories)
  b_0 <- g %*% b_0

  if(dropZeros) {
    i <- Amat[,2] > 0
    Amat <- Amat[i,]
    b_0 <- b_0[i]
  }

  list(Amat=Amat, b_0=b_0)
}



scenario <- function(data,
                       B = data$Budget_Size,
                       Y = data$Current.Final,
                       Y_low = .95 * data$Current.Grant100,
                       groups_const = c("City", "District", "Discipline", "OGPCat", "Objective"),
                       mod=NULL, verbose=FALSE, niter=100
                     ) {

  stopifnot(all(groups_const %in% colnames(data)))

  if(is.character(B)) B <- data[[B]]
  if(is.character(Y)) Y <- data[[Y]]
  if(is.character(Y_low)) Y_low <- data[[Y_low]]

  X <- basis(B)
  Y <- Y

  g_consts_l <- lapply(data[groups_const], make_group_constraint, B=B, b_0=Y_low)
  Amat <- as.matrix(do.call(rbind, lapply(g_consts_l, `[[`, "Amat")))
  b_0 <- Reduce(c, lapply(g_consts_l, `[[`, "b_0"))

  # Remove Constraints from one org
  i <- Amat[,1] > 1
  Amat <- Amat[i, ]
  b_0 <- b_0[i]

  if(hasName(mod, "extra_inq")) {
    Amat <- rbind(Amat, mod$extra_inq[,-10])
    b_0 <- c(b_0, mod$extra_inq[,10])
  }

  k_ineq <- nrow(Amat)

  if(hasName(mod, "extra_eq")) {
    Amat <- rbind(Amat, mod$extra_eq[,-10])
    b_0 <- c(b_0, mod$extra_eq[,10])
  }


  ### Rescale to 0:1
  sigma <- basis(100*1000000)[1,]
  X    <- X %*% diag(1/sigma)
  Amat <- Amat %*% diag(1/sigma)



  # Add slack variables
  slack <- -diag(nrow=nrow(Amat), ncol=k_ineq)

  X2 <- cbind(X, matrix(0, nrow=nrow(X), ncol=ncol(slack)))
  Amat2 <- cbind(Amat, slack)
  b_02 <- b_0
  u2 <- c(7700, sigma[-1], rep(1000000, ncol(slack)) ) # 7700 max + scaled box constraints [$0,$1]
  s <- sqrt(max(sum(b_02, Y)))


  if(hasName(mod, "s")) s <- s * mod[["s"]]
  if(hasName(mod, "base")) u2[1] <- mod[["base"]]
  if(hasName(mod, "slopes")) u2[2:9] <- mod[["slopes"]] * u2[2:9]
  method <- if(hasName(mod, "method"))  mod[["method"]] else "PFCF"




  # coef_consts_l <- make_coef_constraints(ncol(X))
  #
  # Amat <- rbind(Amat, coef_consts_l$Amat)
  # b_0 <- c(b_0, coef_consts_l$b_0)

  # solve.QP(crossprod(X), crossprod(X,Y), t(Amat), b_0)
  sol <- LowRankQP(crossprod(X2) /s, crossprod(X2,Y) / s,
                   as.matrix(Amat2) /s, b_02 /s,
                   u=u2,
                   method=method, verbose=verbose, niter=niter); #print(round(.Last.value$alpha[1:9] / sigma, 4));
  round(sol$alpha[1:9] / sigma, 4)
}


scenario2 <- function(data,
                     B = data$Budget_Size,
                     Y = data$Current.Final,
                     Y_low = .95 * data$Current.Grant100,
                     groups_const = c("City", "District", "Discipline", "OGPCat"),
                     mod=NULL, verbose=FALSE, niter=100,
                     categories=NULL
) {

  stopifnot(all(groups_const %in% colnames(data)))

  if(is.character(B)) B <- data[[B]]
  if(is.character(Y)) Y <- data[[Y]]
  if(is.character(Y_low)) Y_low <- data[[Y_low]]

  X <- basis(B, categories = categories)
  Y <- Y

  g_consts_l <- lapply(data[groups_const], make_group_constraint, B=B, b_0=Y_low, categories=categories)
  Amat <- as.matrix(do.call(rbind, lapply(g_consts_l, `[[`, "Amat")))
  b_0 <- Reduce(c, lapply(g_consts_l, `[[`, "b_0"))

  # Remove Constraints from single org (ie sum of intercepts =1)
  i <- Amat[,1] > 1
  Amat <- Amat[i, ]
  b_0 <- b_0[i]

  k_cats <- ncol(X)+1 # final column is the LHS

  if(hasName(mod, "extra_inq")) {
    Amat <- rbind(Amat, mod$extra_inq[,-k_cats])
    b_0 <- c(b_0, mod$extra_inq[,k_cats])
  }

  #k_ineq <- nrow(Amat)
  meq = 0

  if(hasName(mod, "extra_eq")) {
    meq = nrow(mod$extra_eq)

    Amat <- rbind(mod$extra_eq[,-k_cats], Amat)
    b_0 <- c(mod$extra_eq[,k_cats], b_0)
  }


  ### Rescale to 0:1
  sigma <- basis(max(B), categories = categories)[1,]
  X    <- X %*% diag(1/sigma)
  Amat <- Amat %*% diag(1/sigma)




  sol <- solve.QP(crossprod(X), crossprod(X,Y), t(Amat), b_0, meq=meq)

  round(sol$solution / sigma, 4)
}



# Pretty Printing for Report #####

ogp_summary_table <- function(data, g, caption=NULL, Year=19) {
  g <- enquo(g)
  caption = caption %||% as.character(get_expr(g))
  data %>% filter(Year %in% !!Year) %>% group_by(!!g) %>% summarise(
    n=n(),
    `Total Budget`=sum(BudgetSize),
    `Total Max Request`=sum(Current.Grant100),
    `Total Scored Request`=sum(Current.Awards),
    `Total Awarded` = sum(Current.Final),
    ) %>%
      ungroup() %>%
      mutate(`Percent Awarded` = 100* `Total Awarded` / sum(`Total Awarded`)) %>%
      arrange(-`Percent Awarded`) %>%
    structure(., class=c("ogp_table", class(.)), caption=caption)
}

print.ogp_table <- function(x, ...) {
  if( isTRUE(getOption('knitr.in.progress'))) {
    knitr::kable(x,  digits = 2, format.args=list(big.mark=','), caption = attr(x, "caption")) %>%
      kableExtra::column_spec(1, width="10em") %>%
      knitr::knit_print()
  } else {
    print.data.frame(x)
  }
}

# WWW/JS Generation #####

solveForBrackets <- function(scenario, categories=NULL) {

  if(is.null(categories))
      categories <-   c(0,        1,     7500, 17500, 50000, 100000, 1500000, 10000000, 40000000)

  to_solve <- categories[categories > 0]

  ret <- matrix(0, length(to_solve), 3)

  i <- 1
  for(ts in to_solve) {
    ret[i, 1]   <- ts
    ret[i, 2:3] <- solve(cbind(c(1,1), ts+c(0,10)), OGP.2020_forward(ts + c(0,10), beta = scenario, categories=categories))

    i <- i +1
  }

  ret[, 2] <- round(ret[,2],-1)
  ret[, 3] <- round(ret[,3], 6)
  ret <- ret[!duplicated(ret[,-1]), ]

  k <- nrow(ret)
  ret[1:(k-1),1] <- ret[-1,1]
  ret[k,1] <- 999999999 #1B
  ret
}



to_code <- function(solution) {
  fmt = "if (budgetSize < %10.0f) {\n          grant = %7.0f + %7.6f * budgetSize;\n      }"
  conditions <- apply(solution, 1, function(s) {do.call(sprintf, c(list(fmt=fmt), s))})
  conditions <- paste(conditions, collapse = "\n      else ")
  fmt <- "
 function calculateAndDisplay() {
    var budgetSize = parseInt($('.budget-size').val(), 10);

    if(budgetSize) {

      var grant = 0;
      %s

      grant = numberWithCommas(Math.ceil(grant));

      function numberWithCommas(x) {
          return x.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
      }

      // clear our any string we appended if user clicks button again
      $('.max-grant-request').remove();

      $('.calc-grant').append(\"<p class='max-grant-request'>YOUR MAXIMUM GRANT REQUEST IS: <span class='budget-size-result'>$\" + grant + '</span></p>');
    }

  }

"  ;
  sprintf(fmt, conditions)
}


to_text <- function(solution) {

  fmt = "<li>For B < %s, X = %s + %5.4f * B</li>"
  conditions <- apply(solution, 1, function(s) {do.call(sprintf,
                                                        list(fmt=fmt,
                                                              format(s[1], nsmall=0, digits=10, big.mark=',', scientific=FALSE),
                                                              format(s[2], nsmall=0, digits=10, big.mark=',', scientific=FALSE),
                                                             s[3]))
    })
  conditions <- paste(conditions, collapse = "\n")
  conditions <- gsub("+ 0.0000 * B", "", conditions, fixed=TRUE)

  conditions
}

