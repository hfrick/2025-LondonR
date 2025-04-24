drop_strata <- function(expr, in_plus = TRUE) {
  if (rlang::is_call(expr, "+", n = 2) && in_plus) {
    lhs <- drop_strata(expr[[2]], in_plus = in_plus)
    rhs <- drop_strata(expr[[3]], in_plus = in_plus)
    if (rlang::is_call(lhs, "strata")) {
      rhs
    } else if (rlang::is_call(rhs, "strata")) {
      lhs
    } else {
      rlang::call2("+", lhs, rhs)
    }
  } else if (rlang::is_call(expr)) {
    expr[-1] <- purrr::map(as.list(expr[-1]), drop_strata, in_plus = FALSE)
    expr
  } else {
    expr
  }
}

debugonce(drop_strata)

drop_strata(rlang::expr(a + b))
