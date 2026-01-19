#' Erzeuge ein boostedRIRT-Objekt
#' @keywords internal


# 5. Modellobjekt ---------------------------------------------------------

boostedRIRT <- function(X,
                        Y,
                        id,
                        colsample,
                        subsample,
                        alpha,
                        iter_final,
                        maxdepth = 3) {
  n <- nrow(X)
  f <- rep(mean(Y), n)
  rmse_path <- numeric(iter_final)

  for (iter in 1:iter_final) {
    keep_n <- max(1, floor(subsample * n))
    keep_p <- max(1, floor(colsample * ncol(X)))
    rows <- if (keep_n < n) {
      base::sample.int(n, keep_n)
    } else {
      base::seq_len(n)
    }
    cols <- if (keep_p < ncol(X)) {
      base::sample(base::seq_len(ncol(X)), keep_p)
    } else {
      base::seq_len(ncol(X))
    }

    g <- Y - f
    dat_sub <- data.frame(g = g[rows], X[rows, cols, drop = FALSE])
    tree <- rpart::rpart(
      g ~ .,
      data = dat_sub,
      control = rpart::rpart.control(maxdepth = maxdepth, cp = 0)
    )
    f_cart <- as.numeric(stats::predict(tree, newdata = X))

    e <- Y - (f + f_cart)
    lmm <- lme4::lmer(e ~ 1 + (1 | id), data = data.frame(e = e, id = id))
    beta0 <- as.numeric(lme4::fixef(lmm)[1])
    lmm_fit <- as.numeric(stats::predict(
      lmm,
      newdata = data.frame(id = id),
      allow.new.levels = TRUE
    ))

    f <- f + alpha * (f_cart + (lmm_fit - beta0))
    rmse_path[iter] <- sqrt(mean((Y - f)^2))
  }

  obj <- list(
    fitted_values = f,
    rmse_final    = rmse_path[iter_final],
    rmse_path     = rmse_path,
    iter_final    = iter_final,
    colsample     = colsample,
    subsample     = subsample,
    alpha         = alpha
  )
  class(obj) <- "boostedRIRT"
  obj
}


# 6. Methoden für boosted_rirt --------------------------------------------


#' @export
print.boostedRIRT <- function(x, ...) {
  cat("Boosted Random Intercept Regression Tree\n")
  cat("  Iterations (final):", x$iter_final, "\n")
  cat("  Final RMSE:         ", round(x$rmse_final, 4), "\n")
  cat("  colsample:          ", x$colsample, "\n")
  cat("  subsample:          ", x$subsample, "\n")
  cat("  alpha:              ", x$alpha, "\n")
  invisible(x)
}

#' @export
predict.boostedRIRT <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(object$fitted_values)
  } else {
    stop("Vorhersage auf neuen Daten ist in dieser
         simplen Version nicht implementiert.")
  }
}

#' @export
plot.boostedRIRT <- function(x, ...) {
  graphics::plot(
    seq_along(x$rmse_path),
    x$rmse_path,
    type = "l",
    lwd = 2,
    xlab = "Iteration",
    ylab = "RMSE",
    main = "RMSE-Verlauf (Training)"
  )
  graphics::abline(
    v = x$iter_final,
    col = "red",
    lty = 2
  )
  graphics::points(x$iter_final,
    x$rmse_final,
    col = "red",
    pch = 19
  )
}
