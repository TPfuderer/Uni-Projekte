# 1. Synthetisch ----------------------------------------------------------

test_that("Basis-Output-Struktur (synthetisch)", {
  set.seed(1)
  df <- data.frame(
    id = factor(rep(1:12, each = 5)),
    Y  = rnorm(60),
    p1 = rnorm(60),
    p2 = runif(60)
  )

  out <- run_boostedRIRT(
    df,
    x_cols   = c("p1", "p2"),
    y_col    = "Y",
    id_col   = "id",
    iter_max = 100, # kurz halten, damit Tests schnell sind
    alpha    = 0.1,
    folds    = 5,
    seed     = 1,
    maxdepth = 3
  )

  expect_type(out, "list")
  expect_true(all(c("best_params", "cv_summary", "step3_out", "step4_out",
                    "model") %in% names(out)))
  expect_s3_class(out$model, "boostedRIRT")

  # S3-Methoden
  preds <- predict(out$model)
  expect_length(preds, nrow(df))
  expect_true(is.numeric(out$step4_out$rmse_final))
  expect_error(plot(out$model), NA)
})

test_that("Input-Checks greifen bei Var(Y) == 0", {
  df_bad <- data.frame(
    id = factor(rep(1:5, each = 4)),
    Y  = rep(0, 20),
    p1 = rnorm(20)
  )
  expect_error(
    run_boostedRIRT(df_bad, x_cols = "p1", y_col = "Y", id_col = "id"),
    regexp = "Var\\(Y\\) > 0"
  )
})


# 2. CO2 ------------------------------------------------------------------


test_that("Beispieldatensatz CO2 (nur numerische Prädiktoren zulässig)", {
  data(CO2, package = "datasets")
  df <- CO2[, c("Plant", "uptake", "conc")] # nur ID, Y, numerisches X
  out <- run_boostedRIRT(
    df       = df,
    x_cols   = "conc",
    y_col    = "uptake",
    id_col   = "Plant",
    iter_max = 100,
    alpha    = 0.1,
    folds    = 5,
    seed     = 2,
    maxdepth = 3
  )
  expect_s3_class(out$model, "boostedRIRT")
  expect_length(predict(out$model), nrow(df))
})


# #3. Theoph --------------------------------------------------------------

data(Theoph, package = "datasets")

out_theoph <- run_boostedRIRT(
  df       = Theoph[, c("Subject", "conc", "Time", "Dose", "Wt")],
  x_cols   = c("Time", "Dose", "Wt"),
  y_col    = "conc",
  id_col   = "Subject",
  iter_max = 100,
  alpha    = 0.1,
  folds    = 5,
  seed     = 42,
  maxdepth = 3
)

print(out_theoph$model)
plot(out_theoph$model)
head(predict(out_theoph$model))
