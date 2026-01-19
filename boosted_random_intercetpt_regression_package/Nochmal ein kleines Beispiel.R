
install.packages("lme4")
library(lme4)
df_real <- sleepstudy


# Run boostedRIRT
mod_real <- run_boostedRIRT(
  df_real,
  x_cols   = c("Days"),        # Prädiktor
  y_col    = "Reaction",       # Zielvariable
  id_col   = "Subject",        # Cluster
  iter_max = 50,
  alpha    = 0.1,
  folds    = 5,
  seed     = 4242,
  maxdepth = 3
)

# S3-Methoden testen
print(mod_real)
plot(mod_real)
head(predict(mod_real))
