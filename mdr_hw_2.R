library(faraway)

data(seatpos)

model_all <- lm(hipcenter ~ ., data = seatpos)
summary(model_all)

predictors <- subset(seatpos, select = -hipcenter)
cor_matrix <- cor(predictors)
print(round(cor_matrix, 2))

predictor_names <- names(predictors)
r2_values <- numeric(length(predictor_names))
p_values <- numeric(length(predictor_names))

for (i in seq_along(predictor_names)) {
  form <- as.formula(paste("hipcenter ~", predictor_names[i]))
  model_simple <- lm(form, data = seatpos)
  r2_values[i] <- summary(model_simple)$r.squared
  p_values[i] <- summary(model_simple)$coefficients[2,4]
  
  cat("\n Simple regression with", predictor_names[i], "\n")
  print(summary(model_simple))
}

best_index <- which.max(r2_values)
best_predictor <- predictor_names[best_index]
best_r2 <- r2_values[best_index]

cat("\nBest predictor:", best_predictor, "with R² =", round(best_r2, 4), "\n")
cat("Multiple regression R² was:", round(summary(model_all)$r.squared, 4), "\n")
