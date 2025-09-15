library(MPV)

data("earthquake")

eq_model <- lm(magnitude ~ ., data = earthquake)

summary(eq_model)

par(mfrow = c(2,2))

plot(eq_model)
