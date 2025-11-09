strength <- c(3129, 3000, 2865, 2890,
              3200, 3300, 2975, 3150,
              2800, 2900, 2985, 3050,
              2600, 2700, 2600, 2765)

technique <- factor(rep(1:4, each = 4))

data <- data.frame(technique, strength)

print(data)

full_model <- lm(strength ~ technique-1,data)
null_model <- lm(strength ~ 1,data)
anova(null_model, full_model)
res <- residuals(full_model)
qqnorm(res)
qqline(res, col = "red")
plot(fitted(full_model), res,
     main = "Residuals vs Fitted Values",
     xlab = "Fitted values", ylab = "Residuals",
     pch = 19, col = "blue")
abline(h = 0, col = "red")
library(emmeans)
emm = emmeans(full_model, pairwise~technique, adjust = 'none')
emm
emm = emmeans(full_model, pairwise~technique, adjust = 'bonferroni')
emm
