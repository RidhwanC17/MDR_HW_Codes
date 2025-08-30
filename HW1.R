data(anscombe)

for(i in 1:4){
  
  x = anscombe[, i]     
  y = anscombe[, i+4]
  
  cat("Dataset", i, "\n")
  
  cat("Mean of x:", mean(x), "\n")
  cat("Mean of y:", mean(y), "\n")
  
  cat("Variance of x:", var(x), "\n")
  cat("Variance of y:", var(y), "\n")
  
  cat("Correlation:", cor(x,y), "\n")
  
  fit = lm(y ~ x)
  cat("Regression line: y =", coef(fit)[1], "+", coef(fit)[2],"x\n")
  
  cat("R-squared:", summary(fit)$r.squared, "\n\n")
}

par(mfrow=c(2,2))
plot(anscombe$x1, anscombe$y1, main="Dataset 1")
abline(lm(y1 ~ x1, data=anscombe), col="red")

plot(anscombe$x2, anscombe$y2, main="Dataset 2")
abline(lm(y2 ~ x2, data=anscombe), col="red")

plot(anscombe$x3, anscombe$y3, main="Dataset 3")
abline(lm(y3 ~ x3, data=anscombe), col="red")

plot(anscombe$x4, anscombe$y4, main="Dataset 4")
abline(lm(y4 ~ x4, data=anscombe), col="red")
