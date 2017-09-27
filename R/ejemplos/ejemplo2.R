# require(graphics)
#
# ## Predictions
# x <- rnorm(15)
# y <- x + rnorm(15)
# predict(lm(y ~ x))
# new <- data.frame(x = seq(-3, 3, 0.5))
# predict(lm(y ~ x), new, se.fit = TRUE)
# pred.w.plim <- predict(lm(y ~ x), new, interval = "prediction")
# pred.w.clim <- predict(lm(y ~ x), new, interval = "confidence")
# matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
#         lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")
#
# cbind(pred.w.clim, pred.w.plim[,-1])


# library(ggplot2)
#
# set.seed(100)
# x <- rnorm(50, mean = 10, sd = 0.5)
# t.test(x, mu=10)
#
# fisher.test(contingencyMatrix, alternative = "greater")  # Fisher's exact test to test independence of rows and columns in contingency table
# friedman.test()  # Friedman's rank sum non-parametric test

# is.na(as.Date(as.character('NULL'),format="%d/%m/%Y"))



