#bigmac_eg_slr

bigmac <- read.csv("Data.csv")

lm.fit <- lm(NetHourlyWage ~ BigMacPrice, data = bigmac)
summary(lm.fit)

x <- bigmac[,2]
y <- bigmac[,3]

xbar <- mean(x)
ybar <- mean(y)

###Computing Estimates
#Computing Slope 
a <- sum((x-xbar)*(y-ybar))
b <- sum((x-xbar)^2)
slope = a/b

#Computing Intercept
intercept <- ybar-(slope*xbar)

###Computing Std.Error
yhat <- predict(lm.fit)
sse <- sum((y-yhat)^2) #sum of sq.errors or sum of sq. residuals
mse <- sse/25 
stderror <- sqrt(mse)
stderror_var_x <- stderror/(sqrt(sum((x-xbar)^2)))
stderror_intercept <- stderror_var_x * sqrt(mean(x^2))

###Computing t-Value
#tValue <- ESTIMATE/STDERROR
tval_intercept <- intercept/stderror_intercept
tval_var_x <- slope/stderror_var_x
tval_intercept; tval_var_x

pval_intercept <- pt(q = tval_intercept, n=27, df=1)
pval_var_x <- pt(q = tval_var_x, df=25)
pval_intercept;pval_var_x

qt(p = .102, df = 25)
pt(q = 1.697051, df = 25, lower.tail = F)*2
pt(q = -1.697051, df = 25, lower.tail = T)+(1-pt(q = 1.697051, df = 25, lower.tail = T))

pt(q = 5.144, df = 25, lower.tail = F)+(1-pt(q = 5.144, df = 25, lower.tail = T))
pt(q = 5.144, df = 25, lower.tail = F)*2


#Computing fitted values - yhat
yhat <- as.numeric(lm.fit$fitted.values)
coef(lm.fit)[1]+coef(lm.fit)[2]*0.1449411

#computing rSquared
ssx <- sum((x-xbar)^2)
ssy <- sum((y-ybar)^2)
ssxy <- sum((x-xbar)*(y-ybar))

rSquared = cov((x-xbar),(y-ybar))^2/(var(x)*var(y))
summary(lm.fit)$r.squared

rSquared = sum((x-xbar)*(y-ybar))^2/(ssx*ssy)
summary(lm.fit)$r.squared

ss_tot <- sum((y-ybar)^2)
ss_reg <- sum((yhat-ybar)^2)
ss_res <- sum((y-yhat)^2)

rSquared = 1- (ss_res/ss_tot)


