#Multiple Regression
expect <- read.csv("C:/Users/dabre/OneDrive/Desktop/lfe.csv")

head(expect)

sapply(expect, function(x) sum(is.na(x)))
expect <- expect[complete.cases(expect),]  ## to remove which has null values
sapply(expect, function(x) sum(is.na(x)))
#expect_x <- subset.data.frame(expect, Year == "2000")

View(expect)

# Performing multiple regression on life expectancy dataset
fit <- lm( Life.expectancy~Adult.Mortality+infant.deaths+Alcohol+percentage.expenditure+Hepatitis.B+Measles+BMI+under.five.deaths+Polio+Total.expenditure+Diphtheria+HIV.AIDS+GDP+Population, data=expect)
#show the results
summary(fit)
#Summary has three sections. Section1: How well does the model fit the data (before Coefficients). Section2: Is the hypothesis supported? (until sifnif codes). Section3: How well does data fit the model (again).
# Useful Helper Functions
coefficients(fit)

ggpairs(data=expect[,4:22], title="Expectancy Data")
library(FFally)
library(GGally)
install.packages("GGally", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(GGally)

confint(fit,level=0.95)
# Predicted Values
fitted(fit)
residuals(fit)
#Anova Table
anova(fit)
vcov(fit)
cov2cor(vcov(fit))
temp <- influence.measures(fit)
temp
View(temp)
#diagnostic plots
plot(fit)


# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(expect)-length(fit$coefficients)-2))


plot(fit, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# distribution of studentized residuals
library(MASS)
sresid <- studres(fit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
#Non-constant Error Variance
# Evaluate homoscedasticity





# Global test of model assumptions
library(gvlma)
install.packages("gvlma", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)
fit
summary(fit)
fit1 <- fit
fit2 <- lm(Life.expectancy~GDP+Population+Alcohol, data=expect)
# compare models
anova(fit1, fit2)
step <- stepAIC(fit, direction="both")
step$anova # display results
install.packages("leaps", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(leaps)
leaps<-regsubsets(Life.expectancy~GDP+Population+Alcohol, data=expect,nbest=10)
# view results
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps)
plot(leaps,scale="r2")
subsets(leaps, statistic="rsq")
# All Subsets Regression
plot(leaps,scale="bic")

summary(leaps)
?regsubsets
summary(leaps)
View(leaps)
leaps
coef(leaps,1:5)

# Calculate Relative Importance for Each Predictor
install.packages("relaimpo", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(relaimpo)
calc.relimp(fit,type=c("lmg","last","first","pratt"),
            rela=TRUE)
# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(fit, b = 1000, type = c("lmg",
                                            "last", "first", "pratt"), rank = TRUE,
                    diff = TRUE, rela = TRUE)

booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result
#https://rpubs.com/davoodastaraky/mtRegression
summary(fit)
predict.lm(fit, data.frame(wt =3.2 ,drat=3.9,hp=130,disp=150) )

