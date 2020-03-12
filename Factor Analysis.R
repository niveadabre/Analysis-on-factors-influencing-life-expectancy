expect <- read.csv("C:/Users/dabre/OneDrive/Desktop/lfe.csv")

sapply(expect, function(x) sum(is.na(x)))
expect <- expect[complete.cases(expect),]  ## to remove which has null values
sapply(expect, function(x) sum(is.na(x)))


View(expect)
attach(expect)
expect[1]

# Computing Correlation Matrix
corrm.expect <- cor(expect[,5:22])
corrm.expect
plot(corrm.expect)
expect_pca <- prcomp(expect[,5:22], scale=TRUE)
summary(expect_pca)
plot(expect_pca)

# A table containing eigenvalues and %'s accounted, follows. Eigenvalues are the sdev^2
(eigen_expect <- round(expect_pca$sdev^2,2))
names(eigen_expect) <- paste("PC",1:18,sep="")
eigen_expect
sumlambdas <- sum(eigen_expect)
sumlambdas
cumvar_expect <- cumsum(propvar)
propvar <- round(eigen_expect/sumlambdas,2)
propvar
cumvar_expect <- cumsum(propvar)
cumvar_expect
matlambdas <- rbind(eigen_expect,propvar,cumvar_expect)
matlambdas
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
rownames(matlambdas)
eigvec.expect <- expect_pca$rotation
print(expect_pca)
# Taking the first four PCs to generate linear combinations for all the variables with four factors
pcafactors.expect <- eigvec.expect[,1:8]
pcafactors.expect
# Multiplying each column of the eigenvector’s matrix by the square-root of the corresponding eigenvalue in order to get the factor loadings
unrot.fact.expect <- sweep(pcafactors.expect,MARGIN=2,expect_pca$sdev[1:8],`*`)
unrot.fact.expect

# Computing communalities
communalities.expect <- rowSums(unrot.fact.expect^2)
communalities.expect
# Performing the varimax rotation. The default in the varimax function is norm=TRUE thus, Kaiser normalization is carried out
rot.fact.expect <- varimax(unrot.fact.expect)
View(unrot.fact.expect)
rot.fact.expect
# The print method of varimax omits loadings less than abs(0.1). In order to display all the loadings, it is necessary to ask explicitly the contents of the object $loadings
fact.load.expect <- rot.fact.expect$loadings[,1:8]
fact.load.expect
# Computing the rotated factor scores for the 30 European Countries. Notice that signs are reversed for factors F2 (PC2), F3 (PC3) and F4 (PC4)
scale.expect <- scale(expect[,5:22])
scale.expect
as.matrix(scale.expect)%*%fact.load.expect%*%solve(t(fact.load.expect)%*%fact.load.expect)

library(psych)
install.packages("psych", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(psych)
fit.pc <- principal(expect[,5:22], nfactors=8, rotate="varimax")
fit.pc
round(fit.pc$values, 3)
fit.pc$loadings
# Loadings with more digits
for (i in c(1,2,3,4,5,6,7,8)) { print(fit.pc$loadings[[1,i]])}
# Communalities
fit.pc$communality
# Rotated factor scores, Notice the columns ordering: RC1, RC3, RC2 and RC4
fit.pc$scores
# Play with FA utilities

fa.parallel(expect[,5:22]) # See factor recommendation
fa.plot(fit.pc) # See Correlations within Factors
fa.diagram(fit.pc) # Visualize the relationship
vss(expect[,5:22]) # See Factor recommendations for a simple structure

