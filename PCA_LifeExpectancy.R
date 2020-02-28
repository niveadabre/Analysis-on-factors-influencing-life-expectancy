library(readr)
expect<-read_csv("C:/Users/dabre/OneDrive/Desktop/lfe.csv")
head(expect)

sapply(expect, function(x) sum(is.na(x)))
expect <- expect[complete.cases(expect),]  ## to remove which has null values
sapply(expect, function(x) sum(is.na(x)))
View(expect)
dim(expect)
#Get the Correlations between the measurements
cor(expect[,5:14])
# Using prcomp to compute the principal components (eigenvalues and eigenvectors). With scale=TRUE, variable means are set to zero, and variances set to one
expect_pca <- prcomp(expect[,5:14],scale=TRUE)
expect_pca
summary(expect_pca)
# sample scores stored in sparrows_pca$x
# singular values (square roots of eigenvalues) stored in sparrow_pca$sdev
# loadings (eigenvectors) are stored in sparrows_pca$rotation
# variable means stored in sparrows_pca$center
# variable standard deviations stored in sparrows_pca$scale
# A table containing eigenvalues and %'s accounted, follows
# Eigenvalues are sdev^2

(eigen_expect <- expect_pca$sdev^2)

names(eigen_expect) <- paste("PC",1:10,sep="")
eigen_expect

sumlambdas <- sum(eigen_expect)
sumlambdas

propvar <- eigen_expect/sumlambdas
propvar # shows the percent variance each variable PC1, PC2...PC5 holds

cumvar_expect <- cumsum(propvar)
cumvar_expect

matlambdas <- rbind(eigen_expect,propvar,cumvar_expect)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)

summary(expect_pca)
expect_pca$rotation
print(expect_pca)
# Sample scores stored in expect_pca$x
expect_pca$x
# Identifying the scores by their status of the country
expecttyp_pca <- cbind(data.frame(expect$Status),expect_pca$x)
expecttyp_pca
# Means of scores for all the PC's classified by Status

tabmeansPC <- aggregate(expecttyp_pca[,2:11],by=list(Status=expect$Status),mean)
tabmeansPC

tabmeansPC <- tabmeansPC[rev(order(tabmeansPC$Status)),]
tabmeansPC

tabfmeans <- t(tabmeansPC[-1])
tabfmeans

colnames(tabfmeans) <- t(as.vector(tabmeansPC[1]))
tabfmeans

# Standard deviations of scores for all the PC's classified by Status Of the country
tabsdsPC <- aggregate(expecttyp_pca[,2:11],by=list(Status=expect$Status),sd)
tabfsds <- t(tabsdsPC[,-1])
colnames(tabfsds) <- t(as.vector(tabsdsPC[1]))
tabfsds

t.test(PC1~expect$Status,data=expecttyp_pca)
t.test(PC2~expect$Status,data=expecttyp_pca)
t.test(PC3~expect$Status,data=expecttyp_pca)
t.test(PC4~expect$Status,data=expecttyp_pca)
t.test(PC5~expect$Status,data=expecttyp_pca)
t.test(PC6~expect$Status,data=expecttyp_pca)
t.test(PC7~expect$Status,data=expecttyp_pca)
t.test(PC8~expect$Status,data=expecttyp_pca)
t.test(PC9~expect$Status,data=expecttyp_pca)
t.test(PC10~expect$Status,data=expecttyp_pca)

# F ratio tests
var.test(PC1~expect$Status,data=expecttyp_pca)
var.test(PC2~expect$Status,data=expecttyp_pca)
var.test(PC3~expect$Status,data=expecttyp_pca)
var.test(PC4~expect$Status,data=expecttyp_pca)
var.test(PC5~expect$Status,data=expecttyp_pca)
var.test(PC6~expect$Status,data=expecttyp_pca)
var.test(PC7~expect$Status,data=expecttyp_pca)
var.test(PC8~expect$Status,data=expecttyp_pca)
var.test(PC9~expect$Status,data=expecttyp_pca)
var.test(PC10~expect$Status,data=expecttyp_pca)

# Levene's tests (one-sided)
(LTPC1 <- leveneTest(PC1~expect$Status,data=expecttyp_pca))
library(car)
(LTPC1 <- leveneTest(PC1~expect$Status,data=expecttyp_pca))
(p_PC1_1sided <- LTPC1[[3]][1]/2)
(LTPC2 <- leveneTest(PC2~expect$Status,data=expecttyp_pca))
(p_PC2_1sided=LTPC2[[3]][1]/2)
(LTPC3 <- leveneTest(PC3~expect$Status,data=expecttyp_pca))
(p_PC3_1sided <- LTPC3[[3]][1]/2)
(LTPC4 <- leveneTest(PC4~expect$Status,data=expecttyp_pca))
(p_PC4_1sided <- LTPC4[[3]][1]/2)
(LTPC5 <- leveneTest(PC5~expect$Status,data=expecttyp_pca))
(p_PC5_1sided <- LTPC5[[3]][1]/2)
(LTPC6 <- leveneTest(PC6~expect$Status,data=expecttyp_pca))
(p_PC6_1sided <- LTPC6[[3]][1]/2)
(LTPC7 <- leveneTest(PC7~expect$Status,data=expecttyp_pca))
(p_PC7_1sided <- LTPC7[[3]][1]/2)
(LTPC8 <- leveneTest(PC8~expect$Status,data=expecttyp_pca))
(p_PC8_1sided <- LTPC8[[3]][1]/2)
(LTPC9 <- leveneTest(PC9~expect$Status,data=expecttyp_pca))
(p_PC9_1sided <- LTPC9[[3]][1]/2)
(LTPC10 <- leveneTest(PC10~expect$Status,data=expecttyp_pca))
(p_PC10_1sided <- LTPC10[[3]][1]/2)

# Plotting the scores for the first and second components
plot(expecttyp_pca$PC1, expecttyp_pca$PC2,pch=ifelse(expecttyp_pca$Status == "Developed",2,16),xlab="PC1", ylab="PC2", main="1649 sparrows against values for PC1 & PC2")
abline(h=0)
abline(v=0)
legend("bottomleft", legend=c("Developed","Developing"), pch=c(1,16))
plot(eigen_expect, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(eigen_expect), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")
print(summary(expect_pca))
View(expect_pca)
diag(cov(expect_pca$x))
xlim <- range(expect_pca$x[,1])
expect_pca$x[,1]
expect_pca$x
plot(expect_pca$x,xlim=xlim,ylim=xlim)
expect_pca$rotation[,1]
expect_pca$rotation
#plot(expect[,-1])
expect_pca$x
plot(expect_pca)


#get the original value of the data based on PCA
center <- expect_pca$center
scale <- expect_pca$scale
new_expect <- as.matrix(expect[,-1])
new_expect
#drop(scale(new_expect,center=center, scale=scale)%*%expect_pca$rotation[,1])
predict(expect_pca)[,1]
#The aboved two gives us the same thing. predict is a good function to know.
out <- sapply(2:10, function(i){plot(expect$Status,expect_pca$x[,i],xlab=paste("PC",i,sep=""),ylab="Status")})
pairs(expect_pca$x[,1:10], ylim = c(-8,10),xlim = c(-8,10),panel=function(x,y,...){text(x,y,expect$Status)})


