#' ---
#' title: "Multivariate data"
#' author: "Yohan"
#' output: 
#'   html_document:
#'     preserve_yaml: true
#'     toc: true
#'     toc_float: true
#'     keep_md: true
#' ---

#+ r setup, include = FALSE
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
library(gapminder)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(corrplot)
library(lattice)
library(GGally)
library(scatterplot3d)
library(BDgraph)
library(mvtnorm)
library(MVN)
library(sn)

#' Multivariate normality is required for regression, model-based clustering, PCA and ANOVA. Then how to test? qqplot (qqnorm, qqline) to detect heavier tail, skewness, outliers, and clustered data. If any sinble variable fails to be a normality, we can not have joint multivariate.  

#+ r 
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
View(wine)

colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash', 'Alcalinity', 'Magnesium', 'Phenols', 'Flavanoids', 'Nonflavanoids','Proanthocyanins', 'Color', 'Hue', 'Dilution', 'Proline')
wine$Type <- as.factor(wine$Type)

## plots
var.wine <- var(wine[2:5])
cor.wine <- cor(wine[, 2:5])
corrplot(cor.wine, method = "ellipse")
pairs(wine[,2:5])
splom( ~ wine[,2:5],  pch = 16, col = wine$Type)
(wine.gg <- ggpairs(data = wine, columns = 2:5))
scatterplot3d(wine[, c(2, 3, 5)], color = wine$Type, angle = 70)

## samples
mu.sim <- c(2, -2)
sigma.sim <- matrix(c(9,5,5,4), 2,2)
multnorm.sample <- rmvnorm(n = 100, mean = mu.sim, sigma = sigma.sim)
head(multnorm.sample)
plot(multnorm.sample)

## density 
multnorm.dens <- dmvnorm(multnorm.sample, mean = mu.sim, sigma = sigma.sim)
scatterplot3d(cbind(multnorm.sample, multnorm.dens),    
              color="blue", pch="", type = "h",             
              xlab = "x", ylab = "y", zlab = "density")

mvals <- expand.grid(seq(-5, 10, length.out = 40), seq(-8, 4, length.out = 40))
mvds <- dmvnorm(x = mvals, mean = mu.sim, sigma = sigma.sim)
matrix_mvds <-  matrix(mvds, nrow = 40)
persp(matrix_mvds, theta = 80, phi = 30, expand = 0.6, shade = 0.2, col = "lightblue", xlab = "x", ylab = "y", zlab = "dens")

pmvnorm(lower = c(-1, -1), upper = c(1, 1))
pmvnorm(lower = c(-5, -5), upper = c(5, 5), mean = mu.sim, sigma = sigma.sim)

qmvnorm(0.9, tail = "both", sigma = diag(2))
qmvnorm(0.95, tail = "both", mean = mu.sim, sigma = sigma.sim)

qqnorm(multnorm.sample[, 1])
qqline(multnorm.sample[, 1])

mvn(multnorm.sample)
mvn(wine[, 2:5])

## t distribution (e.g. financial stock time series)
### rmvt, dmvt, qmvt, pmvt
multt.sample <- rmvt(n = 200,sigma = sigma.sim, df = 5, delta = mu.sim)
mvn(multt.sample, multivariatePlot = "qq")
multt.dens <- dmvt(x = multt.sample, delta = mu.sim, sigma = sigma.sim, df = 5, log = F)
scatterplot3d(cbind(multt.sample, multt.dens),    
              color = "blue", pch = "", type = "h",             
              xlab = "x", ylab = "y", zlab = "density")

pmvt(lower = c(-5, -5), upper = c(5, 5), 
     delta = mu.sim, df = 5, sigma = sigma.sim) 
# CDF, e.g., Probability for all 3 stocks between $100 and 200. 
qmvt(p = 0.9, tail = "both", sigma = diag(2)) # inverse CDF, showing the circle of radius for 90%

## skew distribution
skewnorm.sample <- rmsn(n = 100, xi = mu.sim, Omega = sigma.sim, alpha = c(4, -4))
ggplot(as.data.frame(skewnorm.sample), aes(x = V1, y = V2)) + 
  geom_point() + 
  geom_density_2d() 

mvn(skewnorm.sample, multivariatePlot = "qq")

xi <- c(1,2,-5)
omega <- matrix(c(1,1,0,
                1,2,0,
                0,0,5), 3,3)
alpha <- c(4,30,-5)
skew.s <- rmsn(n = 2000, xi = xi, Omega = omega, alpha = alpha)
ggpairs(data = as.data.frame(skew.s))
msn.mle(y = skew.s, opt.method = "BFGS")

skewt.s <- rmst(n = 2000, xi = xi, Omega = omega, alpha = alpha, nu = 4)
ggpairs(data = as.data.frame(skewt.s))
msn.mle(y = skewt.s, opt.method = "BFGS")

