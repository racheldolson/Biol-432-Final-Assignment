#Christina Meier
#March 19th, 2025
#PCA/biplot to look at directionality of features in benign and cancerous samples

#setwd
setwd("~/BIOL432/Final-Assignment")

library(dplyr) #1.1.4, for data manipulation
library(ggplot2) #3.5.1, for data plotting
library(ggfortify) #0.4.17
library(ggrepel) #0.9.6

#read in dataframe
data <- read.csv("cancer_data.csv")

#examine the structure
str(data)

#look at correlated traits in mean, se, and worst categories
means <- data %>%
  select(ends_with("mean"))
cor(means)
round(cor(means, use = "pairwise.complete.obs")^2, 3)
#some high cors 

se <- data %>%
  select(ends_with("se"))
cor(se)
round(cor(se, use = "pairwise.complete.obs")^2, 3)
#correlations here

worst <- data %>%
  select(ends_with("worst"))
cor(worst)
round(cor(worst, use = "pairwise.complete.obs")^2, 3)
#also correlations here

#scale data by z-score to account for differences in mean and variance across different features (predictor variables)
meanPCA <- princomp(means, cor = T)
str(meanPCA)
head(meanPCA$scores)
round(cor(meanPCA$scores),3)
summary(meanPCA)

#scree plot to visualize which PC axes to keep
meanPCloadings <- data.frame(Component=c(1:10),
                             Eigenvalue=meanPCA$sdev^2)

plot <- ggplot(aes(x=Component, y=Eigenvalue), data = meanPCloadings) +
  geom_point() +
  geom_line()

plot
#keeping the first 3 PCvalues is plausible

#for se's
sePCA <- princomp(se, cor = T)
str(sePCA)
head(sePCA$scores)
round(cor(sePCA$scores),3)
summary(sePCA)

#scree plot to visualize which PC axes to keep
meanSEloadings <- data.frame(Component=c(1:10),
                             Eigenvalue=sePCA$sdev^2)

plot_se <- ggplot(aes(x=Component, y=Eigenvalue), data = meanSEloadings) +
  geom_point() +
  geom_line()

plot_se
#keep the first 4 here

worstPCA <- princomp(worst, cor = T)
str(worstPCA)
head(worstPCA$scores)
round(cor(worstPCA$scores),3)
summary(worstPCA)

#scree plot to visualize which PC axes to keep
meanWorstloadings <- data.frame(Component=c(1:10),
                             Eigenvalue=worstPCA$sdev^2)

plot_worst <- ggplot(aes(x=Component, y=Eigenvalue), data = meanWorstloadings) +
  geom_point() +
  geom_line()

plot_worst
#keep first 3 here

#try running them all together
all_pca_dat <- data %>%
  select(-id, -diagnosis)
allPCA <- princomp(all_pca_dat, cor = T)
str(allPCA)
head(allPCA$scores)
round(cor(allPCA$scores),3)
summary(allPCA)

#scree plot to visualize which PC axes to keep
allloadings <- data.frame(Component=c(1:30),
                                Eigenvalue=allPCA$sdev^2)

plot_all <- ggplot(aes(x=Component, y=Eigenvalue), data = allloadings) +
  geom_point() +
  geom_line()

plot_all
#keeping the first 3 or 4 make sense here

#look at loadings from all PCA's
meanPCA$loadings

#try plotting full PCA
p <- autoplot(allPCA, data = data, colour = "diagnosis", 
         loadings = T, loadings.label = T,
         loadings.colour = "black",
         loadings.label.colour = "red")
p  
#watch tutorial to figure out the overlap here

