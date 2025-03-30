#Christina Meier
#March 19th, 2025
#PCA/biplot to look at directionality of features in benign and cancerous samples

#setwd
setwd("~/BIOL432/Final-Assignment")

library(dplyr) #1.1.4, for data manipulation
library(ggplot2) #3.5.1, for data plotting
library(ggfortify) #0.4.17, for plotting
library(ggrepel) #0.9.6, for label jittering

#read in dataframe
data <- read.csv("cancer_data.csv")

#examine the structure
str(data)

#incorporating the bash script regex expression
colnames(data) <- gsub("_", " ", colnames(data))

#capitalize diagnosis column name
data <- data %>%
  rename(Diagnosis = diagnosis)

#make malignant and benign full names
data <- data %>%
  mutate(Diagnosis = ifelse(Diagnosis == "M", "Malignant", Diagnosis)) %>%
  mutate(Diagnosis = ifelse(Diagnosis == "B", "Benign", Diagnosis))

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
  select(-id, -Diagnosis)
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
ggsave("screeplot.pdf", plot = plot_all, height = 7, width = 12)
#keeping the first 2 or 3 make sense here

#look at loadings from all PCA's
meanPCA$loadings

range(allPCA$scores[, 1:2]) 
#-12 to 16
range(allPCA$loadings[, 1:2])
#-0.3 to 0.26

#manually rescale to autoplot for labels
scaling_Factor <- 0.5

#Plot biplot with arrows
p <- autoplot(allPCA, data = data, colour = "Diagnosis", loadings = T, loadings.colour = "grey")
p

#extract loading titles, and apply scaling factor
loadings_df <- as.data.frame(allPCA$loadings[, 1:2] * scaling_Factor)
loadings_df$label <- rownames(loadings_df)


# Add the loading labels manually
biplot <- p + geom_label_repel(data = loadings_df,
                    aes(x = Comp.1, y = Comp.2, label = label),
                    fill = "white", xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
                    colour = "black",
                    label.size = 0.2,
                    size = 3,
                    max.overlaps = Inf,
                    force = 0.05,
                    force_pull = 0.3,
                    box.padding = 0.05,
                    point.padding = 0.01) +
                    theme_bw() +
                    theme(text = element_text(size = 20),
                          plot.title = element_text(size = rel(1.1), hjust = 0.5),
                          axis.title = element_text(size = rel(1.2)),
                          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.25),
                          axis.ticks = element_line(linewidth = 1))
biplot

ggsave("biplot.pdf", plot = biplot, height = 7, width = 12)

