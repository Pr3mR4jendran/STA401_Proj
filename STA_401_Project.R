install.packages("packagename")
install.packages("matrixStats")
library(matrixStats)
library(ggplot2)
library(corrplot)
library(devtools)
library(ggbiplot)
library(psych)
library(factoextra)
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(dplyr)

###Load Dataset

data <- read.csv("4DWW_Survey_Responses.csv")
View(data)
summary(data)

###Data Cleaning and Pre-Processing

#removing first 6 rows since they were trial responses
#removing timestamp, email address, major and commute distance
#also removing some of the survey questions that were removed after the trial responses
data <- data[7:nrow(data),-c(1,2,7,10,22,23,24,25,26,27)]

#Renaming all columns
new_cols = c("Age", "Gender", "Academic_Standing", "College", "Workload_Intensity", 
             "Commuting","PD1", "PD2", "PD3", "PD4", "PD5", "PD6", "PD7", "PD8", 
             "PD9", "PD10", "PHY", "AP1", "AP2", "AP3", "AP4", "AP5", "WLB1", "WLB2", "WLB3", "WLB4", 
             "WLB5", "WLB6", "WLB7", "WLB8", "WLB9", "QL1", "QL2", "QL3", "QL4", "QL5", "QL6", "QL7",
             "QL8", "QL9", "Satisfaction")
names(data) <- new_cols
data$PHY <- as.integer(data$PHY)
str(data)

#creating a new dataset using only the variables
data_var <- data[,-c(1,2,3,4,5,6)]
data_var <- data_var[,-c(35)]
str(data_var)
View(data_var)

#Tests on variables 
bart_spher(data_var)
KMO(data_var)

#Converting analyzing factors into ordinal categorical variables
convert_cols <- c(7:ncol(data))
for (i in convert_cols){
  data[[i]] <- factor(data[[i]], ordered = TRUE)
}
str(data)
#converting other variables into appropriate types
data$Gender = factor(data$Gender)
data$Academic_Standing = factor(data$Academic_Standing)
data$College = factor(data$College)
data$Workload_Intensity = factor(data$Workload_Intensity, ordered = TRUE)
data$Commuting = factor(data$Commuting)

###Exploring the dataset
summary(data)

#Performing Chi-Square Test on all the predictors
for (col in names(data)){
  if (col != "Satisfaction") {
    contingency_table <- table(data[[col]], data$Satisfaction)
    result <- chisq.test(contingency_table)
    print(paste("Chi-square test for", col))
    print(result)
  }
}

#Visualizing satisfaction using a bar plot

ggplot(data, aes(x = data$AP1, fill=data$AP1)) +
  geom_bar() +
  scale_fill_manual("Legend", values = c("blue", "red", "green", "aquamarine", "blueviolet")) +
  labs(title = "Bar Plot of Satisfaction", x = "Satisfaction", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))l_manual("Legend", values = c("blue", "red", "green", "aquamarine", "blueviolet")) +
  labs(title = "Bar Plot of Satisfaction", x = "Satisfaction", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = data$AP2, fill=data$AP2)) +
  geom_bar() +
  scale_fill_manual("Legend", values = c("blue", "red", "green", "aquamarine", "blueviolet")) +
  labs(title = "Bar Plot of Satisfaction", x = "Satisfaction", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = data$AP3, fill=data$AP3)) +
  geom_bar() +
  scale_fill_manual("Legend", values = c("blue", "red", "green", "aquamarine", "blueviolet")) +
  labs(title = "Bar Plot of Satisfaction", x = "Satisfaction", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = data$AP4, fill=data$AP4)) +
  geom_bar() +
  scale_fill_manual("Legend", values = c("blue", "red", "green", "aquamarine", "blueviolet")) +
  labs(title = "Bar Plot of Satisfaction", x = "Satisfaction", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = data$AP5, fill=data$AP5)) +
  geom_bar() +
  scale_fill_manual("Legend", values = c("blue", "red", "green", "aquamarine", "blueviolet")) +
  labs(title = "Bar Plot of Satisfaction", x = "Satisfaction", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = data$Satisfaction, fill=data$Gender)) +
  geom_bar() +
  scale_fill_manual("Legend", values = c("blue", "red", "green", "aquamarine", "blueviolet")) +
  labs(title = "Bar Plot of Satisfaction", x = "Satisfaction", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#using data_var for PCA & FA
correlations <- cor(data_var)
corrplot(correlations, method="color", type="upper", order="hclust")

###PCA
pca_result <- prcomp(data_var)
summary(pca_result)
names(pca_result)
pca_result$sdev
pca_result$rotation
pca_result$center
pca_result$scale
pca_result$x

biplot(pca_result)
ggscreeplot(pca_result)
fviz_pca_var(pca_result, col.var = "contrib", repel = TRUE)

#Fitting a Ordinal Logistic Regression with PCA loadings and Satisfaction
pca_scores <- pca_result$x[,1:5]
pca_model <- polr(data$Satisfaction ~ pca_scores[,1] + pca_scores[,2] + pca_scores[,3] + pca_scores[,4] + 
                    pca_scores[,5], Hess=TRUE)
summary(pca_model)


###Factor Analysis

#EFA, exploratory 
fa_total=fa(data_var, nfactors = 34) #16 significant factors 
names(fa_total)
eigenvalues <- fa_total$values
plot(1:length(eigenvalues), eigenvalues, type = "b", 
     xlab = "Factor Number", ylab = "Eigenvalues", main = "Scree Plot")

M1=fa(data_var, nfactors = 5)
fa.diagram(M1,main="data_var",text=pretty)

plot(density(M1$scores, na.rm = TRUE), 
     main = "Factor Scores")

eigenvalues <- M1$values
plot(1:length(eigenvalues), eigenvalues, type = "b", 
     xlab = "Factor Number", ylab = "Eigenvalues", main = "Scree Plot")

factor_scores <- factor.scores(data_var, M1)
factor_scores
print(factor_scores$scores)


#CFA, confirmatory
model <- ' 
PD =~ PD1+PD2+PD3+PD4+PD5+PD6+PD7+PD8+PD9+PD10+PHY
AP =~ AP1+AP2+AP3+AP4+AP5
WLB1N=~WLB1+WLB2+WLB3+WLB4+WLB5
WLB2N=~WLB6+WLB7+WLB8+WLB9
QL=~QL1+QL2+QL3+QL4+QL5+QL6+QL7+QL8+QL9
'
fit <-cfa(model, data=data_var)
summary(fit,fit.measures=TRUE,standardized=TRUE)
View(data_var)

#Transforming variable scales PD4, PD5, PD7, PD8
data_var$PD4n=4-data_var$PD4
data_var$PD5n=4-data_var$PD5
data_var$PD7n=4-data_var$PD7
data_var$PD8n=4-data_var$PD8
data_var$PHYn=4-data_var$PHY
View(data_var)

#run new CFA model 
model2 <- ' 
PD =~ PD1+PD2+PD3+PD4n+PD5n+PD6+PD7n+PD8n+PD9+PD10+PHYn
AP =~ AP1+AP2+AP3+AP4+AP5
WLB1N=~WLB1+WLB2+WLB3+WLB4+WLB5
WLB2N=~WLB6+WLB7+WLB8+WLB9
QL=~QL1+QL2+QL3+QL4+QL5+QL6+QL7+QL8+QL9
'
fit2 <-cfa(model2, data=data_var)
summary(fit2,fit.measures=TRUE,standardized=TRUE)

model3 <- ' 
PD =~ PD1+PD2+PD3+PD5n+PD6+PD8n+PD9+PD10+PHYn
AP =~ AP1+AP2+AP3+AP4+AP5
WLB1N=~WLB1+WLB2+WLB3+WLB4+WLB5
WLB2N=~WLB6+WLB7+WLB8+WLB9
QL=~QL1+QL2+QL3+QL4+QL5+QL6+QL7+QL8+QL9
'
fit3 <-cfa(model3, data=data_var)
summary(fit3,fit.measures=TRUE,standardized=TRUE)

#removing insignificant variables 
model4 <- ' 
PD =~ PD1+PD2+PD3+PD6+PD9+PD10
AP =~ AP3+AP4+AP5
WLB1N=~WLB1+WLB2+WLB3+WLB4
WLB2N=~WLB6+WLB7+WLB8+WLB9
QL=~QL1+QL2+QL3+QL4+QL5+QL6+QL9
'
fit4 <-cfa(model4, data=data_var)
summary(fit4,fit.measures=TRUE,standardized=TRUE)


#Creating Latent Factors through median of other variables
data_var <- data_var %>%
  mutate(PD = apply(.[c("PD1", "PD2","PD9","PD10")],1,median, na.rm = TRUE))

data_var <- data_var %>%
  mutate(AP = apply(.[c("AP3", "AP4", "AP5")],1,median, na.rm = TRUE))

data_var <- data_var %>%
  mutate(WLB1N = apply(.[c("WLB1", "WLB2", "WLB3","WLB4")],1,median, na.rm = TRUE))

data_var <- data_var %>%
  mutate(WLB2N = apply(.[c("WLB6", "WLB7", "WLB8","WLB9")],1,median, na.rm = TRUE))

data_var <- data_var %>%
  mutate(QL = apply(.[c("QL1", "QL2", "QL3","QL4","QL5","QL6","QL9")],1,median, na.rm = TRUE))

data_var$PD=as.numeric(data_var$PD)
data_var$AP=as.numeric(data_var$AP)
data_var$WLB1N=as.numeric(data_var$WLB1N)
data_var$WLB2N=as.numeric(data_var$WLB2N)
data_var$QL=as.numeric(data_var$QL)
str(data_var)

#Fitting a Ordinal Logistic Regression with Factors and Satisfaction
cfa_model <- polr(data$Satisfaction ~ data_var$PD +data_var$AP + data_var$WLB1N + data_var$WLB2N + data_var$QL, Hess=TRUE)
summary(cfa_model)

#histogram of important factors 
par(mfrow = c(1, 3))
hist(data_var$PD)
hist(data_var$WLB1N)
hist(data_var$QL)

ggplot(data, aes(x = data_var$PD, fill=data$Satisfaction)) +
  geom_histogram(binwidth = 0.5, color = "black", alpha = 0.7) +
  scale_fill_manual("Legend", values = c("blue", "red", "green", "aquamarine", "blueviolet")) +
  labs(title = "Histogram of Psychological Distress", x = "Psychological Distress", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = data_var$WLB1N, fill=data$Satisfaction)) +
  geom_histogram(binwidth = 0.5, color = "black", alpha = 0.7) +
  scale_fill_manual("Legend", values = c("blue", "red", "green", "aquamarine", "blueviolet")) +
  labs(title = "Histogram of Work-Life Balance", x = "Work-Life Balance", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = data_var$QL, fill=data$Satisfaction)) +
  geom_histogram(binwidth = 0.5, color = "black", alpha = 0.7) +
  scale_fill_manual("Legend", values = c("blue", "red", "green", "aquamarine", "blueviolet")) +
  labs(title = "Histogram of Quality of Learning", x = "Quality of Learning", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Exporting cleaned data for future use
write.csv(data, file = "4DWW_Survey_Responses_Cleaned.csv", row.names = FALSE)