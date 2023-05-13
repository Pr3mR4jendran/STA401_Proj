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

str(data)

#Converting analyzing factors into ordinal categorical variables
convert_cols <- c(7:ncol(data))
for (i in convert_cols){
  data[[i]] <- factor(data[[i]], ordered = TRUE)
}

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

#Visualizing satisfaction using a barplot
library(ggplot2)

ggplot(data, aes(x = data$Satisfaction, fill=data$Satisfaction)) +
  geom_bar() +
  scale_fill_manual("Legend", values = c("blue", "red", "green", "aquamarine", "blueviolet")) +
  labs(title = "Bar Plot of Satisfaction", x = "Satisfaction", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#we need to pre-process and extract only the numeric columns from our dataset for PCA and Factor Analysis
cont_data <- as.data.frame(lapply(data, function(x) as.integer(as.character(x))))
#removing non numeric and response variable
cont_data <- cont_data[, -c(1,2,3,4,6,41)]
View(cont_data)

correlations <- cor(cont_data)
library(corrplot)
corrplot(correlations, method="color", type="upper", order="hclust")

###PCA
pca_result <- prcomp(cont_data)
summary(pca_result)
names(pca_result)
pca_result$sdev
pca_result$rotation
pca_result$center
pca_result$scale
pca_result$x

biplot(pca_result)

library(devtools)
library(ggbiplot)
ggscreeplot(pca_result)


library(factoextra)
fviz_pca_var(pca_result, col.var = "contrib", repel = TRUE)

###Factor Analysis
library(psych)
factor_analysis <- fa(cont_data, nfactors = 10, rotate = "varimax")
summary(factor_analysis)
print(factor_analysis$loadings)

names(factor_analysis)
fa.diagram(factor_analysis)

plot(density(factor_analysis$scores, na.rm = TRUE), 
     main = "Factor Scores")

eigenvalues <- factor_analysis$values
plot(1:length(eigenvalues), eigenvalues, type = "b", 
     xlab = "Factor Number", ylab = "Eigenvalues", main = "Scree Plot")

factor_scores <- factor.scores(cont_data, factor_analysis)
print(factor_scores$scores)

#Fitting a Logistic Regression with Factors and Satisfaction
fa_model <- glm(data$Satisfaction ~ factor_scores$scores[,1] + factor_scores$scores[,2] +
               factor_scores$scores[,3] + factor_scores$scores[,4] + factor_scores$scores[,5] + 
               factor_scores$scores[,6] + factor_scores$scores[,7] +factor_scores$scores[,8] + 
               factor_scores$scores[,9] + factor_scores$scores[,10], family = binomial)
summary(fa_model)

#Fitting a Logistic Regression with PCA loadings and Satisfaction
pca_scores <- pca_result$x[,1:10]
pca_model <- glm(data$Satisfaction ~ pca_scores[,1] + pca_scores[,2] + pca_scores[,3] + pca_scores[,4] + 
               pca_scores[,5] + pca_scores[,6] + pca_scores[,7] + pca_scores[,8] + pca_scores[,9] + 
               pca_scores[,10], family=binomial)
summary(pca_model)

#Exporting cleaned data for future use
write.csv(data, file = "4DWW_Survey_Responses_Cleaned.csv", row.names = FALSE)