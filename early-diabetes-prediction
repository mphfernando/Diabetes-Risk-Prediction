library(tidyverse)
library(corrplot) #to get the correlation
dataset<-read.csv("C:\\Users\\mphfe\\Downloads\\early+stage+diabetes+risk+prediction+dataset\\diabetes_data_upload.csv")
str(dataset)
view(dataset)

#preprocessing
#checking missing data
sum(is.na(dataset))
colSums(is.na(dataset))
View(colSums(is.na(dataset)))

#converting age column to numeric
dataset$Age <- as.numeric(dataset$Age)
view(dataset)

#encoding categorical variables to numerical
#1.using label encoding
dataset$Gender <- as.numeric(factor(dataset$Gender))
dataset$Polyuria <- as.numeric(dataset$Polyuria =="Yes")
dataset$Polydipsia <- as.numeric(dataset$Polydipsia =="Yes")

dataset$weakness <- as.numeric(dataset$weakness =="Yes")
dataset$Polyphagia <- as.numeric(dataset$Polyphagia =="Yes")
dataset$Genital.thrush <- as.numeric(dataset$Genital.thrush =="Yes")
dataset$visual.blurring <- as.numeric(dataset$visual.blurring =="Yes")
dataset$sudden.weight.loss <- as.numeric(dataset$sudden.weight.loss =="Yes")
dataset$Irritability <- as.numeric(dataset$Irritability =="Yes")
dataset$delayed.healing <- as.numeric(dataset$delayed.healing =="Yes")
dataset$partial.paresis <- as.numeric(dataset$partial.paresis =="Yes")
dataset$muscle.stiffness <- as.numeric(dataset$muscle.stiffness =="Yes")
dataset$Alopecia <- as.numeric(dataset$Alopecia =="Yes")
dataset$Obesity <- as.numeric(dataset$Obesity =="Yes")
dataset$Itching<- as.numeric(dataset$Itching =="Yes")
dataset$class <- as.numeric(dataset$class == "Positive")
view(dataset)

#correlation with each column
round(cor(dataset),
      digits = 2)

#corelation only with class
round(cor(dataset[-which(names(dataset) == "class")], dataset$class), digits = 2)


#Class Distribution
class_distribution <- table(data$class)
print(class_distribution)
prop.table(class_distribution)
barplot(class_distribution,data=dataset,xlab="Diabetes status",ylab ="number of people",col="red")

#Feature Distribution
# For categorical variables
categorical_vars <- c("Gender", "Polyuria", "Polydipsia", "sudden.weight.loss", "weakness", 
                      "Polyphagia", "Genital.thrush", "visual.blurring", "Itching", 
                      "Irritability", "delayed.healing", "partial.paresis", 
                      "muscle.stiffness", "Alopecia", "Obesity")

for(var in categorical_vars) {
  table<-(table(dataset[[var]]))
  cat("Table for", var, ":")
  print(table)
  cat("\n")
}

#Age distribution
hist(dataset$Age, main="Distribution of Age", xlab="Age")


#visualizations
#1correlatio heat map
library(corrplot)
       
# Compute correlation matrix
cor_matrix <- cor(dataset, use = "pairwise.complete.obs")
       
# Create correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
    tl.col = "black", tl.srt = 45, addCoef.col = "black", 
    number.cex = 0.7, tl.cex = 0.7,
    title = "Correlation Heatmap of Diabetes Data", 
    mar = c(0,0,1,0))
       
#2Create the scatter plot Age vs gender
ggplot(data, aes(x = Age, y = Gender, color = class)) +
    geom_jitter(width = 0, height = 0.2, alpha = 0.7, size = 3) +
    scale_color_manual(values = c("Positive" = "red", "Negative" = "blue")) +
    labs(title = "Age and Gender Distribution by Diabetes Class",
              x = "Age",
              y = "Gender",
              color = "Diabetes Class") +
         theme_minimal() +
         theme(legend.position = "bottom")


#3Polyuria vs polydipsia with class
# Create the data frame
dataP <- data.frame(
  Symptoms = c("Polyuria Yes", "Polyuria Yes", "Polyuria No", "Polyuria No",
               "Polydipsia Yes", "Polydipsia Yes", "Polydipsia No", "Polydipsia No"),
  Class = rep(c("Positive", "Negative"), 4),
  Count = c(103, 7, 67, 143,
            101, 8, 69, 142)
)
view(dataP)

# Create the barplot
ggplot(dataP, aes(x = Symptoms, y = Count, fill = Class)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Polyuria and Polydipsia vs Class",
       x = "Symptoms",
       y = "Count") +
  scale_fill_manual(values = c("Positive" = "#1E90FF", "Negative" = "#FF6347")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")




#classification
library(rpart) #to train decision tree classifier
library(rpart.plot) #to plot the decision tree
library(caTools) #to split the dataset

# Split data into training and testing sets
set.seed(125)
split<-sample.split(dataset,SplitRatio = )
train_data<-subset(dataset,split==TRUE)
test_data<-subset(dataset,split==FALSE)
dim(train_data)
dim(test_data)

# Train decision tree model
dt_classifier <- rpart(class ~ ., data = train_data, method = "class")

# Visualize decision tree
rpart.plot(dt_classifier, extra = 100, box.palette = "RdYlGn", shadow.col = "gray")

# Make predictions on test data
dt_predictions <- predict(dt_classifier, test_data, type = "class")

# Create confusion matrix
conf_matrix <- table(Predicted = dt_predictions, Actual = test_data$class)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", round(accuracy, 4)))

# Calculate precision, recall, and F1 score for positive class
precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])

print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))




#clustering
set.seed(100)

for(i in 1:10) {
  results <- kmeans(dataset, center=i, nstart = 20)
  tot.withinss[i] <- results$tot.withinss
}

plot(1:10, tot.withinss, type = "b", pch=20)

library(cluster)
results <- kmeans(dataset[, c("Polydipsia", "sudden.weight.loss", "partial.paresis", "Polyuria", "Gender","class")], centers = 3, nstart = 20)
results
# Plot the clusters
clusplot(dataset[, c("Polydipsia", "sudden.weight.loss", "partial.paresis", "Polyuria", "Gender","class")], 
         results$cluster, 
         color = TRUE, 
         labels = 0, 
         lines = 0, 
         shade = TRUE, 
         span = TRUE,
         col.clus = c("red", "blue", "green"))


