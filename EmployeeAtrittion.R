# Medha Singh, Shivam Shishangia, Vidisha Mittal, Varun Kapuria
# MIS 545 Section 01
# ProjectGroup07ShishangiaKapuriaSinghMittal.R
# TODO: Add description.

# Install the packages
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("smotefamily")
# install.packages("caret")
# install.packages("e1071")
# install.packages("rpart")
# install.packages("neuralnet")
# install.packages("class")

# Load the packages
library(tidyverse)
library(corrplot)
library(olsrr)
library(smotefamily)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(neuralnet)
library(class)

# Set the working directory
setwd("C:/Users/ual-laptop/Desktop/MIS545/Group Project/MIS545")

# Load the values from .csv file
originalEmployeeRetention <- read_csv(file = "ER2.csv",
                              col_types = "nffnfnnfnnnfnnnfnfnnnffnnnnnnnnnnnn",
                              col_names = TRUE)
employeeRetention <- originalEmployeeRetention

# Recode column values
employeeRetention$Attrition <- recode(employeeRetention$Attrition, 
                                      "No" = 0, 
                                      "Yes" = 1)
employeeRetention$BusinessTravel <- recode(employeeRetention$BusinessTravel, 
                                           "Travel_Rarely" = 1,
                                           "Travel_Frequently" = 2,
                                           "Non-Travel" = 3)
employeeRetention$Department <- recode(employeeRetention$Department, 
                                       "Research & Development" = 1,
                                       "Human Resources" = 2,
                                       "Sales" = 3)
employeeRetention$EducationField <- recode(employeeRetention$EducationField,
                                           "Life Sciences" = 1,
                                           "Medical" = 2,
                                           "Marketing" = 3,
                                           "Technical Degree" = 4,
                                           "Human Resources" = 5,
                                           "Other" = 6)
employeeRetention$Gender <- recode(employeeRetention$Gender,
                                   "Male" = 1,
                                   "Female" = 2)
employeeRetention$JobRole <- recode(employeeRetention$JobRole,
                                    "Sales Executive" = 1,
                                    "Research Scientist" = 2,
                                    "Laboratory Technician" = 3,    
                                    "Manufacturing Director" = 4, 
                                    "Healthcare Representative" = 5, 
                                    "Manager" = 6,      
                                    "Sales Representative" = 7,         
                                    "Research Director" = 8,           
                                    "Human Resources" = 9)
employeeRetention$MaritalStatus <- recode(employeeRetention$MaritalStatus,
                                          "Single" = 1, 
                                          "Married" = 2,
                                          "Divorced"= 3)
employeeRetention$OverTime <- recode(employeeRetention$OverTime, 
                                     "No" = 1, 
                                     "Yes" = 2)

# Convert dependent variable to logical
employeeRetention <- employeeRetention %>%
    mutate(Attrition = as.logical(Attrition))

# Query 1
query1 <- employeeRetention %>%
  select(Gender,
         Attrition)%>%
         filter(Attrition == 'Yes') %>%
         count(Gender)
percGenderAttrition <- (query1[2][1]/sum(query1[2]))*100
print(percGenderAttrition)

#perform data binning on points variable
employeeRetentionBin <- originalEmployeeRetention %>%
  mutate(age_bin = cut(Age, breaks=c(0, 18, 24, 30, 36, 42, 48, 54, 60)))
query2 <- employeeRetentionBin %>%
  select(age_bin,
         Attrition)%>%
  filter(employeeRetentionBin, Attrition == 'Yes')%>%
  count(age_bin)

print(query2)

# Query 3
query3 <- employeeRetention %>%
  select(Department,
         Attrition)%>%
  filter(Attrition == 'Yes')%>%
  count(Department)

# Display employeeRetention in the console
print(employeeRetention)

# Display the structure of employeeRetention in the console
print(str(employeeRetention))

# Display the summary of employeeRetention in the console
print(summary(employeeRetention))

# Remove Irrelevant columns
employeeRetention <- select(employeeRetention, -c('Over18', 
                                                  'StandardHours', 
                                                  'EmployeeCount', 
                                                  'EmployeeNumber'))

# Creating the displayAllHistograms() function
displayAllHistograms <- function(tibbleDataset) {
    tibbleDataset %>%
        keep(is.numeric) %>%
        gather() %>%
        ggplot() + geom_histogram(mapping = aes(x=value,fill=key),
                                  color="black") + 
        facet_wrap(~ key, scales="free") +
        theme_minimal()
}

# Call the displayAllHistograms() function by passing the data tibble
displayAllHistograms(employeeRetention)

# Normalize the dataset
employeeRetention <- employeeRetention %>%
    mutate(Age = log(Age),
           BusinessTravel = log(BusinessTravel),
           DailyRate = log(DailyRate),
           Department = log(Department),
           DistanceFromHome = log(DistanceFromHome),
           Education = log(Education),
           EducationField = log(EducationField),
           EnvironmentSatisfaction = log(EnvironmentSatisfaction),
           Gender = log(Gender),
           HourlyRate = log(HourlyRate),
           JobInvolvement = log(JobInvolvement),
           JobLevel = log(JobLevel),
           JobRole = log(JobRole),
           JobSatisfaction = log(JobSatisfaction),
           MaritalStatus = log(MaritalStatus),
           MonthlyIncome = log(MonthlyIncome),
           MonthlyRate = log(MonthlyRate),
           OverTime = log(OverTime),
           PercentSalaryHike = log(PercentSalaryHike), 
           PerformanceRating = log(PerformanceRating),
           RelationshipSatisfaction = log(RelationshipSatisfaction),
           WorkLifeBalance = log(WorkLifeBalance))

# Display a correlation matrix rounded to two decimal places
employeeRetentionCorrelationMatrix <- cor(employeeRetention)
print(round(employeeRetentionCorrelationMatrix, 2))

# Display a correlation plot and limit output to the bottom left
corrplot(cor(employeeRetention),
         method = "circle",
         type = "lower")

# Drop columns highly correlated to each other, cutoff = 0.6
drop = findCorrelation(employeeRetentionCorrelationMatrix, cutoff = 0.6)
drop = names(employeeRetention)[drop]
print(drop)
employeeRetention <- select(employeeRetention, -c(all_of(drop)))

# Display a correlation matrix rounded to two decimal places
employeeRetentionCorrelationMatrix <- cor(employeeRetention)
print(round(employeeRetentionCorrelationMatrix, 2))

# Display a correlation plot and limit output to the bottom left
corrplot(cor(employeeRetention),
         method = "number",
         type = "lower")

# Set seed to 545
set.seed(545)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSet <- sample(nrow(employeeRetention),
                    round(nrow(employeeRetention) * 0.75),
                    replace = FALSE)

# Split the dataset 
employeeRetentionTraining <- employeeRetention[sampleSet, ]
employeeRetentionTesting <- employeeRetention[-sampleSet, ]

# Check if there is a class imbalance
summary(employeeRetentionTraining$Attrition)

# Print the magnitude of class imbalance
print(923/179)

# Deal with class imbalance
employeeRetentionTrainingSmoted <- 
    tibble(SMOTE(X = data.frame(employeeRetentionTraining),
                 target = employeeRetentionTraining$Attrition,
                 dup_size = 5)$data)

# Convert Attrition back into logical types
employeeRetentionTrainingSmoted <- employeeRetentionTrainingSmoted %>%
    mutate(Attrition = as.logical(Attrition))

# Get rid of the "class" column in the tibble 
employeeRetentionTrainingSmoted <- employeeRetentionTrainingSmoted %>%
    select(-class)

# Check for class imbalance on the smoted dataset
summary(employeeRetentionTrainingSmoted$Attrition)

# Generate the logistic regression model
employeeRetentionLogisticModel <- glm(data = employeeRetentionTrainingSmoted,
                                      family = binomial,
                                      formula = Attrition ~ .)

# Display the logistic regression model results using the summary() function
summary(employeeRetentionLogisticModel)

# Calculate the odds ratios for each independent variable coefficients
exp(coef(employeeRetentionLogisticModel)["Age"])
exp(coef(employeeRetentionLogisticModel)["BusinessTravel"])
exp(coef(employeeRetentionLogisticModel)["DailyRate"])
exp(coef(employeeRetentionLogisticModel)["Department"])
exp(coef(employeeRetentionLogisticModel)["DistanceFromHome"])
exp(coef(employeeRetentionLogisticModel)["Education"])
exp(coef(employeeRetentionLogisticModel)["EducationField"])
exp(coef(employeeRetentionLogisticModel)["EnvironmentSatisfaction"])
exp(coef(employeeRetentionLogisticModel)["Gender"])
exp(coef(employeeRetentionLogisticModel)["HourlyRate"])
exp(coef(employeeRetentionLogisticModel)["JobInvolvement"])
exp(coef(employeeRetentionLogisticModel)["JobLevel"])
exp(coef(employeeRetentionLogisticModel)["JobRole"])
exp(coef(employeeRetentionLogisticModel)["JobSatisfaction"])
exp(coef(employeeRetentionLogisticModel)["MonthlyRate"])
exp(coef(employeeRetentionLogisticModel)["NumCompaniesWorked"])
exp(coef(employeeRetentionLogisticModel)["OverTime"])
exp(coef(employeeRetentionLogisticModel)["PerformanceRating"])
exp(coef(employeeRetentionLogisticModel)["RelationshipSatisfaction"])
exp(coef(employeeRetentionLogisticModel)["StockOptionLevel"])
exp(coef(employeeRetentionLogisticModel)["TotalWorkingYears"])
exp(coef(employeeRetentionLogisticModel)["TrainingTimesLastYear"])
exp(coef(employeeRetentionLogisticModel)["WorkLifeBalance"])
exp(coef(employeeRetentionLogisticModel)["YearsSinceLastPromotion"])
exp(coef(employeeRetentionLogisticModel)["YearsWithCurrManager"])

# Use the model to predict the outcomes of the testing dataset
employeeRetentionLogisticPrediction <- predict(employeeRetentionLogisticModel,
                                               employeeRetentionTesting,
                                               type = "response")

# Display employeeRetentionLogisticPrediction on console
print(employeeRetentionLogisticPrediction)

# Treat anything above 0.5 as TRUE and below or equal to 0.5 as FALSE
employeeRetentionLogisticPrediction <- ifelse(
    employeeRetentionLogisticPrediction > 0.5, TRUE, FALSE)

# Display employeeRetentionLogisticPrediction on console
print(employeeRetentionLogisticPrediction)

# Create confusion matrix
employeeRetentionLogisticConfusionMatrix <- table(
    employeeRetentionTesting$Attrition,
    employeeRetentionLogisticPrediction)

# Creating the displayModelAnalysis() function
displayModelAnalysis <- function(modelConfusionMatrix) {
    # Display the confusion matrix
    print("Confusion Matrix: ")
    print(modelConfusionMatrix)
    
    # Calculate false positive rate
    print(paste0("False Positive Rate: ", modelConfusionMatrix[1,2] / 
        (modelConfusionMatrix[1,1] + modelConfusionMatrix[1,2])))
    
    # Calculate false negative rate
    print(paste0("False Negative Rate: ", modelConfusionMatrix[2,1] / 
        (modelConfusionMatrix[2,1] + modelConfusionMatrix[2,2])))
    
    # Calculate model prediction accuracy
    confusionMatrixAccuracy <- sum(diag(modelConfusionMatrix)) / 
        sum(modelConfusionMatrix)
    
    # Print the model prediction accuracy
    print(paste0("Model Accuracy: ", confusionMatrixAccuracy))
}

# Call the displayModelAnalysis() function by passing the confusion matrix
displayModelAnalysis(employeeRetentionLogisticConfusionMatrix)

# Generate the Naive Bayes model
employeeRetentionBayesModel <- naiveBayes(
    formula = Attrition ~ .,
    data = employeeRetentionTrainingSmoted,
    laplace = 1)

# Build probabilities for each record in the testing dataset
employeeRetentionBayesProbability <- predict(employeeRetentionBayesModel,
                                             employeeRetentionTesting,
                                             type = "raw")

# Display employeeRetentionBayesProbability on the console
print(employeeRetentionBayesProbability)

# Predict classes for each record in the testing dataset
employeeRetentionBayesPrediction <- predict(employeeRetentionBayesModel,
                                            employeeRetentionTesting,
                                            type = "class")

# Display employeeRetentionBayesPrediction on the console
print(employeeRetentionBayesPrediction)

# Evaluate the model by forming a confusion matrix
employeeRetentionBayesConfusionMatrix <- table(
    employeeRetentionTesting$Attrition,
    employeeRetentionBayesPrediction)

# Call the displayModelAnalysis() function by passing the confusion matrix
displayModelAnalysis(employeeRetentionBayesConfusionMatrix)

# Generate the default decision tree model
employeeRetentionDefaultDecisionTreeModel <- rpart(
    formula = Attrition ~.,
    method = "class",
    data = employeeRetentionTrainingSmoted)

# Display the decision tree visualization in R
rpart.plot(employeeRetentionDefaultDecisionTreeModel)

# Predict classes for each record in the testing dataset
employeeRetentionDefaultDecisionTreePrediction <- predict(
    employeeRetentionDefaultDecisionTreeModel,
    employeeRetentionTesting,
    type = "class")

# Display employeeRetentionDefaultDecisionTreePrediction on the console
print(employeeRetentionDefaultDecisionTreePrediction)

# Evaluate the model by forming a confusion matrix
employeeRetentionDefaultDecisionTreeConfusionMatrix <- table(
    employeeRetentionTesting$Attrition,
    employeeRetentionDefaultDecisionTreePrediction)

# Call the displayModelAnalysis() function by passing the confusion matrix
displayModelAnalysis(employeeRetentionDefaultDecisionTreeConfusionMatrix)

# Generate the decision tree model with cp = 0.007
employeeRetentionDecisionTreeModel <- rpart(
    formula = Attrition ~.,
    method = "class",
    cp = 0.007,
    data = employeeRetentionTrainingSmoted)

# Display the decision tree visualization in R
rpart.plot(employeeRetentionDecisionTreeModel)

# Predict classes for each record in the testing dataset
employeeRetentionDecisionTreePrediction <- predict(
    employeeRetentionDecisionTreeModel,
    employeeRetentionTesting,
    type = "class")

# Display employeeRetentionDecisionTreePrediction on the console
print(employeeRetentionDecisionTreePrediction)

# Evaluate the model by forming a confusion matrix
employeeRetentionDecisionTreeConfusionMatrix <- table(
    employeeRetentionTesting$Attrition,
    employeeRetentionDecisionTreePrediction)

# Call the displayModelAnalysis() function by passing the confusion matrix
displayModelAnalysis(employeeRetentionDecisionTreeConfusionMatrix)

# Generate the neural network model
employeeRetentionNeuralNetModel <- neuralnet(
    formula = Attrition ~ .,
    data = employeeRetentionTrainingSmoted,
    hidden = 3,
    act.fct = "logistic",
    linear.output = FALSE,
    stepmax = 1e7)

# Display the neural network numeric results
print(employeeRetentionNeuralNetModel$result.matrix)

# Visualize the neural network
plot(employeeRetentionNeuralNetModel)

# Generate probabilities on employeeRetentionTesting
employeeRetentionNeuralNetProbability <- compute(
    employeeRetentionNeuralNetModel,
    employeeRetentionTesting)

# Display the probabilities from the testing dataset on the console
print(employeeRetentionNeuralNetProbability$net.result)

# Convert probability predictions into TRUE/FALSE predictions
employeeRetentionNeuralNetPrediction <- 
    ifelse(employeeRetentionNeuralNetProbability$net.result > 0.5, TRUE, FALSE)

# Display the TRUE/FALSE predictions on the console
print(employeeRetentionNeuralNetPrediction)

# Evaluate the model by forming a confusion matrix
employeeRetentionNeuralNetConfusionMatrix <- table(
    employeeRetentionTesting$Attrition,
    employeeRetentionNeuralNetPrediction)

# Call the displayModelAnalysis() function by passing the confusion matrix
displayModelAnalysis(employeeRetentionNeuralNetConfusionMatrix)

# Separating the tibble into two. One with just the label 
# and one with the other variables.
employeeRetentionTrainingKNNLabels <- employeeRetentionTrainingSmoted %>%
    select(Attrition)
employeeRetentionTrainingKNN <- employeeRetentionTrainingSmoted %>%
    select(-Attrition)

employeeRetentionTestingKNNLabels <- employeeRetentionTesting %>%
    select(Attrition)
employeeRetentionTestingKNN <- employeeRetentionTesting %>%
    select(-Attrition)

# Generate the KNN model
employeeRetentionKNNPrediction <- knn(
    train = employeeRetentionTrainingKNN,
    test = employeeRetentionTestingKNN,
    cl = employeeRetentionTrainingKNNLabels$Attrition,
    k = 33)

# Displaying the predictions from the testing dataset on the console
print(employeeRetentionKNNPrediction)

# Displaying summary of the predictions from the testing dataset
summary(employeeRetentionKNNPrediction)

# Evaluate the model by forming a confusion matrix
employeeRetentionKNNConfusionMatrix <- table(
    employeeRetentionTestingKNNLabels$Attrition,
    employeeRetentionKNNPrediction)

# Call the displayModelAnalysis() function by passing the confusion matrix
displayModelAnalysis(employeeRetentionKNNConfusionMatrix)

# Create a matrix of k-values with their predictive accuracy
kValueMatrix <- matrix(data = NA,
                       nrow = 0,
                       ncol = 2)

# Assign column names of "k value" and "Predictive accuracy" to kValueMatrix
colnames(kValueMatrix) <- c("k value", "Predictive Accuracy")

# Loop through odd values of k from 1 up to the number of records in the 
# training dataset. With each pass through the loop, store the k-value along 
# with its predictive accuracy.
for (kValue in 1:nrow(employeeRetentionTrainingKNNLabels)) {
    # Only calculate model accuracy for odd k-value
    if (kValue %% 2 == 0) {
        next
    }
    
    # Generate the KNN model
    employeeRetentionKNNPrediction <- knn(
        train = employeeRetentionTrainingKNN,
        test = employeeRetentionTestingKNN,
        cl = employeeRetentionTrainingKNNLabels$Attrition,
        k = kValue)
    
    # Evaluate the model by forming a confusion matrix
    employeeRetentionKNNConfusionMatrix <- table(
        employeeRetentionTestingKNNLabels$Attrition,
        employeeRetentionKNNPrediction)
    
    # Calculate the model predictive accuracy
    predictiveAccuracy <- sum(diag(employeeRetentionKNNConfusionMatrix)) / 
        nrow(employeeRetentionTesting)
    
    # Add a new row to the matrix
    kValueMatrix <- rbind(kValueMatrix, c(kValue, predictiveAccuracy))
}

# Display the kValueMatrix on the console
print(kValueMatrix)

# Generate the KNN model
employeeRetentionKNNPrediction <- knn(
    train = employeeRetentionTrainingKNN,
    test = employeeRetentionTestingKNN,
    cl = employeeRetentionTrainingKNNLabels$Attrition,
    k = 1)

# Displaying the predictions from the testing dataset on the console
print(employeeRetentionKNNPrediction)

# Displaying summary of the predictions from the testing dataset
summary(employeeRetentionKNNPrediction)

# Evaluate the model by forming a confusion matrix
employeeRetentionKNNConfusionMatrix <- table(
    employeeRetentionTestingKNNLabels$Attrition,
    employeeRetentionKNNPrediction)

# Call the displayModelAnalysis() function by passing the confusion matrix
displayModelAnalysis(employeeRetentionKNNConfusionMatrix)

