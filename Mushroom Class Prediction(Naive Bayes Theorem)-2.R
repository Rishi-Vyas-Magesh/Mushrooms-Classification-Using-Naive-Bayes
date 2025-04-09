library(tidyverse)
library(readxl)
library(ggplot2)
library(olsrr)
library(jtools)
library(Metrics)
library(psych)
library(car)
library (caret)
library (e1071)
library(dplyr)


getwd()
# Updating with my file path
setwd("C:/Users/abmag/OneDrive/Documents/predictive 2/Project")
#New Working Directory
getwd()
# Loading the dataset to R
data <- read.csv("mushrooms.csv", stringsAsFactors = FALSE)
View(data)
# Applying transformations to columns using "case_when" for each column data
# Encoding
data <- data %>%
  mutate(
    `class` = case_when(`class` == "e" ~ "edible", `class` == "p" ~ "poisonous"),
    `cap.shape` = case_when(
      `cap.shape` == "b" ~ "bell",
      `cap.shape` == "c" ~ "conical",
      `cap.shape` == "x" ~ "convex",
      `cap.shape` == "f" ~ "flat",
      `cap.shape` == "k" ~ "knobbed",
      `cap.shape` == "s" ~ "sunken"
    ),
    `cap.surface` = case_when(
      `cap.surface` == "f" ~ "fibrous",
      `cap.surface` == "g" ~ "grooves",
      `cap.surface` == "y" ~ "scaly",
      `cap.surface` == "s" ~ "smooth"
    ),
    `cap.color` = case_when(
      `cap.color` == "n" ~ "brown",
      `cap.color` == "b" ~ "buff",
      `cap.color` == "c" ~ "cinnamon",
      `cap.color` == "g" ~ "gray",
      `cap.color` == "r" ~ "green",
      `cap.color` == "p" ~ "pink",
      `cap.color` == "u" ~ "purple",
      `cap.color` == "e" ~ "red",
      `cap.color` == "w" ~ "white",
      `cap.color` == "y" ~ "yellow"
    ),
    `bruises` = case_when(
      `bruises` == "t" ~ "bruises",
      `bruises` == "f" ~ "no"
    ),
    `odor` = case_when(
      `odor` == "a" ~ "almond",
      `odor` == "l" ~ "anise",
      `odor` == "c" ~ "creosote",
      `odor` == "y" ~ "fishy",
      `odor` == "f" ~ "foul",
      `odor` == "m" ~ "musty",
      `odor` == "n" ~ "none",
      `odor` == "p" ~ "pungent",
      `odor` == "s" ~ "spicy"
    ),
    `gill.attachment` = case_when(
      `gill.attachment` == "a" ~ "attached",
      `gill.attachment` == "d" ~ "descending",
      `gill.attachment` == "f" ~ "free",
      `gill.attachment` == "n" ~ "notched"
    ),
    `gill.spacing` = case_when(
      `gill.spacing` == "c" ~ "close",
      `gill.spacing` == "w" ~ "crowded",
      `gill.spacing` == "d" ~ "distant"
    ),
    `gill.size` = case_when(
      `gill.size` == "b" ~ "broad",
      `gill.size` == "n" ~ "narrow"
    ),
    `gill.color` = case_when(
      `gill.color` == "k" ~ "black",
      `gill.color` == "n" ~ "brown",
      `gill.color` == "b" ~ "buff",
      `gill.color` == "h" ~ "chocolate",
      `gill.color` == "g" ~ "gray",
      `gill.color` == "r" ~ "green",
      `gill.color` == "o" ~ "orange",
      `gill.color` == "p" ~ "pink",
      `gill.color` == "u" ~ "purple",
      `gill.color` == "e" ~ "red",
      `gill.color` == "w" ~ "white",
      `gill.color` == "y" ~ "yellow"
    ),
    `stalk.shape` = case_when(
      `stalk.shape` == "e" ~ "enlarging",
      `stalk.shape` == "t" ~ "tapering"
    ),
    `stalk.root` = case_when(
      `stalk.root` == "b" ~ "bulbous",
      `stalk.root` == "c" ~ "club",
      `stalk.root` == "u" ~ "cup",
      `stalk.root` == "e" ~ "equal",
      `stalk.root` == "z" ~ "rhizomorphs",
      `stalk.root` == "r" ~ "rooted",
      `stalk.root` == "?" ~ "missing"
    ),
    `stalk.surface.above.ring` = case_when(
      `stalk.surface.above.ring` == "f" ~ "fibrous",
      `stalk.surface.above.ring` == "y" ~ "scaly",
      `stalk.surface.above.ring` == "k" ~ "silky",
      `stalk.surface.above.ring` == "s" ~ "smooth"
    ),
    `stalk.surface.below.ring` = case_when(
      `stalk.surface.below.ring` == "f" ~ "fibrous",
      `stalk.surface.below.ring` == "y" ~ "scaly",
      `stalk.surface.below.ring` == "k" ~ "silky",
      `stalk.surface.below.ring` == "s" ~ "smooth"
    ),
    `stalk.color.above.ring` = case_when(
      `stalk.color.above.ring` == "n" ~ "brown",
      `stalk.color.above.ring` == "b" ~ "buff",
      `stalk.color.above.ring` == "c" ~ "cinnamon",
      `stalk.color.above.ring` == "g" ~ "gray",
      `stalk.color.above.ring` == "o" ~ "orange",
      `stalk.color.above.ring` == "p" ~ "pink",
      `stalk.color.above.ring` == "e" ~ "red",
      `stalk.color.above.ring` == "w" ~ "white",
      `stalk.color.above.ring` == "y" ~ "yellow"
    ),
    `stalk.color.below.ring` = case_when(
      `stalk.color.below.ring` == "n" ~ "brown",
      `stalk.color.below.ring` == "b" ~ "buff",
      `stalk.color.below.ring` == "c" ~ "cinnamon",
      `stalk.color.below.ring` == "g" ~ "gray",
      `stalk.color.below.ring` == "o" ~ "orange",
      `stalk.color.below.ring` == "p" ~ "pink",
      `stalk.color.below.ring` == "e" ~ "red",
      `stalk.color.below.ring` == "w" ~ "white",
      `stalk.color.below.ring` == "y" ~ "yellow"
    ),
    `veil.type` = case_when(
      `veil.type` == "p" ~ "partial",
      `veil.type` == "u" ~ "universal"
    ),
    `veil.color` = case_when(
      `veil.color` == "n" ~ "brown",
      `veil.color` == "o" ~ "orange",
      `veil.color` == "w" ~ "white",
      `veil.color` == "y" ~ "yellow"
    ),
    `ring.number` = case_when(
      `ring.number` == "n" ~ "none",
      `ring.number` == "o" ~ "one",
      `ring.number` == "t" ~ "two"
    ),
    `ring.type` = case_when(
      `ring.type` == "c" ~ "cobwebby",
      `ring.type` == "e" ~ "evanescent",
      `ring.type` == "f" ~ "flaring",
      `ring.type` == "l" ~ "large",
      `ring.type` == "n" ~ "none",
      `ring.type` == "p" ~ "pendant",
      `ring.type` == "s" ~ "sheathing",
      `ring.type` == "z" ~ "zone"
    ),
    `spore.print.color` = case_when(
      `spore.print.color` == "k" ~ "black",
      `spore.print.color` == "n" ~ "brown",
      `spore.print.color` == "b" ~ "buff",
      `spore.print.color` == "h" ~ "chocolate",
      `spore.print.color` == "r" ~ "green",
      `spore.print.color` == "o" ~ "orange",
      `spore.print.color` == "u" ~ "purple",
      `spore.print.color` == "w" ~ "white",
      `spore.print.color` == "y" ~ "yellow"
    ),
    `population` = case_when(
      `population` == "a" ~ "abundant",
      `population` == "c" ~ "clustered",
      `population` == "n" ~ "numerous",
      `population` == "s" ~ "scattered",
      `population` == "v" ~ "several",
      `population` == "y" ~ "solitary"
    ),
    `habitat` = case_when(
      `habitat` == "g" ~ "grasses",
      `habitat` == "l" ~ "leaves",
      `habitat` == "m" ~ "meadows",
      `habitat` == "p" ~ "paths",
      `habitat` == "u" ~ "urban",
      `habitat` == "w" ~ "waste",
      `habitat` == "d" ~ "woods"
    )
  )
   

# Saving the modified dataset to new excel file name
output_file_path <- "C:/Users/abmag/OneDrive/Documents/predictive 2/Project/mushrooms_modified.csv" # Update with your desired save location
write.csv(data, output_file_path, row.names = FALSE)


#Loading the Modified Data set in R and saving into df
mushroomData <- read.csv("mushrooms_modified.csv")


# Inspecting the data for any losses
str(mushroomData)
View(mushroomData)
attach(mushroomData)
# Converting all independent variables to factors with corresponding levels they have
mushroomData <- mushroomData %>%       ##Using the pipeline operator
  mutate(across(-class, as.factor))
str(mushroomData)

# Ensure the target variable ("class") is a factor
mushroomData$class <- factor(mushroomData$class, levels = c("poisonous", "edible"))
str(mushroomData)

#Now Even the predicted variable is a factor with two levels
#For now there are no irrelevant columns i need to drop like S.no etc which can't be made factors

#correlation matrix among independent variables
# pairs.panels(mushroomData[-c(1)])

# For now, there are no irrelevant columns to drop like S.no etc which can't be made factors

# Split the data into training (70%) and testing (30%) sets
set.seed(123)  # Set seed for reproducibility
trainIndex = createDataPartition(mushroomData$class, p = 0.7, list = FALSE)
trainData  =  mushroomData[trainIndex, ]
testData =  mushroomData[-trainIndex, ]
dim(trainData)
dim(testData)

# Creating a Naive Bayes Model using only the training data
model2_train = naiveBayes(class ~ ., data = trainData)
model2_train

# Make predictions on the test data using the model trained on trainData
predictions  =  predict(model2_train, testData)

# Evaluate the model performance
confusionMatrix1  =  confusionMatrix(predictions, testData$class)
print(confusionMatrix1)
# Gives us accuracy, confusion matrix, sensitivity, specificity, etc.
# We get an accuracy of 94%, which indicates a very good model.

# Mis-classification rate = 1- ACCURACY
# Mis-classification Rate = Total Number of Predictions / Number of Incorrect Predictions

misclassificationRate  = 1 - sum(diag(confusionMatrix1$table)) / sum(confusionMatrix1$table)
print(paste("Misclassification Rate:", misclassificationRate))

#answer roughly 6.11 percent of error/mis-classification only


