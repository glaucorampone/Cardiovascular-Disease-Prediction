# Section 1: Set up and load data
# ------------------------------------------

# Read the CSV file into a dataset
dataset <- read.csv("Cardiovascular_Disease_Dataset.csv")

# Display the first few rows of the dataset
head(dataset)


# Section 2: Data Preprocessing
# ------------------------------------------

# Convert 'gender' to a factor with meaningful labels
dataset$gender <- factor(dataset$gender, labels = c("female", "male"))

# Convert 'chestpain' to a factor with meaningful labels
dataset$chestpain <- factor(
  dataset$chestpain,
  labels = c("typical angina", "atypical angina", "non-anginal pain", "asymptomatic")
)

# Convert 'fastingbloodsugar' to a factor with meaningful labels
dataset$fastingbloodsugar <- factor(dataset$fastingbloodsugar, labels = c("False", "True"))

# Convert 'restingrelectro' to a factor with meaningful labels
dataset$restingrelectro <- factor(
  dataset$restingrelectro,
  labels = c("normal", "ST-T wave anorm", "left v. hypertrophy")
)

# Convert 'exerciseangia' to a factor with meaningful labels
dataset$exerciseangia <- factor(dataset$exerciseangia, labels = c("no", "yes"))

# Convert 'slope' to a factor with meaningful labels
dataset$slope <- factor(dataset$slope, labels = c("NA", "upsloping", "flat", "downsloping"))

# Display the 'slope' variable
dataset$slope


# Section 3: Model Building
# ------------------------------------------

# Build a logistic regression model (glm) with all variables except 'patientid'
full_model <- glm(target ~ . - patientid, dataset, family = "binomial")

# Display summary of the full model
summary(full_model)


# Section 4: Model Selection
# ------------------------------------------

# Load the MASS library for stepwise selection
library(MASS)

# Perform stepwise AIC model selection
selected_model <- stepAIC(full_model, direction = "both", trace = FALSE)

# Display summary of the selected model
summary(selected_model)
