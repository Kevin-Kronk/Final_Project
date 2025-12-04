library(plumber)
library(tidymodels)
library(tidyverse)
library(yardstick)
library(DescTools)

# Load in Diabetes Health Indicators Dataset
diabetes_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv",
                          show_col_types = FALSE)

# Select only the variables used for my analysis and convert binary to factor
analysis_variables <- c("Diabetes_binary", "HighBP", "Smoker", "PhysActivity",
                        "Fruits", "Veggies", "HvyAlcoholConsump", "GenHlth",
                        "DiffWalk", "Sex", "Age", "Income")

# When converting variables to factor, set levels and labels for those levels
diabetes_data <- diabetes_data |>
  select(all_of(analysis_variables)) |>
  mutate(Diabetes_binary = factor(Diabetes_binary, 
                                  levels = c("0", "1"), labels = c("No", "Yes")),
         HighBP = factor(HighBP,
                         levels = c("0", "1"), labels = c("Low", "High")),
         Smoker = factor(Smoker,
                         levels = c("0", "1"), labels = c("No", "Yes")),
         PhysActivity = factor(PhysActivity,
                               levels = c("0", "1"), labels = c("No", "Yes")),
         Fruits = factor(Fruits,
                         levels = c("0", "1"), labels = c("No", "Yes")), 
         Veggies = factor(Veggies,
                          levels = c("0", "1"), labels = c("No", "Yes")),
         HvyAlcoholConsump = factor(HvyAlcoholConsump,
                                    levels = c("0", "1"), labels = c("No", "Yes")),
         DiffWalk = factor(DiffWalk,
                           levels = c("0", "1"), labels = c("No", "Yes")),
         Sex = factor(Sex,
                      levels = c("0", "1"), labels = c("Female", "Male")))

# Set the response variable and predictor variables
# Normalize the variables to be on the same scale
diabetes_rf_rec <- recipe(Diabetes_binary ~ HighBP + Smoker + PhysActivity + 
                            Fruits + Veggies + HvyAlcoholConsump + 
                            GenHlth + DiffWalk + Sex + Age + Income, 
                          data = diabetes_data) |>
  step_normalize(all_numeric())

# Using the best random forest model parameters found in the Modeling file
diabetes_rf_mod <- rand_forest(mtry = 3,
                               trees = 100) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

# Creating classification random forest workflow
diabetes_rf_wkf <- workflow() |>
  add_recipe(diabetes_rf_rec) |>
  add_model(diabetes_rf_mod)

# Fitting the best classification random forest on the full dataset
diabetes_best_model <- diabetes_rf_wkf |>
  fit(diabetes_data)

# Find modes of all binary factor variables
diabetes_data_modes <- diabetes_data |>
  summarize(across(where(is.factor), ~ as.character(Mode(.x)[1])))

# Find means of all numerical variables
diabetes_data_means <- diabetes_data |>
  summarize(across(where(is.numeric), ~ as.character(round(mean(.x),2))))


#* @apiTitle Diabetes Health Indicators Dataset Analysis
#* @apiDescription Using Docker and an API to perform the analysis

#* Using a random forest model to make predictions on diabetes status
#* @param HighBP High Blood Pressure binary (Low, High)
#* @param Smoker Smoker binary (No, Yes)
#* @param PhysActivity Physical Activity binary (No, Yes)
#* @param Fruits Fruits binary (No, Yes)
#* @param Veggies Veggies binary(No, Yes)
#* @param HvyAlcoholConsump Heavy Alcohol Consumption binary (No, Yes)
#* @param GenHlth General Health number from 1 (excellent) to 5 (poor)
#* @param DiffWalk Difficulty Walking binary (No, Yes)
#* @param Sex Sex binary (Female, Male)
#* @param Age Age number from 1 (18-24) to 13 (80+)
#* @param Income Income number from 1 (<$10,000) to 8 (>$75,000)
#* @get /pred
function(HighBP = diabetes_data_modes$HighBP, 
         Smoker = diabetes_data_modes$Smoker, 
         PhysActivity = diabetes_data_modes$PhysActivity, 
         Fruits = diabetes_data_modes$Fruits, 
         Veggies = diabetes_data_modes$Veggies, 
         HvyAlcoholConsump = diabetes_data_modes$HvyAlcoholConsump, 
         GenHlth = diabetes_data_means$GenHlth, 
         DiffWalk = diabetes_data_modes$DiffWalk, 
         Sex = diabetes_data_modes$Sex, 
         Age = diabetes_data_means$Age, 
         Income = diabetes_data_means$Income){
  # Make the new data point be a tibble to be able to use for prediction
  pred_data <- as_tibble(list(HighBP = as.factor(HighBP), 
                           Smoker = as.factor(Smoker), 
                           PhysActivity = as.factor(PhysActivity), 
                           Fruits = as.factor(Fruits), 
                           Veggies = as.factor(Veggies), 
                           HvyAlcoholConsump = as.factor(HvyAlcoholConsump), 
                           GenHlth = as.numeric(GenHlth), 
                           DiffWalk = as.factor(DiffWalk), 
                           Sex = as.factor(Sex), 
                           Age = as.numeric(Age), 
                           Income = as.numeric(Income)))
  
  # Predict the diabetes status of this new data point
  prediction <- predict(diabetes_best_model, new_data = pred_data)
  print(prediction)
}

# http://127.0.0.1:8000/pred?HighBP=High&Smoker=No&PhysActivity=Yes&Fruits=Yes&Veggies=Yes&HvyAlcoholConsump=No&GenHlth=5&DiffWalk=Yes&Sex=Female&Age=11&Income=1

# http://127.0.0.1:8000/pred?HighBP=Low&Smoker=No&PhysActivity=Yes&Fruits=Yes&Veggies=Yes&HvyAlcoholConsump=No&GenHlth=1&DiffWalk=No&Sex=Female&Age=6&Income=8

# http://127.0.0.1:8000/pred?HighBP=High&Smoker=No&PhysActivity=No&Fruits=Yes&Veggies=Yes&HvyAlcoholConsump=No&GenHlth=4&DiffWalk=Yes&Sex=Male&Age=10&Income=5


#* My name and a link to the EDA and Modeling
#* @get /info
function(){
  paste("Hello, my name is Kevin Kronk.",
        "Here's a link to my Diabetes Health Indicators Dataset EDA:",
        "https://kevin-kronk.github.io/Final_Project/EDA.html")
}

#* Confusion Matrix showing model performance on the full dataset
#* @serializer png
#* @get /confusion
function(){
  # Create a confusion matrix table by predicting all of the full dataset
  # This uses the best model trained on the full dataset from above
  diabetes_cm <- yardstick::conf_mat(diabetes_data |> 
                                       mutate(estimate = diabetes_best_model |>
                                                predict(diabetes_data) |>
                                                pull()), # data
                                     Diabetes_binary, #truth
                                     estimate) # estimate from the model
  
  # Convert the table to a data frame
  diabetes_cm <- as.data.frame(diabetes_cm$table) 
  
  # Set the order of the levels for the heatmap plot
  diabetes_cm$Truth <- factor(diabetes_cm$Truth, levels = c("Yes", "No"))
  
  # Plot the Truth vs Predictions in a heatmap, showing the number of values for each
  cm_heatmap <- ggplot(diabetes_cm, aes(Truth, Prediction, fill=Freq)) +
    geom_tile(color = "Black", lwd = 1.5) + 
    geom_text(aes(label=Freq), color = "White", size = 5)
  
  print(cm_heatmap) 
}
