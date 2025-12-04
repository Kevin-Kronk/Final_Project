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
diabetes_data <- diabetes_data |>
  select(all_of(analysis_variables)) |>
  mutate(Diabetes_binary = factor(Diabetes_binary),
         HighBP = factor(HighBP),
         Smoker = factor(Smoker),
         PhysActivity = factor(PhysActivity),
         Fruits = factor(Fruits), 
         Veggies = factor(Veggies),
         HvyAlcoholConsump = factor(HvyAlcoholConsump),
         DiffWalk = factor(DiffWalk),
         Sex = factor(Sex))

diabetes_rf_rec <- recipe(Diabetes_binary ~ HighBP + Smoker + PhysActivity + 
                            Fruits + Veggies + HvyAlcoholConsump + 
                            GenHlth + DiffWalk + Sex + Age + Income, 
                          data = diabetes_data) |>
  step_normalize(all_numeric())

diabetes_rf_mod <- rand_forest(mtry = 3,
                               trees = 150) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

diabetes_rf_wkf <- workflow() |>
  add_recipe(diabetes_rf_rec) |>
  add_model(diabetes_rf_mod)

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
#* @param HighBP High Blood Pressure number
#* @param Smoker Smoker number
#* @param PhysActivity Physical Activity number
#* @param Fruits Fruits number
#* @param Veggies Veggies number
#* @param HvyAlcoholConsump Heavy Alcohol Consumption number
#* @param GenHlth General Health number
#* @param DiffWalk Difficulty Walking number
#* @param Sex Sex number
#* @param Age Age number
#* @param Income Income number
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
  prediction <- predict(diabetes_best_model, new_data = pred_data)
  print(prediction)
}

# http://127.0.0.1:8000/pred?HighBP=1&Smoker=0&PhysActivity=1&Fruits=1&Veggies=1&HvyAlcoholConsump=0&GenHlth=5&DiffWalk=1&Sex=0&Age=11&Income=1

# http://127.0.0.1:8000/pred?HighBP=0&Smoker=0&PhysActivity=1&Fruits=1&Veggies=1&HvyAlcoholConsump=0&GenHlth=1&DiffWalk=0&Sex=0&Age=6&Income=8

# http://127.0.0.1:8000/pred?HighBP=1&Smoker=0&PhysActivity=0&Fruits=1&Veggies=1&HvyAlcoholConsump=0&GenHlth=4&DiffWalk=1&Sex=1&Age=10&Income=5


#* My name and a link to the EDA and Modeling
#* @get /info
function(){
  paste("Hello, my name is Kevin Kronk.",
        "Here's a link to my Diabetes Health Indicators Dataset EDA:",
        "https://kevin-kronk.github.io/Final_Project/EDA.html")
}

#* Confusion Matrix showing model performance on the data
#* @serializer png
#* @get /confusion
function(){
  diabetes_cm <- yardstick::conf_mat(diabetes_data |> 
                                       mutate(estimate = diabetes_best_model |>
                                                predict(diabetes_data) |>
                                                pull()), # data
                                     Diabetes_binary, #truth
                                     estimate) # estimate from the model
  
  diabetes_cm <- as.data.frame(diabetes_cm$table) 
  
  diabetes_cm$Truth <- factor(diabetes_cm$Truth, levels = c("1", "0"))
  
  cm_heatmap <- ggplot(diabetes_cm, aes(Truth, Prediction, fill=Freq)) +
    geom_tile(color = "Black", lwd = 1.5) + 
    geom_text(aes(label=Freq), color = "White", size = 5)
  
  print(cm_heatmap) 
}
