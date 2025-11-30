library(plumber)
library(tidymodels)
library(tidyverse)
library(yardstick)
library(DescTools)

# Load in Diabetes Health Indicators Dataset
diabetes_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")

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
                               trees = 100) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

diabetes_rf_wkf <- workflow() |>
  add_recipe(diabetes_rf_rec) |>
  add_model(diabetes_rf_mod)

diabetes_best_model <- diabetes_rf_wkf |>
  fit(diabetes_data)

# Need to find all modes and means

#* pred endpoint
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
function(HighBP = 0, 
         Smoker = 0, 
         PhysActivity = 0, 
         Fruits = 0, 
         Veggies = 0, 
         HvyAlcoholConsump = 0, 
         GenHlth = 3, 
         DiffWalk = 0, 
         Sex = 0, 
         Age = 7, 
         Income = 4){
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

#* info endpoint
#* @get /info
function(){
  print("Kevin Kronk")
  print("https://kevin-kronk.github.io/Final_Project/EDA.html")
}

#* confusion endpoint
#* @get /confusion
function(){
  diabetes_best_model <- diabetes_rf_wkf |>
    fit(diabetes_data) 
  a <- yardstick::conf_mat(diabetes_data |> 
             mutate(estimate = diabetes_best_model |>
                      predict(diabetes_data) |>
                      pull()), # data
           Diabetes_binary, #truth
           estimate) # estimate from the model
  print(a)
}
