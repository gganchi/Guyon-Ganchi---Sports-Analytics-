library(readxl)
library(dplyr)
library(xgboost)


# Step 1: Loading and Preparing Data 
MVP_Data <- readxl::read_excel("MVP_Data.xlsx")

# Creating Position dummy variable where guard (G) = 0
MVP_Data <- MVP_Data %>%
  mutate(
    POS = ifelse(POS == "G", 0L, 1L)
  ) %>%
  select(-Tm) %>%       # drop Team column
  rename(
    FG_pct        = `FG%`,
    X3P_pct       = `3P%`,
    FT_pct        = `FT%`,
    Team_Wins     = `Team Wins`,
    USG_pct       = `USG%`,
    Plus_Minus    = `Plus/Minus`,
    W_O_Player    = `W/O Player`,
    Voter_Fatigue = `Voter Fatigue`,
    MVP_Winner    = `MVP Winner`
  )

# Make sure target is numeric 0/1
MVP_Data$MVP_Winner <- as.numeric(MVP_Data$MVP_Winner)

# Independent variables
model_vars <- c(
  "POS", "PTS", "REB", "AST", "STL", "BLK",
  "FG_pct", "X3P_pct", "FT_pct", "Team_Wins", "USG_pct",
  "Plus_Minus", "W_O_Player", "Voter_Fatigue"
)

# Step 2: Establishing data that the model will be trained on and what the model will actually predict.
MVP_Data_train <- MVP_Data %>% filter(Season != "2025-26")
MVP_Data_pred  <- MVP_Data %>% filter(Season == "2025-26")

# Build training matrix (numeric)
train_X <- MVP_Data_train %>%
  select(all_of(model_vars)) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

train_y <- MVP_Data_train$MVP_Winner

# Prediction matrix for 2025-26 (numeric)
pred_X <- MVP_Data_pred %>%
  select(all_of(model_vars)) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

# Step 3: Creating DMatrix and Handling Class Imbalance
dtrain <- xgb.DMatrix(data = train_X, label = train_y)

num_pos <- sum(train_y == 1)
num_neg <- sum(train_y == 0)
scale_pos_weight <- num_neg / num_pos   # weight the positive (MVP) class

scale_pos_weight

# Step 4: Setting XGBoost Parameters
params <- list(
  objective        = "binary:logistic",
  eval_metric      = "logloss",
  eta              = 0.05,     # learning rate
  max_depth        = 3,        # tree depth
  subsample        = 0.8,
  colsample_bytree = 0.8,
  scale_pos_weight = scale_pos_weight
)

set.seed(42)

# Step 5: Training the XGBoost Model
xgb_model <- xgb.train(
  params  = params,
  data    = dtrain,
  nrounds = 400,
  verbose = 0   
)

# Step 6: Confusion Matrix
train_pred_probs <- predict(xgb_model, dtrain)
train_pred_class <- ifelse(train_pred_probs >= 0.5, 1, 0)

tab <- table(
  actual    = train_y,
  predicted = train_pred_class
)
tab

# Variable Importance Breakdown
importance_matrix <- xgb.importance(
  model = xgb_model,
  feature_names = model_vars
)

# Print the importance table
print(importance_matrix)

# View ordered by Gain (most important measure)
importance_matrix %>% arrange(desc(Gain))


# Step 7: 2025-26 Prediction
pred_players_raw <- data.frame(
  Season        = rep("2025-26", 10),
  Player        = c(
    "Nikola Jokić",
    "Shai Gilgeous-Alexander",
    "Luka Dončić",
    "Giannis Antetokounmpo",
    "Cade Cunningham",
    "Victor Wembanyama",
    "Donovan Mitchell",
    "Tyrese Maxey",
    "Alperen Sengun",
    "Jalen Johnson"
  ),
  POS           = c("F","G","G","F","G","F","G","G","F","F"),
  PTS           = c(28.9, 32.9, 35.1, 30.9, 28.8, 26.2, 30.6, 31.7, 22.0, 21.9),
  REB           = c(12.4, 4.9, 8.5, 10.9, 6.4, 12.9, 4.7, 4.7, 9.4, 9.6),
  AST           = c(10.9, 6.7, 9.4, 6.6, 9.4, 4.0, 5.3, 7.5, 7.1, 7.3),
  STL           = c(1.5, 1.6, 1.8, 1.0, 1.2, 1.1, 1.4, 1.6, 1.1, 1.7),
  BLK           = c(0.7, 0.8, 0.6, 1.0, 0.7, 3.6, 0.3, 0.8, 0.9, 0.4),
  `FG%`         = c(0.637, 0.545, 0.475, 0.643, 0.456, 0.502, 0.505, 0.469, 0.483, 0.544),
  `3P%`         = c(0.453, 0.423, 0.340, 0.455, 0.312, 0.345, 0.389, 0.401, 0.404, 0.391),
  `FT%`         = c(0.852, 0.891, 0.802, 0.636, 0.815, 0.857, 0.839, 0.883, 0.726, 0.785),
  `Team Wins`   = c(55, 64, 50, 37, 53, 52, 48, 42, 59, 45),
  `USG%`        = c(28.3, 33.7, 37.2, 37.7, 33.3, 30.7, 32.6, 29.8, 25.8, 25.4),
  `Plus/Minus`  = c(207, 282, 84, 103, 113, 113, 118, 33, 134, 33),
  `W/O Player`  = c(0.393, 0.315, 0.493, 0.464, 0.252, 0.396, 0.606, 0.451, 0.429, 0.459),
  `Voter Fatigue` = c(5, 3, 1, 5, 1, 0, 0, 0, 0, 0),
  check.names   = FALSE
)

# Apply same preprocessing as training data
pred_players <- pred_players_raw %>%
  mutate(
    POS = ifelse(POS == "G", 0L, 1L)
  ) %>%
  rename(
    FG_pct        = `FG%`,
    X3P_pct       = `3P%`,
    FT_pct        = `FT%`,
    Team_Wins     = `Team Wins`,
    USG_pct       = `USG%`,
    Plus_Minus    = `Plus/Minus`,
    W_O_Player    = `W/O Player`,
    Voter_Fatigue = `Voter Fatigue`
  )

# Step 8. Building the numeric matrix for XGBoost Prediction
pred_X <- pred_players %>%
  select(all_of(model_vars)) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

dpred <- xgb.DMatrix(data = pred_X)

# Step 9: Predicting the MVP probabilities for chosen players
pred_probs <- predict(xgb_model, dpred)

predictions_xgb <- data.frame(
  player_name = pred_players$Player,
  probability = pred_probs
)

print(predictions_xgb)

# Step 10: Exporting Predictions in specified format 
write.csv(
  predictions_xgb,
  file = "predictions.csv",
  row.names = FALSE
)
