library(readr)
library(Metrics)

linREG <- function(pred_col_name)
{
  training_data <- read_csv(paste0("Data_raw/BorutaSelectedDummyTrainingData_", pred_col_name,".csv"))
  test_data <- read_csv(paste0("Data_raw/BorutaSelectedDummyTrainingData_", pred_col_name,".csv"))
  terms <- paste(names(training_data[,-1]), collapse = "+")
  fmla <- as.formula(paste(pred_col_name, " ~ ", terms))
  linRegModel <- lm(fmla, data=training_data)
  predict <- predict(linRegModel, test_data)
  linRegModel_summary <- summary(linRegModel)
  #View(linRegModel_summary)
  actuals_preds <- data.frame(cbind(actuals=test_data[pred_col_name], predicteds=predict))  # make actuals_predicteds dataframe.
  View(actuals_preds)
  correlation_accuracy <- cor(actuals_preds)
  res <- rmse(actuals_preds[pred_col_name], actuals_preds$predicteds)
  print(res)
  return (res)
}

rmse_M1 = linREG("Outcome_M1")
rmse_M2 = linREG("Outcome_M2")
rmse_M3 = linREG("Outcome_M3")
rmse_M4 = linREG("Outcome_M4")
rmse_M5 = linREG("Outcome_M5")
rmse_M6 = linREG("Outcome_M6")
