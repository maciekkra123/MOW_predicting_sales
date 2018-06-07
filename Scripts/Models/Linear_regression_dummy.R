library(readr)
library(Metrics)

linREG <- function(pred_col_name)
{
  #załadowanie danych treningowych oraz testowych dla każdego z przewidywanych miesięcy
  training_data <- read_csv(paste0("Data_raw/BorutaSelectedDummyTrainingData_", pred_col_name,".csv"))
  test_data <- read_csv(paste0("Data_raw/BorutaSelectedDummyTestData_", pred_col_name,".csv"))
  
  #zamiana (wyrzucenie) wartosci NA w Outcome_Mx na 0
  test_data[pred_col_name][is.na(test_data[pred_col_name])]<-0
  
  #ekstrakcja nazw atrybutów, które wpływają na wartość Outcome_Mx
  terms <- paste(names(training_data[,-1]), collapse = "+")
  
  #budowa formuły oraz modelu
  fmla <- as.formula(paste(pred_col_name, " ~ ", terms))
  linRegModel <- lm(fmla, data=training_data)
  
  #predykcja wartości dla danych testowych
  predict <- predict(linRegModel, test_data)
  linRegModel_summary <- summary(linRegModel)
  
  #liczenie RMSLE oraz zwracanie wyników
  actuals_preds <- data.frame(cbind(actuals=test_data[pred_col_name], predicteds=predict))  # make actuals_predicteds dataframe.
  actuals_preds[actuals_preds<0] <- 0 #sprzedaż nie może być mniejsza niż zero (ponadto logarytm w RMSLE nie działa)
  correlation_accuracy <- cor(actuals_preds)
  err <- rmsle(actuals_preds[pred_col_name], actuals_preds$predicteds)
  print(err)
  return (err)
}


rmsle_M1 = linREG("Outcome_M1")
rmsle_M2 = linREG("Outcome_M2")
rmsle_M3 = linREG("Outcome_M3")
rmsle_M4 = linREG("Outcome_M4")
rmsle_M5 = linREG("Outcome_M5")
rmsle_M6 = linREG("Outcome_M6")
