library(readr)
library(randomForest)
library(Metrics)

lasLosowy <- function(pred_col_name, error_score)
{
  stopifnot(require(randomForest))
  
  #załadowanie danych treningowych oraz testowych dla każdego z przewidywanych miesięcy
  training_data <- read_csv(paste0("Data_raw/BorutaSelectedDummyTrainingData_", pred_col_name,".csv"))
  test_data <- read_csv(paste0("Data_raw/BorutaSelectedDummyTestData_", pred_col_name,".csv"))
  
  #zamiana (wyrzucenie) wartosci NA w Outcome_Mx na 0
  test_data[pred_col_name][is.na(test_data[pred_col_name])]<-0
  
  #ekstrakcja nazw atrybutów, które wpływają na wartość Outcome_Mx
  terms <- paste(names(training_data[,-1]), collapse = "+")
  
  #budowa formuły oraz lasu losowego
  fmla <- as.formula(paste(pred_col_name, " ~ ", terms))
  h_500.rf <- randomForest(fmla, training_data, do.trace=100, ntree=500, test=test_data)
  print(h_500.rf)
  
  #predykcja wartości dla danych testowych
  predict <- round(predict(h_500.rf, test_data))
  rf_500_summary <- summary(h_500.rf)

  #liczenie RMSLE oraz zwracanie wyników
  actuals_preds <- data.frame(cbind(actuals=test_data[pred_col_name], predicteds=predict))  # make actuals_predicteds dataframe.
  correlation_accuracy <- cor(actuals_preds)
  error_score <- rmsle(actuals_preds[pred_col_name], actuals_preds$predicteds)
  print(error_score)
  result <- list("error_score" = error_score, "actuals_preds"=actuals_preds)
  return (result)
}

report_m1_rf <- lasLosowy("Outcome_M1")
report_m2_rf <- lasLosowy("Outcome_M2")
report_m3_rf <- lasLosowy("Outcome_M3")
report_m4_rf <- lasLosowy("Outcome_M4")
# report_m5_rf <- lasLosowy("Outcome_M5")
# report_m6_rf <- lasLosowy("Outcome_M6")
# report_m7_rf <- lasLosowy("Outcome_M7")
# report_m8_rf <- lasLosowy("Outcome_M8")
# report_m9_rf <- lasLosowy("Outcome_M9")
# report_m10_rf <- lasLosowy("Outcome_M10")
# report_m11_rf <- lasLosowy("Outcome_M11")
# report_m12_rf <- lasLosowy("Outcome_M12")

reports = paste0("temp <- report_m", 1:12, "_rf$actuals_preds")


#results <- report_m1_rf$actuals_preds
#temp1 <- report_m1_rf$actuals_preds
#eval("temp <- report_m1_rf$actuals_preds")
#results <- rbind(results, temp1)

#eval({ xx <- pi; xx^2}) ; xx

#print(reports)
# results <- NA
# for(r in reports){
#   eval({r})
#   results <- rbind(results, temp)
# }
# 
# print(report_m1_rf$error_score)
