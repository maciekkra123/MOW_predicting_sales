library(readr)
library(dplyr)
library(randomForest)
library(Metrics)

NTrees <- 200

lasLosowy <- function(pred_col_name, error_score)
{
  stopifnot(require(randomForest))
  
  #zaladowanie danych treningowych oraz testowych dla kazdego z przewidywanych miesiecy
  training_data <- read_csv(paste0("Data_raw/BorutaSelectedDummyTrainingData_", pred_col_name,".csv"))
  test_data <- read_csv(paste0("Data_raw/BorutaSelectedDummyTestData_", pred_col_name,".csv"))
  
  #wyrzucenie rekordow przyjmujacych wartosci NA w Outcome_Mx
  test_data = test_data[complete.cases(test_data[pred_col_name]),]
  
  #ekstrakcja nazw atrybutow, ktore wplywaja na wartosc Outcome_Mx
  terms <- paste(names(training_data[,-1]), collapse = "+")
  
  #budowa formuly oraz lasu losowego
  fmla <- as.formula(paste(pred_col_name, " ~ ", terms))
  h_500.rf <- randomForest(fmla, training_data, do.trace=200, ntree=NTrees, test=test_data)
  print(h_500.rf)
  
  #predykcja wartosci dla danych testowych
  predict <- round(predict(h_500.rf, test_data))
  rf_500_summary <- summary(h_500.rf)
  
  #liczenie RMSLE oraz zwracanie wynikow
  actuals_preds <- data.frame(cbind(actuals=test_data[pred_col_name], predicteds=predict))  #make actuals_predicteds dataframe
  correlation_accuracy <- cor(actuals_preds)
  error_score <- rmsle(actuals_preds[pred_col_name], actuals_preds$predicteds)
  print(error_score)
  result <- list("error_score" = error_score, "actuals_preds"=actuals_preds)
  return (result)
}

#generowanie wynikow predykcji sprzedazy online dla wszystkich 12 miesiecy
report_m1_rf <- lasLosowy("Outcome_M1")
report_m2_rf <- lasLosowy("Outcome_M2")
report_m3_rf <- lasLosowy("Outcome_M3")
report_m4_rf <- lasLosowy("Outcome_M4")
report_m5_rf <- lasLosowy("Outcome_M5")
report_m6_rf <- lasLosowy("Outcome_M6")
report_m7_rf <- lasLosowy("Outcome_M7")
report_m8_rf <- lasLosowy("Outcome_M8")
report_m9_rf <- lasLosowy("Outcome_M9")
report_m10_rf <- lasLosowy("Outcome_M10")
report_m11_rf <- lasLosowy("Outcome_M11")
report_m12_rf <- lasLosowy("Outcome_M12")

#Zbieranie rmsle po miesiacach
rmsle_months <- list()
for(i in 1:12)
{
  eval(parse(text= paste0("rmsle_months[i] <- report_m", i, "_rf$error_score")))
}
df <- data.frame(matrix(unlist(rmsle_months)))
write.csv(df, file = paste0("Scripts/Models/RF/RMSLE_rf", NTrees, ".csv"), row.names=FALSE)

#Liczenie rmsle dla calego modelu
results <- report_m1_rf$actuals_preds
colnames(results) <- c("Outcome", "predicteds")
reports = paste0("report_m", 2:12, "_rf$actuals_preds")
for(report.name in reports)
{
  eval(parse(text= paste0("colnames(", report.name ,") <- c(\"Outcome\", \"predicteds\")")))
  eval(parse(text= paste0("results <- rbind(results, ",report.name,")")))
}
error_score <- rmsle(results$Outcome, results$predicteds)
print(paste0("RMSLE dla calego modelu wynosi: " , error_score))

