library(readr)
library(dplyr)
library(randomForest)
library(Metrics)

NTrees <- 600
seed_s <- 400
lasLosowy <- function(pred_col_name, Nvar)
{
  stopifnot(require(randomForest))
  set.seed(seed_s)
  #zaladowanie danych treningowych oraz testowych dla kazdego z przewidywanych miesiecy
  training_data <- read_csv(paste0("Data_raw/boruta_zero_nan/BorutaSelectedDummyTrainingData_", pred_col_name,".csv"))
  
  #wyrzucenie rekordow przyjmujacych wartosci NA w Outcome_Mx
  test_data <- read_csv(paste0("Data_raw/boruta_zero_nan/BorutaSelectedDummyTestData_", pred_col_name,".csv"))
  test_data = test_data[complete.cases(test_data[pred_col_name]),]
  
  #ekstrakcja nazw atrybutow, ktore wplywaja na wartosc Outcome_Mx
  terms <- paste(names(training_data[,-1]), collapse = "+")
  
  #budowa formuly oraz lasu losowego
  fmla <- as.formula(paste(pred_col_name, " ~ ", terms))
  h.rf <- randomForest(fmla, training_data, do.trace=200, ntree=NTrees, mtry=Nvar, test=test_data)
  #plot(h.rf, main = paste0("RF for ", pred_col_name))
  
  #predykcja wartosci dla danych testowych
  predict <- round(predict(h.rf, test_data))
  rf_500_summary <- summary(h.rf)
  
  #liczenie RMSLE oraz zwracanie wynikow
  actuals_preds <- data.frame(cbind(actuals=test_data[pred_col_name], predicteds=predict))  #make actuals_predicteds dataframe
  correlation_accuracy <- cor(actuals_preds)
  error_score <- rmsle(actuals_preds[pred_col_name], actuals_preds$predicteds)
  print(error_score)
  result <- list("error_score" = error_score, "actuals_preds"=actuals_preds, "model"= h.rf)
  return (result)
}


#generowanie wynikow predykcji sprzedazy online dla wszystkich 12 miesiecy
Nvars <- c(1:15)
best_mtry <- c()

#tune Outcome_M1
errs <- c()
for(nvar in Nvars)
{
  report_m1_rf <- lasLosowy("Outcome_M1", nvar)
  errs <- append(errs, report_m1_rf$error_score)
}

par(mfrow=c(2,3)) 
plot(errs, main = "RMSLE for Outcome_M1", xlab="mtry", ylab = "RMSLE")
best_mtry <- append(best_mtry, which.min(errs))

#tune Outcome_M2
errs <- c()
for(nvar in Nvars)
{
  report_m2_rf <- lasLosowy("Outcome_M2", nvar)
  errs <- append(errs, report_m2_rf$error_score)
}

plot(errs, main = "RMSLE for Outcome_M2", xlab="mtry", ylab = "RMSLE")
best_mtry <- append(best_mtry, which.min(errs))

#tune Outcome_M3
errs <- c()
for(nvar in Nvars)
{
  report_m3_rf <- lasLosowy("Outcome_M3", nvar)
  errs <- append(errs, report_m3_rf$error_score)
}

plot(errs, main = "RMSLE for Outcome_M3", xlab="mtry", ylab = "RMSLE")
best_mtry <- append(best_mtry, which.min(errs))

#tune Outcome_M4
errs <- c()
for(nvar in Nvars)
{
  report_m4_rf <- lasLosowy("Outcome_M4", nvar)
  errs <- append(errs, report_m4_rf$error_score)
}

plot(errs, main = "RMSLE for Outcome_M4", xlab="mtry", ylab = "RMSLE")
best_mtry <- append(best_mtry, which.min(errs))

#tune Outcome_M5
errs <- c()
for(nvar in Nvars)
{
  report_m5_rf <- lasLosowy("Outcome_M5", nvar)
  errs <- append(errs, report_m5_rf$error_score)
}

plot(errs, main = "RMSLE for Outcome_M5", xlab="mtry", ylab = "RMSLE")
best_mtry <- append(best_mtry, which.min(errs))

#tune Outcome_M6
errs <- c()
for(nvar in Nvars)
{
  report_m6_rf <- lasLosowy("Outcome_M6", nvar)
  errs <- append(errs, report_m6_rf$error_score)
}

plot(errs, main = "RMSLE for Outcome_M6", xlab="mtry", ylab = "RMSLE")
best_mtry <- append(best_mtry, which.min(errs))

#tune Outcome_M7
errs <- c()
for(nvar in Nvars)
{
  report_m7_rf <- lasLosowy("Outcome_M7", nvar)
  errs <- append(errs, report_m7_rf$error_score)
}

plot(errs, main = "RMSLE for Outcome_M7", xlab="mtry", ylab = "RMSLE")
best_mtry <- append(best_mtry, which.min(errs))

#tune Outcome_M8
errs <- c()
for(nvar in Nvars)
{
  report_m8_rf <- lasLosowy("Outcome_M8", nvar)
  errs <- append(errs, report_m8_rf$error_score)
}

plot(errs, main = "RMSLE for Outcome_M8", xlab="mtry", ylab = "RMSLE")
best_mtry <- append(best_mtry, which.min(errs))

#tune Outcome_M9
errs <- c()
for(nvar in Nvars)
{
  report_m9_rf <- lasLosowy("Outcome_M9", nvar)
  errs <- append(errs, report_m9_rf$error_score)
}

plot(errs, main = "RMSLE for Outcome_M9", xlab="mtry", ylab = "RMSLE")
best_mtry <- append(best_mtry, which.min(errs))

#tune Outcome_M10
errs <- c()
for(nvar in Nvars)
{
  report_m10_rf <- lasLosowy("Outcome_M10", nvar)
  errs <- append(errs, report_m10_rf$error_score)
}

plot(errs, main = "RMSLE for Outcome_M10", xlab="mtry", ylab = "RMSLE")
best_mtry <- append(best_mtry, which.min(errs))

#tune Outcome_M11
errs <- c()
for(nvar in Nvars)
{
  report_m11_rf <- lasLosowy("Outcome_M11", nvar)
  errs <- append(errs, report_m11_rf$error_score)
}

plot(errs, main = "RMSLE for Outcome_M11", xlab="mtry", ylab = "RMSLE")
best_mtry <- append(best_mtry, which.min(errs))

#tune Outcome_M12
errs <- c()
for(nvar in Nvars)
{
  report_m12_rf <- lasLosowy("Outcome_M12", nvar)
  errs <- append(errs, report_m12_rf$error_score)
}

plot(errs, main = "RMSLE for Outcome_M12", xlab="mtry", ylab = "RMSLE")
best_mtry <- append(best_mtry, which.min(errs))

print(best_mtry)
write.csv(best_mtry, "Scripts/Models/RF/Data/best_mtries1-15.csv")
