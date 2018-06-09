library(readr)
library(Metrics)

linREG <- function(pred_col_name)
{
  #zaladowanie danych treningowych oraz testowych dla kazdego z przewidywanych miesiecy
  training_data <- read_csv(paste0("Data_raw/boruta_mean_nan/BorutaSelectedDummyTrainingData_", pred_col_name,".csv"))
  test_data <- read_csv(paste0("Data_raw/boruta_mean_nan/BorutaSelectedDummyTestData_", pred_col_name,".csv"))
  
  #zamiana (wyrzucenie) wartosci NA w Outcome_Mx na 0
  test_data[pred_col_name][is.na(test_data[pred_col_name])]<-0
  
  #ekstrakcja nazw atrybutow, ktore wplywaja na wartosc Outcome_Mx
  terms <- paste(names(training_data[,-1]), collapse = "+")
  
  #budowa formuly oraz modelu
  fmla <- as.formula(paste(pred_col_name, " ~ ", terms))
  linRegModel <- lm(fmla, data=training_data)
  
  #predykcja wartosci dla danych testowych
  predict <- predict(linRegModel, test_data)
  linRegModel_summary <- summary(linRegModel)
  
  #liczenie RMSLE oraz zwracanie wynikow
  actuals_preds <- data.frame(cbind(actuals=test_data[pred_col_name], predicteds=predict))  # make actuals_predicteds dataframe.
  actuals_preds[actuals_preds<0] <- 0 #sprzedaz nie moze byc mniejsza niz zero (ponadto logarytm w RMSLE nie dziala)
  correlation_accuracy <- cor(actuals_preds)
  error_score <- rmsle(actuals_preds[pred_col_name], actuals_preds$predicteds)
  print(error_score)
  result <- list("error_score" = error_score, "actuals_preds"=actuals_preds, "model"= linRegModel)
  return (result)
}


report_m1_linreg = linREG("Outcome_M1")
report_m2_linreg = linREG("Outcome_M2")
report_m3_linreg = linREG("Outcome_M3")
report_m4_linreg = linREG("Outcome_M4")
report_m5_linreg = linREG("Outcome_M5")
report_m6_linreg = linREG("Outcome_M6")
report_m7_linreg = linREG("Outcome_M7")
report_m8_linreg = linREG("Outcome_M8")
report_m9_linreg = linREG("Outcome_M9")
report_m10_linreg = linREG("Outcome_M10")
report_m11_linreg = linREG("Outcome_M11")
report_m12_linreg = linREG("Outcome_M12")

#Zbieranie rmsle po miesiacach
rmsle_months <- list()
for(i in 1:12)
{
  eval(parse(text= paste0("rmsle_months[i] <- report_m", i, "_linreg$error_score")))
}
df <- data.frame(matrix(unlist(rmsle_months)))
write.csv(df, file = "Scripts/Models/LinReg/Data/RMSLE_linreg.csv", row.names=FALSE)

#Liczenie rmsle dla calego modelu
results <- report_m1_linreg$actuals_preds
colnames(results) <- c("Outcome", "predicteds")
reports = paste0("report_m", 2:12, "_linreg$actuals_preds")
for(report.name in reports)
{
  eval(parse(text= paste0("colnames(", report.name ,") <- c(\"Outcome\", \"predicteds\")")))
  eval(parse(text= paste0("results <- rbind(results, ",report.name,")")))
}
error_score <- rmsle(results$Outcome, results$predicteds)
print(paste0("RMSLE dla calego modelu wynosi: " , error_score))
