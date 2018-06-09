#ten skrypt preprocesuje dane wejsciowe, zastepujac NaNy zerem i srednia, i rodziela je na zbiory treningowy i testowy
library(dummies)

input_data = read.csv("../Data_raw/Dataset_old.csv", na.strings="NaN")

date_variables_names = c("Date_1", "Date_2")

outcome_variables_names = paste("Outcome_M", 1:12, sep="")

#outcome_variables_names = grep("Outcome_", names(input_data))

#usuniecie dwoch wierszy z brakujaca informacja o zmiennej Date_2
input_data = input_data[complete.cases(input_data[date_variables_names]),]

#obliczenie dodatkowych zmiennych zwiazanych z datami

#przyblizony dzien roku
Day_Of_Year_1 = (input_data$Date_1+(365.25/2)) %% 365.25
Day_Of_Year_2 = (input_data$Date_2+(365.25/2)) %% 365.25

#dzien tygodnia
Day_Of_Week_1 = factor(input_data$Date_1 %% 7)
Day_Of_Week_2 = factor(input_data$Date_2 %% 7)

Days_To_Launch = input_data$Date_1 - input_data$Date_2

#dodanie nowych zmiennych do danych
input_data = cbind(input_data, Day_Of_Year_1, Day_Of_Year_2, Day_Of_Week_1, Day_Of_Week_2, Days_To_Launch)

#usuniecie kolumn z zerowa wariancja (o tych samych wartosciach), testy wykazaly, ze Boruta je zawsze odrzuca (i slusznie)
input_data = input_data[,apply(input_data, 2, var, na.rm=TRUE) != 0]

cat_variables_columns = grep("Cat_", names(input_data))

cat_variables_names = names(input_data)[cat_variables_columns]

quant_variables_columns = grep("Quan", names(input_data))

mean_nan_input_data = input_data
zero_nan_input_data = input_data

#zastapienie NaNow w zmiennych Quan ich srednimi <- inna opcja jest zastapienie NaNow zerami, potem zostawimy lepsza z nich
#zastapienie Nanow jest niezbedne dla dzialania Boruty
#do obliczenia sredniej uzywana jest cala informacja (rowniez z wierszy, ktore znajda sie w testsecie), dlatego wykonywane jest teraz
for(i in quant_variables_columns){
  mean_nan_input_data[is.na(mean_nan_input_data[,i]),i] = mean(mean_nan_input_data[,i], na.rm = TRUE)
  zero_nan_input_data[is.na(zero_nan_input_data[,i]),i] = 0
}

#znalezienie wierszy z pelna informacja o sprzedazy
complete_cases = complete.cases(zero_nan_input_data[outcome_variables_names])

zero_nan_complete_outcome_information = zero_nan_input_data[complete_cases,]
mean_nan_complete_outcome_information = mean_nan_input_data[complete_cases,]

#znalezienie wierszy z niepelna informacja o sprzedazy - jedna ze zmiennych Outcome == NaN
zero_nan_incomplete_outcome_information = zero_nan_input_data[!complete_cases,]
mean_nan_incomplete_outcome_information = mean_nan_input_data[!complete_cases,]

#losowa probka 10% wierszy o pelnej informacji sprzedazy do testow, uzywamy tej samej probki
random_sample = sample(1:nrow(zero_nan_complete_outcome_information), round(0.1*nrow(zero_nan_complete_outcome_information)))

#polaczenie tej probki z wierszami o niepelnej informacji sprzedazy tworzy zbior testowy
zero_nan_test_data = rbind(zero_nan_complete_outcome_information[random_sample,],zero_nan_incomplete_outcome_information)
mean_nan_test_data = rbind(mean_nan_complete_outcome_information[random_sample,],mean_nan_incomplete_outcome_information)

#do trainsetu trafiaja wszystkie wiersze z pelna informacja o sprzedazy oprocz tej probki
zero_nan_training_data = zero_nan_complete_outcome_information[-random_sample,]
mean_nan_training_data = mean_nan_complete_outcome_information[-random_sample,]

write.csv(zero_nan_training_data, file = "../Data_raw/ZeroNanTrainingData.csv", row.names=FALSE)
write.csv(zero_nan_test_data, file = "../Data_raw/ZeroNanTestData.csv", row.names=FALSE)

write.csv(mean_nan_training_data, file = "../Data_raw/MeanNanTrainingData.csv", row.names=FALSE)
write.csv(mean_nan_test_data, file = "../Data_raw/MeanNanTestData.csv", row.names=FALSE)

training_datasets = list(zero_nan_training_data, mean_nan_training_data)
test_datasets = list(zero_nan_test_data, mean_nan_test_data)
directories_names = list("no_boruta_zero_nan", "no_boruta_mean_nan")

for(i in 1:2) {

  training_dataset = training_datasets[[i]]
  test_dataset = test_datasets[[i]]
  directory_name = directories_names[[i]]
  
  for (k in 1:12){
    selected_training_data = cbind(training_dataset[k], training_dataset[13:length(training_dataset)])
    selected_test_data = cbind(test_dataset[k], test_dataset[13:length(test_dataset)])
    
    write.csv(selected_training_data, file = sprintf("../Data_raw/%s/TrainingData_Outcome_M%s.csv", directory_name, k), row.names=FALSE)
    write.csv(selected_test_data, file = sprintf("../Data_raw/%s/TestData_Outcome_M%s.csv", directory_name, k), row.names=FALSE)
    
    selected_training_dummy_variables= c()
    selected_test_dummy_variables= c()
    
    for (cat_variable in cat_variables_names){
      selected_training_dummy_variables = cbind(selected_training_dummy_variables, dummy(cat_variable, training_dataset, sep="_"))
      selected_test_dummy_variables = cbind(selected_test_dummy_variables, dummy(cat_variable, test_dataset, sep="_"))
    }
    
    unique_to_training_data = setdiff(colnames(selected_training_dummy_variables), colnames(selected_test_dummy_variables))
    unique_to_test_data = setdiff(colnames(selected_test_dummy_variables), colnames(selected_training_dummy_variables))
    
    selected_test_dummy_variables = data.frame(selected_test_dummy_variables)
    selected_training_dummy_variables = data.frame(selected_training_dummy_variables)
    
    #dodanie brakujacych dummy variables do danych testowych
    selected_test_dummy_variables[, unique_to_training_data] = 0
    
    #usuniecie dummy variables nieobecnych w danych treningowych
    selected_test_dummy_variables = selected_test_dummy_variables[, !(names(selected_test_dummy_variables) %in% unique_to_test_data)]
    
    selected_dummy_training_data = cbind(selected_training_data[, !(names(selected_training_data) %in% cat_variables_names)], selected_training_dummy_variables)
    selected_dummy_test_data = cbind(selected_test_data[, !(names(selected_test_data) %in% cat_variables_names)], selected_test_dummy_variables)
    
    write.csv(selected_dummy_training_data, file = sprintf("../Data_raw/%s/DummyTrainingData_Outcome_M%s.csv", directory_name, k), row.names=FALSE)
    write.csv(selected_dummy_test_data, file = sprintf("../Data_raw/%s/DummyTestData_Outcome_M%s.csv", directory_name , k), row.names=FALSE)
    
    }
}