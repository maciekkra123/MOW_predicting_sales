#ten skrypt przeprowadza selekcje atrybutow wykorzystujac algorytm Boruta dla czterech wariantów danych

library(Boruta)
library(dummies)

mean_nan_training_data = read.csv("../Data_raw/MeanNanTrainingData.csv", header = TRUE)
zero_nan_training_data = read.csv("../Data_raw/ZeroNanTrainingData.csv", header = TRUE)

mean_nan_test_data = read.csv("../Data_raw/MeanNanTestData.csv", header = TRUE)
zero_nan_test_data = read.csv("../Data_raw/ZeroNanTestData.csv", header = TRUE)

#zamiana zmiennych Cat z typu integer na typ factor
cat_variables_columns = grep("Cat_", names(zero_nan_training_data))
cat_variables_names = names(zero_nan_training_data)[cat_variables_columns]

quant_variables_columns = grep("Quan", names(zero_nan_training_data))

#zamiana zmiennych Cat z typu integer na typ factor
mean_nan_training_data[,cat_variables_columns] = lapply(mean_nan_training_data[,cat_variables_columns], factor)
zero_nan_training_data[,cat_variables_columns] = lapply(zero_nan_training_data[,cat_variables_columns], factor)

#usuniecie kolumn bedacych duplikatami
removed_mean_nan_training_data = mean_nan_training_data[, !duplicated(t(mean_nan_training_data))]
removed_zero_nan_training_data = zero_nan_training_data[, !duplicated(t(zero_nan_training_data))]

boruta_results_zero_nan = matrix(rep(list(), 120),nrow = 10, ncol = 12)
attributes_zero = zero_nan_training_data[,13:length(zero_nan_training_data)]

#pierwsza kolumna to indeks
#dla kazdego z 12 atrybutow Outcome Boruta jest uruchamiana 10 razy, jeden run to okolo 3 minut obliczen
for(i in 1:12) {
  single_outcome = zero_nan_training_data[,i]
  for(j in 1:10) {
    boruta_results_zero_nan[[(i-1)*10+j]] = getSelectedAttributes(TentativeRoughFix(Boruta(attributes_zero, single_outcome, maxRuns = 100)))
  }
}

boruta_results_mean_nan = matrix(rep(list(), 120),nrow = 10, ncol = 12)
attributes_mean = mean_nan_training_data[,13:length(mean_nan_training_data)]

for(i in 1:12) {
  single_outcome = mean_nan_training_data[,i]
  for(j in 1:10) {
    boruta_results_mean_nan[[(i-1)*10+j]] = getSelectedAttributes(TentativeRoughFix(Boruta(attributes_mean, single_outcome, maxRuns = 100)))
  }
}

boruta_results_removed_zero_nan = matrix(rep(list(), 120),nrow = 10, ncol = 12)
removed_attributes_zero = removed_zero_nan_training_data[,13:length(removed_zero_nan_training_data)]

for(i in 1:12) {
  single_outcome = removed_zero_nan_training_data[,i]
  for(j in 1:10) {
    boruta_results_removed_zero_nan[[(i-1)*10+j]] = getSelectedAttributes(TentativeRoughFix(Boruta(removed_attributes_zero, single_outcome, maxRuns = 100)))
  }
}

boruta_results_removed_mean_nan = matrix(rep(list(), 120),nrow = 10, ncol = 12)
removed_attributes_mean = removed_mean_nan_training_data[,13:length(removed_mean_nan_training_data)]

for(i in 1:12) {
  single_outcome = removed_mean_nan_training_data[,i]
  for(j in 1:10) {
    boruta_results_removed_mean_nan[[(i-1)*10+j]] = getSelectedAttributes(TentativeRoughFix(Boruta(removed_attributes_mean, single_outcome, maxRuns = 100)))
  }
}

boruta_results = list(boruta_results_zero_nan, boruta_results_mean_nan, boruta_results_removed_zero_nan, boruta_results_removed_mean_nan)
training_datasets = list(zero_nan_training_data, mean_nan_training_data, zero_nan_training_data, mean_nan_training_data)
test_datasets = list(zero_nan_test_data, mean_nan_test_data, zero_nan_test_data, mean_nan_test_data)
directories_names = list("boruta_zero_nan", "boruta_mean_nan", "boruta_removed_duplicates_zero_nan", "boruta_removed_duplicates_mean_nan")

for(i in 1:4) {
  boruta_result = boruta_results[[i]]
  training_dataset = training_datasets[[i]]
  test_dataset = test_datasets[[i]]
  directory_name = directories_names[[i]]
  dir.create(file.path("../Data_raw", directory_name), showWarnings = FALSE)
  
  #wybranie atrybutow wspolnych dla wszystkich prob dla danej zmiennej outcome
  column_selected_attributes = matrix(rep(list(), 12),nrow = 10, ncol = 1)
  for (k in 1:12){
    intersect_selected_attributes = boruta_result[[(k-1)*10 + 1]]
    for (i in 2:10) {
      intersect_selected_attributes = intersect(boruta_result[[(k-1)*10 + i]],intersect_selected_attributes)
    }
    column_selected_attributes[[k]] = intersect_selected_attributes
  }
  
  #polaczenie wszystkich atrybutow dla wszystkich kolumn
  union_selected_attributes = column_selected_attributes[[1]]
  for (k in 2:12){
    union_selected_attributes = union(column_selected_attributes[[k]],union_selected_attributes)
  }
  
  #zamiana wybranych zmiennych kategorycznych na dummy variables
  selected_cat_variables_columns = grep("Cat_", union_selected_attributes)
  selected_cat_variables_names = union_selected_attributes[selected_cat_variables_columns]
  
  selected_training_dummy_variables= c()
  selected_test_dummy_variables= c()
  
  for (cat_variable in selected_cat_variables_names){
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
  
  #zapisanie danych treningowych i testowych dla kazdej zmiennej Outcome w oddzielnym pliku
  for (k in 1:12){
    column_selected_training_data = cbind(training_dataset[(k+1)],training_dataset[column_selected_attributes[[k]]])
    column_selected_test_data = cbind(test_dataset[(k+1)],test_dataset[column_selected_attributes[[k]]])
    write.csv(column_selected_training_data, file = sprintf("../Data_raw/%s/BorutaSelectedTrainingData_Outcome_M%s.csv", directory_name, k),row.names=FALSE)
    write.csv(column_selected_test_data, file = sprintf("../Data_raw/%s/BorutaSelectedTestData_Outcome_M%s.csv", directory_name, k), row.names=FALSE)
    
    column_selected_cat_variables_columns = grep("Cat_", column_selected_attributes[[k]])
    column_selected_cat_variables_names = column_selected_attributes[[k]][column_selected_cat_variables_columns]
    column_selected_dummy_variables_names = paste(column_selected_cat_variables_names, "_", sep="")
    
    column_selected_training_dummy_variables_columns = grep(paste(column_selected_dummy_variables_names, collapse="|"), colnames(selected_training_dummy_variables))
    column_selected_training_dummy_variables = selected_training_dummy_variables[,column_selected_training_dummy_variables_columns]
    
    column_selected_test_dummy_variables_columns = grep(paste(column_selected_dummy_variables_names, collapse="|"), colnames(selected_test_dummy_variables))
    column_selected_test_dummy_variables = selected_test_dummy_variables[,column_selected_test_dummy_variables_columns]
    
    column_selected_dummy_training_data = cbind(column_selected_training_data[ , !(names(column_selected_training_data) %in% column_selected_cat_variables_names)],column_selected_training_dummy_variables)
    column_selected_dummy_test_data = cbind(column_selected_test_data[ , !(names(column_selected_test_data) %in% column_selected_cat_variables_names)],column_selected_test_dummy_variables)
    
    write.csv(column_selected_dummy_training_data, file = sprintf("../Data_raw/%s/BorutaSelectedDummyTrainingData_Outcome_M%s.csv", directory_name, k),row.names=FALSE)
    write.csv(column_selected_dummy_test_data, file = sprintf("../Data_raw/%s/BorutaSelectedDummyTestData_Outcome_M%s.csv", directory_name, k), row.names=FALSE)
  }
}