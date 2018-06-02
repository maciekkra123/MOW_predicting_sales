#ten skrypt przeprowadza selekcje atrybutow wykorzystujac algorytm Boruta

library(Boruta)
library(dummies)

traindata_mean = read.csv("../Data_raw/MeanNanTrainingData.csv", header = TRUE)
traindata_zero = read.csv("../Data_raw/RawTrainingData.csv", header = TRUE)

test_data = read.csv("../Data_raw/RawTestData.csv")

#zamiana zmiennych Cat z typu integer na typ factor
cat_variables_columns = grep("Cat_", names(traindata_zero))
cat_variables_names = names(traindata_zero)[cat_variables_columns]

quant_variables_columns = grep("Quan", names(traindata_zero))

#zastapienie NaNow zerami
for(i in quant_variables_columns){
  traindata_zero[is.na(traindata_zero[,i]),i] = 0
  test_data[is.na(test_data[,i]),i] = 0
}

#zamiana zmiennych Cat z typu integer na typ factor
traindata_mean[,cat_variables_columns] = lapply(traindata_mean[,cat_variables_columns], factor)
traindata_zero[,cat_variables_columns] = lapply(traindata_zero[,cat_variables_columns], factor)

#podobno dla wielu algorytmow problemem sa sytuacje, w ktorych w danych testowych pojawia sie wartosc atrybutu kategorycznego nieobecna w danych treningowych
#rozwiazanie - zamiana zmiennych kategorycznych na dummy variables
dummy_variables= c()

for (cat_variable in cat_variables_names){
  dummy_variables = cbind(dummy_variables, dummy(cat_variable, traindata_zero, sep="_"))
}

dummy_traindata_zero = cbind(traindata_zero[ , !(names(traindata_zero) %in% cat_variables_names)],dummy_variables)
#1763 atrybutow! po usunieciu duplikatow nadal jest ich 1292 (ewentualnie robic to po borucie?)

#usuniecie kolumn bedacych duplikatami
removed_traindata_mean = traindata_mean[, !duplicated(t(traindata_mean))]
removed_traindata_zero = traindata_zero[, !duplicated(t(traindata_zero))]

boruta_results = matrix(rep(list(), 120),nrow = 10, ncol = 12)
attributes_zero = traindata_zero[,14:length(traindata_zero)]

#pierwsza kolumna to indeks
#dla kazdego z 12 atrybutow Outcome Boruta jest puszczana 10 razy, jeden run to okolo 3 minuty obliczen
for(i in 2:13) {
  single_outcome = traindata_zero[,i]
  for(j in 1:10) {
    boruta_results[[(i-2)*10+j]] = getSelectedAttributes(TentativeRoughFix(Boruta(attributes_zero, single_outcome, maxRuns = 100)))
  }
}

boruta_results_mean_nan = matrix(rep(list(), 120),nrow = 10, ncol = 12)
attributes_mean = traindata_mean[,14:length(traindata_mean)]

for(i in 2:13) {
  single_outcome = traindata_mean[,i]
  for(j in 1:10) {
    boruta_results_mean_nan[[(i-2)*10+j]] = getSelectedAttributes(TentativeRoughFix(Boruta(attributes_mean, single_outcome, maxRuns = 100)))
  }
}

boruta_results_removed_mean_nan = matrix(rep(list(), 120),nrow = 10, ncol = 12)
removed_attributes_mean = removed_mean[,14:length(removed_mean)]


for(i in 2:13) {
  single_outcome = removed_mean[,i]
  for(j in 1:10) {
    boruta_results_removed_mean_nan[[(i-2)*10+j]] = getSelectedAttributes(TentativeRoughFix(Boruta(removed_attributes_mean, single_outcome, maxRuns = 100)))
  }
}

boruta_results_removed_zero_nan = matrix(rep(list(), 120),nrow = 10, ncol = 12)
removed_attributes_zero = removed_zero[,14:length(removed_zero)]

for(i in 2:13) {
  single_outcome = removed_zero[,i]
  for(j in 1:10) {
    boruta_results_removed_zero_nan[[(i-2)*10+j]] = getSelectedAttributes(TentativeRoughFix(Boruta(removed_attributes_zero, single_outcome, maxRuns = 100)))
  }
}

#wybranie atrybutow wspolnych dla wszystkich prob dla danej zmiennej outcome
column_selected_attributes = matrix(rep(list(), 12),nrow = 10, ncol = 1)
for (k in 1:12){
  intersect_selected_attributes = boruta_results[[(k-1)*10 + 1]]
  for (i in 2:10) {
    intersect_selected_attributes = intersect(boruta_results[[(k-1)*10 + i]],intersect_selected_attributes)
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
  selected_training_dummy_variables = cbind(selected_training_dummy_variables, dummy(cat_variable, traindata_zero, sep="_"))
  selected_test_dummy_variables = cbind(selected_test_dummy_variables, dummy(cat_variable, test_data, sep="_"))
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
  column_selected_training_data = cbind(traindata_zero[(k+1)],traindata_zero[column_selected_attributes[[k]]])
  column_selected_test_data = cbind(test_data[(k+1)],test_data[column_selected_attributes[[k]]])
  write.csv(column_selected_training_data, file = sprintf("../Data_raw/BorutaSelectedTrainingData_Outcome_M%s.csv", k),row.names=FALSE)
  write.csv(column_selected_test_data, file = sprintf("../Data_raw/BorutaSelectedTestData_Outcome_M%s.csv", k), row.names=FALSE)
  
  column_selected_cat_variables_columns = grep("Cat_", column_selected_attributes[[k]])
  column_selected_cat_variables_names = column_selected_attributes[[k]][column_selected_cat_variables_columns]
  column_selected_dummy_variables_names = paste(column_selected_cat_variables_names, "_", sep="")
  
  column_selected_training_dummy_variables_columns = grep(paste(column_selected_dummy_variables_names, collapse="|"), colnames(selected_training_dummy_variables))
  column_selected_training_dummy_variables = selected_training_dummy_variables[,column_selected_training_dummy_variables_columns]
  
  column_selected_test_dummy_variables_columns = grep(paste(column_selected_dummy_variables_names, collapse="|"), colnames(selected_test_dummy_variables))
  column_selected_test_dummy_variables = selected_test_dummy_variables[,column_selected_test_dummy_variables_columns]
  
  column_selected_dummy_training_data = cbind(column_selected_training_data[ , !(names(column_selected_training_data) %in% column_selected_cat_variables_names)],column_selected_training_dummy_variables)
  column_selected_dummy_test_data = cbind(column_selected_test_data[ , !(names(column_selected_test_data) %in% column_selected_cat_variables_names)],column_selected_test_dummy_variables)

  write.csv(column_selected_dummy_training_data, file = sprintf("../Data_raw/BorutaSelectedDummyTrainingData_Outcome_M%s.csv", k),row.names=FALSE)
  write.csv(column_selected_dummy_test_data, file = sprintf("../Data_raw/BorutaSelectedDummyTestData_Outcome_M%s.csv", k), row.names=FALSE)
}

#selected_training_data = cbind(traindata_zero[2:13],traindata_zero[union_selected_attributes])
#write.csv(selected_training_data, file = "../Data_raw/BorutaSelectedTrainingData.csv", row.names=FALSE)

#selected_test_data = cbind(test_data[2:13],test_data[union_selected_attributes])
#write.csv(selected_test_data, file = "../Data_raw/BorutaSelectedTestData.csv", row.names=FALSE)

#write.csv(selected_dummy_training_data, file = "../Data_raw/BorutaSelectedDummyTrainingData.csv", row.names=FALSE)

#write.csv(selected_dummy_test_data, file = "../Data_raw/BorutaSelectedDummyTestData.csv", row.names=FALSE)

#ponizej obliczenia dla celow sprawozdania

#wyliczenie, ile atrybutow wybranych zostalo poszczegolne liczby razy (ile roznych atrybutow zostalo wskazanych raz, ile dwa razy itd.)
results = matrix(, nrow = 12, ncol = 10)

for (k in 1:12){
  all_selected_attributes = boruta_results[[(k-1)*10 + 1]]
  for (i in 2:10) {
    all_selected_attributes = union(boruta_results[[(k-1)*10 + i]],all_selected_attributes)
  }
  number_of_occurences = all_selected_attributes
  for (i in 1:length(all_selected_attributes)){
    attribute = all_selected_attributes[i]
    number = 0
    for (j in 1:10){
      if (attribute %in% boruta_results[[(k-1)*10 + j]]){
        number = number + 1
      }
    }
    number_of_occurences[i] = number
  }
  for (i in 1:10){
    results[k,i] = length(which(number_of_occurences == i))
  }
}

#wyliczenie liczby wybran (przez kazda zmienna outcome z osobna) dla poszczegolnych wybranych atrybutow
numbers = c(rep(0,12))
for (attribute in union_selected_attributes){
  number = 0
  for (j in 1:12){
    if (attribute %in% column_selected_attributes[[j]]){
      number = number + 1
    }
  }
  numbers[number] = numbers[number] + 1
}








