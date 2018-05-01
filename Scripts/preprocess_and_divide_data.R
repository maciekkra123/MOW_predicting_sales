#ten skrypt preporcesuje dane wejsciowe, zastepujac NaNy, i rodziela je na zbiory treningowy i testowy

input_data = read.csv("../Data_raw/Dataset_old.csv", na.strings="NaN")

date_variables_names = c("Date_1", "Date_2")

outcome_variables_names = paste("Outcome_M", 1:12, sep="")

cat_variables_columns = grep("Cat_", names(input_data))

quant_variables_columns = grep("Quan_", names(input_data))

#usuniecie dwoch wierszy z brakujaca informacja o zmiennej Date_2
input_data = input_data[complete.cases(input_data[date_variables_names]),]

#zamiana zmiennych Cat z typu integer na typ factor
input_data[,cat_variables_columns] = lapply(input_data[,cat_variables_columns], factor)

#usuniecie kolumn z zerowa wariancja (o tych samych wartosciach)
#input_data[,apply(input_data, 2, var, na.rm=TRUE) != 0]

#zastapienie NaNow w zmiennych Quan ich srednimi
for(i in quant_variables_columns){
    input_data[is.na(input_data[,i]),i] = mean(input_data[,i], na.rm = TRUE)
}
#zastapienie NaNow w zmiennych Cat ich medianami -> nie dotyczy
#for(i in cat_variables_columns){
#    input_data[is.na(input_data[,i]),i] = median(as.numeric(as.character(input_data[,i])), na.rm = TRUE)
#}

#znalezienie wierszy z pelna informacja o sprzedazy
complete_outcome_information = input_data[complete.cases(input_data[outcome_variables_names]),]

#znalezienie wierszy z niepelna informacja o sprzedazy - jedna ze zmiennych Outcome == NaN
incomplete_outcome_information = input_data[!complete.cases(input_data[outcome_variables_names]),]

#losowa probka 10% wierszy o pelnej informacji sprzedazy do testow
random_sample = sample(1:nrow(complete_outcome_information), round(0.1*nrow(complete_outcome_information)))

#polaczenie tej probki z wierszami o niepelnej informacji sprzedazy tworzy zbior testowy
test_data = rbind(complete_outcome_information[random_sample,],incomplete_outcome_information)

training_data = complete_outcome_information[-random_sample,]

write.csv(training_data, file = "../Data_raw/TrainingData.csv")

write.csv(test_data, file = "../Data_raw/TestData.csv")


