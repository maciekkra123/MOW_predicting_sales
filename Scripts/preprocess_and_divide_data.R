#ten skrypt preporcesuje dane wejsciowe, zastepujac NaNy, i rodziela je na zbiory treningowy i testowy

input_data = read.csv("../Data_raw/Dataset_old.csv", na.strings="NaN")

date_variables_names = c("Date_1", "Date_2")

outcome_variables_names = paste("Outcome_M", 1:12, sep="")

#outcome_variables_names = grep("Outcome_", names(input_data))

cat_variables_columns = grep("Cat_", names(input_data))

quant_variables_columns = grep("Quan", names(input_data))

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

mean_nan_input_data = input_data

#zastapienie NaNow w zmiennych Quan ich srednimi <- inna opcja jest zastapienie NaNow zerami, potem zostawimy lepsza z nich
#zastapienie Nanow jest niezbedne dla dzialania Boruty
#do obliczenia sredniej uzywana jest cala informacja (rowniez z wierszy, ktore znajda sie w testsecie), dlatego wykonywane jest teraz
for(i in quant_variables_columns){
  mean_nan_input_data[is.na(mean_nan_input_data[,i]),i] = mean(mean_nan_input_data[,i], na.rm = TRUE)
  #input_data[is.na(input_data[,i]),i] = 0
}

#zastapienie NaNow w zmiennych Cat ich medianami -> nie ma takich przypadkow
#for(i in cat_variables_columns){
#    input_data[is.na(input_data[,i]),i] = median(as.numeric(as.character(input_data[,i])), na.rm = TRUE)
#}

#znalezienie wierszy z pelna informacja o sprzedazy
complete_cases = complete.cases(input_data[outcome_variables_names])

complete_outcome_information = input_data[complete_cases,]
mean_nan_complete_outcome_information = mean_nan_input_data[complete_cases,]

#znalezienie wierszy z niepelna informacja o sprzedazy - jedna ze zmiennych Outcome == NaN
incomplete_outcome_information = input_data[!complete_cases,]
mean_nan_incomplete_outcome_information = mean_nan_input_data[!complete_cases,]

#losowa probka 10% wierszy o pelnej informacji sprzedazy do testow, uzywamy tej samej probki
random_sample = sample(1:nrow(complete_outcome_information), round(0.1*nrow(complete_outcome_information)))

#polaczenie tej probki z wierszami o niepelnej informacji sprzedazy tworzy zbior testowy
test_data = rbind(complete_outcome_information[random_sample,],incomplete_outcome_information)
mean_nan_test_data = rbind(mean_nan_complete_outcome_information[random_sample,],mean_nan_incomplete_outcome_information)

#do trainsetu trafiaja wszystkie wiersze z pelna informacja o sprzedazy oprocz tej probki
training_data = complete_outcome_information[-random_sample,]
mean_nan_training_data = mean_nan_complete_outcome_information[-random_sample,]

write.csv(training_data, file = "../Data_raw/RawTrainingData.csv", row.names=FALSE)
write.csv(test_data, file = "../Data_raw/RawTestData.csv", row.names=FALSE)

write.csv(mean_nan_training_data, file = "../Data_raw/MeanNanTrainingData.csv", row.names=FALSE)
write.csv(mean_nan_test_data, file = "../Data_raw/MeanNanTestData.csv", row.names=FALSE)

#znajdz liczbe nanow w kolumnie (dla celow pogladowych)
#number_of_nans = apply(apply(input_data,2,is.na),2,sum)
#number_of_nans = number_of_nans[which(number_of_nans>0)]
