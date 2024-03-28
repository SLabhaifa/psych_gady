repeat_jnd<-function(sub_n = readline("Subject number:"),study=readline("Study folder name:"),attempt= readline(prompt = "Fitting attempt number:")){
  
#{study_number,attempt}
#you need to run the function and it will prompt you with these questions
#attempt<-1;
#sub_n<-003;
  cat("Make sure there is a repeat file and an answers file in the folder or repeat_jnd wont work")
library(data.table) 
library(ggplot2) 
library(dplyr)
library(tidyr)
library(cowplot)
library(magicfor)
library(stringi)
library(rio)
library(here)

sub_folder_name<-paste0("sub_",sub_n)
  
  
#load the answers csv of the repeated staircases
repeat_file<-list.files(here("Studies",study,sub_folder_name), pattern=glob2rx("*repeat*.csv"))[1];
repeat_data<-try(import(here("Studies",study,sub_folder_name,repeat_file)),silent=TRUE)

unlink(here("Studies",study,sub_folder_name,repeat_file))

#load the previous attempt csv (there should be only one attempt file in the folder)
previous_attempt_file<-list.files(here("Studies",study,sub_folder_name), pattern=glob2rx("Answers*.csv"))[1];
previous_attempt_data<-try(import(here("Studies",study,sub_folder_name,previous_attempt_file)),silent=TRUE)
unlink(here("Studies",study,sub_folder_name,previous_attempt_file))

#make a unique vector of the condition codes used in the repeat TrialNumber column
repeat_codes<-unique(repeat_data$TrialNumber)
#turn all codes into the 3 first digits
repeat_codes<-as.numeric(substr(repeat_codes,1,3))
#make a new column of codes with only 3 digits or less
previous_attempt_data<-previous_attempt_data %>% mutate(short_code = TrialNumber)
previous_attempt_data$short_code<-as.numeric(substr(previous_attempt_data$short_code,1,3))

#remove rows with these codes from previous attempt data
cleaned_previous_data<-previous_attempt_data  %>% filter(!short_code %in% repeat_codes)
#remove short_code column
cleaned_previous_data<-subset(cleaned_previous_data, select = -c(short_code))
#fix block number or step number for new conditions
last_block<-max(previous_attempt_data$BlockNumber)
#add block number to the block numbers in the repeat data
repeat_data$BlockNumber[repeat_data$BlockNumber!=0]<-repeat_data$BlockNumber[repeat_data$BlockNumber!=0]+last_block
#add the number of times this condition has been run
repeat_data<- repeat_data %>% mutate(Attempt = as.numeric(attempt))

#rbind repeat to cleaned attempt df
previous_attempt_plus_repeat<-rbind(cleaned_previous_data,repeat_data)

previous_attempt_file<-stri_sub(previous_attempt_file,1,-5)

#save the new data frame as an 'Answers' file
filename<-gsub(" ","",paste(previous_attempt_file,"_",as.character(attempt),"_.csv"))
export(previous_attempt_plus_repeat,here("Studies",study,sub_folder_name,filename),col.names=TRUE)

source(here("run_unreal_psy.R"))
 run_unreal_psy(sub_n=sub_n,study=study,attempt=attempt)
}


