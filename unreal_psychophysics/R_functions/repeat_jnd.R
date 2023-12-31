repeat_jnd<-function(study_number=readline("Study number:")){
  
#{study_number,attempt}
#you need to run the function and it will prompt you with these questions
#attempt<-1;
#sub_n<-003;
  ``
library(data.table) 
library(ggplot2) 
library(dplyr)
library(tidyr)
library(cowplot)
library(magicfor)
library(stringi)
  
  
  sub_n<- readline(prompt = "Subject number:");
  attempt<- readline(prompt = "Staircase attempt number:"); 
  subject_folder<-gsub(" ","",paste("C:\\Users\\User\\OneDrive\\Desktop\\unreal_psychophysics\\unreal_",study_number,"\\sub_",sub_n))
  
  setwd("C:\\Users\\User\\OneDrive\\Desktop\\unreal_psychophysics\\rdas")
  load("psy_paths.rda")
  load("run_unreal_psy.rda")
  paths<-psy_paths(subject_folder,attempt,sub_n)
  subject_folder<-paths[1]
  psychophysics_folder<-paths[2]
  output_matlab<-paths[3]
  unreal_random_temps<-paths[4]
  results_folder<-paths[5]
  unreal_input_folder<- paths[6]
  rda_folder<-paths[7]
  
  repeat_number<-as.numeric(attempt)-1;
  
  setwd(subject_folder)
  
#load the answers csv of the repeated staircases
repeat_file<-list.files(subject_folder, pattern=glob2rx("*repeat*.csv"));
repeat_data<-read.csv(file=repeat_file)
unlink(repeat_file)

#load the previous attempt csv (there should be only one attempt file in the folder)
previous_attempt_file<-list.files(subject_folder, pattern=glob2rx("Answers*.csv"));
previous_attempt_data<-read.csv(file=previous_attempt_file)
unlink(previous_attempt_file)

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
fwrite(previous_attempt_plus_repeat,filename,col.names=TRUE)

run_unreal_psy(study_number=study_number,sub_n,attempt)
}

#save(repeat_jnd, file = 'C:\\Users\\User\\OneDrive\\Desktop\\unreal_psychophysics\\rdas\\repeat_jnd.rda')
