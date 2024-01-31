#Psychophysics analysis & Unreal input creating script
run_unreal_psy<-function(study=readline("Study folder name:"),sub_n = readline("Subject number:"),attempt= readline(prompt = "Fitting attempt number:")){

  library(here)
  library(stringi)
  library(rio)
  
  #type run_unreal_psy() in the console
  #enter name of study: fmri
  #enter subject number:001
  #enter attempt number:1
  #check results in results folder
  
  #if you need to edit after a repeat, place the new result in the results folder
  #and type repeat_jnd() in the console
  #enter subject number:001
  #enter attempt number:1


#define folders path using here()
sub_folder_name<-gsub(" ","",paste("sub_",sub_n))
subject_folder<-here("Studies",study,sub_folder_name)
results_folder<-here(study,sub_folder_name,gsub(" ","",paste("results_sub_",sub_n,'_attempt_',as.character(attempt))))
unreal_input_folder<- gsub(" ","",paste(results_folder,"/unreal_input_sub_",sub_n,'_attempt_',as.character(attempt)))

results_folder_name<-gsub(" ","",paste("results_sub_",sub_n,'_attempt_',as.character(attempt)))
input_folder_name<- gsub(" ","",paste("unreal_input_sub_",sub_n,'_attempt_',as.character(attempt)))

#list of folder names for other functions
folder_names<-list(sub_folder_name,results_folder_name,input_folder_name)

#create subject results folder
here(study,sub_folder_name,try(dir.create(results_folder),silent=TRUE))

#create subject input folder for next experiment
here(study,sub_folder_name,results_folder,try(dir.create(unreal_input_folder),silent=TRUE))


load(here("rdas","name_conditions_and_domains.rda"))
load(here("rdas","unreal_psy.rda"))
load(here("rdas","load_thresholds.rda"))
load(here("rdas","repeat_list.rda"))

#run the main function
unreal_psy(folder_names,attempt,sub_n,study)

#place stimuli in empty input file
load_thresholds(folder_names,sub_n)
load_thresholds_fmri(folder_names,sub_n,study)

#doAgain contains a list of the conditions we need to redo
load(here("rdas","repeat_jnd.rda"))

experiment_folder_path<-here("Unreal_Experiment","UnrealData","Plans","repeat_JND")
basedir<-here()

doAgain_f<-paste0("doAgain_values_sub_",sub_n,".csv")
if (file.exists(here("output_matlab",doAgain_f))){
  doAgain<-import(here("output_matlab",doAgain_f),header=TRUE, stringsAsFactors = TRUE);
  repeat_list(folder_names,sub_n,attempt,doAgain,experiment_folder_path)
  } else {cat('\n','No repeats needed','\n')}
}

#save(run_unreal_psy, file = 'C:\\Users\\User\\OneDrive\\Desktop\\unreal_psychophysics\\rdas\\run_unreal_psy.rda')
