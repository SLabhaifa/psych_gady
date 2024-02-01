source(here("preprocessing.R"))
source(here("name_conditions_and_domains.R"))
source(here("post_fit_adjustments.R"))
source(here("threshold_fix.R"))
source(here("shift_conditions.R"))
source(here("plot_staircase.R"))
source(here("plot_staircase_unity.R"))
source(here("repeat_jnd.R"))
source(here("repeat_list.R"))

study=readline("Study folder name:")
sub_n = readline("Subject number:")
attempt= readline(prompt = "Fitting attempt number:")

sub_folder_name<-gsub(" ","",paste("sub_",sub_n))
subject_folder<-here("Studies",study,sub_folder_name)

results_folder_name<-gsub(" ","",paste("results_sub_",sub_n,'_attempt_',as.character(attempt)))
results_folder<-here("Studies",study,sub_folder_name,results_folder_name)

input_folder_name<- gsub(" ","",paste("unreal_input_sub_",sub_n,'_attempt_',as.character(attempt)))
unreal_input_folder<- gsub(" ","",paste(results_folder,"/",input_folder_name))


#list of folder names
folder_names<-list(sub_folder_name,results_folder_name,input_folder_name)

#create subject results folder
here(study,sub_folder_name,try(dir.create(results_folder),silent=TRUE))

#create subject input folder for next experiment
here(study,sub_folder_name,results_folder,try(dir.create(unreal_input_folder),silent=TRUE))

data_cf<-preprocessing()

cat("Matlab is fitting the data","\n")
run_matlab_script(here("unreal_fit.m"),display=FALSE,verbose=FALSE)

jnd_thresholds<-post_fit_adjustments()

threshold_fix(jnd_thresholds,folder_names,sub_n,study)

jnd_adjusted_thresholds<-shift_conditions()


plot_staircase(con_name,data_cf,jnd_adjusted_thresholds)
plot_staircase_unity(con_name,data_cf,jnd_adjusted_thresholds)

#place stimuli in empty input file
load_thresholds(folder_names,sub_n)
load_thresholds_fmri(folder_names,sub_n,study)

#doAgain contains a list of the conditions we need to redo
experiment_folder_path<-here("Unreal_Experiment","UnrealData","Plans","repeat_JND")
basedir<-here()

doAgain_f<-paste0("doAgain_values_sub_",sub_n,".csv")
if (file.exists(here("output_matlab",doAgain_f))){
  doAgain<-import(here("output_matlab",doAgain_f),header=TRUE, stringsAsFactors = TRUE);
  repeat_list(folder_names,sub_n,attempt,doAgain,experiment_folder_path)
} else {cat('\n','No repeats needed','\n')}
