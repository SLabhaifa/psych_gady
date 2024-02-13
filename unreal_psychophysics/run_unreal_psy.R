run_unreal_psy<-function(study=readline("Study folder name:"),sub_n = readline("Subject number:"),attempt= readline(prompt = "Fitting attempt number:")){

library(here)
source(here("preprocessing.R"))
source(here("name_conditions_and_domains.R"))
source(here("post_fit_adjustments.R"))
source(here("threshold_fix.R"))
source(here("shift_conditions.R"))
source(here("plot_staircase.R"))
source(here("plot_staircase_unity.R"))
source(here("repeat_list.R"))
source(here("make_plots.R"))
source(here("load_thresholds.R"))
source(here("load_thresholds_fmri.R"))
#source(here("repeat_jnd.R"))


sub_folder_name<-gsub(" ","",paste("sub_",sub_n))
subject_folder<-here("Studies",study,sub_folder_name)

results_folder_name<-gsub(" ","",paste("results_sub_",sub_n,'_attempt_',as.character(attempt)))
results_folder<-here("Studies",study,sub_folder_name,results_folder_name)

input_folder_name<- gsub(" ","",paste("unreal_input_sub_",sub_n,'_attempt_',as.character(attempt)))
unreal_input_folder<- gsub(" ","",paste(results_folder,"/",input_folder_name))


#list of folder names
folder_names<-list(sub_folder_name,results_folder_name,input_folder_name)

#create subject results folder
here(study,sub_folder_name,try(dir.create(results_folder,warn_exists=FALSE),silent=TRUE))

#create subject input folder for next experiment
here(study,sub_folder_name,results_folder,try(dir.create(unreal_input_folder,warn_exists=FALSE),silent=TRUE))

data_cf<-preprocessing(sub_n,study,attempt,sub_folder_name,results_folder_name)

cat("\033[1;36mMatlab is fitting the data","\n")
run_matlab_script(here("unreal_fit.m"),display=FALSE,verbose=FALSE)

#save the matlab plots
m_plot_name<-paste0(sub_n,"_fits.png")
file.rename(from=here(m_plot_name),to=here("Studies",study,sub_folder_name,results_folder_name,m_plot_name))

ConditionName_vec<-unique(as.vector(data_cf$ConditionName))
ConditionName_vec<-ConditionName_vec[order(ConditionName_vec)]

jnd_thresholds<-post_fit_adjustments(sub_n,study,attempt,sub_folder_name,results_folder_name,ConditionName_vec,data_cf)

threshold_fix(jnd_thresholds,folder_names,sub_n,study)

jnd_adjusted_thresholds<-shift_conditions(sub_n,study,sub_folder_name,results_folder_name)

make_plots(sub_n,study,attempt,sub_folder_name,results_folder_name,data_cf,jnd_adjusted_thresholds,ConditionName_vec)

#place stimuli in empty input file
load_thresholds(folder_names,sub_n,study)

if (study=="fmri"){
  load_thresholds_fmri(folder_names,sub_n,study)
}

#doAgain contains a list of the conditions we need to redo
experiment_folder_path<-here("Experiment","UnrealData","Plans","repeat_JND")
basedir<-here()

doAgain_f<-paste0("doAgain_values_sub_",sub_n,".csv")
if (file.exists(here("output_matlab",doAgain_f))){
  doAgain<-import(here("output_matlab",doAgain_f),header=TRUE, stringsAsFactors = TRUE);
  repeat_list(folder_names,sub_n,attempt,doAgain,experiment_folder_path)
  cat('\n','                                                \033[1;31m !!! Bad fits were found !!! \033[0m','\n')
  cat('\n','                                        \033[1;31m !!! Run the repeat file and try again !!! \033[0m','\n')
} else {cat('\n','No repeats needed','\n')}

cat('\n','* * * * * * * * * * * * * * * * * * * * * * * * * * * * * The End * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *','\n')
}
