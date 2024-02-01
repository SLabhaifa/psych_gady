source(here("preprocessing.R"))
source(here("name_conditions_and_domains.R"))


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

preprocessing()

