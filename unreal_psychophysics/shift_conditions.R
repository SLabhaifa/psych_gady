shift_conditions<-function(){

#using the adjusted threshold file instead of the original in case of adjusted thresholds
filename_fixed_thresholds<-gsub(" ","",paste("threshold_values_sub_",as.character(sub_n),".csv"))
jnd_adjusted_thresholds<-import(here("Studies",study,sub_folder_name,results_folder_name,filename_fixed_thresholds))

#correcting some condition scales
jnd_adjusted_thresholds$shifted_threshold<-jnd_adjusted_thresholds$adj_stim_val
#Slow is shifted to start at 1
jnd_adjusted_thresholds$shifted_threshold[jnd_adjusted_thresholds$Condition=='Slow'] <- jnd_adjusted_thresholds$shifted_threshold[jnd_adjusted_thresholds$Condition=='Slow']+1
#Fast is shifted to start at 1
jnd_adjusted_thresholds$shifted_threshold[jnd_adjusted_thresholds$Condition=='Fast'] <- jnd_adjusted_thresholds$shifted_threshold[jnd_adjusted_thresholds$Condition=='Fast']+1
#Heavy is rescaled to less than -9.81
jnd_adjusted_thresholds$shifted_threshold[jnd_adjusted_thresholds$Condition=='Heavy'] <- jnd_adjusted_thresholds$shifted_threshold[jnd_adjusted_thresholds$Condition=='Heavy']-9.81
#Light is rescaled between -9.81 and 0
jnd_adjusted_thresholds$shifted_threshold[jnd_adjusted_thresholds$Condition=='Light'] <- jnd_adjusted_thresholds$shifted_threshold[jnd_adjusted_thresholds$Condition=='Light']-9.81

jnd_adjusted_thresholds$max_shifted_strength<-jnd_adjusted_thresholds$max_strength
#Slow is shifted to start at 1
jnd_adjusted_thresholds$max_shifted_strength[jnd_adjusted_thresholds$Condition=='Slow'] <- jnd_adjusted_thresholds$max_shifted_strength[jnd_adjusted_thresholds$Condition=='Slow']+1
#Fast is shifted to start at 1
jnd_adjusted_thresholds$max_shifted_strength[jnd_adjusted_thresholds$Condition=='Fast'] <- jnd_adjusted_thresholds$max_shifted_strength[jnd_adjusted_thresholds$Condition=='Fast']+1
#Heavy is rescaled to less than -9.81
jnd_adjusted_thresholds$max_shifted_strength[jnd_adjusted_thresholds$Condition=='Heavy'] <- jnd_adjusted_thresholds$max_shifted_strength[jnd_adjusted_thresholds$Condition=='Heavy']-9.81
#Light is rescaled between -9.81 and 0
jnd_adjusted_thresholds$max_shifted_strength[jnd_adjusted_thresholds$Condition=='Light'] <- jnd_adjusted_thresholds$max_shifted_strength[jnd_adjusted_thresholds$Condition=='Light']-9.81

#save shifted thresholds in results folder
filename_th<-gsub(" ","",paste("threshold_values_sub_",as.character(sub_n),".csv"))
export(jnd_adjusted_thresholds,here("Studies",study,sub_folder_name,results_folder_name,filename_th))

return(jnd_adjusted_thresholds)
}