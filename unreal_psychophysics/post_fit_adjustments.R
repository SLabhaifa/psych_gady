post_fit_adjustments<-function(sub_n,study,attempt,sub_folder_name,results_folder_name,ConditionName_vec,data_cf){

fitted_file<-gsub(" ","",paste("threshold_values_sub_",sub_n,".csv"))
jnd_thresholds<-import(here("output_matlab",fitted_file))

#set the filename, always take the file called Answers*
filename_ans<-list.files(here("Studies",study,sub_folder_name), pattern=glob2rx("Answers*.csv"))[1]

#removing any imaginary parts from the thresholds
jnd_thresholds$my_thresholds_real<-Re(as.complex(jnd_thresholds$my_thresholds))
jnd_thresholds<- jnd_thresholds %>% mutate(Subject = sub_n)
jnd_thresholds<- jnd_thresholds %>% mutate(Study = study)
jnd_thresholds<- jnd_thresholds %>% mutate(Filename = filename_ans)
jnd_thresholds<- jnd_thresholds %>% mutate(Level = prob)
jnd_thresholds$Level[jnd_thresholds$Level==0.25]<-1
jnd_thresholds$Level[jnd_thresholds$Level==0.5]<-2
jnd_thresholds$Level[jnd_thresholds$Level==0.75]<-3
jnd_thresholds$Level[jnd_thresholds$Level==0.95]<-4

#convert thresholds back from log scale
jnd_thresholds$jnd_anti_log<-expm1(jnd_thresholds$my_thresholds_real)

#add max_strength column to jnd_thresholds
jnd_thresholds$max_strength<-jnd_thresholds$my_thresholds_real

for (e in ConditionName_vec){
  jnd_thresholds[jnd_thresholds$Condition==e,"max_strength"]<-data_cf[data_cf$ConditionName==e,"max_strength"][[1,1]]
  
}
jnd_thresholds$jnd_weighted_threshold<-jnd_thresholds$max_strength
jnd_thresholds$jnd_weighted_threshold<-jnd_thresholds$max_strength*jnd_thresholds$jnd_anti_log/100
return(jnd_thresholds)
}


