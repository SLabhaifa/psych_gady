#give this function the paths variable and it will load thresholds into the staircase commands template and
#save it in the experiment plan for Unreal

load_thresholds_fmri<-function(folder_names,sub_n,study){
  
  sub_folder_name<-unlist(folder_names)[1]
  results_folder_name<-unlist(folder_names)[2]
  input_folder_name<-unlist(folder_names)[3]
  
#load thresholds csv: filename+subject, folder path

#set the filename, always take the file called Answers...something...csv
filename_t<-list.files(here("Studies",study,sub_folder_name,results_folder_name), pattern=glob2rx("threshold_values*.csv"))[1]
#read JND output csv filename
thresholds<-import(here("Studies",study,sub_folder_name,results_folder_name,filename_t));

#load StaircaseCommands template: folder path, filename


#filename_temp<-list.files(here("unreal_random_temps"), pattern=glob2rx())
#read staircase template csv filename
SCtemplate<-import(here("unreal_random_temps","StaircaseCommands_imaging_template.csv"));
#place thresholds in template

#filter 2nd and 4th levels
thresholds_lvls<-thresholds %>% filter(Level==1|Level==2)

#place the shifted threshold values in the initial value column
SCtemplate$initialValue[-1]<-thresholds_lvls$shifted_threshold
SCtemplate$initialValue<-as.numeric(SCtemplate$initialValue)

#add two right side plant rows
two_plant_rows<-SCtemplate[SCtemplate$Condition=="Plant",]
#make them negative and change the staircase codes to 3812, 3814
two_plant_rows$"#stairCaseSerialID"[1]<-3812
two_plant_rows$"#stairCaseSerialID"[2]<-3814

two_plant_rows$initialValue[1]<-two_plant_rows$initialValue[1]*(-1)
two_plant_rows$initialValue[2]<-two_plant_rows$initialValue[2]*(-1)

#add the two new rows to the SCtemplate
SCtemplate<-rbind(SCtemplate,two_plant_rows)

#remove first column we do not need it
SCtemplate<-SCtemplate[,-1]
#colnames(SCtemplate)[1]<-"#stairCaseSerialID"

#send template to experiment folder with changed name and save a copy
#exp_f_folder<-"C:\\Users\\User\\OneDrive\\Desktop\\AliceWindows-20231015-V6.38.2\\UnrealData\\Plans\\Unreal"

filename<-"StaircaseCommands.csv"

export(SCtemplate,here("Experiment","UnrealData","Plans","Unreal",filename),col.names=TRUE)
}
