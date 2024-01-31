unreal_psy<-function(folder_names,attempt,sub_n,study){
  
  library(matlabr)
  library(data.table) 
  library(ggplot2) 
  library(dplyr) 
  library(tidyr) 
  library(cowplot) 
  library(magicfor)
  library(stringi)
  library(scales)
  library(ggpubr)
  library(formattable)
  library(crayon)
  library(rio)
  
  sub_folder_name
  results_folder_name
  input_folder_name<-unlist(folder_names)[3]
  
  #load functions
  load(here("rdas","name_conditions_and_domains.rda"))
  load(here("rdas","threshold_fix.rda"))
  
  
  #set the filename, always take the file called Answers*
  filename_ans<-list.files(here("Studies",study,sub_folder_name), pattern=glob2rx("Answers*.csv"))[1]
  #read JND output csv filename
  data<-import(here("Studies",study,sub_folder_name,filename_ans))
  
  #check if this is the first attempt at fitting
  if (attempt == 1){
    data<- data %>% mutate(Attempt = attempt)
  }
  
  # save file for record
  # add Attempt column so we can keep track of how many repeated conditions there were
  file_rename<-gsub(" ","",paste('Attempt_',as.character(attempt),'_',filename_ans))
  export(data,here("Studies",study,sub_folder_name,results_folder_name,file_rename))#the file has column names no worries
  
  #saving so we can attach to repeat files
  export(data,here("Studies",study,sub_folder_name,filename_ans))
  

  ######################################## Editing the data frame #########################
  
  #remove throw away trials (blocknumber==0)
  data<-data[!(data$BlockNumber==0),]
  #filter the df so that only staircase rows remain
  filter_questions <-data$QuestionID==88
  data<-data[filter_questions,]
  #filter trials with no answer
  data<-data[!(data$QuestionResult=="NoAnswerInTime"),]
  
  #removing columns we dont need right now
  data_c<-subset(data, select=-c(ProbabilityComment,TimesatmpInitialDirection,InitialDirection,SessionID,Phase,QuestionID,TimeOfVaseHit,NegativeAnswersCount,PositiveAnswersCount,ConvertedValue))

  #rename some df columns
  colnames(data_c)[1]<-"Block"
  colnames(data_c)[2]<-"ConditionCode"
  colnames(data_c)[6]<-"TrialNumber"
  
  #adding within-staircase trial index column called 'StepNumber'
  data_c<-data_c %>% dplyr::group_by(BlockNumber) %>% dplyr::mutate(StepNumber = dplyr::row_number())
  #creating a condition_name column
  data_c<-data_c %>% mutate(ConditionName = ConditionCode)
  #creating a domain column
  data_c <- data_c %>% mutate(Domain = ConditionName)
  #start making a reversals column
  data_c<-data_c%>%mutate(ReversalPoints=QuestionResult)
  #adding condition & domain names
  data_c<-name_conditions_and_domains(data_c)
  
  #reorder columns
  col_order <- c("TrialNumber", "BlockNumber","StepNumber","ConditionCode","ConditionName","Domain",
                 "QuestionResult","ReversalPoints","StairCaseValue","TimeStampStartQuestion","TimestampEndQuestion","ResponseTime","Attempt")
  data_c <- data_c[, col_order]
  #save a data frame of the data so far (data_c)
  data_cs<-data_c
  
  #change columns to factors and 0/1 for rev points calculation
  data_cs$ReversalPoints<-as.factor(data_cs$ReversalPoints)
  data_cs$QuestionResult<-as.factor(data_cs$QuestionResult)
  data_cs$ReversalPoints<-droplevels(data_cs$ReversalPoints)
  data_cs$QuestionResult<-droplevels(data_cs$QuestionResult)
  
  #################################### Calculating the reversal points column ###############################
  #This means we are checking the difference between question results every two steps (per condition)
  #Note: the ReversalPoints column currently contains the QuestionResults (i.e. 1's and 0's)
  
  index<-unique(data_cs$BlockNumber)
  for (x in index){
    diffs<-diff(data_cs[data_cs$BlockNumber==x,]$ReversalPoints)
    diffs<- c(0,diffs)
    diffs<-(abs(diffs))
    data_cs[data_cs$BlockNumber==x,]$ReversalPoints<-as.factor(diffs)
  }
  #now the ReversalPoints column represents when a reversal in decision was made.
  
  ####################################### Which conditions are going to be analyzed/plotted #################
      ConditionName_vec<-unique(as.vector(data_cs$ConditionName))
      ConditionName_vec<-ConditionName_vec[order(ConditionName_vec)]
  
  ########################################### Create percent & log of percent columns ##############################################
  
  data_cs$percent<-data_cs$StairCaseValue
  data_cs$log_percent<-data_cs$StairCaseValue
  data_cs$SCV_abs<-abs(data_cs$StairCaseValue)
  data_cs$max_strength<- data_cs$StairCaseValue
  ########################################### Create a ReversalVal column ##########################################################
  #loop by condition name
  for (z in 1:length(ConditionName_vec)){
    #we don't consider reversals that happened in the first 4 steps, they are most likely mistakes.
    data_cs[data_cs$ConditionName==ConditionName_vec[z],]$ReversalPoints[1:4]<-0;
    #calculate percents
    data_cs[data_cs$ConditionName==ConditionName_vec[z],]$percent<- data_cs[data_cs$ConditionName==ConditionName_vec[z],]$SCV_abs/max(data_cs[data_cs$ConditionName==ConditionName_vec[z],]$SCV_abs)*100
    #calculate logs of percents
    data_cs[data_cs$ConditionName==ConditionName_vec[z],]$log_percent<- log1p(data_cs[data_cs$ConditionName==ConditionName_vec[z],]$percent)
    #collect max values in each staircase
    if (data_cs[data_cs$ConditionName==ConditionName_vec[z],]$StairCaseValue[1]>0){
    data_cs[data_cs$ConditionName==ConditionName_vec[z],]$max_strength<- max(data_cs[data_cs$ConditionName==ConditionName_vec[z],]$StairCaseValue)}
    else{data_cs[data_cs$ConditionName==ConditionName_vec[z],]$max_strength<- min(data_cs[data_cs$ConditionName==ConditionName_vec[z],]$StairCaseValue)}
  }
  
  data_cs$ReversalPoints<-as.numeric(as.character(data_cs$ReversalPoints))
  data_cs<- data_cs %>% mutate(ReversalVal = ReversalPoints*SCV_abs)
  data_cs$ReversalVal[data_cs$ReversalVal==0]<-NA
  data_cs$ReversalPoints<-as.factor(data_cs$ReversalPoints)
  data_cs$QuestionResult<-as.numeric(as.character(data_cs$QuestionResult));
  
  #add filename, subject and study number columns
  data_cs<- data_cs %>% mutate(Subject = sub_n)
  data_cs<- data_cs %>% mutate(Study = study)
  data_cs<- data_cs %>% mutate(Filename = filename_ans)
  
  data_cf<-data_cs;
  
  #change the answers encoding so we can fit the data
  data_cf$QuestionResult<-(data_cf$QuestionResult-1)*(-1)
  
  filename<-gsub(" ","",paste("staircase_values_sub_",as.character(sub_n),".csv"))
  export(data_cf,here("Studies",study,sub_folder_name,results_folder_name,filename))
  
  ############################################ Arranging data frame for fitting with unreal_fit.m #######################################

  #adding a marker at the start of the file name so matlab can find this file
  filename<-gsub(" ","",paste("matlab_fitting_file.csv"))
  export(data_cf,here(filename))
  
  
  
  ############################################################## Fit the data ##################################################################
  cat("Matlab is fitting the data","\n")
  run_matlab_script("unreal_fit.m",display=FALSE,verbose=FALSE)
  
  #load the fitted values from output_matlab
  
  
  fitted_file<-gsub(" ","",paste("threshold_values_sub_",sub_n,".csv"))
  jnd_thresholds<-import(here("output_matlab",fitted_file))
  
  #removing any imaginary parts from the thresholds
  jnd_thresholds$my_thresholds_real<-Re(jnd_thresholds$my_thresholds)
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
  
  
  ############################################## Adjusting thresholds ################################################
  
  #In case a threshold value was found to be beyond the limits of the scale we are using for an experimental condition
  #We must adjust the value accordingly.
  #In earlier versions we used to do this (up until 8.2023): Default values are adjusted as such: 95% thresh = Max(staircase value); 75% thresh = Mean(95% thresh + 50% thresh)
  #and 25% thresh = 50% thresh * the condition step size.
  
  #Since August 2023 we do this: if levels 1 or 2 are 4.6 or above Matlab defines the condition as needed to repeat.
  #If level 3 is 4.6 or above and level 2 is not, then we adjust level 4 to 4.6 and only then adjust level 3 to be the mean of levels 2 and 4
  #If level 4 is above 4.6 we adjust it to 4.6
  #We are essentially "squeezing" the top levels to fit in a predefined range no more than the max value of the staircase.
  #We do not do this for JNDs themselves because that would make fitting redundant.
  #The reason we originally started doing this is because, if we do not limit the max values possible,
  #the fitting function will often provide us with 3rd and 4th level values that are far beyond the 
  #range the subject has seen during the staircase procedure, and that could lead to skewed ratings of the experience.
  
  threshold_fix(jnd_thresholds,folder_names,sub_n,study)
  
  ######################################## PLOTTING #########################################################
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
  
  #return questionresults to factors
  data_cf$QuestionResult<-as.factor(data_cf$QuestionResult)
  
  cat("Plotting staircases...","\n")
  #plotting function
  plot_jnd <- function(con_name,data_cf,jnd_adjusted_thresholds) {
    #filter the exact condition from the data frame
    df<-data_cf %>% filter(ConditionName %in% c(con_name))
    df_attempt<-df$Attempt[1]
    stair_max<-max(abs(df$StairCaseValue))
    
    #filter the exact threshold calculated for this condition
    con_threshold <-jnd_adjusted_thresholds %>% filter(Condition %in% c(con_name))
    con_pr2<-con_threshold$Mcf_pR[1]
    con_thresh_025<- con_threshold %>% filter(prob %in% c(0.250))
    con_thresh_05<- con_threshold %>% filter(prob %in% c(0.500))
    con_thresh_075<- con_threshold %>% filter(prob %in% c(0.750))
    con_thresh_095<- con_threshold %>% filter(prob %in% c(0.950))
    mean_rev<-lapply(df[df$ConditionName==con_name,"ReversalVal"],mean,na.rm=TRUE)
    
    # 
    avg_rev<-trunc(abs(mean_rev[[1]])/stair_max*10^3)/10^3
    level1<-trunc(abs(con_thresh_025$adj_stim_val)/stair_max*10^3)/10^3
    level2<-trunc(abs(con_thresh_05$adj_stim_val)/stair_max*10^3)/10^3
    level3<-trunc(abs(con_thresh_075$adj_stim_val)/stair_max*10^3)/10^3
    level4<-trunc(abs(con_thresh_095$adj_stim_val)/stair_max*10^3)/10^3
    
    #check if there are any na's running around here
    con_pr2<-replace(con_pr2,is.na(con_pr2),0)
    level1<-replace(level1,is.na(level1),0)
    level2<-replace(level2,is.na(level2),0)
    level3<-replace(level3,is.na(level3),0)
    level4<-replace(level4,is.na(level4),0)

    
    con_pr2_flag="black"
    try(if (con_pr2<0.3){
      con_pr2_flag="red"
    },silent=TRUE)
    attempt_flag="black"
    try(if (df_attempt>1){
      attempt_flag="purple"
    },silent=TRUE)
    
    #start plotting the condition
    plt_jnd <-ggplot(data=df,aes(x=StepNumber,y=abs(StairCaseValue)/stair_max),group=Run,color=QuestionResult)+
      geom_line(alpha = 0.95)+
      scale_colour_manual(values = c("black"))+
      ggtitle(con_name,subtitle=paste("Thresh:",label_percent()(level4),",",label_percent()(level3),",",label_percent()(level2),",",label_percent()(level1),'\n',"Avg.Rev:",label_percent()(avg_rev),",","PR2:",as.character(round(con_pr2,2))))+scale_x_continuous(breaks=seq(0,25,5),limits=c(0, 25))+
      #scale_x_continuous(breaks=seq(0,25,5),limits=c(0, 26))+
      theme_gray()+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),breaks=seq(0,1.5,0.25),limits=c(0,1.1))+
      theme(plot.subtitle = element_text(size=6,hjust=0.5,colour = con_pr2_flag),plot.title = element_text(hjust = 0.5,size=9,colour = attempt_flag),text = element_text(family = "sans",face="bold"),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y= element_text(size=5),axis.text.x= element_text(size=5))+
      geom_point(aes(shape=factor(df$ReversalPoints),fill=factor(df$QuestionResult),size=factor(df$ReversalPoints)))+
      scale_size_manual(values=c(1,1.5),guide="none")+
      scale_shape_manual(values = c(21,25),guide="none")+
      scale_fill_manual(values=c("black","white"),guide="none")+
      guides(size = "none",fill="none")+
      geom_hline(yintercept=level1,color="red",linetype="dashed")+
      geom_hline(yintercept=level2,color="orange",linetype="dashed")+
      geom_hline(yintercept=level3,color="yellow",linetype="dashed")+
      geom_hline(yintercept=level4,color="green",linetype="dashed")+
      geom_hline(yintercept=avg_rev,linetype="dashed",color="black")#+{annotate(geom = "text",size=2.1,color="blue",x=2,y=org_list_ano,label=paste(as.character(org_list)))}
    return(plt_jnd+theme(legend.position="none"))
  }
  
  JND_plot_list<-vector(mode = "list", length=length(ConditionName_vec))
  
  for (x in 1:length(ConditionName_vec)){
    try(JND_plot_list[[x]]<-plot_jnd(ConditionName_vec[x],data_cf,jnd_adjusted_thresholds),silent = FALSE)
  }
  
#### plotting in unity numbers #######
  plot_jnd_unity <- function(con_name,data_cf,jnd_adjusted_thresholds) {
    #filter the exact condition from the data frame
    df<-data_cf %>% filter(ConditionName %in% c(con_name))
    df_attempt<-df$Attempt[1]
    #filter the exact threshold calculated for this condition
    con_threshold <-jnd_adjusted_thresholds %>% filter(Condition %in% c(con_name))
    con_pr2<-con_threshold$Mcf_pR[1]
    con_thresh_025<- con_threshold %>% filter(prob %in% c(0.250))
    con_thresh_05<- con_threshold %>% filter(prob %in% c(0.500))
    con_thresh_075<- con_threshold %>% filter(prob %in% c(0.750))
    con_thresh_095<- con_threshold %>% filter(prob %in% c(0.950))
    mean_rev<-lapply(df[df$ConditionName==con_name,"ReversalVal"],mean,na.rm=TRUE)
    avg_rev<-round(mean_rev[[1]],3)
    
    if (df$StairCaseValue[1]<0){
      avg_rev<-avg_rev*(-1)
    }
    level1<-trunc(con_thresh_025$adj_stim_val*10^3)/10^3
    level2<-trunc(con_thresh_05$adj_stim_val*10^3)/10^3
    level3<-trunc(con_thresh_075$adj_stim_val*10^3)/10^3
    level4<-trunc(con_thresh_095$adj_stim_val*10^3)/10^3
    
    # 
    con_pr2<-replace(con_pr2,is.na(con_pr2),0)
    level1<-replace(level1,is.na(level1),0)
    level2<-replace(level2,is.na(level2),0)
    level3<-replace(level3,is.na(level3),0)
    level4<-replace(level4,is.na(level4),0)

    # 
    con_pr2_flag="black"
    try(if (con_pr2<0.3){
      con_pr2_flag="red"
    },silent=TRUE)
    
    attempt_flag="black"
    try(if (df_attempt>1){
      attempt_flag="purple"
    },silent=TRUE)
    
    #start plotting the condition
    plt_jnd_unity <-ggplot(data=df,aes(x=StepNumber,y=StairCaseValue),group=Run,color=QuestionResult)+
      geom_line(alpha = 0.95)+
      scale_colour_manual(values = c("black"))+
      ggtitle(con_name,subtitle=paste("Thresh:",as.character(level4),",",as.character(level3),",",as.character(level2),",",as.character(level1),'\n',"Avg.Rev:",avg_rev,",","PR2:",as.character(round(con_pr2,2))))+scale_x_continuous(breaks=seq(0,25,5),limits=c(0, 25))+
      #scale_x_continuous(breaks=seq(0,25,5),limits=c(0, 26))+
      theme_gray()+
      #scale_y_continuous(labels = scales::percent_format(accuracy = 1),breaks=seq(0,1,0.25),limits=c(0,1.01))+
      theme(plot.subtitle = element_text(size=6,hjust=0.5,colour = con_pr2_flag),plot.title = element_text(hjust = 0.5,size=9,colour = attempt_flag),text = element_text(family = "sans",face="bold"),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y= element_text(size=5),axis.text.x= element_text(size=5))+
      geom_point(aes(shape=factor(df$ReversalPoints),fill=factor(df$QuestionResult),size=factor(df$ReversalPoints)))+
      scale_size_manual(values=c(1,1.5),guide="none")+
      scale_shape_manual(values = c(21,25),guide="none")+
      scale_fill_manual(values=c("black","white"),guide="none")+
      guides(size = "none",fill="none")+
      geom_hline(yintercept=level1,color="red",linetype="dashed")+
      geom_hline(yintercept=level2,color="orange",linetype="dashed")+
      geom_hline(yintercept=level3,color="yellow",linetype="dashed")+
      geom_hline(yintercept=level4,color="green",linetype="dashed")+
      geom_hline(yintercept=avg_rev,linetype="dashed",color="black")#+{annotate(geom = "text",size=2.1,color="blue",x=2,y=org_list_ano,label=paste(as.character(org_list)))}
    return(plt_jnd_unity+theme(legend.position="none"))
  }
  
################# unity numbers plot list ##############################
  
  Unity_plot_list<-vector(mode = "list", length=length(ConditionName_vec))
  
  for (x in 1:length(ConditionName_vec)){
    try(Unity_plot_list[[x]]<-plot_jnd_unity(ConditionName_vec[x],data_cf,jnd_adjusted_thresholds),silent = FALSE)
  }
  Unity_plot_list[[2]]<-Unity_plot_list[[2]]+scale_y_continuous(trans="reverse")
  try(Unity_plot_list[[4]]<-Unity_plot_list[[4]]+scale_y_continuous(trans="reverse"),silent=TRUE)
  #Unity_plot_list[[6]]<-Unity_plot_list[[6]]+scale_y_continuous(trans="reverse")
  try(Unity_plot_list[[8]]<-Unity_plot_list[[8]]+scale_y_continuous(trans="reverse"),silent=TRUE)
  
############################################################################################################################

  plot_title <- ggdraw() + draw_label(paste("Subject ",as.character(sub_n)), fontface='bold',x=0,size=14,hjust=-4.75)
  png_title<-gsub(" ","",paste("Psychophysics_Percents_sub_",as.character(sub_n),"_attempt_",attempt,".png"))
  Runs_plot<-cowplot::plot_grid(plotlist = JND_plot_list,nrow=3,ncol=3,rel_heights = c(1,1))
  plot_title_grid<-cowplot::plot_grid(plot_title,Runs_plot,ncol=1,rel_heights=c(0.1,1))
  ggsave(here("Studies",study,sub_folder_name,results_folder_name,png_title),width = 15,height = 15, units = "cm")
  cat(paste("Subject",sub_n,"staircases plot 1 done"),"\n")
  
  
  plot_title <- ggdraw() + draw_label(paste("Subject ",as.character(sub_n)), fontface='bold',x=0,size=14,hjust=-4.75)
  png_title<-gsub(" ","",paste("Psychophysics_Unity_sub_",as.character(sub_n),"_attempt_",attempt,".png"))
  Unity_plot<-cowplot::plot_grid(plotlist = Unity_plot_list,nrow=3,ncol=3,rel_heights = c(1,1))
  plot_title_grid<-cowplot::plot_grid(plot_title,Unity_plot,ncol=1,rel_heights=c(0.1,1))
  ggsave(here("Studies",study,sub_folder_name,results_folder_name,png_title),width = 15,height = 15, units = "cm")
  cat(paste("Subject",sub_n,"staircases plot 2 done"),"\n")
  
}

#save(unreal_psy, file = 'C:\\Users\\User\\OneDrive\\Desktop\\unreal_psychophysics\\rdas\\unreal_psy.rda')
