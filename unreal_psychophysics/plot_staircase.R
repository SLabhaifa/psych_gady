plot_staircase <- function(con_name,data_cf,jnd_adjusted_thresholds) {
  
    cat("Plotting ",con_name," Staircase","\n")
    #filter the exact condition from the data frame
    df<-data_cf %>% filter(ConditionName %in% c(con_name))
    df_attempt<-df$Attempt[1]
    stair_max<-max(abs(df$StairCaseValue))
    
    ConditionName_vec<-unique(as.vector(df$ConditionName))
    ConditionName_vec<-ConditionName_vec[order(ConditionName_vec)]
    
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
  
  
  ############################################################################################################################
  
  
}