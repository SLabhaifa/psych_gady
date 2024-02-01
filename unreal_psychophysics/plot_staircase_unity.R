#### plotting in unity numbers #######
plot_staircase_unity <- function(con_name,data_cf,jnd_adjusted_thresholds) {
  #filter the exact condition from the data frame
  ConditionName_vec<-unique(as.vector(data_cf$ConditionName))
  ConditionName_vec<-ConditionName_vec[order(ConditionName_vec)]
  
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


################# unity numbers plot list ##############################

Unity_plot_list<-vector(mode = "list", length=length(ConditionName_vec))

for (x in 1:length(ConditionName_vec)){
  try(Unity_plot_list[[x]]<-plot_staircase_unity(ConditionName_vec[x],data_cf,jnd_adjusted_thresholds),silent = FALSE)
}
Unity_plot_list[[2]]<-Unity_plot_list[[2]]+scale_y_continuous(trans="reverse")
try(Unity_plot_list[[4]]<-Unity_plot_list[[4]]+scale_y_continuous(trans="reverse"),silent=TRUE)
#Unity_plot_list[[6]]<-Unity_plot_list[[6]]+scale_y_continuous(trans="reverse")
try(Unity_plot_list[[8]]<-Unity_plot_list[[8]]+scale_y_continuous(trans="reverse"),silent=TRUE)

plot_title <- ggdraw() + draw_label(paste("Subject ",as.character(sub_n)), fontface='bold',x=0,size=14,hjust=-4.75)
png_title<-gsub(" ","",paste("Psychophysics_Unity_sub_",as.character(sub_n),"_attempt_",attempt,".png"))
Unity_plot<-cowplot::plot_grid(plotlist = Unity_plot_list,nrow=3,ncol=3,rel_heights = c(1,1))
plot_title_grid<-cowplot::plot_grid(plot_title,Unity_plot,ncol=1,rel_heights=c(0.1,1))
ggsave(here("Studies",study,sub_folder_name,results_folder_name,png_title),width = 15,height = 15, units = "cm")
cat(paste("Subject",sub_n,"staircases plot 2 done"),"\n")

}