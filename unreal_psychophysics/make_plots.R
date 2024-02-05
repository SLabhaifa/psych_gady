make_plots<-function(sub_n,study,attempt,sub_folder_name,results_folder_name,data_cf,jnd_adjusted_thresholds,ConditionName_vec){
  library(ggplot2)
  library(here)
  library(cowplot)

JND_plot_list<-vector(mode = "list", length=length(ConditionName_vec))

for (x in 1:length(ConditionName_vec)){
  try(JND_plot_list[[x]]<-plot_staircase(ConditionName_vec[x],data_cf,jnd_adjusted_thresholds),silent = FALSE)
}

plot_title <- ggdraw() + draw_label(paste("Subject ",as.character(sub_n)), fontface='bold',x=0,size=14,hjust=-4.75)
png_title<-gsub(" ","",paste("Psychophysics_Percents_sub_",as.character(sub_n),"_attempt_",attempt,".png"))
Runs_plot<-cowplot::plot_grid(plotlist = JND_plot_list,nrow=3,ncol=3,rel_heights = c(1,1))
plot_title_grid<-cowplot::plot_grid(plot_title,Runs_plot,ncol=1,rel_heights=c(0.1,1))
ggsave(here("Studies",study,sub_folder_name,results_folder_name,png_title),width = 15,height = 15, units = "cm")
cat(paste("\033[1;34mSubject",sub_n,"staircases plot 1 done\033[0m"),"\n")

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
cat(paste("\033[1;34mSubject",sub_n,"staircases plot 2 done\033[0m"),"\n")


}