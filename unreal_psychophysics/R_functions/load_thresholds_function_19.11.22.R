#run the script with subject fodler path
load_thresholds<-function(folder_names,sub_n){


library(matlabr)#for running matlab from rstudio
library(data.table) 
library(ggplot2) 
library(dplyr) #data wrangling tool
library(tidyr) #data wrangling tool
library(cowplot) #for arranging plots together
library(magicfor)#magical for loops that can output all kinds of stuff
library(stringi)#for manipulating strings
library(ggpubr) #?
library(ggsci)#?
library(RColorBrewer)#?
library(scales)#?
library(ggthemes)#?
library(extrafont)#?
library(gridExtra)#?
library(itertools)#?
library(validate)#?


#load the StaircaseCommands template for fmri/eeg
filename_imaging<-gsub(" ","",paste("unreal_input_",sub_n,".csv"));
unreal_rt<-import(here("unreal_random_temps",filename))
  

#load the random unreal template input csv
filename<-gsub(" ","",paste("unreal_input_",sub_n,".csv"));
unreal_rt<-import(here("unreal_random_temps",filename))


filename4<-"Unreal_05_Training_Master_Template__191122.csv"
unreal_training<-import(here("unreal_random_temps",filename4))

cat(paste("Loaded Unreal random template number:",as.character(sub_n)),"\n")

#############################################################################
                  #Loading thresholds csv #
#############################################################################

filename2<-gsub(" ","",paste("threshold_values_sub_",as.character(sub_n),".csv"))
jnd_thresholds<-import(here("Studies","fmri",folder_names[[1]],folder_names[[2]],filename2))

#load staircase csv
filename3<-gsub(" ","",paste("staircase_values_sub_",as.character(sub_n),".csv"))
raw_data<-import(here("Studies","fmri",folder_names[[1]],folder_names[[2]],filename3))


#############################################################################
              #Organizing template for unreal TEST#
#############################################################################

#renaming some columns
colnames(unreal_rt)[1] <- "Magnitude"
colnames(unreal_rt)[6] <- "Height"
colnames(unreal_rt)[12] <- "Time"
colnames(unreal_rt)[22] <- "Gravity"
colnames(unreal_rt)[27] <- "Code"
colnames(unreal_rt)[29] <- "Delay"
colnames(unreal_rt)[30] <- "Saturation"
colnames(unreal_rt)[33] <- "Ripple"
colnames(unreal_rt)[36] <- "idx_rand"

#save calibration row apart from the df
calibration_row<-unreal_rt[1,]
unreal_rt<-unreal_rt[-c(1),]

##writing to the Unreal input csv file
#the locations for effect values in the template csv are preset to help us match conditions

#Height
gr <- jnd_thresholds %>% filter(Condition %in% c('Grow'))
unreal_rt$Height[unreal_rt$Height=='g1']<-gr$shifted_threshold[1]
unreal_rt$Height[unreal_rt$Height=='g2']<-gr$shifted_threshold[2]
unreal_rt$Height[unreal_rt$Height=='g3']<-gr$shifted_threshold[3]
unreal_rt$Height[unreal_rt$Height=='g4']<-gr$shifted_threshold[4]

sh <- jnd_thresholds %>% filter(Condition %in% c('Shrink'))
#sh$stim_val<-sh$stim_val#*-1
unreal_rt$Height[unreal_rt$Height=='s1']<-sh$shifted_threshold[1]
unreal_rt$Height[unreal_rt$Height=='s2']<-sh$shifted_threshold[2]
unreal_rt$Height[unreal_rt$Height=='s3']<-sh$shifted_threshold[3]
unreal_rt$Height[unreal_rt$Height=='s4']<-sh$shifted_threshold[4]

#Fast - in the JND experiment the range is from 0 to 1 where 0 is normal while in Unreal, the range is from 1 to 2 and 1 is normal
fast_t <- jnd_thresholds %>% filter(Condition %in% c('Fast'))
#fast_t$stim_val<-1+fast_t$stim_val
unreal_rt$Time[unreal_rt$Time=='ft1']<-fast_t$shifted_threshold[1]
unreal_rt$Time[unreal_rt$Time=='ft2']<-fast_t$shifted_threshold[2]
unreal_rt$Time[unreal_rt$Time=='ft3']<-fast_t$shifted_threshold[3]
unreal_rt$Time[unreal_rt$Time=='ft4']<-fast_t$shifted_threshold[4]

#Gravity
hg <- jnd_thresholds %>% filter(Condition %in% c('Heavy'))
#hg$stim_val<-hg$stim_val*-1
#hg$stim_val<-hg$stim_val-9.81
unreal_rt$Gravity[unreal_rt$Gravity=='hg1']<-hg$shifted_threshold[1]
unreal_rt$Gravity[unreal_rt$Gravity=='hg2']<-hg$shifted_threshold[2]
unreal_rt$Gravity[unreal_rt$Gravity=='hg3']<-hg$shifted_threshold[3]
unreal_rt$Gravity[unreal_rt$Gravity=='hg4']<-hg$shifted_threshold[4]

lg <- jnd_thresholds %>% filter(Condition %in% c('Light'))
#lg$stim_val<-lg$stim_val-9.81
unreal_rt$Gravity[unreal_rt$Gravity=='lg1']<-lg$shifted_threshold[1]
unreal_rt$Gravity[unreal_rt$Gravity=='lg2']<-lg$shifted_threshold[2]
unreal_rt$Gravity[unreal_rt$Gravity=='lg3']<-lg$shifted_threshold[3]
unreal_rt$Gravity[unreal_rt$Gravity=='lg4']<-lg$shifted_threshold[4]

#Delay - delay range is from 0 (normal) to 1 (strong delay)
rd <- jnd_thresholds %>% filter(Condition %in% c('Delay'))
unreal_rt$Delay[unreal_rt$Delay=='d1']<-rd$shifted_threshold[1]
unreal_rt$Delay[unreal_rt$Delay=='d2']<-rd$shifted_threshold[2]
unreal_rt$Delay[unreal_rt$Delay=='d3']<-rd$shifted_threshold[3]
unreal_rt$Delay[unreal_rt$Delay=='d4']<-rd$shifted_threshold[4]

#Saturation
hs <- jnd_thresholds %>% filter(Condition %in% c('Saturated'))
unreal_rt$Saturation[unreal_rt$Saturation=='hs1']<-hs$shifted_threshold[1]
unreal_rt$Saturation[unreal_rt$Saturation=='hs2']<-hs$shifted_threshold[2]
unreal_rt$Saturation[unreal_rt$Saturation=='hs3']<-hs$shifted_threshold[3]
unreal_rt$Saturation[unreal_rt$Saturation=='hs4']<-hs$shifted_threshold[4]

ls <- jnd_thresholds %>% filter(Condition %in% c('Unsaturated'))
#ls$stim_val<-ls$stim_val*
unreal_rt$Saturation[unreal_rt$Saturation=='ls1']<-ls$shifted_threshold[1]
unreal_rt$Saturation[unreal_rt$Saturation=='ls2']<-ls$shifted_threshold[2]
unreal_rt$Saturation[unreal_rt$Saturation=='ls3']<-ls$shifted_threshold[3]
unreal_rt$Saturation[unreal_rt$Saturation=='ls4']<-ls$shifted_threshold[4]

#Ripple - ripples get stronger intensity in Unreal from 0->1, 0 is normal
rp <- jnd_thresholds %>% filter(Condition %in% c('Ripple'))
unreal_rt$Ripple[unreal_rt$Ripple=='r1']<-rp$shifted_threshold[1]
unreal_rt$Ripple[unreal_rt$Ripple=='r2']<-rp$shifted_threshold[2]
unreal_rt$Ripple[unreal_rt$Ripple=='r3']<-rp$shifted_threshold[3]
unreal_rt$Ripple[unreal_rt$Ripple=='r4']<-rp$shifted_threshold[4]

#add the calibration row at the top of the csv
unreal_rt<-rbind(calibration_row,unreal_rt)

#sub<-readr::parse_number(basename(unreal_rand_temp))
filename<-gsub(" ","",paste("trials_unreal_",as.character(sub_n),".csv"))
export(unreal_rt,here("Studies","fmri",folder_names[[1]],folder_names[[2]],folder_names[[3]],filename),col.names=FALSE)
#fwrite(data_cs,'pGD_2sc.csv',col.names=TRUE)


#############################################################################
#Organizing template for unreal TRAIN#
#############################################################################

#renaming some columns
colnames(unreal_training)[1] <- "Magnitude"
colnames(unreal_training)[6] <- "Height"
colnames(unreal_training)[12] <- "Time"
colnames(unreal_training)[22] <- "Gravity"
colnames(unreal_training)[27] <- "Code"
colnames(unreal_training)[29] <- "Delay"
colnames(unreal_training)[30] <- "Saturation"
colnames(unreal_training)[33] <- "Ripple"
colnames(unreal_training)[36] <- "idx_rand"

#save calibration row apart from the df
calibration_row<-unreal_training[1,]
unreal_training<-unreal_training[-c(1),]

##writing to the Unreal input csv file
#the locations for effect values in the template csv are preset to help us match conditions

#Height
gr <- jnd_thresholds %>% filter(Condition %in% c('Grow'))
unreal_training$Height[unreal_training$Height=='g1']<-gr$shifted_threshold[1]
unreal_training$Height[unreal_training$Height=='g2']<-gr$shifted_threshold[2]
unreal_training$Height[unreal_training$Height=='g3']<-gr$shifted_threshold[3]
unreal_training$Height[unreal_training$Height=='g4']<-gr$shifted_threshold[4]

sh <- jnd_thresholds %>% filter(Condition %in% c('Shrink'))
#sh$stim_val<-sh$stim_val#*-1
unreal_training$Height[unreal_training$Height=='s1']<-sh$shifted_threshold[1]
unreal_training$Height[unreal_training$Height=='s2']<-sh$shifted_threshold[2]
unreal_training$Height[unreal_training$Height=='s3']<-sh$shifted_threshold[3]
unreal_training$Height[unreal_training$Height=='s4']<-sh$shifted_threshold[4]

#Fast - in the JND experiment the range is from 0 to 1 where 0 is normal while in Unreal, the range is from 1 to 2 and 1 is normal
fast_t <- jnd_thresholds %>% filter(Condition %in% c('Fast'))
#fast_t$stim_val<-1+fast_t$stim_val
unreal_training$Time[unreal_training$Time=='ft1']<-fast_t$shifted_threshold[1]
unreal_training$Time[unreal_training$Time=='ft2']<-fast_t$shifted_threshold[2]
unreal_training$Time[unreal_training$Time=='ft3']<-fast_t$shifted_threshold[3]
unreal_training$Time[unreal_training$Time=='ft4']<-fast_t$shifted_threshold[4]

#Gravity
hg <- jnd_thresholds %>% filter(Condition %in% c('Heavy'))
#hg$stim_val<-hg$stim_val*-1
#hg$stim_val<-hg$stim_val-9.81
unreal_training$Gravity[unreal_training$Gravity=='hg1']<-hg$shifted_threshold[1]
unreal_training$Gravity[unreal_training$Gravity=='hg2']<-hg$shifted_threshold[2]
unreal_training$Gravity[unreal_training$Gravity=='hg3']<-hg$shifted_threshold[3]
unreal_training$Gravity[unreal_training$Gravity=='hg4']<-hg$shifted_threshold[4]

lg <- jnd_thresholds %>% filter(Condition %in% c('Light'))
#lg$stim_val<-lg$stim_val-9.81
unreal_training$Gravity[unreal_training$Gravity=='lg1']<-lg$shifted_threshold[1]
unreal_training$Gravity[unreal_training$Gravity=='lg2']<-lg$shifted_threshold[2]
unreal_training$Gravity[unreal_training$Gravity=='lg3']<-lg$shifted_threshold[3]
unreal_training$Gravity[unreal_training$Gravity=='lg4']<-lg$shifted_threshold[4]

#Delay - delay range is from 0 (normal) to 1 (strong delay)
rd <- jnd_thresholds %>% filter(Condition %in% c('Delay'))
unreal_training$Delay[unreal_training$Delay=='d1']<-rd$shifted_threshold[1]
unreal_training$Delay[unreal_training$Delay=='d2']<-rd$shifted_threshold[2]
unreal_training$Delay[unreal_training$Delay=='d3']<-rd$shifted_threshold[3]
unreal_training$Delay[unreal_training$Delay=='d4']<-rd$shifted_threshold[4]

#Saturation
hs <- jnd_thresholds %>% filter(Condition %in% c('Saturated'))
unreal_training$Saturation[unreal_training$Saturation=='hs1']<-hs$shifted_threshold[1]
unreal_training$Saturation[unreal_training$Saturation=='hs2']<-hs$shifted_threshold[2]
unreal_training$Saturation[unreal_training$Saturation=='hs3']<-hs$shifted_threshold[3]
unreal_training$Saturation[unreal_training$Saturation=='hs4']<-hs$shifted_threshold[4]

ls <- jnd_thresholds %>% filter(Condition %in% c('Unsaturated'))
#ls$stim_val<-ls$stim_val*
unreal_training$Saturation[unreal_training$Saturation=='ls1']<-ls$shifted_threshold[1]
unreal_training$Saturation[unreal_training$Saturation=='ls2']<-ls$shifted_threshold[2]
unreal_training$Saturation[unreal_training$Saturation=='ls3']<-ls$shifted_threshold[3]
unreal_training$Saturation[unreal_training$Saturation=='ls4']<-ls$shifted_threshold[4]

#Ripple - ripples get stronger intensity in Unreal from 0->1, 0 is normal
rp <- jnd_thresholds %>% filter(Condition %in% c('Ripple'))
unreal_training$Ripple[unreal_training$Ripple=='r1']<-rp$shifted_threshold[1]
unreal_training$Ripple[unreal_training$Ripple=='r2']<-rp$shifted_threshold[2]
unreal_training$Ripple[unreal_training$Ripple=='r3']<-rp$shifted_threshold[3]
unreal_training$Ripple[unreal_training$Ripple=='r4']<-rp$shifted_threshold[4]

#add the calibration row at the top of the csv
unreal_training<-rbind(calibration_row,unreal_training)


#sub<-readr::parse_number(basename(unreal_rand_temp))
filename<-gsub(" ","",paste("trials_unreal_TRAIN_",as.character(sub_n),".csv"))
export(unreal_training,here("Studies","fmri",folder_names[[1]],folder_names[[2]],folder_names[[3]],filename),col.names=FALSE)
cat("Staircase results, Unreal input csv & Training csv have been created","\n")

}


#save(load_thresholds, file = 'C:\\Users\\User\\Desktop\\GitHub\\psych_gady\\unreal_psychophysics\\rdas\\load_thresholds.rda')
