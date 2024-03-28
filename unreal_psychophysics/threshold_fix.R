threshold_fix<-function(org_threshold_df,folder_names,sub_n,study){
  library(data.table)
  
  #threshold fix receives: the data frame jnd_thresholds  
  #we check the values do not exceed 4.6 or log+1(100%)
  #using only the my_thresholds column (should be column number 3)
  
  #df is the thresholds csv with the scaled level values in each condition )
  #df<-read.csv(org_threshold_file,header=TRUE, stringsAsFactors = FALSE);
  
  
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
  
  
  df<-org_threshold_df
  #*make a new column to hold adjusted thresholds 
  df<- df %>% mutate(adj_stim_val = jnd_weighted_threshold)
  df<- df %>% mutate(adjusted_status = 0)
  
  #hdf is an empty data frame that only has the names and threshold levels with no Unity values
  hdf<-df
  #empty the stim_val column
  hdf$adj_stim_val<-NA
  flag=FALSE;
  
  #order the rows by condition name alphabetically
  df = subset(df, df$Condition != "Condition")
  df = df[order(df$Condition), ]
  hdf = hdf[order(hdf$Condition), ]
  
  #place the first value of each staircase from staircase_Data into the 0.95 value in hdf per condition
  hdf[hdf$prob==0.95 & hdf$Condition=="Plant","adj_stim_val"]<-df[df$prob==0.95 & df$Condition=="Plant","max_strength"]
  hdf[hdf$prob==0.95 & hdf$Condition=="Slow","adj_stim_val"]<-df[df$prob==0.95 & df$Condition=="Slow","max_strength"]
  hdf[hdf$prob==0.95 & hdf$Condition=="Fast","adj_stim_val"]<-df[df$prob==0.95 & df$Condition=="Fast","max_strength"]
  hdf[hdf$prob==0.95 & hdf$Condition=="Heavy","adj_stim_val"]<-df[df$prob==0.95 & df$Condition=="Heavy","max_strength"]
  hdf[hdf$prob==0.95 & hdf$Condition=="Light","adj_stim_val"]<-df[df$prob==0.95 & df$Condition=="Light","max_strength"]
  hdf[hdf$prob==0.95 & hdf$Condition=="Grow","adj_stim_val"]<-df[df$prob==0.95 & df$Condition=="Grow","max_strength"]
  hdf[hdf$prob==0.95 & hdf$Condition=="Shrink","adj_stim_val"]<-df[df$prob==0.95 & df$Condition=="Shrink","max_strength"]
  hdf[hdf$prob==0.95 & hdf$Condition=="Delay","adj_stim_val"]<-df[df$prob==0.95 & df$Condition=="Delay","max_strength"]
  hdf[hdf$prob==0.95 & hdf$Condition=="Ripple","adj_stim_val"]<-df[df$prob==0.95 & df$Condition=="Ripple","max_strength"]
  hdf[hdf$prob==0.95 & hdf$Condition=="Saturated","adj_stim_val"]<-df[df$prob==0.95 & df$Condition=="Saturated","max_strength"]
  hdf[hdf$prob==0.95 & hdf$Condition=="Unsaturated","adj_stim_val"]<-df[df$prob==0.95 & df$Condition=="Unsaturated","max_strength"]
  
  
  
  
  #place lvl2 and lvl1 in the 0.5/0.25 rows per condition
  hdf[hdf$prob==0.5,"adj_stim_val"]<-df[df$prob==0.5,"jnd_weighted_threshold"]
  
  #we have not limited the lower bound of stim values to what was seen during the staircase
  #It is worth mentioning that there are cases where the lowest value seen in a staircase, was the value of the JND, in this case
  #it would be wrong to limit the minimal stim value to what was seen during the staircase and move the JND value up.
  #So the only case that is relevant is if lvl1 is higher than the max staircase value seen, and that will probably never happen 
  hdf[hdf$prob==0.25 & hdf$Condition=="Plant","adj_stim_val"]<-hdf[hdf$prob==0.5 & hdf$Condition=="Plant","jnd_weighted_threshold"]*0.7
  hdf[hdf$prob==0.25 & hdf$Condition=="Slow","adj_stim_val"]<-hdf[hdf$prob==0.5 & hdf$Condition=="Slow","jnd_weighted_threshold"]*0.85
  hdf[hdf$prob==0.25 & hdf$Condition=="Fast","adj_stim_val"]<-hdf[hdf$prob==0.5 & hdf$Condition=="Fast","jnd_weighted_threshold"]*0.85
  hdf[hdf$prob==0.25 & hdf$Condition=="Heavy","adj_stim_val"]<-hdf[hdf$prob==0.5 & hdf$Condition=="Heavy","jnd_weighted_threshold"]*0.85
  hdf[hdf$prob==0.25 & hdf$Condition=="Light","adj_stim_val"]<-hdf[hdf$prob==0.5 & hdf$Condition=="Light","jnd_weighted_threshold"]*0.85
  hdf[hdf$prob==0.25 & hdf$Condition=="Grow","adj_stim_val"]<-hdf[hdf$prob==0.5 & hdf$Condition=="Grow","jnd_weighted_threshold"]*0.7
  hdf[hdf$prob==0.25 & hdf$Condition=="Shrink","adj_stim_val"]<-hdf[hdf$prob==0.5 & hdf$Condition=="Shrink","jnd_weighted_threshold"]*0.7
  hdf[hdf$prob==0.25 & hdf$Condition=="Delay","adj_stim_val"]<-hdf[hdf$prob==0.5 & hdf$Condition=="Delay","jnd_weighted_threshold"]*0.65
  hdf[hdf$prob==0.25 & hdf$Condition=="Ripple","adj_stim_val"]<-hdf[hdf$prob==0.5 & hdf$Condition=="Ripple","jnd_weighted_threshold"]*0.65
  hdf[hdf$prob==0.25 & hdf$Condition=="Saturated","adj_stim_val"]<-hdf[hdf$prob==0.5 & hdf$Condition=="Saturated","jnd_weighted_threshold"]*0.65
  hdf[hdf$prob==0.25 & hdf$Condition=="Unsaturated","adj_stim_val"]<-hdf[hdf$prob==0.5 & hdf$Condition=="Unsaturated","jnd_weighted_threshold"]*0.65
  
  # 
  hdf[hdf$prob==0.75 & hdf$Condition=="Plant","adj_stim_val"]<-mean(c(hdf[hdf$prob==0.5 & hdf$Condition=="Fast","adj_stim_val"],hdf[hdf$prob==0.95 & hdf$Condition=="Plant","adj_stim_val"]))
  hdf[hdf$prob==0.75 & hdf$Condition=="Slow","adj_stim_val"]<-mean(c(hdf[hdf$prob==0.5 & hdf$Condition=="Fast","adj_stim_val"],hdf[hdf$prob==0.95 & hdf$Condition=="Slow","adj_stim_val"]))
  hdf[hdf$prob==0.75 & hdf$Condition=="Fast","adj_stim_val"]<-mean(c(hdf[hdf$prob==0.5 & hdf$Condition=="Fast","adj_stim_val"],hdf[hdf$prob==0.95 & hdf$Condition=="Fast","adj_stim_val"]))
  hdf[hdf$prob==0.75 & hdf$Condition=="Heavy","adj_stim_val"]<-mean(c(hdf[hdf$prob==0.5 & hdf$Condition=="Heavy","adj_stim_val"],hdf[hdf$prob==0.95 & hdf$Condition=="Heavy","adj_stim_val"]))
  hdf[hdf$prob==0.75 & hdf$Condition=="Light","adj_stim_val"]<-mean(c(hdf[hdf$prob==0.5 & hdf$Condition=="Light","adj_stim_val"],hdf[hdf$prob==0.95 & hdf$Condition=="Light","adj_stim_val"]))
  hdf[hdf$prob==0.75 & hdf$Condition=="Grow","adj_stim_val"]<-mean(c(hdf[hdf$prob==0.5 & hdf$Condition=="Grow","adj_stim_val"],hdf[hdf$prob==0.95 & hdf$Condition=="Grow","adj_stim_val"]))
  hdf[hdf$prob==0.75 & hdf$Condition=="Shrink","adj_stim_val"]<-mean(c(hdf[hdf$prob==0.5 & hdf$Condition=="Shrink","adj_stim_val"],hdf[hdf$prob==0.95 & hdf$Condition=="Shrink","adj_stim_val"]))
  hdf[hdf$prob==0.75 & hdf$Condition=="Delay","adj_stim_val"]<-mean(c(hdf[hdf$prob==0.5 & hdf$Condition=="Delay","adj_stim_val"],hdf[hdf$prob==0.95 & hdf$Condition=="Delay","adj_stim_val"]))
  hdf[hdf$prob==0.75 & hdf$Condition=="Ripple","adj_stim_val"]<-mean(c(hdf[hdf$prob==0.5 & hdf$Condition=="Ripple","adj_stim_val"],hdf[hdf$prob==0.95 & hdf$Condition=="Ripple","adj_stim_val"]))
  hdf[hdf$prob==0.75 & hdf$Condition=="Saturated","adj_stim_val"]<-mean(c(hdf[hdf$prob==0.5 & hdf$Condition=="Saturated","adj_stim_val"],hdf[hdf$prob==0.95 & hdf$Condition=="Saturated","adj_stim_val"]))
  hdf[hdf$prob==0.75 & hdf$Condition=="Unsaturated","adj_stim_val"]<-mean(c(hdf[hdf$prob==0.5 & hdf$Condition=="Unsaturated","adj_stim_val"],hdf[hdf$prob==0.95 & hdf$Condition=="Unsaturated","adj_stim_val"]))
  
  i = 1
  
  temp = df$Condition
  #if any value is larger than the max staircase value seen by the subject, it will be replaced by one of the values placed in hdf
  #in case lvl 1 is larger than max staircase, there will have to be a repeat anyway so the adjustment is irrelevant
  #in case lvl 2 is larger than max staircase, there will also be a repeat so the adjustment is irrelevant
  #in case level 3 is max or above and there was a repeat so lvl1 and lvl2 are not, then lvl3 will be replaced by the mean of the max value seen in the staircase
  #and lvl2, the actual jnd we found.
  #if lvl4 is max or above, it will be adjusted to the max value seen during the staircase.
  for (t in temp) {
    if ((t == "Plant" && (df[i,"jnd_weighted_threshold"] < 0 || df[i,"jnd_weighted_threshold"] > df[df$prob==0.95 & df$Condition=="Plant","max_strength"]|| is.na(df[i,"jnd_weighted_threshold"]))) ||
        (t == "Slow" && (df[i,"jnd_weighted_threshold"] < 0 || df[i,"jnd_weighted_threshold"] > df[df$prob==0.95 & df$Condition=="Slow","max_strength"]|| is.na(df[i,"jnd_weighted_threshold"]))) ||
        (t == "Fast" && (df[i,"jnd_weighted_threshold"] < 0 || df[i,"jnd_weighted_threshold"] > df[df$prob==0.95 & df$Condition=="Fast","max_strength"]|| is.na(df[i,"jnd_weighted_threshold"]))) || 
        (t == "Grow" && (df[i,"jnd_weighted_threshold"] < 0 || df[i,"jnd_weighted_threshold"] > df[df$prob==0.95 & df$Condition=="Grow","max_strength"]|| is.na(df[i,"jnd_weighted_threshold"]))) ||
        (t == "Heavy" && (df[i,"jnd_weighted_threshold"] > 0 || df[i,"jnd_weighted_threshold"] < df[df$prob==0.95 & df$Condition=="Heavy","max_strength"]|| is.na(df[i,"jnd_weighted_threshold"]))) ||
        (t == "Saturated" && (df[i,"jnd_weighted_threshold"] < 0 || df[i,"jnd_weighted_threshold"] > df[df$prob==0.95 & df$Condition=="Saturated","max_strength"]|| is.na(df[i,"jnd_weighted_threshold"]))) ||
        (t == "Light" && (df[i,"jnd_weighted_threshold"] < 0 || df[i,"jnd_weighted_threshold"] > df[df$prob==0.95 & df$Condition=="Light","max_strength"]|| is.na(df[i,"jnd_weighted_threshold"]))) ||
        (t == "Unsaturated" && (df[i,"jnd_weighted_threshold"] > 0 || df[i,"jnd_weighted_threshold"] < df[df$prob==0.95 & df$Condition=="Unsaturated","max_strength"]|| is.na(df[i,"jnd_weighted_threshold"]))) ||
        (t == "Delay" && (df[i,"jnd_weighted_threshold"] < 0 || df[i,"jnd_weighted_threshold"] > df[df$prob==0.95 & df$Condition=="Delay","max_strength"]|| is.na(df[i,"jnd_weighted_threshold"]))) ||
        (t == "Ripple" && (df[i,"jnd_weighted_threshold"] < 0 || df[i,"jnd_weighted_threshold"] > df[df$prob==0.95 & df$Condition=="Ripple","max_strength"]|| is.na(df[i,"jnd_weighted_threshold"]))) ||
        (t == "Shrink" &&(df[i,"jnd_weighted_threshold"] > 0 || df[i,"jnd_weighted_threshold"] < df[df$prob==0.95 & df$Condition=="Shrink","max_strength"]|| is.na(df[i,"jnd_weighted_threshold"]))))
     {
      cat("\033[1;30m Value Change in: ", t, ",", df[i,"prob 033[0m"], "\n")
      cat("\033[1;30m From: ", df[i,"jnd_weighted_threshold"], " to ", hdf[i,"adj_stim_val033[0m"], "\n\n")
      df[i, "adj_stim_val"] <- hdf[i, "adj_stim_val"]
      df[i,"adjusted_status"]<-1
      flag=TRUE;
    }
    i = i + 1
  }
  df<-df[,c("Study","Subject","Mcf_pR","Condition","my_thresholds","prob","Level","my_thresholds_real","max_strength","jnd_anti_log","jnd_weighted_threshold","adj_stim_val","adjusted_status")]
  df$jnd_weighted_threshold<- trunc(df$jnd_weighted_threshold*10^5)/10^5
  #trunc(samples$stim_x*10^5)/10^5
  df$adj_stim_val<- trunc(df$adj_stim_val*10^5)/10^5
  df$max_strength<- trunc(df$max_strength*10^5)/10^5
  
  if (flag){
    filename<-gsub(" ","",paste("threshold_values_sub_",as.character(sub_n),".csv"))
    export(df,here("Studies",study,folder_names[[1]],folder_names[[2]],filename))
    cat("\033[1;34mThresholds were adjusted","\033[0m\n")
  }
  else{
    filename<-gsub(" ","",paste("threshold_values_sub_",as.character(sub_n),".csv"))
    export(df,here("Studies",study,folder_names[[1]],folder_names[[2]],filename))
    cat("No adjustments were made to the thresholds","\n")
  }
}

#save(threshold_fix, file = 'C:\\Users\\User\\OneDrive\\Desktop\\unreal_psychophysics\\rdas\\threshold_fix.rda')