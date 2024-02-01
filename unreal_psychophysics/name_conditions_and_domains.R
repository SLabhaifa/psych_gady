#this function fill the condition name and condition domain columns by the condition code

name_conditions_and_domains<-function(df){
  df$ConditionName[df$ConditionName==6020102|df$ConditionName==6020202|df$ConditionName==6020204|df$ConditionName==6020101|df$ConditionName==6020201|df$ConditionName==6020] <- "Grow"
  df$ConditionName[df$ConditionName==6010102|df$ConditionName==6010101|df$ConditionName==6010201|df$ConditionName==6010202|df$ConditionName==6010204|df$ConditionName==6010] <- "Shrink"
  df$ConditionName[df$ConditionName==3020102|df$ConditionName==3020201|df$ConditionName==3020101|df$ConditionName==3020202|df$ConditionName==3020204|df$ConditionName==3020] <- "Saturated"
  df$ConditionName[df$ConditionName==3010102|df$ConditionName==3010201|df$ConditionName==3010101|df$ConditionName==3010202|df$ConditionName==3010204|df$ConditionName==3010] <- "Unsaturated"
  df$ConditionName[df$ConditionName==330102|df$ConditionName==330101|df$ConditionName==330201|df$ConditionName==330202|df$ConditionName==3302] <- "Ripple"
  df$ConditionName[df$ConditionName==2220101|df$ConditionName==2220201|df$ConditionName==2220102|df$ConditionName==2220202|df$ConditionName==2220] <- "Heavy"
  df$ConditionName[df$ConditionName==2210101|df$ConditionName==2210201|df$ConditionName==2210202|df$ConditionName==2210102|df$ConditionName==2210] <- "Light"
  df$ConditionName[df$ConditionName==1220102|df$ConditionName==1220202|df$ConditionName==1220204|df$ConditionName==1220101|df$ConditionName==1220201|df$ConditionName==1220] <- "Fast"
  df$ConditionName[df$ConditionName==1210102|df$ConditionName==1210202|df$ConditionName==1210204|df$ConditionName==1210101|df$ConditionName==1210201|df$ConditionName==1210] <- "Slow"
  df$ConditionName[df$ConditionName==290101|df$ConditionName==290102|df$ConditionName==290201|df$ConditionName==290202|df$ConditionName==2902]<- "Delay"
  df$ConditionName[df$ConditionName==3812|df$ConditionName==3822]<- "Plant"
  
  
  df$Domain[df$ConditionCode==6020102|df$ConditionCode==6020202|df$ConditionCode==6020204|df$ConditionCode==6020101|df$ConditionCode==6020201|df$ConditionCode==6020] <- "Self"
  df$Domain[df$ConditionCode==6010102|df$ConditionCode==6010101|df$ConditionCode==6010201|df$ConditionCode==6010202|df$ConditionCode==6010204|df$ConditionCode==6010] <- "Self"
  df$Domain[df$ConditionCode==3020102|df$ConditionCode==3020201|df$ConditionCode==3020101|df$ConditionCode==3020202|df$ConditionCode==3020204|df$ConditionCode==3020] <- "Perception"
  df$Domain[df$ConditionCode==3010102|df$ConditionCode==3010201|df$ConditionCode==3010101|df$ConditionCode==3010202|df$ConditionCode==3010204|df$ConditionCode==3010] <- "Perception"
  df$Domain[df$ConditionCode==330102|df$ConditionCode==330101|df$ConditionCode==330201|df$ConditionCode==330202|df$ConditionCode==3302] <- "Perception"
  df$Domain[df$ConditionCode==2220101|df$ConditionCode==2220201|df$ConditionCode==2220102|df$ConditionCode==2220202|df$ConditionCode==2220] <- "Nature"
  df$Domain[df$ConditionCode==2210101|df$ConditionCode==2210201|df$ConditionCode==2210202|df$ConditionCode==2210102|df$ConditionCode==2210] <- "Nature"
  df$Domain[df$ConditionCode==1220102|df$ConditionCode==1220202|df$ConditionCode==1220204|df$ConditionCode==1220101|df$ConditionCode==1220201|df$ConditionCode==1220] <- "Nature"
  df$Domain[df$ConditionCode==1210102|df$ConditionCode==1210202|df$ConditionCode==1210204|df$ConditionCode==1210101|df$ConditionCode==1210201|df$ConditionCode==1210] <- "Nature"
  df$Domain[df$ConditionCode==290101|df$ConditionCode==290102|df$ConditionCode==290201|df$ConditionCode==290202|df$ConditionCode==2902]<- "Self"
  df$Domain[df$ConditionCode==3812|df$ConditionCode==3822]<- "Normal"

  return(df)
}

#save(name_conditions_and_domains, file = 'C:\\Users\\User\\OneDrive\\Desktop\\unreal_psychophysics\\rdas\\name_conditions_and_domains.rda')