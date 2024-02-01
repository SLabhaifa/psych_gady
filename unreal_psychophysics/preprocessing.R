preprocessing<-function(){


#set the filename, always take the file called Answers*
filename_ans<-list.files(here("Studies",study,sub_folder_name), pattern=glob2rx("Answers*.csv"))[1]
#read JND output csv filename
data<-import(here("Studies",study,sub_folder_name,filename_ans))

#check if this is the first attempt at fitting
if (attempt == 1){
  data<- data %>% mutate(Attempt = attempt)
}

# save file
file_rename<-gsub(" ","",paste('Attempt_',as.character(attempt),'_',filename_ans))
export(data,here("Studies",study,sub_folder_name,results_folder_name,file_rename))#the file has column names no worries

#saving so we can attach to repeat files
export(data,here("Studies",study,sub_folder_name,filename_ans))


######################################## Editing the data frame #########################

#remove throw away trials (Blocknumber==0)
data<-data[!(data$BlockNumber==0),]

#filter the df so that only staircase rows remain
filter_questions <-data$QuestionID==88
data<-data[filter_questions,]

#filter trials with no answer
data<-data[!(data$QuestionResult=="NoAnswerInTime"),]

#remove columns we don't need
data_c<-subset(data, select=-c(ProbabilityComment,TimesatmpInitialDirection,InitialDirection,SessionID,Phase,QuestionID,TimeOfVaseHit,NegativeAnswersCount,PositiveAnswersCount,ConvertedValue))

#rename some columns
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
}
