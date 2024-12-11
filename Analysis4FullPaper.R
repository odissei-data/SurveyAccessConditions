####################
######## BACKGROUND
####################

# #SURVEY
# This script concerns the survey entitled “Survey on data access conditions for sensitive data”.
# This survey was performed by The Dutch Open Data Infrastructure for Social Science and Economic Innovations (ODISSEI) and DANS, the Dutch national centre of expertise and repository for research data. 
# The survey was launched in April 2024 and open until July 1st 2024. 
# The goal of the survey was to gather additional information about access conditions and restrictions used by researchers using the DANS and ODISSEI services. 

# #DATA
# The raw data that was used as input for this script is archived on Zenodo: https://doi.org/10.5281/zenodo.12805137
# It is under embargo until December 2024 as we are still in the process of analysing the entire dataset. 

# #ANALYSIS BELOW
# The analyis below was performed for a conference submission.
# It mainly concerns computing basic counts of our answers as well as creating plots to visualize the results of the closed ended questions. 

####################
######## ANALYSIS
####################

######## LOAD PACKAGES
library (tidyverse)
library (here)
library (readODS)
library (skimr)
library(dplyr)
library(ggpubr)
library (ggplot2)
library (gridExtra)
library(scales)     # Functions to format things nicely


######## LOAD AND PREPARE DATA
#Loading Raw data from ODS file
RawData <- read_ods(here("Data","SurveyDataAccessConditions_RawResults.ods"))

#Removing the first two rows from the raw data file, because the variables actually only start in row 3 
Data <- RawData[-c(1,2,3),]

#Removing the participant who answered "No" to whether they work with restricted access data (as they also said "Na" to all other questions)
Data <- subset(Data,Data$...6=="Yes" | is.na(Data$...6))

#As for this analysis we are only interested in the closed questions, and Q1 (which can be recoded), we remove the rest of the columns.  
Data <- Data[,-c(3,7,8,9,11,13,15,17,19,21,23,24,25,26,28,29,30),]

# I create a separate dataframe with all the labels for me to refer to things if needed and before I rename my variables 
#Getting the headings from the raw data and the selecting the same columns as for the data
TempLabel <- RawData[3,]
TempLabel <- TempLabel[,-c(3,7,8,9,11,13,15,17,19,21,23,24,25,26,28,29,30),]
TempLabel[11]<-"Is the use of the data limited to a certain region?" #Rewriting this particular one because it is too long for the header otherwise

Labels <- data.frame (q_text = t(TempLabel),q_id = c("Q1", "Q2", "Q3","Q4","Q5","Q9","Q10","Q11","Q12", "Q13","Q14", "Q15", "Q17"), q_shortname = c("Position", "Organisation", "Country", "ArchivedAtDANS", "ManageRestrictedData", "OtherThanResearch","Students", "Teaching", "LimitedIndividuals","Commercial","LimitedRegion","MotivationRequired","Costs"))
# I am adjusting the names of the rows to match the ones we will use for the Data
row.names(Labels) <-c("Position", "Organisation", "Country", "ArchivedAtDANS", "ManageRestrictedData", "OtherThanResearch","Students", "Teaching", "LimitedIndividuals","Commercial","LimitedRegion","MotivationRequired","Costs")

#Rename the variables in my Data to the short names so it will be easier to refer to them 
names(Data) <-c("Position", "Organisation", "Country", "ArchivedAtDANS", "ManageRestrictedData", "OtherThanResearch","Students", "Teaching", "LimitedIndividuals","Commercial","LimitedRegion","MotivationRequired","Costs")

#From looking at the Data, we saw that in country, multiple spellings of "the Netherlands" were used and we want to ensure they are grouped together.
Data <- Data %>% mutate(Country = recode(Country, 'Netherlands' = 'The Netherlands', 'the Netherlands' = 'The Netherlands', 'NL' =  'The Netherlands', 'Nederland' = 'The Netherlands', 'netherlands' = 'The Netherlands'))

#We also want to recode all "It depends (please elaborate below)" so we loop through the variables
Data <- Data %>% mutate(OtherThanResearch = recode(OtherThanResearch, 'It depends (please elaborate below)' = 'Depends'))
Data <- Data %>% mutate(Students = recode(Students, 'It depends (please elaborate below)' = 'Depends'))
Data <- Data %>% mutate(Teaching = recode(Teaching, 'It depends (please elaborate below)' = 'Depends'))
Data <- Data %>% mutate(LimitedIndividuals = recode(LimitedIndividuals, 'It depends (please elaborate below)' = 'Depends', 'Yes (please elaborate below - what type of organisation(s) / individual(s))' ='Yes'))
Data <- Data %>% mutate(Commercial = recode(Commercial, 'It depends (please elaborate below)' = 'Depends'))
Data <- Data %>% mutate(LimitedRegion = recode(LimitedRegion, 'It depends (please elaborate below)' = 'Depends', 'Yes (please elaborate below - which region?)'='Yes'))

#We also want to replace the values in "Positions" with the recoded values.
#For this we load the csv file
repQ1 <- read.csv(here("Data/RecodedData","Q1 - Recoding - Sheet1.csv"))
Data$Position <-repQ1$Coded.Response

######## CREATING COUNTS AND PLOTS

#Create counts for Q1 Q2, Q3 and Q4, ordered in descending order
Res_Org <- Data %>% count(Organisation) %>% arrange(desc(n))
Res_Pos <- Data %>% count(Position) %>% arrange(desc(n))
Res_Country <- Data %>% count(Country) %>% arrange(desc(n))
Res_AtDANS <- Data %>% count(ArchivedAtDANS) %>% arrange(desc(n))

#Create plots for all other closed questions 
for (i in 6: ncol(Data)) {

  #because I wants able to figure out how to loop through assigning the variables based on i
  if (i==6) {Input=Data$OtherThanResearch}
  else if (i==7) {
  Input=Data$Students
  #level_order <- c('Yes', 'No', 'Depends', 'NA') 
  }
  else if (i==8) {Input=Data$Teaching
  #level_order <- c('Yes', 'No', 'Depends') 
  }
  else if (i==9) {Input=Data$LimitedIndividuals
  #level_order <- c('Yes', 'No', 'Depends') 
  }
  else if (i==10) {Input=Data$Commercial
  #level_order <- c('Yes', 'No', 'Depends') 
  }
  else if (i==11) {Input=Data$LimitedRegion
  #level_order <- c('Yes', 'No', 'Depends') 
  }
  else if (i==12) {Input=Data$MotivationRequired
  #level_order <- c('Yes', 'No') 
  }
  else if (i==13) {Input=Data$Costs
  #level_order <- c('Yes', 'No', 'NA') 
  }

  # #If you want to test the output with just one value, you can use
  # i=6
  # Input=Data$Students
  PlotTitle=Labels[i,1]
  SaveTitle=Labels[i,3]

plt<- ggplot(Data, aes (x=Input, fill=Input)) + 
  geom_bar() + 
  scale_fill_manual(values = c("No" = "darkred", "Yes" = "darkgreen", "Depends" = "orange", "NA" = "gray")) +
  ggtitle(str_wrap(PlotTitle, width=50))+
  ylim(0, 45) +
  geom_text(stat = 'count', aes(label = ..count.., vjust = -1)) +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.title=element_blank(),
    legend.text= element_text(size=30),
    axis.text=element_text(size=30),
    plot.title=element_text(size=30, face = "bold"),
    legend.position = c(0.9,0.9),
   )   #+ scale_title_discrete(labels = label_wrap(10))
#+ labs(title=str_wrap(PlotTitle))
plt

ggsave(paste0("Output/Plot_", SaveTitle, ".jpeg"), dpi=300, units="cm", width= 24, height =20)
graphics.off()
}

# # TRYING TO GET ALL PLOTS INTO ONE IMAGE
# templ<- paste("plot",i, sep="")
# #assign this plot to the temp variable so we can save them all.
# assign(templ,plt)
# rm(plt)
# rm(templ)
# pltall <- ggarrange(plot6,plot7,plot8,plot9, plot10, plot11, plot12, ncol = 2, nrow = 4)
#pltall <- grid.arrange(plot6,plot7,plot8,plot9, plot10, plot11, plot12)


####################
#THINGS I TRIED EARLIER AND I MAY WANT TO GET BACK TO
####################

#I am letting this code in here as I may want to reuse part of it later (I am still learning R so having some old code snippets still available might save me time later)
# ##Creating an overview of the answer categories so we can then easily create a count
# #a2=c("University", "University of Applied Sciences", "Research Institute", "Infrastructure provider", "Other")
# #a4=c("Yes - DANS EASY","Yes - DANS Data Station", "Yes - DataverseNL", "No")
# #a5_15_17=c("Yes", "No")
# #a14=c("Yes (please elaborate below - which region?)","No","It depends (please elaborate below)")
# #a_rest = c("Yes","No","It depends (please elaborate below)")
# 
# #Trying to loop through things
# # for (i in 1: length(Labels[,1])) {
# #   i=6
# #   x=as.name("Country")
# #   Data <- Data %>% mutate(x = recode(x, 'Netherlands' = 'The Netherlands', 'the Netherlands' = 'The Netherlands', 'NL' =  'The Netherlands', 'Nederland' = 'The Netherlands', 'netherlands' = 'The Netherlands'))
# #
# #
# #   Data <- Data %>% mutate(as.name(names(Data[i])) = recode(as.name(names(Data[i])), 'It depends (please elaborate below)' = 'Depends'))
# #
# #Piechart
# pie5<- ggplot(Data, aes (x=factor(1), fill=ManageRestrictedData)) + geom_bar(width=1) + coord_polar("y") + ggtitle("ManageRestrictedData") + theme(text=element_text(size=15)) +theme(axis.text=element_text(size=15))
# pie5+ scale_fill_manual(values=c("#005773","#00ABAF", "#32ADD6")) + theme(axis.title.x = element_blank(),
#             axis.title.y = element_blank(),
#             panel.border = element_blank(),
#             panel.grid=element_blank(),
#             axis.ticks = element_blank(),
#             plot.title=element_text(size=14, face="bold")) + ggtitle("ManageRestrictedData") + theme(text=element_text(size=15)) +theme(axis.text=element_text(size=15))
# 
#+ geom_point(size=10) + ggtitle("This is the Title") + theme(text=element_text(size=20)) +theme(axis.text=element_text(size=30))
#pie + scale_fill_manual(values=c("#005773","#00ABAF", "#32ADD6")) 
#pie + scale_fill_brewer(palette="Set1")
#pie + theme(axis.text.y=element_blank()) 
#+ geom_point(size=10) 
# 

