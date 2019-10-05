#APPENDIX FOR CA2 PROGRAMMING FOR BIG DATA
#DATASET 1.
#STUDENT NO: 18195342 SIOBHAN PURCELL



#--------------------------------------------------------------------------



#A TITANIC DATASET ANALYSIS SCRIPT USING R



#--------------------------------------------------------------------------

# ----------------------------------
#           Load Libraries
# ----------------------------------
install.packages("ggplot2") 
install.packages("VIM")
install.packages("dplyr")
install.packages("stringr")

library(ggplot2)
library(VIM)
library(dplyr)
library(stringr)


#set working directory
setwd("C:/Users/Siobhan/Desktop/Datasets")


#importing csv file from EXCEL into R.
titanic1 <- read.csv("titanic.csv", header = T,  sep=",")

#viewing the dataset in data view
View(titanic1)

#looking at the datatypes of the variable in my dataset
#The dataset has 891 rows and 12 column 
str(titanic1)


#Tabular view of missing values
#(Note a substantial amount of values is missing from the variable "Age")
colSums(is.na(titanic1))

#However, missing values may be read into R as an empty string
#We can address this by including the the na.strings argument when reading in the file
#Then each missing value will be read into R as an NA value
titanic2 <- read.csv("titanic.csv", header = T, na.strings = c(""))

#viewing the dataset in data view again to confirm conversion of empty values into NA's
View(titanic2)

#=================================================================
#Assessment of Missing Vlaues
#===============================================================

#Just view rows with missing data
#As we can see
complete.cases(titanic2)

#First showing rows WITHOUT NAs
titanic2[complete.cases(titanic2),]


#Now showing rows with at least one NA
titanic2[!complete.cases(titanic2),]


#(Note missing values are now detected in the variables "Embarked" and "Cabin")
colSums(is.na(titanic2))


#Visualize Missing Values using the the Vim Package.
library(VIM)
missingview <- aggr(titanic2, prop = FALSE, numbers = TRUE)

#Address missing ages by imputing them with either the median or mean age of all passengers
#To calculate the mean or median age, we must first remove NA values first from the titanic dataset

#First, we must calculate the median and mean values for ages

#However, we get a result of NA here below because there are some NA values in the column
median(titanic2$Age)


#This issues can be addressed by using the na.omit() to remove all NA values from the titanic dataset
NAagesRemoved <- na.omit(titanic2)

#We can then  explicitly calculate the mean and median age for males and females as follows:

#median age for female victims
fem_median_age= median(NAagesRemoved$Age[NAagesRemoved$Sex == "female"])
fem_median_age
#median age for Male Victims
male_median_age = median(NAagesRemoved$Age[NAagesRemoved$Sex == "male"])
male_median_age
#mean age for female victims
fem_mean_age= mean(NAagesRemoved$Age[NAagesRemoved$Sex == "female"])
fem_mean_age
#mean age for Male Victims
male_mean_age = mean(NAagesRemoved$Age[NAagesRemoved$Sex == "male"])
male_mean_age


#We can then create a rough histogram to visualize distribution of ages amoung passengers
ggplot(NAagesRemoved) + geom_histogram(aes(x = Age), bins = 35)

#Better visualisation of age distribution
hist(NAagesRemoved$Age, prob = T, ylim=c(0,0.04), main = "Histogram of Ages", las = 1, xlab = "Age")
lines(density(NAagesRemoved$Age), col = 2, lwd = 3)

# Boxplot of Ages by Sex 
boxplot(NAagesRemoved$Age ~ NAagesRemoved$Sex, main="Boxplot of Passenger Age vs Sex",
        xlab="Sex", ylab="Age")


#Generating figure of results for Age variable after NA removal
par(mfrow = c(1,2)) 
hist(NAagesRemoved$Age, prob = T, ylim=c(0,0.04), main = "Histogram of Ages", las = 1, xlab = "Age")
lines(density(NAagesRemoved$Age), col = 2, lwd = 3)
boxplot(NAagesRemoved$Age ~ NAagesRemoved$Sex, main="Boxplot of Passenger Age vs Sex",
        xlab="Sex", ylab="Age")


#A more detailed descriptive statistic of the age variable can be gathered using the summary() function
summary(NAagesRemoved$Age)

#subset of data for females
FemData = NAagesRemoved[NAagesRemoved$Sex == "female",]
summary(FemData$Age)

#subset of data for males
MaleData = NAagesRemoved[NAagesRemoved$Sex == "male",]
summary(MaleData$Age)

#There are a number of extreme outliers amongst the ages, particularly amongst males. which can skew the mean
#Therefore all NA values with be imputed with the median age of all passengers
#Replace all NA values in the Age column with the calculated median Age of all passengers from the original dataset.
titanic2$Age[is.na(titanic2$Age)] <- median(titanic2$Age,na.rm=T)

#Check replacements of NA's has taken place
titanic2$Age 

#viewing the dataset again in tabular format to confirm correct substitution of all NA values
View(titanic2)

#Re-evaluate missing values (#Note that missing values are now identified in columns "Cabin" and Embarked")
colSums(is.na(titanic2))

# Re-visualize missing values
missingview <- aggr(titanic2, prop = FALSE, numbers = TRUE)


#Note that there are many missing values for the Cabin feature 
#However, we are not interested in this variable for our anaylsis
#It is also not expected to contain useful information about whether a passenger survives or not
#similarly the passenger ID is not expected to contain any insights about this either
# We will therefore remove both Passenger ID and Cabin number from our data set

#creating a new dataset with desired variables
#subsetting the data and isolating Survived, Pclass, Sex, Age and Name 
titanic3 = (titanic2[,c("Survived","Pclass", "Sex", "Age", "Name")])
#This can also be performed else by omitting all of the undesired variables
titanic3 = subset(titanic2, select = -c(PassengerId,Ticket,Cabin, SibSp, Parch, Embarked, Fare))

#Cleaning workspace by Removing excess objects
rm(titanic1)
rm(titanic2)

#=================================================================
#Modifying the table and transforming data class
#===============================================================

#First idenitfy the types of data we are now working with
str(titanic3)

#Further massage data into a format more amenable for data analysis.

#Pclass is a proxy for socio economic status 
#Would be more appropriate as a categorical variable. 

# The Survived feature would also be more approriate as categorical varible.

#Change both variables into categorical data types
titanic3$Pclass = as.factor(titanic3$Pclass)
titanic3$Survived = as.factor(titanic3$Survived)

#One of the research objectives is to see if the title of passengers affected survivability?
#We can take a quick look at the different title types ie. Mr., Mrs., Miss etc
head(as.character(titanic3$Name))

#As observed, the passengers have a certain title attached to their name
# We can extract these title from the passenger name

# A utility function was created to help with title extraction
#(Code from David Langer's Youtube Tutorial was used for title extraction. Please see References section from the Report)

#Adding new variable - Title
#First define a single function called extractTitle that takes in a single parameter, in this case "Name"
#This function will also convert the "Name" feature values into a character string
#grep() is a pattern matching function 
#We will combine this with an if-else statement such that if it recognises the pattern of
#a particular string ("miss", "Mr", "Mrs" and "Master") within the name, it will then return the value 
#of the name that is greater than the length of 0

extractTitle<-function(Name){
  Name<-as.character(Name)
  if(length(grep("Miss.",Name))>0){
    return("Miss.")
  } else if (length(grep("Master.",Name))>0){
    return("Master.")
  } else if(length(grep("Mrs.",Name))>0){
    return("Mrs.")
  } else if(length(grep("Mr.", Name))>0){
    return("Mr.")
  } else {
    return("Other")
  }
}

#Create a variable that will take advantage of the function.
Title<-NULL

#create a loop over all of the rows in the titanic data
#And then take whatever comes out of this function call and 
#add it to totals 
for (i in 1:nrow(titanic3)){
  Title<-c(Title,extractTitle(titanic3[i,"Name"]))
}


### Factorise our newly created "Title" variable
titanic3$title<-as.factor(Title)

#isolate the "Title" column as a vector to view values
New_Variable_Title = titanic3[,6]

#Checking to see the values by selecting the first 200 elements inside of the newly created vector
New_Variable_Title[1:200]

#No longer need "Name" variable in final dataset
titanic3 = subset(titanic3, select = -c(Name))

#We have finally finished treating all of the relevant missing values in the Titanic dataset. 
#We have also successfully created and added new variable called Title

#Checking structure of final table for data analysis
View(titanic3)
str(titanic3)


#==============================================
#EXPLORATORY DATA ANALYSIS/DATA VISUALIZATION
#===============================================

#First checking how many passengers are being analying in this final feature array
#total passengers
total_passengers = nrow(titanic3)
total_passengers

#Frist perform a broad statistical analysis of the finalized dataset
summary(titanic3)

#Research Objectives
#Does SURVIVED assoicate with passenger 
#1. SEX
#2. AGE
#3. PCLASS
#4. TITLE


#####FIRST LOOK AT FREQUENCY DISTRIBUTION OF AGE, SEX AND PCLASS IN THE SUBSETTED DATASET

#EXPLORING AND SUBSETTING DIFFERENT GROUPS IN SEX
#subset of data for females
FemData = titanic3[titanic3$Sex == "female",]
#number of total females
number_of_fem = length(FemData$Age)
number_of_fem

#subset of data for males
MaleData = titanic3[titanic3$Sex == "male",]
#number of total males
number_of_male = length(MaleData$Age)
number_of_male

#FEMALE vs MALE PERCENTAGES for the whole sample
FemPercent = (length(FemData$Age)/total_passengers)*100
FemPercent
MalePercent = (length(MaleData$Age)/total_passengers)*100
MalePercent

#viewing the percentage female to male for the whole sample
table(MalePercent, FemPercent)


#EXPLORING AND STRATIFYING DIFFERENT GROUPS IN AGE
#ADULT vs CHILD
#subset of data for adults
AdultData = titanic3[titanic3$Age >= 18, ]
#Total number of adults
number_of_adult = length(AdultData$Age)
number_of_adult
#subset of data for female adults (>18)
FemDataAdult = titanic3[titanic3$Sex == "female" & titanic3$Age >= 18.00, ]
#number of female adults
number_of_FemAdult = length(FemDataAdult$Age)
number_of_FemAdult
#subset of data for male adults (>18)
MaleDataAdult = titanic3[titanic3$Sex == "male" & titanic3$Age >= 18.00, ]
#number of male adults
number_of_MaleAdult = length(MaleDataAdult$Age)
number_of_MaleAdult

#subset of data for children
ChildData = titanic3[titanic3$Age < 18, ]
#number of children
number_of_child = length(ChildData$Age)
number_of_child
#subset of data for female children (<18)
FemChildData = titanic3[titanic3$Sex =="female" & titanic3$Age < 18.00,]
#number of female children
number_of_FemChild = length(FemChildData$Age)
number_of_FemChild
#subset of data for male children (<18)
MaleChildData = titanic3[titanic3$Sex =="male" & titanic3$Age < 18.00,]
#number of male children
number_of_MaleChild = length(MaleChildData$Age)
number_of_MaleChild

#ADULT CHILD PERCENTAGES
#percentage of adults relative to total number of passengers
AdultPercent = (number_of_adult/total_passengers)*100
AdultPercent
#percentage of children relative to total number of passengers
ChildPercent = (number_of_child/total_passengers)*100
ChildPercent

#viewing the percentages of adult to child passengers
table(AdultPercent, ChildPercent)


#EXPLORING AND SUBSETTING DIFFERENT GROUPS IN PCLASS
#subset first class
firstclass = titanic3[titanic3$Pclass == 1,]
#number of first class passengers
number_of_firstclass = length(firstclass$Age)
number_of_firstclass

#subset second class
secondclass = titanic3[titanic3$Pclass == 2,]
#number of second class passengers
number_of_secondclass = length(secondclass$Age)
number_of_secondclass

#subset third class
thirdclass = titanic3[titanic3$Pclass == 3, ]
#number of third classs passengers
number_of_thirdclass = length(thirdclass$Age)
number_of_thirdclass

#percentage of passengers that were first class
Percentfirstclass = (number_of_firstclass/total_passengers)*100
Percentfirstclass
#percentage of passengers that were second class
Percentsecondclass = (number_of_secondclass/total_passengers)*100
Percentsecondclass
#percentage of passengers that were third class
Percentthirdclass = (number_of_thirdclass/total_passengers)*100
Percentthirdclass

#table of percentages of classes amongst passengers
table(Percentfirstclass, Percentsecondclass, Percentthirdclass)

#PLOTTING DISTRIBUTION OF SEX, AGE AND PCLASS

#make a frequency table of the sexes
Sex_freq = table(titanic3$Sex)
#Making a relative frequency table of the sexes
Sex_freq_percent = (Sex_freq/714)*100

#make a frequency table of classes
Class_freq = table(titanic3$Pclass)
#Making a relative frequency table of classes
Class_freq_percent = (Class_freq/714)*100

#making Age Groups
AgeGroups = cut(titanic3$Age, breaks = c(0, 17, 80), labels = c("Child (<18)", "Adult (18+)"))
#frequency table of Age Groups
AgeGroups_freq = table(AgeGroups)
#Making a relative frequency table of classes
AgeGroups_freq_percent = (AgeGroups_freq/714)*100


#Group final frequency distribution plot of all target variables together for final summary figure
par(mfrow=c(1,3))
#bar chart of relative frequencies of SEX
barplot(Sex_freq_percent, main = "Percentages of female & male passengers", xlab = "Gender", ylab = "Percentage", 
        names.arg = c("Female (36.55%)", "Male (63.45%)"), las = 1, ylim = c(0, 100), col=c("chartreuse", "blue4"))
#bar chart of relative frequencies of AGE
barplot(AgeGroups_freq_percent, main = "Percentages of Adults and children", xlab = "Age", ylab = "Percentage", 
        names.arg = c("Children (15.83%)", "Adult (84.17%)"), las = 1, ylim = c(0,150), col=c("mistyrose", "coral"))
#bar chart of relative frequencies of PCLASS
barplot(Class_freq_percent, main = "Percentages of Classes", xlab = "Class", ylab = "Percentage",
        names.arg = c("1st (26.05%)", "2nd (24.23%)", "3rd (49.72%)"), las = 1, ylim = c(0,80), col=c("aquamarine", "cadetblue3", "cadetblue4"))



#=================================================================
#Distribution of Survivors
#===============================================================

#Now that we're more familiar with the demographics of the passengers in this dataset, we will explore how many of them perished and lived

#Survivors vs victims
#subset of survivors
Survivors = titanic3[titanic3$Survived == 1, ]
#total number of survivors
number_of_Survivors = length(Survivors$Age)
#percentage of survirors
PercentSurvivors = (number_of_Survivors/total_passengers)*100
PercentSurvivors

#victims
Victims = titanic3[titanic3$Survived == 0, ]
#total number of victims
number_of_Victims = length(Victims$Age)
#percentage of victims
PercentVictims = (length(Victims$Age)/total_passengers)*100
PercentVictims

#viewing the percentage of victims vs survivors
(table(PercentVictims, PercentSurvivors))


#Now let's visualise all of passenger that surivived and persished 

#For easier visualisation, we will change 0 and 1 values of Survived to no and yes, respectively.
levels(titanic3$Survived)
levels(titanic3$Survived)[1]<-"no"
levels(titanic3$Survived)[2]<-"yes"
levels(titanic3$Survived)

#One method to visualise this
p <- ggplot(titanic3, aes(Survived))
# Number of cars in each class:
p + geom_bar(aes(fill = Survived))+ ggtitle('Survived?')+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = .9)

#Second method to visualise data (Final figure for report)
survived_dataframe <- data.frame(
  x = factor(c('Survived (38.38%)', 'Died (61.61%)')),
  y = c(number_of_Survivors, number_of_Victims )
)
survived_dataframe

g <- ggplot(survived_dataframe, aes(x=x, y=y))
g <- g + geom_bar(stat='identity', aes(fill=x))
g <- g + labs(title="Titanic survivors", x='', y='')
g


#=================================================================
#1.SEX and SURVIVED
#=============================================================== 

#Hypothesis - women survived at a higer rate

library(stringr)
library(dplyr)
library(ggplot2)

d <- titanic3 %>% select(Survived, Sex)
g <- ggplot(data = d, aes(x=Sex, fill=Survived))
g <- g + geom_bar(position="fill") 
g <- g + labs(title="Women/Men Survivors", x="Sex", y="Number of passengers") 
g <- g + scale_fill_discrete(name="Survived", labels=c("yes", "no"))
g

#OR else
par(mfrow=c(1,1))
table_sex_survived = table(titanic3$Sex, titanic3$Survived)
table_sex_survived
barplot(table_sex_survived, beside = T, 
        main = "Survival by Sex", xlab = "Survived", ylab = "Passenger counts", legend.text = c("female", "male"))


levels(titanic3$Survived)
levels(titanic3$Sex)
titanic3$Survived


#survival relative to each gender
#female survivor dataset
FemSurvivors = titanic3[titanic3$Sex == "female" & titanic3$Survived == "yes",]
#number of female survivors
number_of_FemSurvivors = nrow(FemSurvivors)
#percentage of survivors that are female
PercentFemSurvivorsTotal = (number_of_FemSurvivors/number_of_Survivors)*100
PercentFemSurvivorsTotal
#percentage of females that survived
PercentFemSurvivors = (number_of_FemSurvivors/number_of_fem)*100
PercentFemSurvivors

#male survivor dataset
MaleSurvivors = titanic3[titanic3$Sex == "male" & titanic3$Survived == "yes",]
#numer of male survivors
number_of_MaleSurvivors = nrow(MaleSurvivors)
#percentage of survivors that are male
PercentMaleSurvivorsTotal = (number_of_MaleSurvivors/number_of_Survivors)*100
PercentMaleSurvivorsTotal
#percentage of males that survived
PercentMaleSurvivors = (number_of_MaleSurvivors/number_of_male)*100
PercentMaleSurvivors

#=================================================================
#2. AGE and SURVIVED
#=============================================================== 

#histogram of Age density distribution with density curve
titanic3$Age = as.numeric(titanic3$Age)
par(mfrow = c(1,2)) # use tp put plots in same place
hist(titanic3$Age, prob = T, ylim=c(0,0.06), main = "Histogram of Ages", las = 1, xlab = "Age")
lines(density(titanic3$Age), col = 2, lwd = 3)
boxplot(titanic3$Age ~ titanic3$Sex, main="Boxplot of Passenger Age vs Sex",
        xlab="Sex", ylab="Age")

#Hypothesis - children survived at a higer rate
# number of survivals and nonsurvivals across different ages

par(mfrow=c(1,1))
ggplot(titanic3, aes(x=Age, fill=factor(Survived))) +
  geom_histogram(bins=30)+
  ggtitle("Age vs Survived")+
  scale_fill_discrete(name="Survived") +
  xlab("Age") +
  ylab("Number of passengers") 


#Upon closer inspection of the data, children had a proportionally higher chance of survival.
ggplot(data = titanic3[!is.na(titanic3$Age),],aes(x=Age,fill=Survived))+geom_histogram(binwidth = 3,position="fill")+ylab("Frequency")

#making Age Groups
AgeGroups = cut(titanic3$Age, breaks = c(0, 17, 80), labels = c("Child (<18)", "Adult (18+)"))
#frequency table of Age Groups
AgeGroups_freq = table(AgeGroups)

#relationship between Age and survivorshop
table_agegroups_survived = table(AgeGroups, titanic3$Survived)
barplot(table_agegroups_survived, beside = T, main = "Survival by Age Group", xlab = "Survival: 0 = No,
        1 = Yes", ylab = "Passenger counts", legend.text = c("Child (<18)", "Adult (18+)"))


#=================================================================
#3. PCLASS and SURVIVED
#=============================================================== 

#CLASS AND SURVIVORSHIP
#subset of first class survivors
FirstClassSurvivors = titanic3[titanic3$Pclass == "1" & titanic3$Survived == "yes", ]
#number of first class survivors
number_of_FirstClassSurvivors = nrow(FirstClassSurvivors)
number_of_FirstClassSurvivors
#subset of second class survivors
SecondClassSurvivors = titanic3[titanic3$Pclass == "2" & titanic3$Survived == "yes", ]
#number of second class survivors
number_of_SecondClassSurvivors = nrow(SecondClassSurvivors)
number_of_SecondClassSurvivors
#subset of third class survivors
ThirdClassSurvivors = titanic3[titanic3$Pclass == "3" & titanic3$Survived == "yes", ]
#number of third class survivors
number_of_ThirdClassSurvivors = nrow(ThirdClassSurvivors)
number_of_ThirdClassSurvivors

#table showing the count of survivors according to class
table(number_of_FirstClassSurvivors, number_of_SecondClassSurvivors, number_of_ThirdClassSurvivors)

#percentage of survivors that were first class
PercentSurvivorsWithin1stClass = (number_of_FirstClassSurvivors/number_of_Survivors)*100
PercentSurvivorsWithin1stClass
#percentage of survivors that were second class
PercentSurvivorsWithin2ndClass = (number_of_SecondClassSurvivors/number_of_Survivors)*100
PercentSurvivorsWithin2ndClass
#percentage of survivors that were third class
PercentSurvivorsWithin3rdClass = (number_of_ThirdClassSurvivors/number_of_Survivors)*100
PercentSurvivorsWithin3rdClass

#table showing the percetages of survivors according to class
table(PercentSurvivorsWithin1stClass,PercentSurvivorsWithin2ndClass,PercentSurvivorsWithin3rdClass)


#perentage of first class that were survivors
Percent1stClassSurvivors = (number_of_FirstClassSurvivors/number_of_firstclass)*100
Percent1stClassSurvivors
#perentage of second class that were survivors
Percent2ndClassSurvivors = (number_of_SecondClassSurvivors/number_of_secondclass)*100
Percent2ndClassSurvivors
#perentage of third class that were survivors
Percent3rdClassSurvivors = (number_of_ThirdClassSurvivors/number_of_thirdclass)*100
Percent3rdClassSurvivors

#The percentage of survivors within class subsets
table(Percent1stClassSurvivors, Percent2ndClassSurvivors, Percent3rdClassSurvivors)

#Social Class influence in Survival?
#Hypothesis - Rich folks survived at a higer rate
#if passengers of upper class have better survival rate than its population.
titanic3$pclass <- as.factor(titanic3$Pclass)
ggplot(titanic3, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  ggtitle("Pclass vs Survived")+
  xlab("Pclass") +
  ylab("Number of Passengers") +
  labs(fill = "Survived") 

#option 2
ggplot(titanic3, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(stat="count",position="fill") +
  ggtitle("Pclass vs Survived")+
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 

ggplot(titanic3, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(width = 0.5, position="dodge") +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# option 3
ggplot(data = titanic3,aes(x=Pclass,fill=Survived))+geom_bar(position="fill")+ylab("Frequency")

#calculate t test
t.test(titanic3$Age ~ titanic3$Survived, data=titanic3)

####################### 4. NAME(TITLE) and SURVIVED
#Hypothesis - Title may be predictive for survival

#option 1
titanic3$pclass <- as.factor(titanic3$Pclass)
ggplot(titanic3, aes(x = Title, fill = factor(Survived))) +
  geom_bar() +
  ggtitle("Title vs Survived")+
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived") 

#option 2
ggplot(data = titanic3,aes(x=Title,fill=Survived))+geom_bar(position="fill")+ylab("Frequency")




#survival rate for each title group by creating a  table.

totaltitlesummary <- summary(titanic3$title)
totaltitlesummary
TotalMasters <- totaltitlesummary[1]
TotalMasters
TotalMiss <- totaltitlesummary[2]
TotalMiss
TotalMr <- totaltitlesummary[3]
TotalMr
TotalMrs <- totaltitlesummary[4]
TotalMrs
TotalOther <- totaltitlesummary[5]
TotalOther

#Title AND SURVIVORSHIP
#subset of "Miss" Survivors
MissSurvivors = titanic3[titanic3$title == "Miss." & titanic3$Survived == "yes", ]
#number of "Miss" Survivors
number_of_MissSurvivors = nrow(MissSurvivors)
number_of_MissSurvivors
#subset of "Mrs" Survivors
MrsSurvivors = titanic3[titanic3$title == "Mrs." & titanic3$Survived == "yes", ]
#number of "Mrs" Survivors
number_of_MrsSurvivors = nrow(MrsSurvivors)
number_of_MrsSurvivors
#subset of "Master" Survivors
MasterSurvivors = titanic3[titanic3$title == "Master." & titanic3$Survived == "yes", ]
#number of "Master" Survivors
number_of_MasterSurvivors = nrow(MasterSurvivors)
number_of_MasterSurvivors
#subset of "Mr" Survivors
MrSurvivors = titanic3[titanic3$title == "Mr." & titanic3$Survived == "yes", ]
#number of "Mr" Survivors
number_of_MrSurvivors = nrow(MrSurvivors)
number_of_MrSurvivors
#subset of "Other" Survivors
OtherSurvivors = titanic3[titanic3$title == "Other" & titanic3$Survived == "yes", ]
#number of "Other" Survivors
number_of_OtherSurvivors = nrow(OtherSurvivors)
number_of_OtherSurvivors


#perentage of Miss passengers that were survivors
PercentMissSurvivors = (number_of_MissSurvivors/TotalMiss)*100
PercentMissSurvivors
#perentage of Mrs passengers that were survivors
PercentMrsSurvivors = (number_of_MrsSurvivors/TotalMrs)*100
PercentMrsSurvivors
#perentage of Master passengers that were survivors
PercentMasterSurvivors = (number_of_MasterSurvivors/TotalMasters)*100
PercentMasterSurvivors
#perentage of Mr passengers that were survivors
PercentMrSurvivors = (number_of_MrSurvivors/TotalMr)*100
PercentMrSurvivors
#perentage of Other passengers that were survivors
PercentOtherSurvivors = (number_of_OtherSurvivors/TotalOther)*100
PercentOtherSurvivors

#The percentage of survivors within class subsets
table(PercentMasterSurvivors, PercentMissSurvivors, PercentMrSurvivors, PercentMrsSurvivors, PercentOtherSurvivors)



