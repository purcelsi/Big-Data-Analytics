#APPENDIX FOR CA2 PROGRAMMING FOR BIG DATA
#DATASET 1.
#STUDENT NO: 18195342 SIOBHAN PURCELL



#--------------------------------------------------------------------------



#A HUMAN RESOURCES EMPLOYEE DATASET ANALYSIS SCRIPT USING R



#--------------------------------------------------------------------------

# ----------------------------------
#           Load Libraries
# ----------------------------------

install.packages("ggplot2")
install.packages("devtools")
install.packages("plotly")
install.packages("dplyr")
install.packages("reshape2")

library(ggplot2)
library(devtools)
library(plotly)
library(dplyr)
library(reshape2)


#set working directory
setwd("C:/Users/Siobhan/Desktop/Datasets")


# Read in full hrdata.csv file
hrdata <- read.csv("hrdata.csv", header = T,  sep=",")


#########################################################
##                                                     ##
##          1.	Cleaning The Data                      ##
##                                                     ##
#########################################################

#Inspect the data and observe number of entries
dim(hrdata)  

# Look at the first few rows of the dataset
head(hrdata)

#As we can see this is a very large dataset
#Contains 14,999 employee observations and 10 features

# Inspecting data to check for any missing values
is.na(hrdata)
which(is.na.data.frame(hrdata))
sum(is.na(hrdata)) 
#There is 0 missing values in the dataset.
#Hence we don't need to use any missing value techniques.


#Since there are no NA, no need to omit or complete them. 
#Thus we proceed summarizing the data.

#Then perform basic summary statistic of the dataset.
summary(hrdata)

# View variable names
names(hrdata)
head(hrdata)
str(hrdata)

#From initial obervations we can see that the names for 
#some of these variables are either not representative 
#of their data or are a little ambiguous

#For example, the "Sales" variable does not accurately represent the data it stores
#This values represents the number of employees in each department
summary(hrdata$sales)

#We will therefore rename some of the features to more approriate titles

str(hrdata)
#make copy of orignal dataset
hrdata2 <- hrdata
#rename of of the varibales to more approriate clearer names
names(hrdata2)[2]<-"evaluation_score"
names(hrdata2)[4]<-"average_monthly_hours"
names(hrdata2)[5]<-"years_with_company"
names(hrdata2)[9]<-"department"


#check new title names and compare with original file
names(hrdata)
names(hrdata2)

#view the types of data we have
str(hrdata2)


#check data structure again to confirm changes to variable names and data type
str(hrdata2)


#########################################################
##                                                     ##
##          2. Descriptive Statistics                   ##
##                                                     ##
#########################################################

#What was the employee churn rate in this company? ie Attrition/Turnover rate

left <- hrdata2[hrdata2$left =="1",]
nrow(left)
stayed<- hrdata2[hrdata2$left == "0",]
nrow(stayed)
nrow(hrdata2)
percent_stayed <- nrow(left)/nrow(hrdata2)*100
percent_left <- nrow(stayed)/nrow(hrdata2)*100
#create a summary table of this
table(percent_stayed,percent_left)

#The company had a turnover rate of about 24

#Visualise the turnover vs non-turnover of employees 
ggplot(hrdata2, aes(x=left, fill=factor(left))) +
  geom_histogram(bins=2)+
  ggtitle("Employment Status Of Employees")+
  scale_fill_discrete(name="left")+
  labs(x = 'Status', y = 'Count')

#########################################################
##                                                     ##
##         3. Business nature of company               ##
##                                                     ##
#########################################################

#what types of departments does this company have and how many employees do they have?
#Does a certain department have more employyees than another?
#Are the departments balanced in terms of employee count?
dept.table<-with(hrdata2,table(department))
dept.table


# What is the visual distrbution by employee dept?
ggplot(hrdata2, aes(x = department)) +
  geom_histogram(bins = 5,stat = 'count', binwidth = .5) +
  theme_minimal(base_size = 16, base_family = 'Roboto') + 
  geom_bar(aes(fill = '')) + 
  scale_fill_discrete(name="No. of employees")+
  theme(axis.text.x = element_text(size=9, angle=45)) +
  labs(title = 'Employee Count By Department', x = 'Department', y = 'No. of employees')


#Appears to be a higher count in the Sales, Technical and Support department


#Which department has the highest employee turnover? 

#table showing the number of employee who left and stayed in each department
dept_turnover<-table(hrdata2$department,hrdata2$left)
dept_turnover

#table showing the attriation rate within each department
dept_turnover_stayed <- dept_turnover[,1]
dept_turnover_stayed
dept_turnover_left <- dept_turnover[,2]
dept_turnover_left


#re-create a data frame of dept_turnover table (turnover across all departments)
dept_turnover<-as.data.frame(dept_turnover)
dept_turnover
dept_turnover[1]
dept_turnover[2]
#renmae Variable 1(Var1) and Variable 2 (Var2) 
names(dept_turnover)[1]<-"Department"
names(dept_turnover)[2]<-"left"
dept_turnover
dept_turnover<-subset(dept_turnover,left==1)
#print(d_vis_2)
library(ggplot2)
dept_turnover$Department <- factor(dept_turnover$Department, levels = dept_turnover$Department[order(dept_turnover$Freq)])
p1<-ggplot(dept_turnover, aes(x=Department,y=Freq,fill=Department)) +
  geom_bar(stat='identity') +theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p1)

#Turnover apperas higher in Sales, Technical and Support department

# Relative freq of Employee turnover within each department (relative frequency of employee turnover amongst department)
percent_left_within_dept<- round((dept_turnover_left/dept.table)*100)
percent_left_within_dept


#re-create a data frame of dept_turnover table within each


#########################################################
##                                                     ##
##          Correlation Heatmap                        ##
##                                                     ##
#########################################################

#Correlation Matrix for all employees 
library(reshape2)
library(ggplot2)
cor_vars_all<-hrdata2[,c("satisfaction_level","evaluation_score","number_project","average_monthly_hours","years_with_company","Work_accident","left","promotion_last_5years")]
cor(cor_vars_all)
trans<-cor(cor_vars_all)
melted_cormat <- melt(trans)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +theme(axis.text.x = element_text(angle = 90, hjust = 1))



#Correlation Matrix for employees that left

#subset of employees that left and all numerical variables for heatmap
left_heatmap_subset <- subset(hrdata2, left =="1" , select=c(satisfaction_level, evaluation_score, number_project, average_monthly_hours,years_with_company, Work_accident,promotion_last_5years))
str(hrdata2)
nrow(left_heatmap_subset)

library(reshape2)
library(ggplot2)
cor_vars_left<-left_heatmap_subset 
cor(cor_vars_left)
trans<-cor(cor_vars_left)
melted_cormat2 <- melt(trans)

ggplot(data = melted_cormat2, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +theme(axis.text.x = element_text(angle = 90, hjust = 1))


#From the heatmap, there is a positive(+) correlation between 
#projectCount, averageMonthlyHours, and evaluation

#From the heatmap, there is a negative(-) correlation between 
# turnover and satisfaction


#Separating Employee Left and Employee Stayed into two subsets containing variables most correlated to each other from heatmap

#subset employees on those that left and that were most correlated to each other from heatmap
leftdata <- subset(hrdata2, left =="1" , select=c(satisfaction_level, evaluation_score, number_project, average_monthly_hours))

#Confirming subset selection
nrow(leftdata)
nrow(left)
str(leftdata)

#subset employees on those that stayed and variables that were most correlated to each other from heatmap
stayeddata <- subset(hrdata2, left =="0" , select=c(satisfaction_level, evaluation_score, number_project, average_monthly_hours))


##########################################################################
##                                                                      ##
##    Student's T- Test Hypothesis - Unpaired/Independent Samples       ##
##                                                                      ##
##                                                                      ##
##########################################################################

#Performing Student's T- Test to see if sample mean differs from the population mean
#Is the the mean satisfication levels of employees who left significantly different
#from the rest of the employee population?

#Null hypothesis: no difference in mean satisfication levels between employees who left and stayed
#Alternate hypothesis: difference in mean satisfaction levels between employees who left and stayed

#Conduct an unpaired student t-test to see to compare mean satisfication levels between employees who left and stayed
t.test(hrdata2$satisfaction_level, hrdata2$left, alternative = "two.sided", paired = FALSE)

#Since p- value is <0.05 we can reject null hypothesis and conclude that there is a significant difference

#Conduct  unpaired student t-test to see to compare mean number of monthly hours worked between employees who left and stayed
t.test(hrdata2$average_monthly_hours~hrdata2$left, alternative = "two.sided", paired = FALSE)

#Since p- value is <0.05 we can reject null hypothesis and conclude that there is a significant difference


#Conduct an unparired student t-test to see to compare mean evaluation scores between employees who left and stayed
t.test(hrdata2$evaluation_score~hrdata2$left, alternative = "two.sided", paired = FALSE)

#Since p- value is >0.05 we cannot reject null hypothesis and must therefore conclude that there is no significant difference


#Conduct a t-test to see to compare mean number of projects between employees who left and stayed
t.test(hrdata2$number_project~hrdata2$left, alternative = "two.sided", paired = FALSE)

#Since p- value is <0.05 we can reject null hypothesis and conclude that there is a significant difference



##########################################################################
##                                                                      ##
##  Distribution Plots (Satisfaction - Evaluation - AverageMonthlyHours ##                 
##                                                                      ##
##########################################################################

 
#Distribution Plots of satisfaction, evaluation_score and average_monthly_hours for employees that left               
par(mfrow=c(1,3))
#Histogram of Employees Left by Satisfication Level  
hist(leftdata$satisfaction_level, col="green", main = "Histogram of Satisfaction Level", las = 1, xlab = "Satisfaction level of employees", cex.lab=1.75, cex.axis=1.25, cex.main=1.5)
#Histogram of Employees Left by Evaluation Score 
hist(leftdata$evaluation_score, col="red", main = "Histogram of Evaluation Scores", las = 1, xlab = "Histogram of Evaluation Scores", cex.lab=1.75,cex.axis=1.25, cex.main=1.5)
#Histogram of Employees Left by Average Monthly Hours  
hist(leftdata$average_monthly_hours, col="blue", main = "Histogram of average monthly hours at workplace", las = 1, xlab = "Average monthly hours",cex.lab=1.75, cex.axis=1.25, cex.main=1.5)


#Distribution Plots of satisfaction, evaluation_score and average_monthly_hours for employees that stayed     
par(mfrow=c(1,3))
#Histogram of Employees Stayed by Satisfication Level
hist(stayeddata$satisfaction_level, col="green", main = "Histogram of Satisfaction Level", las = 1, xlab = "Satisfaction level of employees", cex.lab=1.75, cex.axis=1.25,cex.main=1.5)
#Histogram of Employees Stayed by Evaluation Score 
hist(stayeddata$evaluation_score, col="red", main = "Histogram of Evaluation Scores", las = 1, xlab = "Evaluation Scores", cex.lab=1.75,cex.axis=1.25, cex.main=1.5)
#Histogram of Employees Left by Average Monthly Hours
hist(stayeddata$average_monthly_hours, col="blue", main = "Histogram of average monthly hours at workplace", las = 1, xlab = "Average monthly hours", cex.lab=1.75,cex.axis=1.25, cex.main=1.5)

##########################################################################
##                                                                      ##
##               Turnover V.S. ProjectCount                             ##
##                                                                      ##
##########################################################################


project_turnover<-table(hrdata2$number_project,hrdata2$left)
project_turnover<-as.data.frame(project_turnover)
#print(d_vis_1)
library(ggplot2)
p<-ggplot(project_turnover, aes(x=Var1,y=Freq,fill=Var2)) +
  geom_bar(position="dodge",stat='identity') +
  ggtitle("Turnover V.S ProjectCount")
geom_bar(colour = "black")+
  
  
  print(p)

##########################################################################
##                                                                      ##
##               ProjectCount VS AverageMonthlyHours                    ##
##                                                                      ##
##########################################################################

#Boxplot of Project count vs AverageMonthlyHours

#Average number of employees who stayed worked about 200hours/month. 
#Those that had a turnover worked about 250hours/month and 150hours/month

par(mfrow=c(1,1))
p<-ggplot(hrdata2, aes(x = factor(number_project), y = average_monthly_hours, fill = factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("green", "red"))
print(p)

##########################################################################
##                                                                      ##
##               number_project VS Evaluation                             ##
##                                                                      ##
##########################################################################

mean(hrdata$last_evaluation)

#number_project VS Evaluation
#Looks like employees who did not leave the company had an average evaluation of around 70% even with different projectCounts
#There is a huge skew in employees who had a turnover though. It drastically changes after 3 projectCounts. 
#Employees that had two projects and a horrible evaluation left. Employees with more than 3 projects and super high evaluations left
p<-ggplot(hrdata2, aes(x = factor(number_project), y = evaluation_score, fill = factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("blue", "orange"))
print(p)


##########################################################################
##                                                                      ##
##            Satisfaction VS Evaluation and Average_Monthly_Hours      ##
##                                                                      ##
##########################################################################


#atisfaction VS Evaluation
ggplot(leftdata, aes(satisfaction_level, evaluation_score, color = leftdata)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE, colour = "red") 
#satisfaction VS Average_Monthly_Hours
ggplot(leftdata, aes(satisfaction_level, average_monthly_hours, color = leftdata)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE, colour = "Orange")
names(leftdata)


#########################################
##                                     ##
##     Multivariate Comparison         ##
##                                     ##
#########################################

library(plotly)

#Comparison among Clustering in Multivariate Analysis
#amoung employee left data

library(plotly)
interactiveplot <-plot_ly(data = leftdata, x = ~evaluation_score, 
        y = ~satisfaction_level, z = ~average_monthly_hours) %>%
  layout(title = "Last Evaluation Score vs Satisfaction Level vs Hours Worked")

#3D and interative comparative multivariate model of Last Evaluation Score vs Satisfaction Level vs Hours Worked
interactiveplot

#open devntools library to publish interactive graphs
#to the web and make available on my plot ly dashboard account

#Be sure to have installed and opened these packages to to create/view interactive plot online
#install.packages("devtools")
library(devtools)
library(plotly)

Sys.setenv("plotly_username"="purcelsi")
Sys.setenv("plotly_api_key"="AsmelDXKo0BHyL1ZYVgd")

api_create(interactiveplot, filename = "employee-left-multivariate-comparison")

#An interactive web-based version of this 3D model plot directly accessed via the link https://plot.ly/~purcelsi/1/#/.

##################################################################
##                                                              ##
##Definining employee subgrooups with an if/else and  for loop  ##
##                                                              ##
##                                                              ##
##################################################################


#Based on the findings so far we know that employees who left can be clustered in subgroups
#into particular subgroups according to their Last Evaluation Scores, Satisfaction Levels, and no. of Hours Worked


#We can use this information to define some of the employees who have left based on these characteristics 
#This can be used by employers to identify which of their remaining employees may leave soon

#To achieve this we will use a for loop and an if else statement
#Since there were a lot of records in the original hr data, I will use the sample_n() from dplyr to randomly select 1000 rows 
#This new dataset was then used as an example to test this model

#First we will randomly select 1000 employees from the entire dataset (includes employees who left and stayed with company)
#Be sure to have loaded in library(dplyr)
library(dplyr)

samplehrdata2 <- sample_n(hrdata2, 1000)

#To achieve this we will use a for loop and an if else statement
#Using information gathered on all of the employees so far from the previous analysis, we can generate a for loop to help us predict  employees who may leave and the potential nature of it
 for (i in 1:1000) {
  if (samplehrdata2$satisfaction_level[i] >=0.7 & samplehrdata2$evaluation_score[i]>=0.8 & samplehrdata2$average_monthly_hours[i]>=212.5 & samplehrdata2$average_monthly_hours[i]<=275) {
    print(paste("Great-Performing, Hard-working and Happy Employee with Ambitious Career Goals"))
  } else if (samplehrdata2$satisfaction_level[i] >=0.35 & samplehrdata2$satisfaction_level[i] <=0.45 & samplehrdata2$evaluation_score[i]< 0.5 & samplehrdata2$average_monthly_hours[i]<=170) {
    print(paste("Underperforming, Unengaged and Sad Employee"))
  } else if (samplehrdata2$satisfaction_level[i] >=0.2 & samplehrdata2$evaluation_score[i]>=0.75 & samplehrdata2$average_monthly_hours[i]>= 237.5) {
    print(paste("Great-Performing, Hard-working but Sad Employee (Potentailly Overworked)"))
  } else
    print(paste("Other"))
 }


nrow(leftdata)
#Testing for loop in employee left dataset to provide descriptive summary on characterisics employees who left
for (i in 1:nrow(leftdata)) {
  if (leftdata$satisfaction_level[i] >=0.7 & leftdata$evaluation_score[i]>=0.8 & leftdata$average_monthly_hours[i]>=212.5 & leftdata$average_monthly_hours[i]<=275) {
    print(paste("Great-Performing, Hard-working and Happy Employee with Ambitious Career Goals"))
  } else if (leftdata$satisfaction_level[i] >=0.35 & leftdata$satisfaction_level[i] <=0.45 & leftdata$evaluation_score[i]< 0.5 & leftdata$average_monthly_hours[i]<=170) {
    print(paste("Underperforming, Unengaged and Sad Employee"))
  } else if (leftdata$satisfaction_level[i] >=0.2 & leftdata$evaluation_score[i]>=0.75 & leftdata$average_monthly_hours[i]>= 237.5) {
    print(paste("Great-Performing, Hard-working but Sad/Stressed Employee (Potentailly Overworked)"))
  } else
    print(paste("Other"))
}

#Once we've assigned each employee in the dataset with a characteristic 
#We can subset those that would be deemed ideal and valuable to the company 
#ie. those that were defined as "Great-Performing, Hard-working and Happy Employee with Ambitious Career Goals"
#and those that are Great-Performing, Hard-working but Sad Employee (Potentailly Overworked)


####1. Great-Performing, Hard-working and Happy Employee with Ambitious Career Goals"#######
#Ambitious employees may be retained longer if offered a promotion
#Therefore we will see if any of these employees were promoted in the last 5 years
#May feel undervalues for the level of work they do? = see salary level



####2. Great-Performing, Hard-working but Sad Employee (Potentailly Overworked)"######
#Great-Performing, Hard-working but Sad Employee (Potentailly Overworked)
#May feel undervalues for the level of work they do? Might want a pay rise?


#Therfore we will revisit the original hr data. Subset the data for original selected variable as well as
# promotion_last_5years 
# years_with_company
# department
leftdata <- subset(hrdata2, left =="1" , select=c(salary,satisfaction_level, evaluation_score, number_project, average_monthly_hours, department, years_with_company, promotion_last_5years))
names(leftdata)


#We will shorten the employee characteristic descriptions
#Great-Performing, Hard-working and Happy Employee with Ambitious Career Goals = Ideal and Ambitious
#Great-Performing, Hard-working but Sad/Stressed Employee (Potentailly Overworked)" = Ideal and Sad/Stressed
#Underperforming, Unengaged and Sad Employee = Not ideal and poor employee
#Other = Other


#We will now add a new column to this dataset called employee_type
#create new column in leftdata and 
#Make sure to fill it a value through. We will use NA for this
leftdata$employee_type = NA

#Now we can see that we've created a new column
ncol(leftdata)
names(leftdata)

for (i in 1:nrow(leftdata)) {
  if (leftdata$satisfaction_level[i] >=0.7 & leftdata$evaluation_score[i]>=0.8 & leftdata$average_monthly_hours[i]>=212.5 & leftdata$average_monthly_hours[i]<=275) {
    leftdata$employee_type[i] <- "Ideal and Ambitious"
  } else if (leftdata$satisfaction_level >=0.35 & leftdata$satisfaction_level[i] <=0.45 & leftdata$evaluation_score[i]< 0.5 & leftdata$average_monthly_hours[i]<=170) {
    leftdata$employee_type[i] <-"Ideal and Sad/Stressed"
  } else if (leftdata$satisfaction_level >=0.2 & leftdata$evaluation_score[i]>=0.75 & leftdata$average_monthly_hours[i]>= 237.5) {
    leftdata$employee_type[i] <-"poor employee"
  } else
    leftdata$employee_type[i] <-"Other"
}

#Checking to see if employee_type has been populated
str(leftdata)

#############################
# IDEAL AND AMBITIOUS EMPLOYEES
#################################
#subset Ideal and Ambitious employees
Ideal_ambitious = leftdata[leftdata$employee_type ==  "Ideal and Ambitious",]
Ideal_ambitious$promotion_last_5years
Ideal_ambitious$department
Ideal_ambitious$years_with_company
Ideal_ambitious$salary


#we can see these subgroup of employees spent more than the average number of years working at this company
#These are a loyal long-term employees whom we would ideally like to keep
#Perhaps they wanted a change in career
mean(hrdata2$years_with_company)


#Perhaps Ideal_ambitious want a promotion?
#What datatype is promotion_last_5year?
class(Ideal_ambitious$promotion_last_5years) #integer
#Convert this into into a factor
Ideal_ambitious$promotion_last_5years = as.factor(Ideal_ambitious$promotion_last_5years)
promoted <-table(Ideal_ambitious$promotion_last_5years)
#table shows only 1 person of these employees were promoted within last 5 years

#Plot the table showing the number of Ideal ambitious employees who had a promotion in the last 5 years
barplot(promoted, beside = T, 
        main = "Ideal and Ambitious Employees Promoted Within Last 5 years", xlab = "Promoted Within Last 5 Years", ylab = "Freq", legend.text = c("Not Promoted", "Promoted"))


#Or perhaps Ideal_ambitious stayed too long with the company?
Ideal_ambitious$years_with_company
p <- ggplot(Ideal_ambitious, aes(years_with_company))
# Number of Ideal ambitious employees who had a promotion in the last 5 years
p + geom_bar(aes(fill = years_with_company))+ ggtitle('Years Ideal and Ambitious Employees Spent With Company')+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = .9)


#This looks to be the case when we compare this to the average number of years employees spend with the 
#company in the entire original hr data set.
mean(hrdata2$years_with_company)

#Or perhaps Ideal_ambitious they felt underpaid?
Ideal_ambitious$salary
p <- ggplot(Ideal_ambitious, aes(Ideal_ambitious$salary))
p + geom_bar(aes(fill = salary))+ ggtitle('Ideal and Ambitious Employees')+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = .9)

#CONCLUSION

#So maybe these employees worked too long with the company and wanted a promotion?
#May we could retain these valuable employees by offerining them promotions? 
#They also may want a higher salary
#Maybe we could increase their salary?

#############################
# Ideal and Sad employees   ##
#################################
#subset Ideal and Sad employees
Ideal_SadStressed = leftdata[leftdata$employee_type ==  "Ideal and Sad/Stressed",]
Ideal_SadStressed$promotion_last_5years
Ideal_SadStressed$department
Ideal_SadStressed$years_with_company
Ideal_SadStressed$salary

#perhaps Ideal_SadStressed stayed too long with the company?
Ideal_SadStressed$years_with_company
#Intestingly Ideal_SadStressed all work no more than 3 years with the company
length(Ideal_SadStressed$years_with_company)
nrow(Ideal_SadStressed)

#Given that non of these employees worked more than 5 years we will not asses the promotion_last_5_years variable

#But perhaps Ideal_SadStressed they felt underpaid?
p <- ggplot(Ideal_SadStressed, aes(salary))
p + geom_bar(aes(fill = salary))+ ggtitle('Ideal and Sad/Stressed Employees')+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = .9)




#CONCLUSION
#These employees have not worked substantially long with the company.
#Maybe they all leave after 3 years because they feel overworked for the amount of hours they do?
#May we could retain these valuable employees by offerining them promotions?
#May we could retain these valuable employees longer by increasing their salary? 


#############APPLYING FOR LOOP RO EMPLOYEE WHO STAYED ONLY ##################
#Testing for loop in a random sample of employee's who are currently remaining with the company.
#This might help idenitfy those who could potentially leave soon
#From identifying the Ideal_SadStressed and Ideal_ambitious employees, perhaps we could reduce future employee attrition by assessing their needs?



#Original subset of employees of those that stayed with the target variables
stayeddata <- subset(hrdata2, left =="0" , select=c(satisfaction_level, evaluation_score, number_project, average_monthly_hours))
stayeddata

#Again, We will now add a new column to this dataset called employee_type
#create new column in leftdata and 
#Make sure to fill it a value through. We will use NA for this
stayeddata$employee_type = NA


#apply the for loop to employees whos stayed (may leave company soon)a set


for (i in 1:nrow(stayeddata)) {
  if (stayeddata$satisfaction_level[i] >=0.7 & stayeddata$evaluation_score[i]>=0.8 & stayeddata$average_monthly_hours[i]>=212.5 & stayeddata$average_monthly_hours[i]<=275) {
    stayeddata$employee_type[i] <- "Ideal and Ambitious"
  } else if (stayeddata$satisfaction_level >=0.35 & stayeddata$satisfaction_level[i] <=0.45 & stayeddata$evaluation_score[i]< 0.5 & stayeddata$average_monthly_hours[i]<=170) {
    stayeddata$employee_type[i] <-"Ideal and Sad/Stressed"
  } else if (stayeddata$satisfaction_level >=0.2 & stayeddata$evaluation_score[i]>=0.75 & stayeddata$average_monthly_hours[i]>= 237.5) {
    stayeddata$employee_type[i] <-"poor employee"
  } else
    stayeddata$employee_type[i] <-"Other"
}

#Now we can see that we've created a new column
ncol(stayeddata)
names(stayeddata)

#Checking to see if employee_type has been populated
str(stayeddata)

#This can be used by employers identify which one of their most valuables employees may leave next.

#With this the HR team could potentiallty identify values employees who may leave the company soon.
#Based from the conclusions of our previous analysis, we could hopeufully retain them longer by 
#1. offering them promotions
#2  Increasing their salary

#subset of  "Ideal and Sad/Stressed employees" from employees who have stayed
Ideal_SadStressed = stayeddata[stayeddata$employee_type ==  "Ideal and Sad/Stressed",]
#number of "Ideal and Sad/Stressed employees"
number_of_Ideal_SadStressed = nrow(Ideal_SadStressed)
number_of_Ideal_SadStressed
#subset of "Ideal and Ambitious" employees from employees who have stayed
Ideal_ambitious = stayeddata[stayeddata$employee_type ==  "Ideal and Ambitious",]
#number of "Ideal and Ambitious" employees
number_of_Ideal_ambitious = nrow(Ideal_ambitious)
number_of_Ideal_ambitious

#percentage of f  "Ideal and Sad/Stressed employees" from employees who have stayed
Percent_Ideal_SadStressed = (number_of_Ideal_SadStressed/nrow(stayeddata))*100
Percent_Ideal_SadStressed
#percentage oof "Ideal and Ambitious" employees from employees who have stayed
Percent_Ideal_ambitious = (number_of_Ideal_ambitious/nrow(stayeddata))*100
Percent_Ideal_ambitious

#table showing the count of valuable employees who may leave according to their employee_type
table(number_of_Ideal_ambitious, number_of_Ideal_SadStressed)

#table showing the percentage of valuable employees who may leave according to their employee_type
PotentialGoodEmployeesLeaving <- table(Percent_Ideal_SadStressed, Percent_Ideal_ambitious)

#Visualise data for current valuable employees that may leave company (Final figure for report)
stayedgoodemployees_dataframe <- data.frame(
  x = factor(c('Sad/Stressed Employees (1.4%)','Ambitious Employees (7.6%)')),
  y = c(Percent_Ideal_SadStressed, Percent_Ideal_ambitious )
)
stayedgoodemployees_dataframe

g <- ggplot(stayedgoodemployees_dataframe, aes(x=x, y=y))
g <- g + geom_bar(stat='identity', aes(fill=x))
g <- g + labs(title="Current % of Valuable Employees That May Leave Company", x='', y='')
g
