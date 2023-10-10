# Author: Zhou Fan
# Date: 08/31/2023

dataVote <- read.csv("/Users/cindyfan/Desktop/SDS 313/Homework1_Bonds.csv")


# Question 1 
# 1.1 data table that shows numbers of bonds approved/defeated
table(dataVote$Result)

# 1.2 descriptive stats (table + standard deviation)
prop.table(table(dataVote$Result, dataVote$Gov_Type),2)
rateApproved <- c(0.8181818182, 0.8650234742, 0.8266129032, 0.7647058824, 
                  0.7187573547, 1.0000000000, 0.9403202329)
sd(rateApproved)

# 1.3 compare different approved rates across government types (grouped barplot)
barplot(prop.table(table(dataVote$Result, dataVote$Gov_Type),2),
        main = 'Proportion of different voting redults for each government type (purple-carried)',
        xlab ='Government Types', ylab = 'Proportion', 
        col=c('blue','purple','green'))


# Question 2
# 2.1 create the new variable 'Votes_Total' 
Votes_Total <- dataVote$Votes_For +dataVote$Votes_Against

# 2.2 display the entry with the highest voter turnout 
highest <- dataVote[Votes_Total==max(Votes_Total),]

# 2.3 display when & where & why 
highest
highest$Election_Date   # when
highest$County          # where
highest$Purpose         # purpose type
highest$Purpose_Detail  # purpose detail


# Question 3 
# 3.1 Create the new subset   
datasub <- dataVote[(dataVote$Result=='Carried')&(Votes_Total>=10),] 

# 3.2 Create a new variable of percentage for the subset (for/total)
datasub$percentage <- datasub$Votes_For/(datasub$Votes_For+datasub$Votes_Against)

# 3.3 distribution displayed by histogram 
hist(datasub$percentage, main = 'distribution of percentage for the bond measure of the bonds subset', 
     xlab= 'percentage of total votes that are for the bond measure', col = 'aquamarine')

# 3.4 descriptive stats
mean(datasub$percentage)
sd(datasub$percentage)


# Question 4 
# 4.1 create a new variable to denote margin of approved bonds
datasub$margin <- datasub$Votes_For-datasub$Votes_Against

# 4.2 display the relationship between the margin of approved bonds and its cost
plot(datasub$margin,datasub$Amount,  main = 'relationship between margin of approved bonds and their cost',
     xlab = 'margin', ylab = 'cost in dollars', pch=20)

# 4.3 calculate the correlation coefficient
cor(datasub$margin,datasub$Amount)

