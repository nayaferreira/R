"
****************************************************************
Author: Nayane Fereira da Silva
Case Study 2
****************************************************************
"
# dataset = nfs

"
****************************************************************
PART 1 - Does watching the news affect how you spend your day?
****************************************************************
"
######Question 1############
#Contingency table (News vs Anxiety)

rpivotTable(nfs)

######Question 2############

## A ##
#What is the probability that I did not watch the news on a randomly selected day?
#Rule of Complement (NOT)
rpivotTable(nfs)
#Count as fraction of Total
#News answer as NO = 47.5%

## B ##
#What is the probability that on a randomly selected day, I experience anxiety level 3 or watch the news?
#Addition rule for probability (OR) - non-disjoint
rpivotTable(nfs)
#Count as fraction of Total
52.5 + 18.8 - 8.8  #62.5%


## C ##
#Given that I watch the News on a particular day, what is the probability that my anxiety level is 5?
#Conditional probability (GIVEN)
rpivotTable(nfs)
#Count as fraction of Total
#News go to the rows and check only box for anxiety =5
#50%

## D ##
#What is the probability that on a randomly selected day, I did not watch the News and experienced anxiety level 1?
#Multiplication rule (AND)
rpivotTable(nfs)
#Count as fraction of Total
#Check intersection between NO for News and Anxiety 1 = 12.5%


######Question 4############
#Independence
rpivotTable(nfs)
#Count as fraction of Total
#Check intersection between YES for News and Anxiety 4 = 8.8%
#Check P(News=Yes) * P(Anxiety=4) 
42/80 * 12/80
#Answer: P(News=Yes AND Anxiety=4) <> P(News=Yes) * P(Anxiety=4) 

"
****************************************************************
PART 2 - Do you watch the news more than other people?
****************************************************************
"
######Question 1############

p<-0.59 #Population proportion
n<-nrow(nfs) 
prop.table(table(nfs$News)) #Yes = sample proportion #OR
pnews<-sum(nfs$News == 'YES')/n #sample proportion

######Question 2############

pnews<-0.525 #mean
n<-nrow(nfs) #80
(sd<-sqrt(pnews*(1-pnews)/n)) #0.05583178


######Question 3############

pnorm(pnews,p,sd,lower.tail = TRUE) #0.1221692

######Question 4############

CI<-function(p.hat, n, cl){
  #p.hat = sample proportion, enter as decimal
  #n = sample size
  #cl = confidence level, enter as a decimal
  
  q.hat<-1-p.hat
  SE<-sqrt(p.hat*q.hat/n)
  z<-qnorm(cl/2+0.5, 0, 1, lower.tail=TRUE)
  lower<-p.hat-z*SE
  upper<-p.hat+z*SE
  return(c(lower, upper))
  
}

CI(pnews,n,0.95) #0.4155717,0.6344283
#for 50% proportion
CI(0.5,n,0.95) #0.3904347,0.6095653

######Question 5############
#consider alfa = 0.05
#H0: p = 0.59
#HA: p < 0.59
n<-nrow(nfs) #80
p<-0.59
phat<-sum(nfs$News == 'YES')/n #0.525
(SD<-sqrt(p*(1-p)/n)) #0.05498864
(z<-(phat-p)/SD) #-1.182062
(p.value<-pnorm(z, 0, 1, lower.tail=TRUE)) #0.1185905   


