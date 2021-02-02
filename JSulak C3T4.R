#install arules and arulesviz
install.packages("arules")
install.packages("arulesViz")
#-----------------------------------

## MARKET BASKET ANALYSIS ##

#LOAD packages needed
library(arules)
library(arulesViz)

#about the packages
??arules
??arulesViz

### LOAD DATA using read.transactions()
transData <-read.transactions("C:\\Users\\jills\\Downloads\\ElectronidexTransactions.csv",cols=NULL, format="basket", sep=",", rm.duplicates=FALSE)


### EXPLORE DATA ####

#overall distribution
summary(transData)
#min median mean max
summary(size(transData)) 

#view the transactions
inspect(transData) 
#total number of transactions
length(transData)
#number of items per transaction
size(transData) 

#list items per transaction (LIST must be capitalized)
LIST(transData) 
#list item names for a range of transactions
LIST(transData[1:5])
#list item numbers for a range of transactions
LIST(transData[1:5], decode=FALSE)

#view the item labels
itemLabels(transData) 

#number of each item sold in data set
itemFrequency(transData, type="absolute")

#sort the list from itemFrequency
itms <- itemFrequency(transData, type="absolute")
sort(itms, decreasing = TRUE)

#print only top 20 items
head(sort(itms, decreasing = TRUE),n=20)


## VISUALIZATION ###

#plot whole data set (sparse matrix) with items counts on the y - will be unreadable, set too large
itemFrequencyPlot(transData, type="absolute")

#plot top 20 items by frequency
itemFrequencyPlot(transData, type="absolute", topN=20)
    
#- image()
image(transData[1:200, 1:20])
image(transData[1:200, 1:10])
#- image() with sample
image(sample(transData,300))


save.image()

### APRIORI ALGO - RULES ####
# - Remember SUPPORT vs CONFIDENCE - #
#basic function code-
Rules1<- apriori (transData, parameter = list(supp = 0.1, conf = 0.8))
#view the rules 
inspect(Rules1)
  #no output - nothing meets requirements given, adjust supp/conf parameters

#lower conf?
Rules2<- apriori (transData, parameter = list(supp = 0.1, conf = 0.6))
inspect(Rules2)

#lower supp?
Rules3<- apriori (transData, parameter = list(supp = 0.05, conf = 0.8))
inspect(Rules3)

#lower supp and conf?
Rules4<- apriori (transData, parameter = list(supp = 0.04, conf = 0.4))
inspect(Rules4)
  # got something, 3 Rules
summary(Rules4)

#HIGHEST SUPPORT, poor confidence.
Rules6<- apriori (transData, parameter = list(supp = 0.07, conf = 0.001))
inspect(Rules6)
#20 total and still first 18 are all single product purchases... cannot get support fairly high for multiple products


Rules8<- apriori (transData, parameter = list(supp = 0.01, conf = 0.2))
inspect(Rules8)
# ok 288 rules
summary(Rules8)

Rules9<- apriori (transData, parameter = list(supp = 0.01, conf = 0.45))
inspect(Rules9)
summary(Rules9)
#37 rules and erbody loves the imac 


#HIGHEST CONFIDENCE, poor support
Rules10<- apriori (transData, parameter = list(supp = 0.003, conf = 0.9))
inspect(Rules10)
#lhs                                                       rhs         support     confidence coverage    lift    count
#[1] {Acer Aspire,Dell Desktop,iMac,ViewSonic Monitor} => {HP Laptop} 0.003762074 0.902439   0.004168785 4.649286  37
 


Rules11<- apriori (transData, parameter = list(supp = 0.015, conf = 0.5))
inspect(Rules11)
#5 rules

Rules12<- apriori (transData, parameter = list(supp = 0.04, conf = 0.15))
inspect(Rules12)
#25 Rules 

#THIS IS BEST BALANCE WITH DECENT NUMBER OF RULES?
Rules13<- apriori (transData, parameter = list(supp = 0.04, conf = 0.25))
inspect(Rules13)
#10 rules


### PLOT ### should even plot these? well this is how... 

plot(Rules13)




save.image()


#let's try experimenting with minimum length again...

# APRIORI RULES with  MIN LEN #

RulesML1<- apriori (transData, parameter = list(supp = 0.1, conf = 0.8, target="rules", minlen=5))
inspect(RulesML1)
#none

RulesML2<- apriori (transData, parameter = list(supp = 0.05, conf = 0.5, target="rules", minlen=3))
inspect(RulesML2)
#none

RulesML3<- apriori (transData, parameter = list(supp = 0.02, conf = 0.45, target="rules", minlen=3))
inspect(RulesML3)
#none

RulesML4<- apriori (transData, parameter = list(supp = 0.02, conf = 0.45, target="rules", minlen=2))
inspect(RulesML4)
#something, but poor and quality gets weaker and weaker... all imac, all the time
summary(RulesML4)


#maybe inspect rules 9 and 12 and 13 with 37 and 25 and 10 returned...

Rules9<- apriori (transData, parameter = list(supp = 0.01, conf = 0.45))
inspect(Rules9)
inspect(sort(Rules9, decreasing = TRUE, by = "confidence"))
inspect(sort(Rules9, decreasing = TRUE, by = "support"))
inspect(sort(Rules9, decreasing = TRUE, by = "lift"))
summary(Rules9)

Rules12<- apriori (transData, parameter = list(supp = 0.04, conf = 0.15))
inspect(Rules12)
inspect(sort(Rules12, decreasing = TRUE, by = "confidence"))
inspect(sort(Rules12, decreasing = TRUE, by = "support"))
inspect(sort(Rules12, decreasing = TRUE, by = "lift"))
summary(Rules12)

Rules13<- apriori (transData, parameter = list(supp = 0.04, conf = 0.25))
inspect(Rules13)
inspect(sort(Rules13, decreasing = TRUE, by = "confidence"))
inspect(sort(Rules13, decreasing = TRUE, by = "support"))
inspect(sort(Rules13, decreasing = TRUE, by = "lift"))
summary(Rules13)


#SORT BY SUBSET - lets try Rules13 with 10
  

ItemRules13imac <- subset(Rules13, items %in% "iMac")
inspect(ItemRules13imac)
  #lhs                           rhs           support    confidence coverage  lift     count
#[1] {}                         => {iMac}      0.25612608 0.2561261  1.0000000 1.000000 2519 
#[2] {ViewSonic Monitor}        => {iMac}      0.04941535 0.4479263  0.1103203 1.748851  486 
#[3] {Dell Desktop}             => {iMac}      0.05460092 0.4074355  0.1340112 1.590762  537 
#[4] {CYBERPOWER Gamer Desktop} => {iMac}      0.05673615 0.3084577  0.1839349 1.204320  558 
#[5] {Lenovo Desktop Computer}  => {iMac}      0.05876970 0.3969780  0.1480427 1.549932  578 
#[6] {HP Laptop}                => {iMac}      0.07554652 0.3892090  0.1941027 1.519599  743 
#[7] {iMac}                     => {HP Laptop} 0.07554652 0.2949583  0.2561261 1.519599  743 


ItemRules13HP <- subset(Rules13, items %in% "HP Laptop")
inspect(ItemRules13HP)
  #  lhs                          rhs         support    confidence coverage  lift     count
#[1] {ViewSonic Monitor}       => {HP Laptop} 0.04799187 0.4350230  0.1103203 2.241200 472  
#[2] {Dell Desktop}            => {HP Laptop} 0.04494154 0.3353566  0.1340112 1.727728 442  
#[3] {Lenovo Desktop Computer} => {HP Laptop} 0.04616167 0.3118132  0.1480427 1.606434 454  
#[4] {HP Laptop}               => {iMac}      0.07554652 0.3892090  0.1941027 1.519599 743  
#[5] {iMac}                    => {HP Laptop} 0.07554652 0.2949583  0.2561261 1.519599 743  

ItemRules13Dell <- subset(Rules13, items %in% "Dell Desktop")
inspect(ItemRules13Dell)
  # lhs               rhs          support    confidence coverage  lift     count
#[1] {Dell Desktop} => {HP Laptop} 0.04494154 0.3353566  0.1340112 1.727728 442  
#[2] {Dell Desktop} => {iMac}      0.05460092 0.4074355  0.1340112 1.590762 537


ItemRules13CYPOWER <- subset(Rules13, items %in% "CYBERPOWER Gamer Desktop")
inspect(ItemRules13CYPOWER)
   # lhs                           rhs    support    confidence coverage  lift     count
#[1] {CYBERPOWER Gamer Desktop} => {iMac} 0.05673615 0.3084577  0.1839349 1.20432 558 


ItemRules13VSMON <- subset(Rules13, items %in% "ViewSonic Monitor")
inspect(ItemRules13VSMON)
   # lhs                    rhs         support    confidence coverage  lift     count
#[1] {ViewSonic Monitor} => {HP Laptop} 0.04799187 0.4350230  0.1103203 2.241200 472  
#[2] {ViewSonic Monitor} => {iMac}      0.04941535 0.4479263  0.1103203 1.748851 486


ItemRules13LENO <- subset(Rules13, items %in% "Lenovo Desktop Computer")
inspect(ItemRules13LENO)
   # lhs                          rhs          support    confidence coverage  lift     count
#[1] {Lenovo Desktop Computer} => {HP Laptop} 0.04616167 0.3118132  0.1480427 1.606434 454  
#[2] {Lenovo Desktop Computer} => {iMac}      0.05876970 0.3969780  0.1480427 1.549932 578  



#REDUNDANT RULES how to find/remove - should this be done before doing subsets maybe? in POA this way in this order
 #### is.redundant(RulesName)
 ## TRUE means there are redundant rules, and FALSE means there are not.

is.redundant(Rules13)
#no redundant rules - hmmm what about hplap -> imac and vice versa, only conf slightly changes between two?
#thats not considered redundant?


#but try  RULES12 SO KNOW HOW TO REMOVE REDUNDANTS...
is.redundant(Rules12) 
#2 redundant, gotta fix... was #25 rules
rules12clean <- Rules12[!is.redundant(Rules12)]
inspect(rules12clean) #now 23 rules


 
##VISUALIZE##
  #  plot(RulesName[1:# of rules you want to plot], method="graph", control=list(type="items")) #

plot(Rules13[1:10], method="graph", control=list(type="items")) 


plot(rules12clean[1:23], method="graph", control=list(type="items"))



save.image()
