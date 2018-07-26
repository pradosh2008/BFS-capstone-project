library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)

#load data
demographic<-read.csv("Demographic data.csv",stringsAsFactors = F)
credit_beareau<-read.csv("Credit Bureau data.csv",stringsAsFactors = F)


#structure
str(demographic)
str(credit_beareau)


nrow(demographic)
#[1] 71295
nrow(credit_beareau)
#[1] 71295


length(unique(tolower(demographic$Application.ID)))
# [1] 71292
length(unique(tolower(credit_beareau$Application.ID)))
# [1] 71292

sum(duplicated(demographic$Application.ID))
# [1] 3
sum(duplicated(credit_beareau$Application.ID))
# [1] 3

##so we have 3 rows in which application id is duplicated.
demographic[duplicated(demographic$Application.ID),]
credit_beareau[duplicated(credit_beareau$Application.ID),]

#remove duplicate from demographic

demographic[which(duplicated(demographic$Application.ID) == T), ]
demographic[which(demographic$Application.ID %in% c("765011468", "653287861","671989187")),]

demographic <- demographic[-which(duplicated(demographic$Application.ID) == T), ]
sum(duplicated(demographic$Application.ID))

#remove duplicate from credit_beareau

credit_beareau[which(duplicated(credit_beareau$Application.ID) == T), ]
credit_beareau[which(credit_beareau$Application.ID %in% c("765011468", "653287861","671989187")),]

credit_beareau <- credit_beareau[-which(duplicated(credit_beareau$Application.ID) == T), ]
sum(duplicated(credit_beareau$Application.ID))
  

setdiff(demographic$Application.ID,credit_beareau$Application.ID)
# integer(0)
# We can witness application id are same in both the datasets.



sum(is.na(demographic))
# [1] 1428
sum(duplicated(demographic))
# [1] 0

sum(is.na(credit_beareau))
# [1] 3028
sum(duplicated(credit_beareau))
# [1] 0

sapply(demographic, function(x) sum(is.na(x)))
##all NA in dataset demographic pertain to performance tag and other columns.

sapply(credit_beareau, function(x) sum(is.na(x)))
##so we find here that performance tag has maximum number of NA and other column is Avgas.CC.Utilization.in.last.12.months.


##so we remove performance tag which is NA
demographic<-demographic[!is.na(demographic$Performance.Tag), ]

##removing from credit bureau as well
credit_beareau<-credit_beareau[!is.na(credit_beareau$Performance.Tag), ]


setdiff(demographic$Application.ID,credit_beareau$Application.ID)  #app id is same.
sapply(demographic, function(x) sum(is.na(x)))
sapply(credit_beareau, function(x) sum(is.na(x)))



#######Analysis on individual columns on demographics dataset##########
summary(as.factor(demographic$Age))
##we need to remove -3 and 0 ages

summary(as.factor(demographic$No.of.dependents))
summary(as.factor(demographic$Income))  #we have negative income

##open discussion ..we should remove negative income or we should set it as 0 itself?.

summary(as.factor(demographic$Profession))
summary(as.factor(demographic$Type.of.residence))
summary(as.factor(demographic$No.of.months.in.current.residence))
summary(as.factor(demographic$No.of.months.in.current.company))
summary(as.factor(demographic$Performance.Tag))

#   0     1 
# 66922  2948


##Analysis on individual columns on credit bureau dataset
summary(as.factor(credit_beareau$No.of.times.90.DPD.or.worse.in.last.6.months))
summary(as.factor(credit_beareau$Outstanding.Balance))  #it has 272 NA.
summary(as.factor(credit_beareau$Presence.of.open.auto.loan))
summary(as.factor(credit_beareau$Total.No.of.Trades))
summary(as.factor(credit_beareau$No.of.times.60.DPD.or.worse.in.last.6.months))
summary(as.factor(credit_beareau$Presence.of.open.home.loan))   #272 NA
summary(as.factor(credit_beareau$Avgas.CC.Utilization.in.last.12.months))   #NA presence 1023
summary(as.factor(credit_beareau$Presence.of.open.auto.loan))
summary(as.factor(credit_beareau$No.of.times.60.DPD.or.worse.in.last.6.months))
summary(as.factor(credit_beareau$No.of.times.60.DPD.or.worse.in.last.6.months))
summary(as.factor(credit_beareau$No.of.times.60.DPD.or.worse.in.last.6.months))



########Creating master file now.##########
master_file<- merge(x = demographic, y = credit_beareau, by = 'Application.ID')


#we need to replace all rows of average cc utilization with 0
master_file$Avgas.CC.Utilization.in.last.12.months[which(is.na(master_file$Avgas.CC.Utilization.in.last.12.months)==1)] = 0
sum(is.na(master_file$Avgas.CC.Utilization.in.last.12.months))



## 1 NA value detected in "No.of.trades.opened.in.last.6.months" column
master_file[is.na(master_file$No.of.trades.opened.in.last.6.months),]
##we can remove one row which is not much significance
median(!is.na(master_file$No.of.trades.opened.in.last.6.months))
#[1] 1
master_file<-master_file[!is.na(master_file$No.of.trades.opened.in.last.6.months), ]


sapply(master_file, function(x) sum(is.na(x)))
##we have 3 columns in which NA is found now
##analysing no of dependents
master_file[is.na(master_file$No.of.dependents),]
#replacing them with 0 as they are no dependents
master_file$No.of.dependents[which(is.na(master_file$No.of.dependents))] = 0


##LOOKING at column Presence.of.open.home.loan
master_file[is.na(master_file$Presence.of.open.home.loan),]
##looking at the values we find that in all fields where presence of open home loan is null we have outstanding balance as null as well.
summary(master_file$Presence.of.open.home.loan)
 #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 # 0.0000  0.0000  0.0000  0.2597  1.0000  1.0000     272 
summary(master_file$Outstanding.Balance)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   #    0  208411  774243 1253428 2926250 5218801     272
#we can put it by median values
master_file[is.na(master_file$Presence.of.open.home.loan),]$Presence.of.open.home.loan=0
master_file[is.na(master_file$Outstanding.Balance),]$Outstanding.Balance=774243


sapply(master_file, function(x) sum(is.na(x)))
#so now we are all fine with NA values



###looking at age column
summary(master_file$Age)
#we can replace all -3 and 0 ages with median values
which(master_file$Age==-3)
master_file$Age[which(master_file$Age==-3)]=45
master_file$Age[which(master_file$Age== 0)]=45



# checking for outlier--
# 
# https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/

outlierKD <- function(dt, var) {
     var_name <- eval(substitute(var),eval(dt))
     na1 <- sum(is.na(var_name))
     m1 <- mean(var_name, na.rm = T)
     par(mfrow=c(2, 2), oma=c(0,0,3,0))
     boxplot(var_name, main="With outliers")
     hist(var_name, main="With outliers", xlab=NA, ylab=NA)
     outlier <- boxplot.stats(var_name)$out
     mo <- mean(outlier)
     var_name <- ifelse(var_name %in% outlier, NA, var_name)
     boxplot(var_name, main="Without outliers")
     hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
     title("Outlier Check", outer=TRUE)
     na2 <- sum(is.na(var_name))
     cat("Outliers identified:", na2 - na1, "n")
     cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
     cat("Mean of the outliers:", round(mo, 2), "n")
     m2 <- mean(var_name, na.rm = T)
     cat("Mean without removing outliers:", round(m1, 2), "n")
     cat("Mean if we remove outliers:", round(m2, 2), "n")
     response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
     if(response == "y" | response == "yes"){
          dt[as.character(substitute(var))] <- invisible(var_name)
          assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
          cat("Outliers successfully removed", "n")
          return(invisible(dt))
     } else{
          cat("Nothing changed", "n")
          return(invisible(var_name))
     }
}

# 
# source("http://goo.gl/UUyEzD")
outlierKD(master_file, Income)

##we wont make any changes but we can analyse it.
##looking at the outlier Income diagram boxplot we can see that some people have zero and negative incomes
##analysing more 

zero_income<-subset(master_file,Income<=0)

#we can subsitute all zero and negative income with median values
master_file$Income[which(master_file$Income<= 0)]=27




##checking again for outliers we are fine
outlierKD(master_file, Income)
# Outliers identified: 0 from 69863 observations
# Proportion (%) of outliers: 0
# Mean of the outliers: NaN
# Mean without removing outliers: 27.4515122453946
# Mean if we remove outliers: 27.4515122453946
# Do you want to remove outliers and to replace with NA? [yes/no]: 
# Nothing changed


##checking for outstanding balance
outlierKD(master_file, Outstanding.Balance)

# Outliers identified: 0 from 69863 observations
# Proportion (%) of outliers: 0
# Mean of the outliers: NaN
# Mean without removing outliers: 1251562.06366746
# Mean if we remove outliers: 1251562.06366746
# Do you want to remove outliers and to replace with NA? [yes/no]: no
# Nothing changed



outlierKD(master_file, Avgas.CC.Utilization.in.last.12.months)
# Outliers identified: 3702 from 69863 observations
# Proportion (%) of outliers: 5.29894221547887
# Mean of the outliers: 111.813074014046
# Mean without removing outliers: 28.8354780069565
# Mean if we remove outliers: 24.1925152280044
# Do you want to remove outliers and to replace with NA? [yes/no]: no
# Nothing change


outlierKD(master_file, Age)
# outlierKD(master_file, Age)
# Outliers identified: 12 from 69863 observations
# Proportion (%) of outliers: 0.0171764739561714
# Mean of the outliers: 15
# Mean without removing outliers: 45.0088601978157
# Mean if we remove outliers: 45.0140155473794
# Do you want to remove outliers and to replace with NA? [yes/no]: no
# Nothing changed


outlierKD(master_file, No.of.months.in.current.residence)
# Outliers identified: 0 from 69863 observations
# Proportion (%) of outliers: 0
# Mean of the outliers: NaN
# Mean without removing outliers: 34.6064440404792
# Mean if we remove outliers: 34.6064440404792
# Do you want to remove outliers and to replace with NA? [yes/no]: no
# Nothing changed


outlierKD(master_file, No.of.PL.trades.opened.in.last.12.months)
# Outliers identified: 76 from 69863 observations
# Proportion (%) of outliers: 0.108784335055752
# Mean of the outliers: 11.1315789473684
# Mean without removing outliers: 2.36352575755407
# Mean if we remove outliers: 2.35397710175248
# Do you want to remove outliers and to replace with NA? [yes/no]: no
# Nothing changed


##open agenda do we remove it?.There is not much reduction in mean in No.of.PL.trades.opened.in.last.12.months.few are there

outlierKD(master_file, No.of.PL.trades.opened.in.last.6.months)
# Outliers identified: 296 from 69863 observations
# Proportion (%) of outliers: 0.42368635758556
# Mean of the outliers: 6
# Mean without removing outliers: 1.18964258620443
# Mean if we remove outliers: 1.1691750398896
# Do you want to remove outliers and to replace with NA? [yes/no]: no
# Nothing changed



##open agenda do we remove it?.There is not much reduction in mean in No.of.PL.trades.opened.in.last.6.months.However we can see the outliers.few are there



outlierKD(master_file, No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
# Outliers identified: 2059 from 69863 observations
# Proportion (%) of outliers: 2.94719665631307
# Mean of the outliers: 14.2370082564352
# Mean without removing outliers: 3.52511343629675
# Mean if we remove outliers: 3.19982596896938
# Do you want to remove outliers and to replace with NA? [yes/no]: no
# Nothing changed

##open agenda do we remove it?.There is not much reduction in mean in No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.Looking at box plot we can see the outliers.

outlierKD(master_file, No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
# Outliers identified: 1368 from 69863 observations
# Proportion (%) of outliers: 1.95811803100354
# Mean of the outliers: 8.46856725146199
# Mean without removing outliers: 1.75802642314244
# Mean if we remove outliers: 1.6240017519527
# Do you want to remove outliers and to replace with NA? [yes/no]: no
# Nothing changed

##open agenda do we remove it?.There is not much reduction in mean .Looking at box plot we can see the outliers.


outlierKD(master_file, Total.No.of.Trades)
# Outliers identified: 6818 from 69863 observations
# Proportion (%) of outliers: 9.7590999527647
# Mean of the outliers: 26.0225872689938
# Mean without removing outliers: 8.17507121079828
# Mean if we remove outliers: 6.24495201839956
# Do you want to remove outliers and to replace with NA? [yes/no]: no
# Nothing changed


#we can see the outliers and there is big reduction in mean after removing the outliers.Also the boxplot shows that outliers are significant.so we can remove them.

#Have not grouped the variable into bins , we have to decide on if binning is resulting into any information loss or not.


#woe,IV analysis
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(lattice)
library(ggplot2)

library(devtools)
library(httr)
install_github("riv","tomasgreif")
library(woe)        
library(Information)
View(demographic)
View(credit_beareau)

rownames(master_file)=NULL

iv.mult(master_file,"Performance.Tag.y",TRUE)
iv.plot.summary(iv.mult(master_file,"Performance.Tag.y",TRUE))
iv.mult(master_file,"Performance.Tag.y",TRUE)
iv.plot.summary(iv.mult(master_file,"Performance.Tag.y",TRUE))
options(digits=2)

iv.mult(master_file,"Performance.Tag.y")
iv.plot.woe(iv.mult(master_file,"Performance.Tag.y",summary=FALSE))
iv.mult(master_file,"Performance.Tag.y")
iv.plot.woe(iv.mult(master_file,"Performance.Tag.y",summary=FALSE))


###we would be replacing the original columns which are strong predictor with there WOE values
#we need to convert . to _ as it gives syntax error.

names(master_file)[names(master_file) == 'No.of.trades.opened.in.last.12.months'] <- "No_of_trades_opened_in_last_12_months"
names(master_file)[names(master_file) == 'Avgas.CC.Utilization.in.last.12.months'] <- "Avgas_CC_Utilization_in_last_12_months"
names(master_file)[names(master_file) == 'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.'] <- "No_of_Inquiries_in_last_12_months_excluding_home_auto_loans"
names(master_file)[names(master_file) == 'No.of.PL.trades.opened.in.last.12.months'] <- "No_of_PL_trades_opened_in_last_12_months"
names(master_file)[names(master_file) == 'Outstanding.Balance'] <- "Outstanding_Balance"
names(master_file)[names(master_file) == 'No.of.times.30.DPD.or.worse.in.last.6.months'] <- "No_of_times_30_DPD_or_worse_in_last_6_months"
names(master_file)[names(master_file) == 'Total.No.of.Trades'] <- "Total_No_of_Trades"
names(master_file)[names(master_file) == 'No.of.PL.trades.opened.in.last.6.months'] <- "No_of_PL_trades_opened_in_last_6_months"
names(master_file)[names(master_file) == 'No.of.times.90.DPD.or.worse.in.last.12.months'] <- "No_of_times_90_DPD_or_worse_in_last_12_months"
names(master_file)[names(master_file) == 'No.of.times.60.DPD.or.worse.in.last.6.months'] <- "No_of_times_60_DPD_or_worse_in_last_6_months"
names(master_file)[names(master_file) == 'No_of_Inquiries_in_last_6_months_excluding_home_auto_loans'] <- "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans."



outiv <- iv.mult(master_file,"Performance.Tag.y",vars=c("No_of_trades_opened_in_last_12_months","Avgas_CC_Utilization_in_last_12_months","No_of_Inquiries_in_last_12_months_excluding_home_auto_loans","No_of_PL_trades_opened_in_last_12_months",
"Outstanding_Balance","No_of_times_30_DPD_or_worse_in_last_6_months","Total_No_of_Trades","No_of_PL_trades_opened_in_last_6_months","No_of_times_90_DPD_or_worse_in_last_12_months",
"No_of_times_60_DPD_or_worse_in_last_6_months"))

x <- iv.replace.woe(master_file,outiv)

##Now we remove the orginal variable ###
##WOE value analysis , been kept in a separate dataframe.Original values are being replaced
x<-x[-c(12)]
x<-x[-c(13,14,15,18,20,21,22,24,26,27)]



##removing application id and duplicate performance tag
master_file=master_file[,-12]
master_file=master_file[,-1]

##for using random forest we make all factors

master_file$Gender<-as.factor(master_file$Gender)
master_file$Marital.Status..at.the.time.of.application.<-as.factor(master_file$Marital.Status..at.the.time.of.application.)
master_file$Education<-as.factor(master_file$Education)
master_file$Profession<-as.factor(master_file$Profession)
master_file$Type.of.residence<-as.factor(master_file$Type.of.residence)
master_file$Performance.Tag.y<-as.factor(master_file$Performance.Tag.y)
### converting these column into factor
master_file$No.of.dependents<-as.factor(master_file$No.of.dependents)
master_file$Presence.of.open.auto.loan<-as.factor(master_file$Presence.of.open.auto.loan)
master_file$Presence.of.open.home.loan<-as.factor(master_file$Presence.of.open.home.loan)


#Scaling have not applied for decesion tree
# performScaling <- TRUE  # Turn it on/off for experimentation.
# 
# if (performScaling) {
# 
#     # Loop over each column.
#     for (colName in names(master_file)) {
# 
#         # Check if the column contains numeric data.
#         if(class(master_file[,colName]) == 'integer' | class(master_file[,colName]) == 'numeric') {
# 
#             # Scale this column (scale() function applies z-scaling).
#             master_file[,colName] <- scale(master_file[,colName])
#         }
#     }
# }


###Building  model for random forest and decision tree 
library(randomForest)
library(ggplot2)
library(caTools)
library(caret)
library(rpart)

library(rpart)
library(rpart.plot)
library(caret)
#-----------------------------------------------------------------------------------------   

# Spliting the bank data in 70:30 ratio

set.seed(101)
master_file$Performance.Tag.y <- as.factor(ifelse(master_file$Performance.Tag.y==0,"no","yes"))

split_indices <- sample.split(master_file$Performance.Tag.y, SplitRatio = 0.70)
train_rf <- master_file[split_indices, ]
test_rf <- master_file[!split_indices, ]
nrow(train_rf)/nrow(master_file)
nrow(test_rf)/nrow(master_file)

#the severity of imbalance in this data set:
table(train_rf$Performance.Tag.y)
# no   yes 
# 46843  2063 

#check classes distribution
prop.table(table(train_rf$Performance.Tag.y))
# no        yes 
# 0.95781704 0.04218296 

#As we can see, this data set contains only 4.2% of positive cases and 95.7% of negative cases


#Let's build a model on this data. I'll be using decision tree algorithm for modeling purpose
#1build tree model- default hyperparameters
tree.model <- rpart(Performance.Tag.y ~ .,                     # formula
                    data = train_rf,                   # training data
                    method = "class")               # classification or regression

# display decision tree
prp(tree.model)
# make predictions on the test set
tree.predict <- predict(tree.model, test_rf, type = "class")
# evaluate the results
confusionMatrix(tree.predict, test_rf$Performance.Tag.y, positive = "yes")

# All measures are coming very bismal values

#2 Change the algorithm to "information gain" instead of default "gini" ----------------------
tree.model <- rpart(Performance.Tag.y ~ .,                     # formula
                    data = train_rf,                   # training data
                    method = "class",               # classification or regression
                    parms = list(split = "information")
)

# display decision tree
prp(tree.model)

# make predictions on the test set
tree.predict <- predict(tree.model, test_rf, type = "class")

# evaluate the results
confusionMatrix(tree.predict, test_rf$Performance.Tag.y, positive = "yes")

# All measures are coming very bismal values

#3 Tune the hyperparameters ----------------------------------------------------------
tree.model <- rpart(Performance.Tag.y ~ .,                                # formula
                    data = train_rf,                             # training data
                    method = "class",                         # classification or regression
                    control = rpart.control(minsplit = 1000,  # min observations for node
                                            minbucket = 1000, # min observations for leaf node
                                            cp = 0.05))       # complexity parameter

# display decision tree
prp(tree.model)

# make predictions on the test set
tree.predict <- predict(tree.model, test_rf, type = "class")

# evaluate the results
confusionMatrix(tree.predict, test_rf$Performance.Tag.y, positive = "yes")


#All measures are coming very bismal values

###so we see here that sensitivity is very less so we need to treat our imbalanced data.

##The reason can be 
#As the data for minortiy classes is too less
#The majority classes is dominating on decision making

# Below are the methods used to treat imbalanced datasets:

# Undersampling
# Oversampling
# Synthetic Data Generation

#Let's start with oversampling and balance the data.

library(ROSE)
data_balanced_over <- ovun.sample(Performance.Tag.y ~., data = train_rf, method = "over", N = 93686)$data
table(data_balanced_over$Performance.Tag.y)

# no   yes 
# 46843 46843

#we can perform undersampling as well. Remember, undersampling is done without replacement.
data_balanced_down <- ovun.sample(Performance.Tag.y ~., data = train_rf, method = "under", N = 4126)$data
table(data_balanced_down$Performance.Tag.y)

# no  yes 
# 2063 2063


# Now the data set is balanced. But, you see that we've lost significant information from the sample.
# Let's do both undersampling and oversampling on this imbalanced data. This can be achieved using
# method = "both". In this case, the minority class is oversampled with replacement and majority
# class is undersampled without replacement.


data_balanced_both <- ovun.sample(Performance.Tag.y ~., data = train_rf, method = "both",p=0.5,N=nrow(train_rf))$data
table(data_balanced_both$Performance.Tag.y)
# no   yes 
# 24657 24249 

# The data generated from oversampling have expected amount of repeated observations.
# Data generated from undersampling is deprived of important information from the original data.
# This leads to inaccuracies in the resulting performance. To encounter these issues,
# ROSE helps us to generate data synthetically as well. The data generated using ROSE 
# is considered to provide better estimate of original data.

balanced_data_synthetic <- ROSE(Performance.Tag.y ~ ., data = train_rf, seed = 1)$data
table(balanced_data_synthetic$Performance.Tag.y)
# no   yes 
# 24451 24455


#build decision tree models
tree.rose <- rpart(Performance.Tag.y ~ ., data = balanced_data_synthetic)
tree.over <- rpart(Performance.Tag.y ~ ., data = data_balanced_over)
tree.under <- rpart(Performance.Tag.y ~ ., data = data_balanced_down)
tree.both <- rpart(Performance.Tag.y ~ ., data = data_balanced_both)

#make prediction on unseen data
pred.tree.rose <- predict(tree.rose, newdata = test_rf)
pred.tree.over <- predict(tree.over, newdata = test_rf)
pred.tree.under <- predict(tree.under, newdata = test_rf)
pred.tree.both <- predict(tree.both, newdata = test_rf)

#check accuracy
accuracy.meas(test_rf$Performance.Tag.y, pred.tree.rose[,2])
accuracy.meas(test_rf$Performance.Tag.y, pred.tree.over[,2])
accuracy.meas(test_rf$Performance.Tag.y, pred.tree.under[,2])
accuracy.meas(test_rf$Performance.Tag.y, pred.tree.both[,2])

#build decision tree models
tree.model.rose <- rpart(Performance.Tag.y ~ .,                     
                         data = balanced_data_synthetic,                  
                         method = "class")              

tree.model.under <- rpart(Performance.Tag.y ~ .,                     
                         data = data_balanced_down,                  
                         method = "class")    

tree.model.over <- rpart(Performance.Tag.y ~ .,                     
                          data = data_balanced_over,                  
                          method = "class")  

tree.model.both <- rpart(Performance.Tag.y ~ .,                     
                    data = data_balanced_both,                  
                    method = "class")           

# display decision tree
prp(tree.model.rose)
prp(tree.model.under)
prp(tree.model.over)
prp(tree.model.both)


# make predictions on the test set
tree.predict_rose <- predict(tree.model.rose, test_rf, type = "class")
# evaluate the results
confusionMatrix(tree.predict_rose, test_rf$Performance.Tag.y, positive = "yes")

# Accuracy : 0.6917
# Sensitivity : 0.56787        
# Specificity : 0.69720

# make predictions on the test set
tree.predict_under <- predict(tree.model.under, test_rf, type = "class")
# evaluate the results
confusionMatrix(tree.predict_under, test_rf$Performance.Tag.y, positive = "yes")

# Accuracy : 0.5939 
# Sensitivity : 0.69796         
# Specificity : 0.58931

# make predictions on the test set
tree.predict_over <- predict(tree.model.over, test_rf, type = "class")
# evaluate the results
confusionMatrix(tree.predict_over, test_rf$Performance.Tag.y, positive = "yes")

# Accuracy : 0.5586  
# Sensitivity : 0.73416         
# Specificity : 0.55086

# make predictions on the test set
tree.predict_both <- predict(tree.model.both, test_rf, type = "class")
# evaluate the results
confusionMatrix(tree.predict_both, test_rf$Performance.Tag.y, positive = "yes")

# Accuracy : 0.5586 
# Sensitivity : 0.73416         
# Specificity : 0.55086

#########--------------------------------------------#########################

#Choose synthetic data as the dataset to build furthure model and tune hyper parameter

# make predictions on the test set as probability
tree.pred_rose <- predict(tree.model.rose, test_rf[,-28], type = "prob")

#Let's use the probability cutoff of 50%.
test_pred_churn <- factor(ifelse(tree.pred_rose[,2] >= 0.40, "yes", "no"))
table(test_rf$Performance.Tag.y,test_pred_churn)
 
# test_pred_churn
      # no   yes
# no  13997  6079
# yes   382   502


predicted_response <- as.factor(ifelse(tree.pred_rose[, 2] >= .4, "yes", "no"))
conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag.y, positive = "yes")
conf

# Accuracy : 0.6917 
# Sensitivity : 0.56787        
# Specificity : 0.69720  


#Cross test to choose CP ------------------------------------------------------------
library(caret)
# set the number of folds in cross test to 5
tree.control = trainControl(method = "cv", number = 5)

# set the search space for CP
tree.grid = expand.grid(cp = seq(0, 0.02, 0.0025))

# train model
tree.model <- train(Performance.Tag.y ~ .,
                    data = balanced_data_synthetic,
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = tree.control,
                    tuneGrid = tree.grid,
                    control = rpart.control(minsplit = 50,
                                            minbucket = 20))

# look at cross validated model results
tree.model

# look at best value of hyperparameter
tree.model$bestTune

# make predictions on test set
tree.predict <- predict.train(tree.model, test_rf)

# accuracy
confusionMatrix(tree.predict, test_rf$Performance.Tag.y)  


              # Reference
  #Prediction     no   yes
          # no  13996   382
          # yes  6080   502

#Accuracy : 0.6917  
# Sensitivity : 0.69715         
# Specificity : 0.56787 


#Build random Forest model with the synthetic data

rf_synthetic <- randomForest(Performance.Tag.y ~., data = balanced_data_synthetic, proximity = F, do.trace = T, mtry = 5,ntree=1000)
rf_pred_synthetic <- predict(rf_synthetic, test_rf[, -28], type = "prob")

#Build random Forest model with the under sampled  data

rf_down <- randomForest(Performance.Tag.y ~., data = data_balanced_down, proximity = F, do.trace = T, mtry = 5,ntree=1000)
rf_pred_down <- predict(rf_down, test_rf[, -28], type = "prob")

#########################################################################################
# Let's Choose the cutoff value. 


# Let's find out the optimal probalility cutoff for synthetic data
#Cutoff for randomforest to assign yes or no
perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_synthetic[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag.y, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 


# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]


#The plot shows that cutoff value of around 12% optimises sensitivity and accuracy

predicted_response_rf<- factor(ifelse(rf_pred_synthetic[, 2] >= 0.2, "yes", "no"))
conf_rf <- confusionMatrix(predicted_response_rf, test_rf$Performance.Tag.y, positive = "yes")
conf_rf


#for synthetic data sampling
Accuracy : 0.6651
Sensitivity : 0.59729         
Specificity : 0.66811 


####################################################################3
# Let's find out the optimal probalility cutoff for under sampling data data
#Cutoff for randomforest to assign yes or no
perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_down[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag.y, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 


# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]


#The plot shows that cutoff value of around 12% optimises sensitivity and accuracy

predicted_response_rf<- factor(ifelse(rf_pred_down[, 2] >= 0.5, "yes", "no"))
conf_rf <- confusionMatrix(predicted_response_rf, test_rf$Performance.Tag.y, positive = "yes")
conf_rf


# Sensitivity
conf_rf$byClass[1]
# Sensitivity 
# 0.6855204

# Specificity 
conf_rf$byClass[2]
# Specificity 
# 0.5657501

# Accuracy 
conf_rf$overall[1]
# Accuracy 
# 0.5708015 



##-----------------------------------------SVM----------------------------------------

event_col<-c("Performance.Tag.y")  

fact_cols <- c("Gender","Marital.Status..at.the.time.of.application." 
               ,"Education","Profession","Type.of.residence","Presence.of.open.auto.loan","Presence.of.open.home.loan",
               "No.of.dependents")

numeric_cols<-c('Age','Income','No.of.months.in.current.residence','No.of.months.in.current.company'
                ,'Total_No_of_Trades','Outstanding_Balance','Avgas_CC_Utilization_in_last_12_months'
                ,'No.of.times.90.DPD.or.worse.in.last.6.months','No_of_times_60_DPD_or_worse_in_last_6_months',
                'No_of_times_30_DPD_or_worse_in_last_6_months'
                ,'No_of_times_90_DPD_or_worse_in_last_12_months','No.of.times.60.DPD.or.worse.in.last.12.months',
                'No.of.times.30.DPD.or.worse.in.last.12.months'
                ,'No.of.trades.opened.in.last.6.months','No_of_trades_opened_in_last_12_months'
                ,'No_of_PL_trades_opened_in_last_6_months','No_of_PL_trades_opened_in_last_12_months'
                ,'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.'
                ,'No_of_Inquiries_in_last_12_months_excluding_home_auto_loans')



################Scaling numeric columns & creating dummies for factor attributes#############

data_for_scaling<-data.frame(sapply(master_file[numeric_cols], scale))
head(data_for_scaling)
str(data_for_scaling)

data_for_creating_dummies <- master_file[fact_cols] 
str(data_for_creating_dummies) 

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(data_for_creating_dummies, 
                            function(x) data.frame(model.matrix(~x-1,data =data_for_creating_dummies))[,-1])) 

# combine all relevant columns to build final training data
final_df<- cbind(master_file[event_col],data_for_scaling[numeric_cols],dummies)

#final_df$Performance.Tag<-as.factor(final_df$Performance.Tag)

str(final_df)



###############################################################
## Before going to apply ove/under/synthetic sampling only on training data.
## Hence we need to devide the main data to train and test data.
## and then apply sampling on the training data only. 
## Otherwise there is a risk of - having unreal synthetic data in the test dataset.
######splitting whole date to separate test data for model evaluation######


set.seed(101)
#master_file$Performance.Tag.y <- as.factor(ifelse(master_file$Performance.Tag.y==0,"no","yes"))

split_indices <- sample.split(master_file$Performance.Tag.y, SplitRatio = 0.70)
train_svm <- master_file[split_indices, ]
test <- master_file[!split_indices, ]
nrow(train_svm)/nrow(master_file)
nrow(test_svm)/nrow(master_file)





#the severity of imbalance in this data set:
table(train_svm$Performance.Tag.y)

#check classes distribution
prop.table(table(train_svm$Performance.Tag.y))

table(test_svm$Performance.Tag.y)


###########  SMOTE(synthetic minority oversampling technique) by ROSE package
#SMOTE algorithm creates artificial data based on feature space (rather than data space) 
# similarities from minority samples. It generates a random set of minority class observations 
# to shift the classifier learning bias towards minority class.


#Generate data synthetically to avoid errors related to explicitly mentioned probability

traindata <- ROSE(Performance.Tag.y ~ ., data = train_svm, seed = 1)$data
table(traindata$Performance.Tag.y)
str(traindata)

#As It would take a lot of time for modeling on the  whole train data, So we are taking 10% sample of the data and
#building the model which would make the computation faster.

train.indices = sample(2:nrow(traindata), 0.1*nrow(traindata))
train = train[train.indices, ]


######## Linear SVM 

library(plyr)
library(caret)
library(kernlab)
library(readr)
library(caret)
library(caTools)

nrow(train)
table(train$Performance.Tag.y)
str(train)
nrow(test)
table(test$Performance.Tag.y)
str(test)

# 4. Model Building

#--------------------------------------------------------------------
# 4.1 Linear model - SVM  at Cost(C) = 1
#####################################################################

model_1 <- ksvm(Performance.Tag.y ~ ., data = train,scale = TRUE,C=1)
linear_prediction<- predict(model_1, test)
confusionMatrix(linear_prediction, test$Performance.Tag.y)


# Accuracy : 0.7729  
# Sensitivity : 0.78850         
# Specificity : 0.41742


#--------------------------------------------------------------------
# 4.2 Linear model - SVM  at Cost(C) = 10
#####################################################################

# Model with C =10. ( a high value of c will not accommodate many misclassifications -  an overfit model)

model_1 <- ksvm(Performance.Tag.y ~ ., data = train,scale = TRUE,C=10)
linear_prediction<- predict(model_1, test)
confusionMatrix(linear_prediction, test$Performance.Tag.y)

# Accuracy : 0.899
# Sensitivity : 0.93246        
# Specificity : 0.13801

#Improve in Sensitivity and accuracy but drop in  Specificity when we change C=1 and C=10





###############################Using Linear Kernel###############################
Model_linear <- ksvm(Performance.Tag.y~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$Performance.Tag.y)

# Accuracy : 0.6109
# Sensitivity : 0.5658          
# Specificity : 0.7262 


############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 
#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric <- "Accuracy"


####   Hyperparameter tuning and Cross Validation  using Linear SVM for C = 1 to 5 ####
#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
# making a grid of C values. 
grid <- expand.grid(C=seq(1, 5, by=1))
# Performing 5-fold cross validation
fit.svmLinear <- train(Performance.Tag.y~., data=train, method="svmLinear", metric=metric, 
                       tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svmLinear)
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was C = 1.

# Plotting "fit.svm" results
plot(fit.svmLinear)

# Valdiating the model after cross validation on test data
eval_svmLinear<- predict(fit.svmLinear, test)
confusionMatrix(eval_svmLinear, test$Performance.Tag.y)

# Accuracy : 0.6119  
# Sensitivity : 0.6091          
# Specificity : 0.6753
# For the linear kernel it looks the most stable and the values for sensitivity ,specificity and accuracy looks consistent



######################################Using Polynomial Kernel##############################################
# Using Polynomial Kernel : degree=2
Model_poly <- ksvm(Performance.Tag.y~ ., data = train, scale = FALSE, kernel = "polydot",kpar=list(degree=2))
# Predicting the model results 
Eval_Poly<- predict(Model_poly, test)
#confusion matrix - RBF Kernel
confusionMatrix(Eval_Poly,test$Performance.Tag.y)

# Accuracy : 0.9085
# Sensitivity : 0.94456         
# Specificity : 0.09050 

# The specificity looks really low

#### Hyperparameter tuning and Cross Validation : using Polynomial Kernel ####
# We will use the train function from caret package to perform Cross Validation.
#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl<-trainControl(method = "cv", number = 5, returnResamp = "all")
# train function takes Target ~ Prediction, Data, Method = Algorithm
# trcontrol = Our traincontrol method.
fit.svm_poly  <- train(Performance.Tag.y ~ ., data = train, method = "svmPoly", trControl = trainControl, preProc = c("center", "scale"))
print(fit.svm_poly)

# Accuracy was used to select the optimal model using  the largest value.
# The final values used for the model were degree = 2, scale = 0.1 and C = 1.
# Accuracy 0.6730003
plot(fit.svm_poly)

# Predicting the model results 
Eval_fit.svm_poly<- predict(fit.svm_poly, test)
#confusion matrix - RBF Kernel
confusionMatrix(Eval_fit.svm_poly,test$Performance.Tag.y)

# Accuracy : 0.8841 
# Sensitivity : 0.91637         
# Specificity : 0.15158 

###############################Using RBF Kernel###############################
Model_RBF <- ksvm(Performance.Tag.y~ ., data = train, scale = FALSE, kernel = "rbfdot")
RBF_linear<- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(RBF_linear,test$Performance.Tag.y)

# Accuracy : 0.7703 
# Sensitivity : 0.78576        
# Specificity : 0.41968 


####   Hyperparameter tuning and Cross Validation  using Radial SVM for C = 1 to 5 ####
trainControl <- trainControl(method="cv", number=5)
grid <- expand.grid(.sigma=c(0.025, 0.05,0.075,0.1), .C=c(0.1,0.5,1,2,3,4,5))

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(Performance.Tag.y~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)
# Accuracy was used to select the optimal model using  the largest value.
# The final values used for the model were sigma = 0.1 and C = 2.
# 0.5081800

plot(fit.svm)

# Valdiating the model after cross validation on test data
eval_svmRadial<- predict(fit.svm, test)
confusionMatrix(eval_svmRadial, test$Performance.Tag.y)

# Accuracy : 0.1171
# Sensitivity : 0.08000         
# Specificity : 0.95928

# The accuracy is really low on test data for RBF Kernel


#####################Final Conclusion############################################3333
# In cross validation We encountered that Radial kernel gives poor accuracy . 
# After performing five fold cross validation we came to the conclusion that polynomial kernel performs the best to predict 
# Accuracy was used to select the optimal model using  the largest value.
# Linear kernel accuracy: ~61.19 %
# RBF kernel accuracy: ~11.71%
# Polynomial accuracy: ~88.%
# Accuracy = 88% is the best accuracy so far amongst all the models
# The model gives the best accuracy in case of polynomial kernel during cross validation.
# The final values used for the model were degree = 2, scale = 0.1 and C = 1.
# Thus, it is clear that the Polynomial kernel is a good choice for this data set
# As we are doing this modelling on a small dataset due to respource crunch we are not considering it our chosen model.
