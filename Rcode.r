##### PACKAGES & LIBRARY #####

install.packages("ggplot2")
install.packages("skimr")
install.packages("MASS")
install.packages("e1071")
install.packages("lsr")
install.packages("Momocs")
install.packages("data.table")
install.packages("caret")
install.packages("caTools")
install.packages("klaR")
install.packages("ggpubr")
install.packages("moments")
install.packages("sail")
install.packages("DrugClust")
install.packages("lmtest")
install.packages("VGAM")
install.packages("factoextra")
install.packages("randomForest")
install.packages("mice")
install.packages("ipred")

library(skimr)
library(ggplot2)
library(MASS)
library(e1071)
library(lsr)
library(Momocs)
library(data.table)
library(caret)
library(split)
library(caTools)
library(lattice)
library(rgl)
library(klaR)
library(nnet)
library(ggpubr)
library(moments)
library(rpart)
library(rpart.plot)
library(tree)
library(partykit)
library(sail)
library(DrugClust)
library(datasets)
library(lmtest)
library(VGAM)
library(haven)
library(FactoMineR)
library(factoextra)
library(randomForest)
library(mice)
library(ipred)

rm(list = ls())
##### SETWD & IMPORT DATASET #####
setwd(dir = "/Users/geraudtserstevens/Desktop/Market_segmentation")
Train<-read.csv("/Users/geraudtserstevens/Desktop/Market_segmentation/Train.csv", TRUE, sep = ",")

Train$Licence_Plate<-NULL

#########---------- DATA EXPLORATION ----------

#########---------- UNIVARIATE EXPLORATORY DATA ANALYSIS

#View the data frame
View(Train)

#head, length and dim of the dataset
head(Train,4) #first four lines
tail(Train, 4) #last four lines
dim(Train) #7131 data and 14 variables
length(Train)

#summary of the data
summary(Train)


#skimr - providing larger set of statistics
skim(Train)

#Group data by Segmentation then perform skim
Train %>%
  dplyr::group_by(Segmentation) %>%
  skim()

#####----- MISSING VALUES
#total NA values
sum(is.na(Train))
#Number of complete and incomplete individuals
sum(complete.cases(Train) == TRUE)
sum(complete.cases(Train) == FALSE)

#Number of NA values
sum(is.na(Train$Gender))
sum(is.na(Train$Ever_Married))
sum(is.na(Train$Age))
sum(is.na(Train$Graduated))
sum(is.na(Train$Profession))
sum(is.na(Train$Work_Experience))
sum(is.na(Train$Spending_Score))
sum(is.na(Train$Family_Size))
sum(is.na(Train$Car))
sum(is.na(Train$Licence_Plate))
sum(is.na(Train$Credit_Owner))
sum(is.na(Train$Child))
sum(is.na(Train$Var_1))
sum(is.na(Train$Segmentation))

#####----- Variables Categories

#NUMERICAL -> being interpretable as numbers
Train$Age<-as.numeric(Train$Age)
Train$Work_Experience<-as.numeric(Train$Work_Experience)
Train$Family_Size<-as.numeric(Train$Family_Size)
Train$Car<-as.numeric(Train$Car)
Train$Child<-as.numeric(Train$Child)

#FACTOR -> being interpretable as category
Train$Gender<-as.factor(Train$Gender)
Train$Ever_Married<-as.factor(Train$Ever_Married)
Train$Graduated<-as.factor(Train$Graduated)
Train$Profession<-as.factor(Train$Profession)
Train$Spending_Score<-as.factor(Train$Spending_Score)
Train$Credit_Owner<-as.factor(Train$Credit_Owner)
Train$Var_1<-as.factor(Train$Var_1)
Train$Segmentation<-as.factor(Train$Segmentation)

#CHARACTER -> object of type "character"
#Train$Licence_Plate<-as.character(Train$Licence_Plate)

#CHECKING: data types
str(Train) # OK


#####---------- Feature Transformation ----------

#####----- MISSING VALUES --- NA REPLACEMENT


Train.mis <- Train
summary(Train.mis)

md.pattern(Train.mis)

imputed_Data <- mice(Train.mis, m=1)


completeData<- complete(imputed_Data,1)
completeData

Train <- completeData
View(Train)

# OUTLIERS CLEANING

outliers_cleaning<- function(Train){
  # The function is there to perform outliers cleaning only on training sets
  # There are LOG10 in the outliers to remove because the outlier removing is done after the log10 transformation to tend to a normal distribution (see just next)

  ## - Family Size
  no_outlier.familysize <- which( Train$Family_Size <= log10(8))
  Train<- copy(Train[no_outlier.familysize, ])

  ## -  Child
  no_outlier.child <- which( Train$Child <= log10(6))
  Train<- copy(Train[no_outlier.child, ])
  return(Train)
}
outliers_cleaning_stand  <- function(Train){
  # The function is there to perform outliers cleaning only on training sets that are standardized
  # The function is the same as outliers_cleaning, except that the upper bounds are standardized

  ## - Family Size (mean : 0.3913134 , sd : 0.2436068)
  no_outlier.familysize <- which(Train$Family_Size <= (log10(8)-0.3913134)/0.2436068)
  Train<- copy(Train[no_outlier.familysize, ])

  ## -  Child (mean :0.1477174 , sd : 0.2360049 )
  no_outlier.child <- which( abs(Train$Child) <= (log10(6)-0.1477174)/0.2360049)
  Train<- copy(Train[no_outlier.child, ])
  return(Train)
}

## - LOG10 Transformation to tend to a Gaussian Distribution
# Work Experience
if (!is.null(Train$Work_Experience)){
  for(j in 1:length(Train$Work_Experience)){
    if(Train$Work_Experience[j]>0)
      Train$Work_Experience[j]<-log10(Train$Work_Experience[j])
  }
  skewness(Train$Work_Experience)

}
# Family Size
if (!is.null(Train$Family_Size)){
  for(j in 1:length(Train$Family_Size)){
    if(Train$Family_Size[j]>0)
      Train$Family_Size[j]<-log10(Train$Family_Size[j])
  }
  skewness(Train$Family_Size)

}
# Child
if (!is.null(Train$Child)){
  for(j in 1:length(Train$Child)){
    if(Train$Child[j]>0)
      Train$Child[j]<-log10(Train$Child[j])
  }
  skewness(Train$Child)
}

# Age
if (!is.null(Train$Age)){
  for(j in 1:length(Train$Age)){
    if(Train$Age[j]>0)
      Train$Age[j]<-log10(Train$Age[j])
  }
  skewness(Train$Age)
}
# Car
if (!is.null(Train$Car)){
  for(j in 1:length(Train$Car)){
    if(Train$Car[j]>0)
      Train$Car[j]<-log10(Train$Car[j])
  }
  skewness(Train$Car)
}

#Feature Scaling

#Standardization
# Numerical Data standardized contained in dataset Train.stand
Train.stand<-copy(Train)
Train.stand$Age <- (Train.stand$Age - mean(Train.stand$Age)) / sd(Train.stand$Age)
Train.stand$Work_Experience <- (Train.stand$Work_Experience - mean(Train.stand$Work_Experience)) / sd(Train.stand$Work_Experience)
Train.stand$Family_Size <- (Train.stand$Family_Size - mean(Train.stand$Family_Size)) / sd(Train.stand$Family_Size)
if (!is.null(Train.stand$Car))
  Train.stand$Car <- (Train.stand$Car - mean(Train.stand$Car)) / sd(Train.stand$Car)
if (!is.null(Train.stand$Child))
  Train.stand$Child <- (Train.stand$Child - mean(Train.stand$Child)) / sd(Train.stand$Child)

#-------------------------------------------------------------------------------------------------------


#####----- Data Visualisation

## - HISTOGRAMS -> for numericals

hist(Train$Age, xlab = "Age", las = 1, breaks = 10, col = "lightblue", main = "Age category")
hist(Train$Work_Experience, xlab = "Work Experience", las = 1, breaks = 10, col = "lightblue", main = "Work Experience")
#atypical distribution, many individuals don't have work experience or a few of it.
hist(Train$Family_Size, xlab = "Family Size", las = 1, breaks = 10, col = "lightblue", main = "Family Size")
#the distribution tend to the left, small family
hist(Train$Car, xlab = "Car", las = 1, breaks = 10, col = "lightblue", main = "Number of Car")
hist(Train$Child, xlab = "Child", las = 1, breaks = 10, col = "lightblue", main = "Number of Child")
#Again a atypical distribution, 0 or 1 children the most frequent (common)

## - PIE-CHARTS with percentages -> for factors

#Gender
Gender <- c(3217,3914)
lbls <- c("Female", "Male")
pct <- round(Gender/sum(Gender)*100)
lbls <- paste(lbls, pct) #add percent to labels
lbls <- paste(lbls, "%", sep="") #add % to labels
pie(Gender, labels = lbls, col  = rainbow(length(lbls)),
    main = "Pie Chart of Gender")
#Ever_Married
Ever_Married <- c(2909,4222)
lbls <- c("No", "Yes")
pct <- round(Ever_Married/sum(Ever_Married)*100)
lbls <- paste(lbls, pct) #add percent to labels
lbls <- paste(lbls, "%", sep="") #add % to labels
pie(Ever_Married, labels = lbls, col  = rainbow(length(lbls)),
    main = "Pie Chart of Ever_Married")
#Graduated
Graduated <- c(2688,4443)
lbls <- c("No", "Yes")
pct <- round(Graduated/sum(Graduated)*100)
lbls <- paste(lbls, pct) #add percent to labels
lbls <- paste(lbls, "%", sep="") #add % to labels
pie(Graduated, labels = lbls, col  = rainbow(length(lbls)),
    main = "Pie Chart of Graduated")
#Profession
Profession <- c(2203,1178,947,637,625,572,969)
lbls <- c("Artist", "Healthcare","Entertainment","Engineer","Doctor","Lawyer","Other")
pct <- round(Profession/sum(Profession)*100)
lbls <- paste(lbls, pct) #add percent to labels
lbls <- paste(lbls, "%", sep="") #add % to labels
pie(Profession, labels = lbls, col  = rainbow(length(lbls)),
    main = "Pie Chart of Profession")
#31% of artist
#Spending_Score
Spending_Score <- c(1757,1062,4312)
lbls <- c("Average", "High","Low")
pct <- round(Spending_Score/sum(Spending_Score)*100)
lbls <- paste(lbls, pct) #add percent to labels
lbls <- paste(lbls, "%", sep="") #add % to labels
pie(Spending_Score, labels = lbls, col  = rainbow(length(lbls)),
    main = "Pie Chart of Spending_Score")
#60% of low level of spending score
#Credit_Owner
Credit_Owner <- c(5843,1288)
lbls <- c("No","Yes")
pct <- round(Credit_Owner/sum(Credit_Owner)*100)
lbls <- paste(lbls, pct) #add percent to labels
lbls <- paste(lbls, "%", sep="") #add % to labels
pie(Credit_Owner, labels = lbls, col  = rainbow(length(lbls)),
    main = "Pie Chart of Credit_Owner")
#82% of no crediter
#Var_1
Var_1 <- c(123,392,706,1004,148,4580,178)
lbls <- c("Cat_1","Cat_2","Cat_3","Cat_4","Cat_5","Cat_6","Cat_7")
pct <- round(Var_1/sum(Var_1)*100)
lbls <- paste(lbls, pct) #add percent to labels
lbls <- paste(lbls, "%", sep="") #add % to labels
pie(Var_1, labels = lbls, col  = rainbow(length(lbls)),
    main = "Pie Chart of Var_1")
#64% of individuals are in cat6
#Segmentation
Segmentation <- c(1897,1594,1637,2003)
lbls <- c("A","B","C","D")
pct <- round(Segmentation/sum(Segmentation)*100)
lbls <- paste(lbls, pct) #add percent to labels
lbls <- paste(lbls, "%", sep="") #add % to labels
pie(Segmentation, labels = lbls, col  = rainbow(length(lbls)),
    main = "Pie Chart of Segmentation")

## - BOXPLOTS ->for numerical variables

boxplot(Train$Age)
boxplot(Train$Work_Experience)
boxplot(Train$Family_Size)
boxplot(Train$Car)
boxplot(Train$Child)
#lot of extreme variables

#####----- SKEWNESS

skewness(Train$Age)
skewness(Train$Work_Experience)
skewness(Train$Family_Size)
skewness(Train$Car)
skewness(Train$Child)
#Negatively skewed : Work exp, family size and child

qplot(Work_Experience, data = Train, geom = "density",
      alpha = I(1/5))
qplot(Family_Size, data = Train, geom = "density",
      alpha = I(1/5))
qplot(Child, data = Train, geom = "density",
      alpha = I(1/5))
qplot(Age, data = Train, geom = "density",
      alpha = I(1/5))
qplot(Car, data = Train, geom = "density",
      alpha = I(1/5))

#distribution after feature transformation : OK

#####----- KURTOSIS

kurtosis(Train$Age)
kurtosis(Train$Work_Experience)
kurtosis(Train$Family_Size)
kurtosis(Train$Car)
kurtosis(Train$Child)

#########---------- BIVARIATE EXPLORATORY DATA ANALYSIS

#####----- CORRELATION
Train.num<- subset(Train, select = c(Age, Work_Experience, Family_Size, Car, Child))
Train.cat<-subset(Train, select = c(Gender,Ever_Married,Graduated,Spending_Score,Profession,Credit_Owner,Var_1,Segmentation))
# Profession	Work_Experience	Spending_Score	Family_Size	Car	Licence_Plate	Credit_Owner	Child	Var_1	Segmentation
cor(Train.num)

#####----- CHI-Square
chisq.test(Train$Gender, Train$Segmentation)
chisq.test(Train$Ever_Married, Train$Segmentation)
chisq.test(Train$Graduated, Train$Segmentation)
chisq.test(Train$Profession, Train$Segmentation)
chisq.test(Train$Spending_Score, Train$Segmentation)
chisq.test(Train$Credit_Owner, Train$Segmentation)
chisq.test(Train$Var_1, Train$Segmentation)

dim_names=c("Gender","Ever_Married","Graduated","Spending_Score","Profession","Credit_Owner","Var_1","Segmentation") # Du coup je fais juste entre les categorical le chi-squared test au final
P_values<-matrix(nrow = length(Train.cat[1,]), ncol = length(Train.cat[1,]),dimnames=list(dim_names,dim_names))
X_squared<-matrix(nrow = length(Train.cat[1,]), ncol = length(Train.cat[1,]),dimnames=list(dim_names,dim_names))
CramV<-matrix(nrow = length(Train.cat[1,]), ncol = length(Train.cat[1,]),dimnames=list(dim_names,dim_names))
for (i in 1:length(Train.cat[1,])){
  for (j in 1:length(Train.cat[1,])){
    l<-chisq.test(Train.cat[,i],Train.cat[,j],simulate.p.value = TRUE)
    P_values[i,j]<-l$p.value
    X_squared[i,j]<-l$statistic
    if (l$p.value<0.05)
      var1_var2 <- Train[,c(i,j)]
    var1_var2[,1] = as.numeric(var1_var2[,1])
    var1_var2[,2] = as.numeric(var1_var2[,2])
    CramV[i,j]<-cramersV(var1_var2)
  }
}

print(round(P_values,digits=2))
print(round(X_squared,digits=2))
print(round(CramV,digits=2))

#####----- ANOVA
#Correlation between Segmentation and numerical variables
anova_Age<-aov(Train$Age~Train$Segmentation,data=Train)
summary(anova_Age)
anova_Work<-aov(Train$Work_Experience~Train$Segmentation,data=Train)
summary(anova_Work)
anova_Family<-aov(Train$Family_Size~Train$Segmentation,data=Train)
summary(anova_Family)
anova_Car<-aov(Train$Car~Train$Segmentation,data=Train)
summary(anova_Car)
anova_Child<-aov(Train$Child~Train$Segmentation,data=Train)
summary(anova_Child)
# p-values always lower than 5% -> OK


#####----- 2-WAY SPLIT : 80(TRAIN)-20(TEST)
set.seed(123) # for reproducibility
train_size<-floor(0.8 * nrow(Train))
train_ind<- sort(sample(sample(seq_len(nrow(Train)), size = train_size)))
Train.train80<-Train[train_ind,]
Train.train80<-outliers_cleaning(Train.train80)
Train.test20<-Train[-train_ind,]

#########---------- FEATURE ENGINEERING ----------

#####---------- Feature Selection ----------

#--- Delete non relevant variables
#Train$Licence_Plate<-NULL

#---  M1 : Maximum-relevance selection (+ ggplot between y and variables)
#Cramers V and ANOVA
# --RESULT => ALL explanatory variables have a significant impact on the segmentation of the customer, except gender who has 0.20 as p-value...

#--- M2 : Minimum-redundancy selection (association between the features)
#correlation test
# --RESULT => Car - Age, Child - Family Size, these pairs are strongly correlated

#--- M3 : Classification or regression model

#Stepwise log model

## -- BACKWARD : Short Version

null_model <- multinom(Segmentation ~ 1, Train.train80)
full_model <- multinom(Segmentation ~ Gender + Ever_Married + Age + Graduated + Profession + Work_Experience + Spending_Score + Family_Size + Car + Credit_Owner  + Child  + Var_1, Train.train80)
myStepwise <- stepAIC(full_model, scope=list(lower=null_model,upper=full_model), direction="backward", steps=1000, trace=2)
summary(myStepwise) #without work exp, credit and child

pred=predict(myStepwise,Train.test20)
confusionMatrix = table(pred, Train.test20$Segmentation)
Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
Accuracy

## -- BACKWARD : Long Version (with all variables,except licence plates)

fit_full <- multinom(Segmentation ~ Gender + Ever_Married + Age + Graduated + Profession + Work_Experience + Spending_Score + Family_Size + Car + Credit_Owner  + Child  + Var_1, Train.train80)

pred=predict(fit_full,Train.test20)
confusionMatrix = table(pred, Train.test20$Segmentation)
Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
Accuracy

#without credit -> likehood = 0.25
fit_reduced <- multinom(Segmentation ~ Gender + Ever_Married + Age + Graduated + Profession + Work_Experience + Spending_Score + Family_Size + Car + Child  + Var_1, Train.train80)
summary(fit_reduced)
#without child  -> likehood = 0.18
fit_reduced <- multinom(Segmentation ~ Gender + Ever_Married + Age + Graduated + Profession + Work_Experience + Spending_Score + Family_Size  + Car + Var_1, Train.train80)
summary(fit_reduced)
#without work exp -> likehood = 0.07 => STOP HERE
fit_reduced <- multinom(Segmentation ~ Ever_Married + Age + Graduated + Profession + Spending_Score + Family_Size + Car, Train.train80)
summary(fit_reduced)
#without car -> likehood = 0.04
fit_reduced <- multinom(Segmentation ~ Gender + Ever_Married + Age + Graduated + Profession + Spending_Score + Family_Size + Var_1, Train.train80)
summary(fit_reduced)
#without var
fit_reduced <- multinom(Segmentation ~ Gender + Ever_Married + Age + Graduated + Profession + Spending_Score + Family_Size, Train.train80)
summary(fit_reduced)
#without family size
fit_reduced <- multinom(Segmentation ~ Gender + Ever_Married + Age + Graduated + Profession + Spending_Score, Train.train80)
summary(fit_reduced)

pred=predict(fit_reduced,Train.test20)
confusionMatrix = table(pred, Train.test20$Segmentation)
Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
Accuracy

## -- Likehood ratio to see when the reduced model and the full model fit the data equally well, when the p-value < 0.05 the null hypothesis is rejected and then the full model give a improvement comparing at the reduced model
lrtest(fit_full, fit_reduced)

## -- RESULT => Work exp, credit and child are not relevant variables to fit the model


#--- M4 : Likehood Ratio

fit_full <- vglm(Segmentation ~ Gender + Ever_Married + Age + Graduated + Profession + Work_Experience + Spending_Score + Family_Size + Car + Credit_Owner  + Child  + Var_1, family=multinomial, data=Train.train80)
summary(fit_full)

fit_reduced <- vglm(Segmentation ~ Gender + Ever_Married + Age + Graduated + Profession + Work_Experience + Spending_Score + Family_Size + Car + Child  + Var_1, family=multinomial, data=Train.train80)
summary(fit_reduced)

lrtest(fit_full, fit_reduced)
## -- RESULT => #Gender, ever married, age,graduated, profession improve the model, child, var,credit, car, family size give poor information

#FINAL SELECTION : Work Experience, Child, Credit and Car are out of our models  !!! To go further : after viewing some other regression model, gender and var_1 can also be removed.



#####---------- Feature Extraction ----------

#################### ------------- PCA for numerical feature -------------

Train_PCA_MCA_LDA <- data.frame(matrix(nrow = nrow(Train),ncol = 26))
colnames(Train_PCA_MCA_LDA) <- c("Age","Work_Experience","Family_Size","Car","Child","Gender","Ever_Married","Graduated","Profession","Spending_Score","Credit_Owner","Var_1","Segmentation","PC1","PC2","PC3","PC4","PC5","MC1","MC2","MC3","MC4","MC5", "LD1", "LD2","LD3")
Train_PCA_MCA_LDA[,c("Age","Work_Experience","Family_Size","Car","Child","Gender","Ever_Married","Graduated","Profession","Spending_Score","Credit_Owner","Var_1","Segmentation")]<-Train.stand[,c("Age","Work_Experience","Family_Size","Car","Child","Gender","Ever_Married","Graduated","Profession","Spending_Score","Credit_Owner","Var_1","Segmentation")]

vector_PCA<-Train.stand[,c("Age","Work_Experience","Family_Size","Car","Child")]
PCA<-prcomp(vector_PCA,center=TRUE,scale=TRUE)
print(PCA)
summary(PCA)
plot(PCA, type="l")

#See the PCA Circle -> 2 first dimensions represent 74.2% of our database
fviz_pca_var(PCA,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Use the PCA to create new variables -> using PCA dimensions as variables
Train_PCA_MCA_LDA[,c("PC1","PC2","PC3","PC4","PC5")]<-PCA$x

#Split
set.seed(123) # for reproducibility
train_size<-floor(0.8 * nrow(Train_PCA_MCA_LDA))
train_ind<- sort(sample(sample(seq_len(nrow(Train_PCA_MCA_LDA)), size = train_size)))
Train_PCA_MCA_LDA.train80<-Train_PCA_MCA_LDA[train_ind,]
Train_PCA_MCA_LDA.train80<-outliers_cleaning_stand(Train_PCA_MCA_LDA.train80)
Train_PCA_MCA_LDA.test20<-Train_PCA_MCA_LDA[-train_ind,]


#Logistic Regression  PCA1+PCA2 + All categorical variables
reg = multinom(Segmentation~PC1+PC2+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1,data=Train_PCA_MCA_LDA.train80,trace=FALSE)
reg

pred = predict (reg, Train_PCA_MCA_LDA.test20)
confusionMatrix = table(pred,Train_PCA_MCA_LDA.test20$Segmentation)
confusionMatrix
Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
Accuracy # 0.4583041 #0.4667134 with standardized


#Logistic Regression  PCA1+PCA2+PCA3 + All categorical variables
reg = multinom(Segmentation~PC1+PC2+PC3+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1,data=Train_PCA_MCA_LDA.train80,trace=FALSE)
reg

pred = predict (reg, Train_PCA_MCA_LDA.test20)
confusionMatrix = table(pred,Train_PCA_MCA_LDA.test20$Segmentation)
confusionMatrix
Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
Accuracy # 0.4533987 #0.4646111 with standardized

#Logistic Regression  PCA1+PCA2+PCA3+PCA4 + All categorical variables
reg = multinom(Segmentation~PC1+PC2+PC3+PC4+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1,data=Train_PCA_MCA_LDA.train80,trace=FALSE)
reg

pred = predict (reg, Train_PCA_MCA_LDA.test20)
confusionMatrix = table(pred,Train_PCA_MCA_LDA.test20$Segmentation)
confusionMatrix
Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
Accuracy # 0.4540995 #0.4590049 with standardized

#Logistic Regression  PCA1+PCA2+PCA3+PCA4+PCA5 + All categorical variables
reg = multinom(Segmentation~PC1+PC2+PC3+PC4+PC5+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1,data=Train_PCA_MCA_LDA.train80,trace=FALSE)
reg

pred = predict (reg, Train_PCA_MCA_LDA.test20)
confusionMatrix = table(pred,Train_PCA_MCA_LDA.test20$Segmentation)
confusionMatrix
Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
Accuracy # 0.4597057 # 0.4681 with standardized

#Logistic Regression   All categorical variables & All num variables (no PCA )
reg = multinom(Segmentation~Age+Work_Experience+Family_Size+Car+Child+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1,data=Train_PCA_MCA_LDA.train80,trace=FALSE)
reg

pred = predict (reg, Train_PCA_MCA_LDA.test20)
confusionMatrix = table(pred,Train_PCA_MCA_LDA.test20$Segmentation)
confusionMatrix
Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
Accuracy # 0.461808  # 0.4681 with standardized

#################### ------------- MCA for categorical feature -------------

res.mca<-MCA(Train.cat[,c("Gender","Ever_Married","Graduated","Profession","Spending_Score","Credit_Owner","Var_1")], graph = F)
print(res.mca)
summary(res.mca)

#Extraction of eigenvalues / variances of principal components
get_eigenvalue(res.mca)

#Visualisation of eigenvalues
fviz_eig(res.mca)

#Extraction of results for individuals and variables, respectively.
get_mca_ind(res.mca)
get_mca_var(res.mca)

#visualisation of individual and variable results, respectively.
fviz_mca_ind(res.mca)
fviz_mca_var(res.mca)

#Creation of a biplot of individuals and variables.
fviz_mca_biplot(res.mca)


### visualisation of the correlation between the variables and the main axes of the MCA
fviz_mca_var (res.mca, choice = "mca.cor", repel = TRUE,  ggtheme = theme_minimal ()) #the two first dimension represent 20% of our database


Train_PCA_MCA_LDA[,c("MC1","MC2","MC3","MC4","MC5")]<-res.mca$ind$coord
set.seed(123) # for reproducibility
train_size<-floor(0.8 * nrow(Train_PCA_MCA_LDA))
train_ind<- sort(sample(sample(seq_len(nrow(Train_PCA_MCA_LDA)), size = train_size)))
Train_PCA_MCA_LDA.train80<-Train_PCA_MCA_LDA[train_ind,]
Train_PCA_MCA_LDA.train80<-copy(outliers_cleaning_stand(Train_PCA_MCA_LDA.train80))
Train_PCA_MCA_LDA.test20<-Train_PCA_MCA_LDA[-train_ind,]



#Logistic Regression  MCA1+MCA2 + All numerical variables
reg = multinom(Segmentation~MC1+MC2+Age+Work_Experience+Family_Size+Car+Child,data=Train_PCA_MCA_LDA.train80,trace=FALSE)
reg

pred = predict (reg, Train_PCA_MCA_LDA.test20)
confusionMatrix = table(pred,Train_PCA_MCA_LDA.test20$Segmentation)
confusionMatrix
Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
Accuracy # 0.4428872

#Logistic Regression  MCA1+MCA2+MCA3 + All numerical variables
reg = multinom(Segmentation~MC1+MC2+MC3+Age+Work_Experience+Family_Size+Car+Child,data=Train_PCA_MCA_LDA.train80,trace=FALSE)
reg

pred = predict (reg, Train_PCA_MCA_LDA.test20)
confusionMatrix = table(pred,Train_PCA_MCA_LDA.test20$Segmentation)
confusionMatrix
Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
Accuracy # 0.4484933

#Logistic Regression  MCA1+MCA2+MCA3+MCA4+MCA5 + All numerical variables
reg = multinom(Segmentation~MC1+MC2+MC3+MC4+MC5+Age+Work_Experience+Family_Size+Car+Child,data=Train_PCA_MCA_LDA.train80,trace=FALSE)
reg

pred = predict (reg, Train_PCA_MCA_LDA.test20)
confusionMatrix = table(pred,Train_PCA_MCA_LDA.test20$Segmentation)
confusionMatrix
Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
Accuracy #0.4562018

#################### ------------- LDA for numerical feature -------------


### Only with num
lda <- lda(formula = Segmentation ~ Age + Child + Car + Family_Size + Work_Experience, data = Train.train80) # LDA is calculated over the training set
lda$prior
lda$counts
lda$means
lda$scaling


ldavalue <- predict(lda, Train.stand)
ldavalue$x

Train_PCA_MCA_LDA[,c("LD1","LD2","LD3")]<-ldavalue$x


#Split
set.seed(123) # for reproducibility
train_size<-floor(0.8 * nrow(Train_PCA_MCA_LDA))
train_ind<- sort(sample(sample(seq_len(nrow(Train_PCA_MCA_LDA)), size = train_size)))
Train_PCA_MCA_LDA.train80<-Train_PCA_MCA_LDA[train_ind,]
Train_PCA_MCA_LDA.train80<-copy(outliers_cleaning_stand(Train_PCA_MCA_LDA.train80))
Train_PCA_MCA_LDA.test20<-Train_PCA_MCA_LDA[-train_ind,]


# LD1+LD2+LD3
reg = multinom(Segmentation~ LD1 + LD2 + LD3, data=Train_PCA_MCA_LDA.train80,trace=FALSE)
reg

pred = predict (reg, Train_PCA_MCA_LDA.test20)
confusionMatrix = table(pred,Train_PCA_MCA_LDA.test20$Segmentation)
confusionMatrix
Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
Accuracy ## 38.68%


#LD1+LD2+LD3+ ALL CATEGORICAL
reg = multinom(Segmentation~ LD1 + LD2 + LD3 +Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, data=Train_PCA_MCA_LDA.train80,trace=FALSE)
reg

pred = predict (reg, Train_PCA_MCA_LDA.test20)
confusionMatrix = table(pred,Train_PCA_MCA_LDA.test20$Segmentation)
confusionMatrix
Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
Accuracy ## 46.88%

#LD1+LD2 ALL CATEGORICAL
reg = multinom(Segmentation~ LD1 + LD2 +Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, data=Train_PCA_MCA_LDA.train80,trace=FALSE)
reg

pred = predict (reg, Train_PCA_MCA_LDA.test20)
confusionMatrix = table(pred,Train_PCA_MCA_LDA.test20$Segmentation)
confusionMatrix
Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
Accuracy ## 46.67%

#LD1+LD2+LD3+ ALL CATEGORICAL (except Gender, Work_Experience and Var_1 (see feature selction))
reg = multinom(Segmentation~ LD1 + LD2 + LD3 +Ever_Married+Graduated+Profession+Spending_Score, data=Train_PCA_MCA_LDA.train80,trace=FALSE)
reg

pred = predict (reg, Train_PCA_MCA_LDA.test20)
confusionMatrix = table(pred,Train_PCA_MCA_LDA.test20$Segmentation)
confusionMatrix
Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
Accuracy ## 46.32%

#-------------------------------------------------------------------------------------------------------

#Creation of the validation table
dim_names=c("Decision trees","Normal logistic regressions","Stepwise logistic regressions","Naive Bayes","Random Forest","Bagging","X-G Boost")
Validation_table<-matrix(nrow = 7, ncol=20,dimnames=list(dim_names))

#########---------- CROSS VALIDATION AND CLASSIFICATION MODELS


# The cross-validation is quite computationally heavy.
# If you don't want to run all these for loops, just go under the cross validation section, the results are available.

#####----- CROSS VALIDATION OF THE TREES

set.seed(123)
k<-10
folds<-CreateFolds(Train,k)+1 # In folds, there are number from 1 to k to assign each data a "group"
sum_accuracies<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

for (i in 1:k){
  print(i)
  test_ind<-which(folds==i)
  test<-Train_PCA_MCA_LDA[test_ind,]
  train<-Train_PCA_MCA_LDA[-test_ind,]
  train<-outliers_cleaning_stand(train)
  lda <- lda(formula = Segmentation ~ Age + Child + Car + Family_Size + Work_Experience, data = train) # LDA is calculated over the training set
  train[,c("LD1","LD2","LD3")]<- predict(lda, train)$x
  test[,c("LD1","LD2","LD3")]<- predict(lda, test)$x

  # 1) model with raw data 2) models with feature selection 3) models with PCA 4) models with MCA 5) models with LDA 6) Mixed Models
  ########## Raw data  ##########

  #Tree 1 : no parameters
  Train.tree <- rpart(Segmentation ~ Gender+Ever_Married+Age+Graduated+Spending_Score+Profession+Work_Experience+Family_Size+Car+Credit_Owner+Child+Var_1, data = train)
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[1]<-sum_accuracies[1]+Accuracy
  if (i==1){
    rpart.plot(Train.tree, extra = 104)
  }


  #Tree 2 : specific prior (0.25, 0.25, 0.25, 0.25) and "information" split
  Train.tree <- rpart(Segmentation ~ Gender+Ever_Married+Age+Graduated+Spending_Score+Profession+Work_Experience+Family_Size+Car+Credit_Owner+Child+Var_1, data = train,
                      parms = list(prior= c(0.25, 0.25, 0.25, 0.25) , split = "information"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[2]<-sum_accuracies[2]+Accuracy

  #Tree 3 specific prior (0.25, 0.25, 0.25, 0.25) and "gini" split
  Train.tree <- rpart(Segmentation ~ Gender+Ever_Married+Age+Graduated+Spending_Score+Profession+Work_Experience+Family_Size+Car+Credit_Owner+Child+Var_1, data = train,
                      parms = list(prior= c(0.25, 0.25, 0.25, 0.25) , split = "gini"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[3]<-sum_accuracies[3]+Accuracy

  #Tree 4 specific prior (0.3, 0.3, 0.2, 0.2) and "information" split
  Train.tree <- rpart(Segmentation ~ Gender+Ever_Married+Age+Graduated+Spending_Score+Profession+Work_Experience+Family_Size+Car+Credit_Owner+Child+Var_1, data = train,
                      parms = list(prior= c(0.3, 0.3, 0.2, 0.2) , split = "information"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[4]<-sum_accuracies[4]+Accuracy

  # Tree 5 specific prior (0.3, 0.3, 0.2, 0.2) and "gini" split
  Train.tree <- rpart(Segmentation ~ Gender+Ever_Married+Age+Graduated+Spending_Score+Profession+Work_Experience+Family_Size+Car+Credit_Owner+Child+Var_1, data = train,
                      parms = list(prior= c(0.3, 0.3, 0.2, 0.2) , split = "gini"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[5]<-sum_accuracies[5]+Accuracy

  #Tree 6 specific control with minsplit=10 and maxdepth=1
  control_1 <- rpart.control(minsplit = 10, maxdepth = 1)
  Train.tree <- rpart(Segmentation ~ Gender+Ever_Married+Age+Graduated+Spending_Score+Profession+Work_Experience+Family_Size+Car+Credit_Owner+Child+Var_1, data = train, control = control_1)
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[6]<-sum_accuracies[6]+Accuracy

  #Tree 7 specific control with minsplit=10 and maxdepth=6
  control_6 <- rpart.control(minsplit = 10, maxdepth = 6)
  Train.tree <- rpart(Segmentation ~ Gender+Ever_Married+Age+Graduated+Spending_Score+Profession+Work_Experience+Family_Size+Car+Credit_Owner+Child+Var_1, data = train, control = control_6)
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[7]<-sum_accuracies[7]+Accuracy

  #Tree 8 specific prior (0.2, 0.2, 0.3, 0.3) and "information" split
  Train.tree <- rpart(Segmentation ~ Gender+Ever_Married+Age+Graduated+Spending_Score+Profession+Work_Experience+Family_Size+Car+Credit_Owner+Child+Var_1, data = train,
                      parms = list(prior= c(0.3, 0.3, 0.2, 0.2) , split = "information"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[8]<-sum_accuracies[8]+Accuracy


  ########## Data with feature selection (without Work_Experience, Child, Credit_Owner, Car, Gender and Var_1) for the best model here above

  #Tree 9 specific prior (0.2, 0.2, 0.3, 0.3) and "information" split
  Train.tree <- rpart(Segmentation ~ Ever_Married+Age+Graduated+Spending_Score+Profession+Family_Size, data = train,
                      parms = list(prior= c(0.3, 0.3, 0.2, 0.2) , split = "information"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[9]<-sum_accuracies[9]+Accuracy

  #Tree 10 specific prior (0.3, 0.3, 0.2, 0.2) and "information" split
  Train.tree <- rpart(Segmentation ~ Ever_Married+Age+Graduated+Spending_Score+Profession+Family_Size, data = train,
                      parms = list(prior= c(0.3, 0.3, 0.2, 0.2) , split = "information"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[10]<-sum_accuracies[10]+Accuracy


  ########## PCA ##########

  #Tree 11 Tree : no parameters & PCA1+PCA2+PCA3
  Train.tree <- rpart(Segmentation ~ PC1+PC2+PC3, data = train,
                      parms = list(prior= c(0.3, 0.3, 0.2, 0.2) , split = "information"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[11]<-sum_accuracies[11]+Accuracy

  #Tree 12 Tree : no parameters & PCA1+PCA2 + All categorical variables
  Train.tree <- rpart(Segmentation ~ PC1+PC2+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, data = train,
                      parms = list(prior= c(0.3, 0.3, 0.2, 0.2) , split = "information"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[12]<-sum_accuracies[12]+Accuracy


  #13 Tree : no parameters & PCA1+PCA2+PCA3 + All categorical variables
  Train.tree <- rpart(Segmentation ~ PC1+PC2+PC3+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, data = train,
                      parms = list(prior= c(0.3, 0.3, 0.2, 0.2) , split = "information"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[13]<-sum_accuracies[13]+Accuracy


  ########## MCA ##########

  #14 Tree : no parameters & MCA1+MCA2+MCA3
  Train.tree <- rpart(Segmentation ~ MC1+MC2+MC3, data = train,
                      parms = list(prior= c(0.3, 0.3, 0.2, 0.2) , split = "information"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[14]<-sum_accuracies[14]+Accuracy

  #15 Tree : no parameters & MCA1+MCA2+MCA3 + All numerical variables
  Train.tree <- rpart(Segmentation ~ MC1+MC2+MC3+Age+Work_Experience+Family_Size+Car+Child, data = train,
                      parms = list(prior= c(0.3, 0.3, 0.2, 0.2) , split = "information"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[15]<-sum_accuracies[15]+Accuracy

  ########## LDA ##########

  #16 Tree : no parameters & LDA1+LDA2+LDA3
  Train.tree <- rpart(Segmentation ~ LD1+LD2+LD3, data = train,
                      parms = list(prior= c(0.3, 0.3, 0.2, 0.2) , split = "information"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[16]<-sum_accuracies[16]+Accuracy

  #17 Tree : no parameters & LDA1+LDA2+LDA3 + All categorical variables
  Train.tree <- rpart(Segmentation ~ LD1+LD2+LD3+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, data = train,
                      parms = list(prior= c(0.3, 0.3, 0.2, 0.2) , split = "information"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[17]<-sum_accuracies[17]+Accuracy

  ########## Mixed Models ##########

  #18 Tree : no parameters & PCA1+PCA2+PCA3+  MCA1+MCA2+MCA3
  Train.tree <- rpart(Segmentation ~ PC1+PC2+PC3+MC1+MC2+MC3, data = train,
                      parms = list(prior= c(0.3, 0.3, 0.2, 0.2) , split = "information"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[18]<-sum_accuracies[18]+Accuracy

  #19 Tree : no parameters & LDA1+LDA2+LDA3 + MCA1+MCA2+MCA3
  Train.tree <- rpart(Segmentation ~ LD1+LD2+LD3+MC1+MC2+MC3, data = train,
                      parms = list(prior= c(0.3, 0.3, 0.2, 0.2) , split = "information"))
  pred <- predict(Train.tree, newdata = test, type = 'class')
  confusionTree = table(test$Segmentation, pred)
  Accuracy<-(confusionTree[1,1]+confusionTree[2,2]+confusionTree[3,3]+confusionTree[4,4])/sum(confusionTree)
  sum_accuracies[19]<-sum_accuracies[19]+Accuracy
}
Accuracy_kfold<-sum_accuracies/k
print(Accuracy_kfold)
Validation_table["Decision trees",]<-Accuracy_kfold

#####----- CROSS VALIDATION OF SIMPLE LOGISTIC REGRESSION

set.seed(123)
k<-10
folds<-CreateFolds(Train,k)+1 # In folds, there are number from 1 to k to assign each data a "group"
sum_accuracies<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
AIC_table<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for (i in 1:k){
  print(i)
  test_ind<-which(folds==i)
  test<-Train_PCA_MCA_LDA[test_ind,]
  train<-Train_PCA_MCA_LDA[-test_ind,]
  train<-outliers_cleaning_stand(train)
  lda <- lda(formula = Segmentation ~ Age + Child + Car + Family_Size + Work_Experience, data = train) # LDA is calculated over the training set
  train[,c("LD1","LD2","LD3")]<- predict(lda, train)$x
  test[,c("LD1","LD2","LD3")]<- predict(lda, test)$x

  # 1) model with raw data 2) models with feature selection 3) models with PCA 4) models with MCA 5) models with LDA 6) models PCA, MCA,LDA with feature selection 7) Mixed models
  ########## Raw data  ##########

  #1######### Logistic model with RAW data (without Licence Plate)
  reg = multinom(Segmentation~Gender + Ever_Married + Age + Graduated + Profession + Work_Experience + Spending_Score + Family_Size + Car + Credit_Owner  + Child  + Var_1, data=train, trace=FALSE)
  pred  = predict(reg, test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[1]<-sum_accuracies[1]+Accuracy
  AIC_table[1]<-AIC_table[1]+AIC(reg)


  #2######### Logistic model tacking into account Feature Selection (without Licence Plate, Work Exp, Child, Credit Owner)
  reg = multinom(Segmentation ~ Gender + Ever_Married + Age + Graduated + Profession + Spending_Score + Family_Size + Car + Var_1, data=train, trace=FALSE)
  pred  = predict(reg, test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[2]<-sum_accuracies[2]+Accuracy
  AIC_table[2]<-AIC_table[2]+AIC(reg)

  #3######### Logistic model taking into account Feature Selection (without Licence Plate, Work Exp, Child, Credit Owner, Gender and Var 1 )
  reg = multinom(Segmentation ~ Ever_Married + Age + Graduated + Profession + Spending_Score + Family_Size + Car, data=train, trace=FALSE)
  pred  = predict(reg, test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[3]<-sum_accuracies[3]+Accuracy
  AIC_table[3]<-AIC_table[3]+AIC(reg)


  #4######### Logistic model with PC1 + PC2 + All categorical variables  =>BEST MODEL WITH 46.94%
  reg = multinom(Segmentation~PC1+PC2+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1,data=train,trace=FALSE)
  pred = predict (reg, test)
  confusionMatrix = table(pred,test$Segmentation)
  Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[4]<-sum_accuracies[4]+Accuracy
  AIC_table[4]<-AIC_table[4]+AIC(reg)

  #5######### Logistic Regression  PC1+PC2+PC3 + All categorical variables
  reg = multinom(Segmentation~PC1+PC2+PC3+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1,data=train,trace=FALSE)
  pred = predict (reg, test)
  confusionMatrix = table(pred,test$Segmentation)
  Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[5]<-sum_accuracies[5]+Accuracy
  AIC_table[5]<-AIC_table[5]+AIC(reg)


  #6######### Logistic Regression  MC1+MC2 + All numerical variables
  reg = multinom(Segmentation~MC1+MC2+Age+Work_Experience+Family_Size+Car+Child,data=train,trace=FALSE)
  pred = predict (reg, test)
  confusionMatrix = table(pred,test$Segmentation)
  Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[6]<-sum_accuracies[6]+Accuracy
  AIC_table[6]<-AIC_table[6]+AIC(reg)

  #7######### Logistic Regression  MC1+MC2+MC3 + All numerical variables
  reg = multinom(Segmentation~MC1+MC2+MC3+Age+Work_Experience+Family_Size+Car+Child,data=train,trace=FALSE)
  pred = predict (reg, test)
  confusionMatrix = table(pred,test$Segmentation)
  Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[7]<-sum_accuracies[7]+Accuracy
  AIC_table[7]<-AIC_table[7]+AIC(reg)

  #8######### Logistic model with LD1 + LD2 + All categorical variables
  reg = multinom(Segmentation~LD1+LD2+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1,data=train,trace=FALSE)
  pred = predict (reg, test)
  confusionMatrix = table(pred,test$Segmentation)
  Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[8]<-sum_accuracies[8]+Accuracy
  AIC_table[8]<-AIC_table[8]+AIC(reg)

  #9######### Logistic Regression  LD1+LD2+LD3 + All categorical variables
  reg = multinom(Segmentation~LD1+LD2+LD3+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1,data=train,trace=FALSE)
  pred = predict (reg, test)
  confusionMatrix = table(pred,test$Segmentation)
  Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[9]<-sum_accuracies[9]+Accuracy
  AIC_table[9]<-AIC_table[9]+AIC(reg)

  #10######### Logistic model with PC1 + PC2 + All categorical variables + taking into account Feature Selection (without Gender, Credit Owner and Var 1)
  reg = multinom(Segmentation~PC1+PC2+Ever_Married+Graduated+Profession+Spending_Score,data=train,trace=FALSE)
  pred = predict (reg, test)
  confusionMatrix = table(pred,test$Segmentation)
  Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[10]<-sum_accuracies[10]+Accuracy
  AIC_table[10]<-AIC_table[10]+AIC(reg)

  #11######### Logistic model with PC1 + PC2 +PC3 + All categorical variables + taking into account Feature Selection (without Gender, Credit Owner and Var 1)
  reg = multinom(Segmentation~PC1+PC2+PC3+Ever_Married+Graduated+Profession+Spending_Score,data=train,trace=FALSE)
  pred = predict (reg, test)
  confusionMatrix = table(pred,test$Segmentation)
  Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[11]<-sum_accuracies[11]+Accuracy
  AIC_table[11]<-AIC_table[11]+AIC(reg)

  #12######### Logistic Regression  MC1+MC2 + All numerical variables + taking into  account Feature Selection (without Work Experience and Child)
  reg = multinom(Segmentation~MC1+MC2+Age+Family_Size+Car,data=train,trace=FALSE)
  pred = predict (reg, test)
  confusionMatrix = table(pred,test$Segmentation)
  Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[12]<-sum_accuracies[12]+Accuracy
  AIC_table[12]<-AIC_table[12]+AIC(reg)

  #13######### Logistic Regression  MC1+MC2+MC3 + All numerical variables + taking into  account Feature Selection (without Work Experience and Child)
  reg = multinom(Segmentation~MC1+MC2+MC3+Age+Family_Size+Car,data=train,trace=FALSE)
  pred = predict (reg, test)
  confusionMatrix = table(pred,test$Segmentation)
  Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[13]<-sum_accuracies[13]+Accuracy
  AIC_table[13]<-AIC_table[13]+AIC(reg)

  #14######### Logistic model with LD1 + LD2 + All categorical variables + taking into account Feature Selection (without Gender, Credit Owner and Var 1)
  reg = multinom(Segmentation~LD1+LD2+Ever_Married+Graduated+Profession+Spending_Score,data=train,trace=FALSE)
  pred = predict (reg, test)
  confusionMatrix = table(pred,test$Segmentation)
  Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[14]<-sum_accuracies[14]+Accuracy
  AIC_table[14]<-AIC_table[14]+AIC(reg)

  #15######### Logistic model with LD1 + LD2 +LD3 + All categorical variables + taking into account Feature Selection (without Gender, Credit Owner and Var 1)
  reg = multinom(Segmentation~LD1+LD2+LD3+Ever_Married+Graduated+Profession+Spending_Score,data=train,trace=FALSE)
  pred = predict (reg, test)
  confusionMatrix = table(pred,test$Segmentation)
  Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[15]<-sum_accuracies[15]+Accuracy
  AIC_table[15]<-AIC_table[15]+AIC(reg)

  #16######### Mixed Logistic Regression  PC1+PC2+PC3+MC1+MC2+MC3
  reg = multinom(Segmentation~PC1+PC2+PC3+MC1+MC2+MC3,data=train,trace=FALSE)
  pred = predict (reg, test)
  confusionMatrix = table(pred,test$Segmentation)
  Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[16]<-sum_accuracies[16]+Accuracy
  AIC_table[16]<-AIC_table[16]+AIC(reg)

  #17######### Mixed Logistic Regression  LD1+LD2+LC3+MC1+MC2+MC3
  reg = multinom(Segmentation~LD1+LD2+LD3+MC1+MC2+MC3,data=train,trace=FALSE)
  pred = predict (reg, test)
  confusionMatrix = table(pred,test$Segmentation)
  Accuracy <- Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[17]<-sum_accuracies[17]+Accuracy
  AIC_table[17]<-AIC_table[17]+AIC(reg)

}
Accuracy_kfold<-sum_accuracies/k
AIC_table<-AIC_table/k
print(Accuracy_kfold)
print(AIC_table)
Validation_table["Normal logistic regressions",]<-Accuracy_kfold

#####----- CROSS VALIDATION OF STEPWISE LOGISTIC REGRESSION

set.seed(123)
k<-10
folds<-CreateFolds(Train,k)+1 # In folds, there are number from 1 to k to assign each data a "group"
sum_accuracies<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
AIC_table<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

for (i in 1:k){
  print(i)
  test_ind<-which(folds==i)
  test<-Train_PCA_MCA_LDA[test_ind,]
  train<-Train_PCA_MCA_LDA[-test_ind,]
  train<-outliers_cleaning_stand(train)
  lda <- lda(formula = Segmentation ~ Age + Child + Car + Family_Size + Work_Experience, data = train) # LDA is calculated over the training set
  train[,c("LD1","LD2","LD3")]<- predict(lda, train)$x
  test[,c("LD1","LD2","LD3")]<- predict(lda, test)$x

  #1######### Stepwise Logistic model with RAW data (without Licence Plate)
  null_model <- multinom(Segmentation ~ 1, train)
  full_model <- multinom(Segmentation ~ Gender + Ever_Married + Age + Graduated + Profession + Work_Experience + Spending_Score + Family_Size + Car + Credit_Owner  + Child  + Var_1, train)
  myStepwise <- stepAIC(full_model, scope=list(lower=null_model,upper=full_model), direction="backward", steps=1000, trace=2)
  pred=predict(myStepwise,test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[1]<-sum_accuracies[1]+Accuracy
  AIC_table[1]<-AIC_table[1]+AIC(myStepwise)

  # 2######### Stepwise Logistic model with PCA + All categorical variables
  null_model<-multinom(Segmentation~1, train )
  full_model <- multinom(Segmentation ~ PC1+PC2+PC3+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, train)
  myStepwise <- stepAIC(full_model, scope=list(lower=null_model,upper=full_model), direction="backward", steps=1000, trace=2)
  pred=predict(myStepwise,test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[2]<-sum_accuracies[2]+Accuracy
  AIC_table[2]<-AIC_table[2]+AIC(myStepwise)

  #3######### Stepwise Logistic model with MCA + All numerical variables
  null_model<-multinom(Segmentation~1, train )
  full_model <- multinom(Segmentation ~ MC1+MC2+MC3+Age+Work_Experience+Family_Size+Car+Child, train)
  myStepwise <- stepAIC(full_model, scope=list(lower=null_model,upper=full_model), direction="backward", steps=1000, trace=2)
  pred=predict(myStepwise,test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[3]<-sum_accuracies[3]+Accuracy
  AIC_table[3]<-AIC_table[3]+AIC(myStepwise)

  #4######### Stepwise Logistic model with LDA + All categorical variables
  null_model<-multinom(Segmentation~1, train )
  full_model <- multinom(Segmentation ~ LD1+LD2+LD3+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, train)
  myStepwise <- stepAIC(full_model, scope=list(lower=null_model,upper=full_model), direction="backward", steps=1000, trace=2)
  pred=predict(myStepwise,test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[4]<-sum_accuracies[4]+Accuracy
  AIC_table[4]<-AIC_table[4]+AIC(myStepwise)

}
Accuracy_kfold<-sum_accuracies/k
AIC_table<-AIC_table/k
print(Accuracy_kfold)
print(AIC_table)
Validation_table["Stepwise logistic regressions",]<-Accuracy_kfold

#####----- CROSS VALIDATION OF NAIVE BAYES

set.seed(123)
k<-10
folds<-CreateFolds(Train,k)+1 # In folds, there are number from 1 to k to assign each data a "group"
sum_accuracies<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for (i in 1:k){
  print(i)
  test_ind<-which(folds==i)
  test<-Train_PCA_MCA_LDA[test_ind,]
  train<-Train_PCA_MCA_LDA[-test_ind,]
  train<-outliers_cleaning_stand(train)
  test_num<-Train[test_ind,]
  train_num<-Train[-test_ind,]
  train_num<-outliers_cleaning(train_num)
  lda <- lda(formula = Segmentation ~ Age + Child + Car + Family_Size + Work_Experience, data = train) # LDA is calculated over the training set
  train[,c("LD1","LD2","LD3")]<- predict(lda, train)$x
  test[,c("LD1","LD2","LD3")]<- predict(lda, test)$x

  # 1) model with raw data 2) models with feature selection 3) models with PCA 4) models with MCA 5) models with LDA 6) models PCA, MCA,LDA with feature selection 7) Mixed models
  ########## Raw data  ##########

  #1 : Test with standardized and without

  NBclassifier=naiveBayes(Segmentation~ Age+Work_Experience+Family_Size, data=train_num)
  pred  = predict(NBclassifier, newdata=test_num, type="class")
  confusionMatrix = table(pred, test_num$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[1]<-sum_accuracies[1]+Accuracy

  #2 : Raw data
  NBclassifier=naiveBayes(Segmentation~ Gender+Ever_Married+Age+Graduated+Profession+Work_Experience+Spending_Score+Family_Size+Car+Child+Var_1, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[2]<-sum_accuracies[2]+Accuracy

  #3 : with data without car and child -> correlated
  NBclassifier=naiveBayes(Segmentation~ Gender+Ever_Married+Age+Graduated+Profession+Work_Experience+Spending_Score+Family_Size+Var_1, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[3]<-sum_accuracies[3]+Accuracy

  #4 : correlated variable deleted (child+car) and only categorical variable that are well represented on the two dimention of MCA graph
  NBclassifier=naiveBayes(Segmentation~ Age+Graduated+Profession+Spending_Score+Family_Size, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[4]<-sum_accuracies[4]+Accuracy

  #5 : correlated variable deleted (Family size+car) and only categorical variable that are well represented on the two dimention of MCA graph
  NBclassifier=naiveBayes(Segmentation~ Age+Graduated+Profession+Spending_Score+Child + Credit_Owner, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[5]<-sum_accuracies[5]+Accuracy

  #6 : Feature selection : without license plate, Work_exp, Credit_owner, Child
  NBclassifier=naiveBayes(Segmentation~ Gender + Ever_Married + Age + Graduated + Profession + Spending_Score + Family_Size + Car + Var_1, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[6]<-sum_accuracies[6]+Accuracy

  #7 : Feature selection : without license plate, Work_exp, Credit_owner, Child, Gender and Var_1
  NBclassifier=naiveBayes(Segmentation~ Ever_Married + Age + Graduated + Profession + Spending_Score + Family_Size + Car, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[7]<-sum_accuracies[7]+Accuracy

  #8 : Feature selection : without license plate, Work_exp, Credit_owner, Child, Gender and Var_1 + without car
  NBclassifier=naiveBayes(Segmentation~ Ever_Married + Age + Graduated + Profession + Spending_Score + Family_Size, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[8]<-sum_accuracies[8]+Accuracy

  #### PCA ###

  #9 PC1 + PC2 + All categorical variables
  NBclassifier=naiveBayes(Segmentation~ PC1+PC2+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[9]<-sum_accuracies[9]+Accuracy

  #10 PC1+PC2+PC3 + All categorical variables
  NBclassifier=naiveBayes(Segmentation~ PC1+PC2+PC3+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[10]<-sum_accuracies[10]+Accuracy

  #### MCA ####

  #11  MC1+MC2 + All numerical variables
  NBclassifier=naiveBayes(Segmentation~ MC1+MC2+Age+Work_Experience+Family_Size+Car+Child, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[11]<-sum_accuracies[11]+Accuracy


  #12 MC1+MC2+MC3 + All numerical variables
  NBclassifier=naiveBayes(Segmentation~ MC1+MC2+MC3+Age+Work_Experience+Family_Size+Car+Child, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[12]<-sum_accuracies[12]+Accuracy

  #### LDA ####

  #13 LDA1+LDA2+LDA3
  NBclassifier=naiveBayes(Segmentation~ LD1+LD2+LD3, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[13]<-sum_accuracies[13]+Accuracy

  #14 LDA1+LDA2+LDA3  + All categorical
  NBclassifier=naiveBayes(Segmentation~ LD1+LD2+LD3+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[14]<-sum_accuracies[14]+Accuracy

  #15 PC1+PC2+PC3  + All categorical + taking into account Feature Selection (without Gender, Credit Owner and Var 1)
  NBclassifier=naiveBayes(Segmentation~ PC1+PC2+PC3+Ever_Married+Graduated+Profession+Spending_Score, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[15]<-sum_accuracies[15]+Accuracy

  #16 MC1+MC2+MC3  + All numerical + taking into account Feature Selection (without Gender, Credit Owner and Var 1)
  NBclassifier=naiveBayes(Segmentation~ MC1+MC2+MC3+Age+Family_Size+Car, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[16]<-sum_accuracies[16]+Accuracy

  #17 LDA1+LDA2+LDA3 + All categorical + taking into account Feature Selection (without Gender, Credit Owner and Var 1)
  NBclassifier=naiveBayes(Segmentation~ LD1+LD2+LD3+Ever_Married+Graduated+Profession+Spending_Score, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[17]<-sum_accuracies[17]+Accuracy

  #### Mixed Models ####

  #18  PCA1+PCA2+MCA1+MCA2+MCA3
  NBclassifier=naiveBayes(Segmentation~ PC1+PC2+MC1+MC2+MC3, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[18]<-sum_accuracies[18]+Accuracy

  #19 PCA1+PCA2+MCA1+MCA2+MCA3+LD1+LD2+LD3
  NBclassifier=naiveBayes(Segmentation~ PC1+PC2+MC1+MC2+MC3+LD1+LD2+LD3, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[19]<-sum_accuracies[19]+Accuracy

  #20 : with data without car and child and gender-> correlated
  NBclassifier=naiveBayes(Segmentation~ Ever_Married+Age+Graduated+Profession+Work_Experience+Spending_Score+Family_Size+Var_1, data=train)
  pred  = predict(NBclassifier, newdata=test, type="class")
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[20]<-sum_accuracies[20]+Accuracy
}

Accuracy_kfold<-sum_accuracies/k
print(Accuracy_kfold)
Validation_table["Naive Bayes",]<-Accuracy_kfold

#####----- CROSS VALIDATION OF RANDOM FOREST


set.seed(123)
k<-10
folds<-CreateFolds(Train,k)+1 # In folds, there are number from 1 to k to assign each data a "group"
sum_accuracies<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for (i in 1:k){
  print(i)
  test_ind<-which(folds==i)
  test<-Train_PCA_MCA_LDA[test_ind,]
  train<-Train_PCA_MCA_LDA[-test_ind,]
  train<-outliers_cleaning_stand(train)
  lda <- lda(formula = Segmentation ~ Age + Child + Car + Family_Size + Work_Experience, data = train) # LDA is calculated over the training set
  train[,c("LD1","LD2","LD3")]<- predict(lda, train)$x
  test[,c("LD1","LD2","LD3")]<- predict(lda, test)$x

  # 1) model with raw data 2) models with feature selection 3) models with PCA 4) models with MCA 5) models with LDA 6) Mixed Models

  # 1 : test with all data
  rf<-randomForest(Segmentation~Gender + Ever_Married + Age + Graduated + Profession + Spending_Score + Family_Size + Car + Var_1+Work_Experience+Credit_Owner+Child, data = train, ntree=595, mtry=1, importance = TRUE, proximity = TRUE) # c'est ici qu on à changé des bails (grace a la ligne 131)
  pred<-predict(rf, test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[1]<-sum_accuracies[1]+Accuracy

  # 2 : Feature selection : without license plate, Work_exp, Credit_owner, Child
  rf<-randomForest(Segmentation~Gender + Ever_Married + Age + Graduated + Profession + Spending_Score + Family_Size + Car + Var_1, data = train, ntree=595, mtry=1, importance = TRUE, proximity = TRUE) # c'est ici qu on à changé des bails (grace a la ligne 131)
  pred<-predict(rf, test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[2]<-sum_accuracies[2]+Accuracy

  # 3 : Feature selection : without license plate, Work_exp, Credit_owner, Child, Gender and Var_1
  rf<-randomForest(Segmentation~ Ever_Married + Age + Graduated + Profession + Spending_Score + Family_Size + Car, data = train, ntree=595, mtry=1, importance = TRUE, proximity = TRUE) # c'est ici qu on à changé des bails (grace a la ligne 131)
  pred<-predict(rf, test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[3]<-sum_accuracies[3]+Accuracy

  ### PCA ####

  # 4 PC1 + PC2 + All categorical variables
  rf<-randomForest(Segmentation~PC1+PC2+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, data = train, ntree=595, mtry=1, importance = TRUE, proximity = TRUE) # c'est ici qu on à changé des bails (grace a la ligne 131)
  pred<-predict(rf, test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[4]<-sum_accuracies[4]+Accuracy

  # 5 PC1+PC2+PC3 + All categorical variables
  rf<-randomForest(Segmentation~PC1+PC2+PC3+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, data = train, ntree=595, mtry=1, importance = TRUE, proximity = TRUE) # c'est ici qu on à changé des bails (grace a la ligne 131)
  pred<-predict(rf, test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[5]<-sum_accuracies[5]+Accuracy

  ### MCA ####

  # 6  MC1+MC2 + All numerical variables
  rf<-randomForest(Segmentation~ MC1+MC2+Age+Work_Experience+Family_Size+Car+Child, data = train, ntree=595, mtry=1, importance = TRUE, proximity = TRUE) # c'est ici qu on à changé des bails (grace a la ligne 131)
  pred<-predict(rf, test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[6]<-sum_accuracies[6]+Accuracy

  # 7 MC1+MC2+MC3 + All numerical variables
  rf<-randomForest(Segmentation~MC1+MC2+MC3+Age+Work_Experience+Family_Size+Car+Child, data = train, ntree=595, mtry=1, importance = TRUE, proximity = TRUE) # c'est ici qu on à changé des bails (grace a la ligne 131)
  pred<-predict(rf, test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[7]<-sum_accuracies[7]+Accuracy

  ### LDA ####

  # 8 LDA1+LDA2+LDA3
  rf<-randomForest(Segmentation~LD1+LD2+LD3, data = train, ntree=595, mtry=1, importance = TRUE, proximity = TRUE) # c'est ici qu on à changé des bails (grace a la ligne 131)
  pred<-predict(rf, test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[8]<-sum_accuracies[8]+Accuracy

  # 9 LDA1+LDA2+LDA3  + All categorical
  rf<-randomForest(Segmentation~LD1+LD2+LD3+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, data = train, ntree=595, mtry=1, importance = TRUE, proximity = TRUE) # c'est ici qu on à changé des bails (grace a la ligne 131)
  pred<-predict(rf, test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[9]<-sum_accuracies[9]+Accuracy

  ### Mixed Models ####

  # 10  PCA1+PCA2+  MCA1+MCA2+MCA3
  rf<-randomForest(Segmentation~PC1+PC2+MC1+MC2+MC3, data = train, ntree=595, mtry=1, importance = TRUE, proximity = TRUE) # c'est ici qu on à changé des bails (grace a la ligne 131)
  pred<-predict(rf, test)
  confusionMatrix = table(pred, test$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[10]<-sum_accuracies[10]+Accuracy
}

Accuracy_kfold<-sum_accuracies/k
print(Accuracy_kfold)
Validation_table["Random Forest",]<-Accuracy_kfold


############----------- Bagging ------------

set.seed(123)
k<-10
folds<-CreateFolds(Train,k)+1 # In folds, there are number from 1 to k to assign each data a "group"
sum_accuracies<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for (i in 1:k){
  print(i)
  test_ind<-which(folds==i)
  test_std<-Train_PCA_MCA_LDA[test_ind,]
  train_std<-Train_PCA_MCA_LDA[-test_ind,]
  train_final.bag<-bagging(Segmentation~ PC1+PC2+ PC3 + MC1 + MC2 ,data=train_std ,nbagg=50,mfinal=5)

  pred  = predict(train_final.bag, newdata=test_std, type="class")
  confusionMatrix = table(pred, test_std$Segmentation)
  Accuracy<-(confusionMatrix[1,1]+confusionMatrix[2,2]+confusionMatrix[3,3]+confusionMatrix[4,4])/sum(confusionMatrix)
  sum_accuracies[1]<-sum_accuracies[1]+Accuracy
}

Accuracy_kfold<-sum_accuracies/k
print(Accuracy_kfold)
Validation_table["Bagging",]<-Accuracy_kfold

############----------- X-G Boost Gradient ------------

set.seed(123)
k<-10
folds<-CreateFolds(Train,k)+1 # In folds, there are number from 1 to k to assign each data a "group"
sum_accuracies<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for (i in 1:k){
  print(i)
  test_ind<-which(folds==i)
  test<-Train_PCA_MCA_LDA[test_ind,]
  train<-Train_PCA_MCA_LDA[-test_ind,]

  # NB: We are aware that the model is trained over a cross-validation. However, this method worked well in the for loops juste above and seemed to improve the accuracy. There is no problem in doing that since it has no access to the test set of the for loop.
  tuneGrid <- expand.grid(eta=0.3, max_depth=2, gamma=0, colsample_bytree=0.7, min_child_weight=0.9, subsample=1.0, nrounds=100)
  xg.boost <- train(Segmentation ~Gender+Ever_Married+Age+Graduated+Profession+Work_Experience+Spending_Score+Family_Size+Car+Child+Var_1, data = train, method = "xgbTree", trControl = trainControl("cv", number = 10), tuneGrid=tuneGrid)
  xg.boost$bestTune
  pred <- xg.boost %>% predict(test)
  head(pred)
  confusionMatrix=table(pred, test$Segmentation)
  Accuracy<-mean(pred == test$Segmentation)
  sum_accuracies[1]<-sum_accuracies[1]+Accuracy #48.66

  tuneGrid <- expand.grid(eta=0.3, max_depth=2, gamma=0, colsample_bytree=0.7, min_child_weight=0.9, subsample=1.0, nrounds=100)
  xg.boost <- train(Segmentation ~PC1+PC2+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, data = train, method = "xgbTree", trControl = trainControl("cv", number = 10), tuneGrid=tuneGrid)
  xg.boost$bestTune
  pred <- xg.boost %>% predict(test)
  head(pred)
  confusionMatrix=table(pred, test$Segmentation)
  Accuracy<-mean(pred == test$Segmentation)
  sum_accuracies[2]<-sum_accuracies[2]+Accuracy #47.41

  tuneGrid <- expand.grid(eta=0.3, max_depth=2, gamma=0, colsample_bytree=0.7, min_child_weight=0.9, subsample=1.0, nrounds=100)
  xg.boost <- train(Segmentation ~PC1+PC2+PC3+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, data = train, method = "xgbTree", trControl = trainControl("cv", number = 10), tuneGrid=tuneGrid)
  xg.boost$bestTune
  pred <- xg.boost %>% predict(test)
  head(pred)
  confusionMatrix=table(pred, test$Segmentation)
  Accuracy<-mean(pred == test$Segmentation)
  sum_accuracies[3]<-sum_accuracies[3]+Accuracy #46.88

  tuneGrid <- expand.grid(eta=0.3, max_depth=2, gamma=0, colsample_bytree=0.7, min_child_weight=0.9, subsample=1.0, nrounds=100)
  xg.boost <- train(Segmentation ~LD1+LD2+Gender+Ever_Married+Graduated+Profession+Spending_Score+Credit_Owner+Var_1, data = train, method = "xgbTree", trControl = trainControl("cv", number = 10), tuneGrid=tuneGrid)
  xg.boost$bestTune
  pred <- xg.boost %>% predict(test)
  head(pred)
  confusionMatrix=table(pred, test$Segmentation)
  Accuracy<-mean(pred == test$Segmentation)
  sum_accuracies[4]<-sum_accuracies[4]+Accuracy #47.67

  tuneGrid <- expand.grid(eta=0.3, max_depth=2, gamma=0, colsample_bytree=0.7, min_child_weight=0.9, subsample=1.0, nrounds=100)
  xg.boost <- train(Segmentation ~Age+Graduated+Profession+Spending_Score+Family_Size, data = train, method = "xgbTree", trControl = trainControl("cv", number = 10), tuneGrid=tuneGrid)
  xg.boost$bestTune
  pred <- xg.boost %>% predict(test)
  head(pred)
  confusionMatrix=table(pred, test$Segmentation)
  Accuracy<-mean(pred == test$Segmentation)
  sum_accuracies[5]<-sum_accuracies[5]+Accuracy #48.98

  tuneGrid <- expand.grid(eta=0.3, max_depth=2, gamma=0, colsample_bytree=0.7, min_child_weight=0.9, subsample=1.0, nrounds=100)
  xg.boost <- train(Segmentation ~Age+Graduated+Profession+Spending_Score+Family_Size+Ever_Married+Credit_Owner, data = train, method = "xgbTree", trControl = trainControl("cv", number = 10), tuneGrid=tuneGrid)
  xg.boost$bestTune
  pred <- xg.boost %>% predict(test)
  head(pred)
  confusionMatrix=table(pred, test$Segmentation)
  Accuracy<-mean(pred == test$Segmentation)
  sum_accuracies[6]<-sum_accuracies[6]+Accuracy #49.12

  tuneGrid <- expand.grid(eta=0.3, max_depth=2, gamma=0, colsample_bytree=0.7, min_child_weight=0.9, subsample=1.0, nrounds=100)
  xg.boost <- train(Segmentation ~Age+Graduated+Profession+Spending_Score+Family_Size+Ever_Married, data = train, method = "xgbTree", trControl = trainControl("cv", number = 10), tuneGrid=tuneGrid)
  xg.boost$bestTune
  pred <- xg.boost %>% predict(test)
  head(pred)
  confusionMatrix=table(pred, test$Segmentation)
  Accuracy<-mean(pred == test$Segmentation)
  sum_accuracies[7]<-sum_accuracies[7]+Accuracy #48.77
}
Accuracy_kfold<-sum_accuracies/k
print(Accuracy_kfold)
Validation_table["X-G Boost",]<-Accuracy_kfold







print(Validation_table)
print("Best tree : ")
print(which(Validation_table["Decision trees",] == max(Validation_table["Decision trees",]), arr.ind = TRUE))
print("Best performance : ")
print(which(Validation_table["Normal logistic regressions",] == max(Validation_table["Normal logistic regressions",]), arr.ind = TRUE))
print("Best Naive Bayes : ")
print(which(Validation_table["Naive Bayes",] == max(Validation_table["Naive Bayes",]), arr.ind = TRUE))
print("Best performance : ")
print(which(Validation_table["Bagging",] == max(Validation_table["Bagging",]), arr.ind = TRUE))
print("Best performance : ")
print(which(Validation_table["X-G Boost",] == max(Validation_table["X-G Boost",]), arr.ind = TRUE))
print("Best performance : ")
print(which(Validation_table == max(Validation_table), arr.ind = TRUE))





# If you don't want to run the cross-validation since it is quite long, the extended results are the following :

results_decision_trees <- c(0.4487450, 0.4507085, 0.4507085, 0.4565950 ,0.4546315 ,0.3807338, 0.4487450 ,0.4565950, 0.4565950 ,0.4565950, 0.3797508, 0.4421539, 0.4421539, 0.4240633, 0.4488807, 0.3615229,0.4361221,0.4206982, 0.4316403)
results_normal_logistic <- c(0.4693613, 0.4659953, 0.4643111, 0.4717438 ,0.4693601, 0.4504286, 0.4536548, 0.4655759, 0.4675390, 0.4641706, 0.4638911, 0.4512713, 0.4561798, 0.4594028, 0.4615062, 0.4571613 ,0.4537949)
results_stepwise_regression<- c(0.4693609, 0.4688009 ,0.4533747, 0.4662764)
results_naive_bayes<-c(0.4132682, 0.4645939, 0.4631922 ,0.4627693, 0.4673978, 0.4634729 ,0.4584246, 0.4610886 ,0.4650145 ,0.4640331, 0.4372500, 0.4477677, 0.4086422, 0.4599681, 0.4605266, 0.4530950, 0.4575833, 0.4505700 ,0.4415962 ,0.4623505)
results_random_forest <- c(0.4651547, 0.4668372 ,0.4676771, 0.4703413, 0.4666957, 0.4683774, 0.4716022, 0.3661514 ,0.4669766, 0.4455170)
results_bagging<-c(0.4215421)
results_xgboost<-c(0.4881530, 0.4794571, 0.4746903, 0.4804395, 0.4888525, 0.4860486, 0.4892754)






##################################### PREDICTION ########################
Train_for_pred<-copy(Train)
Test<-read.csv("/Users/geraudtserstevens/Desktop/Market_segmentation/TestStudent.csv", TRUE, sep = ",")
Test$Licence_Plate<-NULL
Test$Age<-as.numeric(Test$Age)
Test$Work_Experience<-as.numeric(Test$Work_Experience)
Test$Family_Size<-as.numeric(Test$Family_Size)
Test$Car<-as.numeric(Test$Car)
Test$Child<-as.numeric(Test$Child)
Test$Gender<-as.factor(Test$Gender)
Test$Ever_Married<-as.factor(Test$Ever_Married)
Test$Graduated<-as.factor(Test$Graduated)
Test$Profession<-as.factor(Test$Profession)
Test$Spending_Score<-as.factor(Test$Spending_Score)
Test$Credit_Owner<-as.factor(Test$Credit_Owner)
Test$Var_1<-as.factor(Test$Var_1)

# The test set is transformed the same way the training set was transformed
if (!is.null(Test$Work_Experience)){
  for(j in 1:length(Test$Work_Experience)){
    if(Test$Work_Experience[j]>0)
      Test$Work_Experience[j]<-log10(Test$Work_Experience[j])
  }
}
if (!is.null(Test$Family_Size)){
  for(j in 1:length(Test$Family_Size)){
    if(Test$Family_Size[j]>0)
      Test$Family_Size[j]<-log10(Test$Family_Size[j])
  }
}
if (!is.null(Test$Child)){
  for(j in 1:length(Test$Child)){
    if(Test$Child[j]>0)
      Test$Child[j]<-log10(Test$Child[j])
  }
}
if (!is.null(Test$Age)){
  for(j in 1:length(Test$Age)){
    if(Test$Age[j]>0)
      Test$Age[j]<-log10(Test$Age[j])
  }
}
if (!is.null(Test$Car)){
  for(j in 1:length(Test$Car)){
    if(Test$Car[j]>0)
      Test$Car[j]<-log10(Test$Car[j])
  }
}


# The datasets are standardized with the highest amount of information : Train + Test !

WHOLE_DATA<-rbind(Train[,c("Age","Work_Experience","Family_Size","Car","Child","Gender","Ever_Married","Graduated","Profession","Spending_Score","Credit_Owner","Var_1")],Test)

Train_for_pred$Age <- (Train_for_pred$Age - mean(WHOLE_DATA$Age)) / sd(WHOLE_DATA$Age)
Train_for_pred$Work_Experience <- (Train_for_pred$Work_Experience - mean(WHOLE_DATA$Work_Experience)) / sd(WHOLE_DATA$Work_Experience)
Train_for_pred$Family_Size <- (Train_for_pred$Family_Size - mean(WHOLE_DATA$Family_Size)) / sd(WHOLE_DATA$Family_Size)
Train_for_pred$Car <- (Train_for_pred$Car - mean(WHOLE_DATA$Car)) / sd(WHOLE_DATA$Car)
Train_for_pred$Child <- (Train_for_pred$Child - mean(WHOLE_DATA$Child)) / sd(WHOLE_DATA$Child)

Test$Age <- (Test$Age - mean(WHOLE_DATA$Age)) / sd(WHOLE_DATA$Age)
Test$Work_Experience <- (Test$Work_Experience - mean(WHOLE_DATA$Work_Experience)) / sd(WHOLE_DATA$Work_Experience)
Test$Family_Size <- (Test$Family_Size - mean(WHOLE_DATA$Family_Size)) / sd(WHOLE_DATA$Family_Size)
Test$Car <- (Test$Car - mean(WHOLE_DATA$Car)) / sd(WHOLE_DATA$Car)
Test$Child <- (Test$Child - mean(WHOLE_DATA$Child)) / sd(WHOLE_DATA$Child)


set.seed(123)
tuneGrid <- expand.grid(eta=0.3, max_depth=2, gamma=0, colsample_bytree=0.7, min_child_weight=0.9, subsample=1.0, nrounds=100)
xg.boost <- train(Segmentation ~ Age+Graduated+Profession+Spending_Score+Family_Size+Ever_Married, data = Train_for_pred, method = "xgbTree", trControl = trainControl("cv", number = 10), tuneGrid=tuneGrid) # NB: We are aware that the model is trained over a cross-validation. However, this method worked well in the for loops juste above and seemed to improve the accuracy.
pred <- xg.boost %>% predict(Test)

table(pred)
pred <- as.data.frame(pred)
View(pred)

Test$Segmentation <- pred$pred

View(Test)
names(pred)<-NULL
write.csv(pred,"/Users/geraudtserstevens/Desktop/Market_segmentation/Predictions.csv", row.names = FALSE)





# PLOTS -------------------------------------------------------------------


# --- PLOTS

#age-gender
ggplot(Train, aes(x = Age, fill = Gender)) +
  geom_bar(width=0.5) +
  xlab(label="Age") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#age-ever_married
ggplot(Train, aes(x = Age, fill = Ever_Married)) +
  geom_bar(width=0.5) +
  xlab(label="Age") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#age-graduated
ggplot(Train, aes(x = Age, fill = Graduated)) +
  geom_bar(width=0.5) +
  xlab(label="Age") +
  ylab(label="Total COunt") +
  theme(legend.position = c(0.8,0.8))

#age-profession
ggplot(Train, aes(x = Age, fill = Profession)) +
  geom_bar(width=0.5) +
  xlab(label="Age") +
  ylab(label="Total count") +
  theme(legend.position = c(0.8,0.8))

#age-spending_score
ggplot(Train, aes(x = Age, fill = Spending_Score)) +
  geom_bar(width=0.5) +
  xlab(label="Age") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#age-car
ggplot (Train, aes(x = Age,y = Car, fill = Car, colour = Car)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Age") +
  ylab(label = "Car") +
  theme_classic()

#age-credit_owner
ggplot(Train, aes(x = Age, fill = Credit_Owner)) +
  geom_bar(width=0.5) +
  xlab(label="Age") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Ever_Married-Graduated
ggplot(Train, aes(x = Ever_Married, fill = Graduated)) +
  geom_bar(width=0.5) +
  xlab(label="Ever_Married") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Ever_Married-Profession
ggplot(Train, aes(x = Ever_Married, fill = Profession)) +
  geom_bar(width=0.5) +
  xlab(label="Ever_Married") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Ever_Married-Spending_Score
ggplot(Train, aes(x = Ever_Married, fill = Spending_Score)) +
  geom_bar(width=0.5) +
  xlab(label="Ever_Married") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Ever_Married-Credit_Owner
ggplot(Train, aes(x = Ever_Married, fill = Credit_Owner)) +
  geom_bar(width=0.5) +
  xlab(label="Ever_Married") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Ever_Married-Car
ggplot (Train, aes(x = Ever_Married,y = Car, fill = Car, colour = Car)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Ever_Married") +
  ylab(label = "Car") +
  theme_classic()

#Graduated-Profession
ggplot(Train, aes(x = Graduated, fill = Profession)) +
  geom_bar(width=0.5) +
  xlab(label="Graduated") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Graduated-Spending_Score
ggplot(Train, aes(x = Graduated, fill = Spending_Score)) +
  geom_bar(width=0.5) +
  xlab(label="Graduated") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Graduated-Car
ggplot (Train, aes(x = Graduated,y = Car, fill = Car, colour = Car)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Graduated") +
  ylab(label = "Car") +
  theme_classic()

#Profession-Spending_Score
ggplot(Train, aes(x = Profession, fill = Spending_Score)) +
  geom_bar(width=0.5) +
  xlab(label="Profession") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Spending_Score-Family_Size
ggplot (Train, aes(x = Spending_Score,y = Family_Size, fill = Family_Size, colour = Family_Size)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Spending_Score") +
  ylab(label = "Family_Size") +
  theme_classic()

#Spending_Score-Car
ggplot (Train, aes(x = Spending_Score,y = Car, fill = Car, colour = Car)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Spending_Score") +
  ylab(label = "Car") +
  theme_classic()

#Spending_Score-Child
ggplot (Train, aes(x = Spending_Score,y = Child, fill = Child, colour = Child)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Spending_Score") +
  ylab(label = "Child") +
  theme_classic()

#Family_Size-Credit_Owner
ggplot(Train, aes(x = Family_Size, fill = Credit_Owner)) +
  geom_bar(width=0.5) +
  xlab(label="Family_Size") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Family_Size-Car
ggplot (Train, aes(x = Family_Size,y = Car, fill = Car, colour = Car)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Family_Size") +
  ylab(label = "Car") +
  theme_classic()


#Family_Size-Child
ggplot (Train, aes(x = Family_Size,y = Child, fill = Child, colour = Child)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Family_Size") +
  ylab(label = "Child") +
  theme_classic()

#Car-Child
ggplot (Train, aes(x = Car,y = Child, fill = Child, colour = Child)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Car") +
  ylab(label = "Child") +
  theme_classic()

#Child-Credit_Owner
ggplot(Train, aes(x = Child, fill = Credit_Owner)) +
  geom_bar(width=0.5) +
  xlab(label="Child") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))



# All explanatory variables with the dependant variable

#Age-Segmentation
ggplot(Train, aes(x = Age, fill = Segmentation)) +
  geom_bar(width=0.5) +
  xlab(label="Age") +
  ylab(label="Total Count") +
  labs(fill="Segmentation")

#Gender-Segmentation
ggplot(Train, aes(x = Segmentation, fill = Gender)) +
  geom_bar(width=0.5) +
  xlab(label="Segmentation") +
  ylab(label="Total Count") +
  ggtitle("Histogram Segmentation-Gender") +
  labs(fill="Gender")

#Profession-Segmentation
ggplot(Train, aes(x = Segmentation, fill = Profession)) +
  geom_bar(width=0.5) +
  xlab(label="Segmentation") +
  ylab(label="Total Count") +
  ggtitle("Histogram Segmentation-Profession") +
  labs(fill="Profession")

#Work_Experience-Segmentation
ggplot(Train, aes(x = Work_Experience, fill = Segmentation)) +
  geom_bar(width=0.5) +
  xlab(label="Work experience") +
  ylab(label="Total Count") +
  ggtitle("Histogram Segmentation-Work experience") +
  labs(fill="Segmentation")

#Spending_Score-Segmentation
ggplot(Train, aes(x = Spending_Score, fill = Segmentation)) +
  geom_bar(width=0.5) +
  xlab(label="Spending score") +
  ylab(label="Total Count") +
  ggtitle("Histogram Segmentation-Spending score") +
  labs(fill="Segmentation")

#Family_Size-Segmentation
ggplot(Train, aes(x = Family_Size, fill = Segmentation)) +
  geom_bar(width=0.5) +
  xlab(label="Family size") +
  ylab(label="Total Count") +
  ggtitle("Histogram Segmentation-Family size") +
  labs(fill="Segmentation")

#Car-Segmentation
ggplot(Train, aes(x = Car, fill = Segmentation)) +
  geom_bar(width=0.5) +
  xlab(label="Car") +
  ylab(label="Total Count") +
  ggtitle("Histogram Segmentation-Car") +
  labs(fill="Segmentation")

#Child-Segmentation
ggplot(Train, aes(x = Child, fill = Segmentation)) +
  geom_bar(width=0.5) +
  xlab(label="Child") +
  ylab(label="Total Count") +
  ggtitle("Histogram Segmentation-Child") +
  labs(fill="Segmentation")

#Var_1-Segmentation
ggplot(Train, aes(x = Segmentation, fill = Var_1)) +
  geom_bar(width=0.5) +
  xlab(label="Segmentation") +
  ylab(label="Total Count") +
  ggtitle("Histogram Segmentation-Var 1") +
  labs(fill="Var 1")

#Age-Work_experience
ggplot (Train, aes(x = Age,y = Work_Experience, fill = Work_Experience, colour = Work_Experience)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Age") +
  ylab(label = "Work_Experience") +
  theme_classic()

#Age-Var_1
ggplot(Train, aes(x = Age, fill = Var_1)) +
  geom_bar(width=0.5) +
  xlab(label="Age") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#age-child
ggplot (Train, aes(x = Age,y = Child, fill = Child, colour = Child)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Age") +
  ylab(label = "child") +
  theme_classic()

#Gender-Ever_Married
ggplot(Train, aes(x = Gender, fill = Ever_Married)) +
  geom_bar(width=0.5) +
  xlab(label="Gender") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Gender-Graduated
ggplot(Train, aes(x = Gender, fill = Graduated)) +
  geom_bar(width=0.5) +
  xlab(label="Gender") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Gender-Profession
ggplot(Train, aes(x = Gender, fill = Profession)) +
  geom_bar(width=0.5) +
  xlab(label="Gender") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Gender-Work_Experience
ggplot (Train, aes(x = Gender,y = Work_Experience, fill = Work_Experience, colour = Work_Experience)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Gender") +
  ylab(label = "Work_Experience") +
  theme_classic()


#Gender-Spending_Score
ggplot(Train, aes(x = Gender, fill = Spending_Score)) +
  geom_bar(width=0.5) +
  xlab(label="Gender") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Gender-Var_1
ggplot(Train, aes(x = Gender, fill = Var_1)) +
  geom_bar(width=0.5) +
  xlab(label="Gender") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Gender-Credit_Owner
ggplot(Train, aes(x = Gender, fill = Credit_Owner)) +
  geom_bar(width=0.5) +
  xlab(label="Gender") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Gender-Family_Size
ggplot (Train, aes(x = Gender,y = Family_Size, fill = Family_Size, colour = Family_Size)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Gender") +
  ylab(label = "Family_Size") +
  theme_classic()

#Gender-Car
ggplot (Train, aes(x = Gender,y = Car, fill = Car, colour = Car)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Gender") +
  ylab(label = "Car") +
  theme_classic()

#Gender-Child
ggplot (Train, aes(x = Gender,y = Child, fill = Child, colour = Child)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Gender") +
  ylab(label = "Child") +
  theme_classic()

#Ever_Married-Var_1
ggplot(Train, aes(x = Ever_Married, fill = Var_1)) +
  geom_bar(width=0.5) +
  xlab(label="Ever_Married") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Ever_Married-Work_Experience
ggplot (Train, aes(x = Ever_Married,y = Work_Experience, fill = Work_Experience, colour = Work_Experience)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Ever_Married") +
  ylab(label = "Work_Experience") +
  theme_classic()

#Ever_Married-Family_Size
ggplot (Train, aes(x = Ever_Married,y = Family_Size, fill = Family_Size, colour = Family_Size)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Ever_Married") +
  ylab(label = "Family_Size") +
  theme_classic()

#Ever_Married-Child
ggplot (Train, aes(x = Ever_Married,y = Child, fill = Child, colour = Child)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Ever_Married") +
  ylab(label = "Child") +
  theme_classic()

#Graduated-Var_1
ggplot(Train, aes(x = Graduated, fill = Var_1)) +
  geom_bar(width=0.5) +
  xlab(label="Graduated") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Graduated-Credit_Owner
ggplot(Train, aes(x = Graduated, fill = Credit_Owner)) +
  geom_bar(width=0.5) +
  xlab(label="Graduated") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Graduated-Work_Experience
ggplot (Train, aes(x = Graduated,y = Work_Experience, fill = Work_Experience, colour = Work_Experience)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Graduated") +
  ylab(label = "Work_Experience") +
  theme_classic()

#Graduated-Family_Size
ggplot (Train, aes(x = Graduated,y = Family_Size, fill = Family_Size, colour = Family_Size)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Graduated") +
  ylab(label = "Family_Size") +
  theme_classic()

#Graduated-Child
ggplot (Train, aes(x = Graduated,y = Child, fill = Child, colour = Child)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Graduated") +
  ylab(label = "Child") +
  theme_classic()

#Profession-Var_1
ggplot(Train, aes(x = Profession, fill = Var_1)) +
  geom_bar(width=0.5) +
  xlab(label="Profession") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Profession-Credit_Owner
ggplot(Train, aes(x = Profession, fill = Credit_Owner)) +
  geom_bar(width=0.5) +
  xlab(label="Profession") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Profession-Work_Experience
ggplot (Train, aes(x = Profession,y = Work_Experience, fill = Work_Experience, colour = Work_Experience)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Profession") +
  ylab(label = "Work_Experience") +
  theme_classic()

#Profession-Family_Size
ggplot (Train, aes(x = Profession,y = Family_Size, fill = Family_Size, colour = Family_Size)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Profession") +
  ylab(label = "Family_Size") +
  theme_classic()

#Profession-Car
ggplot (Train, aes(x = Profession,y = Car, fill = Car, colour = Car)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Profession") +
  ylab(label = "Car") +
  theme_classic()

#Profession-Child
ggplot (Train, aes(x = Profession,y = Child, fill = Child, colour = Child)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Profession") +
  ylab(label = "Child") +
  theme_classic()

#Work_Experience-Spending_Score
ggplot(Train, aes(x = Work_Experience, fill = Spending_Score)) +
  geom_bar(width=0.5) +
  xlab(label="Work_Experience") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Work_Experience-Var_1
ggplot(Train, aes(x = Work_Experience, fill = Var_1)) +
  geom_bar(width=0.5) +
  xlab(label="Work_Experience") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Work_Experience-Credit_Owner
ggplot(Train, aes(x = Work_Experience, fill = Credit_Owner)) +
  geom_bar(width=0.5) +
  xlab(label="Work_Experience") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))


#Work_Experience-Family_Size
ggplot (Train, aes(x = Work_Experience,y = Family_Size, fill = Family_Size, colour = Family_Size)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Work_Experience") +
  ylab(label = "Family_Size") +
  theme_classic()

#Work_Experience-Car
ggplot (Train, aes(x = Work_Experience,y = Car, fill = Car, colour = Car)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Work_Experience") +
  ylab(label = "Car") +
  theme_classic()

#Work_Experience-Child
ggplot (Train, aes(x = Work_Experience,y = Child, fill = Child, colour = Child)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  xlab(label = "Work_Experience") +
  ylab(label = "Child") +
  theme_classic()

#Spending_Score-Var_1
ggplot(Train, aes(x = Spending_Score, fill = Var_1)) +
  geom_bar(width=0.5) +
  xlab(label="Spending_Score") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Spending_Score-Credit_Owner
ggplot(Train, aes(x = Spending_Score, fill = Credit_Owner)) +
  geom_bar(width=0.5) +
  xlab(label="Spending_Score") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Family_Size-Var_1
ggplot(Train, aes(x = Family_Size, fill = Var_1)) +
  geom_bar(width=0.5) +
  xlab(label="Family_Size") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Car-Var_1
ggplot(Train, aes(x = Car, fill = Var_1)) +
  geom_bar(width=0.5) +
  xlab(label="Car") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Child-Var_1
ggplot(Train, aes(x = Child, fill = Var_1)) +
  geom_bar(width=0.5) +
  xlab(label="Child") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Child-Credit_Owner
ggplot(Train, aes(x = Child, fill = Credit_Owner)) +
  geom_bar(width=0.5) +
  xlab(label="Child") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))

#Var_1-Credit_Owner
ggplot(Train, aes(x = Var_1, fill = Credit_Owner)) +
  geom_bar(width=0.5) +
  xlab(label="Var_1") +
  ylab(label="Total Count") +
  theme(legend.position = c(0.8,0.8))
