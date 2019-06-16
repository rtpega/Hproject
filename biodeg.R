
##"QSAR biodegradation"

# Load packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")



#Load data

#dl<-read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/00254/biodeg.csv"), sep=";",header=FALSE)

url <- "https://github.com/rtpega/Hproject/blob/master/biodeg.csv?raw=true"
dl <- read.csv(url,sep=";",header=FALSE)

#Since the file has no header, we will assign the names to each variable


names(dl)<-c("SpMax_L","J_Dz","nHM","F01_N_N","F04_C_N","NssssC","nCb_","C_percent","nCp","nO","F03_C_N","SdssC","HyWi_B","LOC","SM6_L","F03_C_O","Me","Mi","nN_N","nArNO2","nCRX3","SpPosA_B","nCIR","B01_C_Br","B03_C_Cl","N_073","SpMax_A","Psi_i_1d","B04_C_Br","SdO","TI2_L","nCrt","C_026","F02_C_N","nHDon","SpMax_B","Psi_i_A","nN","SM6_B","nArCOOR","nX","Eclass")

##Dimensions


dim(dl)


##Structure

tr(dl)

##Dependent variable distribution


y <- dl$Eclass
cbind(freq=table(y), percentage=prop.table(table(y))*100)


##Summarize Data

summary(dl)

#DATA VISUALIZATION

# display the correlation matrix
correlations <- cor(dl[,1:41])

corrplot(correlations, order = "hclust", type = "upper",diag=TRUE,tl.pos = "td",tl.col = "black",tl.cex = 0.6, tl.srt = 90, sig.level = 0.01, insig = "blank")


#Pearson correlation between Eclass and the features

correlatinons2<-cor(cbind(dl[,1:41],Eclass=ifelse(dl$Eclass=='RB',1,0)))

Eclass_cor<-correlatinons2[-42,42]

print(Eclass_cor)

#Boxplot correlated variables with Eclass
p1<-ggplot(dl, aes(x=Eclass, y=SpMax_L)) + geom_boxplot()

p2<-ggplot(dl, aes(x=Eclass, y=SpPosA_B)) + geom_boxplot()

p3<-ggplot(dl, aes(x=Eclass, y=C_percent)) + geom_boxplot()

grid.arrange(p1, p2,p3, nrow = 1)



#Boxplot uncorrelated variables with Eclass
p1<-ggplot(dl, aes(x=Eclass, y=Me)) + geom_boxplot()

p2<-ggplot(dl, aes(x=Eclass, y=F03_C_O)) + geom_boxplot()

p3<-ggplot(dl, aes(x=Eclass, y=Mi)) + geom_boxplot()

p4<-ggplot(dl, aes(x=Eclass, y=J_Dz)) + geom_boxplot()

grid.arrange(p1, p2,p3,p4, nrow = 1)


#DATA CLEAN

#Checking for NA values

sapply(dl,function(x)(sum(is.na(x))))


#RESULTS


#Splitting the data into training and test dataset
set.seed(727)
trainIndex <- createDataPartition(dl$Eclass, p=0.70, list=FALSE)
dataTrain <- dl[ trainIndex,]
dataTest <- dl[-trainIndex,]


#Frequency of each class of the dependant variable in the trainin set
y0 <- dataTrain$Eclass
cbind(freq=table(y), percentage=prop.table(table(y))*100)

#10-fold cross-validation with 3 repeats 
set.seed(727)
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3) 


##Basic models

#Knn
set.seed(727)
fit.knn_1 <- train(Eclass ~ ., method = "knn", data =dataTrain, metric="Accuracy",trControl=trainControl)

#LDA
set.seed(727)
fit.lda_1 <- train(Eclass ~ ., method = "lda", data =dataTrain, metric="Accuracy",trControl=trainControl)

#GLMNET 
set.seed(727)
fit.glmnet_1 <- train(Eclass ~ ., method = "glmnet", data =dataTrain,metric="Accuracy",trControl=trainControl)

#CART

set.seed(727)
fit.cart_1 <- train(Eclass ~ ., method = "rpart", data =dataTrain, metric="Accuracy",trControl=trainControl)

#Naive Bayes


set.seed(727)
fit.nb_1 <- train(Eclass ~ ., method = "nb", data =dataTrain, metric="Accuracy",trControl=trainControl)


#SVM Radial


set.seed(727)
fit.svm_radial_1 <- train(Eclass ~ ., method = "svmRadial", data =dataTrain, metric="Accuracy",trControl=trainControl)


#SVM Linear


set.seed(727)
fit.svm_linear_1 <- train(Eclass ~ ., method = "svmLinear", data =dataTrain, metric="Accuracy",trControl=trainControl)

#Comparing results of the different algorithms.  

results_1 <- resamples(list(LDA_1=fit.lda_1, GLMNET_1=fit.glmnet_1, KNN_1=fit.knn_1, CART_1=fit.cart_1, NB_1=fit.nb_1, SVM_linear_1=fit.svm_linear_1, SVM_radial_1=fit.svm_radial_1))
summary(results_1) 
dotplot(results_1)


##Applying transformations


#Knn
set.seed(727)
fit.knn_2 <- train(Eclass ~ ., method = "knn", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("range"))

#LDA
set.seed(727)
fit.lda_2 <- train(Eclass ~ ., method = "lda", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("range"))

#GLMNET 
set.seed(727)
fit.glmnet_2 <- train(Eclass ~ ., method = "glmnet", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("range"))

#CART

set.seed(727)
fit.cart_2 <- train(Eclass ~ ., method = "rpart", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("range"))

#Naive Bayes


set.seed(727)
fit.nb_2 <- train(Eclass ~ ., method = "nb", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("range"))


#SVM Radial


set.seed(727)
fit.svm_radial_2 <- train(Eclass ~ ., method = "svmRadial", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("range"))


#SVM Linear


set.seed(727)
fit.svm_linear_2 <- train(Eclass ~ ., method = "svmLinear", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("range"))

#Comparing results of the different algorithms.

results_2 <- resamples(list(LDA_2=fit.lda_2, GLMNET_2=fit.glmnet_2, KNN_2=fit.knn_2, CART_2=fit.cart_2, NB_2=fit.nb_2, SVM_linear_2=fit.svm_linear_2, SVM_radial_2=fit.svm_radial_2))
summary(results_2) 
dotplot(results_2)

##PCA

#Knn
set.seed(727)
fit.knn_3 <- train(Eclass ~ ., method = "knn", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("center", "scale", "pca"))

#LDA
set.seed(727)
fit.lda_3 <- train(Eclass ~ ., method = "lda", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("center", "scale", "pca"))

#GLMNET 
set.seed(727)
fit.glmnet_3 <- train(Eclass ~ ., method = "glmnet", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("center", "scale", "pca"))

#CART

set.seed(727)
fit.cart_3 <- train(Eclass ~ ., method = "rpart", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("center", "scale", "pca"))

#Naive Bayes


set.seed(727)
fit.nb_3 <- train(Eclass ~ ., method = "nb", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("center", "scale", "pca"))


#SVM Radial


set.seed(727)
fit.svm_radial_3 <- train(Eclass ~ ., method = "svmRadial", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("center", "scale", "pca"))


#SVM Linear


set.seed(727)
fit.svm_linear_3 <- train(Eclass ~ ., method = "svmLinear", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("center", "scale", "pca"))

#Comparing results of the different algorithms.

results_3 <- resamples(list(LDA_3=fit.lda_3, GLMNET_3=fit.glmnet_3, KNN_3=fit.knn_3, CART_3=fit.cart_3, NB_3=fit.nb_3, SVM_linear_3=fit.svm_linear_3, SVM_radial_3=fit.svm_radial_3))
summary(results_3) 
dotplot(results_3)

##Tuning algorithms


#SVM Radial
grid <- expand.grid(sigma =seq(0.05,0.1,0.01), C = seq(0.9,1.4,0.1))

set.seed(727)
fit.svm_radial_4 <- train(Eclass ~ ., method = "svmRadial", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("range"),tuneGrid = grid, tuneLength = 10)

print("SVM RADIAL")
plot(fit.svm_radial_4)


#GLMNET 

grid2 <- expand.grid(alpha = seq(0.001,0.4,0.01), lambda = seq(0.001, 0.015, length = 100))

set.seed(727)
fit.glmnet_4 <- train(Eclass ~ ., method = "glmnet", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("range"),tuneGrid = grid2)


results_4 <- resamples(list( GLMNET_4=fit.glmnet_4, SVM_radial_4=fit.svm_radial_4))
print("GLMNET")
plot(fit.glmnet_4)


summary(results_4) 
dotplot(results_4)

#Ensemble methods

set.seed(727)
fit.rf <- train(Eclass ~ ., method = "rf", data =dataTrain, metric="Accuracy",

trControl=trainControl,preProc=c("BoxCox"))

fit.treebag<- train(Eclass ~ ., method = "treebag", data =dataTrain, metric="Accuracy",
trControl=trainControl,preProc=c("BoxCox"))




results_5 <- resamples(list( RF=fit.rf, TREEBAG=fit.treebag))

#Comparing results of the different algorithms.

summary(results_5) 

##Validation

#Prediccion of SVM Radial and Confusion matrix

pred.svm_radial_4 <- predict(fit.svm_radial_4, newdata=dataTest) 
confusionMatrix(pred.svm_radial_4, dataTest$Eclass)

#Variable importance
var_imp<-varImp(fit.svm_radial_4, scale = TRUE)

plot(var_imp,top = 25)

# Models correlation

results<-resamples(list(SVM_rad_4=fit.svm_radial_1,SVM_lin_1=fit.svm_linear_1,RF=fit.rf,GLMNET_4=fit.glmnet_4,TREEBAG=fit.treebag,KNN_2=fit.knn_2,LDA_1=fit.lda_1,NB_3=fit.nb_3,CART_3=fit.cart_3))
modelCor(results)

corrplot(modelCor(results))

#Stacking (mayority vote): SVM Radial, treebag and CART


pred.svm_radial_4 <- predict(fit.svm_radial_4, newdata=dataTest) 
pred.treebag <- predict(fit.treebag, newdata=dataTest) 
pred.cart_3 <- predict(fit.cart_3, newdata=dataTest) 

prediccion<-as.factor(ifelse(pred.svm_radial_4=='NRB' & pred.treebag=='NRB','NRB',ifelse(pred.svm_radial_4=='NRB' & pred.cart_3=='NRB','NRB',ifelse(pred.treebag=='NRB' & pred.cart_3=='NRB','NRB','RB'))))

confusionMatrix(prediccion, dataTest$Eclass)

#Stacking (mayority vote): Random Forest, treebag and glmnet

pred.rf <- predict(fit.rf, newdata=dataTest) 
pred.treebag <- predict(fit.treebag, newdata=dataTest) 
pred.glmnet_4 <- predict(fit.glmnet_4, newdata=dataTest) 

prediccion<-as.factor(ifelse(pred.rf=='NRB' & pred.treebag=='NRB','NRB',ifelse(pred.rf=='NRB' & pred.glmnet_4=='NRB','NRB',ifelse(pred.treebag=='NRB' & pred.glmnet_4=='NRB','NRB','RB'))))

confusionMatrix(prediccion, dataTest$Eclass)

