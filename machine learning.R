training<-read.csv("pml-training.csv",sep=",",header=TRUE,na.strings=c("NA",""),stringsAsFactors=FALSE,as.is=TRUE)
training$classe <- as.factor(training$classe)
training <- training[,-nearZeroVar(training)]
training <- training[,-c(1,2,3,4,5,6,7)]
inTrain <- createDataPartition(y=training$classe, p=0.75, list=FALSE)
training <- training[inTrain,]
testing <- training[-inTrain,]
training<-training[,!sapply(training,is.character)]
pre<-preProcess(training[,-length(training)],method=c("center", "scale", "knnImpute", "pca"), thresh=0.95)
clean_data <- predict(pre,training[,-length(training)])
model<-train(training$classe ~.,data=clean_data, method="knn")
test <-predict(pre, testing[,-length(testing)])
confusionMatrix(testing$classe, predict(model,test))