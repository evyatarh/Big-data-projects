#Exercise 3 - Big Data
#Evyatar Hai - 203784939

library(data.table)
library(dplyr)
library(ggplot2)
library(e1071)
library(caret)
library(party)
library(kernlab)
library(FactoMineR)

pulsars = fread("HTRU_2.csv")
pulsars = as.data.table(pulsars)
names(pulsars)[9] = "Class"
pulsars$Class = as.factor(pulsars$Class)

# Section 1
pulsars.indexes = createDataPartition(pulsars$Class, p=0.6,list=FALSE)
# took the 60% for the training
training.set.pulsars = pulsars[pulsars.indexes,]
two.groups.pulsars = pulsars[-pulsars.indexes,]
# from the 40% left, lets devide it to 50%-50% for the testing and the validation
pulsars.indexes = createDataPartition(two.groups.pulsars$Class, p=0.5, list=FALSE)
# 20% for the testing
testing.set.pulsars = two.groups.pulsars[pulsars.indexes]
# 20% for the validation
validating.set.pulsars = two.groups.pulsars[-pulsars.indexes]

# Section 2
# By the graph, we can see that V3 & V4 are in strong coorelation and effected the
# most on dimension 1 (51.71%).
# V1 is in anti-coorelation with V3 (-0.876266), but the most effected on the dimension 2 (26.81%) is V2.
# i will take the V3 & V4 because they have the stronges coorelation (0.9450505) and effected on the most
# largest dimension.
pca = PCA(training.set.pulsars[,1:8],scale.unit = TRUE,graph = T)
print(pca$var$contrib[,1:2])
cat("The coorelation of V3 & V4 is: " ,cor(training.set.pulsars$V3,training.set.pulsars$V4),"\n")

new.train =  as.data.table(cbind(Class = training.set.pulsars$Class,V3 = training.set.pulsars$V3,V4 = training.set.pulsars$V4))
new.train$Class = as.factor(training.set.pulsars$Class)

decision.tree = train(Class~., data=new.train, method="rpart")
random.forest = train(Class~., data=new.train, method="rf")
bagging = bag(new.train[,2:3], new.train$Class, B=10, bagControl = bagControl(fit=ctreeBag$fit, predict=ctreeBag$pred, aggregate=ctreeBag$aggregate))

# Section 3
pca.rcomp = prcomp(training.set.pulsars[,1:8], scale. = T, center = T)
training.after.pca = as.data.table(cbind(Class = training.set.pulsars$Class, pca.rcomp$x))
training.after.pca$Class = as.factor(training.after.pca$Class)
#creating a decision tree model by taking the first 2 dimensions
decision.tree.pca.rcomp = train(Class~., data=training.after.pca[, 1:3], method="rpart")
#creating a random forest model
random.forest.pca.rcomp = train(Class~., data=training.after.pca[, 1:3], method="rf")
#creating a bagging model
bagging.pca.rcomp = bag(training.after.pca[,2:3], training.after.pca$Class, B=10, bagControl = bagControl(fit=ctreeBag$fit, predict=ctreeBag$pred, aggregate=ctreeBag$aggregate))

# Section 4
random.forest.pca.rcomp.all.dimension = train(Class~., data=training.after.pca[,1:8], method="rf")

# Section 5
# for section 2
decision.tree.predictions = predict(decision.tree, newdata=testing.set.pulsars[,3:4])
decision.tree.conf = confusionMatrix(decision.tree.predictions ,testing.set.pulsars$Class)
# decision tree Accuracy with PCA:  0.9790503
cat("decision tree Accuracy with PCA: ",decision.tree.conf$overall[1])


random.forest.predictions = predict(random.forest, newdata=testing.set.pulsars[,3:4])
random.forest.conf = confusionMatrix(random.forest.predictions,testing.set.pulsars$Class)
# random forest Accuracy with PCA:  0.9768156
cat("\nrandom forest Accuracy with PCA: ",random.forest.conf$overall[1])

bagging.predictions = predict(bagging, newdata=testing.set.pulsars[, 3:4])
bagging.conf = confusionMatrix(bagging.predictions,testing.set.pulsars$Class)
# bagging Accuracy with PCA:  0.977933
cat("\nbagging Accuracy with PCA: ",bagging.conf$overall[1])

# for section 3
# we scaling it and multiply it by the rotation to bring it to the same coordinate system
testing.after.pca = scale(testing.set.pulsars[, 1:8]) %*% pca.rcomp$rotation 
testing.after.pca = as.data.table(cbind(Class=testing.set.pulsars$Class, testing.after.pca))
testing.after.pca$Class = as.factor(testing.after.pca$Class)

decision.tree.prediction.pca = predict(decision.tree.pca.rcomp, newdata=testing.after.pca[, 1:3])
decision.tree.conf.pca = confusionMatrix(decision.tree.prediction.pca,testing.after.pca$Class)
# decision tree Accuracy with prcomp on two dimensions:  0.9673184
cat("\ndecision tree Accuracy with prcomp on two dimensions: ",decision.tree.conf.pca$overall[1])

random.forest.prediction.pca = predict(random.forest.pca.rcomp, newdata=testing.after.pca[, 1:3])
random.forest.conf.pca = confusionMatrix(random.forest.prediction.pca,testing.after.pca$Class)
# random forest Accuracy with prcomp on two dimensions:  0.9726257
cat("\nrandom forest Accuracy with prcomp on two dimensions: ",random.forest.conf.pca$overall[1])

bagging.prediction.pca = predict(bagging.pca.rcomp, newdata=testing.after.pca[, 2:3])
bagging.conf.pca = confusionMatrix(bagging.prediction.pca,testing.after.pca$Class)
# bagging Accuracy with prcomp on two dimensions:  0.9756983
cat("\nbagging Accuracy with prcomp on two dimensions: ",bagging.conf.pca$overall[1])

# for section 4
random.forest.all.dimensions.prediction.pca = predict(random.forest.pca.rcomp.all.dimension, newdata=testing.after.pca[, 1:8])
random.forest.all.dimensions.conf.pca = confusionMatrix(random.forest.all.dimensions.prediction.pca,testing.after.pca$Class)
# random forest Accuracy with prcomp on all 8 dimensions:  0.9798883
cat("\nrandom forest Accuracy with prcomp on all 8 dimensions: ",random.forest.all.dimensions.conf.pca$overall[1])

# Section 6
# i picked the random forest becuase he had the most higher accuracy (0.9798883).
validation.after.pca = scale(validating.set.pulsars[, 1:8]) %*% pca.rcomp$rotation
validation.after.pca = as.data.table(cbind(Class=validating.set.pulsars$Class, validation.after.pca))
validation.after.pca$Class = as.factor(validation.after.pca$Class)

random.forest.valid.pca = predict(random.forest.pca.rcomp.all.dimension, newdata=validation.after.pca[, 1:8])
conf.random.forest.valid.pca = confusionMatrix(random.forest.valid.pca,validation.after.pca$Class)
cat("\n------- random forest with all the 8 dimension over validation with prcomp: ",conf.random.forest.valid.pca$overall[1],"-------")