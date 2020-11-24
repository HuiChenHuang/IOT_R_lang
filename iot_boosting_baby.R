install.packages("RCurl")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("C50")
install.packages("UsingR")
install.packages("modeldata")

library(rpart)
library(rpart.plot)
library(C50)

library(UsingR) #baby data
library(modeldata)#churn data

data(babies)
?babies

#排除有遺漏值的資料列
babyData=na.exclude(babies)


n = 0.3*nrow(babies)
test.index = sample(1:nrow(babies), n)
babyTrain = babies[-test.index,]
babyTest = babies[test.index,]

#--- CART ---
baby.tree = rpart(wt~., data = babyTrain)
y = babyTest$wt
y_hat = predict(baby.tree, newdata = babyTest, type = "vector")
table.test = table(y, y_hat)
#預測正確率 = 矩陣對角對角總和 / 矩陣總和
cat("Correct classification ratio(test)=", sum( diag(table.test))/sum(table.test)*100, "%n" )

#MAPE
y=babies$wt[test.index]
y_hat=predict(baby.tree, babyTest, type = "vector")
test.MAPE=mean(abs(y-y_hat)/y)
cat("MAPE(test)=",test.MAPE*100,"%\n")

#plot
rpart.plot(baby.tree, cex = 0.5)

#AdaBoost
install.packages("fastAdaboost")

library(fastAdaboost)

baby_adaboost <- adaboost(wt~., babyTrain, 10)

baby_pred <- predict(baby_adaboost, newdata = babyTest)

cat("Correct classification ratio(test)=",(1-baby_pred$error)*100,"%\n")

#使用GBM建模，解決預測問題
install.packages("gbm")
library(gbm)

set.seed(123)
bwt_GBM=gbm(wt~.,data = babyTrain, distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = 0.001, bag.fraction = 0.5)
# distribution：損失函數  ; 
# n.trees：迭代次數 ; 
# interaction.depth：決策樹深度
# shrinkage: 就是learning rate避免過度訓練     ; 
# bag.fraction建模一開始隨機選取訓練數據進行後續模型訓練的抽樣比率

summary(bwt_GBM)#檢視變數重要性
plot(bwt_GBM, i="gestation")#繪圖檢視X變數與Y變數的關係
y_hat=predict(bwt_GBM, newdata = babyTest, n.trees = 5000)
test_GBM.MAPE=mean(abs(y-y_hat)/y)
cat("MAPE(test)=",test_GBM.MAPE*100,"%\n")
