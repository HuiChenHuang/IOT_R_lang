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


data()
data(mlc_churn)
?mlc_churn

#typeof(mlc_churn)
class(mlc_churn) <- "data.frame"

n = 0.3*nrow(mlc_churn)
test.index = sample(1:nrow(mlc_churn), n)
churnTrain = mlc_churn[-test.index,]
churnTrain = churnTrain[,-3] #排除 area_code 欄位
churnTest = mlc_churn[test.index,]
churnTest = churnTest[,-3]

#CART
churn.tree = rpart(churn~., data = churnTrain)
y = churnTest$churn
y_hat = predict(churn.tree, newdata = churnTest, type = "class")
table.test = table(y, y_hat)
#預測正確率 = 矩陣對角對角總和 / 矩陣總和
cat("Correct classification ratio(test)=", sum( diag(table.test))/sum(table.test)*100, "%n" )
rpart.plot(churn.tree, cex = 0.5)


#AdaBoost
install.packages("fastAdaboost")
library(fastAdaboost)

churn_adaboost <- adaboost(churn~., churnTrain, 10)
pred <- predict(churn_adaboost, newdata = churnTest)
cat("Correct classification ratio(test)=",(1-pred$error)*100,"%\n")


#使用GBM建模,解決分類問題(參數：損失函數distribution選擇"bernoulli")
install.packages("gbm")
library(gbm)
set.seed(123)
churnTrain$churn = ifelse(churnTrain$churn=='yes',1,0)  #GBM的Y變數僅識別 0與1
churn_GBM =gbm(churn~.,
               data=churnTrain , 
               distribution= "bernoulli", #損失函數 
               n.trees =10000, #迭代次數 
               interaction.depth =4, #決策樹深度
               shrinkage = 0.01, # learning rate避免過度訓練 
               bag.fraction = 0.5, #隨機選取訓練數據進行後續模型訓練的抽樣比率
               cv.folds=5 # 交叉驗證組數
)         
# GBM作者建議shrinkage參數設在0.01 ~ 0.001之間
# n.trees參數設在3000-10000之間

# 用交叉驗證確定最佳迭代次數
best.iter <- gbm.perf(churn_GBM,method='cv')

#利用最佳迭代次數再次建模
churn_GBM =gbm(churn~.,
               data=churnTrain , 
               distribution= "bernoulli", #損失函數 
               n.trees = best.iter, #迭代次數 
               interaction.depth =4, #決策樹深度
               shrinkage = 0.01, # learning rate避免過度訓練 
               bag.fraction = 0.5, #隨機選取訓練數據進行後續模型訓練的抽樣比率
               cv.folds=5 # 交叉驗證組數
)

#檢視變數重要性
summary(churn_GBM)

#評分
data_test <- churnTest
data_test$churn = ifelse(data_test$churn=='yes',1,0)  #將yes/no轉為 1/0
pred=predict(churn_GBM ,newdata = data_test,n.trees = best.iter)

#繪製 ROC圖
install.packages("stats")
install.packages("pROC")
