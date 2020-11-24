#從網站取得數據
boston_data = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", header = FALSE)
nrow(boston_data)
head(boston_data)

#對屬性重新命名
names(boston_data) = c("CRIM","ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "PRICE")

#產生訓練樣本(80%), 測試樣本(20%)
n = 0.2*nrow(boston_data)
test.index = sample(1:nrow(boston_data),n)
Train = boston_data[-test.index,]
Test = boston_data[test.index,]

#將訓練資料集分成3份
#3-folds
n=3
n.folds=rep(1:n, nrow(Train)/n) #分3組
train.folds=split(Train, n.folds) #3份資料放到清單

#--- 第一階段：stacking ---
###1 ----------------------------------------------------------
#第一個演算法使用多元回歸
#1st fold for validation
meta.Train_x=vector()
meta.Test_x=list()

#data
stacking.train=rbind(train.folds[[2]], train.folds[[3]])
stacking.valid=train.folds[[1]]
stacking.test=Test

#model
model_1 = lm(PRICE~.,stacking.train) #Y1_hat
#predict
tmp.meta.Train_x = predict(model_1, stacking.valid) #預測validation
tmp.meta.Test_x = predict(model_1, stacking.test) #預測 Test
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) #一開始 meta.x 是空的
##
meta.Test_x[[1]] = tmp.meta.Test_x

#2nd fold for validation
#data
stacking.train = rbind(train.folds[[1]], train.folds[[3]])
stacking.valid = train.folds[[2]]
stacking.test = Test
#model
model_1 = lm(PRICE~.,stacking.train)#Y2_hat
#predict
tmp.meta.Train_x = predict(model_1, stacking.valid)
tmp.meta.Test_x = predict(model_1, stacking.test) #預測 Test
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) 
##
meta.Test_x[[2]] = tmp.meta.Test_x

#3rd fold for validation
#data
stacking.train = rbind(train.folds[[1]], train.folds[[2]])
stacking.valid = train.folds[[3]]
stacking.test = Test
#model
model_1 = lm(PRICE~.,stacking.train)#Y3_hat
#predict
tmp.meta.Train_x = predict(model_1, stacking.valid)
tmp.meta.Test_x = predict(model_1, stacking.test) #預測 Test
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) 
##
meta.Test_x[[3]] = tmp.meta.Test_x

#Average Meta.X for Test
mean.meta.Test_x = (meta.Test_x[[1]] + meta.Test_x[[2]] + meta.Test_x[[3]]) / 3

#Meta-Train DataSet
meta.train.1 = data.frame(meta_x=meta.Train_x, y=Train$PRICE)
meta.test.1  = data.frame(meta_x=mean.meta.Test_x, y=Test$PRICE)
###1 ----------------------------------------------------------

###2 ----------------------------------------------------------
#第二個模型用SVR (Support Vector Regression)
install.packages("e1071")
llibrary(e1071)
meta.Train_x = vector()
meta.Test_x = list()

#1st fold
#data
stacking.train=rbind(train.folds[[2]], train.folds[[3]])
stacking.valid=train.folds[[1]]
stacking.test=Test
#model
model_2 = svm(PRICE~.,stacking.train) #Y1_hat
#predict
tmp.meta.Train_x = predict(model_2, stacking.valid) #預測validation
tmp.meta.Test_x = predict(model_2, stacking.test) #預測 Test
##
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) #一開始 meta.x 是空的
meta.Test_x[[1]] = tmp.meta.Test_x

#2nd fold
#data
stacking.train=rbind(train.folds[[1]], train.folds[[3]])
stacking.valid=train.folds[[2]]
stacking.test=Test
#model
model_2 = svm(PRICE~.,stacking.train) #Y2_hat
#predict
tmp.meta.Train_x = predict(model_2, stacking.valid) #預測validation
tmp.meta.Test_x = predict(model_2, stacking.test) #預測 Test
##
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) #一開始 meta.x 是空的
meta.Test_x[[2]] = tmp.meta.Test_x

#3rd fold
#data
stacking.train=rbind(train.folds[[1]], train.folds[[2]])
stacking.valid=train.folds[[3]]
stacking.test=Test
#model
model_2 = svm(PRICE~.,stacking.train) #Y3_hat
#predict
tmp.meta.Train_x = predict(model_2, stacking.valid) #預測validation
tmp.meta.Test_x = predict(model_2, stacking.test) #預測 Test
##
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) #一開始 meta.x 是空的
meta.Test_x[[3]] = tmp.meta.Test_x

#Average Meta.X for Test
mean.meta.Test_x = (meta.Test_x[[1]] + meta.Test_x[[2]] + meta.Test_x[[3]]) / 3

#Meta-Train DataSet
meta.train.2 = data.frame(meta_x=meta.Train_x, y=Train$PRICE)
meta.test.2  = data.frame(meta_x=mean.meta.Test_x, y=Test$PRICE)
###2 ----------------------------------------------------------

###3 ----------------------------------------------------------
#第三個模型用CART
library(rpart)
meta.Train_x = vector()
meta.Test_x = list()

#1st fold
#data
stacking.train=rbind(train.folds[[2]], train.folds[[3]])
stacking.valid=train.folds[[1]]
stacking.test=Test
#model
model_3 = svm(PRICE~.,stacking.train) #Y1_hat
#predict
tmp.meta.Train_x = predict(model_3, stacking.valid) #預測validation
tmp.meta.Test_x = predict(model_3, stacking.test) #預測 Test
##
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) #一開始 meta.x 是空的
meta.Test_x[[1]] = tmp.meta.Test_x

#2nd fold
#data
stacking.train=rbind(train.folds[[1]], train.folds[[3]])
stacking.valid=train.folds[[2]]
stacking.test=Test
#model
model_3 = svm(PRICE~.,stacking.train) #Y2_hat
#predict
tmp.meta.Train_x = predict(model_3, stacking.valid) #預測validation
tmp.meta.Test_x = predict(model_3, stacking.test) #預測 Test
##
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) 
meta.Test_x[[2]] = tmp.meta.Test_x

#3rd fold
#data
stacking.train=rbind(train.folds[[1]], train.folds[[2]])
stacking.valid=train.folds[[3]]
stacking.test=Test
#model
model_3 = svm(PRICE~.,stacking.train) #Y3_hat
#predict
tmp.meta.Train_x = predict(model_3, stacking.valid) #預測validation
tmp.meta.Test_x = predict(model_3, stacking.test) #預測 Test
##
meta.Train_x = c(meta.Train_x, tmp.meta.Train_x) 
meta.Test_x[[3]] = tmp.meta.Test_x

#Average Meta.X for Test
mean.meta.Test_x = (meta.Test_x[[1]] + meta.Test_x[[2]] + meta.Test_x[[3]]) / 3

#Meta-Train DataSet
meta.train.3 = data.frame(meta_x=meta.Train_x, y=Train$PRICE)
meta.test.3  = data.frame(meta_x=mean.meta.Test_x, y=Test$PRICE)
###3 ----------------------------------------------------------

#--- 第二階段：Blending ---
#建構第二階段的 Meta-Model，使用xgboost 建模
install.packages("xgboost")
library(xgboost)

#先把三個meta-train 合併
allitems.meta.train = rbind(meta.train.1, meta.train.2, meta.train.3)
allitems.meta.test = rbind(meta.test.1, meta.test.2, meta.test.3)

#轉換成xgboost 的格式
train_matrix = xgb.DMatrix(data = as.matrix(allitems.meta.train[,1]), label = allitems.meta.train[,2])
test_matrix = xgb.DMatrix(data = as.matrix(allitems.meta.test[,1]), label = allitems.meta.test[,2])

# 訓練 xgboost model
#step1: 參數設定
xgb.params = list(
  #col的抽樣比例，愈高表示每棵樹使用的col愈多，會增加每顆小樹的複雜度
  colsample_bytree=0.7,
  #row的抽樣比例，愈高表示每棵樹與用的col愈多，會增加每顆小樹的複雜度
  subsample=0.5,
  booster="gbtree",
  max_depth=6,
  eta=0.03,
  eval_metric="rmse",
  objective="reg:squarederror",
  gamma=0
)

#step2: 使用xgb.cv()，優化出最佳的決策樹數量
cv.model = xgb.cv(params = xgb.params, data = train_matrix, nfold = 5, nrounds = 500, early_stopping_rounds = 50, print_every_n = 20)

#step3: 畫圖檢視Overfitting 與取得最適迭代次數
log = cv.model$evaluation_log

plot(x=1:nrow(log), y=log$train_rmse_mean, col='red', xlab = "nround", ylab = "rmse", main = "Check overfitting")
points(x=1:nrow(log), y=log$test_rmse_mean, col="blue")
legend("topright", pch=1, col = c("red","blue"), legend = c("Train","Validation"))

#獲得 best nround
best.nround = cv.model$best_iteration

#建立模型
xgb.model = xgb.train(params = xgb.params, data = train_matrix, nrounds = best.nround, save_name = "stacking_xgboost.model")

#對三組 Meta-Test 進行預測
dtest.1 = xgb.DMatrix(data=as.matrix(meta.test.1[,1]), label=meta.test.1[,2])
final_1 = predict(xgb.model, dtest.1)

dtest.2 = xgb.DMatrix(data=as.matrix(meta.test.2[,1]), label=meta.test.2[,2])
final_2 = predict(xgb.model, dtest.2)

dtest.3 = xgb.DMatrix(data=as.matrix(meta.test.3[,1]), label=meta.test.3[,2])
final_3 = predict(xgb.model, dtest.3)

#把三組結果平均，然後算MAPE
final_y = (final_1 + final_2 + final_3) / 3

test.MAPE=mean(abs(Test$PRICE - final_y) / Test$PRICE)
cat("MAPE(test)=",test.MAPE*100,"%\n")
