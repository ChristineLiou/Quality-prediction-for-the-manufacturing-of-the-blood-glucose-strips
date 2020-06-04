# load data
load("~/HMD/RDATA/IPQC.RData")

#1. IPQC visualization #########################################
#因為樣本數不平衡，所以畫出的圖其實NG和PASS看不太出來太大的差異
library(ggplot2)
library(dplyr)

pn <- ggplot(IPQC, aes(x=passornot, y=ave_rdiffpc))+ geom_boxplot(outlier.size = 1.5, outlier.shape = 21) + scale_x_discrete(limits=c("pass","NG")) + ggtitle("boxplot IPQC")
pn

p <- ggplot(IPQC, aes(x=obs, y=ave_rdiffpc))+ geom_boxplot() + theme(axis.text.x = element_text(angle = 45))+ facet_grid(Row ~passornot)+ scale_x_discrete(limits= c("1(左吸)","2(左吸)","3(左吸)","4(左吸)","5(左吸)","6(右吸)","7(右吸)","8(右吸)","9(右吸)","10(右吸)")) + ggtitle("Boxplot IPQC before balance") 
p 

#先移掉pass的ave_rdiffpc 離群值 (Univariate Outlier Detection by Boxplot)
library(dplyr)
IPQC_PASS <- filter(IPQC, IPQC$passornot=="pass")
IPQC_NG <- filter(IPQC, IPQC$passornot=="NG")
a <- which(IPQC_PASS$ave_rdiffpc %in% boxplot.stats(IPQC_PASS$ave_rdiffpc)$out)
IPQC_PASS <- IPQC_PASS[-a,]
IPQC <- rbind(IPQC_PASS,IPQC_NG)

p_naout <- ggplot(IPQC, aes(x=obs, y=ave_rdiffpc))+ geom_boxplot() + theme(axis.text.x = element_text(angle = 45))+ facet_grid(Row ~passornot)+ scale_x_discrete(limits= c("1(左吸)","2(左吸)","3(左吸)","4(左吸)","5(左吸)","6(右吸)","7(右吸)","8(右吸)","9(右吸)","10(右吸)")) 
p_naout+ ggtitle("Boxplot IPQC before balance_remove pass outlier")

# plot IQR and median ####
IQR_PASS <- aggregate(IPQC_PASS$ave_rdiffpc, by= list(IPQC_PASS$obs,IPQC_PASS$Row) , FUN = "IQR")
IQR_PASS <- cbind(IQR_PASS,"Pass")
colnames(IQR_PASS)<- c("obs","Row","IQR","passornot")

IQR_NG <- aggregate(IPQC_NG$ave_rdiffpc, by= list(IPQC_NG$obs,IPQC_NG$Row) , FUN = "IQR")
IQR_NG <- cbind(IQR_NG,"NG")
colnames(IQR_NG)<- c("obs","Row","IQR","passornot")

med_PASS <- aggregate(IPQC_PASS$ave_rdiffpc, by= list(IPQC_PASS$obs,IPQC_PASS$Row) , FUN = "median")
med_PASS <- cbind(med_PASS,"Pass")
colnames(med_PASS)<- c("obs","Row","median","passornot")

med_NG <- aggregate(IPQC_NG$ave_rdiffpc, by= list(IPQC_NG$obs,IPQC_NG$Row) , FUN = "median")
med_NG <- cbind(med_NG,"NG")
colnames(med_NG)<- c("obs","Row","median","passornot")

IQR <- rbind(IQR_PASS[c("IQR","passornot")],IQR_NG[c("IQR","passornot")])
med <- rbind(med_PASS[c("median","passornot")],med_NG[c("median","passornot")])
IQR_med <- cbind(IQR,med)
IQR_med <- IQR_med[,-2]

IQR <- ggplot(IQR_med,aes(x=IQR, y=median, shape= passornot, group= passornot))+ geom_point()

IQR+  scale_y_continuous(breaks=seq(-7.5, 5, 2.5)) + scale_x_continuous(breaks=seq(-2.5, 12.5, 2.5))

IQR_RO+  scale_y_continuous(breaks=seq(-7.5, 5, 2.5)) + scale_x_continuous(breaks=seq(-2.5, 12.5, 2.5))

IQR_SM+scale_y_continuous(breaks=seq(-7.5, 5, 2.5)) + scale_x_continuous(breaks=seq(-2.5, 12.5, 2.5))

rm(IQR_NG,IQR_PASS,med_NG,med_PASS,med)

# 2. SMOTE +model ############################################## 
## 2.1 SMOTE + tree (rpart) ####################################
### SMOTE
library(DMwR)
set.seed(1111)
table(IPQC$passornot)
IPQC_SMOTE <- SMOTE(passornot ~ ., IPQC, perc.over = 1150,perc.under=150)
table(IPQC_SMOTE$passornot)

SMOTE_pn <- ggplot(IPQC_SMOTE, aes(x=passornot, y=ave_rdiffpc))+ geom_boxplot(outlier.size = 1.5, outlier.shape = 21) + scale_x_discrete(limits=c("pass","NG"))  
SMOTE_pn+ ggtitle("boxplot IPQC_SMOTE")

smote <- ggplot(IPQC_SMOTE, aes(x=obs, y=ave_rdiffpc))+ geom_boxplot() + theme(axis.text.x = element_text(angle = 45))+ facet_grid(Row ~passornot)+ scale_x_discrete(limits= c("1(左吸)","2(左吸)","3(左吸)","4(左吸)","5(左吸)","6(右吸)","7(右吸)","8(右吸)","9(右吸)","10(右吸)")) 
smote + ggtitle("Boxplot IPQC after SMOTE")

# plot IQR and med for SMOTE 
library(dplyr)
IPQC_SPASS <- filter(IPQC_SMOTE, IPQC_SMOTE$passornot=="pass")
IPQC_SNG <- filter(IPQC_SMOTE, IPQC_SMOTE$passornot=="NG")

IQR_SPASS <- aggregate(IPQC_SPASS$ave_rdiffpc, by= list(IPQC_SPASS$obs,IPQC_SPASS$Row) , FUN = "IQR")
IQR_SPASS <- cbind(IQR_SPASS,"Pass")
colnames(IQR_SPASS)<- c("obs","Row","IQR","passornot")

IQR_SNG <- aggregate(IPQC_SNG$ave_rdiffpc, by= list(IPQC_SNG$obs,IPQC_SNG$Row) , FUN = "IQR")
IQR_SNG <- cbind(IQR_SNG,"NG")
colnames(IQR_SNG)<- c("obs","Row","IQR","passornot")

med_SPASS <- aggregate(IPQC_SPASS$ave_rdiffpc, by= list(IPQC_SPASS$obs,IPQC_SPASS$Row) , FUN = "median")
med_SPASS <- cbind(med_SPASS,"Pass")
colnames(med_SPASS)<- c("obs","Row","median","passornot")

med_SNG <- aggregate(IPQC_SNG$ave_rdiffpc, by= list(IPQC_SNG$obs,IPQC_SNG$Row) , FUN = "median")
med_SNG <- cbind(med_SNG,"NG")
colnames(med_SNG)<- c("obs","Row","median","passornot")

IQR_SM <- rbind(IQR_SPASS[c("IQR","passornot")],IQR_SNG[c("IQR","passornot")])
med_SM <- rbind(med_SPASS[c("median","passornot")],med_SNG[c("median","passornot")])
IQR_med_SM <- cbind(IQR_SM,med_SM)
IQR_med_SM <- IQR_med_SM[,-2]

IQR_SM <- ggplot(IQR_med_SM,aes(x=IQR, y=median, shape= passornot, group= passornot))+ geom_point()
IQR_SM

rm(IQR_SPASS,IQR_SNG,med_SM,med_SNG,med_SPASS)

ggplot(IPQC_SMOTE, aes(x=passornot, y=ave_rdiffpc))+ geom_boxplot(outlier.size = 1.5, outlier.shape = 21) + scale_x_discrete(limits=c("pass","NG")) + ggtitle("boxplot IPQC(passornot)") 

ggplot(IPQC_SMOTE, aes(x=Row, y=ave_rdiffpc))+ geom_boxplot(outlier.size = 1.5, outlier.shape = 21) + scale_x_discrete(limits=c("1","2","3","4","5","6")) + ggtitle("boxplot IPQC(Row)") + facet_wrap(~passornot)

### 資料切分
np <- ceiling(0.1*nrow(IPQC_SMOTE))
test <- sample(1:nrow(IPQC_SMOTE),np)
IPQC_test <- IPQC_SMOTE[test,]
IPQC_train <- IPQC_SMOTE[-test,]

table(IPQC_train$passornot)/nrow(IPQC_train)
table(IPQC_test$passornot)/nrow(IPQC_test)

### tree (rpart) 建立決策樹
library(rpart)

IPQC_tree <- rpart(passornot ~   Row+obs + ave_rdiffpc, 
                   data = IPQC_train,
                   method = "class")  
IPQC_tree

### 剪枝
printcp(IPQC_tree)
plotcp(IPQC_tree)

#取最小錯誤率剪枝
prunetree_IPQC <- prune(IPQC_tree, cp = IPQC_tree$cptable[which.min(IPQC_tree$cptable[,"xerror"]),"CP"])

#取最小錯誤率加上一倍標準誤
opt <- which.min(IPQC_tree$cptable[,"xerror"])
oneSe <- which(IPQC_tree$cptable[, "rel error"] < 
                IPQC_tree$cptable[opt,"xerror"] + IPQC_tree$cptable[opt, "xstd"])[1]

cpOneSe <- IPQC_tree$cptable[oneSe, "CP"]
IPQC_pruneOneSe <- prune(IPQC_tree, cp = cpOneSe)

knitr::kable(
  IPQC_tree$cptable, caption = ' 分類樹複雜度參數表',
  booktabs = TRUE
)

### 畫圖
library(rpart.plot)
#opar<-par(no.readonly = T)
#par(mfrow=c(1,2))
rpart.plot(IPQC_tree, digits = 3, cex=0.8, sub="剪枝前")
rpart.plot(prunetree_IPQC, digits = 3, cex=0.8, sub="剪枝後")
rpart.plot(IPQC_pruneOneSe, digits = 3, cex=0.8, sub="onese剪枝後")
#剪枝前後差不多，因為cp最小取0.01，而原本rpart預設值就是0.01所以沒啥差

### 建立規則
rpart.rules(x = IPQC_tree,cover = TRUE)

### cart 的樹樣態
library(partykit)   
rparty.tree <- as.party(IPQC_tree) # 轉換cart決策樹
rparty.tree
plot(rparty.tree) 

### 模型評估

#訓練集
passornot_train <- IPQC_SMOTE$passornot[-test]
train_prob <- predict(IPQC_tree,IPQC_train, type = "prob")
train_label <- predict(IPQC_tree,IPQC_train, type = "class")

table_train <- table(passornot_train,train_label)
correct_train <- sum(diag(table_train))/sum(table_train)*100
correct_train #69.56699

sensitivity <- table_train[2,2]/sum(table_train[2,])*100
sensitivity # 69.99227

#測試集
passornot_test <- IPQC_SMOTE$passornot[test]
test_prob <- predict(IPQC_tree,IPQC_test, type = "prob")
test_label <- predict(IPQC_tree,IPQC_test, type = "class")
table_test <- table(passornot_test,test_label)
# data.frame(test_prob, test_label)
correct_test <- sum(diag(table_test))/sum(table_test)*100
correct_test #訓練集資料正確率 63.23529 %  

sensitivity <- table_test[2,2]/sum(table_test[2,])*100
sensitivity #測試集的sensitivity 65.98639%

### ROC curve
library(pROC)
levels(IPQC_test$passornot)
head(IPQC_test$passornot)
str(IPQC_test$passornot) # "pass": 1, "NG":2

modelroc_Stree <- roc(IPQC_test$passornot,test_prob[,"NG"]) # "pass": 1, "NG":2

plot(modelroc_Stree, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE, auc.polygon.col="skyblue", print.thres=TRUE)

### CV 調參
library(caret)
set.seed(111)
cartTune <- train(passornot ~   Row+obs + ave_rdiffpc, 
                  data = IPQC_train,
                  method = "rpart",
                  trControl = trainControl(method = "cv",number = 10))

cartTune

cartTune$finalModel
plot(cartTune, scales = list(x = list(log = 10)))

library(partykit)
cartTree <- as.party(cartTune$finalModel)
plot(cartTree)

### 變數重要性
cartImp <- varImp(cartTune, scale = FALSE, competes = FALSE)
cartImp

#調參完後的模型評估
passornot_test <- IPQC_SMOTE$passornot[test]
test_prob <- predict(cartTune,IPQC_test, type = "prob", simplify = TRUE)
test_label <- predict(cartTune,IPQC_test, type = "raw")
table_test <- table(passornot_test,test_label)

correct_test <- sum(diag(table_test))/sum(table_test)*100
correct_test #測試集資料正確率 72.40581 %  

sensitivity <- table_test[2,2]/sum(table_test[2,])*100
sensitivity #測試集的sensitivity 72.76941%

library(pROC)
modelroc_Stree <- roc(IPQC_test$passornot,test_prob[,"NG"]) # "pass": 1, "NG":2

plot(modelroc_Stree, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE, auc.polygon.col="skyblue", print.thres=TRUE)

## 2.2 SMOTE + RF #######################
### SMOTE
library(DMwR)
set.seed(1111)
table(IPQC$passornot)
IPQC_SMOTE <- SMOTE(passornot ~ ., IPQC, perc.over = 800,perc.under=100)
table(IPQC_SMOTE$passornot)

###資料切分
np <- ceiling(0.1*nrow(IPQC_SMOTE))
test <- sample(1:nrow(IPQC_SMOTE),np)
IPQC_test <- IPQC_SMOTE[test,]
IPQC_train <- IPQC_SMOTE[-test,]

### 建隨機森林
library(randomForest)
#set.seed(1117)
rf <- randomForest(passornot ~ Row + obs + ave_rdiffpc, data = IPQC_train, ntree = 100)
rf

plot(rf)

# 整體錯誤率（黑色實線）隨著決策樹數量上升，下降到約35%以下並趨於穩定。
# 實際類別為NG的錯誤率（綠色虛線）隨著決策樹數量的上升，下降到約28%並趨於穩定。
# 實際類別為Yes的錯誤率（紅色虛線）隨著決策樹數量的上升，下降到約37%並趨於穩定。
# 而「最佳決策樹數目(ntree)」，約100多棵樹即足夠使誤差趨於穩定（不需要到200棵樹）。

### 變數重要性
importance(rf) 
varImpPlot(rf)

### CV調mtry
library(caret)
#set.seed(100)
mtryGrid <- data.frame(mtry = floor(seq(1, ncol(IPQC_train), length = 10)))
rfTune <- train(passornot ~ Row + obs + ave_rdiffpc, 
                data = IPQC_train,
                method = "rf",
                tuneGrid = mtryGrid,
                ntree = 100,
                importance = TRUE,
                trControl = trainControl(method = "cv",number = 10))
rfTune
plot(rfTune)

#mtryy最佳為

passornot_test <- IPQC_SMOTE$passornot[test]
test_prob <- predict(rfTune,IPQC_test, type = "prob")
test_label <- predict(rfTune,IPQC_test, type = "raw")
table_test <- table(passornot_test,test_label)

correct_test <- sum(diag(table_test))/sum(table_test)*100
correct_test #測試集資料正確率 69.48529 %  

sensitivity <- table_test[2,2]/sum(table_test[2,])*100
sensitivity #測試集的sensitivity 75.1634%

### ROC
library(pROC)
modelroc_Srf <- roc(IPQC_test$passornot,test_prob[,"NG"]) # "pass": 1, "NG":2

plot(modelroc_Srf, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE, auc.polygon.col="skyblue", print.thres=TRUE)

### 變數重要性
rfImp <- varImp(rfTune, scale = FALSE)
rfImp

#3.ROSE +model ##############################  
## 3.1 ROSE + tree (rpart) #################
library(ggplot2)
library(ROSE)
# IPQC_ROSE <- ovun.sample(passornot~., data=IPQC,N=nrow(IPQC), p=0.5, seed=1, method="both")$data    #融合過採樣和欠採樣

set.seed(111)
IPQC_ROSE <- ROSE(passornot~., data=IPQC,N=nrow(IPQC), p=0.5, seed=1, hmult.majo = 0.25, hmult.mino = 0.5)$data            
#ROSE函數包的人工合成數據(ROSE)、過採樣、欠採樣及融合過採樣和欠採樣，根據Nicola文章，ROSE的ROCcurve 表現優於其他三者，所以採用ROSE套件中的ROSE函數進行數據平衡

table(IPQC_ROSE$passornot)

rose <- ggplot(IPQC_ROSE, aes(x=obs, y=ave_rdiffpc))+ geom_boxplot() + theme(axis.text.x = element_text(angle = 45))+ facet_grid(Row ~passornot)+ scale_x_discrete(limits= c("1(左吸)","2(左吸)","3(左吸)","4(左吸)","5(左吸)","6(右吸)","7(右吸)","8(右吸)","9(右吸)","10(右吸)")) 
rose+ ggtitle("Boxplot IPQC after ROSE")

ggplot(IPQC_ROSE, aes(x=Row, y=ave_rdiffpc))+ geom_boxplot(outlier.size = 1.5, outlier.shape = 21) + scale_x_discrete(limits=c("1","2","3","4","5","6")) + ggtitle("boxplot IPQC(Row)") + facet_wrap(~passornot)

###原本的資料分布
library(ggplot2)

# plot IQR and med for ROSE 
library(dplyr)
IPQC_RPASS <- filter(IPQC_ROSE, IPQC_ROSE$passornot=="pass")
IPQC_RNG <- filter(IPQC_ROSE, IPQC_ROSE$passornot=="NG")

IQR_RPASS <- aggregate(IPQC_RPASS$ave_rdiffpc, by= list(IPQC_RPASS$obs,IPQC_RPASS$Row) , FUN = "IQR")
IQR_RPASS <- cbind(IQR_RPASS,"Pass")
colnames(IQR_RPASS)<- c("obs","Row","IQR","passornot")

IQR_RNG <- aggregate(IPQC_RNG$ave_rdiffpc, by= list(IPQC_RNG$obs,IPQC_RNG$Row) , FUN = "IQR")
IQR_RNG <- cbind(IQR_RNG,"NG")
colnames(IQR_RNG)<- c("obs","Row","IQR","passornot")

med_RPASS <- aggregate(IPQC_RPASS$ave_rdiffpc, by= list(IPQC_RPASS$obs,IPQC_RPASS$Row) , FUN = "median")
med_RPASS <- cbind(med_RPASS,"Pass")
colnames(med_RPASS)<- c("obs","Row","median","passornot")

med_RNG <- aggregate(IPQC_RNG$ave_rdiffpc, by= list(IPQC_RNG$obs,IPQC_RNG$Row) , FUN = "median")
med_RNG <- cbind(med_RNG,"NG")
colnames(med_RNG)<- c("obs","Row","median","passornot")

IQR_RO <- rbind(IQR_RPASS[c("IQR","passornot")],IQR_RNG[c("IQR","passornot")])
med_RO <- rbind(med_RPASS[c("median","passornot")],med_RNG[c("median","passornot")])
IQR_med_RO <- cbind(IQR_RO,med_RO)
IQR_med_RO <- IQR_med_RO[,-2]

IQR_RO <- ggplot(IQR_med_RO,aes(x=IQR, y=median, shape= passornot, group= passornot))+ geom_point()
IQR_RO

rm(IQR_RPASS,IQR_RNG,med_RNG,med_RPASS,med_RO)


### 資料切分
np <- ceiling(0.1*nrow(IPQC_ROSE))
test <- sample(1:nrow(IPQC_ROSE),np)
IPQC_test <- IPQC_ROSE[test,]
IPQC_train <- IPQC_ROSE[-test,]

### 建樹
library(rpart)
IPQC_tree <- rpart(passornot ~   Row + obs + ave_rdiffpc, method = "class", data = IPQC_train, control = rpart.control(minbucket = 200)) 

### 剪枝
printcp(IPQC_tree)
plotcp(IPQC_tree)
prunetree_IPQC <- prune(IPQC_tree, cp = IPQC_tree$cptable[which.min(IPQC_tree$cptable[,"xerror"]),"CP"])


### 畫圖
library(rpart.plot)
#opar<-par(no.readonly = T)
#par(mfrow=c(1,2))
rpart.plot(IPQC_tree, digits = 3, cex=0.8, sub="剪枝前")
rpart.plot(prunetree_IPQC, digits = 3, cex=0.8)
#剪枝前後一樣

rpart.rules(x = IPQC_tree,cover = TRUE)

### cart 的樹樣態
library(partykit)   
rparty.tree <- as.party(IPQC_tree) # 轉換cart決策樹
rparty.tree
plot(rparty.tree) 


### 模型評估
#訓練集
passornot_train <- IPQC_ROSE$passornot[-test]
train_prob <- predict(IPQC_tree,IPQC_train, type = "prob")
train_label <- predict(IPQC_tree,IPQC_train, type = "class")

table_train <- table(passornot_train,train_label)
correct_train <- sum(diag(table_train))/sum(table_train)*100
correct_train #69.56699

sensitivity <- table_train[2,2]/sum(table_train[2,])*100
sensitivity # 69.99227

#測試集的正確率
passornot_test <- IPQC_ROSE$passornot[test]
test_prob <- predict(IPQC_tree,IPQC_test, type = "prob")
test_label <- predict(IPQC_tree,IPQC_test, type = "class")
table_test <- table(passornot_test,test_label)
# data.frame(test_prob, test_label)

correct_test <- sum(diag(table_test))/sum(table_test)*100
correct_test #測試集資料正確率 71.2963 %  

sensitivity <- table_test[2,2]/sum(table_test[2,])*100
sensitivity #測試集的sensitivity 15.38462 %

# ROC
library(pROC)
modelroc_Rtree <- roc(IPQC_test$passornot,test_prob[,"NG"]) # "pass": 1, "NG":2

plot(modelroc_Rtree, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE, auc.polygon.col="skyblue", print.thres=TRUE)

### CV 交叉驗證
library(caret)
set.seed(111)
cartTune <- train(passornot ~   Row+obs + ave_rdiffpc, 
                  data = IPQC_train,
                  method = "rpart",
                  trControl = trainControl(method = "cv",number = 10))

cartTune

cartTune$finalModel
plot(cartTune, scales = list(x = list(log = 10)))

#調參後的tree 
library(partykit)
cartTree <- as.party(cartTune$finalModel)
plot(cartTree)

### 變數重要性
cartImp <- varImp(cartTune, scale = FALSE, competes = FALSE)
cartImp

#調參完後的模型評估
passornot_test <- IPQC_ROSE$passornot[test]
test_prob <- predict(cartTune,IPQC_test, type = "prob", simplify = TRUE)
test_label <- predict(cartTune,IPQC_test, type = "raw")
table_test <- table(passornot_test,test_label)

correct_test <- sum(diag(table_test))/sum(table_test)*100
correct_test #測試集資料正確率 72.40581 %  

sensitivity <- table_test[2,2]/sum(table_test[2,])*100
sensitivity #測試集的sensitivity 72.76941%

library(pROC)
modelroc_Rtree <- roc(IPQC_test$passornot,test_prob[,"NG"]) # "pass": 1, "NG":2

plot(modelroc_Rtree, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE, auc.polygon.col="skyblue", print.thres=TRUE)


#3.2 ROSE + RF ###########################################
library(ROSE)

set.seed(1234)
IPQC_ROSE <- ROSE(passornot~., data=IPQC,N=nrow(IPQC), p=0.5, seed=1, hmult.majo = 0.25, hmult.mino = 0.5)$data            

table(IPQC_ROSE$passornot)

### 資料切分
np <- ceiling(0.1*nrow(IPQC_ROSE))
test <- sample(1:nrow(IPQC_ROSE),np)
IPQC_test <- IPQC_ROSE[test,]
IPQC_train <- IPQC_ROSE[-test,]

#建森林
library(randomForest)
#set.seed(111)
rf <- randomForest(passornot ~ Row + obs + ave_rdiffpc, data = IPQC_train, ntree = 100)
rf_predict <- predict(rf, newdata = IPQC_test)
rf 
plot(rf)


### CV調mtry
library(caret)
#set.seed(100)
mtryGrid <- data.frame(mtry = floor(seq(1, ncol(IPQC_train), length = 10)))
rfTune <- train(passornot ~ Row + obs + ave_rdiffpc, 
                data = IPQC_train,
                method = "rf",
                tuneGrid = mtryGrid,
                ntree = 100,
                importance = TRUE,
                trControl = trainControl(method = "cv",number = 10))
rfTune
plot(rfTune)

### 測試集評估
passornot_test <- IPQC_ROSE$passornot[test]
test_prob <- predict(rf,IPQC_test, type = "prob")
test_label <- predict(rfTune,IPQC_test, type = "raw")
table_test <- table(passornot_test,test_label)

correct_test <- sum(diag(table_test))/sum(table_test)*100
correct_test #測試集資料正確率 69.48529 %  

sensitivity <- table_test[2,2]/sum(table_test[2,])*100
sensitivity #測試集的sensitivity 75.1634%

### ROC
library(pROC)
modelroc_Rrf <- roc(IPQC_test$passornot,test_prob[,"NG"]) # "pass": 1, "NG":2

plot(modelroc_Rrf, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE, auc.polygon.col="skyblue", print.thres=TRUE)


### 變數重要性
rfImp <- varImp(rfTune, scale = FALSE)
rfImp


# compared four ROC curve ####
library(pROC)
library(ggplot2)
roc_tree <- ggroc(list(smote_tree=modelroc_Stree,rose_tree=modelroc_Rtree), legacy.axes = TRUE, aes =c("linetype")) + labs(x = "FPR" , y = "TPR")
roc_tree

roc_rf <- ggroc(list(smote_rf=modelroc_Srf,rose_rf=modelroc_Rrf),legacy.axes = TRUE,aes =c("linetype", "color")) + labs(x = "FPR" , y = "TPR")
roc_rf

roc_rf <- ggroc(list(smote_rf=modelroc_Srf,rose_rf=modelroc_Rrf),legacy.axes = TRUE,aes =c("linetype")) + labs(x = "FPR" , y = "TPR")
roc_rf

roc <- ggroc(list(smote_rf=modelroc_Srf,rose_rf=modelroc_Rrf,smote_tree=modelroc_Stree,rose_tree=modelroc_Rtree), legacy.axes = TRUE,aes =c("linetype")) + labs(x = "FPR" , y = "TPR")
roc


# plot IQR and median ####
IQR_PASS <- aggregate(IPQC_PASS$ave_rdiffpc, by= list(IPQC_PASS$obs,IPQC_PASS$Row) , FUN = "IQR")
IQR_PASS <- cbind(IQR_PASS,"Pass")
colnames(IQR_PASS)<- c("obs","Row","IQR","passornot")

IQR_NG <- aggregate(IPQC_NG$ave_rdiffpc, by= list(IPQC_NG$obs,IPQC_NG$Row) , FUN = "IQR")
IQR_NG <- cbind(IQR_NG,"NG")
colnames(IQR_NG)<- c("obs","Row","IQR","passornot")

med_PASS <- aggregate(IPQC_PASS$ave_rdiffpc, by= list(IPQC_PASS$obs,IPQC_PASS$Row) , FUN = "median")
med_PASS <- cbind(med_PASS,"Pass")
colnames(med_PASS)<- c("obs","Row","median","passornot")

med_NG <- aggregate(IPQC_NG$ave_rdiffpc, by= list(IPQC_NG$obs,IPQC_NG$Row) , FUN = "median")
med_NG <- cbind(med_NG,"NG")
colnames(med_NG)<- c("obs","Row","median","passornot")

IQR <- rbind(IQR_PASS[c("IQR","passornot")],IQR_NG[c("IQR","passornot")])
med <- rbind(med_PASS[c("median","passornot")],med_NG[c("median","passornot")])
IQR_med <- cbind(IQR,med)
IQR_med <- IQR_med[,-2]

library(ggplot2)
IQR+  scale_y_continuous(breaks=seq(-7.5, 5, 2.5)) + scale_x_continuous(breaks=seq(-2.5, 12.5, 2.5))

IQR_RO+  scale_y_continuous(breaks=seq(-7.5, 5, 2.5)) + scale_x_continuous(breaks=seq(-2.5, 12.5, 2.5))

IQR_SM+scale_y_continuous(breaks=seq(-7.5, 5, 2.5)) + scale_x_continuous(breaks=seq(-2.5, 12.5, 2.5))

# 5. Boost ####
# 5.1 SMOTEBoost(RF) #############################################################
load("/Users/Vince/cstsouMac/IndustryLink/BloodGlucose/HMD/RDATA/IPQC.RData")
load("~/HMD/RDATA/IPQC.RData")

# 把NG的樣本抽十分之一做test，pass也抽十分之一做test，確保最後test有NG也有pass的樣本

library(dplyr)
NG <- filter(IPQC,IPQC$passornot=="NG") 
np <- ceiling(0.1*nrow(NG))
test <- sample(1:nrow(NG),np)
NG_test <- NG[test,]
NG_train <- NG[-test,]

pass <- filter(IPQC,IPQC$passornot=="pass") #pass=0
np <- ceiling(0.1*nrow(pass))
test <- sample(1:nrow(pass),np)
pass_test <- pass[test,]
pass_train <- pass[-test,] 

IPQC_train <- rbind(NG_train,pass_train)
IPQC_test <- rbind(NG_test,pass_test)
IPQC_train$passornot <- factor(IPQC_train$passornot, levels = c("pass", "NG"), labels = c("0", "1"))
IPQC_test$passornot <- factor(IPQC_test$passornot, levels = c("pass", "NG"), labels = c("0", "1"))

# 用sbo函數做RF的SMOTEboost 
library(ebmc)
smote_rf <- sbo(passornot ~ Row + obs + ave_rdiffpc,IPQC_train, size = 40, over = 5000, alg = "rf", rf.ntree = 100)

# 訓練集做出來的模型評估
smote_rf$errorEstimation
smote_rf$weakLearners[[40]]

# 測試集評估
passornot_test <- IPQC_test$passornot
test_label <- predict(smote_rf,IPQC_test, type = "class")
test_prob <- predict(smote_rf,IPQC_test, type = "prob")

table_test <- table(passornot_test,test_label)

data.frame(test_prob, test_label)

correct_test <- sum(diag(table_test))/sum(table_test)*100
correct_test #測試集資料正確率 67.36111 %  

sensitivity <- table_test[2,2]/sum(table_test[2,])*100
sensitivity #測試集的sensitivity 25%

# ROC curve
library(pROC)
levels(IPQC_test$passornot)
head(IPQC_test$passornot)
str(IPQC_test$passornot) # ??? Why 出現2 

modelroc <- roc(IPQC_test$passornot,test_prob) # "pass": 0, "NG":1

plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE, auc.polygon.col="skyblue", print.thres=TRUE)


# SMOTEBoost(svm) ####
# 資料分群跑SMOTEBoost(rf)前段
# 用sbo函數做svm的SMOTEboost
smote_svm <- sbo(passornot ~ Row + obs + ave_rdiffpc,IPQC_train, size = 40, over = 300, alg = "svm", svm.ker = "sigmoid")


# 測試集評估
passornot_test <- IPQC_test$passornot
test_label <- predict(smote_svm,IPQC_test, type = "class")
test_prob <- predict(smote_svm,IPQC_test, type = "prob")

table_test <- table(passornot_test,test_label)

data.frame(test_prob, test_label)

correct_test <- sum(diag(table_test))/sum(table_test)*100
correct_test #測試集資料正確率 96.2963 %  

sensitivity <- table_test[2,2]/sum(table_test[2,])*100
sensitivity #測試集的sensitivity 0%

#ROC curve
library(pROC)
levels(IPQC_test$passornot)
head(IPQC_test$passornot)
str(IPQC_test$passornot) # ??? Why 出現2 

modelroc <- roc(IPQC_test$passornot,test_prob) # "pass": 0, "NG":1

plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE, auc.polygon.col="skyblue", print.thres=TRUE)



