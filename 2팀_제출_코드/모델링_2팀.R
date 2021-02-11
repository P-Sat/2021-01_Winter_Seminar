###########################################################
#######1. 디렉토리, 라이브러리, 데이터 불러오기 코드#######
###########################################################

setwd('D:/학회/방학세미나/최종')
getwd()

need_packages <- c("data.table", "tidyverse", "magrittr", "gridExtra", "dummies", "caret", "xgboost", "MLmetrics", "DMwR", "progress","randomForest")
options(warn = -1)
for(i in 1:length(need_packages)){
  if(require(need_packages[i], character.only = T) == 0){
    install.packages(need_packages[i])
    require(need_packages[i], character.only = T)
  }
  else{require(need_packages[i], character.only = T)}
}
rm(list = ls())

train_imp = fread('train_imp.csv')
train_imp$class = as.factor(train_imp$class)


#######################
#######2. 모델링#######
#######################

# 사용 모델 : Random Forest
# 파라미터 튜닝 방법 : Grid Search with 7-fold cv


# --- 1. 최종 모델
train_sm = fread('train_imp_ts_sm.csv',data.table = FALSE)
train_sm$class = as.factor(train_sm$class)
train_sm$class = as.numeric(train_sm$class)-1

set.seed(123)
rf_mod = randomForest(class~., train_sm, mtry = 12, ntree = 100)

save.image(file = 'final_model.RData')
