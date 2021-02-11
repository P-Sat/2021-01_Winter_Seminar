###########################################################
#######1. 디렉토리, 라이브러리, 데이터 불러오기 코드#######
###########################################################
#아래 경로를 바꿔서 실행해 주시면 됩니다.

setwd('C:/Users/samsung/Desktop/대학교/4학년 2학기/방세')
getwd()


need_packages <- c("data.table", "tidyverse",  "caret", "xgboost", "MLmetrics") #사용할 라이브러리 
options(warn = -1)
for(i in 1:length(need_packages)){
  if(require(need_packages[i], character.only = T) == 0){
    install.packages(need_packages[i])
    require(need_packages[i], character.only = T)
  }
  else{require(need_packages[i], character.only = T)}
}
rm(list = ls())


data <- fread('train_preprocessed.csv',data.table = FALSE)


#######################
#######2. 모델링#######
#######################
#사용모델, 파라미터 튜닝방법, 변수선택법 등 모델링과정을 주석을 이용해서 잘 정리해 주세요!


#예)
#사용 모델 : Xgboost 
#파라미터 튜닝 방법 : Gridsearch 5-fold CV


#a. gridsearch를 위한  파라미터 리스트 만들기
set.seed(26)
parameters_list = list()
for (iter in 1:3){
  param <- list(max_depth = sample(3:8, 1),
                min_child_weight = sample(3:8, 1),
                subsample = runif(1, .6, 1),
                colsample_bytree = runif(1, .6, 1),
                eta = runif(1, .01, .3),
                nrounds = sample(c(500,600,700,800),1)
  )
  parameters <- as.data.frame(param)
  parameters_list[[iter]] <- parameters
}
parameters_df = do.call(rbind, parameters_list)


#b. 5-fold Gridsearch 하기 
cv <- createFolds(data[,1],k=5)
result_param <- c()
for (row in 1:nrow(parameters_df)){
  F1_result <- c()
  
  for( i in 1:5){
    idx <- cv[[i]]
    train_x<- data[-idx,]
    val_x <- data[idx,]
    
    train_xgb = xgb.DMatrix(as.matrix(train_x[,-1]), label = train_x[,1])
    vali_xgb = xgb.DMatrix(as.matrix(val_x[,-1]), label = val_x[,1])
    
    mdcv <- xgb.train(data=train_xgb,
                      max_depth = parameters_df$max_depth[row],
                      min_child_weight = parameters_df$min_child_weight[row],
                      subsample = parameters_df$subsample[row],
                      colsample_bytree = parameters_df$colsample_bytree[row],
                      eta = parameters_df$eta[row],
                      nrounds= parameters_df$nrounds[row],
                      early_stopping_rounds= 0.05*parameters_df$nrounds[row],
                      watchlist = list(train= train_xgb, val= vali_xgb))
    
    prediction <- predict(mdcv, newdata=vali_xgb) 
    prediction <- ifelse(prediction >= 0.5,1,0)
    
    F1_temp <- F1_Score(val_x[,1],prediction)
    F1_result <- c(F1_temp, F1_result)
    result <- mean(F1_result)
  }
  print(result)
  result_param[row] <- result
}

parameter_best <- parameters_df[which.max(result_param),]

############################
###3. 최종 모델링 및 저장###
############################

data_xgb = xgb.DMatrix(as.matrix(data[,-1]), label = data[,1])

model <- xgb.train(data=data_xgb,
                  max_depth = parameters_df$max_depth[1],
                  min_child_weight = parameters_df$min_child_weight[1],
                  subsample = parameters_df$subsample[1],
                  colsample_bytree = parameters_df$colsample_bytree[1],
                  eta = parameters_df$eta[1],
                  nrounds= parameters_df$nrounds[1],
                  early_stopping_rounds= 0.05*parameters_df$nrounds[1],
                  watchlist = list(train= train_xgb))



save.image(file="model.RData")

rm(list = ls())


