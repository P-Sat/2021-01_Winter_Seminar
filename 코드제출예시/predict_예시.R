###################################################
#######1. 디렉토리, 라이브러리 불러오기 코드#######
###################################################
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


################################
##2. 데이터 불러오기 및 전처리##
################################
#test 데이터는 꼭 prediction에서 전처리 해주세요 ! (data leakeage 방지)
test <- fread('test_feature.csv',data.table = FALSE)

na_imput <- function(x) {
  data.frame(sapply(x, function(y) ifelse(is.na(y) , mean(y, na.rm = TRUE), y)))
  
}               
test <- na_imput(test)
test_xgb = xgb.DMatrix(as.matrix(test))


######################
##3. Test Prediction##
######################

load('model.Rdata')

prediction <- predict(model, newdata=test_xgb) 
prediction <- ifelse(prediction >= 0.5,'pos','neg')


##########################
##4. 최종 제출 파일 저장##
##########################

id <- c(seq(1:8000))
class <- prediction
submission <- cbind(id,class)


write.csv(submission,'Submission.csv',row.names =FALSE)


