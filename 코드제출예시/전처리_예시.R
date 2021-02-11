###########################################################
#######1. 디렉토리, 라이브러리, 데이터 불러오기 코드#######
###########################################################
#아래 경로를 바꿔서 실행해 주시면 됩니다.


setwd('C:/Users/samsung/Desktop/대학교/4학년 2학기/방세')
getwd()


need_packages <- c("data.table", "tidyverse") #사용할 라이브러리
options(warn = -1)
for(i in 1:length(need_packages)){
  if(require(need_packages[i], character.only = T) == 0){
    install.packages(need_packages[i])
    require(need_packages[i], character.only = T)
  }
  else{require(need_packages[i], character.only = T)}
}
rm(list = ls())

data <- fread('train.csv',data.table = FALSE)



###################
######2.EDA########
###################
#아래 처럼 a, b 등으로 인덱싱을 해서 EDA 코드를 정리해 주세요! 

#a. 총 결측치 수 및 비율 확인
sum(is.na(data))
sum(is.na(data)) /(40000*171) *100

#b. 컬럼별 결측치 수 확인 및 히스토그램으로 확인
colSums(is.na(data)) %>% hist(nclass= 20)


#####################
######3. 전처리######
#####################
#아래처럼 a,b 등으로 인덱싱하고, 결측치 처리 방식, 불균형 처리 등 전처리 방식도 적어 주세요!

#a. 결측치 처리_방식 : mean imputation
na_imput <- function(x) {
  data.frame(sapply(x, function(y) ifelse(is.na(y) , mean(y, na.rm = TRUE), y)))
}
data <- cbind(data[,1],na_imput(data[,-1]))
sum(is.na(data))

#b. target 0,1로 바꾸기
data[,1] <- ifelse(data[,1] == 'pos',1,0)

#c. 불균형 등등 ...


###########################
###4. 전처리 데이터 저장###
###########################
#아래처럼 전처리 데이터를 저장하면 편하겠죵! 

write.csv(data,'train_preprocessed.csv', row.names= FALSE)

