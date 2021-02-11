###########################################################
#######1. ?????丮, ???̺귯??, ?????? ?ҷ??��? ?ڵ?#######
###########################################################

setwd('D:/??ȸ/???м??̳?/??��')
getwd()

need_packages = c("data.table", "tidyverse", "mice", "MLmetrics","DMwR","gridExtra","reshape2","lubridate",'RColorBrewer',"rlang","corrplot","bestNormalize")
options(warn = -1)
for(i in 1:length(need_packages)){
  if(require(need_packages[i], character.only = T) == 0){
    install.packages(need_packages[i])
    require(need_packages[i], character.only = T)
  }
  else{require(need_packages[i], character.only = T)}
}
rm(list = ls())

train = fread('train.csv',data.table = FALSE)

dim(train) # 40000 171

start_time = Sys.time()
###################
######2.EDA########
###################

# --- 1. ?÷??? ????ġ?? Ȯ??
colna <- train %>% is.na() %>% colSums() %>% data.frame()
index <- colna %>% rownames()
colna <- cbind(index , colna)
rownames(colna) <- NULL
colnames(colna) <- c('index', 'na_cnt')
colna %>% arrange(desc(na_cnt))

# ?ð?ȭ
plot1 <- ggplot(colna, aes(x=reorder(index, -na_cnt), y=na_cnt, fill=na_cnt, color=na_cnt))+
  geom_bar(stat="identity", alpha=0.1)+
  theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
        axis.title.x=element_text(face="bold", size=10), 
        axis.title.y=element_text(face="bold", size=10),
        plot.title=element_text(face="bold", size=10),
        axis.text.x = element_text(size=7, face="bold", colour = "grey50"),
        axis.text.y = element_text(size=7, face="bold", colour = "grey50"),
        legend.title=element_text(face="bold", size=10),
        legend.text = element_text(face="bold", size=10))+
  scale_fill_gradient(
    high = "#E71D1D",
    low = "#47E71D",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = c("fill","color"))+
  labs(x="?????̸?", y="????ġ ????", fill="????ġ ????", color="????ġ ????", main="?????? ????ġ ????" )+
  geom_text(aes(label = na_cnt, color = na_cnt), position = position_stack(0.5), size = 3)
plot1

## 50%?̻? ????ġ?? ????
colna_20000 <- colna %>% filter(na_cnt>20000)
colna_20000
colna_20000 %>% dim() ##8??
# ?ð?ȭ
plot2 <- ggplot(colna_20000, aes(x=reorder(index, -na_cnt), y=na_cnt, fill=na_cnt, color=na_cnt))+
  geom_bar(stat="identity", alpha=0.1)+
  theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
        axis.title.x=element_text(face="bold", size=10), 
        axis.title.y=element_text(face="bold", size=10),
        plot.title=element_text(face="bold", size=10),
        axis.text.x = element_text(size=7, face="bold", colour = "grey50"),
        axis.text.y = element_text(size=7, face="bold", colour = "grey50"),
        legend.title=element_text(face="bold", size=10),
        legend.text = element_text(face="bold", size=10))+
  scale_fill_gradient(
    high = "#E71D1D",
    low = "#47E71D",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = c("fill","color"))+
  labs(x="?????̸?", y="????ġ ????", fill="????ġ ????", color="????ġ ????", main="50%?̻? ????ġ?? ?????ð?ȭ" )+
  geom_text(aes(label = na_cnt, color = na_cnt), position = position_stack(0.5), size = 3)
plot2

## 30%?̻? ????ġ?? ????
colna_13000 <- colna %>% filter(na_cnt>13000)
colna_13000
colna_13000 %>% dim() ##10??
# ?ð?ȭ
plot3 <- ggplot(colna_13000, aes(x=reorder(index, -na_cnt), y=na_cnt, fill=na_cnt, color=na_cnt))+
  geom_bar(stat="identity", alpha=0.1)+
  theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
        axis.title.x=element_text(face="bold", size=10), 
        axis.title.y=element_text(face="bold", size=10),
        plot.title=element_text(face="bold", size=10),
        axis.text.x = element_text(size=7, face="bold", colour = "grey50"),
        axis.text.y = element_text(size=7, face="bold", colour = "grey50"),
        legend.title=element_text(face="bold", size=10),
        legend.text = element_text(face="bold", size=10))+
  scale_fill_gradient(
    high = "#E71D1D",
    low = "#47E71D",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = c("fill","color"))+
  labs(x="?????̸?", y="????ġ ????", fill="????ġ ????", color="????ġ ????", main="50%?̻? ????ġ?? ?????ð?ȭ" )+
  geom_text(aes(label = na_cnt, color = na_cnt), position = position_stack(0.5), size = 3)
plot3

# ?????? NA???? ?ð?ȭ
nas = sort(apply(is.na(train[,-1]),2,sum),decreasing=T)
nas = data.frame(nas)
colnames(nas)="num"
nas$cat = NULL
for (i in 1:nrow(nas)){
  if (nas$num[i] >= 10000){nas$cat[i]=5}
  else if (nas$num[i] >= 5000){nas$cat[i]=4}
  else if (nas$num[i] >= 1000){nas$cat[i]=3}
  else if (nas$num[i] > 0){nas$cat[i]=2}
  else nas$cat[i]=1
}

table(nas$cat) %>% data.frame() %>% 
  ggplot(aes(x = "", y = Freq, fill = Var1)) + 
  geom_bar(width = 1, stat = "identity", alpha=0.6, color = "white")+
  coord_polar("y")+
  geom_text(aes(label = paste0(Freq)), 
            position = position_stack(vjust = 0.5),color = "black") +
  theme_void()+
  theme(text =element_text(face = "bold"))+
  scale_fill_brewer(palette = "Oranges",
                    labels=c("0??","1???̻? 1000?? ?̸?","1000???̻? 5000?? ?̸?","5000???̻? 10000?? ?̸?","10000???̻?"),
                    name = "NA ????") 


# ????ġ???? 20000??(50%) ?̻??? ?÷? ��??
colna_20000 <- colna %>% filter(na_cnt>20000)
index_20000 <- colna_20000$index
train1 <- train %>% select(-index_20000)
train1 %>% dim()


# --- 2. ?ະ ????ġ?? Ȯ??
na <- apply(is.na(train1),1,sum) ## ?ະ na ?? ???ϱ?
na %>% length()
train2 <- cbind(train1, na)
train2 %>% dim()

# ????ġ???? 85??(?? X?????? 50%) ?̻??? ?? ��??
train3 <- train2 %>% filter( na < 85 ) %>% select(-na) %>% as.data.frame()
train3 %>% dim()

train2$na <- train2$na %>% as.factor()
row_na <- train2 %>% group_by(na) %>% summarise(n= n())
row_na <- data.frame(row_na)
row_na %>% dim()

# ?ð?ȭ
plot4 <- ggplot(row_na, aes(x=reorder(na, na), y=n, fill=n, color=n))+
  geom_bar(stat="identity", alpha=0.1)+
  theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
        axis.title.x=element_text(face="bold", size=10), 
        axis.title.y=element_text(face="bold", size=10),
        plot.title=element_text(face="bold", size=10),
        axis.text.x = element_text(size=7, face="bold", colour = "grey50"),
        axis.text.y = element_text(size=7, face="bold", colour = "grey50"),
        legend.title=element_text(face="bold", size=10),
        legend.text = element_text(face="bold", size=10))+
  scale_fill_gradient(
    high = "#E71D1D",
    low = "#47E71D",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = c("fill","color"))+
  labs(x="????ġ ????", y="?? ????", fill="?? ????", color="?? ????", main="?ະ ????ġ ???? ?ð?ȭ" )+
  geom_text(aes(label = n, color = n), position = position_stack(0.5), size = 3)
plot4


# --- 3. ?????? unique ???? Ȯ??
# dat?? unique ???? ?????ϴ? ?Լ?
cnt_uniq <- function(dat){
  dat = dat[!is.na(dat)]
  cnt = length(unique(dat))
  return(cnt)
} 

uniq_cnt = apply(train3[,-1], 2, cnt_uniq) %>% as.vector()
var_uniq = data.frame(
  index = colnames(train3)[-1],
  uniq_cnt = uniq_cnt
)
var_uniq = var_uniq %>% arrange(uniq_cnt)

# ?ð?ȭ
plot5 <- var_uniq %>% filter(uniq_cnt < 1000) %>%
  ggplot(aes(x=reorder(index, uniq_cnt), y=uniq_cnt, fill=uniq_cnt, color=uniq_cnt))+
  geom_bar(stat="identity", alpha=0.1)+
  theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
        axis.title.x=element_text(face="bold", size=10), 
        axis.title.y=element_text(face="bold", size=10),
        plot.title=element_text(face="bold", size=10),
        axis.text.x = element_text(size=7, face="bold", colour = "grey50"),
        axis.text.y = element_text(size=7, face="bold", colour = "grey50"),
        legend.title=element_text(face="bold", size=10),
        legend.text = element_text(face="bold", size=10))+
  scale_fill_gradient(
    high = "#E71D1D",
    low = "#47E71D",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = c("fill","color"))+
  labs(x="?????̸?", y="uniq ????", fill="uniq ????", color="uniq ????", main="?????? uniq???? ?ð?ȭ" )+
  geom_text(aes(label = uniq_cnt, color = uniq_cnt), position = position_stack(0.5), size = 3)
plot5

# NA?? ��???? unique ?????? 1???? ???? ��??
train3$cd_000 %>% unique()
train4 = train3 %>% select(-cd_000)

dat = train4 %>% as.data.frame()
dat %>% dim() # 39764 162

write_csv(dat, "train_delete.csv")


# --- 4. Ŭ???? ?ұ??? Ȯ??
train$class = as.factor(train$class)
cls=as.data.frame(table(train$class))
colnames(cls)=c("class","Freq")
cls %>% 
  ggplot(aes(x = "", y = Freq, fill = class)) + 
  geom_bar(width = 1, stat = "identity",alpha=0.8, color = "white")+
  coord_polar("y")+
  geom_text(aes(label = paste0(round(Freq/400),"%")), 
            position = position_stack(vjust = 0.5),color = "black") +
  theme_void()   +
  theme(text =element_text(face = "bold"))+
  scale_fill_manual(values = c("#EF7C7D","#15496C"))


# --- 5. ?÷??? 0 ?????? ???? Ȯ??
zeros = sort(apply(train[,-1]==0,2,sum,na.rm=T),decreasing=T)
zeros = data.frame(zeros)
colnames(zeros)="num"
zeros$cat = NULL
for (i in 1:nrow(zeros)){
  if (zeros$num[i] >= 30000){zeros$cat[i]=4}
  else if (zeros$num[i] >= 20000){zeros$cat[i]=3}
  else if (zeros$num[i] >= 10000){zeros$cat[i]=2}
  else zeros$cat[i]=1
}

table(zeros$cat) %>% data.frame() %>% 
  ggplot(aes(x = "", y = Freq, fill = Var1)) + 
  geom_bar(width = 1, stat = "identity",color = "white")+
  coord_polar("y")+
  geom_text(aes(label = paste0(round(Freq/1.7),"%")), 
            position = position_stack(vjust = 0.5),color = "black") +
  theme_void()   +
  theme(text =element_text(face = "bold"))+
  scale_fill_brewer(labels=c("10000???̸?","10000???̻? 20000?? ?̸?","20000???̻? 30000?? ?̸?","30000???̻?"),
                    palette = "Blues",name = "0 ????")


#####################
######3. ??ó??######
#####################

dat = fread('train_delete.csv',data.table = F)

# --- 1. target ???? 0(neg), 1(pos)?? ??????ȭ
dat2 = dat %>% mutate(class = ifelse(class == 'pos',1,0) %>% as.factor)
dat2$class %>% str()

# --- 2. integer64 -> num��?? ????ȯ (mice ???? ??��?????? ???? ?????? ��??)
dat2 %>% str()
dat3 = dat2 %>% mutate_if(bit64::is.integer64,as.numeric)
dat3 %>% str()

# --- 3. ????ġ ó??: MICE

# predictorMatrix ??��: class?? imputator?? ???????? ??��
pred = matrix(1,nrow = ncol(dat3),ncol = ncol(dat3))
rownames(pred) = names(dat3)
colnames(pred) = names(dat3)
diag(pred) = 0
pred[,'class'] = 0

dat3 %>% dim() # 39764 162
dat3 %>% is.na() %>% colSums() %>% data.frame() # class, aa_000 ?????? NA ??��

# mice
set.seed(123)
imp = mice(dat3, m = 1, method = c('','',rep('cart',160)),
           predictorMatrix = pred, maxit = 3, remove.collinear = FALSE)

train_imp = complete(imp)
train_imp %>% is.na() %>% sum()

write_csv(train_imp, "train_imp.csv")

# --- 4. Variable Transformation

# ?ð?ȭ
train_imp$class = as.factor(train_imp$class)
ggplot(train_imp) + geom_density(aes(x = ag_005,col = class)) + theme_bw() + theme(text =element_text(face = "bold"))
ggplot(train_imp) + geom_density(aes(x = yeojohnson(ag_005)$x.t,col = class)) + theme_bw() + theme(text =element_text(face = "bold"))

ggplot(train_imp) + geom_density(aes(x = ed_000,col = class)) + theme_bw() + theme(text =element_text(face = "bold"))
ggplot(train_imp) + geom_density(aes(x = yeojohnson(ed_000)$x.t,col = class)) + theme_bw() + theme(text =element_text(face = "bold"))

train_imp$class = as.factor(train_imp$class)
ggplot(train_imp) + geom_density(aes(x = ce_000,col = class)) + theme_bw() + theme(text =element_text(face = "bold"))
ggplot(train_imp) + geom_density(aes(x = yeojohnson(ce_000)$x.t,col = class)) + theme_bw() + theme(text =element_text(face = "bold"))


# skewed?? ?????? yeo-johnson transformation ??Ű??
train_imp_ts = train_imp %>% mutate(
  aa_000 = yeojohnson(aa_000)$x.t,
  ag_003 = yeojohnson(ag_003)$x.t,
  ag_004 = yeojohnson(ag_004)$x.t,
  ag_005 = yeojohnson(ag_005)$x.t,
  ag_006 = yeojohnson(ag_006)$x.t,
  ag_007 = yeojohnson(ag_007)$x.t,
  ah_000 = yeojohnson(ah_000)$x.t,
  al_000 = yeojohnson(al_000)$x.t,
  am_0 = yeojohnson(am_0)$x.t,
  an_000 = yeojohnson(an_000)$x.t,
  ao_000 = yeojohnson(ao_000)$x.t,
  ap_000 = yeojohnson(ap_000)$x.t,
  aq_000 = yeojohnson(aq_000)$x.t,
  av_000 = yeojohnson(av_000)$x.t,
  ax_000 = yeojohnson(ax_000)$x.t,
  ay_005 = yeojohnson(ay_005)$x.t,
  ay_006 = yeojohnson(ay_006)$x.t,
  ay_007 = yeojohnson(ay_007)$x.t,
  ay_008 = yeojohnson(ay_008)$x.t,
  az_000 = yeojohnson(az_000)$x.t,
  az_001 = yeojohnson(az_001)$x.t,
  az_002 = yeojohnson(az_002)$x.t,
  az_003 = yeojohnson(az_003)$x.t,
  az_004 = yeojohnson(az_004)$x.t,
  az_005 = yeojohnson(az_005)$x.t,
  az_006 = yeojohnson(az_006)$x.t,
  
  ba_000 = yeojohnson(ba_000)$x.t,
  ba_001 = yeojohnson(ba_001)$x.t,
  ba_002 = yeojohnson(ba_002)$x.t,
  ba_003 = yeojohnson(ba_003)$x.t,
  ba_004 = yeojohnson(ba_004)$x.t,
  ba_005 = yeojohnson(ba_005)$x.t,
  ba_006 = yeojohnson(ba_006)$x.t,
  ba_007 = yeojohnson(ba_007)$x.t,
  ba_008 = yeojohnson(ba_008)$x.t,
  ba_009 = yeojohnson(ba_009)$x.t,
  bb_000 = yeojohnson(bb_000)$x.t,
  bc_000 = yeojohnson(bc_000)$x.t,
  bd_000 = yeojohnson(bd_000)$x.t,
  be_000 = yeojohnson(be_000)$x.t,
  bf_000 = yeojohnson(bf_000)$x.t,
  bg_000 = yeojohnson(bg_000)$x.t,
  bh_000 = yeojohnson(bh_000)$x.t,
  bi_000 = yeojohnson(bi_000)$x.t,
  bk_000 = yeojohnson(bk_000)$x.t,
  bl_000 = yeojohnson(bl_000)$x.t,
  bs_000 = yeojohnson(bs_000)$x.t,
  bt_000 = yeojohnson(bt_000)$x.t,
  bu_000 = yeojohnson(bu_000)$x.t,
  bv_000 = yeojohnson(bv_000)$x.t,
  bx_000 = yeojohnson(bx_000)$x.t,
  by_000 = yeojohnson(by_000)$x.t,
  bz_000 = yeojohnson(bz_000)$x.t,
  
  ca_000 = yeojohnson(ca_000)$x.t,
  cb_000 = yeojohnson(cb_000)$x.t,
  cc_000 = yeojohnson(cc_000)$x.t,
  ce_000 = yeojohnson(ce_000)$x.t,
  ci_000 = yeojohnson(ci_000)$x.t,
  ck_000 = yeojohnson(ck_000)$x.t,
  cm_000 = yeojohnson(cm_000)$x.t,
  cn_002 = yeojohnson(cn_002)$x.t,
  cn_003 = yeojohnson(cn_003)$x.t,
  cn_004 = yeojohnson(cn_004)$x.t,
  cn_005 = yeojohnson(cn_005)$x.t,
  cn_006 = yeojohnson(cn_006)$x.t,
  cn_007 = yeojohnson(cn_007)$x.t,
  cn_008 = yeojohnson(cn_008)$x.t,
  cn_009 = yeojohnson(cn_009)$x.t,
  cp_000 = yeojohnson(cp_000)$x.t,
  cq_000 = yeojohnson(cq_000)$x.t,
  cs_000 = yeojohnson(cs_000)$x.t,
  cs_001 = yeojohnson(cs_001)$x.t,
  cs_002 = yeojohnson(cs_002)$x.t,
  cs_003 = yeojohnson(cs_003)$x.t,
  cs_004 = yeojohnson(cs_004)$x.t,
  cs_005 = yeojohnson(cs_005)$x.t,
  cs_006 = yeojohnson(cs_006)$x.t,
  cs_007 = yeojohnson(cs_007)$x.t,
  cs_008 = yeojohnson(cs_008)$x.t,
  cs_009 = yeojohnson(cs_009)$x.t,
  ct_000 = yeojohnson(ct_000)$x.t,
  cu_000 = yeojohnson(cu_000)$x.t,
  cv_000 = yeojohnson(cv_000)$x.t,
  cx_000 = yeojohnson(cx_000)$x.t,
  cz_000 = yeojohnson(cz_000)$x.t,
  
  dc_000 = yeojohnson(dc_000)$x.t,
  dd_000 = yeojohnson(dd_000)$x.t,
  de_000 = yeojohnson(de_000)$x.t,
  dn_000 = yeojohnson(dn_000)$x.t,
  do_000 = yeojohnson(do_000)$x.t,
  dp_000 = yeojohnson(dp_000)$x.t,
  ds_000 = yeojohnson(ds_000)$x.t,
  dt_000 = yeojohnson(dt_000)$x.t,
  du_000 = yeojohnson(du_000)$x.t,
  dv_000 = yeojohnson(dv_000)$x.t,
  
  ec_00 = yeojohnson(ec_00)$x.t,
  ed_000 = yeojohnson(ed_000)$x.t,
  ee_000 = yeojohnson(ee_000)$x.t,
  ee_001 = yeojohnson(ee_001)$x.t,
  ee_002 = yeojohnson(ee_002)$x.t,
  ee_003 = yeojohnson(ee_003)$x.t,
  ee_004 = yeojohnson(ee_004)$x.t,
  ee_005 = yeojohnson(ee_005)$x.t,
  ee_006 = yeojohnson(ee_006)$x.t,
  ee_007 = yeojohnson(ee_007)$x.t,
  ee_008 = yeojohnson(ee_008)$x.t,
)

train_imp_ts %>% is.na() %>% sum

write_csv(train_imp_ts,'train_imp_ts.csv')

# --- 4. ?????? ?ұ??? ó??: SMOTE
train_imp_ts$class = as.factor(train_imp_ts$class)
set.seed(123)
train_imp_ts_sm = SMOTE(class ~ ., data = train_imp_ts, 
                 perc.over = 200, k = 5, perc.under = 300)

# target ???? ???? Ȯ??
table(train_imp_ts_sm$class)

###########################
###4. ??ó?? ?????? ????###
###########################

write_csv(train_imp_ts_sm,'train_imp_ts_sm.csv')
rm(list = ls())

end_time = Sys.time()
end_time - start_time