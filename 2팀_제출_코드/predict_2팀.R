###################################################
#######1. ?????丮, ???̺귯?? ?ҷ??��? ?ڵ?#######
###################################################

setwd('D:/??ȸ/???м??̳?/??��')
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

library(mice)

start_time = Sys.time()


################################
##2. ?????? ?ҷ??��? ?? ??ó??##
################################

test = fread('test_feature.csv',data.table = FALSE)
dim(test) # 8000 170

# --- 1. train???? ��???? ???? test?????? ��???ϱ?
train = fread('train_delete.csv',data.table = FALSE)
deleted_var = setdiff(colnames(test),colnames(train))

test1 = test %>% select(-deleted_var)
train %>% dim() # 39764 162
test1 %>% dim() # 8000 161

write_csv(test1, "test_delete.csv")

# --- 2. integer64 -> numeric��?? ?? ??ȯ
test2 = test1 %>% mutate_if(bit64::is.integer64,as.numeric)

# --- 3. ????ġ ó??: MICE
test2 %>% dim() # 8000 161
test2 %>% is.na() %>% colSums() %>% as.data.frame() # aa_000�� ????ġ ??��

set.seed(123)
imp = mice(test2, m = 1, method = c('',rep('cart',160)), 
            maxit = 3, remove.collinear = F)

test_imp = complete(imp)
test_imp %>% is.na() %>% sum()

write_csv(test_imp, "test_imp.csv")

# --- 4. Variable Transformation
test_imp_ts = test_imp %>% mutate(
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

write_csv(test_imp_ts,'test_imp_ts.csv')


######################
##3. Test Prediction##
######################

test = fread("test_imp_ts.csv",data.table = F)
load('final_model.Rdata')

rf.pred = predict(rf_mod, as.matrix(test))
pre_rf <- rep("neg",nrow(test))
pre_rf[rf.pred >= 0.5]='pos'

result_rf_tuning <- 1:8000
result_rf_tuning <- cbind(result_rf_tuning, pre_rf)
colnames(result_rf_tuning)<- c("id", "class")
result_rf_tuning <- data.frame(result_rf_tuning)

# yhat ???? Ȯ?? 0.952875 0.047125
table(result_rf_tuning$class) / nrow(result_rf_tuning)


end_time = Sys.time()

end_time - start_time
##########################
##4. ??�� ��?? ???? ????##
##########################

write_csv(result_rf_tuning, "result_rf_tuning.csv")
write_csv(result_rf_tuning, "result_rf_tuning_2.csv")
