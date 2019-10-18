#Supplemental Material 6：解析に用いたRコード
### データの読み込み ###
CI_data = read.csv("data.csv", header = T)
# 割り当て変数zの定義
ivec1 = CI_data$cm_dummy #処置群（CMを見た群）を示すベクトル
ivec0 = rep(1, nrow(CI_data))-ivec1　#対照群（CMを見ていない群）を示すベクトル

## 傾向スコア(logi$fitted)の推定 ##
logi=glm(cm_dummy ~ TVwatch_day + age + sex + marry_dummy + child_dummy + inc + pmoney + area_kanto +area_tokai + area_keihanshin + job_dummy1 + job_dummy2 + job_dummy3 + job_dummy4 + job_dummy5 + job_dummy6 + job_dummy7  + fam_str_dummy1 + fam_str_dummy2 + fam_str_dummy3 + fam_str_dummy4, family=binomial(link="logit") , data = CI_data)
CI_data2 = data.frame(CI_data, logi$fitted) # 傾向スコアデータの結合

CI_data_treated = subset(CI_data2, CI_data$cm_dummy==1)　# 処置群のみのデータ
CI_data_untreated = subset(CI_data2, CI_data$cm_dummy==0)　# 対照群のみのデータ

###########################################
### IPW調整前の各変数の平均/標準偏差(表2) ###
###########################################
# 処置群／gamedummy
aveT_gd = mean(CI_data_treated$gamedummy) 
sdT_gd = sd(CI_data_treated$gamedummy)
# 対照群／gamedummy
aveU_gd = mean(CI_data_untreated$gamedummy)
sdU_gd = sd(CI_data_untreated$gamedummy)
# 差
diff_gd = aveT_gd - aveU_gd
# 処置群／gamecount
aveT_gc = mean(CI_data_treated$gamecount)
sdT_gc = sd(CI_data_treated$gamecount)
# 対照群／gamecount
aveU_gc = mean(CI_data_untreated$gamecount)
sdU_gc = sd(CI_data_untreated$gamecount)
# 差
diff_gc = aveT_gc - aveU_gc
# 処置群／gamesecond
aveT_gs = mean(CI_data_treated$gamesecond)
sdT_gs = sd(CI_data_treated$gamesecond)
# 対照群／gamesecond
aveU_gs = mean(CI_data_untreated$gamesecond)
sdU_gs = sd(CI_data_untreated$gamesecond)
# 差
diff_gs = aveT_gs - aveU_gs

#######################
### 平均処置効果(表4) ###
#######################
ivec = cbind(ivec1,ivec0)
iestp1 = (ivec1/logi$fitted)
iestp0 = (ivec0/(1-logi$fitted))
iestp = iestp1+iestp0

ipwe_gd = lm(CI_data$gamedummy ~ ivec+0, weights=iestp)
summary(ipwe_gd)
ipwe_gc = lm(CI_data$gamecount ~ ivec+0, weights=iestp)
summary(ipwe_gc)
ipwe_gs = lm(CI_data$gamesecond ~ ivec+0, weights=iestp)
summary(ipwe_gs)

###################################
## 処置群における平均処置効果(表5)###
###################################
iestp1_ATT = ivec1
iestp0_ATT = ivec0*logi$fitted/(1-logi$fitted)
iestp_ATT = iestp1_ATT+iestp0_ATT
ipwe_ATT_gd = lm(CI_data$gamedummy ~ ivec+0, weights=iestp_ATT)
summary(ipwe_ATT_gd)
ipwe_ATT_gc = lm(CI_data$gamecount ~ ivec+0, weights=iestp_ATT)
summary(ipwe_ATT_gc)
ipwe_ATT_gs = lm(CI_data$gamesecond ~ ivec+0, weights=iestp_ATT)
summary(ipwe_ATT_gs)

############################################
## ゲーム利用秒数に関する調整効果の確認(表6) ##
############################################
# 線形回帰の場合
ME_treated_gamesecond = lm(gamesecond ~ child_dummy + area_kanto +area_tokai + area_keihanshin + T + F1 + F2 + F3 + M1 + M2 ,weights=(1/logi.fitted) ,data=CI_data_treated)
ME_untreated_gamesecond = lm(gamesecond ~ child_dummy + area_kanto +area_tokai + area_keihanshin + T + F1 + F2 + F3 + M1 + M2,weights=(1/(1-logi.fitted)), data=CI_data_untreated)
summary(ME_treated_gamesecond)
summary(ME_untreated_gamesecond)