#-------------------------------------------------------------#
#라이브러리
pacman::p_load(gamm4,mgcv,lme4,splines,ggplot2,gridExtra,dplyr,lubridate,scales,RColorBrewer)

#-------------------------------------------------------------#
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담")
daily_death_final<-read.csv("daily_death_final.csv")

daily_death_final$ddate=ymd(daily_death_final$ddate)
daily_death_final$sido_KN =with(daily_death_final,factor(sido_KN,levels=unique(sido_KN)))
daily_death_final$sidoname=with(daily_death_final,factor(sidoname,levels=unique(sidoname)))

#원시자료 변수 편집, 모델링에 사용한 변수 위주
raw<-daily_death_final %>% select(key:TOT,NON_ACC,CVD,RESP,
                                  sn:dowfirstday,
                                  meantemp,meantemp_m1:meantemp_m7,
                                  meanhumi,meanhumi_m1:meanhumi_m7,
                                  simpat,simpat_m1:simpat_m7,
                                  dewtemp,dewtemp_m1:dewtemp_m7,
                                  pm25_new,pm25_new_m1:pm25_new_m7,
                                  pm10,pm10_m1:pm10_m7,
                                  so2,so2_m1:so2_m7,
                                  no2,no2_m1:no2_m7,
                                  co, co_m1:co_m7,
                                  o3, o3_m1:o3_m7)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#비선형 가정, 모델링 결과 불러오기 ; by gamm4

#자료 저장 디렉토리 
dir1="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\gamm4\\all_cause"
dir2="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\gamm4\\nonacc"
dir3="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\gamm4\\cvd"
dir4="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\gamm4\\respiratory"

dir=c(dir1,dir2,dir3,dir4)

#노출 변수
exposure=c("pm25","pm10","so2","no2","co","o3")

#사망 변수
dth=c("TOT","NON_ACC","CVD","RESP")

#파일 노출별*사망원인별 조합으로 함수 출력할 수 있도록하기 
# 4 * 6

index_list<-paste0(rep(c(1:4),each=6),1:6)

#최종결과 저장할 리스트
#(1) 초과사망 결과 저장 
#(2) 노출반응 결과 저장 

#-------------------------------------------------------------------------------#
#여기서 아마 다른 대기오염 (SO2~O3)에 대한 단위 조정 해야할듯 
#노출 농도 젤 높은 수준까지 만들어 놓기
#당일 노출이 이동평균 농도보다 높을테니 최대 범위까지 고려해서
#예측값 만들어 주고 원래자료에 해당되는 농도 범위내에서만 자료 merge
#주의) SO2, NO2, CO, O3는 모델링시 ppm 단위로 예측 했음 (단위변환 고려하기!!)

#-------------------------------------------------------------------------------#
pred_data_pm25<-pred_data_func(max(raw$pm25_new,na.rm=T))
pred_data_pm10<-pred_data_func(max(raw$pm10,na.rm=T))
pred_data_so2 <-pred_data_func(max(raw$so2,na.rm=T)*1000)
pred_data_no2 <-pred_data_func(max(raw$no2,na.rm=T)*1000)
pred_data_co  <-pred_data_func(max(raw$co,na.rm=T)*1000)
pred_data_o3  <-pred_data_func(max(raw$o3,na.rm=T)*1000)

#PM2.5 노출 최대값까지 반영
pred_data_pm25$pm25_new   =seq(max(raw$pm25_new,na.rm=T))
pred_data_pm25$pm25_new_m1=seq(max(raw$pm25_new,na.rm=T))
pred_data_pm25$pm25_new_m2=seq(max(raw$pm25_new,na.rm=T))
pred_data_pm25$pm25_new_m3=seq(max(raw$pm25_new,na.rm=T))
pred_data_pm25$pm25_new_m4=seq(max(raw$pm25_new,na.rm=T))
pred_data_pm25$pm25_new_m5=seq(max(raw$pm25_new,na.rm=T))
pred_data_pm25$pm25_new_m6=seq(max(raw$pm25_new,na.rm=T))
pred_data_pm25$pm25_new_m7=seq(max(raw$pm25_new,na.rm=T))

#PM10 노출 최대값까지 반영
pred_data_pm10$pm10   =seq(max(raw$pm10,na.rm=T))
pred_data_pm10$pm10_m1=seq(max(raw$pm10,na.rm=T))
pred_data_pm10$pm10_m2=seq(max(raw$pm10,na.rm=T))
pred_data_pm10$pm10_m3=seq(max(raw$pm10,na.rm=T))
pred_data_pm10$pm10_m4=seq(max(raw$pm10,na.rm=T))
pred_data_pm10$pm10_m5=seq(max(raw$pm10,na.rm=T))
pred_data_pm10$pm10_m6=seq(max(raw$pm10,na.rm=T))
pred_data_pm10$pm10_m7=seq(max(raw$pm10,na.rm=T))

#SO2 노출 최대값까지 반영
pred_data_so2$so2   =seq(max(raw$so2,na.rm=T)*1000)/1000
pred_data_so2$so2_m1=seq(max(raw$so2,na.rm=T)*1000)/1000
pred_data_so2$so2_m2=seq(max(raw$so2,na.rm=T)*1000)/1000
pred_data_so2$so2_m3=seq(max(raw$so2,na.rm=T)*1000)/1000
pred_data_so2$so2_m4=seq(max(raw$so2,na.rm=T)*1000)/1000
pred_data_so2$so2_m5=seq(max(raw$so2,na.rm=T)*1000)/1000
pred_data_so2$so2_m6=seq(max(raw$so2,na.rm=T)*1000)/1000
pred_data_so2$so2_m7=seq(max(raw$so2,na.rm=T)*1000)/1000

#NO2 노출 최대값까지 반영
pred_data_no2$no2   =seq(max(raw$no2,na.rm=T)*1000)/1000
pred_data_no2$no2_m1=seq(max(raw$no2,na.rm=T)*1000)/1000
pred_data_no2$no2_m2=seq(max(raw$no2,na.rm=T)*1000)/1000
pred_data_no2$no2_m3=seq(max(raw$no2,na.rm=T)*1000)/1000
pred_data_no2$no2_m4=seq(max(raw$no2,na.rm=T)*1000)/1000
pred_data_no2$no2_m5=seq(max(raw$no2,na.rm=T)*1000)/1000
pred_data_no2$no2_m6=seq(max(raw$no2,na.rm=T)*1000)/1000
pred_data_no2$no2_m7=seq(max(raw$no2,na.rm=T)*1000)/1000

#CO 노출 최대값까지 반영
pred_data_co$co   =seq(max(raw$co,na.rm=T)*1000)/1000
pred_data_co$co_m1=seq(max(raw$co,na.rm=T)*1000)/1000
pred_data_co$co_m2=seq(max(raw$co,na.rm=T)*1000)/1000
pred_data_co$co_m3=seq(max(raw$co,na.rm=T)*1000)/1000
pred_data_co$co_m4=seq(max(raw$co,na.rm=T)*1000)/1000
pred_data_co$co_m5=seq(max(raw$co,na.rm=T)*1000)/1000
pred_data_co$co_m6=seq(max(raw$co,na.rm=T)*1000)/1000
pred_data_co$co_m7=seq(max(raw$co,na.rm=T)*1000)/1000

#O3 노출 최대값까지 반영
pred_data_o3$o3   =seq(max(raw$o3,na.rm=T)*1000)/1000
pred_data_o3$o3_m1=seq(max(raw$o3,na.rm=T)*1000)/1000
pred_data_o3$o3_m2=seq(max(raw$o3,na.rm=T)*1000)/1000
pred_data_o3$o3_m3=seq(max(raw$o3,na.rm=T)*1000)/1000
pred_data_o3$o3_m4=seq(max(raw$o3,na.rm=T)*1000)/1000
pred_data_o3$o3_m5=seq(max(raw$o3,na.rm=T)*1000)/1000
pred_data_o3$o3_m6=seq(max(raw$o3,na.rm=T)*1000)/1000
pred_data_o3$o3_m7=seq(max(raw$o3,na.rm=T)*1000)/1000

#농도 구간별로 예측할 자료를 리스트화 시키기 
pred_data_list=NULL
pred_data_list[[1]]<-pred_data_pm25
pred_data_list[[2]]<-pred_data_pm10
pred_data_list[[3]]<-pred_data_so2
pred_data_list[[4]]<-pred_data_no2
pred_data_list[[5]]<-pred_data_co
pred_data_list[[6]]<-pred_data_o3

#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
result=NULL
gamm4_nonlinear_excess_mor<-function(index,unit){
  
  m=as.numeric(substr(index,1,1)) #mortality에 대한 부분
  e=as.numeric(substr(index,2,2)) #exposure에 대한 부분 
  
  #현재 위치 파일 리스트
  setwd(dir[m])
  lf<-list.files()
  lf[grep(exposure[e],lf)] #노출 물질별 lag0~lag07 결과 
  
  #GAMM4 비선형으로 모델링한 결과 불러오기 
  #비선형을 가정으로 health impact 산출
  #각 노출 lag별로 Prediction
  
  gamm4_m0<-readRDS(lf[grep(exposure[e],lf)][1])
  pred0 <- predict(gamm4_m0$gam, newdata=pred_data_list[[e]], type = "terms", se.fit = TRUE);rm(gamm4_m0)
  
  gamm4_m1<-readRDS(lf[grep(exposure[e],lf)][2])
  pred1 <- predict(gamm4_m1$gam, newdata=pred_data_list[[e]], type = "terms", se.fit = TRUE);rm(gamm4_m1)
  
  gamm4_m2<-readRDS(lf[grep(exposure[e],lf)][3])
  pred2 <- predict(gamm4_m2$gam, newdata=pred_data_list[[e]], type = "terms", se.fit = TRUE);rm(gamm4_m2)
  
  gamm4_m3<-readRDS(lf[grep(exposure[e],lf)][4])
  pred3 <- predict(gamm4_m3$gam, newdata=pred_data_list[[e]], type = "terms", se.fit = TRUE);rm(gamm4_m3)
  
  gamm4_m4<-readRDS(lf[grep(exposure[e],lf)][5])
  pred4 <- predict(gamm4_m4$gam, newdata=pred_data_list[[e]], type = "terms", se.fit = TRUE);rm(gamm4_m4)
  
  gamm4_m5<-readRDS(lf[grep(exposure[e],lf)][6])
  pred5 <- predict(gamm4_m5$gam, newdata=pred_data_list[[e]], type = "terms", se.fit = TRUE);rm(gamm4_m5)
  
  gamm4_m6<-readRDS(lf[grep(exposure[e],lf)][7])
  pred6 <- predict(gamm4_m6$gam, newdata=pred_data_list[[e]], type = "terms", se.fit = TRUE);rm(gamm4_m6)
  
  gamm4_m7<-readRDS(lf[grep(exposure[e],lf)][8])
  pred7 <- predict(gamm4_m7$gam, newdata=pred_data_list[[e]], type = "terms", se.fit = TRUE);rm(gamm4_m7)
  
  #spline term에 있는 (즉, 비선형으로 추정한 노출)에 해당하는 추정값 반환 
  #적용전에 두번째 회귀계수가 노출에 대한 것인지 검토하기!!
  beta0<-as.data.frame(cbind(beta=pred0$fit[,2],beta_lci=pred0$fit[,2]-1.96*pred0$se.fit[,2],beta_uci=pred0$fit[,2]+1.96*pred0$se.fit[,2]))
  beta1<-as.data.frame(cbind(beta=pred1$fit[,2],beta_lci=pred1$fit[,2]-1.96*pred1$se.fit[,2],beta_uci=pred1$fit[,2]+1.96*pred1$se.fit[,2]))
  beta2<-as.data.frame(cbind(beta=pred2$fit[,2],beta_lci=pred2$fit[,2]-1.96*pred2$se.fit[,2],beta_uci=pred2$fit[,2]+1.96*pred2$se.fit[,2]))
  beta3<-as.data.frame(cbind(beta=pred3$fit[,2],beta_lci=pred3$fit[,2]-1.96*pred3$se.fit[,2],beta_uci=pred3$fit[,2]+1.96*pred3$se.fit[,2]))
  beta4<-as.data.frame(cbind(beta=pred4$fit[,2],beta_lci=pred4$fit[,2]-1.96*pred4$se.fit[,2],beta_uci=pred4$fit[,2]+1.96*pred4$se.fit[,2]))
  beta5<-as.data.frame(cbind(beta=pred5$fit[,2],beta_lci=pred5$fit[,2]-1.96*pred5$se.fit[,2],beta_uci=pred5$fit[,2]+1.96*pred5$se.fit[,2]))
  beta6<-as.data.frame(cbind(beta=pred6$fit[,2],beta_lci=pred6$fit[,2]-1.96*pred6$se.fit[,2],beta_uci=pred6$fit[,2]+1.96*pred6$se.fit[,2]))
  beta7<-as.data.frame(cbind(beta=pred7$fit[,2],beta_lci=pred7$fit[,2]-1.96*pred7$se.fit[,2],beta_uci=pred7$fit[,2]+1.96*pred7$se.fit[,2]))
  
  #standard error 복원
  beta0$beta_se=-with(beta0,(beta_lci-beta)/1.96);beta1$beta_se=-with(beta1,(beta_lci-beta)/1.96)
  beta2$beta_se=-with(beta2,(beta_lci-beta)/1.96);beta3$beta_se=-with(beta3,(beta_lci-beta)/1.96)
  beta4$beta_se=-with(beta4,(beta_lci-beta)/1.96);beta5$beta_se=-with(beta5,(beta_lci-beta)/1.96)
  beta6$beta_se=-with(beta6,(beta_lci-beta)/1.96);beta7$beta_se=-with(beta7,(beta_lci-beta)/1.96)
  
  #노출별로 베타값 산출: 
  #노출농도 단위 맞춰주기 (PM은 상관없는데 가스상 오염물질 때매)
  exp_beta0<-beta0 %>% select(beta,beta_se) %>% mutate(exp=1:nrow(beta0),beta=beta,beta_se=beta_se)
  exp_beta1<-beta1 %>% select(beta,beta_se) %>% mutate(exp=1:nrow(beta1),beta=beta,beta_se=beta_se)
  exp_beta2<-beta2 %>% select(beta,beta_se) %>% mutate(exp=1:nrow(beta2),beta=beta,beta_se=beta_se)
  exp_beta3<-beta3 %>% select(beta,beta_se) %>% mutate(exp=1:nrow(beta3),beta=beta,beta_se=beta_se)
  exp_beta4<-beta4 %>% select(beta,beta_se) %>% mutate(exp=1:nrow(beta4),beta=beta,beta_se=beta_se)
  exp_beta5<-beta5 %>% select(beta,beta_se) %>% mutate(exp=1:nrow(beta5),beta=beta,beta_se=beta_se)
  exp_beta6<-beta6 %>% select(beta,beta_se) %>% mutate(exp=1:nrow(beta6),beta=beta,beta_se=beta_se)
  exp_beta7<-beta7 %>% select(beta,beta_se) %>% mutate(exp=1:nrow(beta7),beta=beta,beta_se=beta_se)
  
  #노출 농도를 정수로 표현 
  a0<-raw;a0$exp=unlist(raw[grep(exposure[e],names(raw))[1]]*unit) %>% round
  a1<-raw;a1$exp=unlist(raw[grep(exposure[e],names(raw))[2]]*unit) %>% round
  a2<-raw;a2$exp=unlist(raw[grep(exposure[e],names(raw))[3]]*unit) %>% round
  a3<-raw;a3$exp=unlist(raw[grep(exposure[e],names(raw))[4]]*unit) %>% round
  a4<-raw;a4$exp=unlist(raw[grep(exposure[e],names(raw))[5]]*unit) %>% round
  a5<-raw;a5$exp=unlist(raw[grep(exposure[e],names(raw))[6]]*unit) %>% round
  a6<-raw;a6$exp=unlist(raw[grep(exposure[e],names(raw))[7]]*unit) %>% round
  a7<-raw;a7$exp=unlist(raw[grep(exposure[e],names(raw))[8]]*unit) %>% round
  
  #노출 농도별 beta값 
  am0<-a0 %>% left_join(exp_beta0,by="exp") %>% dplyr::select(ddate:year,TOT:RESP,sn,dow,sido_KN,exp,beta:beta_se)
  am1<-a1 %>% left_join(exp_beta1,by="exp") %>% dplyr::select(ddate:year,TOT:RESP,sn,dow,sido_KN,exp,beta:beta_se)
  am2<-a2 %>% left_join(exp_beta2,by="exp") %>% dplyr::select(ddate:year,TOT:RESP,sn,dow,sido_KN,exp,beta:beta_se)
  am3<-a3 %>% left_join(exp_beta3,by="exp") %>% dplyr::select(ddate:year,TOT:RESP,sn,dow,sido_KN,exp,beta:beta_se)
  am4<-a4 %>% left_join(exp_beta4,by="exp") %>% dplyr::select(ddate:year,TOT:RESP,sn,dow,sido_KN,exp,beta:beta_se)
  am5<-a5 %>% left_join(exp_beta5,by="exp") %>% dplyr::select(ddate:year,TOT:RESP,sn,dow,sido_KN,exp,beta:beta_se)
  am6<-a6 %>% left_join(exp_beta6,by="exp") %>% dplyr::select(ddate:year,TOT:RESP,sn,dow,sido_KN,exp,beta:beta_se)
  am7<-a7 %>% left_join(exp_beta7,by="exp") %>% dplyr::select(ddate:year,TOT:RESP,sn,dow,sido_KN,exp,beta:beta_se)
  
  #시나리오1: 기준 농도 이하는 건강영향 없다고 가정하기
  pm25_ref=15
  pm10_ref=45
  #만들어 놓은 예측 자료 ppb 단위 증가로 만들어놔서 단위 변환 
  so2_ref =40 *24.5/64.07 #ppb
  no2_ref =25 *24.5/46.01 #ppb
  co_ref  =3.492*1000     #ppb
  o3_ref  =60 *24.5/48    #ppb
  
  #시나리오 1의 대기오염 기준 농도 가스상 물질은 ppb 단위
  exp_ref=c(15,45,15,13,3492,30)
  
  #시나리오 1, 대기오염 기준농도에 해당하는 lag별 beta계수
  beta0_ref=subset(exp_beta0,exp==exp_ref[e])$beta;beta1_ref=subset(exp_beta1,exp==exp_ref[e])$beta
  beta2_ref=subset(exp_beta2,exp==exp_ref[e])$beta;beta3_ref=subset(exp_beta3,exp==exp_ref[e])$beta
  beta4_ref=subset(exp_beta4,exp==exp_ref[e])$beta;beta5_ref=subset(exp_beta5,exp==exp_ref[e])$beta
  beta6_ref=subset(exp_beta6,exp==exp_ref[e])$beta;beta7_ref=subset(exp_beta7,exp==exp_ref[e])$beta
  
  #시나리오1에서는 기준 농도 이하는 건강영향 없다고 가정
  #노출 농도가 기준농도보다 작은 경우 beta를 0으로 만들어주기 
  am0$delta_beta1=with(am0,ifelse(exp<=exp_ref[e],0,beta-beta0_ref))
  am1$delta_beta1=with(am1,ifelse(exp<=exp_ref[e],0,beta-beta1_ref))
  am2$delta_beta1=with(am2,ifelse(exp<=exp_ref[e],0,beta-beta2_ref))
  am3$delta_beta1=with(am3,ifelse(exp<=exp_ref[e],0,beta-beta3_ref))
  am4$delta_beta1=with(am4,ifelse(exp<=exp_ref[e],0,beta-beta4_ref))
  am5$delta_beta1=with(am5,ifelse(exp<=exp_ref[e],0,beta-beta5_ref))
  am6$delta_beta1=with(am6,ifelse(exp<=exp_ref[e],0,beta-beta6_ref))
  am7$delta_beta1=with(am7,ifelse(exp<=exp_ref[e],0,beta-beta7_ref))
  
  #시나리오 1 기준농도 에 따른 신뢰하한
  am0$delta_beta1_lci=with(am0,ifelse(exp<=exp_ref[e],0,(beta-beta0_ref)-1.96*beta_se))
  am1$delta_beta1_lci=with(am1,ifelse(exp<=exp_ref[e],0,(beta-beta1_ref)-1.96*beta_se))
  am2$delta_beta1_lci=with(am2,ifelse(exp<=exp_ref[e],0,(beta-beta2_ref)-1.96*beta_se))
  am3$delta_beta1_lci=with(am3,ifelse(exp<=exp_ref[e],0,(beta-beta3_ref)-1.96*beta_se))
  am4$delta_beta1_lci=with(am4,ifelse(exp<=exp_ref[e],0,(beta-beta4_ref)-1.96*beta_se))
  am5$delta_beta1_lci=with(am5,ifelse(exp<=exp_ref[e],0,(beta-beta5_ref)-1.96*beta_se))
  am6$delta_beta1_lci=with(am6,ifelse(exp<=exp_ref[e],0,(beta-beta6_ref)-1.96*beta_se))
  am7$delta_beta1_lci=with(am7,ifelse(exp<=exp_ref[e],0,(beta-beta7_ref)-1.96*beta_se))
  
  #시나리오 1 기준농도 에 따른 신뢰상한 
  am0$delta_beta1_uci=with(am0,ifelse(exp<=exp_ref[e],0,(beta-beta0_ref)+1.96*beta_se))
  am1$delta_beta1_uci=with(am1,ifelse(exp<=exp_ref[e],0,(beta-beta1_ref)+1.96*beta_se))
  am2$delta_beta1_uci=with(am2,ifelse(exp<=exp_ref[e],0,(beta-beta2_ref)+1.96*beta_se))
  am3$delta_beta1_uci=with(am3,ifelse(exp<=exp_ref[e],0,(beta-beta3_ref)+1.96*beta_se))
  am4$delta_beta1_uci=with(am4,ifelse(exp<=exp_ref[e],0,(beta-beta4_ref)+1.96*beta_se))
  am5$delta_beta1_uci=with(am5,ifelse(exp<=exp_ref[e],0,(beta-beta5_ref)+1.96*beta_se))
  am6$delta_beta1_uci=with(am6,ifelse(exp<=exp_ref[e],0,(beta-beta6_ref)+1.96*beta_se))
  am7$delta_beta1_uci=with(am7,ifelse(exp<=exp_ref[e],0,(beta-beta7_ref)+1.96*beta_se))
  
  #-----------------------------------------------------------------------------------------#
  #-----------------------------------------------------------------------------------------#
  
  #시나리오2: 모든 농도에서 보고 싶을 때
  #각 대기오염물질에 대한 이동평균별 최저 농도로 바꿔주기 
  #노출 농도가 최저인 지점
  beta0_ref2=subset(exp_beta0,exp==ifelse(min(am0$exp,na.rm=T)==0,1,min(am0$exp,na.rm=T)))$beta
  beta1_ref2=subset(exp_beta1,exp==ifelse(min(am1$exp,na.rm=T)==0,1,min(am1$exp,na.rm=T)))$beta
  beta2_ref2=subset(exp_beta2,exp==ifelse(min(am2$exp,na.rm=T)==0,1,min(am2$exp,na.rm=T)))$beta
  beta3_ref2=subset(exp_beta3,exp==ifelse(min(am3$exp,na.rm=T)==0,1,min(am3$exp,na.rm=T)))$beta
  beta4_ref2=subset(exp_beta4,exp==ifelse(min(am4$exp,na.rm=T)==0,1,min(am4$exp,na.rm=T)))$beta
  beta5_ref2=subset(exp_beta5,exp==ifelse(min(am5$exp,na.rm=T)==0,1,min(am5$exp,na.rm=T)))$beta
  beta6_ref2=subset(exp_beta6,exp==ifelse(min(am6$exp,na.rm=T)==0,1,min(am6$exp,na.rm=T)))$beta
  beta7_ref2=subset(exp_beta7,exp==ifelse(min(am7$exp,na.rm=T)==0,1,min(am7$exp,na.rm=T)))$beta
  
  #노출 농도별, lag 지연별 노출 비교 
  am0$delta_beta2=with(am0,beta-beta0_ref2);am1$delta_beta2=with(am1,beta-beta1_ref2)
  am2$delta_beta2=with(am2,beta-beta2_ref2);am3$delta_beta2=with(am3,beta-beta3_ref2)
  am4$delta_beta2=with(am4,beta-beta4_ref2);am5$delta_beta2=with(am5,beta-beta5_ref2)
  am6$delta_beta2=with(am6,beta-beta6_ref2);am7$delta_beta2=with(am7,beta-beta7_ref2)
  
  #시나리오 2 기준농도에 따른 신뢰하한
  am0$delta_beta2_lci=with(am0,(beta-beta0_ref2)-1.96*beta_se)
  am1$delta_beta2_lci=with(am1,(beta-beta1_ref2)-1.96*beta_se)
  am2$delta_beta2_lci=with(am2,(beta-beta2_ref2)-1.96*beta_se)
  am3$delta_beta2_lci=with(am3,(beta-beta3_ref2)-1.96*beta_se)
  am4$delta_beta2_lci=with(am4,(beta-beta4_ref2)-1.96*beta_se)
  am5$delta_beta2_lci=with(am5,(beta-beta5_ref2)-1.96*beta_se)
  am6$delta_beta2_lci=with(am6,(beta-beta6_ref2)-1.96*beta_se)
  am7$delta_beta2_lci=with(am7,(beta-beta7_ref2)-1.96*beta_se)
  
  #시나리오 2 기준농도 에 따른 신뢰상한 
  am0$delta_beta2_uci=with(am0,(beta-beta0_ref2)+1.96*beta_se)
  am1$delta_beta2_uci=with(am1,(beta-beta1_ref2)+1.96*beta_se)
  am2$delta_beta2_uci=with(am2,(beta-beta2_ref2)+1.96*beta_se)
  am3$delta_beta2_uci=with(am3,(beta-beta3_ref2)+1.96*beta_se)
  am4$delta_beta2_uci=with(am4,(beta-beta4_ref2)+1.96*beta_se)
  am5$delta_beta2_uci=with(am5,(beta-beta5_ref2)+1.96*beta_se)
  am6$delta_beta2_uci=with(am6,(beta-beta6_ref2)+1.96*beta_se)
  am7$delta_beta2_uci=with(am7,(beta-beta7_ref2)+1.96*beta_se)
  
  #-----------------------------------------------------------------------------------------#
  #위에서 산출한 결과를 가지고 초과사망 계산
  excess_func<-function(data){
    
    dd<-data
    dd$death1    =with(dd,(1-1/exp(delta_beta1))*unlist(dd[dth[m]]))
    dd$death1_lci=with(dd,(1-1/exp(delta_beta1_lci))*unlist(dd[dth[m]]))
    dd$death1_uci=with(dd,(1-1/exp(delta_beta1_uci))*unlist(dd[dth[m]]))
    
    dd$death2    =with(dd,(1-1/exp(delta_beta2))*unlist(dd[dth[m]]))
    dd$death2_lci=with(dd,(1-1/exp(delta_beta2_lci))*unlist(dd[dth[m]]))
    dd$death2_uci=with(dd,(1-1/exp(delta_beta2_uci))*unlist(dd[dth[m]]))
    
    as.data.frame(cbind(aggregate(death1~year,dd,sum),
                        death1_lci=aggregate(death1_lci~year,dd,sum)[,2],
                        death1_uci=aggregate(death1_uci~year,dd,sum)[,2],
                        death2    =aggregate(death2~year,dd,sum)[,2],
                        death2_lci=aggregate(death2_lci~year,dd,sum)[,2],
                        death2_uci=aggregate(death2_uci~year,dd,sum)[,2]))
  }
  
  excess_am0<-excess_func(am0);excess_am0$exposure=exposure[e];excess_am0$lag="lag00"
  excess_am1<-excess_func(am1);excess_am1$exposure=exposure[e];excess_am1$lag="lag01"
  excess_am2<-excess_func(am2);excess_am2$exposure=exposure[e];excess_am2$lag="lag02"
  excess_am3<-excess_func(am3);excess_am3$exposure=exposure[e];excess_am3$lag="lag03"
  excess_am4<-excess_func(am4);excess_am4$exposure=exposure[e];excess_am4$lag="lag04"
  excess_am5<-excess_func(am5);excess_am5$exposure=exposure[e];excess_am5$lag="lag05"
  excess_am6<-excess_func(am6);excess_am6$exposure=exposure[e];excess_am6$lag="lag06"
  excess_am7<-excess_func(am7);excess_am7$exposure=exposure[e];excess_am7$lag="lag07"
  
  #초과사망 산출한 결과/ 연도별
  excess_am0$outcome=dth[m];excess_am1$outcome=dth[m];excess_am2$outcome=dth[m];excess_am3$outcome=dth[m]
  excess_am4$outcome=dth[m];excess_am5$outcome=dth[m];excess_am6$outcome=dth[m];excess_am7$outcome=dth[m]
  
  #각 노출지연별 결과 merge
  excess_result<-rbind(excess_am0,excess_am1,excess_am2,excess_am3,
                       excess_am4,excess_am5,excess_am6,excess_am7)
  
  #노출 지연별, 농도-반응 그림 같이 그려서 보기 
  beta_lag_comp<-function(data){
    d<-data
    d %>% select(exp,beta,beta_se,
                 delta_beta1,delta_beta1_lci,delta_beta1_uci,
                 delta_beta2,delta_beta2_lci,delta_beta2_uci) %>% 
      group_by(exp) %>% summarise(beta    =mean(beta),
                                  beta_se =mean(beta_se),
                                  del1    =mean(delta_beta1),
                                  del1_lci=mean(delta_beta1_lci),
                                  del1_uci=mean(delta_beta1_uci),
                                  del2    =mean(delta_beta2),
                                  del2_lci=mean(delta_beta2_lci),
                                  del2_uci=mean(delta_beta2_uci))}
  
  cr_lag0<-beta_lag_comp(am0);cr_lag0$lag="lag0";cr_lag1<-beta_lag_comp(am1);cr_lag1$lag="lag1"
  cr_lag2<-beta_lag_comp(am2);cr_lag2$lag="lag2";cr_lag3<-beta_lag_comp(am3);cr_lag3$lag="lag3"
  cr_lag4<-beta_lag_comp(am4);cr_lag4$lag="lag4";cr_lag5<-beta_lag_comp(am5);cr_lag5$lag="lag5"
  cr_lag6<-beta_lag_comp(am6);cr_lag6$lag="lag6";cr_lag7<-beta_lag_comp(am7);cr_lag7$lag="lag7"
  
  names(cr_lag0)[1]=exposure[e];names(cr_lag1)[1]=exposure[e]
  names(cr_lag2)[1]=exposure[e];names(cr_lag3)[1]=exposure[e]
  names(cr_lag4)[1]=exposure[e];names(cr_lag5)[1]=exposure[e]
  names(cr_lag6)[1]=exposure[e];names(cr_lag7)[1]=exposure[e]
  
  cr_lag<-as.data.frame(rbind(cr_lag0,cr_lag1,cr_lag2,cr_lag3,cr_lag4,cr_lag5,cr_lag6,cr_lag7))
  
  cr_lag$unit=unit
  
  result[[1]]<-excess_result
  result[[2]]<-cr_lag
  
  result}

#비선형으로 추정한 초과사망자 수와 노출 농도별 베타, 신뢰구간 결과 
#All-cause death & air pollution
result1<-gamm4_nonlinear_excess_mor(index_list[1],1)
result2<-gamm4_nonlinear_excess_mor(index_list[2],1)
result3<-gamm4_nonlinear_excess_mor(index_list[3],1000)
result4<-gamm4_nonlinear_excess_mor(index_list[4],1000)
result5<-gamm4_nonlinear_excess_mor(index_list[5],1000)
result6<-gamm4_nonlinear_excess_mor(index_list[6],1000)

#Non-accidental death & air pollution
result7 <-gamm4_nonlinear_excess_mor(index_list[7] ,1)
result8 <-gamm4_nonlinear_excess_mor(index_list[8] ,1)
result9 <-gamm4_nonlinear_excess_mor(index_list[9] ,1000)
result10<-gamm4_nonlinear_excess_mor(index_list[10],1000)
result11<-gamm4_nonlinear_excess_mor(index_list[11],1000)
result12<-gamm4_nonlinear_excess_mor(index_list[12],1000)

#CVD death & air pollution
result13<-gamm4_nonlinear_excess_mor(index_list[13] ,1)
result14<-gamm4_nonlinear_excess_mor(index_list[14] ,1)
result15<-gamm4_nonlinear_excess_mor(index_list[15] ,1000)
result16<-gamm4_nonlinear_excess_mor(index_list[16],1000)
result17<-gamm4_nonlinear_excess_mor(index_list[17],1000)
result18<-gamm4_nonlinear_excess_mor(index_list[18],1000)

#Respiratory death & air pollution
result19<-gamm4_nonlinear_excess_mor(index_list[19] ,1)
result20<-gamm4_nonlinear_excess_mor(index_list[20] ,1)
result21<-gamm4_nonlinear_excess_mor(index_list[21] ,1000)
result22<-gamm4_nonlinear_excess_mor(index_list[22],1000)
result23<-gamm4_nonlinear_excess_mor(index_list[23],1000)
result24<-gamm4_nonlinear_excess_mor(index_list[24],1000)

setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result\\비선형결과\\excess_mor")
write.csv(result1[1][[1]] ,file="excess_tot_pm25.csv"        ,row.names=F,na="")
write.csv(result2[1][[1]] ,file="excess_tot_pm10.csv"        ,row.names=F,na="")
write.csv(result3[1][[1]] ,file="excess_tot_so2.csv"         ,row.names=F,na="")
write.csv(result4[1][[1]] ,file="excess_tot_no2.csv"         ,row.names=F,na="")
write.csv(result5[1][[1]] ,file="excess_tot_co.csv"          ,row.names=F,na="")
write.csv(result6[1][[1]] ,file="excess_tot_o3.csv"          ,row.names=F,na="")
write.csv(result7[1][[1]] ,file="excess_nonacc_pm25.csv"     ,row.names=F,na="")
write.csv(result8[1][[1]] ,file="excess_nonacc_pm10.csv"     ,row.names=F,na="")
write.csv(result9[1][[1]] ,file="excess_nonacc_so2.csv"      ,row.names=F,na="")
write.csv(result10[1][[1]],file="excess_nonacc_no2.csv"      ,row.names=F,na="")
write.csv(result11[1][[1]],file="excess_nonacc_co.csv"       ,row.names=F,na="")
write.csv(result12[1][[1]],file="excess_nonacc_o3.csv"       ,row.names=F,na="")
write.csv(result13[1][[1]],file="excess_cvd_pm25.csv"        ,row.names=F,na="")
write.csv(result14[1][[1]],file="excess_cvd_pm10.csv"        ,row.names=F,na="")
write.csv(result15[1][[1]],file="excess_cvd_so2.csv"         ,row.names=F,na="")
write.csv(result16[1][[1]],file="excess_cvd_no2.csv"         ,row.names=F,na="")
write.csv(result17[1][[1]],file="excess_cvd_co.csv"          ,row.names=F,na="")
write.csv(result18[1][[1]],file="excess_cvd_o3.csv"          ,row.names=F,na="")
write.csv(result19[1][[1]],file="excess_respiratory_pm25.csv",row.names=F,na="")
write.csv(result20[1][[1]],file="excess_respiratory_pm10.csv",row.names=F,na="")
write.csv(result21[1][[1]],file="excess_respiratory_so2.csv" ,row.names=F,na="")
write.csv(result22[1][[1]],file="excess_respiratory_no2.csv" ,row.names=F,na="")
write.csv(result23[1][[1]],file="excess_respiratory_co.csv"  ,row.names=F,na="")
write.csv(result24[1][[1]],file="excess_respiratory_o3.csv"  ,row.names=F,na="")


setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result\\비선형결과\\cr")
write.csv(result1[2][[1]] ,file="cr_tot_pm25.csv"        ,row.names=F,na="")
write.csv(result2[2][[1]] ,file="cr_tot_pm10.csv"        ,row.names=F,na="")
write.csv(result3[2][[1]] ,file="cr_tot_so2.csv"         ,row.names=F,na="")
write.csv(result4[2][[1]] ,file="cr_tot_no2.csv"         ,row.names=F,na="")
write.csv(result5[2][[1]] ,file="cr_tot_co.csv"          ,row.names=F,na="")
write.csv(result6[2][[1]] ,file="cr_tot_o3.csv"          ,row.names=F,na="")
write.csv(result7[2][[1]] ,file="cr_nonacc_pm25.csv"     ,row.names=F,na="")
write.csv(result8[2][[1]] ,file="cr_nonacc_pm10.csv"     ,row.names=F,na="")
write.csv(result9[2][[1]] ,file="cr_nonacc_so2.csv"      ,row.names=F,na="")
write.csv(result10[2][[1]],file="cr_nonacc_no2.csv"      ,row.names=F,na="")
write.csv(result11[2][[1]],file="cr_nonacc_co.csv"       ,row.names=F,na="")
write.csv(result12[2][[1]],file="cr_nonacc_o3.csv"       ,row.names=F,na="")
write.csv(result13[2][[1]],file="cr_cvd_pm25.csv"        ,row.names=F,na="")
write.csv(result14[2][[1]],file="cr_cvd_pm10.csv"        ,row.names=F,na="")
write.csv(result15[2][[1]],file="cr_cvd_so2.csv"         ,row.names=F,na="")
write.csv(result16[2][[1]],file="cr_cvd_no2.csv"         ,row.names=F,na="")
write.csv(result17[2][[1]],file="cr_cvd_co.csv"          ,row.names=F,na="")
write.csv(result18[2][[1]],file="cr_cvd_o3.csv"          ,row.names=F,na="")
write.csv(result19[2][[1]],file="cr_respiratory_pm25.csv",row.names=F,na="")
write.csv(result20[2][[1]],file="cr_respiratory_pm10.csv",row.names=F,na="")
write.csv(result21[2][[1]],file="cr_respiratory_so2.csv" ,row.names=F,na="")
write.csv(result22[2][[1]],file="cr_respiratory_no2.csv" ,row.names=F,na="")
write.csv(result23[2][[1]],file="cr_respiratory_co.csv"  ,row.names=F,na="")
write.csv(result24[2][[1]],file="cr_respiratory_o3.csv"  ,row.names=F,na="")