#--------------------------------------------------------------------------------------------#
#library
pacman::p_load(gamm4,mgcv,lme4,splines,ggplot2,gridExtra,dplyr,lubridate,scales,RColorBrewer)
#공단 단기 질병부담 자료 
daily_death_final<-read.csv("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\daily_death_final.csv",
                            fileEncoding = "euc-kr")

daily_death_final$ddate=ymd(daily_death_final$ddate)
daily_death_final$sido_KN =with(daily_death_final,factor(sido_KN,levels=unique(sido_KN)))
daily_death_final$sidoname=with(daily_death_final,factor(sidoname,levels=unique(sidoname)))

#여기 이하부터는 모델링 
daily_death_final$sidow=with(daily_death_final,as.factor(sido*10+as.numeric(as.character(dow))))
table(daily_death_final$sidow)

#단일지연 자료를 포함하면 결측이 좀 있으니, 이동평균 노출만 우선고려
#세종지역 제외 

dat<-daily_death_final %>% dplyr:: select(key:sidoname,TOT,NON_ACC,CVD,RESP,meantemp,meanhumi,dewtemp,simpat,
                                          pm25_new,pm10:o3,
                                          sn:dowfirstday,sidow,
                                          meantemp_m1:meantemp_m21,
                                          meanhumi_m1:meanhumi_m21,
                                          dewtemp_m1 :dewtemp_m21,
                                          simpat_m1  :simpat_m21,
                                          pm25_new_m1:pm25_new_m21,
                                          pm10_m1    :pm10_m21,
                                          so2_m1     :so2_m21,
                                          no2_m1     :no2_m21,
                                          co_m1      :co_m21,
                                          o3_m1      :o3_m21) %>% filter(sido_KN!="세종")

dat<-dat[complete.cases(dat),];#약3,500건 빠짐 

#전체 사망, 당일~하루전 이동평균 PM2.5, 당일~하루전 이동평균 체감기온, time trend 변수들 
dat.r<-dat %>% dplyr:: select(ddate,year,month,TOT,NON_ACC,CVD,RESP,sn,sidow,dow,sido_KN,
                              pm25_new,pm25_new_m1:pm25_new_m7,
                              pm10,pm10_m1    :pm10_m7,
                              so2,so2_m1     :so2_m7,
                              no2,no2_m1     :no2_m7,
                              co,co_m1      :co_m7,
                              o3,o3_m1      :o3_m7,simpat,simpat_m1:simpat_m7)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#GAMM4 모형 돌린 결과 저장하기
#선형, 비선형 둘다 추정하기
#오존 warm season (5~9)!! / 4~9 , 6~8
#우리나라 오존 5월 부터 증가시기 

dat.w<-subset(dat.r,month %in% c(5:9))

#-------------------------------------------------------------#
#-------------------------------------------------------------#
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\gamm4_linear")
#전체 원인 사망 (All-cause)
g0<-gamm4(TOT~o3   +s(simpat)   +s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g0,file="gamm4_tot_o3_linear_m0.rds");rm(g0)
g1<-gamm4(TOT~o3_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g1,file="gamm4_tot_o3_linear_m1.rds");rm(g1)
g2<-gamm4(TOT~o3_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g2,file="gamm4_tot_o3_linear_m2.rds");rm(g2)
g3<-gamm4(TOT~o3_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g3,file="gamm4_tot_o3_linear_m3.rds");rm(g3)
g4<-gamm4(TOT~o3_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g4,file="gamm4_tot_o3_linear_m4.rds");rm(g4)
g5<-gamm4(TOT~o3_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g5,file="gamm4_tot_o3_linear_m5.rds");rm(g5)
g6<-gamm4(TOT~o3_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g6,file="gamm4_tot_o3_linear_m6.rds");rm(g6)
g7<-gamm4(TOT~o3_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g7,file="gamm4_tot_o3_linear_m7.rds");rm(g7)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#비사고 사망(NOn-accidential death)
g0<-gamm4(NON_ACC~o3   +s(simpat)   +s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g0,file="gamm4_nonacc_o3_linear_m0.rds");rm(g0)
g1<-gamm4(NON_ACC~o3_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g1,file="gamm4_nonacc_o3_linear_m1.rds");rm(g1)
g2<-gamm4(NON_ACC~o3_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g2,file="gamm4_nonacc_o3_linear_m2.rds");rm(g2)
g3<-gamm4(NON_ACC~o3_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g3,file="gamm4_nonacc_o3_linear_m3.rds");rm(g3)
g4<-gamm4(NON_ACC~o3_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g4,file="gamm4_nonacc_o3_linear_m4.rds");rm(g4)
g5<-gamm4(NON_ACC~o3_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g5,file="gamm4_nonacc_o3_linear_m5.rds");rm(g5)
g6<-gamm4(NON_ACC~o3_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g6,file="gamm4_nonacc_o3_linear_m6.rds");rm(g6)
g7<-gamm4(NON_ACC~o3_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g7,file="gamm4_nonacc_o3_linear_m7.rds");rm(g7)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#전체 심혈관 (CVD)
g0<-gamm4(CVD~o3   +s(simpat)   +s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g0,file="gamm4_cvd_o3_linear_m0.rds");rm(g0)
g1<-gamm4(CVD~o3_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g1,file="gamm4_cvd_o3_linear_m1.rds");rm(g1)
g2<-gamm4(CVD~o3_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g2,file="gamm4_cvd_o3_linear_m2.rds");rm(g2)
g3<-gamm4(CVD~o3_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g3,file="gamm4_cvd_o3_linear_m3.rds");rm(g3)
g4<-gamm4(CVD~o3_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g4,file="gamm4_cvd_o3_linear_m4.rds");rm(g4)
g5<-gamm4(CVD~o3_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g5,file="gamm4_cvd_o3_linear_m5.rds");rm(g5)
g6<-gamm4(CVD~o3_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g6,file="gamm4_cvd_o3_linear_m6.rds");rm(g6)
g7<-gamm4(CVD~o3_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g7,file="gamm4_cvd_o3_linear_m7.rds");rm(g7)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#전체 호흡기(Respiratory)
g0<-gamm4(RESP~o3   +s(simpat)   +s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g0,file="gamm4_resp_o3_linear_m0.rds");rm(g0)
g1<-gamm4(RESP~o3_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g1,file="gamm4_resp_o3_linear_m1.rds");rm(g1)
g2<-gamm4(RESP~o3_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g2,file="gamm4_resp_o3_linear_m2.rds");rm(g2)
g3<-gamm4(RESP~o3_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g3,file="gamm4_resp_o3_linear_m3.rds");rm(g3)
g4<-gamm4(RESP~o3_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g4,file="gamm4_resp_o3_linear_m4.rds");rm(g4)
g5<-gamm4(RESP~o3_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g5,file="gamm4_resp_o3_linear_m5.rds");rm(g5)
g6<-gamm4(RESP~o3_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g6,file="gamm4_resp_o3_linear_m6.rds");rm(g6)
g7<-gamm4(RESP~o3_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g7,file="gamm4_resp_o3_linear_m7.rds");rm(g7)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#비선형 가정 
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\gamm4\\warm_season_ozone") 

#전체 원인 사망 (All-cause)
g0<-gamm4(TOT~s(o3)   +s(simpat)   +s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g0,file="gamm4_tot_o3_nonlinear_m0.rds");rm(g0)
g1<-gamm4(TOT~s(o3_m1)+s(simpat_m1)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g1,file="gamm4_tot_o3_nonlinear_m1.rds");rm(g1)
g2<-gamm4(TOT~s(o3_m2)+s(simpat_m2)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g2,file="gamm4_tot_o3_nonlinear_m2.rds");rm(g2)
g3<-gamm4(TOT~s(o3_m3)+s(simpat_m3)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g3,file="gamm4_tot_o3_nonlinear_m3.rds");rm(g3)
g4<-gamm4(TOT~s(o3_m4)+s(simpat_m4)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g4,file="gamm4_tot_o3_nonlinear_m4.rds");rm(g4)
g5<-gamm4(TOT~s(o3_m5)+s(simpat_m5)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g5,file="gamm4_tot_o3_nonlinear_m5.rds");rm(g5)
g6<-gamm4(TOT~s(o3_m6)+s(simpat_m6)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g6,file="gamm4_tot_o3_nonlinear_m6.rds");rm(g6)
g7<-gamm4(TOT~s(o3_m7)+s(simpat_m7)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g7,file="gamm4_tot_o3_nonlinear_m7.rds");rm(g7)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#비사고 사망(NOn-accidential death)
g0<-gamm4(NON_ACC~s(o3)   +s(simpat)   +s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g0,file="gamm4_nonacc_o3_nonlinear_m0.rds");rm(g0)
g1<-gamm4(NON_ACC~s(o3_m1)+s(simpat_m1)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g1,file="gamm4_nonacc_o3_nonlinear_m1.rds");rm(g1)
g2<-gamm4(NON_ACC~s(o3_m2)+s(simpat_m2)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g2,file="gamm4_nonacc_o3_nonlinear_m2.rds");rm(g2)
g3<-gamm4(NON_ACC~s(o3_m3)+s(simpat_m3)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g3,file="gamm4_nonacc_o3_nonlinear_m3.rds");rm(g3)
g4<-gamm4(NON_ACC~s(o3_m4)+s(simpat_m4)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g4,file="gamm4_nonacc_o3_nonlinear_m4.rds");rm(g4)
g5<-gamm4(NON_ACC~s(o3_m5)+s(simpat_m5)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g5,file="gamm4_nonacc_o3_nonlinear_m5.rds");rm(g5)
g6<-gamm4(NON_ACC~s(o3_m6)+s(simpat_m6)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g6,file="gamm4_nonacc_o3_nonlinear_m6.rds");rm(g6)
g7<-gamm4(NON_ACC~s(o3_m7)+s(simpat_m7)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g7,file="gamm4_nonacc_o3_nonlinear_m7.rds");rm(g7)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#전체 심혈관 (CVD)
g0<-gamm4(CVD~s(o3)   +s(simpat)   +s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g0,file="gamm4_cvd_o3_nonlinear_m0.rds");rm(g0)
g1<-gamm4(CVD~s(o3_m1)+s(simpat_m1)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g1,file="gamm4_cvd_o3_nonlinear_m1.rds");rm(g1)
g2<-gamm4(CVD~s(o3_m2)+s(simpat_m2)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g2,file="gamm4_cvd_o3_nonlinear_m2.rds");rm(g2)
g3<-gamm4(CVD~s(o3_m3)+s(simpat_m3)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g3,file="gamm4_cvd_o3_nonlinear_m3.rds");rm(g3)
g4<-gamm4(CVD~s(o3_m4)+s(simpat_m4)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g4,file="gamm4_cvd_o3_nonlinear_m4.rds");rm(g4)
g5<-gamm4(CVD~s(o3_m5)+s(simpat_m5)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g5,file="gamm4_cvd_o3_nonlinear_m5.rds");rm(g5)
g6<-gamm4(CVD~s(o3_m6)+s(simpat_m6)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g6,file="gamm4_cvd_o3_nonlinear_m6.rds");rm(g6)
g7<-gamm4(CVD~s(o3_m7)+s(simpat_m7)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g7,file="gamm4_cvd_o3_nonlinear_m7.rds");rm(g7)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#전체 호흡기(Respiratory)
g0<-gamm4(RESP~s(o3)   +s(simpat)   +s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g0,file="gamm4_resp_o3_nonlinear_m0.rds");rm(g0)
g1<-gamm4(RESP~s(o3_m1)+s(simpat_m1)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g1,file="gamm4_resp_o3_nonlinear_m1.rds");rm(g1)
g2<-gamm4(RESP~s(o3_m2)+s(simpat_m2)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g2,file="gamm4_resp_o3_nonlinear_m2.rds");rm(g2)
g3<-gamm4(RESP~s(o3_m3)+s(simpat_m3)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g3,file="gamm4_resp_o3_nonlinear_m3.rds");rm(g3)
g4<-gamm4(RESP~s(o3_m4)+s(simpat_m4)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g4,file="gamm4_resp_o3_nonlinear_m4.rds");rm(g4)
g5<-gamm4(RESP~s(o3_m5)+s(simpat_m5)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g5,file="gamm4_resp_o3_nonlinear_m5.rds");rm(g5)
g6<-gamm4(RESP~s(o3_m6)+s(simpat_m6)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g6,file="gamm4_resp_o3_nonlinear_m6.rds");rm(g6)
g7<-gamm4(RESP~s(o3_m7)+s(simpat_m7)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.w)
saveRDS(g7,file="gamm4_resp_o3_nonlinear_m7.rds");rm(g7)

#--------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------#
#library
pacman::p_load(gamm4,mgcv,lme4,splines,ggplot2,gridExtra,dplyr,lubridate,scales,RColorBrewer,
               stringr)

#선형 추정한 자료 
dir1<-"D:/SNU/연구/KEI_환경보건감시체계/분석/단기질병부담/gamm4_linear/warm_season_ozone"
setwd(dir1)
lf.dir<-list.dirs()
wd<-substr(lf.dir[2:length(lf.dir)],2,20)

#오존 warm season period (5~9월)만 모델링한 결과 베타 반환 
gamm4_beta_func<-function(result,disease,lag){
  z<-result
  
  df <-as.data.frame(t(c(summary(z)$p.table[2,],summary(z)$r.sq)))
  df2<-data.frame(outcome=disease,exposure="o3",lag,df)
  
  #relabel
  names(df2)=c("outcome","exposure","lag","Estimate","SE","zval","Pval","R2")
  
  #unit change, PM이면 ug/m3, 그 외 대기오염 ppm
  df2$단위="ppm";df2$unit=1000
  
  #unit change 대기오염 물질 같은 경우 검토
  uc<-1/df2$unit
  
  df2$RR    =exp(uc*df2$Estimate)
  df2$RR_lci=exp(uc*(df2$Estimate-1.96*df2$SE))
  df2$RR_uci=exp(uc*(df2$Estimate+1.96*df2$SE))
  df2
}

warm_o3_beta_list=NULL
for(i in 1:4){
  setwd(paste0(dir1,wd[i]))
  g0<-readRDS(list.files()[1]);r1<-gamm4_beta_func(g0$gam,gsub("/","",wd[i]),0);rm(g0)
  g1<-readRDS(list.files()[2]);r2<-gamm4_beta_func(g1$gam,gsub("/","",wd[i]),1);rm(g1)
  g2<-readRDS(list.files()[3]);r3<-gamm4_beta_func(g2$gam,gsub("/","",wd[i]),2);rm(g2)
  g3<-readRDS(list.files()[4]);r4<-gamm4_beta_func(g3$gam,gsub("/","",wd[i]),3);rm(g3)
  g4<-readRDS(list.files()[5]);r5<-gamm4_beta_func(g4$gam,gsub("/","",wd[i]),4);rm(g4)
  g5<-readRDS(list.files()[6]);r6<-gamm4_beta_func(g5$gam,gsub("/","",wd[i]),5);rm(g5)
  g6<-readRDS(list.files()[7]);r7<-gamm4_beta_func(g6$gam,gsub("/","",wd[i]),6);rm(g6)
  g7<-readRDS(list.files()[8]);r8<-gamm4_beta_func(g7$gam,gsub("/","",wd[i]),7);rm(g7)
  
  warm_o3_beta_list[[i]]<-rbind(r1,r2,r3,r4,r5,r6,r7,r8)
  print(i)}

o3_results<-do.call(rbind,warm_o3_beta_list)

write.csv(o3_results,
          file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result\\warm_o3_beta.csv",
          row.names=F,na="",fileEncoding = "euc-kr")

#--------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------#
#비선형 추정한 자료 
dir2<-"D:/SNU/연구/KEI_환경보건감시체계/분석/단기질병부담/gamm4/warm_season_ozone"
setwd(dir2)

paste0(dir2,wd[1])
paste0(dir2,wd[2])
paste0(dir2,wd[3])
paste0(dir2,wd[4])

setwd(paste0(dir2,wd[1]))

g0<-readRDS(list.files()[1])
g1<-readRDS(list.files()[2])
g2<-readRDS(list.files()[3])
g3<-readRDS(list.files()[4])
g4<-readRDS(list.files()[5])
g5<-readRDS(list.files()[6])
g6<-readRDS(list.files()[7])
g7<-readRDS(list.files()[8])

x11();par(mfrow=c(2,4))
plot(g0$gam,select=1,cex.lab=1.45,cex.lab=1.45,cex.main=2,main="Lag00",xlab="O3");
plot(g1$gam,select=1,cex.lab=1.45,cex.lab=1.45,cex.main=2,main="Lag01",xlab="O3");
plot(g2$gam,select=1,cex.lab=1.45,cex.lab=1.45,cex.main=2,main="Lag02",xlab="O3");
plot(g3$gam,select=1,cex.lab=1.45,cex.lab=1.45,cex.main=2,main="Lag03",xlab="O3");
plot(g4$gam,select=1,cex.lab=1.45,cex.lab=1.45,cex.main=2,main="Lag04",xlab="O3");
plot(g5$gam,select=1,cex.lab=1.45,cex.lab=1.45,cex.main=2,main="Lag05",xlab="O3");
plot(g6$gam,select=1,cex.lab=1.45,cex.lab=1.45,cex.main=2,main="Lag06",xlab="O3");
plot(g7$gam,select=1,cex.lab=1.45,cex.lab=1.45,cex.main=2,main="Lag07",xlab="O3");


#-------------------------------------------------------------#
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담")

daily_death_final<-read.csv("daily_death_final.csv",fileEncoding = "euc-kr")
daily_death_final$ddate=ymd(daily_death_final$ddate)
daily_death_final$sido_KN =with(daily_death_final,factor(sido_KN,levels=unique(sido_KN)))
daily_death_final$sidoname=with(daily_death_final,factor(sidoname,levels=unique(sidoname)))

#원시자료 변수 편집, 모델링에 사용한 변수 위주
raw<-daily_death_final %>% dplyr:: select(key:TOT,NON_ACC,CVD,RESP,
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

#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#비선형 가정, 모델링 결과 불러오기 ; by gamm4

#Cr function
#농도별 산출할 자료 
#0도부터 최대 230까지 노출 농도를 가진 예측 자료
pred_data_func<-function(max_exposure){
  mydata<-data.frame(myseq   =seq(max_exposure), 
                     simpat     =mean(raw$simpat,na.rm=T),
                     simpat_m1  =mean(raw$simpat_m1,na.rm=T),
                     simpat_m2  =mean(raw$simpat_m2,na.rm=T),
                     simpat_m3  =mean(raw$simpat_m3,na.rm=T),
                     simpat_m4  =mean(raw$simpat_m4,na.rm=T),
                     simpat_m5  =mean(raw$simpat_m5,na.rm=T),
                     simpat_m6  =mean(raw$simpat_m6,na.rm=T),
                     simpat_m7  =mean(raw$simpat_m7,na.rm=T),
                     sn         =median(raw$sn),
                     dow        ="1")
  mydata
}

pred_data_o3  <-pred_data_func(max(raw$o3,na.rm=T)*1000)

#O3 노출 최대값까지 반영
pred_data_o3$o3   =seq(max(raw$o3,na.rm=T)*1000)/1000
pred_data_o3$o3_m1=seq(max(raw$o3,na.rm=T)*1000)/1000
pred_data_o3$o3_m2=seq(max(raw$o3,na.rm=T)*1000)/1000
pred_data_o3$o3_m3=seq(max(raw$o3,na.rm=T)*1000)/1000
pred_data_o3$o3_m4=seq(max(raw$o3,na.rm=T)*1000)/1000
pred_data_o3$o3_m5=seq(max(raw$o3,na.rm=T)*1000)/1000
pred_data_o3$o3_m6=seq(max(raw$o3,na.rm=T)*1000)/1000
pred_data_o3$o3_m7=seq(max(raw$o3,na.rm=T)*1000)/1000

#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#

pred0 <- predict(g0$gam,newdata=pred_data_o3,type="terms",se.fit=TRUE)
pred1 <- predict(g1$gam,newdata=pred_data_o3,type="terms",se.fit=TRUE)
pred2 <- predict(g2$gam,newdata=pred_data_o3,type="terms",se.fit=TRUE)
pred3 <- predict(g3$gam,newdata=pred_data_o3,type="terms",se.fit=TRUE)
pred4 <- predict(g4$gam,newdata=pred_data_o3,type="terms",se.fit=TRUE)
pred5 <- predict(g5$gam,newdata=pred_data_o3,type="terms",se.fit=TRUE)
pred6 <- predict(g6$gam,newdata=pred_data_o3,type="terms",se.fit=TRUE)
pred7 <- predict(g7$gam,newdata=pred_data_o3,type="terms",se.fit=TRUE)

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
a0<-raw;a0$exp=round(raw$o3   *1000)
a1<-raw;a1$exp=round(raw$o3_m1*1000)
a2<-raw;a2$exp=round(raw$o3_m2*1000)
a3<-raw;a3$exp=round(raw$o3_m3*1000)
a4<-raw;a4$exp=round(raw$o3_m4*1000)
a5<-raw;a5$exp=round(raw$o3_m5*1000)
a6<-raw;a6$exp=round(raw$o3_m6*1000)
a7<-raw;a7$exp=round(raw$o3_m7*1000)

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
o3_ref  =60 *24.5/48    #30.625 ppb 

#시나리오 1의 대기오염 기준 농도 가스상 물질은 ppb 단위
exp_ref=30

#시나리오 1, 대기오염 기준농도에 해당하는 lag별 beta계수
beta0_ref=subset(exp_beta0,exp==30)$beta;beta1_ref=subset(exp_beta1,exp==30)$beta
beta2_ref=subset(exp_beta2,exp==30)$beta;beta3_ref=subset(exp_beta3,exp==30)$beta
beta4_ref=subset(exp_beta4,exp==30)$beta;beta5_ref=subset(exp_beta5,exp==30)$beta
beta6_ref=subset(exp_beta6,exp==30)$beta;beta7_ref=subset(exp_beta7,exp==30)$beta

#시나리오1에서는 기준 농도 이하는 건강영향 없다고 가정
#노출 농도가 기준농도보다 작은 경우 beta를 0으로 만들어주기 
am0$delta_beta1=with(am0,ifelse(exp<=30,0,beta-beta0_ref))
am1$delta_beta1=with(am1,ifelse(exp<=30,0,beta-beta1_ref))
am2$delta_beta1=with(am2,ifelse(exp<=30,0,beta-beta2_ref))
am3$delta_beta1=with(am3,ifelse(exp<=30,0,beta-beta3_ref))
am4$delta_beta1=with(am4,ifelse(exp<=30,0,beta-beta4_ref))
am5$delta_beta1=with(am5,ifelse(exp<=30,0,beta-beta5_ref))
am6$delta_beta1=with(am6,ifelse(exp<=30,0,beta-beta6_ref))
am7$delta_beta1=with(am7,ifelse(exp<=30,0,beta-beta7_ref))

#시나리오 1 기준농도 에 따른 신뢰하한
am0$delta_beta1_lci=with(am0,ifelse(exp<=30,0,(beta-beta0_ref)-1.96*beta_se))
am1$delta_beta1_lci=with(am1,ifelse(exp<=30,0,(beta-beta1_ref)-1.96*beta_se))
am2$delta_beta1_lci=with(am2,ifelse(exp<=30,0,(beta-beta2_ref)-1.96*beta_se))
am3$delta_beta1_lci=with(am3,ifelse(exp<=30,0,(beta-beta3_ref)-1.96*beta_se))
am4$delta_beta1_lci=with(am4,ifelse(exp<=30,0,(beta-beta4_ref)-1.96*beta_se))
am5$delta_beta1_lci=with(am5,ifelse(exp<=30,0,(beta-beta5_ref)-1.96*beta_se))
am6$delta_beta1_lci=with(am6,ifelse(exp<=30,0,(beta-beta6_ref)-1.96*beta_se))
am7$delta_beta1_lci=with(am7,ifelse(exp<=30,0,(beta-beta7_ref)-1.96*beta_se))

#시나리오 1 기준농도 에 따른 신뢰상한 
am0$delta_beta1_uci=with(am0,ifelse(exp<=30,0,(beta-beta0_ref)+1.96*beta_se))
am1$delta_beta1_uci=with(am1,ifelse(exp<=30,0,(beta-beta1_ref)+1.96*beta_se))
am2$delta_beta1_uci=with(am2,ifelse(exp<=30,0,(beta-beta2_ref)+1.96*beta_se))
am3$delta_beta1_uci=with(am3,ifelse(exp<=30,0,(beta-beta3_ref)+1.96*beta_se))
am4$delta_beta1_uci=with(am4,ifelse(exp<=30,0,(beta-beta4_ref)+1.96*beta_se))
am5$delta_beta1_uci=with(am5,ifelse(exp<=30,0,(beta-beta5_ref)+1.96*beta_se))
am6$delta_beta1_uci=with(am6,ifelse(exp<=30,0,(beta-beta6_ref)+1.96*beta_se))
am7$delta_beta1_uci=with(am7,ifelse(exp<=30,0,(beta-beta7_ref)+1.96*beta_se))

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
#고민: warm season(5~9월)만 고려한 베타값을 가지고, 
# 5~9월 기간만 초과사망/질환을 내야하는지?
# 전체 기간을 내야하는지?
excess_func<-function(data,m){
  
  dd<-data;outcome=m
  dd$outcome=m
  
  dd$death1    =with(dd,(1-1/exp(delta_beta1))    *unlist(outcome))
  dd$death1_lci=with(dd,(1-1/exp(delta_beta1_lci))*unlist(outcome))
  dd$death1_uci=with(dd,(1-1/exp(delta_beta1_uci))*unlist(outcome))
  
  dd$death2    =with(dd,(1-1/exp(delta_beta2))    *unlist(outcome))
  dd$death2_lci=with(dd,(1-1/exp(delta_beta2_lci))*unlist(outcome))
  dd$death2_uci=with(dd,(1-1/exp(delta_beta2_uci))*unlist(outcome))
  
  df1<-as.data.frame(cbind(aggregate(outcome~year,dd,sum),
                           death1    =aggregate(death1~year,dd,sum)[,2],
                           death1_lci=aggregate(death1_lci~year,dd,sum)[,2],
                           death1_uci=aggregate(death1_uci~year,dd,sum)[,2],
                           death2    =aggregate(death2~year,dd,sum)[,2],
                           death2_lci=aggregate(death2_lci~year,dd,sum)[,2],
                           death2_uci=aggregate(death2_uci~year,dd,sum)[,2]))
  
  dd<-data
  dd$outcome=m
  dd$month=month(dd$ddate)
  dd<- dd %>% subset(month %in% (5:9))
  
  dd$death1    =with(dd,(1-1/exp(delta_beta1))    *unlist(dd$outcome))
  dd$death1_lci=with(dd,(1-1/exp(delta_beta1_lci))*unlist(dd$outcome))
  dd$death1_uci=with(dd,(1-1/exp(delta_beta1_uci))*unlist(dd$outcome))
  
  dd$death2    =with(dd,(1-1/exp(delta_beta2))    *unlist(dd$outcome))
  dd$death2_lci=with(dd,(1-1/exp(delta_beta2_lci))*unlist(dd$outcome))
  dd$death2_uci=with(dd,(1-1/exp(delta_beta2_uci))*unlist(dd$outcome))
  
  df2<-as.data.frame(cbind(aggregate(outcome~year,dd,sum),
                           death1    =aggregate(death1~year,dd,sum)[,2],
                           death1_lci=aggregate(death1_lci~year,dd,sum)[,2],
                           death1_uci=aggregate(death1_uci~year,dd,sum)[,2],
                           death2    =aggregate(death2~year,dd,sum)[,2],
                           death2_lci=aggregate(death2_lci~year,dd,sum)[,2],
                           death2_uci=aggregate(death2_uci~year,dd,sum)[,2]))
  
  df1$period="Total"
  df2$period="warm_season(5~9)"
  rbind(df1,df2)
  
}

excess_am0<-excess_func(am0,am0$TOT);excess_am0$exposure="o3";excess_am0$lag="lag00"
excess_am1<-excess_func(am1,am1$TOT);excess_am1$exposure="o3";excess_am1$lag="lag01"
excess_am2<-excess_func(am2,am2$TOT);excess_am2$exposure="o3";excess_am2$lag="lag02"
excess_am3<-excess_func(am3,am3$TOT);excess_am3$exposure="o3";excess_am3$lag="lag03"
excess_am4<-excess_func(am4,am4$TOT);excess_am4$exposure="o3";excess_am4$lag="lag04"
excess_am5<-excess_func(am5,am5$TOT);excess_am5$exposure="o3";excess_am5$lag="lag05"
excess_am6<-excess_func(am6,am6$TOT);excess_am6$exposure="o3";excess_am6$lag="lag06"
excess_am7<-excess_func(am7,am7$TOT);excess_am7$exposure="o3";excess_am7$lag="lag07"

#초과사망 산출한 결과/ 연도별
excess_am0$disease="TOT";excess_am1$disease="TOT";excess_am2$disease="TOT";excess_am3$disease="TOT"
excess_am4$disease="TOT";excess_am5$disease="TOT";excess_am6$disease="TOT";excess_am7$disease="TOT"

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

names(cr_lag0)[1]="o3";names(cr_lag1)[1]="o3"
names(cr_lag2)[1]="o3";names(cr_lag3)[1]="o3"
names(cr_lag4)[1]="o3";names(cr_lag5)[1]="o3"
names(cr_lag6)[1]="o3";names(cr_lag7)[1]="o3"

cr_lag<-as.data.frame(rbind(cr_lag0,cr_lag1,cr_lag2,cr_lag3,cr_lag4,cr_lag5,cr_lag6,cr_lag7))
cr_lag$unit=0.001

setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result")
write.csv(excess_result,file="warm_season_excess_tot_o3.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(cr_lag       ,file="warm_season_cr_tot_o3.csv"    ,row.names=F,na="",fileEncoding = "euc-kr")

