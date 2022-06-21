#-------------------------------------------------------------#
##################KEI-환경보건감시체계과제#####################
#-------------------------------------------------------------#
pacman::p_load(gamm4,mgcv,lme4,splines,ggplot2,gridExtra,dplyr,lubridate,scales,RcolorBrewer)

#-------------------------------------------------------------#
setwd("D:\\JM\\KETI_GBD")

daily_death_final<-read.csv("daily_death_final.csv")

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
dat.r<-dat %>% dplyr:: select(ddate,year,TOT,NON_ACC,CVD,RESP,sn,sidow,dow,sido_KN,
                              pm25_new_m1:pm25_new_m7,
                              pm10_m1:pm10_m7,simpat_m1:simpat_m7)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#GAMM4 모형 돌린 결과 저장하기
#비선형 관련성으로 먼저 살펴보기-> 관련성 보고 선형성 적합 비교
#model1: 체감기온, 타임 보정, 요일(day of week)/ random effect: 시도
#time-plot 그려서 요일 살펴보면, 입원이랑 달리 트렌드가 뚜렷하지 않음
#이동평균으로만 진행, 당일부터 7일 이전평균 노출 자료 고려
#질환 대분류만 고려 

setwd("D:\\JM\\KETI_GBD\\gamm4") 
#비사고 사망(NOn-accidential death)
g1<-gamm4(NON_ACC~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g1,file="gamm4_nonacc_pm25_m1.rds");rm(g1)
g2<-gamm4(NON_ACC~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g2,file="gamm4_nonacc_pm25_m2.rds");rm(g2)
g3<-gamm4(NON_ACC~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g3,file="gamm4_nonacc_pm25_m3.rds");rm(g3)
g4<-gamm4(NON_ACC~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g4,file="gamm4_nonacc_pm25_m4.rds");rm(g4)
g5<-gamm4(NON_ACC~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g5,file="gamm4_nonacc_pm25_m5.rds");rm(g5)
g6<-gamm4(NON_ACC~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g6,file="gamm4_nonacc_pm25_m6.rds");rm(g6)
g7<-gamm4(NON_ACC~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g7,file="gamm4_nonacc_pm25_m7.rds");rm(g7)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#전체 심혈관 (CVD)
g1<-gamm4(CVD~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g1,file="gamm4_cvd_pm25_m1.rds");rm(g1)
g2<-gamm4(CVD~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g2,file="gamm4_cvd_pm25_m2.rds");rm(g2)
g3<-gamm4(CVD~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g3,file="gamm4_cvd_pm25_m3.rds");rm(g3)
g4<-gamm4(CVD~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g4,file="gamm4_cvd_pm25_m4.rds");rm(g4)
g5<-gamm4(CVD~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g5,file="gamm4_cvd_pm25_m5.rds");rm(g5)
g6<-gamm4(CVD~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g6,file="gamm4_cvd_pm25_m6.rds");rm(g6)
g7<-gamm4(CVD~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g7,file="gamm4_cvd_pm25_m7.rds");rm(g7)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#전체 호흡기(Respiratory)
g1<-gamm4(RESP~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g1,file="gamm4_resp_pm25_m1.rds");rm(g1)
g2<-gamm4(RESP~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g2,file="gamm4_resp_pm25_m2.rds");rm(g2)
g3<-gamm4(RESP~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g3,file="gamm4_resp_pm25_m3.rds");rm(g3)
g4<-gamm4(RESP~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g4,file="gamm4_resp_pm25_m4.rds");rm(g4)
g5<-gamm4(RESP~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g5,file="gamm4_resp_pm25_m5.rds");rm(g5)
g6<-gamm4(RESP~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g6,file="gamm4_resp_pm25_m6.rds");rm(g6)
g7<-gamm4(RESP~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g7,file="gamm4_resp_pm25_m7.rds");rm(g7)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#비사고 사망(NOn-accidential death)

g1<-gamm4(NON_ACC~s(pm10_m1)+s(simpat_m1)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g1,file="gamm4_nonacc_pm10_m1.rds");rm(g1)
g2<-gamm4(NON_ACC~s(pm10_m2)+s(simpat_m2)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g2,file="gamm4_nonacc_pm10_m2.rds");rm(g2)
g3<-gamm4(NON_ACC~s(pm10_m3)+s(simpat_m3)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g3,file="gamm4_nonacc_pm10_m3.rds");rm(g3)
g4<-gamm4(NON_ACC~s(pm10_m4)+s(simpat_m4)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g4,file="gamm4_nonacc_pm10_m4.rds");rm(g4)
g5<-gamm4(NON_ACC~s(pm10_m5)+s(simpat_m5)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g5,file="gamm4_nonacc_pm10_m5.rds");rm(g5)
g6<-gamm4(NON_ACC~s(pm10_m6)+s(simpat_m6)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g6,file="gamm4_nonacc_pm10_m6.rds");rm(g6)
g7<-gamm4(NON_ACC~s(pm10_m7)+s(simpat_m7)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g7,file="gamm4_nonacc_pm10_m7.rds");rm(g7)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#전체 심혈관 (CVD)
g1<-gamm4(CVD~s(pm10_m1)+s(simpat_m1)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g1,file="gamm4_cvd_pm10_m1.rds");rm(g1)
g2<-gamm4(CVD~s(pm10_m2)+s(simpat_m2)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g2,file="gamm4_cvd_pm10_m2.rds");rm(g2)
g3<-gamm4(CVD~s(pm10_m3)+s(simpat_m3)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g3,file="gamm4_cvd_pm10_m3.rds");rm(g3)
g4<-gamm4(CVD~s(pm10_m4)+s(simpat_m4)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g4,file="gamm4_cvd_pm10_m4.rds");rm(g4)
g5<-gamm4(CVD~s(pm10_m5)+s(simpat_m5)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g5,file="gamm4_cvd_pm10_m5.rds");rm(g5)
g6<-gamm4(CVD~s(pm10_m6)+s(simpat_m6)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g6,file="gamm4_cvd_pm10_m6.rds");rm(g6)
g7<-gamm4(CVD~s(pm10_m7)+s(simpat_m7)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g7,file="gamm4_cvd_pm10_m7.rds");rm(g7)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#전체 호흡기(Respiratory)
g1<-gamm4(RESP~s(pm10_m1)+s(simpat_m1)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g1,file="gamm4_resp_pm10_m1.rds");rm(g1)
g2<-gamm4(RESP~s(pm10_m2)+s(simpat_m2)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g2,file="gamm4_resp_pm10_m2.rds");rm(g2)
g3<-gamm4(RESP~s(pm10_m3)+s(simpat_m3)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g3,file="gamm4_resp_pm10_m3.rds");rm(g3)
g4<-gamm4(RESP~s(pm10_m4)+s(simpat_m4)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g4,file="gamm4_resp_pm10_m4.rds");rm(g4)
g5<-gamm4(RESP~s(pm10_m5)+s(simpat_m5)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g5,file="gamm4_resp_pm10_m5.rds");rm(g5)
g6<-gamm4(RESP~s(pm10_m6)+s(simpat_m6)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g6,file="gamm4_resp_pm10_m6.rds");rm(g6)
g7<-gamm4(RESP~s(pm10_m7)+s(simpat_m7)+s(sn,k=6*10)+factor(dow),random=~(1|sido_KN),family="poisson",data=dat.r)
saveRDS(g7,file="gamm4_resp_pm10_m7.rds");rm(g7)
