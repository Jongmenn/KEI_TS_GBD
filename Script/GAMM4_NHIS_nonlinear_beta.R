#-------------------------------------------------------------#
##################KEI-환경보건감시체계과제#####################
#-------------------------------------------------------------#
#대기오염 노출로 인한 건강영향 평가, 질병부담 산정
#자료원: 2021년 공단 반출자료 
#Non-linear association
#--------------------------------------------------------------------------------------------#
#라이브러리 
pacman::p_load(gamm4,mgcv,lme4,splines,ggplot2,gridExtra,dplyr,lubridate,scales,RColorBrewer,readxl)

#공단 단기 질병부담 자료 
daily_nhis_final<-read.csv("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\daily_nhis_final.csv")

daily_nhis_final$ddate   =as.Date(daily_nhis_final$ddate)
daily_nhis_final$sidoname=with(daily_nhis_final,factor(sidoname,levels=unique(sidoname)))
daily_nhis_final$dow     =weekdays(daily_nhis_final$ddate)
daily_nhis_final$sn      =as.numeric(as.factor(daily_nhis_final$ddd))

#세종 제외 16개 시도
dat<-daily_nhis_final %>% select(key:sidoname,
                                 astm02_tot,astm03_tot,astm0203_tot,cvd02_tot,diz02_tot,diz03_tot,diz0203_tot,
                                 hemo02_tot,hemo03_tot,hemo0203_tot,hf02_tot,hf03_tot,
                                 hf0203_tot,htn02_tot,ihd02_tot,isch02_tot,isch03_tot,isch0203_tot,
                                 olfa03_tot,om02_tot,
                                 simpat,simpat_m1:simpat_m7,
                                 meantemp,meantemp_m1:meantemp_m21,
                                 meanhumi,meanhumi_m1:meanhumi_m21,
                                 dewtemp,dewtemp_m1 :dewtemp_m21,
                                 pm25_new,pm25_new_m1:pm25_new_m7,
                                 pm10,pm10_m1:pm10_m7,
                                 
                                 so2,so2_m1:so2_m7,
                                 no2,no2_m1:no2_m7,
                                 co,co_m1:co_m7,
                                 o3,o3_m1:o3_m7) %>% filter(area!="세종")


dat<-dat[complete.cases(dat),];
dat$area=factor(dat$area)

nrow(daily_nhis_final)  #86921
nrow(dat)               #81675

#sido * dow ,
dat$sidow   =with(dat,as.factor(sidolist_nhis*10+as.numeric(as.factor(as.character(dat$dow)))))
dat$sidow   =factor(dat$sidow)
dat$sn      =as.numeric(as.factor(dat$ddd))

#연구기간 * 시도 
study_period=length(seq(as.Date("2006-01-01"),as.Date("2019-12-31"),1))
city_length =unique(daily_nhis_final$area) %>% length

#GAMM4, 비선형선형가정, 질환 자료 
#보정: 체감기온, 시간 추세(Time trend), 랜덤 효과(시도*요일)
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\gamm4_nonlinear")

#Asthma
g0=gamm4(astm02_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_astm02_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(astm02_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_astm02_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(astm02_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_astm02_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(astm02_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_astm02_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(astm02_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_astm02_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(astm02_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_astm02_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(astm02_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_astm02_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(astm02_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_astm02_pm25_nonlinear_m7.rds");rm(g7)

#Asthma
g0=gamm4(astm03_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_astm03_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(astm03_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_astm03_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(astm03_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_astm03_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(astm03_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_astm03_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(astm03_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_astm03_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(astm03_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_astm03_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(astm03_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_astm03_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(astm03_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_astm03_pm25_nonlinear_m7.rds");rm(g7)

#Asthma
g0=gamm4(astm0203_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_astm0203_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(astm0203_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_astm0203_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(astm0203_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_astm0203_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(astm0203_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_astm0203_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(astm0203_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_astm0203_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(astm0203_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_astm0203_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(astm0203_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_astm0203_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(astm0203_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_astm0203_pm25_nonlinear_m7.rds");rm(g7)

#CVD
g0=gamm4(cvd02_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_cvd02_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(cvd02_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_cvd02_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(cvd02_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_cvd02_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(cvd02_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_cvd02_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(cvd02_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_cvd02_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(cvd02_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_cvd02_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(cvd02_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_cvd02_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(cvd02_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_cvd02_pm25_nonlinear_m7.rds");rm(g7)

#IHD
g0=gamm4(ihd02_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_ihd02_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(ihd02_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_ihd02_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(ihd02_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_ihd02_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(ihd02_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_ihd02_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(ihd02_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_ihd02_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(ihd02_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_ihd02_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(ihd02_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_ihd02_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(ihd02_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_ihd02_pm25_nonlinear_m7.rds");rm(g7)

#Heart failure
g0=gamm4(hf02_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_hf02_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(hf02_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_hf02_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(hf02_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_hf02_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(hf02_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_hf02_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(hf02_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_hf02_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(hf02_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_hf02_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(hf02_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_hf02_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(hf02_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_hf02_pm25_nonlinear_m7.rds");rm(g7)

#Heart failure
g0=gamm4(hf03_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_hf03_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(hf03_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_hf03_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(hf03_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_hf03_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(hf03_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_hf03_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(hf03_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_hf03_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(hf03_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_hf03_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(hf03_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_hf03_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(hf03_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_hf03_pm25_nonlinear_m7.rds");rm(g7)

#Heart failure
g0=gamm4(hf0203_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_hf0203_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(hf0203_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_hf0203_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(hf0203_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_hf0203_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(hf0203_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_hf0203_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(hf0203_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_hf0203_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(hf0203_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_hf0203_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(hf0203_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_hf0203_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(hf0203_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_hf0203_pm25_nonlinear_m7.rds");rm(g7)

#Hemoragic Stroke
g0=gamm4(hemo02_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_hemo02_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(hemo02_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_hemo02_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(hemo02_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_hemo02_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(hemo02_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_hemo02_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(hemo02_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_hemo02_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(hemo02_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_hemo02_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(hemo02_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_hemo02_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(hemo02_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_hemo02_pm25_nonlinear_m7.rds");rm(g7)

#Hemoragic Stroke
g0=gamm4(hemo03_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_hemo03_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(hemo03_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_hemo03_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(hemo03_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_hemo03_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(hemo03_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_hemo03_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(hemo03_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_hemo03_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(hemo03_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_hemo03_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(hemo03_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_hemo03_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(hemo03_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_hemo03_pm25_nonlinear_m7.rds");rm(g7)

#Hemoragic Stroke
g0=gamm4(hemo0203_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_hemo0203_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(hemo0203_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_hemo0203_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(hemo0203_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_hemo0203_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(hemo0203_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_hemo0203_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(hemo0203_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_hemo0203_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(hemo0203_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_hemo0203_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(hemo0203_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_hemo0203_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(hemo0203_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_hemo0203_pm25_nonlinear_m7.rds");rm(g7)

#Ischemic Stroke
g0=gamm4(isch02_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_isch02_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(isch02_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_isch02_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(isch02_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_isch02_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(isch02_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_isch02_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(isch02_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_isch02_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(isch02_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_isch02_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(isch02_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_isch02_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(isch02_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_isch02_pm25_nonlinear_m7.rds");rm(g7)

#Ischemic Stroke
g0=gamm4(isch03_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_isch03_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(isch03_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_isch03_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(isch03_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_isch03_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(isch03_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_isch03_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(isch03_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_isch03_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(isch03_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_isch03_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(isch03_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_isch03_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(isch03_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_isch03_pm25_nonlinear_m7.rds");rm(g7)

#Ischemic Stroke
g0=gamm4(isch0203_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_isch0203_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(isch0203_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_isch0203_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(isch0203_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_isch0203_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(isch0203_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_isch0203_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(isch0203_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_isch0203_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(isch0203_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_isch0203_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(isch0203_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_isch0203_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(isch0203_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_isch0203_pm25_nonlinear_m7.rds");rm(g7)

#Hypertension
g0=gamm4(htn02_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_htn02_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(htn02_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_htn02_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(htn02_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_htn02_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(htn02_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_htn02_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(htn02_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_htn02_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(htn02_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_htn02_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(htn02_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_htn02_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(htn02_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_htn02_pm25_nonlinear_m7.rds");rm(g7)

#Otitis media
g0=gamm4(om02_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_om02_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(om02_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_om02_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(om02_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_om02_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(om02_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_om02_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(om02_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_om02_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(om02_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_om02_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(om02_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_om02_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(om02_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_om02_pm25_nonlinear_m7.rds");rm(g7)

#어지러움 
g0=gamm4(diz02_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_diz02_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(diz02_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_diz02_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(diz02_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_diz02_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(diz02_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_diz02_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(diz02_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_diz02_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(diz02_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_diz02_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(diz02_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_diz02_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(diz02_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_diz02_pm25_nonlinear_m7.rds");rm(g7)

#어지러움 
g0=gamm4(diz03_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_diz03_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(diz03_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_diz03_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(diz03_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_diz03_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(diz03_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_diz03_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(diz03_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_diz03_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(diz03_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_diz03_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(diz03_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_diz03_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(diz03_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_diz03_pm25_nonlinear_m7.rds");rm(g7)

#어지러움 
g0=gamm4(diz0203_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_diz0203_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(diz0203_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_diz0203_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(diz0203_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_diz0203_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(diz0203_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_diz0203_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(diz0203_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_diz0203_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(diz0203_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_diz0203_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(diz0203_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_diz0203_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(diz0203_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_diz0203_pm25_nonlinear_m7.rds");rm(g7)

#후각 장애
g0=gamm4(olfa03_tot~s(pm25_new)   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_olfa03_pm25_nonlinear_m0.rds");rm(g0)
g1=gamm4(olfa03_tot~s(pm25_new_m1)+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_olfa03_pm25_nonlinear_m1.rds");rm(g1)
g2=gamm4(olfa03_tot~s(pm25_new_m2)+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_olfa03_pm25_nonlinear_m2.rds");rm(g2)
g3=gamm4(olfa03_tot~s(pm25_new_m3)+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_olfa03_pm25_nonlinear_m3.rds");rm(g3)
g4=gamm4(olfa03_tot~s(pm25_new_m4)+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_olfa03_pm25_nonlinear_m4.rds");rm(g4)
g5=gamm4(olfa03_tot~s(pm25_new_m5)+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_olfa03_pm25_nonlinear_m5.rds");rm(g5)
g6=gamm4(olfa03_tot~s(pm25_new_m6)+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_olfa03_pm25_nonlinear_m6.rds");rm(g6)
g7=gamm4(olfa03_tot~s(pm25_new_m7)+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_olfa03_pm25_nonlinear_m7.rds");rm(g7)
