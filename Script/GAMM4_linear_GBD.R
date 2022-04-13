pacman::p_load(gamm4,mgcv,lme4,splines,ggplot2,gridExtra,dplyr,lubridate,scales,
               RColorBrewer)

#사망 정리자료 
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담")
daily_death_final<-read.csv("daily_death_final.csv")

daily_death_final$ddate=ymd(daily_death_final$ddate)
daily_death_final$sido_KN =with(daily_death_final,factor(sido_KN,levels=unique(sido_KN)))
daily_death_final$sidoname=with(daily_death_final,factor(sidoname,levels=unique(sidoname)))

#선형가정 추정한 모델 결과 
gamm4_result<-read_excel("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\KEI_SNU_TS_analysis_OJM_20220413.xlsx",
                         sheet="선형결과(gamm4)")

setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result\\도시별")

raw<-daily_death_final

#환산식 (SO2/NO2,O3: ug/m3 ; CO: mg/m3)
#ug/m3 -> ppb
#mg/m3 -> ppm
#ppm 기준농도 * 24.5/분자량
#직접 계산한 뒤 환산식 계산해주는 홈페이지에서 결과 비교 
so2_ref=40 *24.5/64.07/1000 #ppm
no2_ref=25 *24.5/46.01/1000 #ppm
co_ref =3.492               #ppm
o3_ref =60 *24.5/48/1000    #ppm

#노출변화량
raw$pm25_m0_diff=with(raw,ifelse(pm25_new   -15>0,pm25_new   -15,0))
raw$pm25_m1_diff=with(raw,ifelse(pm25_new_m1-15>0,pm25_new_m1-15,0))
raw$pm25_m2_diff=with(raw,ifelse(pm25_new_m2-15>0,pm25_new_m2-15,0))
raw$pm25_m3_diff=with(raw,ifelse(pm25_new_m3-15>0,pm25_new_m3-15,0))
raw$pm25_m4_diff=with(raw,ifelse(pm25_new_m4-15>0,pm25_new_m4-15,0))
raw$pm25_m5_diff=with(raw,ifelse(pm25_new_m5-15>0,pm25_new_m5-15,0))
raw$pm25_m6_diff=with(raw,ifelse(pm25_new_m6-15>0,pm25_new_m6-15,0))
raw$pm25_m7_diff=with(raw,ifelse(pm25_new_m7-15>0,pm25_new_m7-15,0))

raw$pm10_m0_diff=with(raw,ifelse(pm10   -45>0,pm10   -45,0))
raw$pm10_m1_diff=with(raw,ifelse(pm10_m1-45>0,pm10_m1-45,0))
raw$pm10_m2_diff=with(raw,ifelse(pm10_m2-45>0,pm10_m2-45,0))
raw$pm10_m3_diff=with(raw,ifelse(pm10_m3-45>0,pm10_m3-45,0))
raw$pm10_m4_diff=with(raw,ifelse(pm10_m4-45>0,pm10_m4-45,0))
raw$pm10_m5_diff=with(raw,ifelse(pm10_m5-45>0,pm10_m5-45,0))
raw$pm10_m6_diff=with(raw,ifelse(pm10_m6-45>0,pm10_m6-45,0))
raw$pm10_m7_diff=with(raw,ifelse(pm10_m7-45>0,pm10_m7-45,0))

raw$so2_m0_diff=with(raw,ifelse(so2   -so2_ref>0,so2   -so2_ref,0))
raw$so2_m1_diff=with(raw,ifelse(so2_m1-so2_ref>0,so2_m1-so2_ref,0))
raw$so2_m2_diff=with(raw,ifelse(so2_m2-so2_ref>0,so2_m2-so2_ref,0))
raw$so2_m3_diff=with(raw,ifelse(so2_m3-so2_ref>0,so2_m3-so2_ref,0))
raw$so2_m4_diff=with(raw,ifelse(so2_m4-so2_ref>0,so2_m4-so2_ref,0))
raw$so2_m5_diff=with(raw,ifelse(so2_m5-so2_ref>0,so2_m5-so2_ref,0))
raw$so2_m6_diff=with(raw,ifelse(so2_m6-so2_ref>0,so2_m6-so2_ref,0))
raw$so2_m7_diff=with(raw,ifelse(so2_m7-so2_ref>0,so2_m7-so2_ref,0))

raw$no2_m0_diff=with(raw,ifelse(no2   -no2_ref>0,no2   -no2_ref,0))
raw$no2_m1_diff=with(raw,ifelse(no2_m1-no2_ref>0,no2_m1-no2_ref,0))
raw$no2_m2_diff=with(raw,ifelse(no2_m2-no2_ref>0,no2_m2-no2_ref,0))
raw$no2_m3_diff=with(raw,ifelse(no2_m3-no2_ref>0,no2_m3-no2_ref,0))
raw$no2_m4_diff=with(raw,ifelse(no2_m4-no2_ref>0,no2_m4-no2_ref,0))
raw$no2_m5_diff=with(raw,ifelse(no2_m5-no2_ref>0,no2_m5-no2_ref,0))
raw$no2_m6_diff=with(raw,ifelse(no2_m6-no2_ref>0,no2_m6-no2_ref,0))
raw$no2_m7_diff=with(raw,ifelse(no2_m7-no2_ref>0,no2_m7-no2_ref,0))

raw$co_m0_diff=with(raw,ifelse(co   -co_ref>0,co   -co_ref,0))
raw$co_m1_diff=with(raw,ifelse(co_m1-co_ref>0,co_m1-co_ref,0))
raw$co_m2_diff=with(raw,ifelse(co_m2-co_ref>0,co_m2-co_ref,0))
raw$co_m3_diff=with(raw,ifelse(co_m3-co_ref>0,co_m3-co_ref,0))
raw$co_m4_diff=with(raw,ifelse(co_m4-co_ref>0,co_m4-co_ref,0))
raw$co_m5_diff=with(raw,ifelse(co_m5-co_ref>0,co_m5-co_ref,0))
raw$co_m6_diff=with(raw,ifelse(co_m6-co_ref>0,co_m6-co_ref,0))
raw$co_m7_diff=with(raw,ifelse(co_m7-co_ref>0,co_m7-co_ref,0))

raw$o3_m0_diff=with(raw,ifelse(o3   -o3_ref>0,o3   -o3_ref,0))
raw$o3_m1_diff=with(raw,ifelse(o3_m1-o3_ref>0,o3_m1-o3_ref,0))
raw$o3_m2_diff=with(raw,ifelse(o3_m2-o3_ref>0,o3_m2-o3_ref,0))
raw$o3_m3_diff=with(raw,ifelse(o3_m3-o3_ref>0,o3_m3-o3_ref,0))
raw$o3_m4_diff=with(raw,ifelse(o3_m4-o3_ref>0,o3_m4-o3_ref,0))
raw$o3_m5_diff=with(raw,ifelse(o3_m5-o3_ref>0,o3_m5-o3_ref,0))
raw$o3_m6_diff=with(raw,ifelse(o3_m6-o3_ref>0,o3_m6-o3_ref,0))
raw$o3_m7_diff=with(raw,ifelse(o3_m7-o3_ref>0,o3_m7-o3_ref,0))


gamm4_excess_func<-function(Y,X,expdiff,exposure,i,outcome){
  d<-raw
  d$expdiff=expdiff  #delta exposure (노출 변화량)
  d$exposue=exposure #노출변수 (대기오염)
  d$outcome=outcome  #결과변수 (사망자수, 입원자수)
  
  #기준농도-최저농도  (delta exposure)
  d$expdiff2=with(d,exposure-min(exposure,na.rm=T)
  
  #원시자료에서 보고자하는 노출시점(lag01이면 lag01노출에 대한)
  #노출 값 존재하는 경우에 대해서만 산출
  #추정한 RR 값 
  ss<-subset(gamm4_result, outcome==Y & exposure==X)[i,]
  RR    <-ss$RR
  RR_lci<-ss$RR_lci
  RR_uci<-ss$RR_uci
  
  #Attributable risk % (RR-1)/RR  ;  1-1/RR
  #AR% = (RR ??? 1) / RR x 100. AR% is also known as “Attributable Fraction (Exposed)”
  
  #시나리오 1: 농도차이(고농도만 관심)*관심질환(사망자 수)*Attributable risk
  d$e_dth    =with(d,1/ss$unit*expdiff*outcome*((RR-1)/RR))
  d$e_dth_lci=with(d,1/ss$unit*expdiff*outcome*((RR_lci-1)/RR_lci))
  d$e_dth_uci=with(d,1/ss$unit*expdiff*outcome*((RR_uci-1)/RR_uci))
  
  #시나리오 2: 농도차이(모든 농도고려)*관심질환(사망자 수)*Attributable risk
  d$e_dth2    =with(d,1/ss$unit*expdiff2*outcome*((RR-1)/RR))
  d$e_dth_lci2=with(d,1/ss$unit*expdiff2*outcome*((RR_lci-1)/RR_lci))
  d$e_dth_uci2=with(d,1/ss$unit*expdiff2*outcome*((RR_uci-1)/RR_uci))
  
  d2<-d %>% group_by(sido_KN,year) %>% summarise(e_dth=sum(e_dth,na.rm=T),
                                            e_dth_lci =sum(e_dth_lci,na.rm=T),
                                            e_dth_uci =sum(e_dth_uci,na.rm=T),
                                            e_dth2    =sum(e_dth2,na.rm=T),
                                            e_dth_lci2=sum(e_dth_lci2,na.rm=T),
                                            e_dth_uci2=sum(e_dth_uci2,na.rm=T)) %>%
    
    mutate(outcome=Y,exposure=X,lag=paste0("lag0",i-1))
  
  d2 %>% select(outcome:lag,sido_KN,year:e_dth_uci2)
}

#전체 사망 & 초미세먼지 (by gamm4)
tb0<-gamm4_excess_func("all_cause","PM25",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$TOT)
tb1<-gamm4_excess_func("all_cause","PM25",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$TOT)
tb2<-gamm4_excess_func("all_cause","PM25",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$TOT)
tb3<-gamm4_excess_func("all_cause","PM25",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$TOT)
tb4<-gamm4_excess_func("all_cause","PM25",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$TOT)
tb5<-gamm4_excess_func("all_cause","PM25",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$TOT)
tb6<-gamm4_excess_func("all_cause","PM25",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$TOT)
tb7<-gamm4_excess_func("all_cause","PM25",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$TOT)
tb_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 사망 & 미세먼지 (by gamm4)
tb0<-gamm4_excess_func("all_cause","PM10",raw$pm10_m0_diff,raw$pm10   ,1,raw$TOT)
tb1<-gamm4_excess_func("all_cause","PM10",raw$pm10_m1_diff,raw$pm10_m1,2,raw$TOT)
tb2<-gamm4_excess_func("all_cause","PM10",raw$pm10_m2_diff,raw$pm10_m2,3,raw$TOT)
tb3<-gamm4_excess_func("all_cause","PM10",raw$pm10_m3_diff,raw$pm10_m3,4,raw$TOT)
tb4<-gamm4_excess_func("all_cause","PM10",raw$pm10_m4_diff,raw$pm10_m4,5,raw$TOT)
tb5<-gamm4_excess_func("all_cause","PM10",raw$pm10_m5_diff,raw$pm10_m5,6,raw$TOT)
tb6<-gamm4_excess_func("all_cause","PM10",raw$pm10_m6_diff,raw$pm10_m6,7,raw$TOT)
tb7<-gamm4_excess_func("all_cause","PM10",raw$pm10_m7_diff,raw$pm10_m7,8,raw$TOT)
tb_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 사망 & 이산화황 (by gamm4)
tb0<-gamm4_excess_func("all_cause","SO2",raw$so2_m0_diff,raw$so2   ,1,raw$TOT)
tb1<-gamm4_excess_func("all_cause","SO2",raw$so2_m1_diff,raw$so2_m1,2,raw$TOT)
tb2<-gamm4_excess_func("all_cause","SO2",raw$so2_m2_diff,raw$so2_m2,3,raw$TOT)
tb3<-gamm4_excess_func("all_cause","SO2",raw$so2_m3_diff,raw$so2_m3,4,raw$TOT)
tb4<-gamm4_excess_func("all_cause","SO2",raw$so2_m4_diff,raw$so2_m4,5,raw$TOT)
tb5<-gamm4_excess_func("all_cause","SO2",raw$so2_m5_diff,raw$so2_m5,6,raw$TOT)
tb6<-gamm4_excess_func("all_cause","SO2",raw$so2_m6_diff,raw$so2_m6,7,raw$TOT)
tb7<-gamm4_excess_func("all_cause","SO2",raw$so2_m7_diff,raw$so2_m7,8,raw$TOT)
tb_so2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 사망 & 이산화질소 (by gamm4)
tb0<-gamm4_excess_func("all_cause","NO2",raw$no2_m0_diff,raw$no2   ,1,raw$TOT)
tb1<-gamm4_excess_func("all_cause","NO2",raw$no2_m1_diff,raw$no2_m1,2,raw$TOT)
tb2<-gamm4_excess_func("all_cause","NO2",raw$no2_m2_diff,raw$no2_m2,3,raw$TOT)
tb3<-gamm4_excess_func("all_cause","NO2",raw$no2_m3_diff,raw$no2_m3,4,raw$TOT)
tb4<-gamm4_excess_func("all_cause","NO2",raw$no2_m4_diff,raw$no2_m4,5,raw$TOT)
tb5<-gamm4_excess_func("all_cause","NO2",raw$no2_m5_diff,raw$no2_m5,6,raw$TOT)
tb6<-gamm4_excess_func("all_cause","NO2",raw$no2_m6_diff,raw$no2_m6,7,raw$TOT)
tb7<-gamm4_excess_func("all_cause","NO2",raw$no2_m7_diff,raw$no2_m7,8,raw$TOT)
tb_no2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 사망 & 일산화탄소 (by gamm4)
tb0<-gamm4_excess_func("all_cause","CO",raw$co_m0_diff,raw$co   ,1,raw$TOT)
tb1<-gamm4_excess_func("all_cause","CO",raw$co_m1_diff,raw$co_m1,2,raw$TOT)
tb2<-gamm4_excess_func("all_cause","CO",raw$co_m2_diff,raw$co_m2,3,raw$TOT)
tb3<-gamm4_excess_func("all_cause","CO",raw$co_m3_diff,raw$co_m3,4,raw$TOT)
tb4<-gamm4_excess_func("all_cause","CO",raw$co_m4_diff,raw$co_m4,5,raw$TOT)
tb5<-gamm4_excess_func("all_cause","CO",raw$co_m5_diff,raw$co_m5,6,raw$TOT)
tb6<-gamm4_excess_func("all_cause","CO",raw$co_m6_diff,raw$co_m6,7,raw$TOT)
tb7<-gamm4_excess_func("all_cause","CO",raw$co_m7_diff,raw$co_m7,8,raw$TOT)
tb_co<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 사망 & 오존 (by gamm4)
tb0<-gamm4_excess_func("all_cause","O3",raw$o3_m0_diff,raw$o3   ,1,raw$TOT)
tb1<-gamm4_excess_func("all_cause","O3",raw$o3_m1_diff,raw$o3_m1,2,raw$TOT)
tb2<-gamm4_excess_func("all_cause","O3",raw$o3_m2_diff,raw$o3_m2,3,raw$TOT)
tb3<-gamm4_excess_func("all_cause","O3",raw$o3_m3_diff,raw$o3_m3,4,raw$TOT)
tb4<-gamm4_excess_func("all_cause","O3",raw$o3_m4_diff,raw$o3_m4,5,raw$TOT)
tb5<-gamm4_excess_func("all_cause","O3",raw$o3_m5_diff,raw$o3_m5,6,raw$TOT)
tb6<-gamm4_excess_func("all_cause","O3",raw$o3_m6_diff,raw$o3_m6,7,raw$TOT)
tb7<-gamm4_excess_func("all_cause","O3",raw$o3_m7_diff,raw$o3_m7,8,raw$TOT)
tb_o3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb_tot_ap<-rbind(tb_pm25,tb_pm10,tb_so2,tb_no2,tb_co,tb_o3)
write.csv(tb_tot_ap,file="gamm4_tot_AP.csv"  ,row.names=F,na="")
#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#

#전체 비사고사망 & 초미세먼지 (by gamm4)
tb0<-gamm4_excess_func("nonacc","PM25",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$NON_ACC)
tb1<-gamm4_excess_func("nonacc","PM25",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$NON_ACC)
tb2<-gamm4_excess_func("nonacc","PM25",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$NON_ACC)
tb3<-gamm4_excess_func("nonacc","PM25",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$NON_ACC)
tb4<-gamm4_excess_func("nonacc","PM25",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$NON_ACC)
tb5<-gamm4_excess_func("nonacc","PM25",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$NON_ACC)
tb6<-gamm4_excess_func("nonacc","PM25",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$NON_ACC)
tb7<-gamm4_excess_func("nonacc","PM25",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$NON_ACC)
tb_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 비사고사망 & 미세먼지 (by gamm4)
tb0<-gamm4_excess_func("nonacc","PM10",raw$pm10_m0_diff,raw$pm10   ,1,raw$NON_ACC)
tb1<-gamm4_excess_func("nonacc","PM10",raw$pm10_m1_diff,raw$pm10_m1,2,raw$NON_ACC)
tb2<-gamm4_excess_func("nonacc","PM10",raw$pm10_m2_diff,raw$pm10_m2,3,raw$NON_ACC)
tb3<-gamm4_excess_func("nonacc","PM10",raw$pm10_m3_diff,raw$pm10_m3,4,raw$NON_ACC)
tb4<-gamm4_excess_func("nonacc","PM10",raw$pm10_m4_diff,raw$pm10_m4,5,raw$NON_ACC)
tb5<-gamm4_excess_func("nonacc","PM10",raw$pm10_m5_diff,raw$pm10_m5,6,raw$NON_ACC)
tb6<-gamm4_excess_func("nonacc","PM10",raw$pm10_m6_diff,raw$pm10_m6,7,raw$NON_ACC)
tb7<-gamm4_excess_func("nonacc","PM10",raw$pm10_m7_diff,raw$pm10_m7,8,raw$NON_ACC)
tb_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 비사고사망 & 이산화황 (by gamm4)
tb0<-gamm4_excess_func("nonacc","SO2",raw$so2_m0_diff,raw$so2   ,1,raw$NON_ACC)
tb1<-gamm4_excess_func("nonacc","SO2",raw$so2_m1_diff,raw$so2_m1,2,raw$NON_ACC)
tb2<-gamm4_excess_func("nonacc","SO2",raw$so2_m2_diff,raw$so2_m2,3,raw$NON_ACC)
tb3<-gamm4_excess_func("nonacc","SO2",raw$so2_m3_diff,raw$so2_m3,4,raw$NON_ACC)
tb4<-gamm4_excess_func("nonacc","SO2",raw$so2_m4_diff,raw$so2_m4,5,raw$NON_ACC)
tb5<-gamm4_excess_func("nonacc","SO2",raw$so2_m5_diff,raw$so2_m5,6,raw$NON_ACC)
tb6<-gamm4_excess_func("nonacc","SO2",raw$so2_m6_diff,raw$so2_m6,7,raw$NON_ACC)
tb7<-gamm4_excess_func("nonacc","SO2",raw$so2_m7_diff,raw$so2_m7,8,raw$NON_ACC)
tb_so2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 비사고사망 & 이산화질소 (by gamm4)
tb0<-gamm4_excess_func("nonacc","NO2",raw$no2_m0_diff,raw$no2   ,1,raw$NON_ACC)
tb1<-gamm4_excess_func("nonacc","NO2",raw$no2_m1_diff,raw$no2_m1,2,raw$NON_ACC)
tb2<-gamm4_excess_func("nonacc","NO2",raw$no2_m2_diff,raw$no2_m2,3,raw$NON_ACC)
tb3<-gamm4_excess_func("nonacc","NO2",raw$no2_m3_diff,raw$no2_m3,4,raw$NON_ACC)
tb4<-gamm4_excess_func("nonacc","NO2",raw$no2_m4_diff,raw$no2_m4,5,raw$NON_ACC)
tb5<-gamm4_excess_func("nonacc","NO2",raw$no2_m5_diff,raw$no2_m5,6,raw$NON_ACC)
tb6<-gamm4_excess_func("nonacc","NO2",raw$no2_m6_diff,raw$no2_m6,7,raw$NON_ACC)
tb7<-gamm4_excess_func("nonacc","NO2",raw$no2_m7_diff,raw$no2_m7,8,raw$NON_ACC)
tb_no2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 비사고사망 & 일산화탄소 (by gamm4)
tb0<-gamm4_excess_func("nonacc","CO",raw$co_m0_diff,raw$co   ,1,raw$NON_ACC)
tb1<-gamm4_excess_func("nonacc","CO",raw$co_m1_diff,raw$co_m1,2,raw$NON_ACC)
tb2<-gamm4_excess_func("nonacc","CO",raw$co_m2_diff,raw$co_m2,3,raw$NON_ACC)
tb3<-gamm4_excess_func("nonacc","CO",raw$co_m3_diff,raw$co_m3,4,raw$NON_ACC)
tb4<-gamm4_excess_func("nonacc","CO",raw$co_m4_diff,raw$co_m4,5,raw$NON_ACC)
tb5<-gamm4_excess_func("nonacc","CO",raw$co_m5_diff,raw$co_m5,6,raw$NON_ACC)
tb6<-gamm4_excess_func("nonacc","CO",raw$co_m6_diff,raw$co_m6,7,raw$NON_ACC)
tb7<-gamm4_excess_func("nonacc","CO",raw$co_m7_diff,raw$co_m7,8,raw$NON_ACC)
tb_co<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 비사고사망 & 오존 (by gamm4)
tb0<-gamm4_excess_func("nonacc","O3",raw$o3_m0_diff,raw$o3   ,1,raw$NON_ACC)
tb1<-gamm4_excess_func("nonacc","O3",raw$o3_m1_diff,raw$o3_m1,2,raw$NON_ACC)
tb2<-gamm4_excess_func("nonacc","O3",raw$o3_m2_diff,raw$o3_m2,3,raw$NON_ACC)
tb3<-gamm4_excess_func("nonacc","O3",raw$o3_m3_diff,raw$o3_m3,4,raw$NON_ACC)
tb4<-gamm4_excess_func("nonacc","O3",raw$o3_m4_diff,raw$o3_m4,5,raw$NON_ACC)
tb5<-gamm4_excess_func("nonacc","O3",raw$o3_m5_diff,raw$o3_m5,6,raw$NON_ACC)
tb6<-gamm4_excess_func("nonacc","O3",raw$o3_m6_diff,raw$o3_m6,7,raw$NON_ACC)
tb7<-gamm4_excess_func("nonacc","O3",raw$o3_m7_diff,raw$o3_m7,8,raw$NON_ACC)
tb_o3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb_nonacc_ap<-rbind(tb_pm25,tb_pm10,tb_so2,tb_no2,tb_co,tb_o3)
write.csv(tb_nonacc_ap,file="gamm4_nonacc_AP.csv"  ,row.names=F,na="")

#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#

#전체 심혈관사망 & 초미세먼지 (by gamm4)
tb0<-gamm4_excess_func("cvd","PM25",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$CVD)
tb1<-gamm4_excess_func("cvd","PM25",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$CVD)
tb2<-gamm4_excess_func("cvd","PM25",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$CVD)
tb3<-gamm4_excess_func("cvd","PM25",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$CVD)
tb4<-gamm4_excess_func("cvd","PM25",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$CVD)
tb5<-gamm4_excess_func("cvd","PM25",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$CVD)
tb6<-gamm4_excess_func("cvd","PM25",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$CVD)
tb7<-gamm4_excess_func("cvd","PM25",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$CVD)
tb_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 심혈관사망 & 미세먼지 (by gamm4)
tb0<-gamm4_excess_func("cvd","PM10",raw$pm10_m0_diff,raw$pm10   ,1,raw$CVD)
tb1<-gamm4_excess_func("cvd","PM10",raw$pm10_m1_diff,raw$pm10_m1,2,raw$CVD)
tb2<-gamm4_excess_func("cvd","PM10",raw$pm10_m2_diff,raw$pm10_m2,3,raw$CVD)
tb3<-gamm4_excess_func("cvd","PM10",raw$pm10_m3_diff,raw$pm10_m3,4,raw$CVD)
tb4<-gamm4_excess_func("cvd","PM10",raw$pm10_m4_diff,raw$pm10_m4,5,raw$CVD)
tb5<-gamm4_excess_func("cvd","PM10",raw$pm10_m5_diff,raw$pm10_m5,6,raw$CVD)
tb6<-gamm4_excess_func("cvd","PM10",raw$pm10_m6_diff,raw$pm10_m6,7,raw$CVD)
tb7<-gamm4_excess_func("cvd","PM10",raw$pm10_m7_diff,raw$pm10_m7,8,raw$CVD)
tb_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 심혈관사망 & 이산화황 (by gamm4)
tb0<-gamm4_excess_func("cvd","SO2",raw$so2_m0_diff,raw$so2   ,1,raw$CVD)
tb1<-gamm4_excess_func("cvd","SO2",raw$so2_m1_diff,raw$so2_m1,2,raw$CVD)
tb2<-gamm4_excess_func("cvd","SO2",raw$so2_m2_diff,raw$so2_m2,3,raw$CVD)
tb3<-gamm4_excess_func("cvd","SO2",raw$so2_m3_diff,raw$so2_m3,4,raw$CVD)
tb4<-gamm4_excess_func("cvd","SO2",raw$so2_m4_diff,raw$so2_m4,5,raw$CVD)
tb5<-gamm4_excess_func("cvd","SO2",raw$so2_m5_diff,raw$so2_m5,6,raw$CVD)
tb6<-gamm4_excess_func("cvd","SO2",raw$so2_m6_diff,raw$so2_m6,7,raw$CVD)
tb7<-gamm4_excess_func("cvd","SO2",raw$so2_m7_diff,raw$so2_m7,8,raw$CVD)
tb_so2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 심혈관사망 & 이산화질소 (by gamm4)
tb0<-gamm4_excess_func("cvd","NO2",raw$no2_m0_diff,raw$no2   ,1,raw$CVD)
tb1<-gamm4_excess_func("cvd","NO2",raw$no2_m1_diff,raw$no2_m1,2,raw$CVD)
tb2<-gamm4_excess_func("cvd","NO2",raw$no2_m2_diff,raw$no2_m2,3,raw$CVD)
tb3<-gamm4_excess_func("cvd","NO2",raw$no2_m3_diff,raw$no2_m3,4,raw$CVD)
tb4<-gamm4_excess_func("cvd","NO2",raw$no2_m4_diff,raw$no2_m4,5,raw$CVD)
tb5<-gamm4_excess_func("cvd","NO2",raw$no2_m5_diff,raw$no2_m5,6,raw$CVD)
tb6<-gamm4_excess_func("cvd","NO2",raw$no2_m6_diff,raw$no2_m6,7,raw$CVD)
tb7<-gamm4_excess_func("cvd","NO2",raw$no2_m7_diff,raw$no2_m7,8,raw$CVD)
tb_no2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 심혈관사망 & 일산화탄소 (by gamm4)
tb0<-gamm4_excess_func("cvd","CO",raw$co_m0_diff,raw$co   ,1,raw$CVD)
tb1<-gamm4_excess_func("cvd","CO",raw$co_m1_diff,raw$co_m1,2,raw$CVD)
tb2<-gamm4_excess_func("cvd","CO",raw$co_m2_diff,raw$co_m2,3,raw$CVD)
tb3<-gamm4_excess_func("cvd","CO",raw$co_m3_diff,raw$co_m3,4,raw$CVD)
tb4<-gamm4_excess_func("cvd","CO",raw$co_m4_diff,raw$co_m4,5,raw$CVD)
tb5<-gamm4_excess_func("cvd","CO",raw$co_m5_diff,raw$co_m5,6,raw$CVD)
tb6<-gamm4_excess_func("cvd","CO",raw$co_m6_diff,raw$co_m6,7,raw$CVD)
tb7<-gamm4_excess_func("cvd","CO",raw$co_m7_diff,raw$co_m7,8,raw$CVD)
tb_co<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 심혈관사망 & 오존 (by gamm4)
tb0<-gamm4_excess_func("cvd","O3",raw$o3_m0_diff,raw$o3   ,1,raw$CVD)
tb1<-gamm4_excess_func("cvd","O3",raw$o3_m1_diff,raw$o3_m1,2,raw$CVD)
tb2<-gamm4_excess_func("cvd","O3",raw$o3_m2_diff,raw$o3_m2,3,raw$CVD)
tb3<-gamm4_excess_func("cvd","O3",raw$o3_m3_diff,raw$o3_m3,4,raw$CVD)
tb4<-gamm4_excess_func("cvd","O3",raw$o3_m4_diff,raw$o3_m4,5,raw$CVD)
tb5<-gamm4_excess_func("cvd","O3",raw$o3_m5_diff,raw$o3_m5,6,raw$CVD)
tb6<-gamm4_excess_func("cvd","O3",raw$o3_m6_diff,raw$o3_m6,7,raw$CVD)
tb7<-gamm4_excess_func("cvd","O3",raw$o3_m7_diff,raw$o3_m7,8,raw$CVD)
tb_o3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb_cvd_ap<-rbind(tb_pm25,tb_pm10,tb_so2,tb_no2,tb_co,tb_o3)
write.csv(tb_cvd_ap,file="gamm4_cvd_AP.csv"  ,row.names=F,na="")

#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#

#전체 호흡기사망 & 초미세먼지 (by gamm4)
tb0<-gamm4_excess_func("respiratory","PM25",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$RESP)
tb1<-gamm4_excess_func("respiratory","PM25",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$RESP)
tb2<-gamm4_excess_func("respiratory","PM25",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$RESP)
tb3<-gamm4_excess_func("respiratory","PM25",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$RESP)
tb4<-gamm4_excess_func("respiratory","PM25",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$RESP)
tb5<-gamm4_excess_func("respiratory","PM25",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$RESP)
tb6<-gamm4_excess_func("respiratory","PM25",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$RESP)
tb7<-gamm4_excess_func("respiratory","PM25",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$RESP)
tb_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 호흡기사망 & 미세먼지 (by gamm4)
tb0<-gamm4_excess_func("respiratory","PM10",raw$pm10_m0_diff,raw$pm10   ,1,raw$RESP)
tb1<-gamm4_excess_func("respiratory","PM10",raw$pm10_m1_diff,raw$pm10_m1,2,raw$RESP)
tb2<-gamm4_excess_func("respiratory","PM10",raw$pm10_m2_diff,raw$pm10_m2,3,raw$RESP)
tb3<-gamm4_excess_func("respiratory","PM10",raw$pm10_m3_diff,raw$pm10_m3,4,raw$RESP)
tb4<-gamm4_excess_func("respiratory","PM10",raw$pm10_m4_diff,raw$pm10_m4,5,raw$RESP)
tb5<-gamm4_excess_func("respiratory","PM10",raw$pm10_m5_diff,raw$pm10_m5,6,raw$RESP)
tb6<-gamm4_excess_func("respiratory","PM10",raw$pm10_m6_diff,raw$pm10_m6,7,raw$RESP)
tb7<-gamm4_excess_func("respiratory","PM10",raw$pm10_m7_diff,raw$pm10_m7,8,raw$RESP)
tb_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 호흡기사망 & 이산화황 (by gamm4)
tb0<-gamm4_excess_func("respiratory","SO2",raw$so2_m0_diff,raw$so2   ,1,raw$RESP)
tb1<-gamm4_excess_func("respiratory","SO2",raw$so2_m1_diff,raw$so2_m1,2,raw$RESP)
tb2<-gamm4_excess_func("respiratory","SO2",raw$so2_m2_diff,raw$so2_m2,3,raw$RESP)
tb3<-gamm4_excess_func("respiratory","SO2",raw$so2_m3_diff,raw$so2_m3,4,raw$RESP)
tb4<-gamm4_excess_func("respiratory","SO2",raw$so2_m4_diff,raw$so2_m4,5,raw$RESP)
tb5<-gamm4_excess_func("respiratory","SO2",raw$so2_m5_diff,raw$so2_m5,6,raw$RESP)
tb6<-gamm4_excess_func("respiratory","SO2",raw$so2_m6_diff,raw$so2_m6,7,raw$RESP)
tb7<-gamm4_excess_func("respiratory","SO2",raw$so2_m7_diff,raw$so2_m7,8,raw$RESP)
tb_so2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 호흡기사망 & 이산화질소 (by gamm4)
tb0<-gamm4_excess_func("respiratory","NO2",raw$no2_m0_diff,raw$no2   ,1,raw$RESP)
tb1<-gamm4_excess_func("respiratory","NO2",raw$no2_m1_diff,raw$no2_m1,2,raw$RESP)
tb2<-gamm4_excess_func("respiratory","NO2",raw$no2_m2_diff,raw$no2_m2,3,raw$RESP)
tb3<-gamm4_excess_func("respiratory","NO2",raw$no2_m3_diff,raw$no2_m3,4,raw$RESP)
tb4<-gamm4_excess_func("respiratory","NO2",raw$no2_m4_diff,raw$no2_m4,5,raw$RESP)
tb5<-gamm4_excess_func("respiratory","NO2",raw$no2_m5_diff,raw$no2_m5,6,raw$RESP)
tb6<-gamm4_excess_func("respiratory","NO2",raw$no2_m6_diff,raw$no2_m6,7,raw$RESP)
tb7<-gamm4_excess_func("respiratory","NO2",raw$no2_m7_diff,raw$no2_m7,8,raw$RESP)
tb_no2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 호흡기사망 & 일산화탄소 (by gamm4)
tb0<-gamm4_excess_func("respiratory","CO",raw$co_m0_diff,raw$co   ,1,raw$RESP)
tb1<-gamm4_excess_func("respiratory","CO",raw$co_m1_diff,raw$co_m1,2,raw$RESP)
tb2<-gamm4_excess_func("respiratory","CO",raw$co_m2_diff,raw$co_m2,3,raw$RESP)
tb3<-gamm4_excess_func("respiratory","CO",raw$co_m3_diff,raw$co_m3,4,raw$RESP)
tb4<-gamm4_excess_func("respiratory","CO",raw$co_m4_diff,raw$co_m4,5,raw$RESP)
tb5<-gamm4_excess_func("respiratory","CO",raw$co_m5_diff,raw$co_m5,6,raw$RESP)
tb6<-gamm4_excess_func("respiratory","CO",raw$co_m6_diff,raw$co_m6,7,raw$RESP)
tb7<-gamm4_excess_func("respiratory","CO",raw$co_m7_diff,raw$co_m7,8,raw$RESP)
tb_co<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#전체 호흡기사망 & 오존 (by gamm4)
tb0<-gamm4_excess_func("respiratory","O3",raw$o3_m0_diff,raw$o3   ,1,raw$RESP)
tb1<-gamm4_excess_func("respiratory","O3",raw$o3_m1_diff,raw$o3_m1,2,raw$RESP)
tb2<-gamm4_excess_func("respiratory","O3",raw$o3_m2_diff,raw$o3_m2,3,raw$RESP)
tb3<-gamm4_excess_func("respiratory","O3",raw$o3_m3_diff,raw$o3_m3,4,raw$RESP)
tb4<-gamm4_excess_func("respiratory","O3",raw$o3_m4_diff,raw$o3_m4,5,raw$RESP)
tb5<-gamm4_excess_func("respiratory","O3",raw$o3_m5_diff,raw$o3_m5,6,raw$RESP)
tb6<-gamm4_excess_func("respiratory","O3",raw$o3_m6_diff,raw$o3_m6,7,raw$RESP)
tb7<-gamm4_excess_func("respiratory","O3",raw$o3_m7_diff,raw$o3_m7,8,raw$RESP)
tb_o3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb_respiratory_ap<-rbind(tb_pm25,tb_pm10,tb_so2,tb_no2,tb_co,tb_o3)
write.csv(tb_respiratory_ap,file="gamm4_respiratory_AP.csv"  ,row.names=F,na="")


