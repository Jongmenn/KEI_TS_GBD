#library
pacman::p_load(gamm4,mgcv,lme4,splines,ggplot2,gridExtra,dplyr,lubridate,scales,RColorBrewer)

#---------------------------------------------------------------------------------------------------------------------------#
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담")

dat_tot<-read_excel("KEI_SNU_TS_analysis_OJM_20220603.xlsx",sheet="통계청사망_선형결과(gamm4)")
dat_sub<-read_excel("KEI_SNU_TS_analysis_OJM_20220603.xlsx",sheet="층화분석")

dat_tot$outcome<-gsub("all_cause","All-cause",dat_tot$outcome)
dat_tot$outcome<-gsub("cvd","CVD",dat_tot$outcome)
dat_tot$subgroup="Total"


dat_m<-rbind(dat_tot %>% dplyr:: select(outcome,exposure,subgroup,lag,RR,RR_lci,RR_uci),
             dat_sub %>% dplyr:: select(outcome,exposure,subgroup,lag,RR,RR_lci,RR_uci))

dat_m$subgroup=gsub("AG0014","Age group 0-14" ,dat_m$subgroup)
dat_m$subgroup=gsub("AG1564","Age group 15-64",dat_m$subgroup)
dat_m$subgroup=gsub("AG65"  ,"Age group 65+"  ,dat_m$subgroup)
dat_m$subgroup=gsub("SEX_F"  ,"Women"        ,dat_m$subgroup)
dat_m$subgroup=gsub("SEX_M"  ,"Men"          ,dat_m$subgroup)

dat_m$subgroup=factor(dat_m$subgroup,levels=c("Total","Age group 0-14","Age group 15-64","Age group 65+",
                                              "Women","Men"))


all_mor1<-dat_m %>% filter(outcome=="All-cause" & exposure=="PM25")
all_mor2<-dat_m %>% filter(outcome=="All-cause" & exposure=="PM10")
all_mor3<-dat_m %>% filter(outcome=="All-cause" & exposure=="O3")

nonacc_mor1<-dat_m %>% filter(outcome=="nonacc" & exposure=="PM25")
nonacc_mor2<-dat_m %>% filter(outcome=="nonacc" & exposure=="PM10")
nonacc_mor3<-dat_m %>% filter(outcome=="nonacc" & exposure=="O3")

cvd_mor1<-dat_m %>% filter(outcome=="CVD" & exposure=="PM25")
cvd_mor2<-dat_m %>% filter(outcome=="CVD" & exposure=="PM10")
cvd_mor3<-dat_m %>% filter(outcome=="CVD" & exposure=="O3")

res_mor1<-dat_m %>% filter(outcome=="respiratory" & exposure=="PM25")
res_mor2<-dat_m %>% filter(outcome=="respiratory" & exposure=="PM10")
res_mor3<-dat_m %>% filter(outcome=="respiratory" & exposure=="O3")

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
ggplot_rr<-function(data,ylimit1,ylimit2){
  dd<-data
  x11();ggplot(dd,aes(lag,RR,group=subgroup))+geom_point(aes(shape=subgroup),size=4,position=position_dodge(0.5))+
    geom_errorbar(aes(ymin=RR_lci,ymax=RR_uci),position=position_dodge(0.5),width=0.5,lwd=1)+
    scale_shape_manual(values=c(19,15,17,18,0,2))+theme_gray(base_size=30)+
    theme(legend.position = "top",legend.title = element_blank())+
    labs(x="",y="Relative Risk (95% Confidence intervals)")+
    geom_hline(yintercept=1,col="red",linetype=2)+coord_cartesian(ylim=c(ylimit1,ylimit2))}

ggplot_rr(all_mor1,0.999,1.003)
ggplot_rr(all_mor2,0.999,1.003)
ggplot_rr(all_mor3,0.999,1.003)

ggplot_rr(nonacc_mor1,0.999,1.003)
ggplot_rr(nonacc_mor2,0.999,1.003)
ggplot_rr(nonacc_mor3,0.998,1.003)

cvd_mor1.r<-dat_m %>% filter(outcome=="CVD" & exposure=="PM25") %>% filter(!subgroup %in% c("Age group 0-14") )
cvd_mor2.r<-dat_m %>% filter(outcome=="CVD" & exposure=="PM10") %>% filter(!subgroup %in% c("Age group 0-14") )
cvd_mor3.r<-dat_m %>% filter(outcome=="CVD" & exposure=="O3")   %>% filter(!subgroup %in% c("Age group 0-14") )

ggplot_rr(cvd_mor1,0.98,1.003)

ggplot_rr(cvd_mor1.r,0.998,1.001)
ggplot_rr(cvd_mor2.r,0.998,1.001)
ggplot_rr(cvd_mor3.r,0.998,1.002)

ggplot_rr(res_mor1,0.992,1.02)
ggplot_rr(res_mor2,0.992,1.01)
ggplot_rr(res_mor3,0.992,1.02)

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

#사망 정리자료 
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담")
daily_death_final<-read.csv("daily_death_final.csv",fileEncoding = "euc-kr")

daily_death_final$ddate=ymd(daily_death_final$ddate)
daily_death_final$sido_KN =with(daily_death_final,factor(sido_KN,levels=unique(sido_KN)))
daily_death_final$sidoname=with(daily_death_final,factor(sidoname,levels=unique(sidoname)))

#선형가정 추정한 모델 결과 
gamm4_result<-read_excel("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\KEI_SNU_TS_analysis_OJM_20220603.xlsx",sheet="층화분석")

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
#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
gamm4_excess_func_strata<-function(Y,X,sub,expdiff,exposure,i,outcome){
  d<-raw
  
  d$expdiff=expdiff  #delta exposure (노출 변화량)
  d$exposue=exposure #노출변수 (대기오염)
  d$outcome=outcome  #결과변수 (사망자수, 입원자수)
  
  #기준농도-최저농도  (delta exposure)
  d$expdiff2=with(d,exposure-min(exposure,na.rm=T))
  
  #원시자료에서 보고자하는 노출시점(lag01이면 lag01노출에 대한)
  #노출 값 존재하는 경우에 대해서만 산출
  #추정한 RR 값 
  
  ss<-subset(gamm4_result, outcome==Y & exposure==X & subgroup==sub)[i,]
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
  
  d2 %>% dplyr:: select(outcome:lag,sido_KN,year:e_dth_uci2)
}

#전체 사망 & 초미세먼지 (by gamm4, linear)
tb0<-gamm4_excess_func_strata("All-cause","PM25","AG0014",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$TOT_AG0014)
tb1<-gamm4_excess_func_strata("All-cause","PM25","AG0014",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$TOT_AG0014)
tb2<-gamm4_excess_func_strata("All-cause","PM25","AG0014",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$TOT_AG0014)
tb3<-gamm4_excess_func_strata("All-cause","PM25","AG0014",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$TOT_AG0014)
tb4<-gamm4_excess_func_strata("All-cause","PM25","AG0014",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$TOT_AG0014)
tb5<-gamm4_excess_func_strata("All-cause","PM25","AG0014",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$TOT_AG0014)
tb6<-gamm4_excess_func_strata("All-cause","PM25","AG0014",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$TOT_AG0014)
tb7<-gamm4_excess_func_strata("All-cause","PM25","AG0014",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$TOT_AG0014)
tb_tot_pm25_s1<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("All-cause","PM25","AG1564",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$TOT_AG1564)
tb1<-gamm4_excess_func_strata("All-cause","PM25","AG1564",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$TOT_AG1564)
tb2<-gamm4_excess_func_strata("All-cause","PM25","AG1564",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$TOT_AG1564)
tb3<-gamm4_excess_func_strata("All-cause","PM25","AG1564",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$TOT_AG1564)
tb4<-gamm4_excess_func_strata("All-cause","PM25","AG1564",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$TOT_AG1564)
tb5<-gamm4_excess_func_strata("All-cause","PM25","AG1564",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$TOT_AG1564)
tb6<-gamm4_excess_func_strata("All-cause","PM25","AG1564",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$TOT_AG1564)
tb7<-gamm4_excess_func_strata("All-cause","PM25","AG1564",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$TOT_AG1564)
tb_tot_pm25_s2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("All-cause","PM25","AG65",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$TOT_AG65)
tb1<-gamm4_excess_func_strata("All-cause","PM25","AG65",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$TOT_AG65)
tb2<-gamm4_excess_func_strata("All-cause","PM25","AG65",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$TOT_AG65)
tb3<-gamm4_excess_func_strata("All-cause","PM25","AG65",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$TOT_AG65)
tb4<-gamm4_excess_func_strata("All-cause","PM25","AG65",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$TOT_AG65)
tb5<-gamm4_excess_func_strata("All-cause","PM25","AG65",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$TOT_AG65)
tb6<-gamm4_excess_func_strata("All-cause","PM25","AG65",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$TOT_AG65)
tb7<-gamm4_excess_func_strata("All-cause","PM25","AG65",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$TOT_AG65)
tb_tot_pm25_s3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("All-cause","PM25","SEX_F",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$TOT_SEX_F)
tb1<-gamm4_excess_func_strata("All-cause","PM25","SEX_F",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$TOT_SEX_F)
tb2<-gamm4_excess_func_strata("All-cause","PM25","SEX_F",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$TOT_SEX_F)
tb3<-gamm4_excess_func_strata("All-cause","PM25","SEX_F",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$TOT_SEX_F)
tb4<-gamm4_excess_func_strata("All-cause","PM25","SEX_F",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$TOT_SEX_F)
tb5<-gamm4_excess_func_strata("All-cause","PM25","SEX_F",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$TOT_SEX_F)
tb6<-gamm4_excess_func_strata("All-cause","PM25","SEX_F",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$TOT_SEX_F)
tb7<-gamm4_excess_func_strata("All-cause","PM25","SEX_F",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$TOT_SEX_F)
tb_tot_pm25_s4<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("All-cause","PM25","SEX_M",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$TOT_SEX_M)
tb1<-gamm4_excess_func_strata("All-cause","PM25","SEX_M",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$TOT_SEX_M)
tb2<-gamm4_excess_func_strata("All-cause","PM25","SEX_M",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$TOT_SEX_M)
tb3<-gamm4_excess_func_strata("All-cause","PM25","SEX_M",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$TOT_SEX_M)
tb4<-gamm4_excess_func_strata("All-cause","PM25","SEX_M",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$TOT_SEX_M)
tb5<-gamm4_excess_func_strata("All-cause","PM25","SEX_M",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$TOT_SEX_M)
tb6<-gamm4_excess_func_strata("All-cause","PM25","SEX_M",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$TOT_SEX_M)
tb7<-gamm4_excess_func_strata("All-cause","PM25","SEX_M",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$TOT_SEX_M)
tb_tot_pm25_s5<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#전체 사망 & 미세먼지 (by gamm4, linear)
tb0<-gamm4_excess_func_strata("All-cause","PM10","AG0014",raw$pm10_m0_diff,raw$pm10   ,1,raw$TOT_AG0014)
tb1<-gamm4_excess_func_strata("All-cause","PM10","AG0014",raw$pm10_m1_diff,raw$pm10_m1,2,raw$TOT_AG0014)
tb2<-gamm4_excess_func_strata("All-cause","PM10","AG0014",raw$pm10_m2_diff,raw$pm10_m2,3,raw$TOT_AG0014)
tb3<-gamm4_excess_func_strata("All-cause","PM10","AG0014",raw$pm10_m3_diff,raw$pm10_m3,4,raw$TOT_AG0014)
tb4<-gamm4_excess_func_strata("All-cause","PM10","AG0014",raw$pm10_m4_diff,raw$pm10_m4,5,raw$TOT_AG0014)
tb5<-gamm4_excess_func_strata("All-cause","PM10","AG0014",raw$pm10_m5_diff,raw$pm10_m5,6,raw$TOT_AG0014)
tb6<-gamm4_excess_func_strata("All-cause","PM10","AG0014",raw$pm10_m6_diff,raw$pm10_m6,7,raw$TOT_AG0014)
tb7<-gamm4_excess_func_strata("All-cause","PM10","AG0014",raw$pm10_m7_diff,raw$pm10_m7,8,raw$TOT_AG0014)
tb_tot_pm10_s1<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("All-cause","PM10","AG1564",raw$pm10_m0_diff,raw$pm10   ,1,raw$TOT_AG1564)
tb1<-gamm4_excess_func_strata("All-cause","PM10","AG1564",raw$pm10_m1_diff,raw$pm10_m1,2,raw$TOT_AG1564)
tb2<-gamm4_excess_func_strata("All-cause","PM10","AG1564",raw$pm10_m2_diff,raw$pm10_m2,3,raw$TOT_AG1564)
tb3<-gamm4_excess_func_strata("All-cause","PM10","AG1564",raw$pm10_m3_diff,raw$pm10_m3,4,raw$TOT_AG1564)
tb4<-gamm4_excess_func_strata("All-cause","PM10","AG1564",raw$pm10_m4_diff,raw$pm10_m4,5,raw$TOT_AG1564)
tb5<-gamm4_excess_func_strata("All-cause","PM10","AG1564",raw$pm10_m5_diff,raw$pm10_m5,6,raw$TOT_AG1564)
tb6<-gamm4_excess_func_strata("All-cause","PM10","AG1564",raw$pm10_m6_diff,raw$pm10_m6,7,raw$TOT_AG1564)
tb7<-gamm4_excess_func_strata("All-cause","PM10","AG1564",raw$pm10_m7_diff,raw$pm10_m7,8,raw$TOT_AG1564)
tb_tot_pm10_s2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("All-cause","PM10","AG65",raw$pm10_m0_diff,raw$pm10   ,1,raw$TOT_AG65)
tb1<-gamm4_excess_func_strata("All-cause","PM10","AG65",raw$pm10_m1_diff,raw$pm10_m1,2,raw$TOT_AG65)
tb2<-gamm4_excess_func_strata("All-cause","PM10","AG65",raw$pm10_m2_diff,raw$pm10_m2,3,raw$TOT_AG65)
tb3<-gamm4_excess_func_strata("All-cause","PM10","AG65",raw$pm10_m3_diff,raw$pm10_m3,4,raw$TOT_AG65)
tb4<-gamm4_excess_func_strata("All-cause","PM10","AG65",raw$pm10_m4_diff,raw$pm10_m4,5,raw$TOT_AG65)
tb5<-gamm4_excess_func_strata("All-cause","PM10","AG65",raw$pm10_m5_diff,raw$pm10_m5,6,raw$TOT_AG65)
tb6<-gamm4_excess_func_strata("All-cause","PM10","AG65",raw$pm10_m6_diff,raw$pm10_m6,7,raw$TOT_AG65)
tb7<-gamm4_excess_func_strata("All-cause","PM10","AG65",raw$pm10_m7_diff,raw$pm10_m7,8,raw$TOT_AG65)
tb_tot_pm10_s3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("All-cause","PM10","SEX_F",raw$pm10_m0_diff,raw$pm10   ,1,raw$TOT_SEX_F)
tb1<-gamm4_excess_func_strata("All-cause","PM10","SEX_F",raw$pm10_m1_diff,raw$pm10_m1,2,raw$TOT_SEX_F)
tb2<-gamm4_excess_func_strata("All-cause","PM10","SEX_F",raw$pm10_m2_diff,raw$pm10_m2,3,raw$TOT_SEX_F)
tb3<-gamm4_excess_func_strata("All-cause","PM10","SEX_F",raw$pm10_m3_diff,raw$pm10_m3,4,raw$TOT_SEX_F)
tb4<-gamm4_excess_func_strata("All-cause","PM10","SEX_F",raw$pm10_m4_diff,raw$pm10_m4,5,raw$TOT_SEX_F)
tb5<-gamm4_excess_func_strata("All-cause","PM10","SEX_F",raw$pm10_m5_diff,raw$pm10_m5,6,raw$TOT_SEX_F)
tb6<-gamm4_excess_func_strata("All-cause","PM10","SEX_F",raw$pm10_m6_diff,raw$pm10_m6,7,raw$TOT_SEX_F)
tb7<-gamm4_excess_func_strata("All-cause","PM10","SEX_F",raw$pm10_m7_diff,raw$pm10_m7,8,raw$TOT_SEX_F)
tb_tot_pm10_s4<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("All-cause","PM10","SEX_M",raw$pm10_m0_diff,raw$pm10   ,1,raw$TOT_SEX_M)
tb1<-gamm4_excess_func_strata("All-cause","PM10","SEX_M",raw$pm10_m1_diff,raw$pm10_m1,2,raw$TOT_SEX_M)
tb2<-gamm4_excess_func_strata("All-cause","PM10","SEX_M",raw$pm10_m2_diff,raw$pm10_m2,3,raw$TOT_SEX_M)
tb3<-gamm4_excess_func_strata("All-cause","PM10","SEX_M",raw$pm10_m3_diff,raw$pm10_m3,4,raw$TOT_SEX_M)
tb4<-gamm4_excess_func_strata("All-cause","PM10","SEX_M",raw$pm10_m4_diff,raw$pm10_m4,5,raw$TOT_SEX_M)
tb5<-gamm4_excess_func_strata("All-cause","PM10","SEX_M",raw$pm10_m5_diff,raw$pm10_m5,6,raw$TOT_SEX_M)
tb6<-gamm4_excess_func_strata("All-cause","PM10","SEX_M",raw$pm10_m6_diff,raw$pm10_m6,7,raw$TOT_SEX_M)
tb7<-gamm4_excess_func_strata("All-cause","PM10","SEX_M",raw$pm10_m7_diff,raw$pm10_m7,8,raw$TOT_SEX_M)
tb_tot_pm10_s5<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#전체 사망 & 오존 (by gamm4, linear)
tb0<-gamm4_excess_func_strata("All-cause","O3","AG0014",raw$o3_m0_diff,raw$o3   ,1,raw$TOT_AG0014)
tb1<-gamm4_excess_func_strata("All-cause","O3","AG0014",raw$o3_m1_diff,raw$o3_m1,2,raw$TOT_AG0014)
tb2<-gamm4_excess_func_strata("All-cause","O3","AG0014",raw$o3_m2_diff,raw$o3_m2,3,raw$TOT_AG0014)
tb3<-gamm4_excess_func_strata("All-cause","O3","AG0014",raw$o3_m3_diff,raw$o3_m3,4,raw$TOT_AG0014)
tb4<-gamm4_excess_func_strata("All-cause","O3","AG0014",raw$o3_m4_diff,raw$o3_m4,5,raw$TOT_AG0014)
tb5<-gamm4_excess_func_strata("All-cause","O3","AG0014",raw$o3_m5_diff,raw$o3_m5,6,raw$TOT_AG0014)
tb6<-gamm4_excess_func_strata("All-cause","O3","AG0014",raw$o3_m6_diff,raw$o3_m6,7,raw$TOT_AG0014)
tb7<-gamm4_excess_func_strata("All-cause","O3","AG0014",raw$o3_m7_diff,raw$o3_m7,8,raw$TOT_AG0014)
tb_tot_o3_s1<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("All-cause","O3","AG1564",raw$o3_m0_diff,raw$o3   ,1,raw$TOT_AG1564)
tb1<-gamm4_excess_func_strata("All-cause","O3","AG1564",raw$o3_m1_diff,raw$o3_m1,2,raw$TOT_AG1564)
tb2<-gamm4_excess_func_strata("All-cause","O3","AG1564",raw$o3_m2_diff,raw$o3_m2,3,raw$TOT_AG1564)
tb3<-gamm4_excess_func_strata("All-cause","O3","AG1564",raw$o3_m3_diff,raw$o3_m3,4,raw$TOT_AG1564)
tb4<-gamm4_excess_func_strata("All-cause","O3","AG1564",raw$o3_m4_diff,raw$o3_m4,5,raw$TOT_AG1564)
tb5<-gamm4_excess_func_strata("All-cause","O3","AG1564",raw$o3_m5_diff,raw$o3_m5,6,raw$TOT_AG1564)
tb6<-gamm4_excess_func_strata("All-cause","O3","AG1564",raw$o3_m6_diff,raw$o3_m6,7,raw$TOT_AG1564)
tb7<-gamm4_excess_func_strata("All-cause","O3","AG1564",raw$o3_m7_diff,raw$o3_m7,8,raw$TOT_AG1564)
tb_tot_o3_s2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("All-cause","O3","AG65",raw$o3_m0_diff,raw$o3   ,1,raw$TOT_AG65)
tb1<-gamm4_excess_func_strata("All-cause","O3","AG65",raw$o3_m1_diff,raw$o3_m1,2,raw$TOT_AG65)
tb2<-gamm4_excess_func_strata("All-cause","O3","AG65",raw$o3_m2_diff,raw$o3_m2,3,raw$TOT_AG65)
tb3<-gamm4_excess_func_strata("All-cause","O3","AG65",raw$o3_m3_diff,raw$o3_m3,4,raw$TOT_AG65)
tb4<-gamm4_excess_func_strata("All-cause","O3","AG65",raw$o3_m4_diff,raw$o3_m4,5,raw$TOT_AG65)
tb5<-gamm4_excess_func_strata("All-cause","O3","AG65",raw$o3_m5_diff,raw$o3_m5,6,raw$TOT_AG65)
tb6<-gamm4_excess_func_strata("All-cause","O3","AG65",raw$o3_m6_diff,raw$o3_m6,7,raw$TOT_AG65)
tb7<-gamm4_excess_func_strata("All-cause","O3","AG65",raw$o3_m7_diff,raw$o3_m7,8,raw$TOT_AG65)
tb_tot_o3_s3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("All-cause","O3","SEX_F",raw$o3_m0_diff,raw$o3   ,1,raw$TOT_SEX_F)
tb1<-gamm4_excess_func_strata("All-cause","O3","SEX_F",raw$o3_m1_diff,raw$o3_m1,2,raw$TOT_SEX_F)
tb2<-gamm4_excess_func_strata("All-cause","O3","SEX_F",raw$o3_m2_diff,raw$o3_m2,3,raw$TOT_SEX_F)
tb3<-gamm4_excess_func_strata("All-cause","O3","SEX_F",raw$o3_m3_diff,raw$o3_m3,4,raw$TOT_SEX_F)
tb4<-gamm4_excess_func_strata("All-cause","O3","SEX_F",raw$o3_m4_diff,raw$o3_m4,5,raw$TOT_SEX_F)
tb5<-gamm4_excess_func_strata("All-cause","O3","SEX_F",raw$o3_m5_diff,raw$o3_m5,6,raw$TOT_SEX_F)
tb6<-gamm4_excess_func_strata("All-cause","O3","SEX_F",raw$o3_m6_diff,raw$o3_m6,7,raw$TOT_SEX_F)
tb7<-gamm4_excess_func_strata("All-cause","O3","SEX_F",raw$o3_m7_diff,raw$o3_m7,8,raw$TOT_SEX_F)
tb_tot_o3_s4<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("All-cause","O3","SEX_M",raw$o3_m0_diff,raw$o3   ,1,raw$TOT_SEX_M)
tb1<-gamm4_excess_func_strata("All-cause","O3","SEX_M",raw$o3_m1_diff,raw$o3_m1,2,raw$TOT_SEX_M)
tb2<-gamm4_excess_func_strata("All-cause","O3","SEX_M",raw$o3_m2_diff,raw$o3_m2,3,raw$TOT_SEX_M)
tb3<-gamm4_excess_func_strata("All-cause","O3","SEX_M",raw$o3_m3_diff,raw$o3_m3,4,raw$TOT_SEX_M)
tb4<-gamm4_excess_func_strata("All-cause","O3","SEX_M",raw$o3_m4_diff,raw$o3_m4,5,raw$TOT_SEX_M)
tb5<-gamm4_excess_func_strata("All-cause","O3","SEX_M",raw$o3_m5_diff,raw$o3_m5,6,raw$TOT_SEX_M)
tb6<-gamm4_excess_func_strata("All-cause","O3","SEX_M",raw$o3_m6_diff,raw$o3_m6,7,raw$TOT_SEX_M)
tb7<-gamm4_excess_func_strata("All-cause","O3","SEX_M",raw$o3_m7_diff,raw$o3_m7,8,raw$TOT_SEX_M)
tb_tot_o3_s5<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)


#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
tb_tot_pm25_s1$subgroup="Age group 0-14"
tb_tot_pm25_s2$subgroup="Age group 15-64"
tb_tot_pm25_s3$subgroup="Age group 65+"
tb_tot_pm25_s4$subgroup="Women"
tb_tot_pm25_s5$subgroup="Men"

tb_tot_pm10_s1$subgroup="Age group 0-14"
tb_tot_pm10_s2$subgroup="Age group 15-64"
tb_tot_pm10_s3$subgroup="Age group 65+"
tb_tot_pm10_s4$subgroup="Women"
tb_tot_pm10_s5$subgroup="Men"

tb_tot_o3_s1$subgroup="Age group 0-14"
tb_tot_o3_s2$subgroup="Age group 15-64"
tb_tot_o3_s3$subgroup="Age group 65+"
tb_tot_o3_s4$subgroup="Women"
tb_tot_o3_s5$subgroup="Men"

setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result\\gamm4_linear_strata")
tb_tot_pm25<-rbind(tb_tot_pm25_s1,tb_tot_pm25_s2,tb_tot_pm25_s3,tb_tot_pm25_s4,tb_tot_pm25_s5)
tb_tot_pm10<-rbind(tb_tot_pm10_s1,tb_tot_pm10_s2,tb_tot_pm10_s3,tb_tot_pm10_s4,tb_tot_pm10_s5)
tb_tot_o3  <-rbind(tb_tot_o3_s1  ,tb_tot_o3_s2  ,tb_tot_o3_s3  ,tb_tot_o3_s4  ,tb_tot_o3_s5)

write.csv(tb_tot_pm25,file="tb_tot_pm25.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_tot_pm10,file="tb_tot_pm10.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_tot_o3  ,file="tb_tot_o3.csv"  ,row.names=F,na="",fileEncoding = "euc-kr")

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

tb_summ<-function(data){
  dd<-data
  dd  %>% group_by(year,lag) %>% dplyr:: summarise(e_dth     =sum(e_dth),
                                                   e_dth_lci =sum(e_dth_lci),
                                                   e_dth_uci =sum(e_dth_uci),
                                                   e_dth2    =sum(e_dth2),
                                                   e_dth_lci2=sum(e_dth_lci2),
                                                   e_dth_uci2=sum(e_dth_uci2)) %>% arrange(lag,year)}

tb_tot_pm25_s1.r<-tb_summ(tb_tot_pm25_s1);tb_tot_pm25_s1.r$subgroup="Age group 0-14"
tb_tot_pm25_s2.r<-tb_summ(tb_tot_pm25_s2);tb_tot_pm25_s2.r$subgroup="Age group 15-64"
tb_tot_pm25_s3.r<-tb_summ(tb_tot_pm25_s3);tb_tot_pm25_s3.r$subgroup="Age group 65+"
tb_tot_pm25_s4.r<-tb_summ(tb_tot_pm25_s4);tb_tot_pm25_s4.r$subgroup="Women"
tb_tot_pm25_s5.r<-tb_summ(tb_tot_pm25_s5);tb_tot_pm25_s5.r$subgroup="Men"

tb_tot_pm10_s1.r<-tb_summ(tb_tot_pm10_s1);tb_tot_pm10_s1.r$subgroup="Age group 0-14"
tb_tot_pm10_s2.r<-tb_summ(tb_tot_pm10_s2);tb_tot_pm10_s2.r$subgroup="Age group 15-64"
tb_tot_pm10_s3.r<-tb_summ(tb_tot_pm10_s3);tb_tot_pm10_s3.r$subgroup="Age group 65+"
tb_tot_pm10_s4.r<-tb_summ(tb_tot_pm10_s4);tb_tot_pm10_s4.r$subgroup="Women"
tb_tot_pm10_s5.r<-tb_summ(tb_tot_pm10_s5);tb_tot_pm10_s5.r$subgroup="Men"

tb_tot_o3_s1.r<-tb_summ(tb_tot_o3_s1);tb_tot_o3_s1.r$subgroup="Age group 0-14"
tb_tot_o3_s2.r<-tb_summ(tb_tot_o3_s2);tb_tot_o3_s2.r$subgroup="Age group 15-64"
tb_tot_o3_s3.r<-tb_summ(tb_tot_o3_s3);tb_tot_o3_s3.r$subgroup="Age group 65+"
tb_tot_o3_s4.r<-tb_summ(tb_tot_o3_s4);tb_tot_o3_s4.r$subgroup="Women"
tb_tot_o3_s5.r<-tb_summ(tb_tot_o3_s5);tb_tot_o3_s5.r$subgroup="Men"

tb_tot_pm25.r<-rbind(tb_tot_pm25_s1.r,tb_tot_pm25_s2.r,tb_tot_pm25_s3.r,tb_tot_pm25_s4.r,tb_tot_pm25_s5.r)
tb_tot_pm10.r<-rbind(tb_tot_pm10_s1.r,tb_tot_pm10_s2.r,tb_tot_pm10_s3.r,tb_tot_pm10_s4.r,tb_tot_pm10_s5.r)
tb_tot_o3.r  <-rbind(tb_tot_o3_s1.r  ,tb_tot_o3_s2.r  ,tb_tot_o3_s3.r  ,tb_tot_o3_s4.r  ,tb_tot_o3_s5.r)

write.csv(tb_tot_pm25.r,file="tb_tot_pm25.r.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_tot_pm10.r,file="tb_tot_pm10.r.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_tot_o3.r  ,file="tb_tot_o3.r.csv"  ,row.names=F,na="",fileEncoding = "euc-kr")
#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

#비사고 사망 & 초미세먼지 (by gamm4, linear)

tb0<-gamm4_excess_func_strata("nonacc","PM25","AG0014",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$NON_ACC_AG0014)
tb1<-gamm4_excess_func_strata("nonacc","PM25","AG0014",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$NON_ACC_AG0014)
tb2<-gamm4_excess_func_strata("nonacc","PM25","AG0014",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$NON_ACC_AG0014)
tb3<-gamm4_excess_func_strata("nonacc","PM25","AG0014",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$NON_ACC_AG0014)
tb4<-gamm4_excess_func_strata("nonacc","PM25","AG0014",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$NON_ACC_AG0014)
tb5<-gamm4_excess_func_strata("nonacc","PM25","AG0014",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$NON_ACC_AG0014)
tb6<-gamm4_excess_func_strata("nonacc","PM25","AG0014",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$NON_ACC_AG0014)
tb7<-gamm4_excess_func_strata("nonacc","PM25","AG0014",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$NON_ACC_AG0014)
tb_nonacc_pm25_s1<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("nonacc","PM25","AG1564",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$NON_ACC_AG1564)
tb1<-gamm4_excess_func_strata("nonacc","PM25","AG1564",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$NON_ACC_AG1564)
tb2<-gamm4_excess_func_strata("nonacc","PM25","AG1564",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$NON_ACC_AG1564)
tb3<-gamm4_excess_func_strata("nonacc","PM25","AG1564",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$NON_ACC_AG1564)
tb4<-gamm4_excess_func_strata("nonacc","PM25","AG1564",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$NON_ACC_AG1564)
tb5<-gamm4_excess_func_strata("nonacc","PM25","AG1564",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$NON_ACC_AG1564)
tb6<-gamm4_excess_func_strata("nonacc","PM25","AG1564",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$NON_ACC_AG1564)
tb7<-gamm4_excess_func_strata("nonacc","PM25","AG1564",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$NON_ACC_AG1564)
tb_nonacc_pm25_s2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("nonacc","PM25","AG65",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$NON_ACC_AG65)
tb1<-gamm4_excess_func_strata("nonacc","PM25","AG65",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$NON_ACC_AG65)
tb2<-gamm4_excess_func_strata("nonacc","PM25","AG65",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$NON_ACC_AG65)
tb3<-gamm4_excess_func_strata("nonacc","PM25","AG65",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$NON_ACC_AG65)
tb4<-gamm4_excess_func_strata("nonacc","PM25","AG65",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$NON_ACC_AG65)
tb5<-gamm4_excess_func_strata("nonacc","PM25","AG65",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$NON_ACC_AG65)
tb6<-gamm4_excess_func_strata("nonacc","PM25","AG65",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$NON_ACC_AG65)
tb7<-gamm4_excess_func_strata("nonacc","PM25","AG65",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$NON_ACC_AG65)
tb_nonacc_pm25_s3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("nonacc","PM25","SEX_F",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$NON_ACC_SEX_F)
tb1<-gamm4_excess_func_strata("nonacc","PM25","SEX_F",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$NON_ACC_SEX_F)
tb2<-gamm4_excess_func_strata("nonacc","PM25","SEX_F",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$NON_ACC_SEX_F)
tb3<-gamm4_excess_func_strata("nonacc","PM25","SEX_F",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$NON_ACC_SEX_F)
tb4<-gamm4_excess_func_strata("nonacc","PM25","SEX_F",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$NON_ACC_SEX_F)
tb5<-gamm4_excess_func_strata("nonacc","PM25","SEX_F",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$NON_ACC_SEX_F)
tb6<-gamm4_excess_func_strata("nonacc","PM25","SEX_F",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$NON_ACC_SEX_F)
tb7<-gamm4_excess_func_strata("nonacc","PM25","SEX_F",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$NON_ACC_SEX_F)
tb_nonacc_pm25_s4<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("nonacc","PM25","SEX_M",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$NON_ACC_SEX_M)
tb1<-gamm4_excess_func_strata("nonacc","PM25","SEX_M",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$NON_ACC_SEX_M)
tb2<-gamm4_excess_func_strata("nonacc","PM25","SEX_M",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$NON_ACC_SEX_M)
tb3<-gamm4_excess_func_strata("nonacc","PM25","SEX_M",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$NON_ACC_SEX_M)
tb4<-gamm4_excess_func_strata("nonacc","PM25","SEX_M",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$NON_ACC_SEX_M)
tb5<-gamm4_excess_func_strata("nonacc","PM25","SEX_M",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$NON_ACC_SEX_M)
tb6<-gamm4_excess_func_strata("nonacc","PM25","SEX_M",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$NON_ACC_SEX_M)
tb7<-gamm4_excess_func_strata("nonacc","PM25","SEX_M",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$NON_ACC_SEX_M)
tb_nonacc_pm25_s5<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#비사고 사망 & 미세먼지 (by gamm4, linear)
tb0<-gamm4_excess_func_strata("nonacc","PM10","AG0014",raw$pm10_m0_diff,raw$pm10   ,1,raw$NON_ACC_AG0014)
tb1<-gamm4_excess_func_strata("nonacc","PM10","AG0014",raw$pm10_m1_diff,raw$pm10_m1,2,raw$NON_ACC_AG0014)
tb2<-gamm4_excess_func_strata("nonacc","PM10","AG0014",raw$pm10_m2_diff,raw$pm10_m2,3,raw$NON_ACC_AG0014)
tb3<-gamm4_excess_func_strata("nonacc","PM10","AG0014",raw$pm10_m3_diff,raw$pm10_m3,4,raw$NON_ACC_AG0014)
tb4<-gamm4_excess_func_strata("nonacc","PM10","AG0014",raw$pm10_m4_diff,raw$pm10_m4,5,raw$NON_ACC_AG0014)
tb5<-gamm4_excess_func_strata("nonacc","PM10","AG0014",raw$pm10_m5_diff,raw$pm10_m5,6,raw$NON_ACC_AG0014)
tb6<-gamm4_excess_func_strata("nonacc","PM10","AG0014",raw$pm10_m6_diff,raw$pm10_m6,7,raw$NON_ACC_AG0014)
tb7<-gamm4_excess_func_strata("nonacc","PM10","AG0014",raw$pm10_m7_diff,raw$pm10_m7,8,raw$NON_ACC_AG0014)
tb_nonacc_pm10_s1<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("nonacc","PM10","AG1564",raw$pm10_m0_diff,raw$pm10   ,1,raw$NON_ACC_AG1564)
tb1<-gamm4_excess_func_strata("nonacc","PM10","AG1564",raw$pm10_m1_diff,raw$pm10_m1,2,raw$NON_ACC_AG1564)
tb2<-gamm4_excess_func_strata("nonacc","PM10","AG1564",raw$pm10_m2_diff,raw$pm10_m2,3,raw$NON_ACC_AG1564)
tb3<-gamm4_excess_func_strata("nonacc","PM10","AG1564",raw$pm10_m3_diff,raw$pm10_m3,4,raw$NON_ACC_AG1564)
tb4<-gamm4_excess_func_strata("nonacc","PM10","AG1564",raw$pm10_m4_diff,raw$pm10_m4,5,raw$NON_ACC_AG1564)
tb5<-gamm4_excess_func_strata("nonacc","PM10","AG1564",raw$pm10_m5_diff,raw$pm10_m5,6,raw$NON_ACC_AG1564)
tb6<-gamm4_excess_func_strata("nonacc","PM10","AG1564",raw$pm10_m6_diff,raw$pm10_m6,7,raw$NON_ACC_AG1564)
tb7<-gamm4_excess_func_strata("nonacc","PM10","AG1564",raw$pm10_m7_diff,raw$pm10_m7,8,raw$NON_ACC_AG1564)
tb_nonacc_pm10_s2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("nonacc","PM10","AG65",raw$pm10_m0_diff,raw$pm10   ,1,raw$NON_ACC_AG65)
tb1<-gamm4_excess_func_strata("nonacc","PM10","AG65",raw$pm10_m1_diff,raw$pm10_m1,2,raw$NON_ACC_AG65)
tb2<-gamm4_excess_func_strata("nonacc","PM10","AG65",raw$pm10_m2_diff,raw$pm10_m2,3,raw$NON_ACC_AG65)
tb3<-gamm4_excess_func_strata("nonacc","PM10","AG65",raw$pm10_m3_diff,raw$pm10_m3,4,raw$NON_ACC_AG65)
tb4<-gamm4_excess_func_strata("nonacc","PM10","AG65",raw$pm10_m4_diff,raw$pm10_m4,5,raw$NON_ACC_AG65)
tb5<-gamm4_excess_func_strata("nonacc","PM10","AG65",raw$pm10_m5_diff,raw$pm10_m5,6,raw$NON_ACC_AG65)
tb6<-gamm4_excess_func_strata("nonacc","PM10","AG65",raw$pm10_m6_diff,raw$pm10_m6,7,raw$NON_ACC_AG65)
tb7<-gamm4_excess_func_strata("nonacc","PM10","AG65",raw$pm10_m7_diff,raw$pm10_m7,8,raw$NON_ACC_AG65)
tb_nonacc_pm10_s3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("nonacc","PM10","SEX_F",raw$pm10_m0_diff,raw$pm10   ,1,raw$NON_ACC_SEX_F)
tb1<-gamm4_excess_func_strata("nonacc","PM10","SEX_F",raw$pm10_m1_diff,raw$pm10_m1,2,raw$NON_ACC_SEX_F)
tb2<-gamm4_excess_func_strata("nonacc","PM10","SEX_F",raw$pm10_m2_diff,raw$pm10_m2,3,raw$NON_ACC_SEX_F)
tb3<-gamm4_excess_func_strata("nonacc","PM10","SEX_F",raw$pm10_m3_diff,raw$pm10_m3,4,raw$NON_ACC_SEX_F)
tb4<-gamm4_excess_func_strata("nonacc","PM10","SEX_F",raw$pm10_m4_diff,raw$pm10_m4,5,raw$NON_ACC_SEX_F)
tb5<-gamm4_excess_func_strata("nonacc","PM10","SEX_F",raw$pm10_m5_diff,raw$pm10_m5,6,raw$NON_ACC_SEX_F)
tb6<-gamm4_excess_func_strata("nonacc","PM10","SEX_F",raw$pm10_m6_diff,raw$pm10_m6,7,raw$NON_ACC_SEX_F)
tb7<-gamm4_excess_func_strata("nonacc","PM10","SEX_F",raw$pm10_m7_diff,raw$pm10_m7,8,raw$NON_ACC_SEX_F)
tb_nonacc_pm10_s4<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("nonacc","PM10","SEX_M",raw$pm10_m0_diff,raw$pm10   ,1,raw$NON_ACC_SEX_M)
tb1<-gamm4_excess_func_strata("nonacc","PM10","SEX_M",raw$pm10_m1_diff,raw$pm10_m1,2,raw$NON_ACC_SEX_M)
tb2<-gamm4_excess_func_strata("nonacc","PM10","SEX_M",raw$pm10_m2_diff,raw$pm10_m2,3,raw$NON_ACC_SEX_M)
tb3<-gamm4_excess_func_strata("nonacc","PM10","SEX_M",raw$pm10_m3_diff,raw$pm10_m3,4,raw$NON_ACC_SEX_M)
tb4<-gamm4_excess_func_strata("nonacc","PM10","SEX_M",raw$pm10_m4_diff,raw$pm10_m4,5,raw$NON_ACC_SEX_M)
tb5<-gamm4_excess_func_strata("nonacc","PM10","SEX_M",raw$pm10_m5_diff,raw$pm10_m5,6,raw$NON_ACC_SEX_M)
tb6<-gamm4_excess_func_strata("nonacc","PM10","SEX_M",raw$pm10_m6_diff,raw$pm10_m6,7,raw$NON_ACC_SEX_M)
tb7<-gamm4_excess_func_strata("nonacc","PM10","SEX_M",raw$pm10_m7_diff,raw$pm10_m7,8,raw$NON_ACC_SEX_M)
tb_nonacc_pm10_s5<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#비사고 사망 & 오존 (by gamm4, linear)
tb0<-gamm4_excess_func_strata("nonacc","O3","AG0014",raw$o3_m0_diff,raw$o3   ,1,raw$NON_ACC_AG0014)
tb1<-gamm4_excess_func_strata("nonacc","O3","AG0014",raw$o3_m1_diff,raw$o3_m1,2,raw$NON_ACC_AG0014)
tb2<-gamm4_excess_func_strata("nonacc","O3","AG0014",raw$o3_m2_diff,raw$o3_m2,3,raw$NON_ACC_AG0014)
tb3<-gamm4_excess_func_strata("nonacc","O3","AG0014",raw$o3_m3_diff,raw$o3_m3,4,raw$NON_ACC_AG0014)
tb4<-gamm4_excess_func_strata("nonacc","O3","AG0014",raw$o3_m4_diff,raw$o3_m4,5,raw$NON_ACC_AG0014)
tb5<-gamm4_excess_func_strata("nonacc","O3","AG0014",raw$o3_m5_diff,raw$o3_m5,6,raw$NON_ACC_AG0014)
tb6<-gamm4_excess_func_strata("nonacc","O3","AG0014",raw$o3_m6_diff,raw$o3_m6,7,raw$NON_ACC_AG0014)
tb7<-gamm4_excess_func_strata("nonacc","O3","AG0014",raw$o3_m7_diff,raw$o3_m7,8,raw$NON_ACC_AG0014)
tb_nonacc_o3_s1<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("nonacc","O3","AG1564",raw$o3_m0_diff,raw$o3   ,1,raw$NON_ACC_AG1564)
tb1<-gamm4_excess_func_strata("nonacc","O3","AG1564",raw$o3_m1_diff,raw$o3_m1,2,raw$NON_ACC_AG1564)
tb2<-gamm4_excess_func_strata("nonacc","O3","AG1564",raw$o3_m2_diff,raw$o3_m2,3,raw$NON_ACC_AG1564)
tb3<-gamm4_excess_func_strata("nonacc","O3","AG1564",raw$o3_m3_diff,raw$o3_m3,4,raw$NON_ACC_AG1564)
tb4<-gamm4_excess_func_strata("nonacc","O3","AG1564",raw$o3_m4_diff,raw$o3_m4,5,raw$NON_ACC_AG1564)
tb5<-gamm4_excess_func_strata("nonacc","O3","AG1564",raw$o3_m5_diff,raw$o3_m5,6,raw$NON_ACC_AG1564)
tb6<-gamm4_excess_func_strata("nonacc","O3","AG1564",raw$o3_m6_diff,raw$o3_m6,7,raw$NON_ACC_AG1564)
tb7<-gamm4_excess_func_strata("nonacc","O3","AG1564",raw$o3_m7_diff,raw$o3_m7,8,raw$NON_ACC_AG1564)
tb_nonacc_o3_s2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("nonacc","O3","AG65",raw$o3_m0_diff,raw$o3   ,1,raw$NON_ACC_AG65)
tb1<-gamm4_excess_func_strata("nonacc","O3","AG65",raw$o3_m1_diff,raw$o3_m1,2,raw$NON_ACC_AG65)
tb2<-gamm4_excess_func_strata("nonacc","O3","AG65",raw$o3_m2_diff,raw$o3_m2,3,raw$NON_ACC_AG65)
tb3<-gamm4_excess_func_strata("nonacc","O3","AG65",raw$o3_m3_diff,raw$o3_m3,4,raw$NON_ACC_AG65)
tb4<-gamm4_excess_func_strata("nonacc","O3","AG65",raw$o3_m4_diff,raw$o3_m4,5,raw$NON_ACC_AG65)
tb5<-gamm4_excess_func_strata("nonacc","O3","AG65",raw$o3_m5_diff,raw$o3_m5,6,raw$NON_ACC_AG65)
tb6<-gamm4_excess_func_strata("nonacc","O3","AG65",raw$o3_m6_diff,raw$o3_m6,7,raw$NON_ACC_AG65)
tb7<-gamm4_excess_func_strata("nonacc","O3","AG65",raw$o3_m7_diff,raw$o3_m7,8,raw$NON_ACC_AG65)
tb_nonacc_o3_s3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("nonacc","O3","SEX_F",raw$o3_m0_diff,raw$o3   ,1,raw$NON_ACC_SEX_F)
tb1<-gamm4_excess_func_strata("nonacc","O3","SEX_F",raw$o3_m1_diff,raw$o3_m1,2,raw$NON_ACC_SEX_F)
tb2<-gamm4_excess_func_strata("nonacc","O3","SEX_F",raw$o3_m2_diff,raw$o3_m2,3,raw$NON_ACC_SEX_F)
tb3<-gamm4_excess_func_strata("nonacc","O3","SEX_F",raw$o3_m3_diff,raw$o3_m3,4,raw$NON_ACC_SEX_F)
tb4<-gamm4_excess_func_strata("nonacc","O3","SEX_F",raw$o3_m4_diff,raw$o3_m4,5,raw$NON_ACC_SEX_F)
tb5<-gamm4_excess_func_strata("nonacc","O3","SEX_F",raw$o3_m5_diff,raw$o3_m5,6,raw$NON_ACC_SEX_F)
tb6<-gamm4_excess_func_strata("nonacc","O3","SEX_F",raw$o3_m6_diff,raw$o3_m6,7,raw$NON_ACC_SEX_F)
tb7<-gamm4_excess_func_strata("nonacc","O3","SEX_F",raw$o3_m7_diff,raw$o3_m7,8,raw$NON_ACC_SEX_F)
tb_nonacc_o3_s4<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("nonacc","O3","SEX_M",raw$o3_m0_diff,raw$o3   ,1,raw$NON_ACC_SEX_M)
tb1<-gamm4_excess_func_strata("nonacc","O3","SEX_M",raw$o3_m1_diff,raw$o3_m1,2,raw$NON_ACC_SEX_M)
tb2<-gamm4_excess_func_strata("nonacc","O3","SEX_M",raw$o3_m2_diff,raw$o3_m2,3,raw$NON_ACC_SEX_M)
tb3<-gamm4_excess_func_strata("nonacc","O3","SEX_M",raw$o3_m3_diff,raw$o3_m3,4,raw$NON_ACC_SEX_M)
tb4<-gamm4_excess_func_strata("nonacc","O3","SEX_M",raw$o3_m4_diff,raw$o3_m4,5,raw$NON_ACC_SEX_M)
tb5<-gamm4_excess_func_strata("nonacc","O3","SEX_M",raw$o3_m5_diff,raw$o3_m5,6,raw$NON_ACC_SEX_M)
tb6<-gamm4_excess_func_strata("nonacc","O3","SEX_M",raw$o3_m6_diff,raw$o3_m6,7,raw$NON_ACC_SEX_M)
tb7<-gamm4_excess_func_strata("nonacc","O3","SEX_M",raw$o3_m7_diff,raw$o3_m7,8,raw$NON_ACC_SEX_M)
tb_nonacc_o3_s5<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)


#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
tb_nonacc_pm25_s1$subgroup="Age group 0-14"
tb_nonacc_pm25_s2$subgroup="Age group 15-64"
tb_nonacc_pm25_s3$subgroup="Age group 65+"
tb_nonacc_pm25_s4$subgroup="Women"
tb_nonacc_pm25_s5$subgroup="Men"

tb_nonacc_pm10_s1$subgroup="Age group 0-14"
tb_nonacc_pm10_s2$subgroup="Age group 15-64"
tb_nonacc_pm10_s3$subgroup="Age group 65+"
tb_nonacc_pm10_s4$subgroup="Women"
tb_nonacc_pm10_s5$subgroup="Men"

tb_nonacc_o3_s1$subgroup="Age group 0-14"
tb_nonacc_o3_s2$subgroup="Age group 15-64"
tb_nonacc_o3_s3$subgroup="Age group 65+"
tb_nonacc_o3_s4$subgroup="Women"
tb_nonacc_o3_s5$subgroup="Men"

tb_nonacc_pm25<-rbind(tb_nonacc_pm25_s1,tb_nonacc_pm25_s2,tb_nonacc_pm25_s3,tb_nonacc_pm25_s4,tb_nonacc_pm25_s5)
tb_nonacc_pm10<-rbind(tb_nonacc_pm10_s1,tb_nonacc_pm10_s2,tb_nonacc_pm10_s3,tb_nonacc_pm10_s4,tb_nonacc_pm10_s5)
tb_nonacc_o3  <-rbind(tb_nonacc_o3_s1  ,tb_nonacc_o3_s2  ,tb_nonacc_o3_s3  ,tb_nonacc_o3_s4  ,tb_nonacc_o3_s5)

setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result\\gamm4_linear_strata")
write.csv(tb_nonacc_pm25,file="tb_nonacc_pm25.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_nonacc_pm10,file="tb_nonacc_pm10.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_nonacc_o3  ,file="tb_nonacc_o3.csv"  ,row.names=F,na="",fileEncoding = "euc-kr")

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

tb_nonacc_pm25_s1.r<-tb_summ(tb_nonacc_pm25_s1);tb_nonacc_pm25_s1.r$subgroup="Age group 0-14"
tb_nonacc_pm25_s2.r<-tb_summ(tb_nonacc_pm25_s2);tb_nonacc_pm25_s2.r$subgroup="Age group 15-64"
tb_nonacc_pm25_s3.r<-tb_summ(tb_nonacc_pm25_s3);tb_nonacc_pm25_s3.r$subgroup="Age group 65+"
tb_nonacc_pm25_s4.r<-tb_summ(tb_nonacc_pm25_s4);tb_nonacc_pm25_s4.r$subgroup="Women"
tb_nonacc_pm25_s5.r<-tb_summ(tb_nonacc_pm25_s5);tb_nonacc_pm25_s5.r$subgroup="Men"

tb_nonacc_pm10_s1.r<-tb_summ(tb_nonacc_pm10_s1);tb_nonacc_pm10_s1.r$subgroup="Age group 0-14"
tb_nonacc_pm10_s2.r<-tb_summ(tb_nonacc_pm10_s2);tb_nonacc_pm10_s2.r$subgroup="Age group 15-64"
tb_nonacc_pm10_s3.r<-tb_summ(tb_nonacc_pm10_s3);tb_nonacc_pm10_s3.r$subgroup="Age group 65+"
tb_nonacc_pm10_s4.r<-tb_summ(tb_nonacc_pm10_s4);tb_nonacc_pm10_s4.r$subgroup="Women"
tb_nonacc_pm10_s5.r<-tb_summ(tb_nonacc_pm10_s5);tb_nonacc_pm10_s5.r$subgroup="Men"

tb_nonacc_o3_s1.r<-tb_summ(tb_nonacc_o3_s1);tb_nonacc_o3_s1.r$subgroup="Age group 0-14"
tb_nonacc_o3_s2.r<-tb_summ(tb_nonacc_o3_s2);tb_nonacc_o3_s2.r$subgroup="Age group 15-64"
tb_nonacc_o3_s3.r<-tb_summ(tb_nonacc_o3_s3);tb_nonacc_o3_s3.r$subgroup="Age group 65+"
tb_nonacc_o3_s4.r<-tb_summ(tb_nonacc_o3_s4);tb_nonacc_o3_s4.r$subgroup="Women"
tb_nonacc_o3_s5.r<-tb_summ(tb_nonacc_o3_s5);tb_nonacc_o3_s5.r$subgroup="Men"

tb_nonacc_pm25.r<-rbind(tb_nonacc_pm25_s1.r,tb_nonacc_pm25_s2.r,tb_nonacc_pm25_s3.r,tb_nonacc_pm25_s4.r,tb_nonacc_pm25_s5.r)
tb_nonacc_pm10.r<-rbind(tb_nonacc_pm10_s1.r,tb_nonacc_pm10_s2.r,tb_nonacc_pm10_s3.r,tb_nonacc_pm10_s4.r,tb_nonacc_pm10_s5.r)
tb_nonacc_o3.r  <-rbind(tb_nonacc_o3_s1.r  ,tb_nonacc_o3_s2.r  ,tb_nonacc_o3_s3.r  ,tb_nonacc_o3_s4.r  ,tb_nonacc_o3_s5.r)

write.csv(tb_nonacc_pm25.r,file="tb_nonacc_pm25.r.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_nonacc_pm10.r,file="tb_nonacc_pm10.r.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_nonacc_o3.r  ,file="tb_nonacc_o3.r.csv"  ,row.names=F,na="",fileEncoding = "euc-kr")

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#심혈관 사망 & 초미세먼지 (by gamm4, linear)

tb0<-gamm4_excess_func_strata("CVD","PM25","AG0014",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$CVD_AG0014)
tb1<-gamm4_excess_func_strata("CVD","PM25","AG0014",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$CVD_AG0014)
tb2<-gamm4_excess_func_strata("CVD","PM25","AG0014",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$CVD_AG0014)
tb3<-gamm4_excess_func_strata("CVD","PM25","AG0014",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$CVD_AG0014)
tb4<-gamm4_excess_func_strata("CVD","PM25","AG0014",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$CVD_AG0014)
tb5<-gamm4_excess_func_strata("CVD","PM25","AG0014",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$CVD_AG0014)
tb6<-gamm4_excess_func_strata("CVD","PM25","AG0014",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$CVD_AG0014)
tb7<-gamm4_excess_func_strata("CVD","PM25","AG0014",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$CVD_AG0014)
tb_cvd_pm25_s1<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("CVD","PM25","AG1564",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$CVD_AG1564)
tb1<-gamm4_excess_func_strata("CVD","PM25","AG1564",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$CVD_AG1564)
tb2<-gamm4_excess_func_strata("CVD","PM25","AG1564",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$CVD_AG1564)
tb3<-gamm4_excess_func_strata("CVD","PM25","AG1564",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$CVD_AG1564)
tb4<-gamm4_excess_func_strata("CVD","PM25","AG1564",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$CVD_AG1564)
tb5<-gamm4_excess_func_strata("CVD","PM25","AG1564",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$CVD_AG1564)
tb6<-gamm4_excess_func_strata("CVD","PM25","AG1564",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$CVD_AG1564)
tb7<-gamm4_excess_func_strata("CVD","PM25","AG1564",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$CVD_AG1564)
tb_cvd_pm25_s2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("CVD","PM25","AG65",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$CVD_AG65)
tb1<-gamm4_excess_func_strata("CVD","PM25","AG65",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$CVD_AG65)
tb2<-gamm4_excess_func_strata("CVD","PM25","AG65",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$CVD_AG65)
tb3<-gamm4_excess_func_strata("CVD","PM25","AG65",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$CVD_AG65)
tb4<-gamm4_excess_func_strata("CVD","PM25","AG65",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$CVD_AG65)
tb5<-gamm4_excess_func_strata("CVD","PM25","AG65",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$CVD_AG65)
tb6<-gamm4_excess_func_strata("CVD","PM25","AG65",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$CVD_AG65)
tb7<-gamm4_excess_func_strata("CVD","PM25","AG65",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$CVD_AG65)
tb_cvd_pm25_s3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("CVD","PM25","SEX_F",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$CVD_SEX_F)
tb1<-gamm4_excess_func_strata("CVD","PM25","SEX_F",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$CVD_SEX_F)
tb2<-gamm4_excess_func_strata("CVD","PM25","SEX_F",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$CVD_SEX_F)
tb3<-gamm4_excess_func_strata("CVD","PM25","SEX_F",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$CVD_SEX_F)
tb4<-gamm4_excess_func_strata("CVD","PM25","SEX_F",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$CVD_SEX_F)
tb5<-gamm4_excess_func_strata("CVD","PM25","SEX_F",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$CVD_SEX_F)
tb6<-gamm4_excess_func_strata("CVD","PM25","SEX_F",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$CVD_SEX_F)
tb7<-gamm4_excess_func_strata("CVD","PM25","SEX_F",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$CVD_SEX_F)
tb_cvd_pm25_s4<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("CVD","PM25","SEX_M",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$CVD_SEX_M)
tb1<-gamm4_excess_func_strata("CVD","PM25","SEX_M",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$CVD_SEX_M)
tb2<-gamm4_excess_func_strata("CVD","PM25","SEX_M",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$CVD_SEX_M)
tb3<-gamm4_excess_func_strata("CVD","PM25","SEX_M",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$CVD_SEX_M)
tb4<-gamm4_excess_func_strata("CVD","PM25","SEX_M",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$CVD_SEX_M)
tb5<-gamm4_excess_func_strata("CVD","PM25","SEX_M",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$CVD_SEX_M)
tb6<-gamm4_excess_func_strata("CVD","PM25","SEX_M",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$CVD_SEX_M)
tb7<-gamm4_excess_func_strata("CVD","PM25","SEX_M",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$CVD_SEX_M)
tb_cvd_pm25_s5<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#심혈관 사망 & 미세먼지 (by gamm4, linear)
tb0<-gamm4_excess_func_strata("CVD","PM10","AG0014",raw$pm10_m0_diff,raw$pm10   ,1,raw$CVD_AG0014)
tb1<-gamm4_excess_func_strata("CVD","PM10","AG0014",raw$pm10_m1_diff,raw$pm10_m1,2,raw$CVD_AG0014)
tb2<-gamm4_excess_func_strata("CVD","PM10","AG0014",raw$pm10_m2_diff,raw$pm10_m2,3,raw$CVD_AG0014)
tb3<-gamm4_excess_func_strata("CVD","PM10","AG0014",raw$pm10_m3_diff,raw$pm10_m3,4,raw$CVD_AG0014)
tb4<-gamm4_excess_func_strata("CVD","PM10","AG0014",raw$pm10_m4_diff,raw$pm10_m4,5,raw$CVD_AG0014)
tb5<-gamm4_excess_func_strata("CVD","PM10","AG0014",raw$pm10_m5_diff,raw$pm10_m5,6,raw$CVD_AG0014)
tb6<-gamm4_excess_func_strata("CVD","PM10","AG0014",raw$pm10_m6_diff,raw$pm10_m6,7,raw$CVD_AG0014)
tb7<-gamm4_excess_func_strata("CVD","PM10","AG0014",raw$pm10_m7_diff,raw$pm10_m7,8,raw$CVD_AG0014)
tb_cvd_pm10_s1<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("CVD","PM10","AG1564",raw$pm10_m0_diff,raw$pm10   ,1,raw$CVD_AG1564)
tb1<-gamm4_excess_func_strata("CVD","PM10","AG1564",raw$pm10_m1_diff,raw$pm10_m1,2,raw$CVD_AG1564)
tb2<-gamm4_excess_func_strata("CVD","PM10","AG1564",raw$pm10_m2_diff,raw$pm10_m2,3,raw$CVD_AG1564)
tb3<-gamm4_excess_func_strata("CVD","PM10","AG1564",raw$pm10_m3_diff,raw$pm10_m3,4,raw$CVD_AG1564)
tb4<-gamm4_excess_func_strata("CVD","PM10","AG1564",raw$pm10_m4_diff,raw$pm10_m4,5,raw$CVD_AG1564)
tb5<-gamm4_excess_func_strata("CVD","PM10","AG1564",raw$pm10_m5_diff,raw$pm10_m5,6,raw$CVD_AG1564)
tb6<-gamm4_excess_func_strata("CVD","PM10","AG1564",raw$pm10_m6_diff,raw$pm10_m6,7,raw$CVD_AG1564)
tb7<-gamm4_excess_func_strata("CVD","PM10","AG1564",raw$pm10_m7_diff,raw$pm10_m7,8,raw$CVD_AG1564)
tb_cvd_pm10_s2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("CVD","PM10","AG65",raw$pm10_m0_diff,raw$pm10   ,1,raw$CVD_AG65)
tb1<-gamm4_excess_func_strata("CVD","PM10","AG65",raw$pm10_m1_diff,raw$pm10_m1,2,raw$CVD_AG65)
tb2<-gamm4_excess_func_strata("CVD","PM10","AG65",raw$pm10_m2_diff,raw$pm10_m2,3,raw$CVD_AG65)
tb3<-gamm4_excess_func_strata("CVD","PM10","AG65",raw$pm10_m3_diff,raw$pm10_m3,4,raw$CVD_AG65)
tb4<-gamm4_excess_func_strata("CVD","PM10","AG65",raw$pm10_m4_diff,raw$pm10_m4,5,raw$CVD_AG65)
tb5<-gamm4_excess_func_strata("CVD","PM10","AG65",raw$pm10_m5_diff,raw$pm10_m5,6,raw$CVD_AG65)
tb6<-gamm4_excess_func_strata("CVD","PM10","AG65",raw$pm10_m6_diff,raw$pm10_m6,7,raw$CVD_AG65)
tb7<-gamm4_excess_func_strata("CVD","PM10","AG65",raw$pm10_m7_diff,raw$pm10_m7,8,raw$CVD_AG65)
tb_cvd_pm10_s3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("CVD","PM10","SEX_F",raw$pm10_m0_diff,raw$pm10   ,1,raw$CVD_SEX_F)
tb1<-gamm4_excess_func_strata("CVD","PM10","SEX_F",raw$pm10_m1_diff,raw$pm10_m1,2,raw$CVD_SEX_F)
tb2<-gamm4_excess_func_strata("CVD","PM10","SEX_F",raw$pm10_m2_diff,raw$pm10_m2,3,raw$CVD_SEX_F)
tb3<-gamm4_excess_func_strata("CVD","PM10","SEX_F",raw$pm10_m3_diff,raw$pm10_m3,4,raw$CVD_SEX_F)
tb4<-gamm4_excess_func_strata("CVD","PM10","SEX_F",raw$pm10_m4_diff,raw$pm10_m4,5,raw$CVD_SEX_F)
tb5<-gamm4_excess_func_strata("CVD","PM10","SEX_F",raw$pm10_m5_diff,raw$pm10_m5,6,raw$CVD_SEX_F)
tb6<-gamm4_excess_func_strata("CVD","PM10","SEX_F",raw$pm10_m6_diff,raw$pm10_m6,7,raw$CVD_SEX_F)
tb7<-gamm4_excess_func_strata("CVD","PM10","SEX_F",raw$pm10_m7_diff,raw$pm10_m7,8,raw$CVD_SEX_F)
tb_cvd_pm10_s4<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("CVD","PM10","SEX_M",raw$pm10_m0_diff,raw$pm10   ,1,raw$CVD_SEX_M)
tb1<-gamm4_excess_func_strata("CVD","PM10","SEX_M",raw$pm10_m1_diff,raw$pm10_m1,2,raw$CVD_SEX_M)
tb2<-gamm4_excess_func_strata("CVD","PM10","SEX_M",raw$pm10_m2_diff,raw$pm10_m2,3,raw$CVD_SEX_M)
tb3<-gamm4_excess_func_strata("CVD","PM10","SEX_M",raw$pm10_m3_diff,raw$pm10_m3,4,raw$CVD_SEX_M)
tb4<-gamm4_excess_func_strata("CVD","PM10","SEX_M",raw$pm10_m4_diff,raw$pm10_m4,5,raw$CVD_SEX_M)
tb5<-gamm4_excess_func_strata("CVD","PM10","SEX_M",raw$pm10_m5_diff,raw$pm10_m5,6,raw$CVD_SEX_M)
tb6<-gamm4_excess_func_strata("CVD","PM10","SEX_M",raw$pm10_m6_diff,raw$pm10_m6,7,raw$CVD_SEX_M)
tb7<-gamm4_excess_func_strata("CVD","PM10","SEX_M",raw$pm10_m7_diff,raw$pm10_m7,8,raw$CVD_SEX_M)
tb_cvd_pm10_s5<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#심혈관 사망 & 오존 (by gamm4, linear)
tb0<-gamm4_excess_func_strata("CVD","O3","AG0014",raw$o3_m0_diff,raw$o3   ,1,raw$CVD_AG0014)
tb1<-gamm4_excess_func_strata("CVD","O3","AG0014",raw$o3_m1_diff,raw$o3_m1,2,raw$CVD_AG0014)
tb2<-gamm4_excess_func_strata("CVD","O3","AG0014",raw$o3_m2_diff,raw$o3_m2,3,raw$CVD_AG0014)
tb3<-gamm4_excess_func_strata("CVD","O3","AG0014",raw$o3_m3_diff,raw$o3_m3,4,raw$CVD_AG0014)
tb4<-gamm4_excess_func_strata("CVD","O3","AG0014",raw$o3_m4_diff,raw$o3_m4,5,raw$CVD_AG0014)
tb5<-gamm4_excess_func_strata("CVD","O3","AG0014",raw$o3_m5_diff,raw$o3_m5,6,raw$CVD_AG0014)
tb6<-gamm4_excess_func_strata("CVD","O3","AG0014",raw$o3_m6_diff,raw$o3_m6,7,raw$CVD_AG0014)
tb7<-gamm4_excess_func_strata("CVD","O3","AG0014",raw$o3_m7_diff,raw$o3_m7,8,raw$CVD_AG0014)
tb_cvd_o3_s1<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("CVD","O3","AG1564",raw$o3_m0_diff,raw$o3   ,1,raw$CVD_AG1564)
tb1<-gamm4_excess_func_strata("CVD","O3","AG1564",raw$o3_m1_diff,raw$o3_m1,2,raw$CVD_AG1564)
tb2<-gamm4_excess_func_strata("CVD","O3","AG1564",raw$o3_m2_diff,raw$o3_m2,3,raw$CVD_AG1564)
tb3<-gamm4_excess_func_strata("CVD","O3","AG1564",raw$o3_m3_diff,raw$o3_m3,4,raw$CVD_AG1564)
tb4<-gamm4_excess_func_strata("CVD","O3","AG1564",raw$o3_m4_diff,raw$o3_m4,5,raw$CVD_AG1564)
tb5<-gamm4_excess_func_strata("CVD","O3","AG1564",raw$o3_m5_diff,raw$o3_m5,6,raw$CVD_AG1564)
tb6<-gamm4_excess_func_strata("CVD","O3","AG1564",raw$o3_m6_diff,raw$o3_m6,7,raw$CVD_AG1564)
tb7<-gamm4_excess_func_strata("CVD","O3","AG1564",raw$o3_m7_diff,raw$o3_m7,8,raw$CVD_AG1564)
tb_cvd_o3_s2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("CVD","O3","AG65",raw$o3_m0_diff,raw$o3   ,1,raw$CVD_AG65)
tb1<-gamm4_excess_func_strata("CVD","O3","AG65",raw$o3_m1_diff,raw$o3_m1,2,raw$CVD_AG65)
tb2<-gamm4_excess_func_strata("CVD","O3","AG65",raw$o3_m2_diff,raw$o3_m2,3,raw$CVD_AG65)
tb3<-gamm4_excess_func_strata("CVD","O3","AG65",raw$o3_m3_diff,raw$o3_m3,4,raw$CVD_AG65)
tb4<-gamm4_excess_func_strata("CVD","O3","AG65",raw$o3_m4_diff,raw$o3_m4,5,raw$CVD_AG65)
tb5<-gamm4_excess_func_strata("CVD","O3","AG65",raw$o3_m5_diff,raw$o3_m5,6,raw$CVD_AG65)
tb6<-gamm4_excess_func_strata("CVD","O3","AG65",raw$o3_m6_diff,raw$o3_m6,7,raw$CVD_AG65)
tb7<-gamm4_excess_func_strata("CVD","O3","AG65",raw$o3_m7_diff,raw$o3_m7,8,raw$CVD_AG65)
tb_cvd_o3_s3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("CVD","O3","SEX_F",raw$o3_m0_diff,raw$o3   ,1,raw$CVD_SEX_F)
tb1<-gamm4_excess_func_strata("CVD","O3","SEX_F",raw$o3_m1_diff,raw$o3_m1,2,raw$CVD_SEX_F)
tb2<-gamm4_excess_func_strata("CVD","O3","SEX_F",raw$o3_m2_diff,raw$o3_m2,3,raw$CVD_SEX_F)
tb3<-gamm4_excess_func_strata("CVD","O3","SEX_F",raw$o3_m3_diff,raw$o3_m3,4,raw$CVD_SEX_F)
tb4<-gamm4_excess_func_strata("CVD","O3","SEX_F",raw$o3_m4_diff,raw$o3_m4,5,raw$CVD_SEX_F)
tb5<-gamm4_excess_func_strata("CVD","O3","SEX_F",raw$o3_m5_diff,raw$o3_m5,6,raw$CVD_SEX_F)
tb6<-gamm4_excess_func_strata("CVD","O3","SEX_F",raw$o3_m6_diff,raw$o3_m6,7,raw$CVD_SEX_F)
tb7<-gamm4_excess_func_strata("CVD","O3","SEX_F",raw$o3_m7_diff,raw$o3_m7,8,raw$CVD_SEX_F)
tb_cvd_o3_s4<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("CVD","O3","SEX_M",raw$o3_m0_diff,raw$o3   ,1,raw$CVD_SEX_M)
tb1<-gamm4_excess_func_strata("CVD","O3","SEX_M",raw$o3_m1_diff,raw$o3_m1,2,raw$CVD_SEX_M)
tb2<-gamm4_excess_func_strata("CVD","O3","SEX_M",raw$o3_m2_diff,raw$o3_m2,3,raw$CVD_SEX_M)
tb3<-gamm4_excess_func_strata("CVD","O3","SEX_M",raw$o3_m3_diff,raw$o3_m3,4,raw$CVD_SEX_M)
tb4<-gamm4_excess_func_strata("CVD","O3","SEX_M",raw$o3_m4_diff,raw$o3_m4,5,raw$CVD_SEX_M)
tb5<-gamm4_excess_func_strata("CVD","O3","SEX_M",raw$o3_m5_diff,raw$o3_m5,6,raw$CVD_SEX_M)
tb6<-gamm4_excess_func_strata("CVD","O3","SEX_M",raw$o3_m6_diff,raw$o3_m6,7,raw$CVD_SEX_M)
tb7<-gamm4_excess_func_strata("CVD","O3","SEX_M",raw$o3_m7_diff,raw$o3_m7,8,raw$CVD_SEX_M)
tb_cvd_o3_s5<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)


#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
tb_cvd_pm25_s1$subgroup="Age group 0-14"
tb_cvd_pm25_s2$subgroup="Age group 15-64"
tb_cvd_pm25_s3$subgroup="Age group 65+"
tb_cvd_pm25_s4$subgroup="Women"
tb_cvd_pm25_s5$subgroup="Men"

tb_cvd_pm10_s1$subgroup="Age group 0-14"
tb_cvd_pm10_s2$subgroup="Age group 15-64"
tb_cvd_pm10_s3$subgroup="Age group 65+"
tb_cvd_pm10_s4$subgroup="Women"
tb_cvd_pm10_s5$subgroup="Men"

tb_cvd_o3_s1$subgroup="Age group 0-14"
tb_cvd_o3_s2$subgroup="Age group 15-64"
tb_cvd_o3_s3$subgroup="Age group 65+"
tb_cvd_o3_s4$subgroup="Women"
tb_cvd_o3_s5$subgroup="Men"

tb_cvd_pm25<-rbind(tb_cvd_pm25_s1,tb_cvd_pm25_s2,tb_cvd_pm25_s3,tb_cvd_pm25_s4,tb_cvd_pm25_s5)
tb_cvd_pm10<-rbind(tb_cvd_pm10_s1,tb_cvd_pm10_s2,tb_cvd_pm10_s3,tb_cvd_pm10_s4,tb_cvd_pm10_s5)
tb_cvd_o3  <-rbind(tb_cvd_o3_s1  ,tb_cvd_o3_s2  ,tb_cvd_o3_s3  ,tb_cvd_o3_s4  ,tb_cvd_o3_s5)

setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result\\gamm4_linear_strata")
write.csv(tb_cvd_pm25,file="tb_cvd_pm25.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_cvd_pm10,file="tb_cvd_pm10.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_cvd_o3  ,file="tb_cvd_o3.csv"  ,row.names=F,na="",fileEncoding = "euc-kr")

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

tb_cvd_pm25_s1.r<-tb_summ(tb_cvd_pm25_s1);tb_cvd_pm25_s1.r$subgroup="Age group 0-14"
tb_cvd_pm25_s2.r<-tb_summ(tb_cvd_pm25_s2);tb_cvd_pm25_s2.r$subgroup="Age group 15-64"
tb_cvd_pm25_s3.r<-tb_summ(tb_cvd_pm25_s3);tb_cvd_pm25_s3.r$subgroup="Age group 65+"
tb_cvd_pm25_s4.r<-tb_summ(tb_cvd_pm25_s4);tb_cvd_pm25_s4.r$subgroup="Women"
tb_cvd_pm25_s5.r<-tb_summ(tb_cvd_pm25_s5);tb_cvd_pm25_s5.r$subgroup="Men"

tb_cvd_pm10_s1.r<-tb_summ(tb_cvd_pm10_s1);tb_cvd_pm10_s1.r$subgroup="Age group 0-14"
tb_cvd_pm10_s2.r<-tb_summ(tb_cvd_pm10_s2);tb_cvd_pm10_s2.r$subgroup="Age group 15-64"
tb_cvd_pm10_s3.r<-tb_summ(tb_cvd_pm10_s3);tb_cvd_pm10_s3.r$subgroup="Age group 65+"
tb_cvd_pm10_s4.r<-tb_summ(tb_cvd_pm10_s4);tb_cvd_pm10_s4.r$subgroup="Women"
tb_cvd_pm10_s5.r<-tb_summ(tb_cvd_pm10_s5);tb_cvd_pm10_s5.r$subgroup="Men"

tb_cvd_o3_s1.r<-tb_summ(tb_cvd_o3_s1);tb_cvd_o3_s1.r$subgroup="Age group 0-14"
tb_cvd_o3_s2.r<-tb_summ(tb_cvd_o3_s2);tb_cvd_o3_s2.r$subgroup="Age group 15-64"
tb_cvd_o3_s3.r<-tb_summ(tb_cvd_o3_s3);tb_cvd_o3_s3.r$subgroup="Age group 65+"
tb_cvd_o3_s4.r<-tb_summ(tb_cvd_o3_s4);tb_cvd_o3_s4.r$subgroup="Women"
tb_cvd_o3_s5.r<-tb_summ(tb_cvd_o3_s5);tb_cvd_o3_s5.r$subgroup="Men"

tb_cvd_pm25.r<-rbind(tb_cvd_pm25_s1.r,tb_cvd_pm25_s2.r,tb_cvd_pm25_s3.r,tb_cvd_pm25_s4.r,tb_cvd_pm25_s5.r)
tb_cvd_pm10.r<-rbind(tb_cvd_pm10_s1.r,tb_cvd_pm10_s2.r,tb_cvd_pm10_s3.r,tb_cvd_pm10_s4.r,tb_cvd_pm10_s5.r)
tb_cvd_o3.r  <-rbind(tb_cvd_o3_s1.r  ,tb_cvd_o3_s2.r  ,tb_cvd_o3_s3.r  ,tb_cvd_o3_s4.r  ,tb_cvd_o3_s5.r)

write.csv(tb_cvd_pm25.r,file="tb_cvd_pm25.r.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_cvd_pm10.r,file="tb_cvd_pm10.r.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_cvd_o3.r  ,file="tb_cvd_o3.r.csv"  ,row.names=F,na="",fileEncoding = "euc-kr")

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#호흡기 사망 & 초미세먼지 (by gamm4, linear)

tb0<-gamm4_excess_func_strata("respiratory","PM25","AG0014",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$RESP_AG0014)
tb1<-gamm4_excess_func_strata("respiratory","PM25","AG0014",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$RESP_AG0014)
tb2<-gamm4_excess_func_strata("respiratory","PM25","AG0014",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$RESP_AG0014)
tb3<-gamm4_excess_func_strata("respiratory","PM25","AG0014",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$RESP_AG0014)
tb4<-gamm4_excess_func_strata("respiratory","PM25","AG0014",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$RESP_AG0014)
tb5<-gamm4_excess_func_strata("respiratory","PM25","AG0014",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$RESP_AG0014)
tb6<-gamm4_excess_func_strata("respiratory","PM25","AG0014",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$RESP_AG0014)
tb7<-gamm4_excess_func_strata("respiratory","PM25","AG0014",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$RESP_AG0014)
tb_respiratory_pm25_s1<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("respiratory","PM25","AG1564",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$RESP_AG1564)
tb1<-gamm4_excess_func_strata("respiratory","PM25","AG1564",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$RESP_AG1564)
tb2<-gamm4_excess_func_strata("respiratory","PM25","AG1564",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$RESP_AG1564)
tb3<-gamm4_excess_func_strata("respiratory","PM25","AG1564",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$RESP_AG1564)
tb4<-gamm4_excess_func_strata("respiratory","PM25","AG1564",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$RESP_AG1564)
tb5<-gamm4_excess_func_strata("respiratory","PM25","AG1564",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$RESP_AG1564)
tb6<-gamm4_excess_func_strata("respiratory","PM25","AG1564",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$RESP_AG1564)
tb7<-gamm4_excess_func_strata("respiratory","PM25","AG1564",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$RESP_AG1564)
tb_respiratory_pm25_s2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("respiratory","PM25","AG65",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$RESP_AG65)
tb1<-gamm4_excess_func_strata("respiratory","PM25","AG65",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$RESP_AG65)
tb2<-gamm4_excess_func_strata("respiratory","PM25","AG65",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$RESP_AG65)
tb3<-gamm4_excess_func_strata("respiratory","PM25","AG65",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$RESP_AG65)
tb4<-gamm4_excess_func_strata("respiratory","PM25","AG65",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$RESP_AG65)
tb5<-gamm4_excess_func_strata("respiratory","PM25","AG65",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$RESP_AG65)
tb6<-gamm4_excess_func_strata("respiratory","PM25","AG65",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$RESP_AG65)
tb7<-gamm4_excess_func_strata("respiratory","PM25","AG65",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$RESP_AG65)
tb_respiratory_pm25_s3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("respiratory","PM25","SEX_F",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$RESP_SEX_F)
tb1<-gamm4_excess_func_strata("respiratory","PM25","SEX_F",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$RESP_SEX_F)
tb2<-gamm4_excess_func_strata("respiratory","PM25","SEX_F",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$RESP_SEX_F)
tb3<-gamm4_excess_func_strata("respiratory","PM25","SEX_F",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$RESP_SEX_F)
tb4<-gamm4_excess_func_strata("respiratory","PM25","SEX_F",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$RESP_SEX_F)
tb5<-gamm4_excess_func_strata("respiratory","PM25","SEX_F",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$RESP_SEX_F)
tb6<-gamm4_excess_func_strata("respiratory","PM25","SEX_F",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$RESP_SEX_F)
tb7<-gamm4_excess_func_strata("respiratory","PM25","SEX_F",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$RESP_SEX_F)
tb_respiratory_pm25_s4<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("respiratory","PM25","SEX_M",raw$pm25_m0_diff,raw$pm25_new   ,1,raw$RESP_SEX_M)
tb1<-gamm4_excess_func_strata("respiratory","PM25","SEX_M",raw$pm25_m1_diff,raw$pm25_new_m1,2,raw$RESP_SEX_M)
tb2<-gamm4_excess_func_strata("respiratory","PM25","SEX_M",raw$pm25_m2_diff,raw$pm25_new_m2,3,raw$RESP_SEX_M)
tb3<-gamm4_excess_func_strata("respiratory","PM25","SEX_M",raw$pm25_m3_diff,raw$pm25_new_m3,4,raw$RESP_SEX_M)
tb4<-gamm4_excess_func_strata("respiratory","PM25","SEX_M",raw$pm25_m4_diff,raw$pm25_new_m4,5,raw$RESP_SEX_M)
tb5<-gamm4_excess_func_strata("respiratory","PM25","SEX_M",raw$pm25_m5_diff,raw$pm25_new_m5,6,raw$RESP_SEX_M)
tb6<-gamm4_excess_func_strata("respiratory","PM25","SEX_M",raw$pm25_m6_diff,raw$pm25_new_m6,7,raw$RESP_SEX_M)
tb7<-gamm4_excess_func_strata("respiratory","PM25","SEX_M",raw$pm25_m7_diff,raw$pm25_new_m7,8,raw$RESP_SEX_M)
tb_respiratory_pm25_s5<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#호흡기 사망 & 미세먼지 (by gamm4, linear)
tb0<-gamm4_excess_func_strata("respiratory","PM10","AG0014",raw$pm10_m0_diff,raw$pm10   ,1,raw$RESP_AG0014)
tb1<-gamm4_excess_func_strata("respiratory","PM10","AG0014",raw$pm10_m1_diff,raw$pm10_m1,2,raw$RESP_AG0014)
tb2<-gamm4_excess_func_strata("respiratory","PM10","AG0014",raw$pm10_m2_diff,raw$pm10_m2,3,raw$RESP_AG0014)
tb3<-gamm4_excess_func_strata("respiratory","PM10","AG0014",raw$pm10_m3_diff,raw$pm10_m3,4,raw$RESP_AG0014)
tb4<-gamm4_excess_func_strata("respiratory","PM10","AG0014",raw$pm10_m4_diff,raw$pm10_m4,5,raw$RESP_AG0014)
tb5<-gamm4_excess_func_strata("respiratory","PM10","AG0014",raw$pm10_m5_diff,raw$pm10_m5,6,raw$RESP_AG0014)
tb6<-gamm4_excess_func_strata("respiratory","PM10","AG0014",raw$pm10_m6_diff,raw$pm10_m6,7,raw$RESP_AG0014)
tb7<-gamm4_excess_func_strata("respiratory","PM10","AG0014",raw$pm10_m7_diff,raw$pm10_m7,8,raw$RESP_AG0014)
tb_respiratory_pm10_s1<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("respiratory","PM10","AG1564",raw$pm10_m0_diff,raw$pm10   ,1,raw$RESP_AG1564)
tb1<-gamm4_excess_func_strata("respiratory","PM10","AG1564",raw$pm10_m1_diff,raw$pm10_m1,2,raw$RESP_AG1564)
tb2<-gamm4_excess_func_strata("respiratory","PM10","AG1564",raw$pm10_m2_diff,raw$pm10_m2,3,raw$RESP_AG1564)
tb3<-gamm4_excess_func_strata("respiratory","PM10","AG1564",raw$pm10_m3_diff,raw$pm10_m3,4,raw$RESP_AG1564)
tb4<-gamm4_excess_func_strata("respiratory","PM10","AG1564",raw$pm10_m4_diff,raw$pm10_m4,5,raw$RESP_AG1564)
tb5<-gamm4_excess_func_strata("respiratory","PM10","AG1564",raw$pm10_m5_diff,raw$pm10_m5,6,raw$RESP_AG1564)
tb6<-gamm4_excess_func_strata("respiratory","PM10","AG1564",raw$pm10_m6_diff,raw$pm10_m6,7,raw$RESP_AG1564)
tb7<-gamm4_excess_func_strata("respiratory","PM10","AG1564",raw$pm10_m7_diff,raw$pm10_m7,8,raw$RESP_AG1564)
tb_respiratory_pm10_s2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("respiratory","PM10","AG65",raw$pm10_m0_diff,raw$pm10   ,1,raw$RESP_AG65)
tb1<-gamm4_excess_func_strata("respiratory","PM10","AG65",raw$pm10_m1_diff,raw$pm10_m1,2,raw$RESP_AG65)
tb2<-gamm4_excess_func_strata("respiratory","PM10","AG65",raw$pm10_m2_diff,raw$pm10_m2,3,raw$RESP_AG65)
tb3<-gamm4_excess_func_strata("respiratory","PM10","AG65",raw$pm10_m3_diff,raw$pm10_m3,4,raw$RESP_AG65)
tb4<-gamm4_excess_func_strata("respiratory","PM10","AG65",raw$pm10_m4_diff,raw$pm10_m4,5,raw$RESP_AG65)
tb5<-gamm4_excess_func_strata("respiratory","PM10","AG65",raw$pm10_m5_diff,raw$pm10_m5,6,raw$RESP_AG65)
tb6<-gamm4_excess_func_strata("respiratory","PM10","AG65",raw$pm10_m6_diff,raw$pm10_m6,7,raw$RESP_AG65)
tb7<-gamm4_excess_func_strata("respiratory","PM10","AG65",raw$pm10_m7_diff,raw$pm10_m7,8,raw$RESP_AG65)
tb_respiratory_pm10_s3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("respiratory","PM10","SEX_F",raw$pm10_m0_diff,raw$pm10   ,1,raw$RESP_SEX_F)
tb1<-gamm4_excess_func_strata("respiratory","PM10","SEX_F",raw$pm10_m1_diff,raw$pm10_m1,2,raw$RESP_SEX_F)
tb2<-gamm4_excess_func_strata("respiratory","PM10","SEX_F",raw$pm10_m2_diff,raw$pm10_m2,3,raw$RESP_SEX_F)
tb3<-gamm4_excess_func_strata("respiratory","PM10","SEX_F",raw$pm10_m3_diff,raw$pm10_m3,4,raw$RESP_SEX_F)
tb4<-gamm4_excess_func_strata("respiratory","PM10","SEX_F",raw$pm10_m4_diff,raw$pm10_m4,5,raw$RESP_SEX_F)
tb5<-gamm4_excess_func_strata("respiratory","PM10","SEX_F",raw$pm10_m5_diff,raw$pm10_m5,6,raw$RESP_SEX_F)
tb6<-gamm4_excess_func_strata("respiratory","PM10","SEX_F",raw$pm10_m6_diff,raw$pm10_m6,7,raw$RESP_SEX_F)
tb7<-gamm4_excess_func_strata("respiratory","PM10","SEX_F",raw$pm10_m7_diff,raw$pm10_m7,8,raw$RESP_SEX_F)
tb_respiratory_pm10_s4<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("respiratory","PM10","SEX_M",raw$pm10_m0_diff,raw$pm10   ,1,raw$RESP_SEX_M)
tb1<-gamm4_excess_func_strata("respiratory","PM10","SEX_M",raw$pm10_m1_diff,raw$pm10_m1,2,raw$RESP_SEX_M)
tb2<-gamm4_excess_func_strata("respiratory","PM10","SEX_M",raw$pm10_m2_diff,raw$pm10_m2,3,raw$RESP_SEX_M)
tb3<-gamm4_excess_func_strata("respiratory","PM10","SEX_M",raw$pm10_m3_diff,raw$pm10_m3,4,raw$RESP_SEX_M)
tb4<-gamm4_excess_func_strata("respiratory","PM10","SEX_M",raw$pm10_m4_diff,raw$pm10_m4,5,raw$RESP_SEX_M)
tb5<-gamm4_excess_func_strata("respiratory","PM10","SEX_M",raw$pm10_m5_diff,raw$pm10_m5,6,raw$RESP_SEX_M)
tb6<-gamm4_excess_func_strata("respiratory","PM10","SEX_M",raw$pm10_m6_diff,raw$pm10_m6,7,raw$RESP_SEX_M)
tb7<-gamm4_excess_func_strata("respiratory","PM10","SEX_M",raw$pm10_m7_diff,raw$pm10_m7,8,raw$RESP_SEX_M)
tb_respiratory_pm10_s5<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#호흡기 사망 & 오존 (by gamm4, linear)
tb0<-gamm4_excess_func_strata("respiratory","O3","AG0014",raw$o3_m0_diff,raw$o3   ,1,raw$RESP_AG0014)
tb1<-gamm4_excess_func_strata("respiratory","O3","AG0014",raw$o3_m1_diff,raw$o3_m1,2,raw$RESP_AG0014)
tb2<-gamm4_excess_func_strata("respiratory","O3","AG0014",raw$o3_m2_diff,raw$o3_m2,3,raw$RESP_AG0014)
tb3<-gamm4_excess_func_strata("respiratory","O3","AG0014",raw$o3_m3_diff,raw$o3_m3,4,raw$RESP_AG0014)
tb4<-gamm4_excess_func_strata("respiratory","O3","AG0014",raw$o3_m4_diff,raw$o3_m4,5,raw$RESP_AG0014)
tb5<-gamm4_excess_func_strata("respiratory","O3","AG0014",raw$o3_m5_diff,raw$o3_m5,6,raw$RESP_AG0014)
tb6<-gamm4_excess_func_strata("respiratory","O3","AG0014",raw$o3_m6_diff,raw$o3_m6,7,raw$RESP_AG0014)
tb7<-gamm4_excess_func_strata("respiratory","O3","AG0014",raw$o3_m7_diff,raw$o3_m7,8,raw$RESP_AG0014)
tb_respiratory_o3_s1<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("respiratory","O3","AG1564",raw$o3_m0_diff,raw$o3   ,1,raw$RESP_AG1564)
tb1<-gamm4_excess_func_strata("respiratory","O3","AG1564",raw$o3_m1_diff,raw$o3_m1,2,raw$RESP_AG1564)
tb2<-gamm4_excess_func_strata("respiratory","O3","AG1564",raw$o3_m2_diff,raw$o3_m2,3,raw$RESP_AG1564)
tb3<-gamm4_excess_func_strata("respiratory","O3","AG1564",raw$o3_m3_diff,raw$o3_m3,4,raw$RESP_AG1564)
tb4<-gamm4_excess_func_strata("respiratory","O3","AG1564",raw$o3_m4_diff,raw$o3_m4,5,raw$RESP_AG1564)
tb5<-gamm4_excess_func_strata("respiratory","O3","AG1564",raw$o3_m5_diff,raw$o3_m5,6,raw$RESP_AG1564)
tb6<-gamm4_excess_func_strata("respiratory","O3","AG1564",raw$o3_m6_diff,raw$o3_m6,7,raw$RESP_AG1564)
tb7<-gamm4_excess_func_strata("respiratory","O3","AG1564",raw$o3_m7_diff,raw$o3_m7,8,raw$RESP_AG1564)
tb_respiratory_o3_s2<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("respiratory","O3","AG65",raw$o3_m0_diff,raw$o3   ,1,raw$RESP_AG65)
tb1<-gamm4_excess_func_strata("respiratory","O3","AG65",raw$o3_m1_diff,raw$o3_m1,2,raw$RESP_AG65)
tb2<-gamm4_excess_func_strata("respiratory","O3","AG65",raw$o3_m2_diff,raw$o3_m2,3,raw$RESP_AG65)
tb3<-gamm4_excess_func_strata("respiratory","O3","AG65",raw$o3_m3_diff,raw$o3_m3,4,raw$RESP_AG65)
tb4<-gamm4_excess_func_strata("respiratory","O3","AG65",raw$o3_m4_diff,raw$o3_m4,5,raw$RESP_AG65)
tb5<-gamm4_excess_func_strata("respiratory","O3","AG65",raw$o3_m5_diff,raw$o3_m5,6,raw$RESP_AG65)
tb6<-gamm4_excess_func_strata("respiratory","O3","AG65",raw$o3_m6_diff,raw$o3_m6,7,raw$RESP_AG65)
tb7<-gamm4_excess_func_strata("respiratory","O3","AG65",raw$o3_m7_diff,raw$o3_m7,8,raw$RESP_AG65)
tb_respiratory_o3_s3<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("respiratory","O3","SEX_F",raw$o3_m0_diff,raw$o3   ,1,raw$RESP_SEX_F)
tb1<-gamm4_excess_func_strata("respiratory","O3","SEX_F",raw$o3_m1_diff,raw$o3_m1,2,raw$RESP_SEX_F)
tb2<-gamm4_excess_func_strata("respiratory","O3","SEX_F",raw$o3_m2_diff,raw$o3_m2,3,raw$RESP_SEX_F)
tb3<-gamm4_excess_func_strata("respiratory","O3","SEX_F",raw$o3_m3_diff,raw$o3_m3,4,raw$RESP_SEX_F)
tb4<-gamm4_excess_func_strata("respiratory","O3","SEX_F",raw$o3_m4_diff,raw$o3_m4,5,raw$RESP_SEX_F)
tb5<-gamm4_excess_func_strata("respiratory","O3","SEX_F",raw$o3_m5_diff,raw$o3_m5,6,raw$RESP_SEX_F)
tb6<-gamm4_excess_func_strata("respiratory","O3","SEX_F",raw$o3_m6_diff,raw$o3_m6,7,raw$RESP_SEX_F)
tb7<-gamm4_excess_func_strata("respiratory","O3","SEX_F",raw$o3_m7_diff,raw$o3_m7,8,raw$RESP_SEX_F)
tb_respiratory_o3_s4<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func_strata("respiratory","O3","SEX_M",raw$o3_m0_diff,raw$o3   ,1,raw$RESP_SEX_M)
tb1<-gamm4_excess_func_strata("respiratory","O3","SEX_M",raw$o3_m1_diff,raw$o3_m1,2,raw$RESP_SEX_M)
tb2<-gamm4_excess_func_strata("respiratory","O3","SEX_M",raw$o3_m2_diff,raw$o3_m2,3,raw$RESP_SEX_M)
tb3<-gamm4_excess_func_strata("respiratory","O3","SEX_M",raw$o3_m3_diff,raw$o3_m3,4,raw$RESP_SEX_M)
tb4<-gamm4_excess_func_strata("respiratory","O3","SEX_M",raw$o3_m4_diff,raw$o3_m4,5,raw$RESP_SEX_M)
tb5<-gamm4_excess_func_strata("respiratory","O3","SEX_M",raw$o3_m5_diff,raw$o3_m5,6,raw$RESP_SEX_M)
tb6<-gamm4_excess_func_strata("respiratory","O3","SEX_M",raw$o3_m6_diff,raw$o3_m6,7,raw$RESP_SEX_M)
tb7<-gamm4_excess_func_strata("respiratory","O3","SEX_M",raw$o3_m7_diff,raw$o3_m7,8,raw$RESP_SEX_M)
tb_respiratory_o3_s5<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)


#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
tb_respiratory_pm25_s1$subgroup="Age group 0-14"
tb_respiratory_pm25_s2$subgroup="Age group 15-64"
tb_respiratory_pm25_s3$subgroup="Age group 65+"
tb_respiratory_pm25_s4$subgroup="Women"
tb_respiratory_pm25_s5$subgroup="Men"

tb_respiratory_pm10_s1$subgroup="Age group 0-14"
tb_respiratory_pm10_s2$subgroup="Age group 15-64"
tb_respiratory_pm10_s3$subgroup="Age group 65+"
tb_respiratory_pm10_s4$subgroup="Women"
tb_respiratory_pm10_s5$subgroup="Men"

tb_respiratory_o3_s1$subgroup="Age group 0-14"
tb_respiratory_o3_s2$subgroup="Age group 15-64"
tb_respiratory_o3_s3$subgroup="Age group 65+"
tb_respiratory_o3_s4$subgroup="Women"
tb_respiratory_o3_s5$subgroup="Men"

tb_respiratory_pm25<-rbind(tb_respiratory_pm25_s1,tb_respiratory_pm25_s2,tb_respiratory_pm25_s3,tb_respiratory_pm25_s4,tb_respiratory_pm25_s5)
tb_respiratory_pm10<-rbind(tb_respiratory_pm10_s1,tb_respiratory_pm10_s2,tb_respiratory_pm10_s3,tb_respiratory_pm10_s4,tb_respiratory_pm10_s5)
tb_respiratory_o3  <-rbind(tb_respiratory_o3_s1  ,tb_respiratory_o3_s2  ,tb_respiratory_o3_s3  ,tb_respiratory_o3_s4  ,tb_respiratory_o3_s5)

setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result\\gamm4_linear_strata")
write.csv(tb_respiratory_pm25,file="tb_respiratory_pm25.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_respiratory_pm10,file="tb_respiratory_pm10.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_respiratory_o3  ,file="tb_respiratory_o3.csv"  ,row.names=F,na="",fileEncoding = "euc-kr")

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

tb_respiratory_pm25_s1.r<-tb_summ(tb_respiratory_pm25_s1);tb_respiratory_pm25_s1.r$subgroup="Age group 0-14"
tb_respiratory_pm25_s2.r<-tb_summ(tb_respiratory_pm25_s2);tb_respiratory_pm25_s2.r$subgroup="Age group 15-64"
tb_respiratory_pm25_s3.r<-tb_summ(tb_respiratory_pm25_s3);tb_respiratory_pm25_s3.r$subgroup="Age group 65+"
tb_respiratory_pm25_s4.r<-tb_summ(tb_respiratory_pm25_s4);tb_respiratory_pm25_s4.r$subgroup="Women"
tb_respiratory_pm25_s5.r<-tb_summ(tb_respiratory_pm25_s5);tb_respiratory_pm25_s5.r$subgroup="Men"

tb_respiratory_pm10_s1.r<-tb_summ(tb_respiratory_pm10_s1);tb_respiratory_pm10_s1.r$subgroup="Age group 0-14"
tb_respiratory_pm10_s2.r<-tb_summ(tb_respiratory_pm10_s2);tb_respiratory_pm10_s2.r$subgroup="Age group 15-64"
tb_respiratory_pm10_s3.r<-tb_summ(tb_respiratory_pm10_s3);tb_respiratory_pm10_s3.r$subgroup="Age group 65+"
tb_respiratory_pm10_s4.r<-tb_summ(tb_respiratory_pm10_s4);tb_respiratory_pm10_s4.r$subgroup="Women"
tb_respiratory_pm10_s5.r<-tb_summ(tb_respiratory_pm10_s5);tb_respiratory_pm10_s5.r$subgroup="Men"

tb_respiratory_o3_s1.r<-tb_summ(tb_respiratory_o3_s1);tb_respiratory_o3_s1.r$subgroup="Age group 0-14"
tb_respiratory_o3_s2.r<-tb_summ(tb_respiratory_o3_s2);tb_respiratory_o3_s2.r$subgroup="Age group 15-64"
tb_respiratory_o3_s3.r<-tb_summ(tb_respiratory_o3_s3);tb_respiratory_o3_s3.r$subgroup="Age group 65+"
tb_respiratory_o3_s4.r<-tb_summ(tb_respiratory_o3_s4);tb_respiratory_o3_s4.r$subgroup="Women"
tb_respiratory_o3_s5.r<-tb_summ(tb_respiratory_o3_s5);tb_respiratory_o3_s5.r$subgroup="Men"

tb_respiratory_pm25.r<-rbind(tb_respiratory_pm25_s1.r,tb_respiratory_pm25_s2.r,tb_respiratory_pm25_s3.r,tb_respiratory_pm25_s4.r,tb_respiratory_pm25_s5.r)
tb_respiratory_pm10.r<-rbind(tb_respiratory_pm10_s1.r,tb_respiratory_pm10_s2.r,tb_respiratory_pm10_s3.r,tb_respiratory_pm10_s4.r,tb_respiratory_pm10_s5.r)
tb_respiratory_o3.r  <-rbind(tb_respiratory_o3_s1.r  ,tb_respiratory_o3_s2.r  ,tb_respiratory_o3_s3.r  ,tb_respiratory_o3_s4.r  ,tb_respiratory_o3_s5.r)

write.csv(tb_respiratory_pm25.r,file="tb_respiratory_pm25.r.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_respiratory_pm10.r,file="tb_respiratory_pm10.r.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_respiratory_o3.r  ,file="tb_respiratory_o3.r.csv"  ,row.names=F,na="",fileEncoding = "euc-kr")

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

