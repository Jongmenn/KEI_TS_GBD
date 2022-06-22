#library
pacman::p_load(gamm4,mgcv,lme4,splines,ggplot2,gridExtra,dplyr,lubridate,scales,
               RColorBrewer)

#NHIS(공단 질환 자료), 선형으로 추정한 결과 가져오기 

#질환별 & 대기오염 관련성 선형으로 추정한 결과 저장된 폴더  디렉토리
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\gamm4_linear")
directory<-getwd()

#각 질환별 & 대기오염 관련성 선형으로 추정한 결과 저장된 폴더 
dir01<-paste0(directory,substr(list.dirs()[2],2,nchar(list.dirs()[2])))
dir02<-paste0(directory,substr(list.dirs()[3],2,nchar(list.dirs()[3])))
dir03<-paste0(directory,substr(list.dirs()[4],2,nchar(list.dirs()[4])))
dir04<-paste0(directory,substr(list.dirs()[5],2,nchar(list.dirs()[5])))
dir05<-paste0(directory,substr(list.dirs()[6],2,nchar(list.dirs()[6])))
dir06<-paste0(directory,substr(list.dirs()[7],2,nchar(list.dirs()[7])))
dir07<-paste0(directory,substr(list.dirs()[8],2,nchar(list.dirs()[8])))
dir08<-paste0(directory,substr(list.dirs()[9],2,nchar(list.dirs()[9])))
dir09<-paste0(directory,substr(list.dirs()[10],2,nchar(list.dirs()[10])))
dir10<-paste0(directory,substr(list.dirs()[11],2,nchar(list.dirs()[11])))
dir11<-paste0(directory,substr(list.dirs()[12],2,nchar(list.dirs()[12])))
dir12<-paste0(directory,substr(list.dirs()[13],2,nchar(list.dirs()[13])))
dir13<-paste0(directory,substr(list.dirs()[14],2,nchar(list.dirs()[14])))
dir14<-paste0(directory,substr(list.dirs()[15],2,nchar(list.dirs()[15])))
dir15<-paste0(directory,substr(list.dirs()[16],2,nchar(list.dirs()[16])))
dir16<-paste0(directory,substr(list.dirs()[17],2,nchar(list.dirs()[17])))
dir17<-paste0(directory,substr(list.dirs()[18],2,nchar(list.dirs()[18])))
dir18<-paste0(directory,substr(list.dirs()[19],2,nchar(list.dirs()[19])))
dir19<-paste0(directory,substr(list.dirs()[20],2,nchar(list.dirs()[20])))
dir20<-paste0(directory,substr(list.dirs()[21],2,nchar(list.dirs()[21])))

#폴더명 선형 결과 저장할 리스트 파일
dir1_list=NULL ;dir2_list=NULL ;dir3_list=NULL ;dir4_list=NULL
dir5_list=NULL ;dir6_list=NULL ;dir7_list=NULL ;dir8_list=NULL
dir9_list=NULL ;dir10_list=NULL;dir11_list=NULL;dir12_list=NULL
dir13_list=NULL;dir14_list=NULL;dir15_list=NULL;dir16_list=NULL
dir17_list=NULL;dir18_list=NULL;dir19_list=NULL;dir20_list=NULL

gamm4_linear_beta_func<-function(dir,result_list){
  setwd(dir)
  
  for(i in 1:length(list.files())){
    label<-list.files()[i]
    
    outcome <-unlist(strsplit(label,"_"))[2]
    exposure<-unlist(strsplit(label,"_"))[3]
    lag     <-substr(unlist(strsplit(label,"_"))[5],1,2)
    
    #gamm4 linear assumption results 
    f<-readRDS(list.files()[i])
    #results table 
    df <-as.data.frame(t(c(summary(f$gam)$p.table[2,],summary(f$gam)$r.sq)))
    df2<-data.frame(outcome,exposure,lag,df)
    
    #relabel
    names(df2)=c("outcome","exposure","lag","Estimate","SE","zval","Pval","R2")
    
    #unit change, PM이면 ug/m3, 그 외 대기오염 ppm
    df2$단위=ifelse(exposure=="pm25" | exposure=="pm10","ug/m3","ppm")
    
    df2$unit=ifelse(exposure=="pm25" | exposure=="pm10",1,
                    ifelse(exposure=="co",0.01,0.001))
    
    #unit change 대기오염 물질 같은 경우 검토
    uc<-1/df2$unit
    
    df2$RR    =exp(uc*df2$Estimate)
    df2$RR_lci=exp(uc*(df2$Estimate-1.96*df2$SE))
    df2$RR_uci=exp(uc*(df2$Estimate+1.96*df2$SE))
    
    df2$lag<-gsub("m","lag0",df2$lag)
    
    result_list[[i]]<-df2 %>% dplyr::select(outcome:SE,Pval,unit,RR:RR_uci,단위,R2)
    print(i)
  }
  do.call(rbind,result_list)}

nhis_linear_dis01<-gamm4_linear_beta_func(dir01,dir1_list)
nhis_linear_dis02<-gamm4_linear_beta_func(dir02,dir2_list)
nhis_linear_dis03<-gamm4_linear_beta_func(dir03,dir3_list)
nhis_linear_dis04<-gamm4_linear_beta_func(dir04,dir4_list)
nhis_linear_dis05<-gamm4_linear_beta_func(dir05,dir5_list)
nhis_linear_dis06<-gamm4_linear_beta_func(dir06,dir6_list)
nhis_linear_dis07<-gamm4_linear_beta_func(dir07,dir7_list)
nhis_linear_dis08<-gamm4_linear_beta_func(dir08,dir8_list)
nhis_linear_dis09<-gamm4_linear_beta_func(dir09,dir9_list)
nhis_linear_dis10<-gamm4_linear_beta_func(dir10,dir10_list)
nhis_linear_dis11<-gamm4_linear_beta_func(dir11,dir11_list)
nhis_linear_dis12<-gamm4_linear_beta_func(dir12,dir12_list)
nhis_linear_dis13<-gamm4_linear_beta_func(dir13,dir13_list)
nhis_linear_dis14<-gamm4_linear_beta_func(dir14,dir14_list)
nhis_linear_dis15<-gamm4_linear_beta_func(dir15,dir15_list)
nhis_linear_dis16<-gamm4_linear_beta_func(dir16,dir16_list)
nhis_linear_dis17<-gamm4_linear_beta_func(dir17,dir17_list)
nhis_linear_dis18<-gamm4_linear_beta_func(dir18,dir18_list)
nhis_linear_dis19<-gamm4_linear_beta_func(dir19,dir19_list)
nhis_linear_dis20<-gamm4_linear_beta_func(dir20,dir20_list)


nhis_linear_results<-rbind(nhis_linear_dis01,nhis_linear_dis02,nhis_linear_dis03,nhis_linear_dis04,
                           nhis_linear_dis05,nhis_linear_dis06,nhis_linear_dis07,nhis_linear_dis08,
                           nhis_linear_dis09,nhis_linear_dis10,nhis_linear_dis11,nhis_linear_dis12,
                           nhis_linear_dis13,nhis_linear_dis14,nhis_linear_dis15,nhis_linear_dis16,
                           nhis_linear_dis17,nhis_linear_dis18,nhis_linear_dis19,nhis_linear_dis20)


setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result")
write.csv(nhis_linear_results,file="nhis_linear_results.csv",row.names=F,na="",fileEncoding ="euc-kr")

#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#
library(readxl)
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담")
dat<-read_excel("KEI_SNU_TS_analysis_OJM_20220608.xlsx",sheet="공단질환_선형결과(gamm4)")

dat$exposure=factor(dat$exposure,levels=c("PM2.5","PM10"))

table(dat$outcome)

nhis_asthma_pm10<-subset(dat,outcome %in% c("astm02","astm03","astm0203") & exposure=="PM10")
nhis_asthma_pm25<-subset(dat,outcome %in% c("astm02","astm03","astm0203") & exposure=="PM2.5")

nhis_asthma_pm10$outcome=with(nhis_asthma_pm10,ifelse(outcome=="astm02","천식:입원",
                                                      ifelse(outcome=="astm03","천식:외래","천식:입원 또는 외래")))
nhis_asthma_pm25$outcome=with(nhis_asthma_pm25,ifelse(outcome=="astm02","천식:입원",
                                                      ifelse(outcome=="astm03","천식:외래","천식:입원 또는 외래")))


x11();ggplot(nhis_asthma_pm10,aes(lag,RR,group=outcome))+
  geom_point(size=2,
             aes(shape=outcome),
             position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=RR_lci,ymax=RR_uci),
                position=position_dodge(0.5),width=0.1)+
  theme_gray(base_size=30)+labs(x="",y="Relative risk (95% CI)")+
  theme(legend.title = element_blank())


#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#
table(dat$outcome)
zz<-dat %>% filter(outcome %in% c("astm02","cvd02","diz02","hemo02","hf02","htn02",
                                  "ihd02","isch02","om02") )

zz$obs=as.numeric(gsub("lag","",zz$lag))
x11();ggplot(zz,aes(obs,RR,group=exposure))+
  geom_point(size=2,
             aes(shape=exposure),
             position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=RR_lci,ymax=RR_uci),
                position=position_dodge(0.5),width=0.1)+
  theme_gray(base_size=25)+labs(x="",y="Relative risk (95% CI)")+
  theme(legend.title = element_blank(),
        legend.position = "top")+facet_wrap(~outcome,nrow=2,scales="free")+
  scale_x_continuous(breaks=c(1:8)-1)+
  geom_hline(yintercept = 1,col="red")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
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
                                 o3,o3_m1:o3_m7)

dat$area=factor(dat$area)

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
dat$pm25_m0_diff=with(dat,ifelse(pm25_new   -15>0,pm25_new   -15,0))
dat$pm25_m1_diff=with(dat,ifelse(pm25_new_m1-15>0,pm25_new_m1-15,0))
dat$pm25_m2_diff=with(dat,ifelse(pm25_new_m2-15>0,pm25_new_m2-15,0))
dat$pm25_m3_diff=with(dat,ifelse(pm25_new_m3-15>0,pm25_new_m3-15,0))
dat$pm25_m4_diff=with(dat,ifelse(pm25_new_m4-15>0,pm25_new_m4-15,0))
dat$pm25_m5_diff=with(dat,ifelse(pm25_new_m5-15>0,pm25_new_m5-15,0))
dat$pm25_m6_diff=with(dat,ifelse(pm25_new_m6-15>0,pm25_new_m6-15,0))
dat$pm25_m7_diff=with(dat,ifelse(pm25_new_m7-15>0,pm25_new_m7-15,0))

dat$pm10_m0_diff=with(dat,ifelse(pm10   -45>0,pm10   -45,0))
dat$pm10_m1_diff=with(dat,ifelse(pm10_m1-45>0,pm10_m1-45,0))
dat$pm10_m2_diff=with(dat,ifelse(pm10_m2-45>0,pm10_m2-45,0))
dat$pm10_m3_diff=with(dat,ifelse(pm10_m3-45>0,pm10_m3-45,0))
dat$pm10_m4_diff=with(dat,ifelse(pm10_m4-45>0,pm10_m4-45,0))
dat$pm10_m5_diff=with(dat,ifelse(pm10_m5-45>0,pm10_m5-45,0))
dat$pm10_m6_diff=with(dat,ifelse(pm10_m6-45>0,pm10_m6-45,0))
dat$pm10_m7_diff=with(dat,ifelse(pm10_m7-45>0,pm10_m7-45,0))

gamm4_result<-read_excel("KEI_SNU_TS_analysis_OJM_20220608.xlsx",sheet="공단질환_선형결과(gamm4)")

gamm4_excess_func<-function(Y,X,expdiff,exposure,i,outcome){
  d<-dat
  d$expdiff=expdiff  #delta exposure (노출 변화량)
  d$exposue=exposure #노출변수 (대기오염)
  d$outcome=outcome  #결과변수 (사망자수, 입원자수)
  
  #기준농도-최저농도  (delta exposure)
  d$expdiff2=with(d,exposure-min(exposure,na.rm=T))
  
  #원시자료에서 보고자하는 노출시점(lag01이면 lag01노출에 대한)
  #노출 값 존재하는 경우에 대해서만 산출
  #추정한 RR 값 
  ss<-subset(gamm4_result, outcome==Y & exposure==X)[i,]
  RR    <-ss$RR
  RR_lci<-ss$RR_lci
  RR_uci<-ss$RR_uci
  
  #Attributable risk % (RR-1)/RR  ;  1-1/RR
  #AR% = (RR-1) / RR x 100. AR% is also known as “Attributable Fraction (Exposed)”
  
  #시나리오 1: 농도차이(고농도만 관심)*관심질환(사망자 수)*Attributable risk
  d$e_dth    =with(d,1/ss$unit*expdiff*outcome*((RR-1)/RR))
  d$e_dth_lci=with(d,1/ss$unit*expdiff*outcome*((RR_lci-1)/RR_lci))
  d$e_dth_uci=with(d,1/ss$unit*expdiff*outcome*((RR_uci-1)/RR_uci))
  
  #시나리오 2: 농도차이(모든 농도고려)*관심질환(사망자 수)*Attributable risk
  d$e_dth2    =with(d,1/ss$unit*expdiff2*outcome*((RR-1)/RR))
  d$e_dth_lci2=with(d,1/ss$unit*expdiff2*outcome*((RR_lci-1)/RR_lci))
  d$e_dth_uci2=with(d,1/ss$unit*expdiff2*outcome*((RR_uci-1)/RR_uci))
  
  d2<-d %>% group_by(area,year) %>% summarise(e_dth=sum(e_dth,na.rm=T),
                                              e_dth_lci =sum(e_dth_lci,na.rm=T),
                                              e_dth_uci =sum(e_dth_uci,na.rm=T),
                                              e_dth2    =sum(e_dth2,na.rm=T),
                                              e_dth_lci2=sum(e_dth_lci2,na.rm=T),
                                              e_dth_uci2=sum(e_dth_uci2,na.rm=T)) %>%
    
    mutate(outcome=Y,exposure=X,lag=paste0("lag0",i-1))
  
  d2 %>% dplyr:: select(outcome:lag,area,year:e_dth_uci2)
}

#8*17*14 (lag*city*year)
#------------------------------------------------------------------------------#
#천식: 입원 
tb0<-gamm4_excess_func("astm02","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$astm02_tot)
tb1<-gamm4_excess_func("astm02","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$astm02_tot)
tb2<-gamm4_excess_func("astm02","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$astm02_tot)
tb3<-gamm4_excess_func("astm02","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$astm02_tot)
tb4<-gamm4_excess_func("astm02","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$astm02_tot)
tb5<-gamm4_excess_func("astm02","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$astm02_tot)
tb6<-gamm4_excess_func("astm02","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$astm02_tot)
tb7<-gamm4_excess_func("astm02","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$astm02_tot)

tb_astm02_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("astm02","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$astm02_tot)
tb1<-gamm4_excess_func("astm02","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$astm02_tot)
tb2<-gamm4_excess_func("astm02","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$astm02_tot)
tb3<-gamm4_excess_func("astm02","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$astm02_tot)
tb4<-gamm4_excess_func("astm02","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$astm02_tot)
tb5<-gamm4_excess_func("astm02","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$astm02_tot)
tb6<-gamm4_excess_func("astm02","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$astm02_tot)
tb7<-gamm4_excess_func("astm02","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$astm02_tot)

tb_astm02_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#천식:외래 
tb0<-gamm4_excess_func("astm03","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$astm03_tot)
tb1<-gamm4_excess_func("astm03","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$astm03_tot)
tb2<-gamm4_excess_func("astm03","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$astm03_tot)
tb3<-gamm4_excess_func("astm03","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$astm03_tot)
tb4<-gamm4_excess_func("astm03","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$astm03_tot)
tb5<-gamm4_excess_func("astm03","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$astm03_tot)
tb6<-gamm4_excess_func("astm03","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$astm03_tot)
tb7<-gamm4_excess_func("astm03","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$astm03_tot)

tb_astm03_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("astm03","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$astm03_tot)
tb1<-gamm4_excess_func("astm03","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$astm03_tot)
tb2<-gamm4_excess_func("astm03","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$astm03_tot)
tb3<-gamm4_excess_func("astm03","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$astm03_tot)
tb4<-gamm4_excess_func("astm03","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$astm03_tot)
tb5<-gamm4_excess_func("astm03","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$astm03_tot)
tb6<-gamm4_excess_func("astm03","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$astm03_tot)
tb7<-gamm4_excess_func("astm03","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$astm03_tot)

tb_astm03_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#천식:입원 또는 외래 
tb0<-gamm4_excess_func("astm0203","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$astm0203_tot)
tb1<-gamm4_excess_func("astm0203","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$astm0203_tot)
tb2<-gamm4_excess_func("astm0203","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$astm0203_tot)
tb3<-gamm4_excess_func("astm0203","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$astm0203_tot)
tb4<-gamm4_excess_func("astm0203","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$astm0203_tot)
tb5<-gamm4_excess_func("astm0203","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$astm0203_tot)
tb6<-gamm4_excess_func("astm0203","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$astm0203_tot)
tb7<-gamm4_excess_func("astm0203","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$astm0203_tot)

tb_astm0203_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("astm0203","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$astm0203_tot)
tb1<-gamm4_excess_func("astm0203","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$astm0203_tot)
tb2<-gamm4_excess_func("astm0203","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$astm0203_tot)
tb3<-gamm4_excess_func("astm0203","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$astm0203_tot)
tb4<-gamm4_excess_func("astm0203","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$astm0203_tot)
tb5<-gamm4_excess_func("astm0203","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$astm0203_tot)
tb6<-gamm4_excess_func("astm0203","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$astm0203_tot)
tb7<-gamm4_excess_func("astm0203","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$astm0203_tot)

tb_astm0203_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#심혈관: 입원 
tb0<-gamm4_excess_func("cvd02","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$cvd02_tot)
tb1<-gamm4_excess_func("cvd02","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$cvd02_tot)
tb2<-gamm4_excess_func("cvd02","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$cvd02_tot)
tb3<-gamm4_excess_func("cvd02","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$cvd02_tot)
tb4<-gamm4_excess_func("cvd02","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$cvd02_tot)
tb5<-gamm4_excess_func("cvd02","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$cvd02_tot)
tb6<-gamm4_excess_func("cvd02","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$cvd02_tot)
tb7<-gamm4_excess_func("cvd02","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$cvd02_tot)

tb_cvd02_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("cvd02","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$cvd02_tot)
tb1<-gamm4_excess_func("cvd02","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$cvd02_tot)
tb2<-gamm4_excess_func("cvd02","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$cvd02_tot)
tb3<-gamm4_excess_func("cvd02","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$cvd02_tot)
tb4<-gamm4_excess_func("cvd02","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$cvd02_tot)
tb5<-gamm4_excess_func("cvd02","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$cvd02_tot)
tb6<-gamm4_excess_func("cvd02","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$cvd02_tot)
tb7<-gamm4_excess_func("cvd02","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$cvd02_tot)

tb_cvd02_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#어지러움: 입원 
tb0<-gamm4_excess_func("diz02","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$diz02_tot)
tb1<-gamm4_excess_func("diz02","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$diz02_tot)
tb2<-gamm4_excess_func("diz02","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$diz02_tot)
tb3<-gamm4_excess_func("diz02","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$diz02_tot)
tb4<-gamm4_excess_func("diz02","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$diz02_tot)
tb5<-gamm4_excess_func("diz02","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$diz02_tot)
tb6<-gamm4_excess_func("diz02","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$diz02_tot)
tb7<-gamm4_excess_func("diz02","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$diz02_tot)

tb_diz02_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("diz02","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$diz02_tot)
tb1<-gamm4_excess_func("diz02","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$diz02_tot)
tb2<-gamm4_excess_func("diz02","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$diz02_tot)
tb3<-gamm4_excess_func("diz02","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$diz02_tot)
tb4<-gamm4_excess_func("diz02","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$diz02_tot)
tb5<-gamm4_excess_func("diz02","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$diz02_tot)
tb6<-gamm4_excess_func("diz02","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$diz02_tot)
tb7<-gamm4_excess_func("diz02","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$diz02_tot)

tb_diz02_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#어지러움:외래 
tb0<-gamm4_excess_func("diz03","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$diz03_tot)
tb1<-gamm4_excess_func("diz03","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$diz03_tot)
tb2<-gamm4_excess_func("diz03","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$diz03_tot)
tb3<-gamm4_excess_func("diz03","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$diz03_tot)
tb4<-gamm4_excess_func("diz03","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$diz03_tot)
tb5<-gamm4_excess_func("diz03","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$diz03_tot)
tb6<-gamm4_excess_func("diz03","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$diz03_tot)
tb7<-gamm4_excess_func("diz03","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$diz03_tot)

tb_diz03_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("diz03","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$diz03_tot)
tb1<-gamm4_excess_func("diz03","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$diz03_tot)
tb2<-gamm4_excess_func("diz03","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$diz03_tot)
tb3<-gamm4_excess_func("diz03","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$diz03_tot)
tb4<-gamm4_excess_func("diz03","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$diz03_tot)
tb5<-gamm4_excess_func("diz03","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$diz03_tot)
tb6<-gamm4_excess_func("diz03","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$diz03_tot)
tb7<-gamm4_excess_func("diz03","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$diz03_tot)

tb_diz03_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#어지러움:입원 또는 외래 
tb0<-gamm4_excess_func("diz0203","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$diz0203_tot)
tb1<-gamm4_excess_func("diz0203","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$diz0203_tot)
tb2<-gamm4_excess_func("diz0203","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$diz0203_tot)
tb3<-gamm4_excess_func("diz0203","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$diz0203_tot)
tb4<-gamm4_excess_func("diz0203","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$diz0203_tot)
tb5<-gamm4_excess_func("diz0203","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$diz0203_tot)
tb6<-gamm4_excess_func("diz0203","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$diz0203_tot)
tb7<-gamm4_excess_func("diz0203","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$diz0203_tot)

tb_diz0203_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("diz0203","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$diz0203_tot)
tb1<-gamm4_excess_func("diz0203","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$diz0203_tot)
tb2<-gamm4_excess_func("diz0203","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$diz0203_tot)
tb3<-gamm4_excess_func("diz0203","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$diz0203_tot)
tb4<-gamm4_excess_func("diz0203","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$diz0203_tot)
tb5<-gamm4_excess_func("diz0203","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$diz0203_tot)
tb6<-gamm4_excess_func("diz0203","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$diz0203_tot)
tb7<-gamm4_excess_func("diz0203","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$diz0203_tot)

tb_diz0203_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)
#------------------------------------------------------------------------------#
#출혈성 뇌졸중: 입원 
tb0<-gamm4_excess_func("hemo02","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$hemo02_tot)
tb1<-gamm4_excess_func("hemo02","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$hemo02_tot)
tb2<-gamm4_excess_func("hemo02","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$hemo02_tot)
tb3<-gamm4_excess_func("hemo02","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$hemo02_tot)
tb4<-gamm4_excess_func("hemo02","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$hemo02_tot)
tb5<-gamm4_excess_func("hemo02","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$hemo02_tot)
tb6<-gamm4_excess_func("hemo02","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$hemo02_tot)
tb7<-gamm4_excess_func("hemo02","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$hemo02_tot)

tb_hemo02_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("hemo02","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$hemo02_tot)
tb1<-gamm4_excess_func("hemo02","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$hemo02_tot)
tb2<-gamm4_excess_func("hemo02","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$hemo02_tot)
tb3<-gamm4_excess_func("hemo02","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$hemo02_tot)
tb4<-gamm4_excess_func("hemo02","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$hemo02_tot)
tb5<-gamm4_excess_func("hemo02","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$hemo02_tot)
tb6<-gamm4_excess_func("hemo02","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$hemo02_tot)
tb7<-gamm4_excess_func("hemo02","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$hemo02_tot)

tb_hemo02_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#출혈성 뇌졸중:외래 
tb0<-gamm4_excess_func("hemo03","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$hemo03_tot)
tb1<-gamm4_excess_func("hemo03","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$hemo03_tot)
tb2<-gamm4_excess_func("hemo03","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$hemo03_tot)
tb3<-gamm4_excess_func("hemo03","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$hemo03_tot)
tb4<-gamm4_excess_func("hemo03","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$hemo03_tot)
tb5<-gamm4_excess_func("hemo03","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$hemo03_tot)
tb6<-gamm4_excess_func("hemo03","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$hemo03_tot)
tb7<-gamm4_excess_func("hemo03","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$hemo03_tot)

tb_hemo03_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("hemo03","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$hemo03_tot)
tb1<-gamm4_excess_func("hemo03","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$hemo03_tot)
tb2<-gamm4_excess_func("hemo03","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$hemo03_tot)
tb3<-gamm4_excess_func("hemo03","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$hemo03_tot)
tb4<-gamm4_excess_func("hemo03","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$hemo03_tot)
tb5<-gamm4_excess_func("hemo03","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$hemo03_tot)
tb6<-gamm4_excess_func("hemo03","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$hemo03_tot)
tb7<-gamm4_excess_func("hemo03","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$hemo03_tot)

tb_hemo03_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#출혈성 뇌졸중:입원 또는 외래 
tb0<-gamm4_excess_func("hemo0203","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$hemo0203_tot)
tb1<-gamm4_excess_func("hemo0203","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$hemo0203_tot)
tb2<-gamm4_excess_func("hemo0203","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$hemo0203_tot)
tb3<-gamm4_excess_func("hemo0203","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$hemo0203_tot)
tb4<-gamm4_excess_func("hemo0203","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$hemo0203_tot)
tb5<-gamm4_excess_func("hemo0203","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$hemo0203_tot)
tb6<-gamm4_excess_func("hemo0203","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$hemo0203_tot)
tb7<-gamm4_excess_func("hemo0203","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$hemo0203_tot)

tb_hemo0203_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("hemo0203","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$hemo0203_tot)
tb1<-gamm4_excess_func("hemo0203","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$hemo0203_tot)
tb2<-gamm4_excess_func("hemo0203","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$hemo0203_tot)
tb3<-gamm4_excess_func("hemo0203","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$hemo0203_tot)
tb4<-gamm4_excess_func("hemo0203","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$hemo0203_tot)
tb5<-gamm4_excess_func("hemo0203","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$hemo0203_tot)
tb6<-gamm4_excess_func("hemo0203","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$hemo0203_tot)
tb7<-gamm4_excess_func("hemo0203","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$hemo0203_tot)

tb_hemo0203_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#심부전: 입원 
tb0<-gamm4_excess_func("hf02","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$hf02_tot)
tb1<-gamm4_excess_func("hf02","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$hf02_tot)
tb2<-gamm4_excess_func("hf02","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$hf02_tot)
tb3<-gamm4_excess_func("hf02","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$hf02_tot)
tb4<-gamm4_excess_func("hf02","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$hf02_tot)
tb5<-gamm4_excess_func("hf02","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$hf02_tot)
tb6<-gamm4_excess_func("hf02","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$hf02_tot)
tb7<-gamm4_excess_func("hf02","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$hf02_tot)

tb_hf02_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("hf02","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$hf02_tot)
tb1<-gamm4_excess_func("hf02","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$hf02_tot)
tb2<-gamm4_excess_func("hf02","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$hf02_tot)
tb3<-gamm4_excess_func("hf02","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$hf02_tot)
tb4<-gamm4_excess_func("hf02","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$hf02_tot)
tb5<-gamm4_excess_func("hf02","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$hf02_tot)
tb6<-gamm4_excess_func("hf02","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$hf02_tot)
tb7<-gamm4_excess_func("hf02","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$hf02_tot)

tb_hf02_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#심부전:외래 
tb0<-gamm4_excess_func("hf03","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$hf03_tot)
tb1<-gamm4_excess_func("hf03","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$hf03_tot)
tb2<-gamm4_excess_func("hf03","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$hf03_tot)
tb3<-gamm4_excess_func("hf03","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$hf03_tot)
tb4<-gamm4_excess_func("hf03","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$hf03_tot)
tb5<-gamm4_excess_func("hf03","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$hf03_tot)
tb6<-gamm4_excess_func("hf03","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$hf03_tot)
tb7<-gamm4_excess_func("hf03","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$hf03_tot)

tb_hf03_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("hf03","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$hf03_tot)
tb1<-gamm4_excess_func("hf03","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$hf03_tot)
tb2<-gamm4_excess_func("hf03","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$hf03_tot)
tb3<-gamm4_excess_func("hf03","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$hf03_tot)
tb4<-gamm4_excess_func("hf03","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$hf03_tot)
tb5<-gamm4_excess_func("hf03","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$hf03_tot)
tb6<-gamm4_excess_func("hf03","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$hf03_tot)
tb7<-gamm4_excess_func("hf03","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$hf03_tot)

tb_hf03_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#심부전:입원 또는 외래 
tb0<-gamm4_excess_func("hf0203","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$hf0203_tot)
tb1<-gamm4_excess_func("hf0203","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$hf0203_tot)
tb2<-gamm4_excess_func("hf0203","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$hf0203_tot)
tb3<-gamm4_excess_func("hf0203","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$hf0203_tot)
tb4<-gamm4_excess_func("hf0203","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$hf0203_tot)
tb5<-gamm4_excess_func("hf0203","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$hf0203_tot)
tb6<-gamm4_excess_func("hf0203","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$hf0203_tot)
tb7<-gamm4_excess_func("hf0203","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$hf0203_tot)

tb_hf0203_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("hf0203","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$hf0203_tot)
tb1<-gamm4_excess_func("hf0203","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$hf0203_tot)
tb2<-gamm4_excess_func("hf0203","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$hf0203_tot)
tb3<-gamm4_excess_func("hf0203","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$hf0203_tot)
tb4<-gamm4_excess_func("hf0203","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$hf0203_tot)
tb5<-gamm4_excess_func("hf0203","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$hf0203_tot)
tb6<-gamm4_excess_func("hf0203","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$hf0203_tot)
tb7<-gamm4_excess_func("hf0203","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$hf0203_tot)

tb_hf0203_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)


#------------------------------------------------------------------------------#
#고혈압: 입원 
tb0<-gamm4_excess_func("htn02","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$htn02_tot)
tb1<-gamm4_excess_func("htn02","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$htn02_tot)
tb2<-gamm4_excess_func("htn02","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$htn02_tot)
tb3<-gamm4_excess_func("htn02","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$htn02_tot)
tb4<-gamm4_excess_func("htn02","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$htn02_tot)
tb5<-gamm4_excess_func("htn02","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$htn02_tot)
tb6<-gamm4_excess_func("htn02","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$htn02_tot)
tb7<-gamm4_excess_func("htn02","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$htn02_tot)

tb_htn02_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("htn02","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$htn02_tot)
tb1<-gamm4_excess_func("htn02","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$htn02_tot)
tb2<-gamm4_excess_func("htn02","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$htn02_tot)
tb3<-gamm4_excess_func("htn02","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$htn02_tot)
tb4<-gamm4_excess_func("htn02","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$htn02_tot)
tb5<-gamm4_excess_func("htn02","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$htn02_tot)
tb6<-gamm4_excess_func("htn02","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$htn02_tot)
tb7<-gamm4_excess_func("htn02","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$htn02_tot)

tb_htn02_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#허혈성심장질환: 입원 
tb0<-gamm4_excess_func("ihd02","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$ihd02_tot)
tb1<-gamm4_excess_func("ihd02","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$ihd02_tot)
tb2<-gamm4_excess_func("ihd02","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$ihd02_tot)
tb3<-gamm4_excess_func("ihd02","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$ihd02_tot)
tb4<-gamm4_excess_func("ihd02","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$ihd02_tot)
tb5<-gamm4_excess_func("ihd02","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$ihd02_tot)
tb6<-gamm4_excess_func("ihd02","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$ihd02_tot)
tb7<-gamm4_excess_func("ihd02","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$ihd02_tot)

tb_ihd02_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("ihd02","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$ihd02_tot)
tb1<-gamm4_excess_func("ihd02","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$ihd02_tot)
tb2<-gamm4_excess_func("ihd02","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$ihd02_tot)
tb3<-gamm4_excess_func("ihd02","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$ihd02_tot)
tb4<-gamm4_excess_func("ihd02","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$ihd02_tot)
tb5<-gamm4_excess_func("ihd02","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$ihd02_tot)
tb6<-gamm4_excess_func("ihd02","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$ihd02_tot)
tb7<-gamm4_excess_func("ihd02","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$ihd02_tot)

tb_ihd02_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#허혈성 뇌졸중: 입원 
tb0<-gamm4_excess_func("isch02","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$isch02_tot)
tb1<-gamm4_excess_func("isch02","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$isch02_tot)
tb2<-gamm4_excess_func("isch02","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$isch02_tot)
tb3<-gamm4_excess_func("isch02","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$isch02_tot)
tb4<-gamm4_excess_func("isch02","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$isch02_tot)
tb5<-gamm4_excess_func("isch02","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$isch02_tot)
tb6<-gamm4_excess_func("isch02","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$isch02_tot)
tb7<-gamm4_excess_func("isch02","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$isch02_tot)

tb_isch02_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("isch02","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$isch02_tot)
tb1<-gamm4_excess_func("isch02","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$isch02_tot)
tb2<-gamm4_excess_func("isch02","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$isch02_tot)
tb3<-gamm4_excess_func("isch02","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$isch02_tot)
tb4<-gamm4_excess_func("isch02","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$isch02_tot)
tb5<-gamm4_excess_func("isch02","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$isch02_tot)
tb6<-gamm4_excess_func("isch02","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$isch02_tot)
tb7<-gamm4_excess_func("isch02","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$isch02_tot)

tb_isch02_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#허혈성 뇌졸중:외래 
tb0<-gamm4_excess_func("isch03","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$isch03_tot)
tb1<-gamm4_excess_func("isch03","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$isch03_tot)
tb2<-gamm4_excess_func("isch03","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$isch03_tot)
tb3<-gamm4_excess_func("isch03","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$isch03_tot)
tb4<-gamm4_excess_func("isch03","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$isch03_tot)
tb5<-gamm4_excess_func("isch03","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$isch03_tot)
tb6<-gamm4_excess_func("isch03","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$isch03_tot)
tb7<-gamm4_excess_func("isch03","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$isch03_tot)

tb_isch03_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("isch03","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$isch03_tot)
tb1<-gamm4_excess_func("isch03","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$isch03_tot)
tb2<-gamm4_excess_func("isch03","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$isch03_tot)
tb3<-gamm4_excess_func("isch03","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$isch03_tot)
tb4<-gamm4_excess_func("isch03","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$isch03_tot)
tb5<-gamm4_excess_func("isch03","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$isch03_tot)
tb6<-gamm4_excess_func("isch03","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$isch03_tot)
tb7<-gamm4_excess_func("isch03","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$isch03_tot)

tb_isch03_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#허혈성 뇌졸중:입원 또는 외래 
tb0<-gamm4_excess_func("isch0203","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$isch0203_tot)
tb1<-gamm4_excess_func("isch0203","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$isch0203_tot)
tb2<-gamm4_excess_func("isch0203","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$isch0203_tot)
tb3<-gamm4_excess_func("isch0203","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$isch0203_tot)
tb4<-gamm4_excess_func("isch0203","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$isch0203_tot)
tb5<-gamm4_excess_func("isch0203","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$isch0203_tot)
tb6<-gamm4_excess_func("isch0203","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$isch0203_tot)
tb7<-gamm4_excess_func("isch0203","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$isch0203_tot)

tb_isch0203_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("isch0203","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$isch0203_tot)
tb1<-gamm4_excess_func("isch0203","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$isch0203_tot)
tb2<-gamm4_excess_func("isch0203","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$isch0203_tot)
tb3<-gamm4_excess_func("isch0203","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$isch0203_tot)
tb4<-gamm4_excess_func("isch0203","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$isch0203_tot)
tb5<-gamm4_excess_func("isch0203","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$isch0203_tot)
tb6<-gamm4_excess_func("isch0203","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$isch0203_tot)
tb7<-gamm4_excess_func("isch0203","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$isch0203_tot)

tb_isch0203_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#후각장애:외래 
tb0<-gamm4_excess_func("olfa03","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$olfa03_tot)
tb1<-gamm4_excess_func("olfa03","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$olfa03_tot)
tb2<-gamm4_excess_func("olfa03","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$olfa03_tot)
tb3<-gamm4_excess_func("olfa03","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$olfa03_tot)
tb4<-gamm4_excess_func("olfa03","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$olfa03_tot)
tb5<-gamm4_excess_func("olfa03","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$olfa03_tot)
tb6<-gamm4_excess_func("olfa03","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$olfa03_tot)
tb7<-gamm4_excess_func("olfa03","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$olfa03_tot)

tb_olfa03_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("olfa03","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$olfa03_tot)
tb1<-gamm4_excess_func("olfa03","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$olfa03_tot)
tb2<-gamm4_excess_func("olfa03","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$olfa03_tot)
tb3<-gamm4_excess_func("olfa03","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$olfa03_tot)
tb4<-gamm4_excess_func("olfa03","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$olfa03_tot)
tb5<-gamm4_excess_func("olfa03","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$olfa03_tot)
tb6<-gamm4_excess_func("olfa03","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$olfa03_tot)
tb7<-gamm4_excess_func("olfa03","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$olfa03_tot)

tb_olfa03_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

#------------------------------------------------------------------------------#
#중이염:입원

tb0<-gamm4_excess_func("om02","PM2.5",dat$pm25_m0_diff,dat$pm25_new   ,1,dat$om02_tot)
tb1<-gamm4_excess_func("om02","PM2.5",dat$pm25_m1_diff,dat$pm25_new_m1,2,dat$om02_tot)
tb2<-gamm4_excess_func("om02","PM2.5",dat$pm25_m2_diff,dat$pm25_new_m2,3,dat$om02_tot)
tb3<-gamm4_excess_func("om02","PM2.5",dat$pm25_m3_diff,dat$pm25_new_m3,4,dat$om02_tot)
tb4<-gamm4_excess_func("om02","PM2.5",dat$pm25_m4_diff,dat$pm25_new_m4,5,dat$om02_tot)
tb5<-gamm4_excess_func("om02","PM2.5",dat$pm25_m5_diff,dat$pm25_new_m5,6,dat$om02_tot)
tb6<-gamm4_excess_func("om02","PM2.5",dat$pm25_m6_diff,dat$pm25_new_m6,7,dat$om02_tot)
tb7<-gamm4_excess_func("om02","PM2.5",dat$pm25_m7_diff,dat$pm25_new_m7,8,dat$om02_tot)

tb_om02_pm25<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)

tb0<-gamm4_excess_func("om02","PM10",dat$pm10_m0_diff,dat$pm10   ,1,dat$om02_tot)
tb1<-gamm4_excess_func("om02","PM10",dat$pm10_m1_diff,dat$pm10_m1,2,dat$om02_tot)
tb2<-gamm4_excess_func("om02","PM10",dat$pm10_m2_diff,dat$pm10_m2,3,dat$om02_tot)
tb3<-gamm4_excess_func("om02","PM10",dat$pm10_m3_diff,dat$pm10_m3,4,dat$om02_tot)
tb4<-gamm4_excess_func("om02","PM10",dat$pm10_m4_diff,dat$pm10_m4,5,dat$om02_tot)
tb5<-gamm4_excess_func("om02","PM10",dat$pm10_m5_diff,dat$pm10_m5,6,dat$om02_tot)
tb6<-gamm4_excess_func("om02","PM10",dat$pm10_m6_diff,dat$pm10_m6,7,dat$om02_tot)
tb7<-gamm4_excess_func("om02","PM10",dat$pm10_m7_diff,dat$pm10_m7,8,dat$om02_tot)

tb_om02_pm10<-rbind(tb0,tb1,tb2,tb3,tb4,tb5,tb6,tb7)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
tb_astm<-as.data.frame(rbind(tb_astm02_pm25  ,tb_astm02_pm10,
                             tb_astm03_pm25  ,tb_astm03_pm10,
                             tb_astm0203_pm25,tb_astm0203_pm10))

tb_cvd<-as.data.frame(rbind(tb_cvd02_pm25,tb_cvd02_pm10))

tb_diz<-as.data.frame(rbind(tb_diz02_pm25  ,tb_diz02_pm10,
                            tb_diz03_pm25  ,tb_diz03_pm10,
                            tb_diz0203_pm25,tb_diz0203_pm10))

tb_hemo<-as.data.frame(rbind(tb_hemo02_pm25  ,tb_hemo02_pm10,
                             tb_hemo03_pm25  ,tb_hemo03_pm10,
                             tb_hemo0203_pm25,tb_hemo0203_pm10))

tb_hf<-as.data.frame(rbind(tb_hf02_pm25  ,tb_hf02_pm10,
                           tb_hf03_pm25  ,tb_hf03_pm10,
                           tb_hf0203_pm25,tb_hf0203_pm10))

tb_htn<-as.data.frame(rbind(tb_htn02_pm25,tb_htn02_pm10))
tb_ihd<-as.data.frame(rbind(tb_ihd02_pm25,tb_ihd02_pm10))

tb_isch<-as.data.frame(rbind(tb_isch02_pm25  ,tb_isch02_pm10,
                             tb_isch03_pm25  ,tb_isch03_pm10,
                             tb_isch0203_pm25,tb_isch0203_pm10))

tb_olfa<-as.data.frame(rbind(tb_olfa03_pm25,tb_olfa03_pm10))
tb_om  <-as.data.frame(rbind(tb_om02_pm25,tb_om02_pm10))


setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result\\NHIS_gamm4_linear_city")
write.csv(tb_astm,file="tb_astm.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_cvd ,file="tb_cvd.csv" ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_diz ,file="tb_diz.csv" ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_hemo,file="tb_hemo.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_hf  ,file="tb_hf.csv"  ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_htn ,file="tb_htn.csv" ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_ihd ,file="tb_ihd.csv" ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_isch,file="tb_isch.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_olfa,file="tb_olfa.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(tb_om  ,file="tb_om.csv"  ,row.names=F,na="",fileEncoding = "euc-kr")
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

summ_yr_astm<-tb_astm %>% group_by(outcome,exposure,lag,year) %>% 
  summarise(e_dth    =sum(e_dth)  ,e_dth_lci=sum(e_dth_lci)  ,e_dth_uci=sum(e_dth_uci),
            e_dth2    =sum(e_dth2),e_dth_lci2=sum(e_dth_lci2),e_dth_uci2=sum(e_dth_uci2))

summ_yr_cvd<-tb_cvd %>% group_by(outcome,exposure,lag,year) %>% 
  summarise(e_dth    =sum(e_dth)  ,e_dth_lci=sum(e_dth_lci)  ,e_dth_uci=sum(e_dth_uci),
            e_dth2    =sum(e_dth2),e_dth_lci2=sum(e_dth_lci2),e_dth_uci2=sum(e_dth_uci2))

summ_yr_diz<-tb_diz %>% group_by(outcome,exposure,lag,year) %>% 
  summarise(e_dth    =sum(e_dth)  ,e_dth_lci=sum(e_dth_lci)  ,e_dth_uci=sum(e_dth_uci),
            e_dth2    =sum(e_dth2),e_dth_lci2=sum(e_dth_lci2),e_dth_uci2=sum(e_dth_uci2))

summ_yr_hemo<-tb_hemo %>% group_by(outcome,exposure,lag,year) %>% 
  summarise(e_dth    =sum(e_dth)  ,e_dth_lci=sum(e_dth_lci)  ,e_dth_uci=sum(e_dth_uci),
            e_dth2    =sum(e_dth2),e_dth_lci2=sum(e_dth_lci2),e_dth_uci2=sum(e_dth_uci2))

summ_yr_hf<-tb_hf %>% group_by(outcome,exposure,lag,year) %>% 
  summarise(e_dth    =sum(e_dth)  ,e_dth_lci=sum(e_dth_lci)  ,e_dth_uci=sum(e_dth_uci),
            e_dth2    =sum(e_dth2),e_dth_lci2=sum(e_dth_lci2),e_dth_uci2=sum(e_dth_uci2))

summ_yr_htn<-tb_htn %>% group_by(outcome,exposure,lag,year) %>% 
  summarise(e_dth    =sum(e_dth)  ,e_dth_lci=sum(e_dth_lci)  ,e_dth_uci=sum(e_dth_uci),
            e_dth2    =sum(e_dth2),e_dth_lci2=sum(e_dth_lci2),e_dth_uci2=sum(e_dth_uci2))

summ_yr_ihd<-tb_ihd %>% group_by(outcome,exposure,lag,year) %>% 
  summarise(e_dth    =sum(e_dth)  ,e_dth_lci=sum(e_dth_lci)  ,e_dth_uci=sum(e_dth_uci),
            e_dth2    =sum(e_dth2),e_dth_lci2=sum(e_dth_lci2),e_dth_uci2=sum(e_dth_uci2))

summ_yr_isch<-tb_isch %>% group_by(outcome,exposure,lag,year) %>% 
  summarise(e_dth    =sum(e_dth)  ,e_dth_lci=sum(e_dth_lci)  ,e_dth_uci=sum(e_dth_uci),
            e_dth2    =sum(e_dth2),e_dth_lci2=sum(e_dth_lci2),e_dth_uci2=sum(e_dth_uci2))

summ_yr_olfa<-tb_olfa %>% group_by(outcome,exposure,lag,year) %>% 
  summarise(e_dth    =sum(e_dth)  ,e_dth_lci=sum(e_dth_lci)  ,e_dth_uci=sum(e_dth_uci),
            e_dth2    =sum(e_dth2),e_dth_lci2=sum(e_dth_lci2),e_dth_uci2=sum(e_dth_uci2))

summ_yr_om<-tb_om %>% group_by(outcome,exposure,lag,year) %>% 
  summarise(e_dth    =sum(e_dth)  ,e_dth_lci=sum(e_dth_lci)  ,e_dth_uci=sum(e_dth_uci),
            e_dth2    =sum(e_dth2),e_dth_lci2=sum(e_dth_lci2),e_dth_uci2=sum(e_dth_uci2))

setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result\\NHIS_gamm4_linear_yr")
write.csv(summ_yr_astm,file="summ_yr_astm.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(summ_yr_cvd ,file="summ_yr_cvd.csv" ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(summ_yr_diz ,file="summ_yr_diz.csv" ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(summ_yr_hemo,file="summ_yr_hemo.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(summ_yr_hf  ,file="summ_yr_hf.csv"  ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(summ_yr_htn ,file="summ_yr_htn.csv" ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(summ_yr_ihd ,file="summ_yr_ihd.csv" ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(summ_yr_isch,file="summ_yr_isch.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(summ_yr_olfa,file="summ_yr_olfa.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(summ_yr_om  ,file="summ_yr_om.csv"  ,row.names=F,na="",fileEncoding = "euc-kr")

#----------------------------------------------------------------------#
#연도별 질환 빈도
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result")
aggregate(astm02_tot~year,data=dat,sum)
aggregate(astm02_tot~year,data=dat,sum)[,2] %>% sum
aggregate(astm03_tot~year,data=dat,sum)
aggregate(astm03_tot~year,data=dat,sum)[,2] %>% sum
aggregate(astm0203_tot~year,data=dat,sum)
aggregate(astm0203_tot~year,data=dat,sum)[,2] %>% sum

aggregate(cvd02_tot~year,data=dat,sum)
aggregate(cvd02_tot~year,data=dat,sum)[,2] %>% sum

aggregate(diz02_tot~year,data=dat,sum)
aggregate(diz02_tot~year,data=dat,sum)[,2] %>% sum
aggregate(diz03_tot~year,data=dat,sum)
aggregate(diz03_tot~year,data=dat,sum)[,2] %>% sum
aggregate(diz0203_tot~year,data=dat,sum)
aggregate(diz0203_tot~year,data=dat,sum)[,2] %>% sum

z<-cbind(aggregate(hemo02_tot~year,data=dat,sum),
         "03"=aggregate(hemo03_tot~year,data=dat,sum)[,2],
         "0203"=aggregate(hemo0203_tot~year,data=dat,sum)[,2])

write.csv(z,file="z.csv",row.names=F,na="",fileEncoding = "euc-kr")

z<-cbind(aggregate(hf02_tot~year,data=dat,sum),
         "03"=aggregate(hf03_tot~year,data=dat,sum)[,2],
         "0203"=aggregate(hf0203_tot~year,data=dat,sum)[,2])

write.csv(z,file="z.csv",row.names=F,na="",fileEncoding = "euc-kr")

aggregate(htn02_tot~year,data=dat,sum)
aggregate(ihd02_tot~year,data=dat,sum)

z<-cbind(aggregate(isch02_tot~year,data=dat,sum),
         "03"=aggregate(isch03_tot~year,data=dat,sum)[,2],
         "0203"=aggregate(isch0203_tot~year,data=dat,sum)[,2])

write.csv(z,file="z.csv",row.names=F,na="",fileEncoding = "euc-kr")


aggregate(olfa03_tot~year,data=dat,sum)
aggregate(om02_tot~year,data=dat,sum)
