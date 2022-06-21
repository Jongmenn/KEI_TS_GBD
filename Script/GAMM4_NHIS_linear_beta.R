#-------------------------------------------------------------#
##################KEI-환경보건감시체계과제#####################
#-------------------------------------------------------------#
#대기오염 노출로 인한 건강영향 평가, 질병부담 산정
#자료원: 2021년 공단 반출자료 
#--------------------------------------------------------------------------------------------#
#라이브러리 
#Heat 패키지 cran에서 없어서 저장되어있는거 긁어다가 쓰기 
pacman::p_load(gamm4,mgcv,lme4,splines,ggplot2,gridExtra,dplyr,lubridate,scales,RColorBrewer,readxl)
#--------------------------------------------------------------------------------------------#
#지역코드 & 레이블
#통계청 사망자료 지역코드 
sidolist_stat=c(11,21,22,23,24,25,26,29,31,32,33,34,35,36,37,38,39)
#공단자료 지역코드
sidolist_nhis=c(11,26,27,28,29,30,31,36,41,42,43,44,45,46,47,48,49)

#17개 시도 영문
sidoname=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan",
           "Sejong","KK", "KW", "CB", "CN", "JB", "JN", "KB", "KN", "JJ")

#17개 시도 한글
area=c("서울","부산","대구","인천","광주","대전","울산","세종","경기",
       "강원","충북","충남","전북","전남","경북","경남","제주")

#지역코드 데이터프레임
sido_df=data.frame(sidolist_stat,sidolist_nhis,sidoname,area)
#--------------------------------------------------------------------------------------------#
#통계청 일일 집계 자료+기상자료 정리한 파일에서 기상 변수만 가져오기 
setwd("D:\\SNU\\연구\\질병관리본부\\기후보건영향평가_평가체계구축및시범사업\\2022\\자료\\통계청\\version")
ap<-read.csv("통계청사망_dailycount_2001_2020_ver0.csv",fileEncoding = "euc-kr") %>% select(ddate:EN_SIDO,mintemp:pm25_model)

ap_wea<-ap %>% left_join(sido_df,by="area") %>% select(ddate:EN_SIDO,sidolist_stat:sidoname,mintemp:pm25_model)

#2015년 이전 PM2.5 노출 자료+2015년 이후는 모니터링 자료 이용 
ap_wea$pm25_new=with(ap_wea,ifelse(year>2015,pm25,pm25_model))

#변수들 추가 정리 
#compute simple at (schwartz version) 
ap_wea$simpat     =with(ap_wea,-2.653 + (0.994*meantemp) + (0.0153*dewtemp*dewtemp))
ap_wea$sn         =ap_wea$ddd             #도시별 serial number
ap_wea$dow        =as.numeric(ap_wea$dow)
ap_wea$dow_fac    =as.factor(ap_wea$dow)
ap_wea$firstday   =ifelse(substr(ap_wea$ddate,9,10)=="01", 1, 0)
ap_wea$dowfirstday=ifelse(substr(ap_wea$ddate,9,10)=="01" | ap_wea$dow==2, 2, ap_wea$dow)

#통계청 사망자료에서 이용한, lagdata.func() 함수 조금 변형해서 사용함

lagdata.func<-function(data,sido,city,var,lag){
  
  #Input data
  dat     =ap_wea  #원자료 무엇인지?
  #전체 자료에서 특정 도시와 단일지연을 생성한 변수 선택
  subdat<-subset(dat,area==city) %>% select(var)
  
  #지정한 lag days 만큼 생성 (단일지연)
  lag.list=NULL
  for(i in 1:lag){
    lag.list[[i]] <-subdat %>% lag(i)
  }
  
  #생성한 열로 묶음 
  lag_exposure<-as.data.frame(do.call(cbind,lag.list))
  
  #입력한 노출 변수가 몇개인지? 
  var_length=length(var)  
  
  #노출자료 변수명 생성 lag(단일지연)한 결과별로
  text1<-rep(var,lag)
  text2<-rep(1:lag,each=var_length) 
  names(lag_exposure)=paste0(text1,"_s",text2)
  
  #단일지연한 노출 자료 이용해서 이동평균 자료 생성
  final_exp_list=NULL
  for(j in 1:length(var)){
    
    #당일 노출 자료
    currentday<-subdat %>% select(var[j])
    
    #단일 지연 노출 자료 
    single_lag<-lag_exposure[grep(var[j],names(lag_exposure))]
    
    #이동평균 자료 저장할 리스트 
    moving.list=NULL
    
    #지연일 만큼 이동평균 자료 생성
    #lag01 (m1) = 당일과 하루전 노출 평균자료
    #lag021(m21)= 당일과 21일전 노출 평균자료 
    for(k in 1:lag){
      #이동평균(Moving avergae)생성, 결측자료 있을시, 평균 낼지 말지?
      #우선 내자...
      moving.list[[k]]<-apply(cbind(currentday,single_lag[1:k]),1,mean,na.rm=T)
    }
    moving_lag=as.data.frame(do.call(cbind,moving.list))
    names(moving_lag)=paste0(rep(var[j],lag),"_m",1:lag)
    
    final_exp_list[[j]]<-cbind(single_lag,moving_lag)
  }
  #최종 노출자료(단일지연, 이동평균) 생성
  final_exp<-do.call(cbind,final_exp_list)
  
  #도시별 최종노출자료 붙인 자료 출력 
  cbind(subset(ap_wea,area==city),final_exp)
}

#함수 출력 예시
#전체 시도중 서울에서 meantemp~o3 해당하는 노출자료를 최대 지연일 21일까지 고려하여 생성

lagdata.func(ap_wea,
             ap_wea$area,
             "서울",
             c("meantemp","meanhumi","simpat","dewtemp",
               "pm25_new","pm10","so2","no2","co","o3"),21)

#도시별로 함수적용해서 출력결과 merge 하기 
sido_df$sido_KN

lagdata_city_list=NULL
for(i in 1:length(sido_df$area)){
  lagdata_city_list[[i]]<-lagdata.func(ap_wea,
                                       ap_wea$area,
                                       sido_df$area[i],
                                       c("meantemp","meanhumi","simpat","dewtemp",
                                         "pm25_new","pm10","so2","no2","co","o3"),21)
  print(i)}


#노출 시도별 단일지연, 이동평균 만든자료, 연도 2006~2019로 제한 (공단자료랑 맞추려고)
ap_wea2<-do.call(rbind,lagdata_city_list) %>%  filter(year %in% c(2006:2019))

#Jeju 49 ->50으로 변경 
ap_wea2$sidolist_nhis=ifelse(ap_wea2$sidolist_nhis==49,50,ap_wea2$sidolist_nhis)

table(ap_wea2$sidolist_nhis)
# write.csv(ap_wea2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\ap_revise0619.csv",row.names=F,na="")

ap_revise<-read.csv("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\ap_revise0619.csv")
table(ap_revise$sidolist_nhis)
ap_revise$key=with(ap_revise,paste0(sidolist_nhis,"-",ddate))

#--------------------------------------------------------------------------------------------#
#자료 검토해보면, 시도가 17개 아니라 18개 시도로 되어있는 자료 존재
#49->50 제주 인거 같은데... 동일한 날짜, 예를들면 2006년 1월 1일에 49, 50 자료 둘다 카운트 되어있어서..
#그냥 합치기 좀그럼.. 
#코딩 오류로 간주하고 버리기...

#data load
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\자료\\KEI_서울대_건강자료(보안)\\시도별일별자료")

a01<-read.csv("ASTM_0619_sido_f02.csv")
a02<-read.csv("ASTM_0619_sido_f03.csv")
a03<-read.csv("ASTM_0619_sido_f0203.csv")
a04<-read.csv("CVD_0619_sido_f02.csv")
a05<-read.csv("DIZ_0619_sido_f02.csv")
a06<-read.csv("DIZ_0619_sido_f03.csv")
a07<-read.csv("DIZ_0619_sido_f0203.csv")
a08<-read.csv("HEMO_0619_sido_f02.csv")
a09<-read.csv("HEMO_0619_sido_f03.csv")
a10<-read.csv("HEMO_0619_sido_f0203.csv")
a11<-read.csv("HF_0619_sido_f02.csv")
a12<-read.csv("HF_0619_sido_f03.csv")
a13<-read.csv("HF_0619_sido_f0203.csv")
a14<-read.csv("HTN_0619_sido_f02.csv")
a15<-read.csv("IHD_0619_sido_f02.csv")
a16<-read.csv("ISCH_0619_sido_f02.csv")
a17<-read.csv("ISCH_0619_sido_f03.csv")
a18<-read.csv("ISCH_0619_sido_f0203.csv")
a19<-read.csv("OLFA_0619_sido_f03.csv")
a20<-read.csv("OM_0619_sido_f02.csv")


#시도별, 집계해서 "49"인 지역 총계 몇인지? 
agg_tot<-function(data,text){
  d<-data
  d$tot=d[,4]
  d$sido=substr(d$COKEY,1,2)
  res<-as.data.frame(aggregate(tot~sido,data=d,sum))
  names(res)[2]=text
  res
}

list.files()
agg_res<-agg_tot(a01,"astm02") %>% 
  left_join(agg_tot(a02,"astm03")  ,by="sido") %>% left_join(agg_tot(a03,"astm0203"),by="sido") %>% 
  left_join(agg_tot(a04,"cvd02")   ,by="sido") %>% left_join(agg_tot(a05,"diz02")   ,by="sido") %>% 
  left_join(agg_tot(a06,"diz03")   ,by="sido") %>% left_join(agg_tot(a07,"diz0203") ,by="sido") %>% 
  left_join(agg_tot(a08,"hemo02")  ,by="sido") %>% left_join(agg_tot(a09,"hemo03")  ,by="sido") %>% 
  left_join(agg_tot(a10,"hemo0203"),by="sido") %>% left_join(agg_tot(a11,"hf02")    ,by="sido") %>% 
  left_join(agg_tot(a12,"hf03")    ,by="sido") %>% left_join(agg_tot(a13,"hf0203")  ,by="sido") %>% 
  left_join(agg_tot(a14,"htn02")   ,by="sido") %>% left_join(agg_tot(a15,"ihd02")   ,by="sido") %>% 
  left_join(agg_tot(a16,"isch02")  ,by="sido") %>% left_join(agg_tot(a17,"isch03")  ,by="sido") %>% 
  left_join(agg_tot(a18,"isch0203"),by="sido") %>% left_join(agg_tot(a19,"olfa03")  ,by="sido") %>% 
  left_join(agg_tot(a20,"om02")    ,by="sido")

# write.csv(agg_res,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\work\\agg_res.csv",row.names=F,na="")

#시도별 연도별로 봐서 49로 코딩된거 몇건인지? 
agg_yr<-function(data,text){
  d<-data
  d$tot=d[,4]
  d$sido=substr(d$COKEY,1,2)
  d$year=year(ymd(d$DATE))
  table(subset(d,sido=="49")$year)
}
agg_yr_table<-bind_rows(agg_yr(a01),agg_yr(a02),agg_yr(a03),agg_yr(a05),agg_yr(a06),agg_yr(a07),
                        agg_yr(a09),agg_yr(a10),agg_yr(a12),agg_yr(a13),agg_yr(a14),
                        agg_yr(a17),agg_yr(a18),agg_yr(a19),agg_yr(a20))


# write.csv(agg_yr_table,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\work\\agg_yr_table.csv",row.names=F,na="")

#--------------------------------------------------------------------------------------------#
dlabel<-c("tot","m","f","ag00","ag0014","ag1564","ag65","ag00_m","ag0014_m","ag1564_m","ag65_m","ag00_f","ag0014_f","ag1564_f","ag65_f")

a01.r<-a01[,c(1,4:18)];names(a01.r)=c("key",paste0("astm02_",dlabel))
a02.r<-a02[,c(1,4:18)];names(a02.r)=c("key",paste0("astm03_",dlabel))
a03.r<-a03[,c(1,4:18)];names(a03.r)=c("key",paste0("astm0203_",dlabel))
a04.r<-a04[,c(1,4:18)];names(a04.r)=c("key",paste0("cvd02_",dlabel))
a05.r<-a05[,c(1,4:18)];names(a05.r)=c("key",paste0("diz02_",dlabel))
a06.r<-a06[,c(1,4:18)];names(a06.r)=c("key",paste0("diz03_",dlabel))
a07.r<-a07[,c(1,4:18)];names(a07.r)=c("key",paste0("diz0203_",dlabel))
a08.r<-a08[,c(1,4:18)];names(a08.r)=c("key",paste0("hemo02_",dlabel))
a09.r<-a09[,c(1,4:18)];names(a09.r)=c("key",paste0("hemo03_",dlabel))
a10.r<-a10[,c(1,4:18)];names(a10.r)=c("key",paste0("hemo0203_",dlabel))
a11.r<-a11[,c(1,4:18)];names(a11.r)=c("key",paste0("hf02_",dlabel))
a12.r<-a12[,c(1,4:18)];names(a12.r)=c("key",paste0("hf03_",dlabel))
a13.r<-a13[,c(1,4:18)];names(a13.r)=c("key",paste0("hf0203_",dlabel))
a14.r<-a14[,c(1,4:18)];names(a14.r)=c("key",paste0("htn02_",dlabel))
a15.r<-a15[,c(1,4:18)];names(a15.r)=c("key",paste0("ihd02_",dlabel))
a16.r<-a16[,c(1,4:18)];names(a16.r)=c("key",paste0("isch02_",dlabel))
a17.r<-a17[,c(1,4:18)];names(a17.r)=c("key",paste0("isch03_",dlabel))
a18.r<-a18[,c(1,4:18)];names(a18.r)=c("key",paste0("isch0203_",dlabel))
a19.r<-a19[,c(1,4:18)];names(a19.r)=c("key",paste0("olfa03_",dlabel))
a20.r<-a20[,c(1,4:18)];names(a20.r)=c("key",paste0("om02_",dlabel))

#노출자료랑 건강보험공단 반출 자료(질환) 연계
z01<-ap_revise %>% left_join(a01.r,by="key")
z02<-ap_revise %>% left_join(a02.r,by="key")
z03<-ap_revise %>% left_join(a03.r,by="key")
z04<-ap_revise %>% left_join(a04.r,by="key")
z05<-ap_revise %>% left_join(a05.r,by="key")
z06<-ap_revise %>% left_join(a06.r,by="key")
z07<-ap_revise %>% left_join(a07.r,by="key")
z08<-ap_revise %>% left_join(a08.r,by="key")
z09<-ap_revise %>% left_join(a09.r,by="key")
z10<-ap_revise %>% left_join(a10.r,by="key")
z11<-ap_revise %>% left_join(a11.r,by="key")
z12<-ap_revise %>% left_join(a12.r,by="key")
z13<-ap_revise %>% left_join(a13.r,by="key")
z14<-ap_revise %>% left_join(a14.r,by="key")
z15<-ap_revise %>% left_join(a15.r,by="key")
z16<-ap_revise %>% left_join(a16.r,by="key")
z17<-ap_revise %>% left_join(a17.r,by="key")
z18<-ap_revise %>% left_join(a18.r,by="key")
z19<-ap_revise %>% left_join(a19.r,by="key")
z20<-ap_revise %>% left_join(a20.r,by="key")

names(z01)

#연계해서 해당일에 노출 값 없는 경우 0으로 메꿔주기 
z01[is.na(z01$astm02_tot)  ,459:473]<-0
z02[is.na(z02$astm03_tot)  ,459:473]<-0
z03[is.na(z03$astm0203_tot),459:473]<-0
z04[is.na(z04$cvd02_tot)   ,459:473]<-0
z05[is.na(z05$diz02_tot)   ,459:473]<-0
z06[is.na(z06$diz03_tot)   ,459:473]<-0
z07[is.na(z07$diz0203_tot) ,459:473]<-0
z08[is.na(z08$hemo02_tot)  ,459:473]<-0
z09[is.na(z09$hemo03_tot)  ,459:473]<-0
z10[is.na(z10$hemo0203_tot),459:473]<-0
z11[is.na(z11$hf02_tot)    ,459:473]<-0
z12[is.na(z12$hf03_tot)    ,459:473]<-0
z13[is.na(z13$hf0203_tot)  ,459:473]<-0
z14[is.na(z14$htn02_tot)   ,459:473]<-0
z15[is.na(z15$ihd02_tot)   ,459:473]<-0
z16[is.na(z16$isch02_tot)  ,459:473]<-0
z17[is.na(z17$isch03_tot)  ,459:473]<-0
z18[is.na(z18$isch0203_tot),459:473]<-0
z19[is.na(z19$olfa03_tot)  ,459:473]<-0
z20[is.na(z20$om02_tot)    ,459:473]<-0


head(daily_nhis_final)

daily_nhis_final<-cbind(z01,z02[,459:473],z03[,459:473],z04[,459:473],z05[,459:473],
                        z06[,459:473],z07[,459:473],z08[,459:473],z09[,459:473],
                        z10[,459:473],z11[,459:473],z12[,459:473],z13[,459:473],
                        z14[,459:473],z15[,459:473],z16[,459:473],z17[,459:473],
                        z18[,459:473],z19[,459:473],z20[,459:473]) %>% select(key,ddate:sidoname,astm02_tot:om02_ag65_f,mintemp:o3_m21)

setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담")
# write.csv(daily_nhis_final,file="daily_nhis_final.csv",row.names=F,na="")

#--------------------------------------------------------------------------------------------#
#자료 집계 함수 만들기 
#전체 자료에서 해당 사망원인에 해당하는 자료 추출 

agg_nhis_dis<-function(data,i){
  dd<-data
  dd$sido =substr(dd$COKEY,1,2)
  dd$year =year(ymd(substr(dd$COKEY,4,13)))
  dd$month=month(ymd(substr(dd$COKEY,4,13)))
  
  
  dd<-dd[,c(1,4:10,19:21)] %>% filter(sido!="49")
  
  #전체
  agg.t0<-aggregate(dd[,2],list(dd$year),sum) #전체
  #성별
  agg.t1<-aggregate(dd[,3],list(dd$year),sum) #남
  agg.t2<-aggregate(dd[,4],list(dd$year),sum) #여
  
  #연령그룹별
  agg.t3<-aggregate(dd[,5],list(dd$year),sum) #0세
  agg.t4<-aggregate(dd[,6],list(dd$year),sum) #15세 미만
  agg.t5<-aggregate(dd[,7],list(dd$year),sum) #15-64세
  agg.t6<-aggregate(dd[,8],list(dd$year),sum) #65세 이상
  
  #시도,월별 
  agg.t7<-aggregate(dd[,2],list(dd$year,dd$sido),sum) #전체, 시도별
  agg.t8<-aggregate(dd[,2],list(dd$year,dd$month),sum) #전체, 월별
  
  names(agg.t7)=c("year","sido","count")
  names(agg.t8)=c("year","month","count")
  
  #전체, 성별, 연령 그룹별은 변수 구성이 동일해서 그대로 merge
  agg.df<-as.data.frame(rbind(t(agg.t0$x),t(agg.t1$x),t(agg.t2$x),t(agg.t3$x),
                              t(agg.t4$x),t(agg.t5$x),t(agg.t6$x)))
  
  #전체, 성별, 연령그룹
  agg.df$categories=c("Total","M","F","AG0000","AG0015","AG1564","AG65+")
  agg.df<-agg.df %>% select(categories,V1:V14)
  names(agg.df)[2:length(agg.df)]=c(2006:2019)
  
  #시도,월별 
  agg.df2<-reshape2::dcast(agg.t7,sido~year) #시도별 연도별
  agg.df3<-reshape2::dcast(agg.t8,month~year)#월별 연도별
  
  #첫째열 동일하게 변경 
  names(agg.df2)[1]="categories"
  names(agg.df3)[1]="categories"
  
  #전체, 성별, 연령 그룹별, 시도, 월별 연도별 집계자료 
  agg.merge.tb<-rbind(agg.df,agg.df2,agg.df3)
  
  #질병명 붙여주기 
  nhis_diesease=c("astm02  ","astm03  ","astm0203",
                  "cvd02   ",
                  "diz02   ","diz03   ","diz0203 ",
                  "hemo02  ","hemo03  ","hemo0203",
                  "hf02    ","hf03    ","hf0203  ",
                  "htn02   ",
                  "ihd02   ",
                  "isch02  ","isch03  ","isch0203",
                  "olfa03  ",
                  "om02    ")
  
  agg.merge.tb$disease=nhis_diesease[i]
  
  #순서 재정리
  agg.merge.tb %>% select(disease,categories,`2010`:`2019`)}

setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\work")
write.csv(agg_nhis_dis(a01,1) ,file="dis01.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a02,2) ,file="dis02.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a03,3) ,file="dis03.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a04,4) ,file="dis04.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a05,5) ,file="dis05.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a06,6) ,file="dis06.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a07,7) ,file="dis07.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a08,8) ,file="dis08.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a09,9) ,file="dis09.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a10,10),file="dis10.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a11,11),file="dis11.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a12,12),file="dis12.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a13,13),file="dis13.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a14,14),file="dis14.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a15,15),file="dis15.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a16,16),file="dis16.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a17,17),file="dis17.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a18,18),file="dis18.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a19,19),file="dis19.csv",row.names=F,na="")
write.csv(agg_nhis_dis(a20,20),file="dis20.csv",row.names=F,na="")
#--------------------------------------------------------------------------------------------#
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

# x11();ggplot(dat,aes(ddate,astm02_tot,col=dow))+geom_point()+theme_gray(base_size=25)+
#   theme(legend.position = "none",legend.title = element_blank())+
#   facet_wrap(~area,scales="free")

#GAMM4, 선형가정, 질환 자료 
#보정: 체감기온, 시간 추세(Time trend), 랜덤 효과(시도*요일)
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\gamm4_linear")

#Asthma
g0=gamm4(astm02_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_astm02_pm10_linear_m0.rds")
g1=gamm4(astm02_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_astm02_pm10_linear_m1.rds")
g2=gamm4(astm02_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_astm02_pm10_linear_m2.rds")
g3=gamm4(astm02_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_astm02_pm10_linear_m3.rds")
g4=gamm4(astm02_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_astm02_pm10_linear_m4.rds")
g5=gamm4(astm02_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_astm02_pm10_linear_m5.rds")
g6=gamm4(astm02_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_astm02_pm10_linear_m6.rds")
g7=gamm4(astm02_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_astm02_pm10_linear_m7.rds")

#Asthma
g0=gamm4(astm03_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_astm03_pm10_linear_m0.rds")
g1=gamm4(astm03_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_astm03_pm10_linear_m1.rds")
g2=gamm4(astm03_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_astm03_pm10_linear_m2.rds")
g3=gamm4(astm03_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_astm03_pm10_linear_m3.rds")
g4=gamm4(astm03_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_astm03_pm10_linear_m4.rds")
g5=gamm4(astm03_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_astm03_pm10_linear_m5.rds")
g6=gamm4(astm03_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_astm03_pm10_linear_m6.rds")
g7=gamm4(astm03_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_astm03_pm10_linear_m7.rds")

#Asthma
g0=gamm4(astm0203_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_astm0203_pm10_linear_m0.rds")
g1=gamm4(astm0203_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_astm0203_pm10_linear_m1.rds")
g2=gamm4(astm0203_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_astm0203_pm10_linear_m2.rds")
g3=gamm4(astm0203_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_astm0203_pm10_linear_m3.rds")
g4=gamm4(astm0203_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_astm0203_pm10_linear_m4.rds")
g5=gamm4(astm0203_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_astm0203_pm10_linear_m5.rds")
g6=gamm4(astm0203_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_astm0203_pm10_linear_m6.rds")
g7=gamm4(astm0203_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_astm0203_pm10_linear_m7.rds")

#CVD
g0=gamm4(cvd02_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_cvd02_pm10_linear_m0.rds")
g1=gamm4(cvd02_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_cvd02_pm10_linear_m1.rds")
g2=gamm4(cvd02_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_cvd02_pm10_linear_m2.rds")
g3=gamm4(cvd02_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_cvd02_pm10_linear_m3.rds")
g4=gamm4(cvd02_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_cvd02_pm10_linear_m4.rds")
g5=gamm4(cvd02_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_cvd02_pm10_linear_m5.rds")
g6=gamm4(cvd02_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_cvd02_pm10_linear_m6.rds")
g7=gamm4(cvd02_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_cvd02_pm10_linear_m7.rds")

#IHD
g0=gamm4(ihd02_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_ihd02_pm10_linear_m0.rds")
g1=gamm4(ihd02_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_ihd02_pm10_linear_m1.rds")
g2=gamm4(ihd02_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_ihd02_pm10_linear_m2.rds")
g3=gamm4(ihd02_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_ihd02_pm10_linear_m3.rds")
g4=gamm4(ihd02_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_ihd02_pm10_linear_m4.rds")
g5=gamm4(ihd02_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_ihd02_pm10_linear_m5.rds")
g6=gamm4(ihd02_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_ihd02_pm10_linear_m6.rds")
g7=gamm4(ihd02_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_ihd02_pm10_linear_m7.rds")

#Heart failure
g0=gamm4(hf02_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_hf02_pm10_linear_m0.rds")
g1=gamm4(hf02_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_hf02_pm10_linear_m1.rds")
g2=gamm4(hf02_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_hf02_pm10_linear_m2.rds")
g3=gamm4(hf02_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_hf02_pm10_linear_m3.rds")
g4=gamm4(hf02_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_hf02_pm10_linear_m4.rds")
g5=gamm4(hf02_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_hf02_pm10_linear_m5.rds")
g6=gamm4(hf02_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_hf02_pm10_linear_m6.rds")
g7=gamm4(hf02_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_hf02_pm10_linear_m7.rds")

#Heart failure
g0=gamm4(hf03_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_hf03_pm10_linear_m0.rds")
g1=gamm4(hf03_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_hf03_pm10_linear_m1.rds")
g2=gamm4(hf03_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_hf03_pm10_linear_m2.rds")
g3=gamm4(hf03_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_hf03_pm10_linear_m3.rds")
g4=gamm4(hf03_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_hf03_pm10_linear_m4.rds")
g5=gamm4(hf03_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_hf03_pm10_linear_m5.rds")
g6=gamm4(hf03_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_hf03_pm10_linear_m6.rds")
g7=gamm4(hf03_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_hf03_pm10_linear_m7.rds")

#Heart failure
g0=gamm4(hf0203_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_hf0203_pm10_linear_m0.rds")
g1=gamm4(hf0203_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_hf0203_pm10_linear_m1.rds")
g2=gamm4(hf0203_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_hf0203_pm10_linear_m2.rds")
g3=gamm4(hf0203_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_hf0203_pm10_linear_m3.rds")
g4=gamm4(hf0203_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_hf0203_pm10_linear_m4.rds")
g5=gamm4(hf0203_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_hf0203_pm10_linear_m5.rds")
g6=gamm4(hf0203_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_hf0203_pm10_linear_m6.rds")
g7=gamm4(hf0203_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_hf0203_pm10_linear_m7.rds")

#Hemoragic Stroke
g0=gamm4(hemo02_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_hemo02_pm10_linear_m0.rds")
g1=gamm4(hemo02_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_hemo02_pm10_linear_m1.rds")
g2=gamm4(hemo02_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_hemo02_pm10_linear_m2.rds")
g3=gamm4(hemo02_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_hemo02_pm10_linear_m3.rds")
g4=gamm4(hemo02_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_hemo02_pm10_linear_m4.rds")
g5=gamm4(hemo02_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_hemo02_pm10_linear_m5.rds")
g6=gamm4(hemo02_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_hemo02_pm10_linear_m6.rds")
g7=gamm4(hemo02_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_hemo02_pm10_linear_m7.rds")

#Hemoragic Stroke
g0=gamm4(hemo03_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_hemo03_pm10_linear_m0.rds")
g1=gamm4(hemo03_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_hemo03_pm10_linear_m1.rds")
g2=gamm4(hemo03_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_hemo03_pm10_linear_m2.rds")
g3=gamm4(hemo03_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_hemo03_pm10_linear_m3.rds")
g4=gamm4(hemo03_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_hemo03_pm10_linear_m4.rds")
g5=gamm4(hemo03_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_hemo03_pm10_linear_m5.rds")
g6=gamm4(hemo03_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_hemo03_pm10_linear_m6.rds")
g7=gamm4(hemo03_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_hemo03_pm10_linear_m7.rds")

#Hemoragic Stroke
g0=gamm4(hemo0203_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_hemo0203_pm10_linear_m0.rds")
g1=gamm4(hemo0203_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_hemo0203_pm10_linear_m1.rds")
g2=gamm4(hemo0203_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_hemo0203_pm10_linear_m2.rds")
g3=gamm4(hemo0203_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_hemo0203_pm10_linear_m3.rds")
g4=gamm4(hemo0203_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_hemo0203_pm10_linear_m4.rds")
g5=gamm4(hemo0203_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_hemo0203_pm10_linear_m5.rds")
g6=gamm4(hemo0203_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_hemo0203_pm10_linear_m6.rds")
g7=gamm4(hemo0203_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_hemo0203_pm10_linear_m7.rds")

#Ischemic Stroke
g0=gamm4(isch02_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_isch02_pm10_linear_m0.rds")
g1=gamm4(isch02_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_isch02_pm10_linear_m1.rds")
g2=gamm4(isch02_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_isch02_pm10_linear_m2.rds")
g3=gamm4(isch02_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_isch02_pm10_linear_m3.rds")
g4=gamm4(isch02_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_isch02_pm10_linear_m4.rds")
g5=gamm4(isch02_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_isch02_pm10_linear_m5.rds")
g6=gamm4(isch02_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_isch02_pm10_linear_m6.rds")
g7=gamm4(isch02_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_isch02_pm10_linear_m7.rds")

#Ischemic Stroke
g0=gamm4(isch03_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_isch03_pm10_linear_m0.rds")
g1=gamm4(isch03_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_isch03_pm10_linear_m1.rds")
g2=gamm4(isch03_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_isch03_pm10_linear_m2.rds")
g3=gamm4(isch03_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_isch03_pm10_linear_m3.rds")
g4=gamm4(isch03_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_isch03_pm10_linear_m4.rds")
g5=gamm4(isch03_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_isch03_pm10_linear_m5.rds")
g6=gamm4(isch03_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_isch03_pm10_linear_m6.rds")
g7=gamm4(isch03_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_isch03_pm10_linear_m7.rds")

#Ischemic Stroke
g0=gamm4(isch0203_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_isch0203_pm10_linear_m0.rds")
g1=gamm4(isch0203_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_isch0203_pm10_linear_m1.rds")
g2=gamm4(isch0203_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_isch0203_pm10_linear_m2.rds")
g3=gamm4(isch0203_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_isch0203_pm10_linear_m3.rds")
g4=gamm4(isch0203_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_isch0203_pm10_linear_m4.rds")
g5=gamm4(isch0203_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_isch0203_pm10_linear_m5.rds")
g6=gamm4(isch0203_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_isch0203_pm10_linear_m6.rds")
g7=gamm4(isch0203_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_isch0203_pm10_linear_m7.rds")

#Hypertension
g0=gamm4(htn02_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_htn02_pm10_linear_m0.rds")
g1=gamm4(htn02_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_htn02_pm10_linear_m1.rds")
g2=gamm4(htn02_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_htn02_pm10_linear_m2.rds")
g3=gamm4(htn02_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_htn02_pm10_linear_m3.rds")
g4=gamm4(htn02_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_htn02_pm10_linear_m4.rds")
g5=gamm4(htn02_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_htn02_pm10_linear_m5.rds")
g6=gamm4(htn02_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_htn02_pm10_linear_m6.rds")
g7=gamm4(htn02_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_htn02_pm10_linear_m7.rds")

#Otitis media
g0=gamm4(om02_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_om02_pm10_linear_m0.rds")
g1=gamm4(om02_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_om02_pm10_linear_m1.rds")
g2=gamm4(om02_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_om02_pm10_linear_m2.rds")
g3=gamm4(om02_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_om02_pm10_linear_m3.rds")
g4=gamm4(om02_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_om02_pm10_linear_m4.rds")
g5=gamm4(om02_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_om02_pm10_linear_m5.rds")
g6=gamm4(om02_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_om02_pm10_linear_m6.rds")
g7=gamm4(om02_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_om02_pm10_linear_m7.rds")

#어지러움 
g0=gamm4(diz02_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_diz02_pm10_linear_m0.rds")
g1=gamm4(diz02_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_diz02_pm10_linear_m1.rds")
g2=gamm4(diz02_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_diz02_pm10_linear_m2.rds")
g3=gamm4(diz02_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_diz02_pm10_linear_m3.rds")
g4=gamm4(diz02_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_diz02_pm10_linear_m4.rds")
g5=gamm4(diz02_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_diz02_pm10_linear_m5.rds")
g6=gamm4(diz02_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_diz02_pm10_linear_m6.rds")
g7=gamm4(diz02_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_diz02_pm10_linear_m7.rds")

#어지러움 
g0=gamm4(diz03_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_diz03_pm10_linear_m0.rds")
g1=gamm4(diz03_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_diz03_pm10_linear_m1.rds")
g2=gamm4(diz03_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_diz03_pm10_linear_m2.rds")
g3=gamm4(diz03_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_diz03_pm10_linear_m3.rds")
g4=gamm4(diz03_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_diz03_pm10_linear_m4.rds")
g5=gamm4(diz03_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_diz03_pm10_linear_m5.rds")
g6=gamm4(diz03_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_diz03_pm10_linear_m6.rds")
g7=gamm4(diz03_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_diz03_pm10_linear_m7.rds")

#어지러움 
g0=gamm4(diz0203_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_diz0203_pm10_linear_m0.rds")
g1=gamm4(diz0203_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_diz0203_pm10_linear_m1.rds")
g2=gamm4(diz0203_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_diz0203_pm10_linear_m2.rds")
g3=gamm4(diz0203_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_diz0203_pm10_linear_m3.rds")
g4=gamm4(diz0203_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_diz0203_pm10_linear_m4.rds")
g5=gamm4(diz0203_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_diz0203_pm10_linear_m5.rds")
g6=gamm4(diz0203_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_diz0203_pm10_linear_m6.rds")
g7=gamm4(diz0203_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_diz0203_pm10_linear_m7.rds")

#후각 장애
g0=gamm4(olfa03_tot~pm10   +s(simpat)   +s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g0,file="gamm4_olfa03_pm10_linear_m0.rds")
g1=gamm4(olfa03_tot~pm10_m1+s(simpat_m1)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g1,file="gamm4_olfa03_pm10_linear_m1.rds")
g2=gamm4(olfa03_tot~pm10_m2+s(simpat_m2)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g2,file="gamm4_olfa03_pm10_linear_m2.rds")
g3=gamm4(olfa03_tot~pm10_m3+s(simpat_m3)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g3,file="gamm4_olfa03_pm10_linear_m3.rds")
g4=gamm4(olfa03_tot~pm10_m4+s(simpat_m4)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g4,file="gamm4_olfa03_pm10_linear_m4.rds")
g5=gamm4(olfa03_tot~pm10_m5+s(simpat_m5)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g5,file="gamm4_olfa03_pm10_linear_m5.rds")
g6=gamm4(olfa03_tot~pm10_m6+s(simpat_m6)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g6,file="gamm4_olfa03_pm10_linear_m6.rds")
g7=gamm4(olfa03_tot~pm10_m7+s(simpat_m7)+s(sn,k=6*14),random=~(1|sidow),family="poisson",data=dat);saveRDS(g7,file="gamm4_olfa03_pm10_linear_m7.rds")
