pacman::p_load(lubridate,dplyr)

setwd("D:\\SNU\\KEI\\자료\\KEI_서울대_건강자료(보안)\\AMI_OM")

#공단 연도별 시군구 -> 2019년 기준 병합하기 
nhis_sgg<-read_excel("D:\\SNU\\KEI\\NHIS_SGG.xlsx",sheet=1)
names(nhis_sgg)=c("sido","sgg","SIDO_NM","SGG_NM","SGG_NM2","sgg2019","nhis_sgg2019","V1","V2")

#공단 반출자료와 연계해서 시군구 변경할 변수만 남기기 
nhis_sgg_final<-nhis_sgg %>% select(sgg,SIDO_NM,SGG_NM2,sgg2019,nhis_sgg2019)
nhis_sgg_final<-nhis_sgg_final[complete.cases(nhis_sgg_final$nhis_sgg2019),]

nhis_sgg_final$nhis_sgg2019 %>% unique %>% length #250

#2006~2019년 날짜 만들기
#자료 분양할때는 2010~2019년만 전달
ddate=seq(as.Date("2006-01-01"),as.Date("2019-12-31"),1) #5,113
5113*250
nhis_sgg=unique(nhis_sgg_final$nhis_sgg2019) #250

#cross join (시군구별, 일자)
sgg_date<-merge(nhis_sgg,ddate,all=T) %>% arrange(x,y)
sgg_date$NHIS_SGGKEY=with(sgg_date,paste0(x,"-",y))

#날짜 연계할 키(시군구+일자) 
sgg_date<-sgg_date %>% select(NHIS_SGGKEY)

#질환 자료 
ami<-read.csv("AMI_daily_sgg_f0203_0619.csv")
om <-read.csv("OM_daily_sgg_f0203_0619.csv")

nrow(ami) #996,417
nrow(om)  #1,274,060

ami$sido=substr(ami$COKEY,1,2)
om$sido =substr(om$COKEY,1,2)

ami$sgg =substr(ami$COKEY,1,5)
om$sgg  =substr(om$COKEY,1,5)

ami$date=ymd(substr(ami$COKEY,7,16))
om$date =ymd(substr(om$COKEY,7,16))

#2019년 기준 시군구 변경코드 연계
ami_nhis<-ami %>% left_join(nhis_sgg_final,by="sgg") %>% mutate(NHIS_SGGKEY=paste0(nhis_sgg2019,"-",date)) %>% select(-COKEY)
om_nhis <-om  %>% left_join(nhis_sgg_final,by="sgg") %>% mutate(NHIS_SGGKEY=paste0(nhis_sgg2019,"-",date)) %>% select(-COKEY)  

#연계키 기준 (2019년 공단 시군구+일자)으로 자료 합계
#예를 들어 제주도 같은경우 49110, 49710 -> 50110으로 변경했으니, 50110으로 집계 합산

ami_nhis2<-ami_nhis %>% group_by(NHIS_SGGKEY) %>% 
  summarise(AMI_TOT =  sum(AMI_TOT   ,na.rm=T),
            AMI_M     =sum(AMI_M     ,na.rm=T),
            AMI_F     =sum(AMI_F     ,na.rm=T),
            AMI_00    =sum(AMI_00    ,na.rm=T),
            AMI_0014  =sum(AMI_0014  ,na.rm=T),
            AMI_1564  =sum(AMI_1564  ,na.rm=T),
            AMI_65    =sum(AMI_65    ,na.rm=T),
            AMI_00_M  =sum(AMI_00_M  ,na.rm=T),
            AMI_00_F  =sum(AMI_00_F  ,na.rm=T),
            AMI_0014_M=sum(AMI_0014_M,na.rm=T),
            AMI_0014_F=sum(AMI_0014_F,na.rm=T),
            AMI_1564_M=sum(AMI_1564_M,na.rm=T),
            AMI_1564_F=sum(AMI_1564_F,na.rm=T),
            AMI_65_M  =sum(AMI_65_M  ,na.rm=T),
            AMI_65_F  =sum(AMI_65_F  ,na.rm=T))

om_nhis2<-om_nhis %>% group_by(NHIS_SGGKEY) %>% 
  summarise(OM_TOT =  sum(OM_TOT   ,na.rm=T),
            OM_M     =sum(OM_M     ,na.rm=T),
            OM_F     =sum(OM_F     ,na.rm=T),
            OM_00    =sum(OM_00    ,na.rm=T),
            OM_0014  =sum(OM_0014  ,na.rm=T),
            OM_1564  =sum(OM_1564  ,na.rm=T),
            OM_65    =sum(OM_65    ,na.rm=T),
            OM_00_M  =sum(OM_00_M  ,na.rm=T),
            OM_00_F  =sum(OM_00_F  ,na.rm=T),
            OM_0014_M=sum(OM_0014_M,na.rm=T),
            OM_0014_F=sum(OM_0014_F,na.rm=T),
            OM_1564_M=sum(OM_1564_M,na.rm=T),
            OM_1564_F=sum(OM_1564_F,na.rm=T),
            OM_65_M  =sum(OM_65_M  ,na.rm=T),
            OM_65_F  =sum(OM_65_F  ,na.rm=T))


ami_0619<-merge(sgg_date,ami_nhis2,by="NHIS_SGGKEY",all.x=T)
om_0619 <-sgg_date %>% left_join(om_nhis2 ,by="NHIS_SGGKEY",all.x=T)

#merge시 비어있는 부분 채우기 
ami_0619[is.na(ami_0619$AMI_TOT),2:16]<-0
om_0619[is.na(om_0619$AMI_TOT),2:16]  <-0

#변수 일부 생성해서 넣어주기 
ami_0619$nhis_sido=substr(ami_0619$NHIS_SGGKEY,1,2)
ami_0619$nhis_sgg =substr(ami_0619$NHIS_SGGKEY,1,5)
ami_0619$date     =ymd(substr(ami_0619$NHIS_SGGKEY,7,16))
ami_0619$year     =year(ami_0619$date)
ami_0619$month    =month(ami_0619$date)
ami_0619$day      =day(ami_0619$date)
ami_0619$dow      =weekdays(ami_0619$date)

om_0619$nhis_sido=substr(om_0619$NHIS_SGGKEY,1,2)
om_0619$nhis_sgg =substr(om_0619$NHIS_SGGKEY,1,5)
om_0619$date     =ymd(substr(om_0619$NHIS_SGGKEY,7,16))
om_0619$year     =year(om_0619$date)
om_0619$month    =month(om_0619$date)
om_0619$day      =day(om_0619$date)
om_0619$dow      =weekdays(om_0619$date)

#최종자료에 시도, 시군구 레이블 변경 
nhis_sgg_df<-nhis_sgg_final %>% select(nhis_sgg2019,SIDO_NM,SGG_NM2)
nhis_sgg_df<-nhis_sgg_df[!duplicated(nhis_sgg_df$nhis_sgg2019),]

names(nhis_sgg_df)[1]=c("nhis_sgg")
names(nhis_sgg_df)[3]=c("SGG_NM")

#최종 자료 변수정리 
ami_0619_final<-ami_0619 %>% left_join(nhis_sgg_df,by="nhis_sgg") %>% 
  select(NHIS_SGGKEY,nhis_sido,nhis_sgg:dow,SIDO_NM,SGG_NM,AMI_TOT:AMI_65_F)

om_0619_final<-om_0619 %>% left_join(nhis_sgg_df,by="nhis_sgg") %>% 
  select(NHIS_SGGKEY,nhis_sido,nhis_sgg:dow,SIDO_NM,SGG_NM,OM_TOT:OM_65_F)


#자료 저장
write.csv(ami_0619_final,file="daily_sgg_ami_0619.csv",row.names=F,na="")
write.csv(om_0619_final ,file="daily_sgg_om_0619.csv" ,row.names=F,na="")

#UNIST 전달할 자료, 2010~2019년
#집계자료도 2010~2019년 (연구기간)으로 정리

ami_1019<-ami_0619_final %>% filter(year>=2010)
om_1019 <-om_0619_final  %>% filter(year>=2010)

nrow(ami_1019)
nrow(om_1019)

write.csv(ami_1019,file="daily_sgg_ami_1019.csv",row.names=F,na="")
write.csv(om_1019 ,file="daily_sgg_om_1019.csv" ,row.names=F,na="")

#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

ami_1019$SIDO_NM=factor(ami_1019$SIDO_NM,levels=unique(ami_1019$SIDO_NM))
om_1019$SIDO_NM =factor(om_1019$SIDO_NM ,levels=unique(om_1019$SIDO_NM))

#요약 테이블 :AMI
null_tb=t(rep(NA,10))

tb1<-aggregate(AMI_TOT~year,data=ami_1019,sum)

tb_tot<-t(cbind(tb1$AMI_TOT))

tb2<-aggregate(AMI_M~year,data=ami_1019,sum)
tb3<-aggregate(AMI_F~year,data=ami_1019,sum)

tb_sex<-t(cbind(tb2$AMI_M,tb3$AMI_F))

tb4<-aggregate(AMI_00  ~year,data=ami_1019,sum)
tb5<-aggregate(AMI_0014~year,data=ami_1019,sum)
tb6<-aggregate(AMI_1564~year,data=ami_1019,sum)
tb7<-aggregate(AMI_65  ~year,data=ami_1019,sum)

tb_age_group=t(cbind(tb4$AMI_00,tb5$AMI_0014,tb6$AMI_1564,tb7$AMI_65))

tb8<-aggregate(AMI_TOT ~SIDO_NM+year,data=ami_1019,sum)
tb9<-aggregate(AMI_TOT ~month+year,data=ami_1019,sum)

tb_sido<-cbind(subset(tb8,year==2010)$AMI_TOT,subset(tb8,year==2011)$AMI_TOT,
      subset(tb8,year==2012)$AMI_TOT,subset(tb8,year==2013)$AMI_TOT,
      subset(tb8,year==2014)$AMI_TOT,subset(tb8,year==2015)$AMI_TOT,
      subset(tb8,year==2016)$AMI_TOT,subset(tb8,year==2017)$AMI_TOT,
      subset(tb8,year==2018)$AMI_TOT,subset(tb8,year==2019)$AMI_TOT)

tb_month<-cbind(subset(tb9,year==2010)$AMI_TOT,subset(tb9,year==2011)$AMI_TOT,
      subset(tb9,year==2012)$AMI_TOT,subset(tb9,year==2013)$AMI_TOT,
      subset(tb9,year==2014)$AMI_TOT,subset(tb9,year==2015)$AMI_TOT,
      subset(tb9,year==2016)$AMI_TOT,subset(tb9,year==2017)$AMI_TOT,
      subset(tb9,year==2018)$AMI_TOT,subset(tb9,year==2019)$AMI_TOT)


ami_tb=as.data.frame(rbind(tb_tot,null_tb,tb_sex,null_tb,tb_age_group,null_tb,tb_sido,null_tb,tb_month))
colnames(ami_tb)=2010:2019

row.names(ami_tb)=c("전체","","남","여"," ","0세","15세미만","15-64세","65+","  ",
                    "서울","부산","대구","인천","광주","대전","울산","세종","경기",
                    "강원","충북","충남","전북","전남","경북","경남","제주","    ",
                    1:12)

write.csv(ami_tb,file="ami_tb.csv",na="")

#요약 테이블 :OM
tb1<-aggregate(OM_TOT~year,data=om_1019,sum)

tb_tot<-t(cbind(tb1$OM_TOT))

tb2<-aggregate(OM_M~year,data=om_1019,sum)
tb3<-aggregate(OM_F~year,data=om_1019,sum)

tb_sex<-t(cbind(tb2$OM_M,tb3$OM_F))

tb4<-aggregate(OM_00  ~year,data=om_1019,sum)
tb5<-aggregate(OM_0014~year,data=om_1019,sum)
tb6<-aggregate(OM_1564~year,data=om_1019,sum)
tb7<-aggregate(OM_65  ~year,data=om_1019,sum)

tb_age_group=t(cbind(tb4$OM_00,tb5$OM_0014,tb6$OM_1564,tb7$OM_65))

tb8<-aggregate(OM_TOT ~SIDO_NM+year,data=om_1019,sum)
tb9<-aggregate(OM_TOT ~month+year,data=om_1019,sum)

tb_sido<-cbind(subset(tb8,year==2010)$OM_TOT,subset(tb8,year==2011)$OM_TOT,
               subset(tb8,year==2012)$OM_TOT,subset(tb8,year==2013)$OM_TOT,
               subset(tb8,year==2014)$OM_TOT,subset(tb8,year==2015)$OM_TOT,
               subset(tb8,year==2016)$OM_TOT,subset(tb8,year==2017)$OM_TOT,
               subset(tb8,year==2018)$OM_TOT,subset(tb8,year==2019)$OM_TOT)

tb_month<-cbind(subset(tb9,year==2010)$OM_TOT,subset(tb9,year==2011)$OM_TOT,
                subset(tb9,year==2012)$OM_TOT,subset(tb9,year==2013)$OM_TOT,
                subset(tb9,year==2014)$OM_TOT,subset(tb9,year==2015)$OM_TOT,
                subset(tb9,year==2016)$OM_TOT,subset(tb9,year==2017)$OM_TOT,
                subset(tb9,year==2018)$OM_TOT,subset(tb9,year==2019)$OM_TOT)


om_tb=as.data.frame(rbind(tb_tot,null_tb,tb_sex,null_tb,tb_age_group,null_tb,tb_sido,null_tb,tb_month))
colnames(om_tb)=2010:2019

row.names(om_tb)=c("전체","","남","여"," ","0세","15세미만","15-64세","65+","  ",
                    "서울","부산","대구","인천","광주","대전","울산","세종","경기",
                    "강원","충북","충남","전북","전남","경북","경남","제주","    ",
                    1:12)

write.csv(om_tb,file="om_tb.csv",na="")
