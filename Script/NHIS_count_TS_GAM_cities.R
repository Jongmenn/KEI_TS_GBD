library(mgcv)
library(lubridate)
library(dplyr)
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\자료\\KEI_서울대_건강자료(보안)")
dat<-read.csv("finaldata_20230309.csv",fileEncoding = "euc-kr")

dat$dow=weekdays(ymd(dat$ddate))
dat<-subset(dat,year>=2010)
dat<-dat[complete.cases(dat %>% select(pm25_new,pm25_new_m1:pm25_new_m7,simpat,simpat_m1:simpat_m7,dow)),]
table(dat$sido)
s01<-subset(dat,sido==11)
s02<-subset(dat,sido==21)
s03<-subset(dat,sido==22)
s04<-subset(dat,sido==23)
s05<-subset(dat,sido==24)
s06<-subset(dat,sido==25)
s07<-subset(dat,sido==26)
s08<-subset(dat,sido==29)
s09<-subset(dat,sido==31)
s10<-subset(dat,sido==32)
s11<-subset(dat,sido==33)
s12<-subset(dat,sido==34)
s13<-subset(dat,sido==35)
s14<-subset(dat,sido==36)
s15<-subset(dat,sido==37)
s16<-subset(dat,sido==38)
s17<-subset(dat,sido==39)

names(dat)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#ADHD
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(adhd02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(adhd02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(adhd02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(adhd02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(adhd02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(adhd02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(adhd02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(adhd02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(adhd03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(adhd03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(adhd03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(adhd03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(adhd03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(adhd03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(adhd03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(adhd03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(adhd0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(adhd0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(adhd0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(adhd0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(adhd0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(adhd0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(adhd0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(adhd0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="ADHD";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="ADHD";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="ADHD";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_adhd_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#aki

gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(akid02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(akid02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(akid02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(akid02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(akid02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(akid02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(akid02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(akid02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(akid03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(akid03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(akid03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(akid03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(akid03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(akid03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(akid03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(akid03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(akid0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(akid0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(akid0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(akid0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(akid0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(akid0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(akid0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(akid0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="AKI";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="AKI";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="AKI";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)
      
write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_aki_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#ALRI
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(alri02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(alri02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(alri02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(alri02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(alri02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(alri02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(alri02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(alri02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(alri03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(alri03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(alri03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(alri03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(alri03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(alri03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(alri03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(alri03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df1$CoD="ALRI";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="ALRI";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  rbind(df1,df2)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_alri_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Anxiety
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(anx02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(anx02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(anx02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(anx02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(anx02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(anx02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(anx02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(anx02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(anx03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(anx03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(anx03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(anx03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(anx03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(anx03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(anx03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(anx03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(anx0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(anx0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(anx0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(anx0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(anx0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(anx0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(anx0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(anx0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="Anxiety";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="Anxiety";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="Anxiety";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_Anxiety_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#CVD
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(cvd02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(cvd02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(cvd02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(cvd02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(cvd02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(cvd02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(cvd02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(cvd02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(cvd03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(cvd03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(cvd03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(cvd03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(cvd03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(cvd03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(cvd03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(cvd03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(cvd0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(cvd0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(cvd0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(cvd0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(cvd0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(cvd0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(cvd0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(cvd0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="CVD";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="CVD";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="CVD";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_CVD_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Hemorrhagic strokes
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(hemo02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(hemo02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(hemo02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(hemo02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(hemo02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(hemo02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(hemo02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(hemo02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(hemo03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(hemo03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(hemo03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(hemo03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(hemo03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(hemo03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(hemo03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(hemo03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(hemo0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(hemo0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(hemo0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(hemo0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(hemo0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(hemo0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(hemo0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(hemo0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="Hemorrhagic strokes";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="Hemorrhagic strokes";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="Hemorrhagic strokes";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_hemo_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Heart failure
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(hf02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(hf02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(hf02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(hf02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(hf02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(hf02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(hf02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(hf02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(hf03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(hf03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(hf03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(hf03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(hf03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(hf03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(hf03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(hf03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(hf0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(hf0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(hf0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(hf0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(hf0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(hf0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(hf0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(hf0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="Heart failure";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="Heart failure";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="Heart failure";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_hf_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Hypertension
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(htn02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(htn02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(htn02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(htn02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(htn02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(htn02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(htn02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(htn02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(htn03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(htn03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(htn03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(htn03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(htn03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(htn03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(htn03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(htn03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(htn0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(htn0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(htn0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(htn0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(htn0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(htn0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(htn0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(htn0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="Hypertension";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="Hypertension";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="Hypertension";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_htn_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#IHD
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(ihd02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(ihd02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(ihd02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(ihd02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(ihd02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(ihd02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(ihd02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(ihd02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(ihd03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(ihd03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(ihd03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(ihd03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(ihd03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(ihd03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(ihd03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(ihd03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(ihd0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(ihd0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(ihd0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(ihd0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(ihd0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(ihd0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(ihd0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(ihd0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="IHD";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="IHD";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="IHD";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_ihd_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#olfa
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(olfa02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(olfa02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(olfa02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(olfa02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(olfa02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(olfa02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(olfa02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(olfa02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(olfa03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(olfa03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(olfa03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(olfa03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(olfa03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(olfa03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(olfa03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(olfa03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(olfa0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(olfa0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(olfa0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(olfa0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(olfa0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(olfa0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(olfa0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(olfa0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="olfa";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="olfa";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="olfa";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_olfa_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#om
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(om02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(om02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(om02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(om02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(om02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(om02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(om02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(om02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(om03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(om03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(om03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(om03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(om03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(om03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(om03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(om03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(om0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(om0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(om0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(om0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(om0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(om0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(om0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(om0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="om";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="om";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="om";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_om_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#pulm
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(pulm02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(pulm02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(pulm02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(pulm02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(pulm02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(pulm02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(pulm02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(pulm02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(pulm03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(pulm03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(pulm03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(pulm03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(pulm03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(pulm03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(pulm03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(pulm03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(pulm0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(pulm0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(pulm0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(pulm0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(pulm0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(pulm0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(pulm0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(pulm0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="pulm";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="pulm";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="pulm";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_pulm_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Respiratory
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(resp02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(resp02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(resp02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(resp02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(resp02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(resp02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(resp02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(resp02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(resp03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(resp03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(resp03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(resp03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(resp03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(resp03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(resp03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(resp03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(resp0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(resp0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(resp0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(resp0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(resp0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(resp0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(resp0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(resp0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="resp";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="resp";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="resp";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_resp_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Atopic dermatitis
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(atop02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(atop02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(atop02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(atop02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(atop02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(atop02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(atop02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(atop02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(atop03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(atop03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(atop03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(atop03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(atop03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(atop03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(atop03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(atop03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(atop0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(atop0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(atop0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(atop0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(atop0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(atop0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(atop0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(atop0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="Atopic dermatitis";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="Atopic dermatitis";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="Atopic dermatitis";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_atop_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Conjunctivitis
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(conj02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(conj02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(conj02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(conj02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(conj02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(conj02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(conj02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(conj02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(conj03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(conj03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(conj03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(conj03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(conj03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(conj03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(conj03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(conj03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(conj0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(conj0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(conj0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(conj0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(conj0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(conj0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(conj0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(conj0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="Conjunctivitis";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="Conjunctivitis";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="Conjunctivitis";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_conj_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#lipression
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(lip02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(lip02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(lip02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(lip02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(lip02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(lip02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(lip02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(lip02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(lip03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(lip03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(lip03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(lip03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(lip03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(lip03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(lip03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(lip03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(lip0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(lip0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(lip0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(lip0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(lip0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(lip0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(lip0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(lip0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="lipression";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="lipression";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="lipression";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_lip_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#T2DM
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(lip02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(lip02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(lip02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(lip02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(lip02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(lip02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(lip02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(lip02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(lip03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(lip03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(lip03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(lip03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(lip03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(lip03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(lip03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(lip03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(lip0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(lip0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(lip0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(lip0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(lip0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(lip0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(lip0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(lip0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="T2DM";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="T2DM";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="T2DM";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_lip_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#T2DM
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(dm02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(dm02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(dm02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(dm02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(dm02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(dm02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(dm02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(dm02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(dm03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(dm03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(dm03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(dm03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(dm03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(dm03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(dm03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(dm03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(dm0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(dm0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(dm0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(dm0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(dm0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(dm0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(dm0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(dm0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="T2DM";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="T2DM";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="T2DM";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_T2DM_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#pd
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(pd02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(pd02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(pd02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(pd02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(pd02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(pd02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(pd02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(pd02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(pd03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(pd03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(pd03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(pd03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(pd03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(pd03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(pd03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(pd03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(pd0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(pd0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(pd0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(pd0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(pd0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(pd0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(pd0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(pd0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="pd";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="pd";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="pd";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_pd_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Sleep
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(slep02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(slep02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(slep02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(slep02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(slep02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(slep02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(slep02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(slep02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(slep03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(slep03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(slep03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(slep03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(slep03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(slep03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(slep03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(slep03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(slep0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(slep0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(slep0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(slep0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(slep0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(slep0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(slep0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(slep0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="slep";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="slep";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="slep";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_slep_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#asinu
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(asinu02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(asinu02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(asinu02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(asinu02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(asinu02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(asinu02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(asinu02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(asinu02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(asinu03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(asinu03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(asinu03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(asinu03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(asinu03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(asinu03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(asinu03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(asinu03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(asinu0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(asinu0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(asinu0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(asinu0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(asinu0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(asinu0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(asinu0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(asinu0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="asinu";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="asinu";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="asinu";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_asinu_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#alz
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(alz02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(alz02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(alz02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(alz02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(alz02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(alz02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(alz02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(alz02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(alz03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(alz03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(alz03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(alz03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(alz03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(alz03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(alz03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(alz03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(alz0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(alz0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(alz0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(alz0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(alz0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(alz0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(alz0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(alz0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="alz";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="alz";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="alz";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_alz_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#vdem
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(vdem02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(vdem02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(vdem02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(vdem02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(vdem02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(vdem02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(vdem02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(vdem02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(vdem03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(vdem03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(vdem03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(vdem03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(vdem03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(vdem03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(vdem03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(vdem03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(vdem0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(vdem0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(vdem0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(vdem0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(vdem0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(vdem0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(vdem0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(vdem0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="vdem";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="vdem";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="vdem";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_vdem_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#infl
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(infl02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(infl02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(infl02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(infl02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(infl02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(infl02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(infl02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(infl02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(infl03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(infl03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(infl03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(infl03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(infl03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(infl03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(infl03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(infl03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(infl0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(infl0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(infl0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(infl0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(infl0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(infl0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(infl0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(infl0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="infl";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="infl";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="infl";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_infl_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#msc
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(msc02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(msc02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(msc02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(msc02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(msc02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(msc02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(msc02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(msc02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(msc03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(msc03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(msc03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(msc03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(msc03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(msc03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(msc03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(msc03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(msc0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(msc0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(msc0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(msc0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(msc0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(msc0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(msc0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(msc0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="msc";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="msc";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="msc";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_msc_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#rhin
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(rhin02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(rhin02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(rhin02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(rhin02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(rhin02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(rhin02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(rhin02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(rhin02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(rhin03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(rhin03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(rhin03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(rhin03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(rhin03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(rhin03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(rhin03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(rhin03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(rhin0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(rhin0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(rhin0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(rhin0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(rhin0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(rhin0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(rhin0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(rhin0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="rhin";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="rhin";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="rhin";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_rhin_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#paro
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(paro02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(paro02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(paro02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(paro02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(paro02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(paro02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(paro02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(paro02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(paro03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(paro03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(paro03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(paro03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(paro03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(paro03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(paro03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(paro03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(paro0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(paro0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(paro0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(paro0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(paro0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(paro0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(paro0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(paro0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="paro";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="paro";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="paro";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_paro_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#ob
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(ob02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(ob02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(ob02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(ob02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(ob02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(ob02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(ob02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(ob02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(ob03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(ob03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(ob03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(ob03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(ob03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(ob03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(ob03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(ob03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(ob0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(ob0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(ob0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(ob0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(ob0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(ob0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(ob0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(ob0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="ob";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="ob";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="ob";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_ob_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#myco
gam_sido<-function(data){
  d<-data
  
  mor1_00<-gam(myco02_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_01<-gam(myco02_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_02<-gam(myco02_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_03<-gam(myco02_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_04<-gam(myco02_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_05<-gam(myco02_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_06<-gam(myco02_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor1_07<-gam(myco02_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor2_00<-gam(myco03_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_01<-gam(myco03_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_02<-gam(myco03_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_03<-gam(myco03_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_04<-gam(myco03_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_05<-gam(myco03_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_06<-gam(myco03_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor2_07<-gam(myco03_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  mor3_00<-gam(myco0203_tot~pm25_new+s(simpat)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_01<-gam(myco0203_tot~pm25_new_m1+s(simpat_m1)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_02<-gam(myco0203_tot~pm25_new_m2+s(simpat_m2)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_03<-gam(myco0203_tot~pm25_new_m3+s(simpat_m3)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_04<-gam(myco0203_tot~pm25_new_m4+s(simpat_m4)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_05<-gam(myco0203_tot~pm25_new_m5+s(simpat_m5)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_06<-gam(myco0203_tot~pm25_new_m6+s(simpat_m6)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  mor3_07<-gam(myco0203_tot~pm25_new_m7+s(simpat_m7)+s(sn,k=6*10)+factor(dow),family="poisson",data=d)
  
  df1<-as.data.frame(rbind(summary(mor1_00)$p.table[2,],summary(mor1_01)$p.table[2,],
                           summary(mor1_02)$p.table[2,],summary(mor1_03)$p.table[2,],
                           summary(mor1_04)$p.table[2,],summary(mor1_05)$p.table[2,],
                           summary(mor1_06)$p.table[2,],summary(mor1_07)$p.table[2,]))
  
  df2<-as.data.frame(rbind(summary(mor2_00)$p.table[2,],summary(mor2_01)$p.table[2,],
                           summary(mor2_02)$p.table[2,],summary(mor2_03)$p.table[2,],
                           summary(mor2_04)$p.table[2,],summary(mor2_05)$p.table[2,],
                           summary(mor2_06)$p.table[2,],summary(mor2_07)$p.table[2,]))
  
  df3<-as.data.frame(rbind(summary(mor3_00)$p.table[2,],summary(mor3_01)$p.table[2,],
                           summary(mor3_02)$p.table[2,],summary(mor3_03)$p.table[2,],
                           summary(mor3_04)$p.table[2,],summary(mor3_05)$p.table[2,],
                           summary(mor3_06)$p.table[2,],summary(mor3_07)$p.table[2,]))
  
  df1$CoD="myco";df1$type="02"      ;df1$lag=paste0("lag0",1:8-1)
  df2$CoD="myco";df2$type="03"      ;df2$lag=paste0("lag0",1:8-1)
  df3$CoD="myco";df3$type="0203"    ;df3$lag=paste0("lag0",1:8-1)
  rbind(df1,df2,df3)}

res01<-gam_sido(s01);res01$sido=11
res02<-gam_sido(s02);res02$sido=26
res03<-gam_sido(s03);res03$sido=27
res04<-gam_sido(s04);res04$sido=28
res05<-gam_sido(s05);res05$sido=29
res06<-gam_sido(s06);res06$sido=30
res07<-gam_sido(s07);res07$sido=31
res08<-gam_sido(s08);res08$sido=36
res09<-gam_sido(s09);res09$sido=41
res10<-gam_sido(s10);res10$sido=42
res11<-gam_sido(s11);res11$sido=43
res12<-gam_sido(s12);res12$sido=44
res13<-gam_sido(s13);res13$sido=45
res14<-gam_sido(s14);res14$sido=46
res15<-gam_sido(s15);res15$sido=47
res16<-gam_sido(s16);res16$sido=48
res17<-gam_sido(s17);res17$sido=50

res_m<-rbind(res01,res02,res03,res04,res05,res06,res07,res08,
             res09,res10,res11,res12,res13,res14,res15,res16,res17)
names(res_m)[5]="CoD"

res_m
library(metafor)

names(res_m)[1:2]=c("est","SE")
#meta-anlysis

sido_u<-dat %>% select(sido,EN_SIDO)
sido_u<-sido_u[!duplicated(sido_u),]
res_m2<-res_m %>% left_join(sido_u,by="sido")
table(res_m2$sido)

write.csv(res_m2,file="D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\nhis\\TS_Hospitalization_myco_by_sido.csv",row.names=F,na="",fileEncoding = "euc-kr")
