setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result\\비선형결과\\cr")

d01<-read.csv("cr_tot_pm25.csv")
d02<-read.csv("cr_tot_pm10.csv")
d03<-read.csv("cr_tot_so2.csv")
d04<-read.csv("cr_tot_no2.csv")
d05<-read.csv("cr_tot_co.csv")
d06<-read.csv("cr_tot_o3.csv")

d07<-read.csv("cr_nonacc_pm25.csv")
d08<-read.csv("cr_nonacc_pm10.csv")
d09<-read.csv("cr_nonacc_so2.csv")
d10<-read.csv("cr_nonacc_no2.csv")
d11<-read.csv("cr_nonacc_co.csv")
d12<-read.csv("cr_nonacc_o3.csv")

d13<-read.csv("cr_cvd_pm25.csv")
d14<-read.csv("cr_cvd_pm10.csv")
d15<-read.csv("cr_cvd_so2.csv")
d16<-read.csv("cr_cvd_no2.csv")
d17<-read.csv("cr_cvd_co.csv")
d18<-read.csv("cr_cvd_o3.csv")

d19<-read.csv("cr_respiratory_pm25.csv")
d20<-read.csv("cr_respiratory_pm10.csv")
d21<-read.csv("cr_respiratory_so2.csv")
d22<-read.csv("cr_respiratory_no2.csv")
d23<-read.csv("cr_respiratory_co.csv")
d24<-read.csv("cr_respiratory_o3.csv")

names(d01)[1]="exp";d01$exposure="pm25" ;d01$outcome="All-cause"
names(d02)[1]="exp";d02$exposure="pm10" ;d02$outcome="All-cause"
names(d03)[1]="exp";d03$exposure="so2"  ;d03$outcome="All-cause"
names(d04)[1]="exp";d04$exposure="no2"  ;d04$outcome="All-cause" 
names(d05)[1]="exp";d05$exposure="co"   ;d05$outcome="All-cause" 
names(d06)[1]="exp";d06$exposure="o3"   ;d06$outcome="All-cause"
names(d07)[1]="exp";d07$exposure="pm25" ;d07$outcome="nonacc"
names(d08)[1]="exp";d08$exposure="pm10" ;d08$outcome="nonacc"
names(d09)[1]="exp";d09$exposure="so2"  ;d09$outcome="nonacc" 
names(d10)[1]="exp";d10$exposure="no2"  ;d10$outcome="nonacc"
names(d11)[1]="exp";d11$exposure="co"   ;d11$outcome="nonacc"
names(d12)[1]="exp";d12$exposure="o3"   ;d12$outcome="nonacc"
names(d13)[1]="exp";d13$exposure="pm25" ;d13$outcome="CVD"
names(d14)[1]="exp";d14$exposure="pm10" ;d14$outcome="CVD"
names(d15)[1]="exp";d15$exposure="so2"  ;d15$outcome="CVD"
names(d16)[1]="exp";d16$exposure="no2"  ;d16$outcome="CVD"
names(d17)[1]="exp";d17$exposure="co"   ;d17$outcome="CVD"
names(d18)[1]="exp";d18$exposure="o3"   ;d18$outcome="CVD"
names(d19)[1]="exp";d19$exposure="pm25" ;d19$outcome="respiratory"
names(d20)[1]="exp";d20$exposure="pm10" ;d20$outcome="respiratory"
names(d21)[1]="exp";d21$exposure="so2"  ;d21$outcome="respiratory"
names(d22)[1]="exp";d22$exposure="no2"  ;d22$outcome="respiratory"
names(d23)[1]="exp";d23$exposure="co"   ;d23$outcome="respiratory"
names(d24)[1]="exp";d24$exposure="o3"   ;d24$outcome="respiratory"

dd<-rbind(d01,d02,d03,d04,d05,d06,d07,d08,d09,d10,d11,d12,
          d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23,d24)

dd$concentration=dd$exp/dd$unit
dd2<-dd %>% select(lag,exposure,outcome,concentration,beta:del2_uci)
dd2<-dd2[complete.cases(dd2$beta),]

names(dd2)=gsub("del","beta",names(dd2))
write.csv(dd2,file="cr_AP_merge.csv",row.names=F,na="")
