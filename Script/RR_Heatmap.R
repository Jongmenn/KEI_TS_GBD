#----------------------------------------------------------------------#
#library
pacman::p_load(dplyr,lubridate,ggplot2,psych,gridExtra,stringr,e1071,readxl,
               adabag,ada,gbm,rpart,xgboost,fastAdaboost,pROC)

#----------------------------------------------------------------------#
#----------------------------------------------------------------------#
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담")
dat<-read_excel("KEI_SNU_TS_analysis_OJM_20230213.xlsx",sheet="통계청사망_선형(gamm4)")

#----------------------------------------------------------------------#
#PM2.5 & Mortality
hm<-subset(dat,exposure=="PM25")
hm$Lag=factor(hm$Lag,levels=c("Lag07","Lag06","Lag05","Lag04","Lag03","Lag02","Lag01","Lag00"))
hm$outcome=factor(hm$outcome,levels=unique(hm$outcome))
x11();ggplot(data=hm,aes(outcome,Lag))+geom_tile(aes(fill=RR))+
  theme_bw(base_size=28)+labs(x="",y="")+
  scale_fill_gradientn(colours=c("green","gold","Red"),limits=c(0.9994,1.0005))+
  geom_text(data=hm,aes(y=Lag,
                        label=paste0(sprintf("%.6f",hm$RR),ifelse(hm$Pval<0.05," *",""),"\n",
                                     "(",
                                     sprintf("%.6f",hm$RR_lci),"-",
                                     sprintf("%.6f",hm$RR_uci),")")),
            size=8)+
  guides(fill=guide_legend(title="Relative risk (95% CI)"))+
  theme(legend.position = "top")

#----------------------------------------------------------------------#
#PM10 & Mortality
hm<-subset(dat,exposure=="PM10")
hm$Lag=factor(hm$Lag,levels=c("Lag07","Lag06","Lag05","Lag04","Lag03","Lag02","Lag01","Lag00"))
hm$outcome=factor(hm$outcome,levels=unique(hm$outcome))
x11();ggplot(data=hm,aes(outcome,Lag))+geom_tile(aes(fill=RR))+
  theme_bw(base_size=28)+labs(x="",y="")+
  scale_fill_gradientn(colours=c("green","gold","Red"),limits=c(0.9994,1.0005))+
  geom_text(data=hm,aes(y=Lag,
                        label=paste0(sprintf("%.6f",hm$RR),ifelse(hm$Pval<0.05," *",""),"\n",
                                     "(",
                                     sprintf("%.6f",hm$RR_lci),"-",
                                     sprintf("%.6f",hm$RR_uci),")")),
            size=8)+
  guides(fill=guide_legend(title="Relative risk (95% CI)"))+
  theme(legend.position = "top")      

#----------------------------------------------------------------------#
#SO2 & Mortality
hm<-subset(dat,exposure=="SO2")
hm$Lag=factor(hm$Lag,levels=c("Lag07","Lag06","Lag05","Lag04","Lag03","Lag02","Lag01","Lag00"))
hm$outcome=factor(hm$outcome,levels=unique(hm$outcome))
x11();ggplot(data=hm,aes(outcome,Lag))+geom_tile(aes(fill=RR))+
  theme_bw(base_size=28)+labs(x="",y="")+
  scale_fill_gradientn(colours=c("green","gold","Red"),limits=c(0.996,1.002))+
  geom_text(data=hm,aes(y=Lag,
                        label=paste0(sprintf("%.6f",hm$RR),ifelse(hm$Pval<0.05," *",""),"\n",
                                     "(",
                                     sprintf("%.6f",hm$RR_lci),"-",
                                     sprintf("%.6f",hm$RR_uci),")")),
            size=8)+
  guides(fill=guide_legend(title="Relative risk (95% CI)"))+
  theme(legend.position = "top")  

#----------------------------------------------------------------------#
#NO2 & Mortality
hm<-subset(dat,exposure=="NO2")
hm$Lag=factor(hm$Lag,levels=c("Lag07","Lag06","Lag05","Lag04","Lag03","Lag02","Lag01","Lag00"))
hm$outcome=factor(hm$outcome,levels=unique(hm$outcome))
x11();ggplot(data=hm,aes(outcome,Lag))+geom_tile(aes(fill=RR))+
  theme_bw(base_size=28)+labs(x="",y="")+
  scale_fill_gradientn(colours=c("green","gold","Red"),limits=c(0.996,1.002))+
  geom_text(data=hm,aes(y=Lag,
                        label=paste0(sprintf("%.6f",hm$RR),ifelse(hm$Pval<0.05," *",""),"\n",
                                     "(",
                                     sprintf("%.6f",hm$RR_lci),"-",
                                     sprintf("%.6f",hm$RR_uci),")")),
            size=8)+
  guides(fill=guide_legend(title="Relative risk (95% CI)"))+
  theme(legend.position = "top")  

#----------------------------------------------------------------------#
#CO & Mortality
hm<-subset(dat,exposure=="CO")
hm$Lag=factor(hm$Lag,levels=c("Lag07","Lag06","Lag05","Lag04","Lag03","Lag02","Lag01","Lag00"))
hm$outcome=factor(hm$outcome,levels=unique(hm$outcome))
x11();ggplot(data=hm,aes(outcome,Lag))+geom_tile(aes(fill=RR))+
  theme_bw(base_size=28)+labs(x="",y="")+
  scale_fill_gradientn(colours=c("green","gold","Red"),limits=c(0.996,1.002))+
  geom_text(data=hm,aes(y=Lag,
                        label=paste0(sprintf("%.6f",hm$RR),ifelse(hm$Pval<0.05," *",""),"\n",
                                     "(",
                                     sprintf("%.6f",hm$RR_lci),"-",
                                     sprintf("%.6f",hm$RR_uci),")")),
            size=8)+
  guides(fill=guide_legend(title="Relative risk (95% CI)"))+
  theme(legend.position = "top")  

#----------------------------------------------------------------------#
#O3 & Mortality
hm<-subset(dat,exposure=="O3")
hm$Lag=factor(hm$Lag,levels=c("Lag07","Lag06","Lag05","Lag04","Lag03","Lag02","Lag01","Lag00"))
hm$outcome=factor(hm$outcome,levels=unique(hm$outcome))
x11();ggplot(data=hm,aes(outcome,Lag))+geom_tile(aes(fill=RR))+
  theme_bw(base_size=28)+labs(x="",y="")+
  scale_fill_gradientn(colours=c("green","gold","Red"),limits=c(0.996,1.002))+
  geom_text(data=hm,aes(y=Lag,
                        label=paste0(sprintf("%.6f",hm$RR),ifelse(hm$Pval<0.05," *",""),"\n",
                                     "(",
                                     sprintf("%.6f",hm$RR_lci),"-",
                                     sprintf("%.6f",hm$RR_uci),")")),
            size=8)+
  guides(fill=guide_legend(title="Relative risk (95% CI)"))+
  theme(legend.position = "top")  


#----------------------------------------------------------------------#
#O3 & Mortality
dat<-read_excel("KEI_SNU_TS_analysis_OJM_20230213.xlsx",sheet="통계청사망_여름기간_오존_선형")
hm<-subset(dat,exposure=="O3")
hm$Lag=factor(hm$Lag,levels=c("Lag07","Lag06","Lag05","Lag04","Lag03","Lag02","Lag01","Lag00"))
hm$outcome=factor(hm$outcome,levels=unique(hm$outcome))
x11();ggplot(data=hm,aes(outcome,Lag))+geom_tile(aes(fill=RR))+
  theme_bw(base_size=28)+labs(x="",y="")+
  scale_fill_gradientn(colours=c("green","gold","Red"),limits=c(0.996,1.002))+
  geom_text(data=hm,aes(y=Lag,
                        label=paste0(sprintf("%.6f",hm$RR),ifelse(hm$Pval<0.05," *",""),"\n",
                                     "(",
                                     sprintf("%.6f",hm$RR_lci),"-",
                                     sprintf("%.6f",hm$RR_uci),")")),
            size=8)+
  guides(fill=guide_legend(title="Relative risk (95% CI)"))+
  theme(legend.position = "top")

#----------------------------------------------------------------------#
#----------------------------------------------------------------------#
dat1<-read_excel("KEI_SNU_TS_analysis_OJM_20230213.xlsx",sheet="통계청사망_선형(gamm4)")
dat2<-read_excel("KEI_SNU_TS_analysis_OJM_20230213.xlsx",sheet="통계청사망_여름기간_오존_선형")

dat1<-subset(dat1,exposure!="O3")

dd<-rbind(dat1 %>% select(outcome,exposure,Lag,Pval,RR:RR_uci),
      dat2 %>% select(outcome,exposure,Lag,Pval,RR:RR_uci))

dd$outcome =factor(dd$outcome,levels=unique(dd$outcome))
dd$exposure=factor(dd$exposure,levels=unique(dd$exposure))
dd$Lag=factor(dd$Lag,levels=unique(dd$Lag))

dd<-dd %>% arrange(outcome,exposure)

dd$label=with(dd,paste0(outcome,": ",exposure))
dd$Pval


dd$label=factor(dd$label,levels=rev(unique(dd$label)))

x11();ggplot(data=dd,aes(Lag,label))+geom_tile(aes(fill=RR))+
  theme_bw(base_size=22)+labs(x="",y="")+
  scale_fill_gradientn(colours=c("green","gold","Red"),limits=c(0.996,1.002))+
  geom_text(data=dd,aes(x=Lag,
                        label=paste0(sprintf("%.6f",dd$RR),ifelse(dd$Pval<0.05," *",""),"\n",
                                     "(",
                                     sprintf("%.6f",dd$RR_lci),"-",
                                     sprintf("%.6f",dd$RR_uci),")")),
            size=3)+
  guides(fill=guide_legend(title="Relative risk (95% CI)"))+
  theme(legend.position = "top")

#----------------------------------------------------------------------#
#----------------------------------------------------------------------#
dat<-read_excel("KEI_SNU_TS_analysis_OJM_20230213.xlsx",sheet="공단질환_선형결과(gamm4)")

zz<-subset(dat,exposure=="PM2.5")

zz$categories=factor(zz$categories,levels=rev(c("천식:입원","천식:외래","천식:입원 또는 외래",
                                                "전체 심혈관:입원",
                                                "출혈성 뇌졸중:입원","출혈성 뇌졸중:외래","출혈성 뇌졸중:입원 또는 외래",
                                                "심부전:입원","심부전:외래","심부전:입원 또는 외래",
                                                "고혈압:입원","허혈성심장질환:입원",
                                                "허혈성 뇌졸중:입원","허혈성 뇌졸중:외래","허혈성 뇌졸중:입원 또는 외래",
                                                "어지러움:입원","어지러움:외래","어지러움:입원 또는 외래",
                                                "후각장애:외래","중이염:입원")))
zz$lag=factor(zz$lag,levels=unique(zz$lag))

summary(zz$RR)
x11();ggplot(data=zz,aes(lag,categories))+geom_tile(aes(fill=RR))+
  theme_bw(base_size=22)+labs(x="",y="")+
  scale_fill_gradientn(colours=c("green","gold","Red"),limits=c(0.9994,1.002))+
  geom_text(data=zz,aes(x=lag,
                        label=paste0(sprintf("%.6f",zz$RR),ifelse(zz$Pval<0.05," *",""),"\n",
                                     "(",
                                     sprintf("%.6f",zz$RR_lci),"-",
                                     sprintf("%.6f",zz$RR_uci),")")),
            size=3)+
  guides(fill=guide_legend(title="Relative risk (95% CI)"))+
  theme(legend.position = "top")
