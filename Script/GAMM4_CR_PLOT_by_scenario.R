setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result\\비선형결과\\cr")

d<-read.csv("cr_AP_merge.csv")

#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#
#Figure, 모형적합한 원시자료 지역별 노출 농도별 누적 히스토그램 그림 
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

#------------------------------------------------------------------------------#
ap1<-raw %>% select(pm25_new,pm25_new_m1:pm25_new_m7)
ap2<-raw %>% select(pm10,pm10_m1:pm10_m7)
ap3<-raw %>% select(so2 ,so2_m1:so2_m7)
ap4<-raw %>% select(no2 ,no2_m1:no2_m7)
ap5<-raw %>% select(co  ,co_m1:co_m7)
ap6<-raw %>% select(o3  ,o3_m1:o3_m7)


exp_hist_func<-function(data,k){
  ap<-data
  names(ap)=c("e0","e1","e2","e3","e4","e5","e6","e7")
lag0_hist=as.data.frame(prop.table(table(round(ap$e0,k))))
lag1_hist=as.data.frame(prop.table(table(round(ap$e1,k))))
lag2_hist=as.data.frame(prop.table(table(round(ap$e2,k))))
lag3_hist=as.data.frame(prop.table(table(round(ap$e3,k))))
lag4_hist=as.data.frame(prop.table(table(round(ap$e4,k))))
lag5_hist=as.data.frame(prop.table(table(round(ap$e5,k))))
lag6_hist=as.data.frame(prop.table(table(round(ap$e6,k))))
lag7_hist=as.data.frame(prop.table(table(round(ap$e7,k))))

names(lag0_hist)[1]=c("concentration");lag0_hist$lag="lag00"
names(lag1_hist)[1]=c("concentration");lag1_hist$lag="lag01"
names(lag2_hist)[1]=c("concentration");lag2_hist$lag="lag02"
names(lag3_hist)[1]=c("concentration");lag3_hist$lag="lag03"
names(lag4_hist)[1]=c("concentration");lag4_hist$lag="lag04"
names(lag5_hist)[1]=c("concentration");lag5_hist$lag="lag05"
names(lag6_hist)[1]=c("concentration");lag6_hist$lag="lag06"
names(lag7_hist)[1]=c("concentration");lag7_hist$lag="lag07"

dd.hist<-rbind(lag0_hist,lag1_hist,
      lag2_hist,lag3_hist,
      lag4_hist,lag5_hist,
      lag6_hist,lag7_hist)
dd.hist$concentration=as.numeric(as.character(dd.hist$concentration))
dd.hist
}

exp_hist1<-exp_hist_func(ap1,0)
exp_hist2<-exp_hist_func(ap2,0)
exp_hist3<-exp_hist_func(ap3,3)
exp_hist4<-exp_hist_func(ap4,3)
exp_hist5<-exp_hist_func(ap5,3)
exp_hist6<-exp_hist_func(ap6,3)
#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#
d01<-subset(d,outcome=="All-cause" & exposure=="pm25")
d02<-subset(d,outcome=="All-cause" & exposure=="pm10")
d03<-subset(d,outcome=="All-cause" & exposure=="so2")
d04<-subset(d,outcome=="All-cause" & exposure=="no2")
d05<-subset(d,outcome=="All-cause" & exposure=="co")
d06<-subset(d,outcome=="All-cause" & exposure=="o3")

d07<-subset(d,outcome=="nonacc" & exposure=="pm25")
d08<-subset(d,outcome=="nonacc" & exposure=="pm10")
d09<-subset(d,outcome=="nonacc" & exposure=="so2")
d10<-subset(d,outcome=="nonacc" & exposure=="no2")
d11<-subset(d,outcome=="nonacc" & exposure=="co")
d12<-subset(d,outcome=="nonacc" & exposure=="o3")

d13<-subset(d,outcome=="CVD" & exposure=="pm25")
d14<-subset(d,outcome=="CVD" & exposure=="pm10")
d15<-subset(d,outcome=="CVD" & exposure=="so2")
d16<-subset(d,outcome=="CVD" & exposure=="no2")
d17<-subset(d,outcome=="CVD" & exposure=="co")
d18<-subset(d,outcome=="CVD" & exposure=="o3")

d19<-subset(d,outcome=="respiratory" & exposure=="pm25")
d20<-subset(d,outcome=="respiratory" & exposure=="pm10")
d21<-subset(d,outcome=="respiratory" & exposure=="so2")
d22<-subset(d,outcome=="respiratory" & exposure=="no2")
d23<-subset(d,outcome=="respiratory" & exposure=="co")
d24<-subset(d,outcome=="respiratory" & exposure=="o3")

#ggplot 색깔
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
getPalette(8)

xlabel=NULL
xlabel[[1]]<-expression(paste(PM[2.5]," (",mu,g/m^3,")"))
xlabel[[2]]<-expression(paste(PM[10]," (",mu,g/m^3,")"))
xlabel[[3]]<-expression(paste(SO[2]," (",ppm,")"))
xlabel[[4]]<-expression(paste(NO[2]," (",ppm,")"))
xlabel[[5]]<-expression(paste(CO[]," (",ppm,")"))
xlabel[[6]]<-expression(paste(O[3]," (",ppm,")"))

exp_ref=c(15,45,15/1000,13/1000,3492/1000,30/1000)

#C-R by scenario1,2
ggplot_cr_scenario<-function(data,data2,xlabel,ref,T1,T2){
  
  dd<-data
  
  g1<-ggplot(dd,aes(concentration,beta1,col=lag))+geom_line(size=1.2)+
    theme_bw(base_size=23)+
    coord_cartesian(xlim=c(0,max(dd$concentration)+quantile(dd$concentration,p=c(0.2))))+
    labs(x=xlabel,y="Log RR",title=T1)+
    theme(legend.position = c(0.9,0.65),
          legend.title = element_blank(),
          legend.text = element_text(color = "black", size = 13))+
    geom_vline(xintercept = ref,linetype="dashed")+
    geom_hline(yintercept = 0,lwd=1)+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    scale_colour_manual(values = getPalette(8))
  
  g2<-ggplot(dd,aes(concentration,beta2,col=lag))+geom_line(size=1.2)+
    theme_bw(base_size=23)+
    coord_cartesian(xlim=c(0,max(dd$concentration)+quantile(dd$concentration,p=c(0.2))))+
    labs(x=xlabel,y="Log RR",title=T2)+
    theme(legend.position = c(0.9,0.65),
          legend.title = element_blank(),
          legend.text = element_text(color = "black", size = 13))+
    geom_vline(xintercept = ref,linetype="dashed")+
    geom_hline(yintercept = 0,lwd=1)+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    scale_colour_manual(values = getPalette(8))
  
  g3<-ggplot(data=data2,aes(concentration,Freq,fill=lag))+geom_bar(stat='identity')+
    labs(x=xlabel,y="Log RR")+theme_bw(base_size=15)+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    theme(legend.position = "none",
          legend.title = element_blank())+
    scale_fill_manual(values = getPalette(8))+facet_wrap(~lag,ncol=4)
  
  g3<-g3+geom_line(data=dd,aes(concentration,beta1,col=lag),size=1.2)+
    scale_y_continuous(sec.axis= sec_axis(~., name = "Density"))+
    geom_hline(yintercept = 0,lwd=1)+
    geom_ribbon(aes(ymin=dd$beta1_lci,ymax=dd$beta1_uci),alpha=0.2)+
    scale_color_manual(values = getPalette(8))+facet_wrap(~lag,ncol=4)

  
  g4<-ggplot(data=data2,aes(concentration,Freq,fill=lag))+geom_bar(stat='identity')+
    labs(x=xlabel,y="Log RR")+theme_bw(base_size=15)+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    theme(legend.position = "none",
          legend.title = element_blank())+
    scale_fill_manual(values = getPalette(8))+facet_wrap(~lag,ncol=4)
  
  g4<-g4+geom_line(data=dd,aes(concentration,beta2,col=lag),size=1.2)+
    scale_y_continuous(sec.axis= sec_axis(~., name = "Density"))+
    geom_hline(yintercept = 0,lwd=1)+
    geom_ribbon(aes(ymin=dd$beta2_lci,ymax=dd$beta2_uci),alpha=0.2)+
    scale_color_manual(values = getPalette(8))+facet_wrap(~lag,ncol=4)
  
  x11();grid.arrange(g1,g2,g3,g4,ncol=2)
  
}

#C-R, All-cause death & Air pollution
ggplot_cr_scenario(d01,exp_hist1,xlabel[[1]],exp_ref[1],"S1: All-cause death","S2: All-cause death")
ggplot_cr_scenario(d02,exp_hist2,xlabel[[2]],exp_ref[2],"S1: All-cause death","S2: All-cause death")
ggplot_cr_scenario(d03,exp_hist3,xlabel[[3]],exp_ref[3],"S1: All-cause death","S2: All-cause death")
ggplot_cr_scenario(d04,exp_hist4,xlabel[[4]],exp_ref[4],"S1: All-cause death","S2: All-cause death")
ggplot_cr_scenario(d05,exp_hist5,xlabel[[5]],exp_ref[5],"S1: All-cause death","S2: All-cause death")
ggplot_cr_scenario(d06,exp_hist6,xlabel[[6]],exp_ref[6],"S1: All-cause death","S2: All-cause death")

#C-R, Non-accidental death & Air pollution
ggplot_cr_scenario(d07,exp_hist1,xlabel[[1]],exp_ref[1],"S1: Non-accidental death","S2: Non-accidental death")
ggplot_cr_scenario(d08,exp_hist2,xlabel[[2]],exp_ref[2],"S1: Non-accidental death","S2: Non-accidental death")
ggplot_cr_scenario(d09,exp_hist3,xlabel[[3]],exp_ref[3],"S1: Non-accidental death","S2: Non-accidental death")
ggplot_cr_scenario(d10,exp_hist4,xlabel[[4]],exp_ref[4],"S1: Non-accidental death","S2: Non-accidental death")
ggplot_cr_scenario(d11,exp_hist5,xlabel[[5]],exp_ref[5],"S1: Non-accidental death","S2: Non-accidental death")
ggplot_cr_scenario(d12,exp_hist6,xlabel[[6]],exp_ref[6],"S1: Non-accidental death","S2: Non-accidental death")

#C-R, CVD death & Air pollution
ggplot_cr_scenario(d13,exp_hist1,xlabel[[1]],exp_ref[1],"S1: CVD death","S2: CVD death")
ggplot_cr_scenario(d14,exp_hist2,xlabel[[2]],exp_ref[2],"S1: CVD death","S2: CVD death")
ggplot_cr_scenario(d15,exp_hist3,xlabel[[3]],exp_ref[3],"S1: CVD death","S2: CVD death")
ggplot_cr_scenario(d16,exp_hist4,xlabel[[4]],exp_ref[4],"S1: CVD death","S2: CVD death")
ggplot_cr_scenario(d17,exp_hist5,xlabel[[5]],exp_ref[5],"S1: CVD death","S2: CVD death")
ggplot_cr_scenario(d18,exp_hist6,xlabel[[6]],exp_ref[6],"S1: CVD death","S2: CVD death")

#C-R, Respiratory death & Air pollution
ggplot_cr_scenario(d19,exp_hist1,xlabel[[1]],exp_ref[1],"S1: Respiratory death","S2: Respiratory death")
ggplot_cr_scenario(d20,exp_hist2,xlabel[[2]],exp_ref[2],"S1: Respiratory death","S2: Respiratory death")
ggplot_cr_scenario(d21,exp_hist3,xlabel[[3]],exp_ref[3],"S1: Respiratory death","S2: Respiratory death")
ggplot_cr_scenario(d22,exp_hist4,xlabel[[4]],exp_ref[4],"S1: Respiratory death","S2: Respiratory death")
ggplot_cr_scenario(d23,exp_hist5,xlabel[[5]],exp_ref[5],"S1: Respiratory death","S2: Respiratory death")
ggplot_cr_scenario(d24,exp_hist6,xlabel[[6]],exp_ref[6],"S1: Respiratory death","S2: Respiratory death")

#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#

chist<-ggplot(raw, aes(x =round(pm25_new_m1), fill= factor(sidoname))) +
  geom_histogram(aes(y = ..density..), binwidth = 1) 	+ 
  xlab(expression(paste(PM[2.5]," (",mu,g/m^3,")"))) +ylab("") +
  scale_fill_manual(values = getPalette(17),name="",
                    breaks=unique(raw$sidoname),
                    labels=unique(raw$sidoname))+	
  theme_minimal(base_size=30)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "top")

cr<-ggplot(z,aes(concentration,beta2))+geom_line(size=1.2)+
  geom_hline(yintercept = 0,lwd=1)+
  theme_minimal(base_size=30)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_ribbon(aes(ymin=z$beta2_lci,ymax=z$beta2_uci),alpha=0.2)+
  labs(x="",y="Log RR")

x11();grid.arrange(cr,chist,ncol=1)

z1<-subset(d01,lag=="lag01")
z2<-subset(d02,lag=="lag01")
z3<-subset(d03,lag=="lag01")
z4<-subset(d04,lag=="lag01")
z5<-subset(d05,lag=="lag00")
z6<-subset(d06,lag=="lag05")

z1$sidoname=NA;z2$sidoname=NA;z3$sidoname=NA
z4$sidoname=NA;z5$sidoname=NA;z6$sidoname=NA



#----------------------------------------------------------------------------------------#
#PM2.5 histogram
chist1<-ggplot(raw, aes(x =round(pm25_new_m1), fill= factor(sidoname))) +
  geom_histogram(aes(y = ..density..), binwidth = 1) 	+ 
  xlab(expression(paste(PM[2.5]," (",mu,g/m^3,")"))) +ylab("Density") +
  scale_fill_manual(values = getPalette(17),name="",breaks=unique(raw$sidoname),labels=unique(raw$sidoname))+	
  theme_minimal(base_size=30)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#PM10 histogram
chist2<-ggplot(raw, aes(x =round(pm10_m1), fill= factor(sidoname))) +
  geom_histogram(aes(y = ..density..), binwidth = 1) 	+ 
  xlab(expression(paste(PM[10]," (",mu,g/m^3,")"))) +ylab("Density") +
  scale_fill_manual(values = getPalette(17),name="",breaks=unique(raw$sidoname),labels=unique(raw$sidoname))+	
  theme_minimal(base_size=30)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#SO2 histogram
chist3<-ggplot(subset(raw,so2_m1>0.0005), aes(x =round(so2_m1,3), fill= factor(sidoname))) +
  geom_histogram(aes(y = ..density..), binwidth = 0.003) 	+ 
  xlab(expression(paste(SO[2]," (ppm)"))) +ylab("Density") +
  scale_fill_manual(values = getPalette(17),name="",breaks=unique(raw$sidoname),labels=unique(raw$sidoname))+	
  theme_minimal(base_size=30)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#NO2 histogram
chist4<-ggplot(raw, aes(x =round(no2_m1,3), fill= factor(sidoname))) +
  geom_histogram(aes(y = ..density..), binwidth = 0.004) 	+ 
  xlab(expression(paste(NO[2]," (ppm)"))) +ylab("Density") +
  scale_fill_manual(values = getPalette(17),name="",breaks=unique(raw$sidoname),labels=unique(raw$sidoname))+	
  theme_minimal(base_size=30)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#CO histogram
chist5<-ggplot(raw, aes(x =round(co,3), fill= factor(sidoname))) +
  geom_histogram(aes(y = ..density..), binwidth = 0.03) 	+ 
  xlab(expression(paste(CO[]," (ppm)"))) +ylab("Density") +
  scale_fill_manual(values = getPalette(17),name="",breaks=unique(raw$sidoname),labels=unique(raw$sidoname))+	
  theme_minimal(base_size=30)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#O3 histogram
chist6<-ggplot(raw, aes(x =round(o3,3), fill= factor(sidoname))) +
  geom_histogram(aes(y = ..density..), binwidth = 0.001) 	+ 
  xlab(expression(paste(O[3]," (ppm)"))) +ylab("Density") +
  scale_fill_manual(values = getPalette(17),name="",breaks=unique(raw$sidoname),labels=unique(raw$sidoname))+	
  theme_minimal(base_size=30)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

x11();chist1+geom_line(data=z1,aes(concentration,beta2*10),size=1.2)+
  scale_y_continuous(sec.axis= sec_axis(~./10, name = "log RR"))+
  geom_line(data=z,aes(concentration,beta2_lci*10),linetype="dashed")+
  geom_line(data=z,aes(concentration,beta2_uci*10),linetype="dashed")+
  theme_minimal(base_size=30)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "top")

x11();chist2+geom_line(data=z2,aes(concentration,beta2*10),size=1.2)+
  scale_y_continuous(sec.axis= sec_axis(~./10, name = "log RR"))+
  geom_line(data=z2,aes(concentration,beta2_lci*10),linetype="dashed")+
  geom_line(data=z2,aes(concentration,beta2_uci*10),linetype="dashed")+
  theme_minimal(base_size=30)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "top")


x11();chist3+geom_line(data=z3,aes(concentration,beta2*100000),size=1.2)+
  scale_y_continuous(sec.axis= sec_axis(~./100000, name = "log RR"))+
  geom_line(data=z3,aes(concentration,beta2_lci*100000),linetype="dashed")+
  geom_line(data=z3,aes(concentration,beta2_uci*100000),linetype="dashed")+
  theme_minimal(base_size=30)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "top")

x11();chist4+geom_line(data=z4,aes(concentration,beta2*30000),size=1.2)+
  scale_y_continuous(sec.axis= sec_axis(~./30000, name = "log RR"))+
  geom_line(data=z4,aes(concentration,beta2_lci*30000),linetype="dashed")+
  geom_line(data=z4,aes(concentration,beta2_uci*30000),linetype="dashed")+
  theme_minimal(base_size=30)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "top")

x11();chist5+geom_line(data=z5,aes(concentration,beta2*500),size=1.2)+
  scale_y_continuous(sec.axis= sec_axis(~./500, name = "log RR"))+
  geom_line(data=z5,aes(concentration,beta2_lci*500),linetype="dashed")+
  geom_line(data=z5,aes(concentration,beta2_uci*500),linetype="dashed")+
  theme_minimal(base_size=30)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "top")

x11();chist6+geom_line(data=z6,aes(concentration,beta2*10000),size=1.2)+
  scale_y_continuous(sec.axis= sec_axis(~./10000, name = "log RR"))+
  geom_line(data=z6,aes(concentration,beta2_lci*10000),linetype="dashed")+
  geom_line(data=z6,aes(concentration,beta2_uci*10000),linetype="dashed")+
  theme_minimal(base_size=30)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "top")+coord_cartesian(xlim=c(0,0.1))

#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#
