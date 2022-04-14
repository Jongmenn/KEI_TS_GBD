setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\result\\비선형결과\\cr")

d<-read.csv("cr_AP_merge.csv")

d01<-subset(d,outcome=="All-cause" & exposure=="pm25")
d02<-subset(d,outcome=="All-cause" & exposure=="pm10")
d03<-subset(d,outcome=="All-cause" & exposure=="so2")
d04<-subset(d,outcome=="All-cause" & exposure=="no2")
d05<-subset(d,outcome=="All-cause" & exposure=="co")
d06<-subset(d,outcome=="All-cause" & exposure=="o3")

d01 %>% group_by(lag) %>% summarise(n=length(lag))
d07 %>% group_by(lag) %>% summarise(n=length(lag))

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
ggplot_cr_scenario<-function(data,xlabel,ref,T1,T2){
  
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
  
  g3<-ggplot(dd,aes(concentration,beta1,col=lag))+geom_line(size=1.2)+
    theme_bw(base_size=15)+
    labs(x=xlabel,y="Log RR")+
    theme(legend.position = "none",
          legend.title = element_blank())+
    geom_hline(yintercept = 0,lwd=1)+
    geom_ribbon(aes(ymin=beta1_lci,ymax=beta1_uci),alpha=0.2)+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    scale_colour_manual(values = getPalette(8))+facet_wrap(~lag,ncol=4)
  
  g4<-ggplot(dd,aes(concentration,beta2,col=lag))+geom_line(size=1.2)+
    theme_bw(base_size=15)+
    labs(x=xlabel,y="Log RR")+
    theme(legend.position = "none",
          legend.title = element_blank())+
    geom_hline(yintercept = 0,lwd=1)+
    geom_ribbon(aes(ymin=beta2_lci,ymax=beta2_uci),alpha=0.2)+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    scale_colour_manual(values = getPalette(8))+facet_wrap(~lag,ncol=4)
  
  x11();grid.arrange(g1,g2,g3,g4,ncol=2)
  
}

ggplot_cr_scenario(d01,xlabel[[1]],exp_ref[1],"S1: All-cause death","S2: All-cause death")
ggplot_cr_scenario(d02,xlabel[[2]],exp_ref[2],"S1: All-cause death","S2: All-cause death")
ggplot_cr_scenario(d03,xlabel[[3]],exp_ref[3],"S1: All-cause death","S2: All-cause death")
ggplot_cr_scenario(d04,xlabel[[4]],exp_ref[4],"S1: All-cause death","S2: All-cause death")
ggplot_cr_scenario(d05,xlabel[[5]],exp_ref[5],"S1: All-cause death","S2: All-cause death")
ggplot_cr_scenario(d06,xlabel[[6]],exp_ref[6],"S1: All-cause death","S2: All-cause death")
  
ggplot_cr_scenario(d07,xlabel[[1]],exp_ref[1],"S1: Non-accidental death","S2: Non-accidental death")
ggplot_cr_scenario(d08,xlabel[[2]],exp_ref[2],"S1: Non-accidental death","S2: Non-accidental death")
ggplot_cr_scenario(d09,xlabel[[3]],exp_ref[3],"S1: Non-accidental death","S2: Non-accidental death")
ggplot_cr_scenario(d10,xlabel[[4]],exp_ref[4],"S1: Non-accidental death","S2: Non-accidental death")
ggplot_cr_scenario(d11,xlabel[[5]],exp_ref[5],"S1: Non-accidental death","S2: Non-accidental death")
ggplot_cr_scenario(d12,xlabel[[6]],exp_ref[6],"S1: Non-accidental death","S2: Non-accidental death")

ggplot_cr_scenario(d13,xlabel[[1]],exp_ref[1],"S1: CVD death","S2: CVD death")
ggplot_cr_scenario(d14,xlabel[[2]],exp_ref[2],"S1: CVD death","S2: CVD death")
ggplot_cr_scenario(d15,xlabel[[3]],exp_ref[3],"S1: CVD death","S2: CVD death")
ggplot_cr_scenario(d16,xlabel[[4]],exp_ref[4],"S1: CVD death","S2: CVD death")
ggplot_cr_scenario(d17,xlabel[[5]],exp_ref[5],"S1: CVD death","S2: CVD death")
ggplot_cr_scenario(d18,xlabel[[6]],exp_ref[6],"S1: CVD death","S2: CVD death")

ggplot_cr_scenario(d19,xlabel[[1]],exp_ref[1],"S1: Respiratory death","S2: Respiratory death")
ggplot_cr_scenario(d20,xlabel[[2]],exp_ref[2],"S1: Respiratory death","S2: Respiratory death")
ggplot_cr_scenario(d21,xlabel[[3]],exp_ref[3],"S1: Respiratory death","S2: Respiratory death")
ggplot_cr_scenario(d22,xlabel[[4]],exp_ref[4],"S1: Respiratory death","S2: Respiratory death")
ggplot_cr_scenario(d23,xlabel[[5]],exp_ref[5],"S1: Respiratory death","S2: Respiratory death")
ggplot_cr_scenario(d24,xlabel[[6]],exp_ref[6],"S1: Respiratory death","S2: Respiratory death")
