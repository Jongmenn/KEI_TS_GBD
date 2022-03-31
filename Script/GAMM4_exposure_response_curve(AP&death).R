#GAMM4, exposure-response plot

pacman::p_load(gamm4,mgcv,lme4,splines,ggplot2,gridExtra,dplyr,lubridate,scales,
               RColorBrewer)

#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\gamm4\\all_cause")

#전체 원인사망: 초미세먼지
g1<-readRDS("gamm4_tot_pm25_m0.rds");g2<-readRDS("gamm4_tot_pm25_m1.rds")
g3<-readRDS("gamm4_tot_pm25_m2.rds");g4<-readRDS("gamm4_tot_pm25_m3.rds")
g5<-readRDS("gamm4_tot_pm25_m4.rds");g6<-readRDS("gamm4_tot_pm25_m5.rds")
g7<-readRDS("gamm4_tot_pm25_m6.rds");g8<-readRDS("gamm4_tot_pm25_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="All-cause, Lag 00",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g2$gam,select=1,main="All-cause, Lag 01",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g3$gam,select=1,main="All-cause, Lag 02",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g4$gam,select=1,main="All-cause, Lag 03",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g5$gam,select=1,main="All-cause, Lag 04",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g6$gam,select=1,main="All-cause, Lag 05",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g7$gam,select=1,main="All-cause, Lag 06",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g8$gam,select=1,main="All-cause, Lag 07",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 원인사망: 미세먼지
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_tot_pm10_m0.rds");g2<-readRDS("gamm4_tot_pm10_m1.rds")
g3<-readRDS("gamm4_tot_pm10_m2.rds");g4<-readRDS("gamm4_tot_pm10_m3.rds")
g5<-readRDS("gamm4_tot_pm10_m4.rds");g6<-readRDS("gamm4_tot_pm10_m5.rds")
g7<-readRDS("gamm4_tot_pm10_m6.rds");g8<-readRDS("gamm4_tot_pm10_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="All-cause, Lag 00",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.05,0.15));abline(h=0,col="red")
plot(g2$gam,select=1,main="All-cause, Lag 01",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.05,0.15));abline(h=0,col="red")
plot(g3$gam,select=1,main="All-cause, Lag 02",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.05,0.15));abline(h=0,col="red")
plot(g4$gam,select=1,main="All-cause, Lag 03",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.05,0.15));abline(h=0,col="red")
plot(g5$gam,select=1,main="All-cause, Lag 04",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.05,0.15));abline(h=0,col="red")
plot(g6$gam,select=1,main="All-cause, Lag 05",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.05,0.15));abline(h=0,col="red")
plot(g7$gam,select=1,main="All-cause, Lag 06",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.05,0.15));abline(h=0,col="red")
plot(g8$gam,select=1,main="All-cause, Lag 07",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.05,0.15));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 원인사망: 이산화황 
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_tot_so2_m0.rds");g2<-readRDS("gamm4_tot_so2_m1.rds")
g3<-readRDS("gamm4_tot_so2_m2.rds");g4<-readRDS("gamm4_tot_so2_m3.rds")
g5<-readRDS("gamm4_tot_so2_m4.rds");g6<-readRDS("gamm4_tot_so2_m5.rds")
g7<-readRDS("gamm4_tot_so2_m6.rds");g8<-readRDS("gamm4_tot_so2_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="All-cause, Lag 00",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g2$gam,select=1,main="All-cause, Lag 01",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g3$gam,select=1,main="All-cause, Lag 02",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g4$gam,select=1,main="All-cause, Lag 03",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g5$gam,select=1,main="All-cause, Lag 04",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g6$gam,select=1,main="All-cause, Lag 05",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g7$gam,select=1,main="All-cause, Lag 06",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g8$gam,select=1,main="All-cause, Lag 07",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 원인사망: 이산화질소
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_tot_no2_m0.rds");g2<-readRDS("gamm4_tot_no2_m1.rds")
g3<-readRDS("gamm4_tot_no2_m2.rds");g4<-readRDS("gamm4_tot_no2_m3.rds")
g5<-readRDS("gamm4_tot_no2_m4.rds");g6<-readRDS("gamm4_tot_no2_m5.rds")
g7<-readRDS("gamm4_tot_no2_m6.rds");g8<-readRDS("gamm4_tot_no2_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="All-cause, Lag 00",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g2$gam,select=1,main="All-cause, Lag 01",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g3$gam,select=1,main="All-cause, Lag 02",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g4$gam,select=1,main="All-cause, Lag 03",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g5$gam,select=1,main="All-cause, Lag 04",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g6$gam,select=1,main="All-cause, Lag 05",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g7$gam,select=1,main="All-cause, Lag 06",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g8$gam,select=1,main="All-cause, Lag 07",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 원인사망: 일산화탄소 
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_tot_co_m0.rds");g2<-readRDS("gamm4_tot_co_m1.rds")
g3<-readRDS("gamm4_tot_co_m2.rds");g4<-readRDS("gamm4_tot_co_m3.rds")
g5<-readRDS("gamm4_tot_co_m4.rds");g6<-readRDS("gamm4_tot_co_m5.rds")
g7<-readRDS("gamm4_tot_co_m6.rds");g8<-readRDS("gamm4_tot_co_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="All-cause, Lag 00",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g2$gam,select=1,main="All-cause, Lag 01",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g3$gam,select=1,main="All-cause, Lag 02",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g4$gam,select=1,main="All-cause, Lag 03",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g5$gam,select=1,main="All-cause, Lag 04",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g6$gam,select=1,main="All-cause, Lag 05",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g7$gam,select=1,main="All-cause, Lag 06",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g8$gam,select=1,main="All-cause, Lag 07",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 원인사망: 오존
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_tot_o3_m0.rds");g2<-readRDS("gamm4_tot_o3_m1.rds")
g3<-readRDS("gamm4_tot_o3_m2.rds");g4<-readRDS("gamm4_tot_o3_m3.rds")
g5<-readRDS("gamm4_tot_o3_m4.rds");g6<-readRDS("gamm4_tot_o3_m5.rds")
g7<-readRDS("gamm4_tot_o3_m6.rds");g8<-readRDS("gamm4_tot_o3_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="All-cause, Lag 00",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g2$gam,select=1,main="All-cause, Lag 01",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g3$gam,select=1,main="All-cause, Lag 02",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g4$gam,select=1,main="All-cause, Lag 03",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g5$gam,select=1,main="All-cause, Lag 04",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g6$gam,select=1,main="All-cause, Lag 05",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g7$gam,select=1,main="All-cause, Lag 06",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g8$gam,select=1,main="All-cause, Lag 07",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\gamm4\\nonacc")

#전체 비사고사망: 초미세먼지
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_nonacc_pm25_m0.rds");g2<-readRDS("gamm4_nonacc_pm25_m1.rds")
g3<-readRDS("gamm4_nonacc_pm25_m2.rds");g4<-readRDS("gamm4_nonacc_pm25_m3.rds")
g5<-readRDS("gamm4_nonacc_pm25_m4.rds");g6<-readRDS("gamm4_nonacc_pm25_m5.rds")
g7<-readRDS("gamm4_nonacc_pm25_m6.rds");g8<-readRDS("gamm4_nonacc_pm25_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="Nonacc, Lag 00",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g2$gam,select=1,main="Nonacc, Lag 01",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g3$gam,select=1,main="Nonacc, Lag 02",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g4$gam,select=1,main="Nonacc, Lag 03",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g5$gam,select=1,main="Nonacc, Lag 04",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g6$gam,select=1,main="Nonacc, Lag 05",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g7$gam,select=1,main="Nonacc, Lag 06",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g8$gam,select=1,main="Nonacc, Lag 07",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 비사고사망: 미세먼지
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_nonacc_pm10_m0.rds");g2<-readRDS("gamm4_nonacc_pm10_m1.rds")
g3<-readRDS("gamm4_nonacc_pm10_m2.rds");g4<-readRDS("gamm4_nonacc_pm10_m3.rds")
g5<-readRDS("gamm4_nonacc_pm10_m4.rds");g6<-readRDS("gamm4_nonacc_pm10_m5.rds")
g7<-readRDS("gamm4_nonacc_pm10_m6.rds");g8<-readRDS("gamm4_nonacc_pm10_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="Nonacc, Lag 00",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g2$gam,select=1,main="Nonacc, Lag 01",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g3$gam,select=1,main="Nonacc, Lag 02",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g4$gam,select=1,main="Nonacc, Lag 03",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g5$gam,select=1,main="Nonacc, Lag 04",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g6$gam,select=1,main="Nonacc, Lag 05",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g7$gam,select=1,main="Nonacc, Lag 06",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g8$gam,select=1,main="Nonacc, Lag 07",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 비사고사망: 이산화황 
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_nonacc_so2_m0.rds");g2<-readRDS("gamm4_nonacc_so2_m1.rds")
g3<-readRDS("gamm4_nonacc_so2_m2.rds");g4<-readRDS("gamm4_nonacc_so2_m3.rds")
g5<-readRDS("gamm4_nonacc_so2_m4.rds");g6<-readRDS("gamm4_nonacc_so2_m5.rds")
g7<-readRDS("gamm4_nonacc_so2_m6.rds");g8<-readRDS("gamm4_nonacc_so2_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="Nonacc, Lag 00",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g2$gam,select=1,main="Nonacc, Lag 01",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g3$gam,select=1,main="Nonacc, Lag 02",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g4$gam,select=1,main="Nonacc, Lag 03",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g5$gam,select=1,main="Nonacc, Lag 04",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g6$gam,select=1,main="Nonacc, Lag 05",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g7$gam,select=1,main="Nonacc, Lag 06",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g8$gam,select=1,main="Nonacc, Lag 07",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 비사고사망: 이산화질소
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_nonacc_no2_m0.rds");g2<-readRDS("gamm4_nonacc_no2_m1.rds")
g3<-readRDS("gamm4_nonacc_no2_m2.rds");g4<-readRDS("gamm4_nonacc_no2_m3.rds")
g5<-readRDS("gamm4_nonacc_no2_m4.rds");g6<-readRDS("gamm4_nonacc_no2_m5.rds")
g7<-readRDS("gamm4_nonacc_no2_m6.rds");g8<-readRDS("gamm4_nonacc_no2_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="Nonacc, Lag 00",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g2$gam,select=1,main="Nonacc, Lag 01",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g3$gam,select=1,main="Nonacc, Lag 02",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g4$gam,select=1,main="Nonacc, Lag 03",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g5$gam,select=1,main="Nonacc, Lag 04",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g6$gam,select=1,main="Nonacc, Lag 05",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g7$gam,select=1,main="Nonacc, Lag 06",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g8$gam,select=1,main="Nonacc, Lag 07",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 비사고사망: 일산화탄소 
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_nonacc_co_m0.rds");g2<-readRDS("gamm4_nonacc_co_m1.rds")
g3<-readRDS("gamm4_nonacc_co_m2.rds");g4<-readRDS("gamm4_nonacc_co_m3.rds")
g5<-readRDS("gamm4_nonacc_co_m4.rds");g6<-readRDS("gamm4_nonacc_co_m5.rds")
g7<-readRDS("gamm4_nonacc_co_m6.rds");g8<-readRDS("gamm4_nonacc_co_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="Nonacc, Lag 00",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g2$gam,select=1,main="Nonacc, Lag 01",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g3$gam,select=1,main="Nonacc, Lag 02",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g4$gam,select=1,main="Nonacc, Lag 03",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g5$gam,select=1,main="Nonacc, Lag 04",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g6$gam,select=1,main="Nonacc, Lag 05",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g7$gam,select=1,main="Nonacc, Lag 06",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g8$gam,select=1,main="Nonacc, Lag 07",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 비사고사망: 오존
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_nonacc_o3_m0.rds");g2<-readRDS("gamm4_nonacc_o3_m1.rds")
g3<-readRDS("gamm4_nonacc_o3_m2.rds");g4<-readRDS("gamm4_nonacc_o3_m3.rds")
g5<-readRDS("gamm4_nonacc_o3_m4.rds");g6<-readRDS("gamm4_nonacc_o3_m5.rds")
g7<-readRDS("gamm4_nonacc_o3_m6.rds");g8<-readRDS("gamm4_nonacc_o3_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="Nonacc, Lag 00",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g2$gam,select=1,main="Nonacc, Lag 01",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g3$gam,select=1,main="Nonacc, Lag 02",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g4$gam,select=1,main="Nonacc, Lag 03",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g5$gam,select=1,main="Nonacc, Lag 04",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g6$gam,select=1,main="Nonacc, Lag 05",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g7$gam,select=1,main="Nonacc, Lag 06",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g8$gam,select=1,main="Nonacc, Lag 07",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\gamm4\\cvd")

#전체 심혈관사망: 초미세먼지
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_cvd_pm25_m0.rds");g2<-readRDS("gamm4_cvd_pm25_m1.rds")
g3<-readRDS("gamm4_cvd_pm25_m2.rds");g4<-readRDS("gamm4_cvd_pm25_m3.rds")
g5<-readRDS("gamm4_cvd_pm25_m4.rds");g6<-readRDS("gamm4_cvd_pm25_m5.rds")
g7<-readRDS("gamm4_cvd_pm25_m6.rds");g8<-readRDS("gamm4_cvd_pm25_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="CVD, Lag 00",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g2$gam,select=1,main="CVD, Lag 01",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g3$gam,select=1,main="CVD, Lag 02",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g4$gam,select=1,main="CVD, Lag 03",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g5$gam,select=1,main="CVD, Lag 04",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g6$gam,select=1,main="CVD, Lag 05",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g7$gam,select=1,main="CVD, Lag 06",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g8$gam,select=1,main="CVD, Lag 07",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 심혈관사망: 미세먼지
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_cvd_pm10_m0.rds");g2<-readRDS("gamm4_cvd_pm10_m1.rds")
g3<-readRDS("gamm4_cvd_pm10_m2.rds");g4<-readRDS("gamm4_cvd_pm10_m3.rds")
g5<-readRDS("gamm4_cvd_pm10_m4.rds");g6<-readRDS("gamm4_cvd_pm10_m5.rds")
g7<-readRDS("gamm4_cvd_pm10_m6.rds");g8<-readRDS("gamm4_cvd_pm10_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="CVD, Lag 00",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g2$gam,select=1,main="CVD, Lag 01",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g3$gam,select=1,main="CVD, Lag 02",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g4$gam,select=1,main="CVD, Lag 03",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g5$gam,select=1,main="CVD, Lag 04",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g6$gam,select=1,main="CVD, Lag 05",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g7$gam,select=1,main="CVD, Lag 06",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g8$gam,select=1,main="CVD, Lag 07",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 심혈관사망: 이산화황 
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_cvd_so2_m0.rds");g2<-readRDS("gamm4_cvd_so2_m1.rds")
g3<-readRDS("gamm4_cvd_so2_m2.rds");g4<-readRDS("gamm4_cvd_so2_m3.rds")
g5<-readRDS("gamm4_cvd_so2_m4.rds");g6<-readRDS("gamm4_cvd_so2_m5.rds")
g7<-readRDS("gamm4_cvd_so2_m6.rds");g8<-readRDS("gamm4_cvd_so2_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="CVD, Lag 00",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g2$gam,select=1,main="CVD, Lag 01",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g3$gam,select=1,main="CVD, Lag 02",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g4$gam,select=1,main="CVD, Lag 03",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g5$gam,select=1,main="CVD, Lag 04",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g6$gam,select=1,main="CVD, Lag 05",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g7$gam,select=1,main="CVD, Lag 06",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g8$gam,select=1,main="CVD, Lag 07",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 심혈관사망: 이산화질소
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_cvd_no2_m0.rds");g2<-readRDS("gamm4_cvd_no2_m1.rds")
g3<-readRDS("gamm4_cvd_no2_m2.rds");g4<-readRDS("gamm4_cvd_no2_m3.rds")
g5<-readRDS("gamm4_cvd_no2_m4.rds");g6<-readRDS("gamm4_cvd_no2_m5.rds")
g7<-readRDS("gamm4_cvd_no2_m6.rds");g8<-readRDS("gamm4_cvd_no2_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="CVD, Lag 00",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g2$gam,select=1,main="CVD, Lag 01",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g3$gam,select=1,main="CVD, Lag 02",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g4$gam,select=1,main="CVD, Lag 03",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g5$gam,select=1,main="CVD, Lag 04",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g6$gam,select=1,main="CVD, Lag 05",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g7$gam,select=1,main="CVD, Lag 06",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g8$gam,select=1,main="CVD, Lag 07",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 심혈관사망: 일산화탄소 
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_cvd_co_m0.rds");g2<-readRDS("gamm4_cvd_co_m1.rds")
g3<-readRDS("gamm4_cvd_co_m2.rds");g4<-readRDS("gamm4_cvd_co_m3.rds")
g5<-readRDS("gamm4_cvd_co_m4.rds");g6<-readRDS("gamm4_cvd_co_m5.rds")
g7<-readRDS("gamm4_cvd_co_m6.rds");g8<-readRDS("gamm4_cvd_co_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="CVD, Lag 00",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g2$gam,select=1,main="CVD, Lag 01",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g3$gam,select=1,main="CVD, Lag 02",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g4$gam,select=1,main="CVD, Lag 03",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g5$gam,select=1,main="CVD, Lag 04",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g6$gam,select=1,main="CVD, Lag 05",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g7$gam,select=1,main="CVD, Lag 06",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g8$gam,select=1,main="CVD, Lag 07",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 심혈관사망: 오존
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_cvd_o3_m0.rds");g2<-readRDS("gamm4_cvd_o3_m1.rds")
g3<-readRDS("gamm4_cvd_o3_m2.rds");g4<-readRDS("gamm4_cvd_o3_m3.rds")
g5<-readRDS("gamm4_cvd_o3_m4.rds");g6<-readRDS("gamm4_cvd_o3_m5.rds")
g7<-readRDS("gamm4_cvd_o3_m6.rds");g8<-readRDS("gamm4_cvd_o3_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="CVD, Lag 00",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")
plot(g2$gam,select=1,main="CVD, Lag 01",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")
plot(g3$gam,select=1,main="CVD, Lag 02",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")
plot(g4$gam,select=1,main="CVD, Lag 03",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")
plot(g5$gam,select=1,main="CVD, Lag 04",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")
plot(g6$gam,select=1,main="CVD, Lag 05",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")
plot(g7$gam,select=1,main="CVD, Lag 06",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")
plot(g8$gam,select=1,main="CVD, Lag 07",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#
setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\gamm4\\respiratory")
list.files()
#전체 심혈관사망: 초미세먼지
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_resp_pm25_m0.rds");g2<-readRDS("gamm4_resp_pm25_m1.rds")
g3<-readRDS("gamm4_resp_pm25_m2.rds");g4<-readRDS("gamm4_resp_pm25_m3.rds")
g5<-readRDS("gamm4_resp_pm25_m4.rds");g6<-readRDS("gamm4_resp_pm25_m5.rds")
g7<-readRDS("gamm4_resp_pm25_m6.rds");g8<-readRDS("gamm4_resp_pm25_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="Respiratory, Lag 00",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g2$gam,select=1,main="Respiratory, Lag 01",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g3$gam,select=1,main="Respiratory, Lag 02",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g4$gam,select=1,main="Respiratory, Lag 03",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g5$gam,select=1,main="Respiratory, Lag 04",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g6$gam,select=1,main="Respiratory, Lag 05",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g7$gam,select=1,main="Respiratory, Lag 06",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")
plot(g8$gam,select=1,main="Respiratory, Lag 07",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.15));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 심혈관사망: 미세먼지
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_resp_pm10_m0.rds");g2<-readRDS("gamm4_resp_pm10_m1.rds")
g3<-readRDS("gamm4_resp_pm10_m2.rds");g4<-readRDS("gamm4_resp_pm10_m3.rds")
g5<-readRDS("gamm4_resp_pm10_m4.rds");g6<-readRDS("gamm4_resp_pm10_m5.rds")
g7<-readRDS("gamm4_resp_pm10_m6.rds");g8<-readRDS("gamm4_resp_pm10_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="Respiratory, Lag 00",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g2$gam,select=1,main="Respiratory, Lag 01",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g3$gam,select=1,main="Respiratory, Lag 02",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g4$gam,select=1,main="Respiratory, Lag 03",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g5$gam,select=1,main="Respiratory, Lag 04",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g6$gam,select=1,main="Respiratory, Lag 05",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g7$gam,select=1,main="Respiratory, Lag 06",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g8$gam,select=1,main="Respiratory, Lag 07",xlab=expression(paste(PM[10]," (",mu,g/m^3,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 심혈관사망: 이산화황 
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_resp_so2_m0.rds");g2<-readRDS("gamm4_resp_so2_m1.rds")
g3<-readRDS("gamm4_resp_so2_m2.rds");g4<-readRDS("gamm4_resp_so2_m3.rds")
g5<-readRDS("gamm4_resp_so2_m4.rds");g6<-readRDS("gamm4_resp_so2_m5.rds")
g7<-readRDS("gamm4_resp_so2_m6.rds");g8<-readRDS("gamm4_resp_so2_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="Respiratory, Lag 00",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g2$gam,select=1,main="Respiratory, Lag 01",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g3$gam,select=1,main="Respiratory, Lag 02",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g4$gam,select=1,main="Respiratory, Lag 03",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g5$gam,select=1,main="Respiratory, Lag 04",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g6$gam,select=1,main="Respiratory, Lag 05",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g7$gam,select=1,main="Respiratory, Lag 06",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g8$gam,select=1,main="Respiratory, Lag 07",xlab=expression(paste(SO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 심혈관사망: 이산화질소
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_resp_no2_m0.rds");g2<-readRDS("gamm4_resp_no2_m1.rds")
g3<-readRDS("gamm4_resp_no2_m2.rds");g4<-readRDS("gamm4_resp_no2_m3.rds")
g5<-readRDS("gamm4_resp_no2_m4.rds");g6<-readRDS("gamm4_resp_no2_m5.rds")
g7<-readRDS("gamm4_resp_no2_m6.rds");g8<-readRDS("gamm4_resp_no2_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="Respiratory, Lag 00",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g2$gam,select=1,main="Respiratory, Lag 01",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g3$gam,select=1,main="Respiratory, Lag 02",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g4$gam,select=1,main="Respiratory, Lag 03",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g5$gam,select=1,main="Respiratory, Lag 04",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g6$gam,select=1,main="Respiratory, Lag 05",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g7$gam,select=1,main="Respiratory, Lag 06",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")
plot(g8$gam,select=1,main="Respiratory, Lag 07",xlab=expression(paste(NO[2]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.08,0.08));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 심혈관사망: 일산화탄소 
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_resp_co_m0.rds");g2<-readRDS("gamm4_resp_co_m1.rds")
g3<-readRDS("gamm4_resp_co_m2.rds");g4<-readRDS("gamm4_resp_co_m3.rds")
g5<-readRDS("gamm4_resp_co_m4.rds");g6<-readRDS("gamm4_resp_co_m5.rds")
g7<-readRDS("gamm4_resp_co_m6.rds");g8<-readRDS("gamm4_resp_co_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="Respiratory, Lag 00",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g2$gam,select=1,main="Respiratory, Lag 01",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g3$gam,select=1,main="Respiratory, Lag 02",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g4$gam,select=1,main="Respiratory, Lag 03",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g5$gam,select=1,main="Respiratory, Lag 04",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g6$gam,select=1,main="Respiratory, Lag 05",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g7$gam,select=1,main="Respiratory, Lag 06",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(g8$gam,select=1,main="Respiratory, Lag 07",xlab=expression(paste(CO[]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.2,0.2));abline(h=0,col="red")

#-----------------------------------------------------------------------------------------#
#전체 심혈관사망: 오존
rm(g1);rm(g2);rm(g3);rm(g4);rm(g5);rm(g6);rm(g7)
g1<-readRDS("gamm4_resp_o3_m0.rds");g2<-readRDS("gamm4_resp_o3_m1.rds")
g3<-readRDS("gamm4_resp_o3_m2.rds");g4<-readRDS("gamm4_resp_o3_m3.rds")
g5<-readRDS("gamm4_resp_o3_m4.rds");g6<-readRDS("gamm4_resp_o3_m5.rds")
g7<-readRDS("gamm4_resp_o3_m6.rds");g8<-readRDS("gamm4_resp_o3_m7.rds")

x11();par(mfrow=c(2,4))
plot(g1$gam,select=1,main="Respiratory, Lag 00",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")
plot(g2$gam,select=1,main="Respiratory, Lag 01",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")
plot(g3$gam,select=1,main="Respiratory, Lag 02",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")
plot(g4$gam,select=1,main="Respiratory, Lag 03",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")
plot(g5$gam,select=1,main="Respiratory, Lag 04",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")
plot(g6$gam,select=1,main="Respiratory, Lag 05",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")
plot(g7$gam,select=1,main="Respiratory, Lag 06",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")
plot(g8$gam,select=1,main="Respiratory, Lag 07",xlab=expression(paste(O[3]," (",ppm,")")),ylab="Log RR",cex.axis=1.45,cex.lab=1.45,cex.main=2,ylim=c(-0.1,0.1));abline(h=0,col="red")

