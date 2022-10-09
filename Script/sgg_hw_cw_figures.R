r2;n;p
R_adj=1-(1-r2)*((n-1)/(n-p-1))


hw<-read_excel("C:\\Users\\a\\Downloads\\aws20102019_heat_coldwave (1).xlsx",sheet="시군구_연도별_폭염_일수")
cw<-read_excel("C:\\Users\\a\\Downloads\\aws20102019_heat_coldwave (1).xlsx",sheet="시군구_연도별_한파_일수")

hw$시군구=with(hw,factor(시군구,levels=unique(시군구)))
cw$시군구=with(cw,factor(시군구,levels=unique(시군구)))

hw_sido01<-subset(hw,substr(hw$시도코드,1,2)=="11") %>% select(시군구,year2010:year2019)
hw_sido02<-subset(hw,substr(hw$시도코드,1,2)=="21") %>% select(시군구,year2010:year2019)
hw_sido03<-subset(hw,substr(hw$시도코드,1,2)=="22") %>% select(시군구,year2010:year2019)
hw_sido04<-subset(hw,substr(hw$시도코드,1,2)=="23") %>% select(시군구,year2010:year2019)
hw_sido05<-subset(hw,substr(hw$시도코드,1,2)=="24") %>% select(시군구,year2010:year2019)
hw_sido06<-subset(hw,substr(hw$시도코드,1,2)=="25") %>% select(시군구,year2010:year2019)
hw_sido07<-subset(hw,substr(hw$시도코드,1,2)=="26") %>% select(시군구,year2010:year2019)
hw_sido08<-subset(hw,substr(hw$시도코드,1,2)=="29") %>% select(시군구,year2010:year2019)
hw_sido09<-subset(hw,substr(hw$시도코드,1,2)=="31") %>% select(시군구,year2010:year2019)
hw_sido10<-subset(hw,substr(hw$시도코드,1,2)=="32") %>% select(시군구,year2010:year2019)
hw_sido11<-subset(hw,substr(hw$시도코드,1,2)=="33") %>% select(시군구,year2010:year2019)
hw_sido12<-subset(hw,substr(hw$시도코드,1,2)=="34") %>% select(시군구,year2010:year2019)
hw_sido13<-subset(hw,substr(hw$시도코드,1,2)=="35") %>% select(시군구,year2010:year2019)
hw_sido14<-subset(hw,substr(hw$시도코드,1,2)=="36") %>% select(시군구,year2010:year2019)
hw_sido15<-subset(hw,substr(hw$시도코드,1,2)=="37") %>% select(시군구,year2010:year2019)
hw_sido16<-subset(hw,substr(hw$시도코드,1,2)=="38") %>% select(시군구,year2010:year2019)
hw_sido17<-subset(hw,substr(hw$시도코드,1,2)=="39") %>% select(시군구,year2010:year2019)

cw_sido01<-subset(cw,substr(cw$시도코드,1,2)=="11") %>% select(시군구,year2010:year2019)
cw_sido02<-subset(cw,substr(cw$시도코드,1,2)=="21") %>% select(시군구,year2010:year2019)
cw_sido03<-subset(cw,substr(cw$시도코드,1,2)=="22") %>% select(시군구,year2010:year2019)
cw_sido04<-subset(cw,substr(cw$시도코드,1,2)=="23") %>% select(시군구,year2010:year2019)
cw_sido05<-subset(cw,substr(cw$시도코드,1,2)=="24") %>% select(시군구,year2010:year2019)
cw_sido06<-subset(cw,substr(cw$시도코드,1,2)=="25") %>% select(시군구,year2010:year2019)
cw_sido07<-subset(cw,substr(cw$시도코드,1,2)=="26") %>% select(시군구,year2010:year2019)
cw_sido08<-subset(cw,substr(cw$시도코드,1,2)=="29") %>% select(시군구,year2010:year2019)
cw_sido09<-subset(cw,substr(cw$시도코드,1,2)=="31") %>% select(시군구,year2010:year2019)
cw_sido10<-subset(cw,substr(cw$시도코드,1,2)=="32") %>% select(시군구,year2010:year2019)
cw_sido11<-subset(cw,substr(cw$시도코드,1,2)=="33") %>% select(시군구,year2010:year2019)
cw_sido12<-subset(cw,substr(cw$시도코드,1,2)=="34") %>% select(시군구,year2010:year2019)
cw_sido13<-subset(cw,substr(cw$시도코드,1,2)=="35") %>% select(시군구,year2010:year2019)
cw_sido14<-subset(cw,substr(cw$시도코드,1,2)=="36") %>% select(시군구,year2010:year2019)
cw_sido15<-subset(cw,substr(cw$시도코드,1,2)=="37") %>% select(시군구,year2010:year2019)
cw_sido16<-subset(cw,substr(cw$시도코드,1,2)=="38") %>% select(시군구,year2010:year2019)
cw_sido17<-subset(cw,substr(cw$시도코드,1,2)=="39") %>% select(시군구,year2010:year2019)


hw_long_sido01<-melt(hw_sido01) %>% mutate(year=substr(variable,5,8))
hw_long_sido02<-melt(hw_sido02) %>% mutate(year=substr(variable,5,8))
hw_long_sido03<-melt(hw_sido03) %>% mutate(year=substr(variable,5,8))
hw_long_sido04<-melt(hw_sido04) %>% mutate(year=substr(variable,5,8))
hw_long_sido05<-melt(hw_sido05) %>% mutate(year=substr(variable,5,8))
hw_long_sido06<-melt(hw_sido06) %>% mutate(year=substr(variable,5,8))
hw_long_sido07<-melt(hw_sido07) %>% mutate(year=substr(variable,5,8))
hw_long_sido08<-melt(hw_sido08) %>% mutate(year=substr(variable,5,8))
hw_long_sido09<-melt(hw_sido09) %>% mutate(year=substr(variable,5,8))
hw_long_sido10<-melt(hw_sido10) %>% mutate(year=substr(variable,5,8))
hw_long_sido11<-melt(hw_sido11) %>% mutate(year=substr(variable,5,8))
hw_long_sido12<-melt(hw_sido12) %>% mutate(year=substr(variable,5,8))
hw_long_sido13<-melt(hw_sido13) %>% mutate(year=substr(variable,5,8))
hw_long_sido14<-melt(hw_sido14) %>% mutate(year=substr(variable,5,8))
hw_long_sido15<-melt(hw_sido15) %>% mutate(year=substr(variable,5,8))
hw_long_sido16<-melt(hw_sido16) %>% mutate(year=substr(variable,5,8))
hw_long_sido17<-melt(hw_sido17) %>% mutate(year=substr(variable,5,8))


cw_long_sido01<-melt(cw_sido01) %>% mutate(year=substr(variable,5,8))
cw_long_sido02<-melt(cw_sido02) %>% mutate(year=substr(variable,5,8))
cw_long_sido03<-melt(cw_sido03) %>% mutate(year=substr(variable,5,8))
cw_long_sido04<-melt(cw_sido04) %>% mutate(year=substr(variable,5,8))
cw_long_sido05<-melt(cw_sido05) %>% mutate(year=substr(variable,5,8))
cw_long_sido06<-melt(cw_sido06) %>% mutate(year=substr(variable,5,8))
cw_long_sido07<-melt(cw_sido07) %>% mutate(year=substr(variable,5,8))
cw_long_sido08<-melt(cw_sido08) %>% mutate(year=substr(variable,5,8))
cw_long_sido09<-melt(cw_sido09) %>% mutate(year=substr(variable,5,8))
cw_long_sido10<-melt(cw_sido10) %>% mutate(year=substr(variable,5,8))
cw_long_sido11<-melt(cw_sido11) %>% mutate(year=substr(variable,5,8))
cw_long_sido12<-melt(cw_sido12) %>% mutate(year=substr(variable,5,8))
cw_long_sido13<-melt(cw_sido13) %>% mutate(year=substr(variable,5,8))
cw_long_sido14<-melt(cw_sido14) %>% mutate(year=substr(variable,5,8))
cw_long_sido15<-melt(cw_sido15) %>% mutate(year=substr(variable,5,8))
cw_long_sido16<-melt(cw_sido16) %>% mutate(year=substr(variable,5,8))
cw_long_sido17<-melt(cw_sido17) %>% mutate(year=substr(variable,5,8))

#폭염인 날 색깔
hw_col<-c(RColorBrewer::brewer.pal(9,"YlOrRd"),"brown")
cw_col<-c(RColorBrewer::brewer.pal(9,"Blues"),"#081D58")

sgg_col<-c(RColorBrewer::brewer.pal(7 ,"Set2"),
           RColorBrewer::brewer.pal(12,"Paired"),
           RColorBrewer::brewer.pal(9 ,"GnBu"),
           RColorBrewer::brewer.pal(9 ,"Set1"),
           RColorBrewer::brewer.pal(9 ,"PiYG"))

#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#서울 
hw.g01.1<-ggplot(hw_long_sido01,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g01.2<-ggplot(hw_long_sido01,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido01)])+guides(col=guide_legend(nrow=3))

cw.g01.1<-ggplot(cw_long_sido01,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g01.2<-ggplot(cw_long_sido01,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido01)])+guides(col=guide_legend(nrow=3))

x11();hw.g01.1
x11();hw.g01.2
x11();cw.g01.1
x11();cw.g01.2
#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#부산
hw.g02.1<-ggplot(hw_long_sido02,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g02.2<-ggplot(hw_long_sido02,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido02)])+guides(col=guide_legend(nrow=2))

cw.g02.1<-ggplot(cw_long_sido02,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g02.2<-ggplot(cw_long_sido02,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido02)])+guides(col=guide_legend(nrow=2))

x11();hw.g02.1
x11();hw.g02.2
x11();cw.g02.1
x11();cw.g02.2
#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#대구
hw.g03.1<-ggplot(hw_long_sido03,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g03.2<-ggplot(hw_long_sido03,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido03)])+guides(col=guide_legend(nrow=2))

cw.g03.1<-ggplot(cw_long_sido03,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g03.2<-ggplot(cw_long_sido03,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido03)])+guides(col=guide_legend(nrow=2))

x11();hw.g03.1
x11();hw.g03.2
x11();cw.g03.1
x11();cw.g03.2
#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#인천
hw.g04.1<-ggplot(hw_long_sido04,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g04.2<-ggplot(hw_long_sido04,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido04)])+guides(col=guide_legend(nrow=2))

cw.g04.1<-ggplot(cw_long_sido04,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g04.2<-ggplot(cw_long_sido04,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido04)])+guides(col=guide_legend(nrow=2))

x11();hw.g04.1
x11();hw.g04.2
x11();cw.g04.1
x11();cw.g04.2
#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#광주
hw.g05.1<-ggplot(hw_long_sido05,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g05.2<-ggplot(hw_long_sido05,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido05)])+guides(col=guide_legend(nrow=2))

cw.g05.1<-ggplot(cw_long_sido05,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g05.2<-ggplot(cw_long_sido05,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido05)])+guides(col=guide_legend(nrow=2))

x11();hw.g05.1
x11();hw.g05.2
x11();cw.g05.1
x11();cw.g05.2
#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#광주
hw.g06.1<-ggplot(hw_long_sido06,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g06.2<-ggplot(hw_long_sido06,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido06)])+guides(col=guide_legend(nrow=2))

cw.g06.1<-ggplot(cw_long_sido06,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g06.2<-ggplot(cw_long_sido06,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido06)])+guides(col=guide_legend(nrow=2))

x11();hw.g06.1
x11();hw.g06.2
x11();cw.g06.1
x11();cw.g06.2
#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#울산
hw.g07.1<-ggplot(hw_long_sido07,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g07.2<-ggplot(hw_long_sido07,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido07)])+guides(col=guide_legend(nrow=2))

cw.g07.1<-ggplot(cw_long_sido07,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g07.2<-ggplot(cw_long_sido07,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido07)])+guides(col=guide_legend(nrow=2))

x11();hw.g07.1
x11();hw.g07.2
x11();cw.g07.1
x11();cw.g07.2
#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#세종
hw.g08.1<-ggplot(hw_long_sido08,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g08.2<-ggplot(hw_long_sido08,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido08)])+guides(col=guide_legend(nrow=2))

cw.g08.1<-ggplot(cw_long_sido08,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g08.2<-ggplot(cw_long_sido08,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido08)])+guides(col=guide_legend(nrow=2))

x11();hw.g08.1
x11();hw.g08.2
x11();cw.g08.1
x11();cw.g08.2

x11();grid.arrange(hw.g08.1,hw.g08.2)
x11();grid.arrange(cw.g08.1,cw.g08.2)

#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#경기
hw.g09.1<-ggplot(hw_long_sido09,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g09.2<-ggplot(hw_long_sido09,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido09)])+guides(col=guide_legend(nrow=5))

cw.g09.1<-ggplot(cw_long_sido09,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g09.2<-ggplot(cw_long_sido09,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido09)])+guides(col=guide_legend(nrow=5))

x11();hw.g09.1
x11();hw.g09.2
x11();cw.g09.1
x11();cw.g09.2

#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#강원
hw.g10.1<-ggplot(hw_long_sido10,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g10.2<-ggplot(hw_long_sido10,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido10)])+guides(col=guide_legend(nrow=2))

cw.g10.1<-ggplot(cw_long_sido10,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g10.2<-ggplot(cw_long_sido10,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido10)])+guides(col=guide_legend(nrow=2))

x11();hw.g10.1
x11();hw.g10.2
x11();cw.g10.1
x11();cw.g10.2
#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#충북
hw.g11.1<-ggplot(hw_long_sido11,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g11.2<-ggplot(hw_long_sido11,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido11)])+guides(col=guide_legend(nrow=2))

cw.g11.1<-ggplot(cw_long_sido11,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g11.2<-ggplot(cw_long_sido11,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido11)])+guides(col=guide_legend(nrow=2))

x11();hw.g11.1
x11();hw.g11.2
x11();cw.g11.1
x11();cw.g11.2
#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#충남
hw.g12.1<-ggplot(hw_long_sido12,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g12.2<-ggplot(hw_long_sido12,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido12)])+guides(col=guide_legend(nrow=2))

cw.g12.1<-ggplot(cw_long_sido12,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g12.2<-ggplot(cw_long_sido12,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido12)])+guides(col=guide_legend(nrow=2))

x11();hw.g12.1
x11();hw.g12.2
x11();cw.g12.1
x11();cw.g12.2
#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#전북
hw.g13.1<-ggplot(hw_long_sido13,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g13.2<-ggplot(hw_long_sido13,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido13)])+guides(col=guide_legend(nrow=2))

cw.g13.1<-ggplot(cw_long_sido13,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g13.2<-ggplot(cw_long_sido13,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido13)])+guides(col=guide_legend(nrow=2))

x11();hw.g13.1
x11();hw.g13.2
x11();cw.g13.1
x11();cw.g13.2
#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#전남
hw.g14.1<-ggplot(hw_long_sido14,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g14.2<-ggplot(hw_long_sido14,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido14)])+guides(col=guide_legend(nrow=2))

cw.g14.1<-ggplot(cw_long_sido14,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g14.2<-ggplot(cw_long_sido14,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido14)])+guides(col=guide_legend(nrow=2))

x11();hw.g14.1
x11();hw.g14.2
x11();cw.g14.1
x11();cw.g14.2
#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#경북
hw.g15.1<-ggplot(hw_long_sido15,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g15.2<-ggplot(hw_long_sido15,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido15)])+guides(col=guide_legend(nrow=2))

cw.g15.1<-ggplot(cw_long_sido15,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g15.2<-ggplot(cw_long_sido15,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido15)])+guides(col=guide_legend(nrow=2))

x11();hw.g15.1
x11();hw.g15.2
x11();cw.g15.1
x11();cw.g15.2

#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#경남
hw_long_sido16$시군구=factor(hw_long_sido16$시군구,levels=unique(hw_long_sido16$시군구))
cw_long_sido16$시군구=factor(cw_long_sido16$시군구,levels=unique(cw_long_sido16$시군구))

hw.g16.1<-ggplot(hw_long_sido16,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g16.2<-ggplot(hw_long_sido16,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido16)])+guides(col=guide_legend(nrow=2))

cw.g16.1<-ggplot(cw_long_sido16,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g16.2<-ggplot(cw_long_sido16,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido16)])+guides(col=guide_legend(nrow=2))

x11();hw.g16.1
x11();hw.g16.2
x11();cw.g16.1
x11();cw.g16.2

#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#제주
hw_long_sido17$시군구=factor(hw_long_sido17$시군구,levels=unique(hw_long_sido17$시군구))
cw_long_sido17$시군구=factor(cw_long_sido17$시군구,levels=unique(cw_long_sido17$시군구))

hw.g17.1<-ggplot(hw_long_sido17,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g17.2<-ggplot(hw_long_sido17,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido17)])+guides(col=guide_legend(nrow=2))

cw.g17.1<-ggplot(cw_long_sido17,aes(시군구,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g17.2<-ggplot(cw_long_sido17,aes(as.numeric(year),value,col=시군구))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido17)])+guides(col=guide_legend(nrow=2))

x11();hw.g17.1
x11();hw.g17.2
x11();cw.g17.1
x11();cw.g17.2

x11();grid.arrange(hw.g17.1,hw.g17.2)
x11();grid.arrange(cw.g17.1,cw.g17.2)



