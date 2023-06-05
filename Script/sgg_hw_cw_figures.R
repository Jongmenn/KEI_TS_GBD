r2;n;p
R_adj=1-(1-r2)*((n-1)/(n-p-1))


hw<-read_excel("C:\\Users\\a\\Downloads\\aws20102019_heat_coldwave (1).xlsx",sheet="�ñ���_������_����_�ϼ�")
cw<-read_excel("C:\\Users\\a\\Downloads\\aws20102019_heat_coldwave (1).xlsx",sheet="�ñ���_������_����_�ϼ�")

hw$�ñ���=with(hw,factor(�ñ���,levels=unique(�ñ���)))
cw$�ñ���=with(cw,factor(�ñ���,levels=unique(�ñ���)))

hw_sido01<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="11") %>% select(�ñ���,year2010:year2019)
hw_sido02<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="21") %>% select(�ñ���,year2010:year2019)
hw_sido03<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="22") %>% select(�ñ���,year2010:year2019)
hw_sido04<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="23") %>% select(�ñ���,year2010:year2019)
hw_sido05<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="24") %>% select(�ñ���,year2010:year2019)
hw_sido06<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="25") %>% select(�ñ���,year2010:year2019)
hw_sido07<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="26") %>% select(�ñ���,year2010:year2019)
hw_sido08<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="29") %>% select(�ñ���,year2010:year2019)
hw_sido09<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="31") %>% select(�ñ���,year2010:year2019)
hw_sido10<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="32") %>% select(�ñ���,year2010:year2019)
hw_sido11<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="33") %>% select(�ñ���,year2010:year2019)
hw_sido12<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="34") %>% select(�ñ���,year2010:year2019)
hw_sido13<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="35") %>% select(�ñ���,year2010:year2019)
hw_sido14<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="36") %>% select(�ñ���,year2010:year2019)
hw_sido15<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="37") %>% select(�ñ���,year2010:year2019)
hw_sido16<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="38") %>% select(�ñ���,year2010:year2019)
hw_sido17<-subset(hw,substr(hw$�õ��ڵ�,1,2)=="39") %>% select(�ñ���,year2010:year2019)

cw_sido01<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="11") %>% select(�ñ���,year2010:year2019)
cw_sido02<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="21") %>% select(�ñ���,year2010:year2019)
cw_sido03<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="22") %>% select(�ñ���,year2010:year2019)
cw_sido04<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="23") %>% select(�ñ���,year2010:year2019)
cw_sido05<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="24") %>% select(�ñ���,year2010:year2019)
cw_sido06<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="25") %>% select(�ñ���,year2010:year2019)
cw_sido07<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="26") %>% select(�ñ���,year2010:year2019)
cw_sido08<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="29") %>% select(�ñ���,year2010:year2019)
cw_sido09<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="31") %>% select(�ñ���,year2010:year2019)
cw_sido10<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="32") %>% select(�ñ���,year2010:year2019)
cw_sido11<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="33") %>% select(�ñ���,year2010:year2019)
cw_sido12<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="34") %>% select(�ñ���,year2010:year2019)
cw_sido13<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="35") %>% select(�ñ���,year2010:year2019)
cw_sido14<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="36") %>% select(�ñ���,year2010:year2019)
cw_sido15<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="37") %>% select(�ñ���,year2010:year2019)
cw_sido16<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="38") %>% select(�ñ���,year2010:year2019)
cw_sido17<-subset(cw,substr(cw$�õ��ڵ�,1,2)=="39") %>% select(�ñ���,year2010:year2019)


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

#������ �� ����
hw_col<-c(RColorBrewer::brewer.pal(9,"YlOrRd"),"brown")
cw_col<-c(RColorBrewer::brewer.pal(9,"Blues"),"#081D58")

sgg_col<-c(RColorBrewer::brewer.pal(7 ,"Set2"),
           RColorBrewer::brewer.pal(12,"Paired"),
           RColorBrewer::brewer.pal(9 ,"GnBu"),
           RColorBrewer::brewer.pal(9 ,"Set1"),
           RColorBrewer::brewer.pal(9 ,"PiYG"))

#-------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------#
#���� 
hw.g01.1<-ggplot(hw_long_sido01,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g01.2<-ggplot(hw_long_sido01,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido01)])+guides(col=guide_legend(nrow=3))

cw.g01.1<-ggplot(cw_long_sido01,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g01.2<-ggplot(cw_long_sido01,aes(as.numeric(year),value,col=�ñ���))+
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
#�λ�
hw.g02.1<-ggplot(hw_long_sido02,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g02.2<-ggplot(hw_long_sido02,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido02)])+guides(col=guide_legend(nrow=2))

cw.g02.1<-ggplot(cw_long_sido02,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g02.2<-ggplot(cw_long_sido02,aes(as.numeric(year),value,col=�ñ���))+
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
#�뱸
hw.g03.1<-ggplot(hw_long_sido03,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g03.2<-ggplot(hw_long_sido03,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido03)])+guides(col=guide_legend(nrow=2))

cw.g03.1<-ggplot(cw_long_sido03,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g03.2<-ggplot(cw_long_sido03,aes(as.numeric(year),value,col=�ñ���))+
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
#��õ
hw.g04.1<-ggplot(hw_long_sido04,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g04.2<-ggplot(hw_long_sido04,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido04)])+guides(col=guide_legend(nrow=2))

cw.g04.1<-ggplot(cw_long_sido04,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g04.2<-ggplot(cw_long_sido04,aes(as.numeric(year),value,col=�ñ���))+
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
#����
hw.g05.1<-ggplot(hw_long_sido05,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g05.2<-ggplot(hw_long_sido05,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido05)])+guides(col=guide_legend(nrow=2))

cw.g05.1<-ggplot(cw_long_sido05,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g05.2<-ggplot(cw_long_sido05,aes(as.numeric(year),value,col=�ñ���))+
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
#����
hw.g06.1<-ggplot(hw_long_sido06,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g06.2<-ggplot(hw_long_sido06,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido06)])+guides(col=guide_legend(nrow=2))

cw.g06.1<-ggplot(cw_long_sido06,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g06.2<-ggplot(cw_long_sido06,aes(as.numeric(year),value,col=�ñ���))+
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
#���
hw.g07.1<-ggplot(hw_long_sido07,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g07.2<-ggplot(hw_long_sido07,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido07)])+guides(col=guide_legend(nrow=2))

cw.g07.1<-ggplot(cw_long_sido07,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g07.2<-ggplot(cw_long_sido07,aes(as.numeric(year),value,col=�ñ���))+
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
#����
hw.g08.1<-ggplot(hw_long_sido08,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g08.2<-ggplot(hw_long_sido08,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido08)])+guides(col=guide_legend(nrow=2))

cw.g08.1<-ggplot(cw_long_sido08,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g08.2<-ggplot(cw_long_sido08,aes(as.numeric(year),value,col=�ñ���))+
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
#���
hw.g09.1<-ggplot(hw_long_sido09,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g09.2<-ggplot(hw_long_sido09,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido09)])+guides(col=guide_legend(nrow=5))

cw.g09.1<-ggplot(cw_long_sido09,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g09.2<-ggplot(cw_long_sido09,aes(as.numeric(year),value,col=�ñ���))+
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
#����
hw.g10.1<-ggplot(hw_long_sido10,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g10.2<-ggplot(hw_long_sido10,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido10)])+guides(col=guide_legend(nrow=2))

cw.g10.1<-ggplot(cw_long_sido10,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g10.2<-ggplot(cw_long_sido10,aes(as.numeric(year),value,col=�ñ���))+
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
#���
hw.g11.1<-ggplot(hw_long_sido11,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g11.2<-ggplot(hw_long_sido11,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido11)])+guides(col=guide_legend(nrow=2))

cw.g11.1<-ggplot(cw_long_sido11,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g11.2<-ggplot(cw_long_sido11,aes(as.numeric(year),value,col=�ñ���))+
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
#�泲
hw.g12.1<-ggplot(hw_long_sido12,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g12.2<-ggplot(hw_long_sido12,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido12)])+guides(col=guide_legend(nrow=2))

cw.g12.1<-ggplot(cw_long_sido12,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g12.2<-ggplot(cw_long_sido12,aes(as.numeric(year),value,col=�ñ���))+
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
#����
hw.g13.1<-ggplot(hw_long_sido13,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g13.2<-ggplot(hw_long_sido13,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido13)])+guides(col=guide_legend(nrow=2))

cw.g13.1<-ggplot(cw_long_sido13,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g13.2<-ggplot(cw_long_sido13,aes(as.numeric(year),value,col=�ñ���))+
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
#����
hw.g14.1<-ggplot(hw_long_sido14,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g14.2<-ggplot(hw_long_sido14,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido14)])+guides(col=guide_legend(nrow=2))

cw.g14.1<-ggplot(cw_long_sido14,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g14.2<-ggplot(cw_long_sido14,aes(as.numeric(year),value,col=�ñ���))+
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
#���
hw.g15.1<-ggplot(hw_long_sido15,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g15.2<-ggplot(hw_long_sido15,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido15)])+guides(col=guide_legend(nrow=2))

cw.g15.1<-ggplot(cw_long_sido15,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g15.2<-ggplot(cw_long_sido15,aes(as.numeric(year),value,col=�ñ���))+
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
#�泲
hw_long_sido16$�ñ���=factor(hw_long_sido16$�ñ���,levels=unique(hw_long_sido16$�ñ���))
cw_long_sido16$�ñ���=factor(cw_long_sido16$�ñ���,levels=unique(cw_long_sido16$�ñ���))

hw.g16.1<-ggplot(hw_long_sido16,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g16.2<-ggplot(hw_long_sido16,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido16)])+guides(col=guide_legend(nrow=2))

cw.g16.1<-ggplot(cw_long_sido16,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g16.2<-ggplot(cw_long_sido16,aes(as.numeric(year),value,col=�ñ���))+
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
#����
hw_long_sido17$�ñ���=factor(hw_long_sido17$�ñ���,levels=unique(hw_long_sido17$�ñ���))
cw_long_sido17$�ñ���=factor(cw_long_sido17$�ñ���,levels=unique(cw_long_sido17$�ñ���))

hw.g17.1<-ggplot(hw_long_sido17,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=hw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

hw.g17.2<-ggplot(hw_long_sido17,aes(as.numeric(year),value,col=�ñ���))+
  geom_point(size=3)+geom_line(lwd=1.5)+labs(x="",y="")+
  theme_bw(base_size=30)+theme(legend.position = "top",legend.title = element_blank())+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_x_continuous(breaks=2010:2019)+
  scale_color_manual(values=sgg_col[1:nrow(hw_sido17)])+guides(col=guide_legend(nrow=2))

cw.g17.1<-ggplot(cw_long_sido17,aes(�ñ���,value,fill=year))+
  geom_bar(stat="identity",position = position_dodge(.9))+labs(x="",y="")+
  theme_bw(base_size=32)+theme(legend.position = "top",legend.title = element_blank())+
  scale_fill_manual(values=cw_col)+scale_x_discrete(guide = guide_axis(angle = 90))

cw.g17.2<-ggplot(cw_long_sido17,aes(as.numeric(year),value,col=�ñ���))+
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


