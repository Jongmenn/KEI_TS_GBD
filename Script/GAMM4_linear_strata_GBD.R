library(dplyr)
library(stringr)

sub_gamm4_linear_func<-function(i,j){
  
  #working directory
  setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\단기질병부담\\gamm4_linear\\strata")
  
  #GAMM4 (linear association assumption), subgroup 결과 폴더
  listdirs<-list.dirs()[2:13]
  
  #디렉토리 
  dirs<-paste0(getwd(),substr(listdirs[i],2,nchar(listdirs[i])))
  
  #디렉토리 변경 
  setwd(dirs)
  
  #디렉토리 내 파일, GAMM results
  lf<-list.files()
  
  #GAM 결과 load
  g0<-readRDS(lf[j]) #j 로 변경해주기 
  
  tb<-as.data.frame(t(summary(g0$gam)$p.table[2,]))
  
  #노출명, PM25는 NEW -> 제거하기 
  ex_lab <-str_to_upper(names(g0$gam$model)[2]) 
  ex_lab<-gsub("PM25_NEW","PM25",ex_lab)
  
  #질환명_서브그룹 가져와서 수정 
  dis_lab<-str_to_upper(names(g0$gam$model)[1])
  
  #최종 결과 자료에 붙일 레이블 수정 
  exposure_label<-str_split(ex_lab,"_")[[1]][1]
  lag_label     <-str_split(ex_lab,"_")[[1]][2]
  lag_label     <-gsub("M","lag0",lag_label)
  
  disease_label <-str_split(dis_lab,"_")[[1]][[1]]
  subgroup_label<-str_split(dis_lab,"_")[[1]][[2]]
  
  #미세먼지면 1 ug/m3, 그외 대기오염 농도면 0.001 ppm (1 ppb)
  unit<-ifelse(exposure_label=="PM25",1,ifelse(exposure_label=="PM10",1,0.001))
  
  #모델링을 ppb 단위로 수행해서, 단위변환 필요
  #PM2.5, PM10, SO2, NO2, O3 적용 
  #CO는 100으로 변경할 필요 있음 
  
  unit_inc<-ifelse(unit==1,1,1000)
  unit_lab=ifelse(unit==1,"ug/m3","ppm")
  
  data.frame(outcome =disease_label,
             exposure=exposure_label,
             subgroup=subgroup_label,
             lag=lag_label,tb[c(1,2,4)],
             unit,단위=unit_lab,
             R2=summary(g0$gam)$r.sq) %>% mutate(SE=Std..Error,
                                                 Pval=Pr...z..,
                                                 RR=exp(Estimate/unit_inc),
                                                 RR_lci=exp(Estimate/unit_inc-1.96*SE/unit_inc),
                                                 RR_uci=exp(Estimate/unit_inc+1.96*SE/unit_inc)) %>% 
    select(outcome:Estimate,SE,Pval,unit,RR:RR_uci,단위,R2)
}

#총 디렉토리 12개
#각 개별결과 40개 (8(lag0~lag07) * 그룹 수 5 (연령(남/여), 연령그룹(15세미만, 15-64세, 65세 이상)))

res01=NULL;res02=NULL;res03=NULL;res04=NULL
res05=NULL;res06=NULL;res07=NULL;res08=NULL
res09=NULL;res10=NULL;res11=NULL;res12=NULL

for(i in 1:40){
  
  res01[[i]]<-sub_gamm4_linear_func(1,i)
  res02[[i]]<-sub_gamm4_linear_func(2,i)
  res03[[i]]<-sub_gamm4_linear_func(3,i)
  res04[[i]]<-sub_gamm4_linear_func(4,i)
  res05[[i]]<-sub_gamm4_linear_func(5,i)
  res06[[i]]<-sub_gamm4_linear_func(6,i)
  res07[[i]]<-sub_gamm4_linear_func(7,i)
  res08[[i]]<-sub_gamm4_linear_func(8,i)
  res09[[i]]<-sub_gamm4_linear_func(9,i)
  res10[[i]]<-sub_gamm4_linear_func(10,i)
  res11[[i]]<-sub_gamm4_linear_func(11,i)
  res12[[i]]<-sub_gamm4_linear_func(12,i)
  print(i)
}

res.df01<-as.data.frame(do.call(rbind,res01))
res.df02<-as.data.frame(do.call(rbind,res02))
res.df03<-as.data.frame(do.call(rbind,res03))
res.df04<-as.data.frame(do.call(rbind,res04))
res.df05<-as.data.frame(do.call(rbind,res05))
res.df06<-as.data.frame(do.call(rbind,res06))
res.df07<-as.data.frame(do.call(rbind,res07))
res.df08<-as.data.frame(do.call(rbind,res08))
res.df09<-as.data.frame(do.call(rbind,res09))
res.df10<-as.data.frame(do.call(rbind,res10))
res.df11<-as.data.frame(do.call(rbind,res11))
res.df12<-as.data.frame(do.call(rbind,res12))

setwd("D:\\SNU\\연구\\KEI_환경보건감시체계\\분석\\work")
write.csv(res.df01,file="res.df01.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(res.df02,file="res.df02.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(res.df03,file="res.df03.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(res.df04,file="res.df04.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(res.df05,file="res.df05.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(res.df06,file="res.df06.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(res.df07,file="res.df07.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(res.df08,file="res.df08.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(res.df09,file="res.df09.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(res.df10,file="res.df10.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(res.df11,file="res.df11.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(res.df12,file="res.df12.csv",row.names=F,na="",fileEncoding = "euc-kr")
