#library
pacman::p_load(dplyr,ggplot2,lubridate,readxl)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
setwd("C:\\Users\\kard1\\OneDrive\\바탕 화면\\표준화\\data")
pop    <-read_excel("연앙인구_시군구_final_NHIS.xlsx")
std_pop<-read_excel("연앙인구_시군구_final_NHIS.xlsx",sheet=2) %>% 
  select(age_group,std_pop)

#age group
ag=c("age0004","age0509","age1014","age1519","age2024","age2529","age3034","age3539",
     "age4044","age4549","age5054","age5559","age6064","age6569","age7074","age7579",
     "age8084","age85")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
setwd("C:\\Users\\kard1\\OneDrive\\바탕 화면\\표준화\\data\\시군구연도별_유병건수_2010_2019")
lf<-list.files()[1:40]
disease<-gsub("_SGG_1019.csv","",lf)

for(k in 1:40){
  setwd("C:\\Users\\kard1\\OneDrive\\바탕 화면\\표준화\\data\\시군구연도별_유병건수_2010_2019")
  d<-read.csv(list.files()[k],fileEncoding = "euc-kr")
  
  #시군구 결측 있어서 잘못 붙거나, 연도 잘못된거 제외
  d[grep("NA-",d$NHIS_SGGKEY),]$year=as.numeric(gsub("NA-","",d[grep("NA-",d$NHIS_SGGKEY),]$NHIS_SGGKEY))
  #연구기간: 2010~2019 고려
  d         <-subset(d,year %in% c(2010:2019))
  d$nhis_sgg<-gsub("NA-20","NA",d$nhis_sgg)
  
  #표준화 산출시 0세, 1-4세 묶어서(0-4세)하기 (인구자료가 그렇게 있어서 맞춰주기)
  d<-d %>% mutate(H1_AG0004  =H1_AG00+H1_AG0104,
                  H2_AG0004  =H2_AG00+H2_AG0104,
                  H3_AG0004  =H3_AG00+H3_AG0104,
                  H1_M_AG0004=H1_M_AG00+H1_M_AG0104,
                  H2_M_AG0004=H2_M_AG00+H2_M_AG0104,
                  H3_M_AG0004=H3_M_AG00+H3_M_AG0104,
                  H1_F_AG0004=H1_F_AG00+H1_F_AG0104,
                  H2_F_AG0004=H2_F_AG00+H2_F_AG0104,
                  H3_F_AG0004=H3_F_AG00+H3_F_AG0104)
  
  #전체, 남, 여 구분 
  d_H1<-d %>% select(year,nhis_sido,nhis_sgg,SIDO_NM,SGG_NM,names(d[,grep("H1",names(d))]))
  d_H2<-d %>% select(year,nhis_sido,nhis_sgg,SIDO_NM,SGG_NM,names(d[,grep("H2",names(d))]))
  d_H3<-d %>% select(year,nhis_sido,nhis_sgg,SIDO_NM,SGG_NM,names(d[,grep("H3",names(d))]))
  
  d_H1_T<-d %>% select(year,nhis_sido,nhis_sgg,SIDO_NM,SGG_NM,names(d[,grep("H1_A",names(d))]))
  d_H1_M<-d %>% select(year,nhis_sido,nhis_sgg,SIDO_NM,SGG_NM,names(d[,grep("H1_M",names(d))]))
  d_H1_F<-d %>% select(year,nhis_sido,nhis_sgg,SIDO_NM,SGG_NM,names(d[,grep("H1_F",names(d))]))
  
  d_H2_T<-d %>% select(year,nhis_sido,nhis_sgg,SIDO_NM,SGG_NM,names(d[,grep("H2_A",names(d))]))
  d_H2_M<-d %>% select(year,nhis_sido,nhis_sgg,SIDO_NM,SGG_NM,names(d[,grep("H2_M",names(d))]))
  d_H2_F<-d %>% select(year,nhis_sido,nhis_sgg,SIDO_NM,SGG_NM,names(d[,grep("H2_F",names(d))]))
  
  d_H3_T<-d %>% select(year,nhis_sido,nhis_sgg,SIDO_NM,SGG_NM,names(d[,grep("H3_A",names(d))]))
  d_H3_M<-d %>% select(year,nhis_sido,nhis_sgg,SIDO_NM,SGG_NM,names(d[,grep("H3_M",names(d))]))
  d_H3_F<-d %>% select(year,nhis_sido,nhis_sgg,SIDO_NM,SGG_NM,names(d[,grep("H3_F",names(d))]))
  
  d_H1_T<-d_H1_T[complete.cases(d_H1_T$nhis_sido),]
  d_H1_M<-d_H1_M[complete.cases(d_H1_M$nhis_sido),]
  d_H1_F<-d_H1_F[complete.cases(d_H1_F$nhis_sido),]
  d_H2_T<-d_H2_T[complete.cases(d_H2_T$nhis_sido),]
  d_H2_M<-d_H2_M[complete.cases(d_H2_M$nhis_sido),]
  d_H2_F<-d_H2_F[complete.cases(d_H2_F$nhis_sido),]
  d_H3_T<-d_H3_T[complete.cases(d_H3_T$nhis_sido),]
  d_H3_M<-d_H3_M[complete.cases(d_H3_M$nhis_sido),]
  d_H3_F<-d_H3_F[complete.cases(d_H3_F$nhis_sido),]
  
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#
  out.F=NULL
  out.list1=NULL
  out.list2=NULL
  
  func_sgg_std<-function(d1,d2,d3,k){
  
    for(i in 1:2500){
      
      year=paste0("P",d1[i,]$year)
      sgg =d1[i,]$nhis_sgg
      
      tot<-apply(cbind(subset(pop,pop$NHIS_SGG==sgg & sex=="Male") %>% select(year),
                       subset(pop,pop$NHIS_SGG==sgg & sex=="Female") %>% select(year)),1,sum)
      
      z1<-as.data.frame(cbind(t(d1[i,c(25,8:24)]),tot)) %>% mutate(age_group=ag)
      z2<-as.data.frame(cbind(t(d2[i,c(25,8:24)]),
                              subset(pop,pop$NHIS_SGG==sgg & sex=="Male") %>% select(year))) %>% 
        mutate(age_group=ag)
      z3<-as.data.frame(cbind(t(d3[i,c(25,8:24)]),
                              subset(pop,pop$NHIS_SGG==sgg & sex=="Female") %>% select(year))) %>% 
        mutate(age_group=ag)
      
      names(z1)[1:2]=c("cnt","pop")    ;row.names(z1)=NULL
      names(z2)[1:2]=c("cnt_m","pop_m");row.names(z2)=NULL
      names(z3)[1:2]=c("cnt_f","pop_f");row.names(z3)=NULL
      
      z1_t<-z1 %>% summarise(cnt=sum(cnt),pop=sum(pop)) %>% mutate(age_group="tot")
      z2_t<-z2 %>% summarise(cnt_m=sum(cnt_m),pop_m=sum(pop_m)) %>% mutate(age_group="M")
      z3_t<-z3 %>% summarise(cnt_f=sum(cnt_f),pop_f=sum(pop_f)) %>% mutate(age_group="F")
      
      #100,000당 유병률
      z1_rev<-rbind(z1,z1_t) %>% mutate(preval=cnt/pop*100000)
      z2_rev<-rbind(z2,z2_t) %>% mutate(preval=cnt_m/pop_m*100000)
      z3_rev<-rbind(z3,z3_t) %>% mutate(preval=cnt_f/pop_f*100000)
      
      #연령 표준화; 표준인구 집단
      z1_rev$std_pop=c(std_pop$std_pop,sum(std_pop$std_pop))
      z2_rev$std_pop=c(std_pop$std_pop,sum(std_pop$std_pop))
      z3_rev$std_pop=c(std_pop$std_pop,sum(std_pop$std_pop))
      
      z1_rev2<-subset(z1_rev,age_group!="tot")
      z2_rev2<-subset(z2_rev,age_group!="M")
      z3_rev2<-subset(z3_rev,age_group!="F")
      
      z1_rev2$preval_std_pop=with(z1_rev2,cnt/pop*std_pop)
      z2_rev2$preval_std_pop=with(z2_rev2,cnt_m/pop_m*std_pop)
      z3_rev2$preval_std_pop=with(z3_rev2,cnt_f/pop_f*std_pop)
      
      out_preval<-data.frame(pval=z1_rev2$preval,
                             pval_m=z2_rev2$preval,
                             pval_f=z3_rev2$preval,
                             ag=ag)
      
      #연령 표준화 유병률 (인구 100,000명당)
      out<-as.data.frame(rbind(cbind(subset(z1_rev,age_group=="tot")$preval,
                                     subset(z2_rev,age_group=="M")$preval,
                                     subset(z3_rev,age_group=="F")$preval),
                               cbind(sum(z1_rev2$preval_std_pop)/sum(std_pop$std_pop)*100000,
                                     sum(z2_rev2$preval_std_pop)/sum(std_pop$std_pop)*100000,
                                     sum(z3_rev2$preval_std_pop)/sum(std_pop$std_pop)*100000)))
      
      names(out)=c("Total","Male","Female")
      out$category=c("prevalence","std_prevalence")
      out$note    =c("유병률 (100,000당)","연령표준화 유병률 (100,000당)")
      out$disease =disease[k]
      out$year    =d1[i,]$year
      out$SIDO    =d1[i,]$nhis_sido
      out$sgg     =sgg
      out$SIDO_NM =d1[i,]$SIDO_NM
      out$SGG_NM  =d1[i,]$SGG_NM
      
      out_preval$disease =disease[k]
      out_preval$year    =d1[i,]$year
      out_preval$SIDO    =d1[i,]$nhis_sido
      out_preval$sgg     =sgg
      out_preval$SIDO_NM =d1[i,]$SIDO_NM
      out_preval$SGG_NM  =d1[i,]$SGG_NM
      
      out.list1[[i]]<-out
      out.list2[[i]]<-out_preval
      print(i)
    }
    out.F$res1=do.call(rbind,out.list1)
    out.F$res2=do.call(rbind,out.list2)
    
    out.F
  }
  PVAL_H1<-func_sgg_std(d_H1_T,d_H1_M,d_H1_F,k)
  PVAL_H2<-func_sgg_std(d_H2_T,d_H2_M,d_H2_F,k)
  PVAL_H3<-func_sgg_std(d_H3_T,d_H3_M,d_H3_F,k)
  
  PVAL_H1$res1$G="입원"          ;PVAL_H1$res2$G="입원"
  PVAL_H2$res1$G="외래"          ;PVAL_H2$res2$G="외래"
  PVAL_H3$res1$G="입원 또는 외래";PVAL_H3$res2$G="입원 또는 외래"
  
  setwd("C:\\Users\\kard1\\OneDrive\\바탕 화면\\표준화\\data\\outcome")
  write.csv(PVAL_H1$res1,
            file=paste0("PVAL",ifelse(k<10,paste0("0",k),k),"_H1.csv"),
            row.names=F,na="",fileEncoding = "euc-kr")
  write.csv(PVAL_H1$res2,
            file=paste0("PVAL",ifelse(k<10,paste0("0",k),k),"_AG_H1.csv"),
            row.names=F,na="",fileEncoding = "euc-kr")
  write.csv(PVAL_H2$res1,
            file=paste0("PVAL",ifelse(k<10,paste0("0",k),k),"_H2.csv"),
            row.names=F,na="",fileEncoding = "euc-kr")
  write.csv(PVAL_H2$res2,
            file=paste0("PVAL",ifelse(k<10,paste0("0",k),k),"_AG_H2.csv"),
            row.names=F,na="",fileEncoding = "euc-kr")
  write.csv(PVAL_H3$res1,
            file=paste0("PVAL",ifelse(k<10,paste0("0",k),k),"_H3.csv"),
            row.names=F,na="",fileEncoding = "euc-kr")
  write.csv(PVAL_H3$res2,
            file=paste0("PVAL",ifelse(k<10,paste0("0",k),k),"_AG_H3.csv"),
            row.names=F,na="",fileEncoding = "euc-kr")
}