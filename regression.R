library(tidyverse)
# GAD-7 as sample

#-----regression code R-----------

#1000 individuals as demo sample
A_S_PRS<-read.table("q_UK_anscore2_demo.txt",header=T)
d<-read.table("imd_mental_demo.txt",header = T,sep=" ")
PRS_data3<-d[,c(1,17,34,35:44,28,29,49:50,6,30:33,9:16)]
PRS_data_1<-full_join(PRS_data3,A_S_PRS,by=c("IID","FID"))
PRS_data_anscore<-PRS_data_1[,c(1:17,22:32)]
nPRS_data_anscore<-na.omit(PRS_data_anscore)

nPRS_data_anscore2<-nPRS_data_anscore
#anscore
nPRS_data_anscore2<-nPRS_data_anscore %>%
  arrange(PRS.x) %>%
  mutate(tertiles=ntile(PRS.x,3)) %>%
  mutate(tertiles=if_else(tertiles==1,'Low',if_else(tertiles==2,'Medium','High')))
nPRS_data_anscore2<-nPRS_data_anscore2 %>%
  arrange(nPRS_data_anscore2$imd) %>%
  mutate(dimd=ntile(imd,4))

nPRS_data_anscore2$dimd<-factor(nPRS_data_anscore2$dimd,levels=c(1,2,3,4),
                                labels=c("Quartile1","Quartile2","Quartile3","Quartile4"))


r1<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+PC1.y+PC2.y+PC3.y+PC4.y+PC5.y+PC6.y+PC7.y+PC8.y+PC9.y+PC10.y+imd+PRS.x+imd*PRS.x,data=nPRS_data_anscore2)
r2<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+imd+PRS.x+imd*PRS.x,data=nPRS_data_anscore2)
r3<-glm(anxiety_score.y~imd+PRS.x+imd*PRS.x,data=nPRS_data_anscore2)
summary(r1)
summary(r2)
summary(r3)


sink("imdanscore.txt")
r1<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+PC1.y+PC2.y+PC3.y+PC4.y+PC5.y+PC6.y+PC7.y+PC8.y+PC9.y+PC10.y+imd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Low'),])
r2<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+imd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Low'),])
r3<-glm(anxiety_score.y~imd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Low'),])
r4<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+PC1.y+PC2.y+PC3.y+PC4.y+PC5.y+PC6.y+PC7.y+PC8.y+PC9.y+PC10.y+imd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Medium'),])
r5<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+imd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Medium'),])
r6<-glm(anxiety_score.y~imd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Medium'),])
r7<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+PC1.y+PC2.y+PC3.y+PC4.y+PC5.y+PC6.y+PC7.y+PC8.y+PC9.y+PC10.y+imd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='High'),])
r8<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+imd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='High'),])
r9<-glm(anxiety_score.y~imd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='High'),])
summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)
summary(r7)
summary(r8)
summary(r9)
sink()

sink("4imdanscore.txt")
r1<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+PC1.y+PC2.y+PC3.y+PC4.y+PC5.y+PC6.y+PC7.y+PC8.y+PC9.y+PC10.y+dimd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Low'),])
r2<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+dimd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Low'),])
r3<-glm(anxiety_score.y~dimd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Low'),])
r4<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+PC1.y+PC2.y+PC3.y+PC4.y+PC5.y+PC6.y+PC7.y+PC8.y+PC9.y+PC10.y+dimd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Medium'),])
r5<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+dimd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Medium'),])
r6<-glm(anxiety_score.y~dimd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Medium'),])
r7<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+PC1.y+PC2.y+PC3.y+PC4.y+PC5.y+PC6.y+PC7.y+PC8.y+PC9.y+PC10.y+dimd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='High'),])
r8<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+dimd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='High'),])
r9<-glm(anxiety_score.y~dimd,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='High'),])
summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)
summary(r7)
summary(r8)
summary(r9)
sink()



#income as sample
nPRS_data_anscore2<-nPRS_data_anscore2 %>%
  arrange(nPRS_data_anscore2$income) %>%
  mutate(income2=ntile(income,4))
nPRS_data_anscore2$income2<-factor(nPRS_data_anscore2$income2,levels=c(1,2,3,4),
                                   labels=c("Qincome1","Qincome2","Qincome3","Qincome4"))


sink("anscoreincome.txt")
r1<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+PC1.y+PC2.y+PC3.y+PC4.y+PC5.y+PC6.y+PC7.y+PC8.y+PC9.y+PC10.y+income,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Low'),])
r2<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+income,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Low'),])
r3<-glm(anxiety_score.y~income,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Low'),])
r4<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+PC1.y+PC2.y+PC3.y+PC4.y+PC5.y+PC6.y+PC7.y+PC8.y+PC9.y+PC10.y+income,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Medium'),])
r5<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+income,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Medium'),])
r6<-glm(anxiety_score.y~income,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Medium'),])
r7<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+PC1.y+PC2.y+PC3.y+PC4.y+PC5.y+PC6.y+PC7.y+PC8.y+PC9.y+PC10.y+income,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='High'),])
r8<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+income,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='High'),])
r9<-glm(anxiety_score.y~income,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='High'),])
summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)
summary(r7)
summary(r8)
summary(r9)
sink()

sink("anscoreincomrfen4.txt")
r1<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+PC1.y+PC2.y+PC3.y+PC4.y+PC5.y+PC6.y+PC7.y+PC8.y+PC9.y+PC10.y+income2,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Low'),])
r2<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+income2,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Low'),])
r3<-glm(anxiety_score.y~income2,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Low'),])
r4<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+PC1.y+PC2.y+PC3.y+PC4.y+PC5.y+PC6.y+PC7.y+PC8.y+PC9.y+PC10.y+income2,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Medium'),])
r5<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+income2,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Medium'),])
r6<-glm(anxiety_score.y~income2,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='Medium'),])
r7<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+PC1.y+PC2.y+PC3.y+PC4.y+PC5.y+PC6.y+PC7.y+PC8.y+PC9.y+PC10.y+income2,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='High'),])
r8<-glm(anxiety_score.y~age.y+sex.y+alcohol.freq.week+smoke.freq.day+income2,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='High'),])
r9<-glm(anxiety_score.y~income2,data=nPRS_data_anscore2[(nPRS_data_anscore2$tertiles=='High'),])
summary(r1)
summary(r2)
summary(r3)
summary(r4)
summary(r5)
summary(r6)
summary(r7)
summary(r8)
summary(r9)
sink()
