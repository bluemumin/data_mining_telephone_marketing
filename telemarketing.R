#부  록 (R code & SAS code)

#R Part
#0. 사용 패키지 정리
#install.packages("ggplot2")
library(ggplot2)
#install.packages("randomForest")
library(randomForest)
#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)
#install.packages("party")
library(party)
#install.packages("Epi")
library(Epi)
#install.packages("ROCR")
library(ROCR)
#install.packages("car")
library(car)


#1. 데이터 불러오기 및 전체적인 자료 파악 
bank<read.csv("C:/Users/bluedice/Desktop/BankData/bank-full.csv",sep=";")
head(bank)
str(bank)
summary(bank)



#2. 전체적인 시각화 작업을 통한 이상치 파악 제거(기본적인 변수만 파악 작업)
bank$tk<-1 
#ggplot에 갯수 추가하려고 잠깐 넣은 변수

#a. 연령
table(bank$age)
hist(bank$age,breaks=seq(10,100,by=10),axes=F,ann=F,labels=T)
axis(1,at=seq(10,100,10))
axis(2,ylim=c(0,20100))
title(xlab="연령대",cex.lab=2)
title(ylab="인원 수",cex.lab=2)
title(main="은행 이용 고객 연령 분포",cex.main=2)

#b. 직업
h<-ggplot(bank, aes(x=job)) +geom_bar() + theme_bw() + theme(axis.text.x=element_text(angle=45, hjust=1,size=15)) 
h2<-h+scale_x_discrete(limits=c("blue-collar","management","technician","admin.","services","retired","self-employed","entrepreneur","unemployed","housemaid","student","unknown"))
#h3<-h2+ ggtitle("은행 이용 고객의 직업 종류") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "black")) 
h3 <- h2 + geom_text(aes(label=..count..), stat="count", vjust=1.0, colour="white")
h3+ labs(x="직업 종류", y="인원수")+theme(axis.title = element_text(face = "bold", size = 18, color = "black")) 


#c. 결혼 유무
table(bank$marital)
prop.table( table(bank$marital,bank$y),1)
bar<-ggplot(bank, aes(x=marital,y=bank$tk, fill=bank$y))
bar2<-bar+geom_bar(stat="identity", position="fill", width=0.7)
bar3 <- bar2+theme_bw()
#bar3<-bar2 + ggtitle("결혼 여부 별 캠페인 비율") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
bar4<-bar3+labs(x="혼인 상태", y="예금 구매 여부")+theme(axis.title = element_text(face = "bold", size = 20, color = "black")) 
bar5<-bar4+labs(fill="예금 구매 여부")+theme(legend.title=element_text(face = "bold", size = 15, color = "black"))
bar6<-bar5+geom_text(x=1, y=0.03, label="0.1194", alpha=.1)+geom_text(x=2, y=0.03, label="0.1012", alpha=.1) +geom_text(x=3, y=0.03, label="0.1494", alpha=.1) 
bar5+scale_fill_manual(values=c("#A0A19D", "#4F5458"))
#이혼,사별-> 싱글

bank2<-bank
bank2[bank2$marital=="divorced",3]<-"single"
table(bank2$marital)
prop.table( table(bank2$marital,bank$y),1)
bar<-ggplot(bank2, aes(x=marital,y=bank2$tk, fill=bank2$y))
bar2<-bar+geom_bar(stat="identity", position="fill")
bar3<-bar2 + ggtitle("결혼 여부 별 캠페인 비율") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
bar4<-bar3+labs(x="결혼 여부", y="현재 등록 비율")+theme(axis.title = element_text(face = "bold", size = 18, color = "darkblue")) 
bar5<-bar4+labs(fill="현재 등록 여부")+theme(legend.title=element_text(face = "bold", size = 12, color = "black"))
bar5 +geom_text(x=1, y=0.08, label="0.1012", alpha=.1) +geom_text(x=2, y=0.08, label="0.1408", alpha=.1) 
#큰 차이 없음 #범주형 변수에서 또다른 그룹화 필요로 보임

#d 교육
e1<-ggplot(bank, aes(x=education)) +geom_bar() + theme_bw() + theme(axis.text.x=element_text(angle=15, hjust=1,size=14)) 
e2<-e1+scale_x_discrete(limits=c("secondary","tertiary","primary","unknown") )
#e3<-e2 + ggtitle("은행 이용 고객의 학력") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
e4<-e2+labs(x="학력", y="인원수")+theme(axis.title = element_text(face = "bold", size = 20, color = "black")) 
e4+geom_text(aes(label=..count..), stat="count", vjust=1.4, colour="white", size=5)
#e. default
e2<-ggplot(bank, aes(x=default)) +geom_bar() + theme_bw() + theme(axis.text.x=element_text(size=14)) 
#e3<-e2 + ggtitle("채무 이행 별 인원수") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
e4<-e2+labs(x="채무 불이행 여부", y="인원수")+theme(axis.title = element_text(face = "bold", size = 20, color = "black"))
e4+geom_text(aes(label=..count..), stat="count", vjust=-0.4, colour="black", size=5)
bar<-ggplot(bank, aes(x=default,y=bank$tk, fill=bank$y))
bar2<-bar+geom_bar(stat="identity", position="fill")
bar3<-bar2 + ggtitle("채무 이행 별 캠페인 비율") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
bar4<-bar3+labs(x="채무 이행 여부", y="현재 등록 비율")+theme(axis.title = element_text(face = "bold", size = 18, color = "darkblue"))
bar4+labs(fill="현재 등록 여부")+theme(legend.title=element_text(face = "bold", size = 12, color = "black"))
#일단 갯수에서 차이가 심하게 나는데 이 변수를 기준으로 y의 yes,no를 확인해봐도 극단적인 변화차이는 없어보여서 #샘플링 과정에서도 차이가 없으면 제거하여도 될 것으로 보인다.


#f. balance
summary(bank$balance)
hist(bank$balance)
quantile(bank$balance, probs=c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95,0.975,0.99))
#qunatile에서 이정도로 극단적인 값을 가지는 1%미만의 값들을 제거하고 수행하는게 오히려 분포 구성이나 표준화 작업에서 유리해보인다
#극이상치 제거작업
bank4<-bank
bank4<-bank4[(bank4$balance>=-627 & bank4$balance<=13164.9),]
dim(bank4[(bank4$balance<-627 | bank4$balance>13164.9),])
boxplot(bank4$balance)
boxplot(bank4$balance~bank4$y)#약간 차이
hist(bank$balance,axes=F,ann=F,labels=T,col="#4F5458")
axis(1,at=seq(-10000,110000,5000))
axis(2,ylim=c(0,36000))
title(xlab="평균 연간 잔고(유로)",cex.lab=1.5)
title(ylab="인원 수",cex.lab=1.5)
title(main="은행 이용 고객 잔고 분포",cex.main=2)

#g. housing,
table(bank$housing)
bar<-ggplot(bank, aes(x=housing,y=bank$tk, fill=bank$y))
bar2<-bar+geom_bar(stat="identity", position="fill", width=0.6)
#bar3<-bar2 + ggtitle("집 채무 별 캠페인 비율") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
bar3<-bar2+theme_bw()
bar4<-bar3+labs(x="주택담보대출 여부", y="예금 구매 여부")+theme(axis.title = element_text(face = "bold", size = 20, color = "black"))
bar5<-bar4+labs(fill="예금 구매 여부")+theme(legend.title=element_text(face = "bold", size = 15, color = "black"))
bar5+scale_fill_manual(values=c("#A0A19D", "#4F5458"))
e2<-ggplot(bank, aes(x=housing)) +geom_bar() + theme_bw() + theme(axis.text.x=element_text(angle=0, hjust=1,size=14)) 
e3<-e2 + ggtitle("집 채무 별 인원수") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
e3+labs(x="집 채무 여부", y="인원수")+theme(axis.title = element_text(face = "bold", size = 18, color = "darkblue")) 

#h. loan
table(bank$loan)
bar<-ggplot(bank, aes(x=loan,y=bank$tk, fill=bank$y))
bar2<-bar+geom_bar(stat="identity", position="fill")
#bar3<-bar2 + ggtitle("개인 채무 별 캠페인 비율") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
bar3<-bar2+theme_bw()
bar4<-bar3+labs(x="개인 대출 여부", y="예금 구매 여부")+theme(axis.title = element_text(face = "bold", size = 20, color = "black"))
bar5<-bar4+labs(fill="예금 구매 여부")+theme(legend.title=element_text(face = "bold", size = 15, color = "black"))
bar5+scale_fill_manual(values=c("#A0A19D", "#4F5458"))
e2<-ggplot(bank, aes(x=loan)) +geom_bar() + theme_bw() + theme(axis.text.x=element_text(angle=0, hjust=1,size=14)) 
e3<-e2 + ggtitle("개인 채무 별 인원수") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
e3+labs(x="개인 채무 여부", y="인원수")+theme(axis.title = element_text(face = "bold", size = 18, color = "darkblue"))

#i. contact(마지막 연락 방식)
table(bank$contact)
bar<-ggplot(bank, aes(x=contact,y=bank$tk, fill=bank$y))
bar2<-bar+geom_bar(stat="identity", position="fill")
#bar3<-bar2 + ggtitle("연락수단 별 캠페인 비율") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
bar3<-bar2+theme_bw()
bar4<-bar3+labs(x="연락 수단", y="현재 등록 비율")+theme(axis.title = element_text(face = "bold", size = 18, color = "darkblue"))
bar4+labs(fill="현재 등록 여부")+theme(legend.title=element_text(face = "bold", size = 12, color = "black"))
e2<-ggplot(bank, aes(x=contact)) +geom_bar() + theme_bw() + theme(axis.text.x=element_text(size=14)) 
#e3<-e2 + ggtitle("채무 이행 별 인원수") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
e4<-e2+labs(x="연락 수단", y="인원수")+theme(axis.title = element_text(face = "bold", size = 20, color = "black"))
e4+geom_text(aes(label=..count..), stat="count", vjust=-0.4, colour="black", size=5)

#j. 일별, 월별 (day,month)
bank$year<-2008
bank[27730:42591,"year"]<-2009
bank[42592:45211,"year"]<-2010
bank$year2 <- paste(bank$year,bank$month)
table(bank$year2)
#k. duration(지속시간)
summary(bank$duration)
box2<-ggplot(bank,aes(x=y,y=duration,fill=y))+geom_boxplot()
#box3<-box2 + ggtitle("현 캠페인 등록 여부 별 지속시간") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
box3<-box2+theme_bw()
box4<-box3+labs(x="예금 구매 여부", y="지속 시간")+theme(axis.title = element_text(face = "bold", size = 20, color = "black"))
box5<-box4+labs(fill="예금 구매 여부")+theme(legend.title=element_text(face = "bold", size = 15, color = "black"))
box5+scale_fill_manual(values=c("#A0A19D", "#4F5458"))
quantile(bank$duration, probs=c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95,0.975,0.99))
bank6<-bank
bank6<-bank6[bank6$duration<=1269,]
box2<-ggplot(bank6,aes(x=y,y=duration,fill=y))+geom_boxplot()
box3<-box2 + ggtitle("현 캠페인 등록 여부 별 지속시간") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
box4<-box3+labs(x="현재 등록 여부", y="지속 시간")+theme(axis.title = element_text(face = "bold", size = 18, color = "darkblue"))
box4+labs(fill="현재 등록 여부")+theme(legend.title=element_text(face = "bold", size = 12, color = "black"))

#l. campaign(현재 캠페인 연결 횟수)
summary(bank$campaign)
quantile(bank$campaign, probs=c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95,0.975,0.99))
boxplot(bank$campaign)
boxplot(bank$campaign~bank$y) #99퍼에 시도 16번인데 그 이상의 이상치는 시각화를 하는데 방해가 되어보임
bank7<-bank
bank7<-bank7[bank7$campaign<=16,] #극 이상치 제거후
box2<-ggplot(bank,aes(x=y,y=campaign,fill=y))+geom_boxplot()
box3<-box2 + ggtitle("현 캠페인 등록 여부 별 연결 횟수") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
box4<-box3+labs(x="현재 등록 여부", y="연결 횟수")+theme(axis.title = element_text(face = "bold", size = 18, color = "darkblue"))
box4+labs(fill="현재 등록 여부")+theme(legend.title=element_text(face = "bold", size = 12, color = "black"))
#중간값에서는 차이가 없지만 애초에 1,2,3의 값이 너무 많은 상태이기 때문에 차이가 없다고 하기에는 애매하며 #오히려 yes의 경우는 median과 3rd의 차이가 없는 것을 보아 yes의 경우가 평균적인 campaign 연락 횟수가 적을 것으로 보인다.

#m. pdays
summary(bank$pdays)
quantile(bank$pdays, probs=c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95,0.975,0.99))
bank8<-bank
bank8$cu<-"잠재고객"
bank8[bank$pdays!=-1,19]<-"이전고객"
table(bank8[,19])
bar<-ggplot(bank, aes(x=cu,y=bank8$tk, fill=bank8$y))
bar2<-bar+geom_bar(stat="identity", position="fill")
bar3<-bar2 + ggtitle("이전 참가 여부 별 캠페인 비율") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
bar4<-bar3+labs(x="이전 참가 여부", y="현재 등록 비율")+theme(axis.title = element_text(face = "bold", size = 18, color = "darkblue"))
bar4+labs(fill="현재 등록 여부")+theme(legend.title=element_text(face = "bold", size = 12, color = "black"))
#n. previous
summary(bank$previous)
quantile(bank13$previous, probs=c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95,0.975,0.995))
bank13<-bank[bank$previous>0,]
boxplot(bank13$previous~bank13$y) #이전 캠페인 횟수와 현재 y의 관계
table(bank13$previous,bank13$y)
prop.table( table(bank13$previous,bank13$y),1) #잠재고객 이전에 참여 고객 구분 확률
table(bank13$y)#전체 ; 5260/(5260+39766) #0.1168
#0의 경우는 잠재고객이고 1부터는 이전부터 존재하였던 고객
bar<-ggplot(bank13, aes(x=as.character(bank13$previous),y=bank13$tk, fill=bank13$y))
bar2<-bar+geom_bar(stat="identity", position="fill")
bar3<-bar2 + ggtitle("이전 횟수 별 캠페인 비율") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
bar4<-bar3+labs(x="이전 횟수", y="현재 등록 비율")+theme(axis.title = element_text(face = "bold", size = 18, color = "darkblue"))
bar5<-bar4+labs(fill="현재 등록 여부")+theme(legend.title=element_text(face = "bold", size = 12, color = "black"))
bar5+scale_x_discrete(limits=c("0","1","2","3","4","5","6","7","8","9","10","11","12"))
box2<-ggplot(bank13,aes(y=previous))+geom_boxplot(color="#4F5458")
#box3<-box2 + ggtitle("현 캠페인 등록 여부 별 연결 횟수") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
box3<-box2+theme_bw()
box3+labs(y="이전 캠페인 연락 수")+theme(axis.title = element_text(face = "bold", size = 20, color = "black"))

#o. poutcome
table(bank$poutcome,bank$y)
prop.table( table(bank$poutcome,bank$y) ,1)
bar<-ggplot(bank, aes(x=poutcome,y=bank$tk, fill=bank$y)) #바로 위에 prop.table을 시각화
bar2<-bar+geom_bar(stat="identity", position="fill")
#bar3<-bar2 + ggtitle("이전 캠페인 현황 별 현 캠페인 비율") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
bar3<-bar2+theme_bw()
bar4<-bar3+labs(x="이전 캠페인 결과", y="예금 구매 여부")+theme(axis.title = element_text(face = "bold", size = 20, color = "black"))
bar5<-bar4+labs(fill="예금 구매 여부")+theme(legend.title=element_text(face = "bold", size = 15, color = "black"))
bar5+scale_fill_manual(values=c("#A0A19D", "#4F5458"))
#해석을 해보자면 이전 캠페인에 성공을 한 경우에 바로 다음에 진행되는 캠페인에 참가를 하려는 경향이 커보이며 #잠재고객이 대부분인 unknwon에 대해서는 다른 곳에 비해서 성공확률이 낮으며 #이전 고객의 결과를 중 나머지인 failure과 other의 경우에 대해서는 그래도 잠재고객들 보다는 성공확률이 높다.


#p. ### y
e2<-ggplot(bank, aes(x=y)) +geom_bar() + theme_bw() + theme(axis.text.x=element_text(size=14)) 
#e3<-e2 + ggtitle("채무 이행 별 인원수") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
e4<-e2+labs(x="타깃 변수", y="인원수")+theme(axis.title = element_text(face = "bold", size = 20, color = "black"))
e4+geom_text(aes(label=..count..), stat="count", vjust=-0.4, colour="black", size=5)
#y에 대한 누적(필요한 경우에 사용)
prop.table( table(bank$education,bank$y),1)
bar<-ggplot(bank, aes(x=education,y=bank$tk, fill=bank$y))
bar2<-bar+geom_bar(stat="identity", position="fill")
bar3<-bar2+theme_bw()
#bar3<-bar2 + ggtitle("학력 별 캠페인 비율") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
bar4<-bar3+labs(x="학력", y="예금 구매 여부")+theme(axis.title = element_text(face = "bold", size = 20, color = "black"))
bar5<-bar4+labs(fill="예금 구매 여부")+theme(legend.title=element_text(face = "bold", size = 15, color = "black"))
bar5+scale_fill_manual(values=c("#A0A19D", "#4F5458"))


#3. 사용하지 않을 변수와 복합적인 이상치에 대한 삭제 작업
#a. 사용하지 않을 변수인 contact 삭제
bank<-bank[,-9]

#b. 이전 참여고객의 이전 결과가 unknown인 5명에 대해서 제거
bank<-bank[(bank$pdays==-1 & bank$poutcome=="unknown") | (bank$pdays!=-1 & bank$poutcome!="unknown") ,]

#c. 복합적 이상치 제거작업
#c1. 초졸에 나이가 좀 있는데 학생인 사람 #44명
bank<-bank[bank$education!="primary" | bank$job!="student",] 

#c2. 나이가 40살 미만, 직업이 은퇴인 사람 #25명
bank<-bank[ bank$age>39 | bank$job!="retired",] #이상치로 이야기가 나온 부분으로 제거를 고려해 볼 필요가 있어보인다.

#c3. campaign(현 캠페인 연결 횟수) 극이상치 처리
quantile(bank$campaign, probs=c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95,0.975,0.99))
bank<-bank[bank$campaign<=16,] #극 이상치 제거

#c4. previous에서 제일 큰 275값 1개 제거
summary(bank)
bank<-bank[bank$previous!=275,] #극 이상치 제거
#4. age, job 재 범주화, day, month로 week변수 만든 후 day변수 제거, education 이상치 처리

#a. age 재범주화
bank$age2<-"-01"
bank[bank$age<=29,"age2"]<-"20대이하"
bank[bank$age>=30 & bank$age<=39,"age2"]<-"30대"
bank[bank$age>=40 & bank$age<=49,"age2"]<-"40대"
bank[bank$age>=50 & bank$age<=59,"age2"]<-"50대"
bank[bank$age>=60 & bank$age<=69,"age2"]<-"60대"
bank[bank$age>=70,"age2"]<-"70대이상"
bank$age<-as.factor(bank$age2)
bank<-bank[,-17]
### 재범주화한 age 막대 그래프
bank$age2 <- NA
bank[bank$age<=29,"age2"]<-"20대이하"
bank[bank$age>=30 & bank$age<=39,"age2"]<-"30대"
bank[bank$age>=40 & bank$age<=49,"age2"]<-"40대"
bank[bank$age>=50 & bank$age<=59,"age2"]<-"50대"
bank[bank$age>=60 & bank$age<=69,"age2"]<-"60대"
bank[bank$age>=70,"age2"]<-"70대이상"
bank$age2 <- as.factor(bank$age2)
e2<-ggplot(bank, aes(x=age2)) +geom_bar() + theme_bw() + theme(axis.text.x=element_text(size=14)) 
#e3<-e2 + ggtitle("채무 이행 별 인원수") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 18, color = "darkblue")) 
e4<-e2+labs(x="연령대", y="인원수")+theme(axis.title = element_text(face = "bold", size = 20, color = "black"))
e4+geom_text(aes(label=..count..), stat="count", vjust=-0.4, colour="black", size=5)

#b. job 재범주화 #unknown제거(285개)
bank <- bank[bank$job!="unknown",]
levels(bank$job)
for(i in 1:nrow(bank)){
  if(bank$job[i]=="admin."){
    bank$job2[i] <- "admin"
  }else if(bank$job[i]=="management"){
    bank$job2[i] <- "management"
  }else if(bank$job[i]=="entrepreneur"){
    bank$job2[i] <- "entrepreneur"
  }else if(bank$job[i]=="self-employed"){
    bank$job2[i] <- "entrepreneur"
  }else if(bank$job[i]=="blue-collar"){
    bank$job2[i] <- "blue-collar"
  }else if(bank$job[i]=="technician"){
    bank$job2[i] <- "technician"
  }else if(bank$job[i]=="services"){
    bank$job2[i] <- "pink-collar"
  }else if(bank$job[i]=="housemaid"){
    bank$job2[i] <- "pink-collar"
  }else{
    bank$job2[i] <- "1unemployed"
  }   }
bank$job <- as.factor(bank$job2)
bank<-bank[,-17] #job2 제거
str(bank)

#c. 순서 가정하에 연도변수 추가 후 day->평일,휴일로 변경
rownames(bank)<-seq(length=nrow(bank)) #index 초기화
bank$year<-2008
bank[27074:41820,"year"]<-2009
bank[41821:44401,"year"]<-2010
#date변환용 임시 투입
bank$month2<-"-01"
bank[as.character(bank$month)=="feb","month2"]<-"-02"
bank[as.character(bank$month)=="mar","month2"]<-"-03"
bank[as.character(bank$month)=="apr","month2"]<-"-04"
bank[as.character(bank$month)=="may","month2"]<-"-05"
bank[as.character(bank$month)=="jun","month2"]<-"-06"
bank[as.character(bank$month)=="jul","month2"]<-"-07"
bank[as.character(bank$month)=="aug","month2"]<-"-08"
bank[as.character(bank$month)=="sep","month2"]<-"-09"
bank[as.character(bank$month)=="oct","month2"]<-"-10"
bank[as.character(bank$month)=="nov","month2"]<-"-11"
bank[as.character(bank$month)=="dec","month2"]<-"-12"

bank$date<-paste0(as.character(bank$year),bank$month2,"-",as.character(bank$day))
bank$date<-as.Date(bank$date)
bank$day2<-weekdays(bank$date)
bank$day3<-"평일"
bank[as.character(bank$day2)=="토요일","day3"]<-"휴일"
bank[as.character(bank$day2)=="일요일","day3"]<-"휴일"
bank$day<-as.factor(bank$day3)
bank<-bank[,c(-17,-18,-19,-20,-21)] #평일,휴일 이후에 사용한 변수들 전부 제거
#기준점 잡기용 월 정리
bank$month2<-"1jan"
bank[as.character(bank$month)=="feb","month2"]<-"2feb"
bank[as.character(bank$month)=="mar","month2"]<-"3mar"
bank[as.character(bank$month)=="apr","month2"]<-"4apr"
bank[as.character(bank$month)=="may","month2"]<-"5may"
bank[as.character(bank$month)=="jun","month2"]<-"6jun"
bank[as.character(bank$month)=="jul","month2"]<-"7jul"
bank[as.character(bank$month)=="aug","month2"]<-"8aug"
bank[as.character(bank$month)=="sep","month2"]<-"9sep"
bank[as.character(bank$month)=="oct","month2"]<-"l0oct"
bank[as.character(bank$month)=="nov","month2"]<-"l1nov"
bank[as.character(bank$month)=="dec","month2"]<-"l2dec"
bank$month<-as.factor(bank$month2)
bank<-bank[,-17]

#d. education 이상치 처리
set.seed(1234)
bank_edu <- bank[bank$education!="unknown",]
bank_edu_pri <- bank_edu[bank_edu$education=="primary",]
bank_edu_sec <- bank_edu[bank_edu$education=="secondary",]
bank_edu_ter <- bank_edu[bank_edu$education=="tertiary",]
#pri, sec, ter 비율 맞추기
idx <- sample(nrow(bank_edu_sec), nrow(bank_edu_sec)*0.295)
bank_edu_sec <- bank_edu_sec[idx,]
idx <- sample(nrow(bank_edu_ter), nrow(bank_edu_ter)*0.513)
bank_edu_ter <- bank_edu_ter[idx,]
bank_edu <- rbind(bank_edu_pri, bank_edu_sec, bank_edu_ter)
#train,test 나누기
set.seed(1234)
idx <- sample(nrow(bank_edu), nrow(bank_edu)*0.7)
bank_edu_train <- bank_edu[idx,]
bank_edu_test <- bank_edu[-idx,]
bank_edu_check <- bank_edu[-idx, "education"]
#age + marital + job2 68.66215% 가장 좋은 조합
set.seed(1234)
bank_edu_dt <- ctree(education ~ age + marital + job, data=bank_edu_train)
bank_edu_pred <- predict(bank_edu_dt, newdata=bank_edu_test, type="response")
bank_edu_table <- table(bank_edu_pred, bank_edu_check)
sum(diag(bank_edu_table))/sum(bank_edu_table)

#가장 좋은 조합을 unknwon에만 적용
bank_edu <- bank[bank$education!="unknown",]
bank_edu_test <- bank[bank$education=="unknown",]
set.seed(1234)
bank_edu_dt <- ctree(education ~ age + marital + job, data=bank_edu)
bank_edu_pred <- predict(bank_edu_dt, newdata=bank_edu_test, type="response")
table(bank_edu_pred)
bank_edu_test$education<-bank_edu_pred
bank<-rbind(bank_edu,bank_edu_test)
bank$education<-as.factor( as.character(bank$education) ) #factor 3개로 변경작업
str(bank)

#cf 필요없는 변수 contact 처리, 이상치 처리, 변수변환 된 자료 내보내기
#write.table(bank, "C:/Users/bluedice/Desktop/BankData/bank_real2.txt", sep = "," )

#전처리 된 데이터 내보낸거 다시 불러오기
#bank<-read.table("C:/Users/bluedice/Desktop/BankData/bank_real2.txt",sep=",")
str(bank)


# 해당 부분 이전까지만 kaggle에서 확인 가능
# 이후 부분은 모델링 영역역


#5. 모델 나누기

#5-1-1. 잠재 고객 모델
bank_pre0<-bank[bank$poutcome=="unknown",]
summary(bank_pre0)
bank_pre0 <- bank_pre0[,-c(13,14,15)] #remove pdays, previous, poutcome

#5-1-2 y변수에 대한 사전적인 변수처리
#a. 범주형 변수
#(marital,education,default,housing,loan,day,month)
#job,month은 중요해보여 검정없이 들고감
#marital
tk<-table(bank_pre0$marital,bank_pre0$y)
prop.test( c(tk[1,2],tk[2,2]),c(tk[1,1]+tk[1,2],tk[2,1]+tk[2,2]),alternative="two.sided",conf.level=1-(0.05/3))
prop.test( c(tk[1,2],tk[3,2]),c(tk[1,1]+tk[1,2],tk[3,1]+tk[3,2]),alternative="two.sided",conf.level=1-(0.05/3))
prop.test( c(tk[2,2],tk[3,2]),c(tk[2,1]+tk[2,2],tk[3,1]+tk[3,2]),alternative="two.sided",conf.level=1-(0.05/3))
#education
tk<-table(bank_pre0$education,bank_pre0$y)
prop.test( c(tk[1,2],tk[2,2]),c(tk[1,1]+tk[1,2],tk[2,1]+tk[2,2]),alternative="two.sided",conf.level=1-(0.05/3))
prop.test( c(tk[1,2],tk[3,2]),c(tk[1,1]+tk[1,2],tk[3,1]+tk[3,2]),alternative="two.sided",conf.level=1-(0.05/3))
prop.test( c(tk[2,2],tk[3,2]),c(tk[2,1]+tk[2,2],tk[3,1]+tk[3,2]),alternative="two.sided",conf.level=1-(0.05/3))
#default
tk<-table(bank_pre0$default,bank_pre0$y)
prop.test( c(tk[1,2],tk[2,2]),c(tk[1,1]+tk[1,2],tk[2,1]+tk[2,2]),alternative="two.sided",conf.level=0.95)
#housing
tk<-table(bank_pre0$housing,bank_pre0$y)
prop.test( c(tk[1,2],tk[2,2]),c(tk[1,1]+tk[1,2],tk[2,1]+tk[2,2]),alternative="two.sided",conf.level=0.95)
#loan
tk<-table(bank_pre0$loan,bank_pre0$y)
prop.test( c(tk[1,2],tk[2,2]),c(tk[1,1]+tk[1,2],tk[2,1]+tk[2,2]),alternative="two.sided",conf.level=0.95)
#day
tk<-table(bank_pre0$day,bank_pre0$y)
prop.test( c(tk[1,2],tk[2,2]),c(tk[1,1]+tk[1,2],tk[2,1]+tk[2,2]),alternative="two.sided",conf.level=0.95)
#day의 경우 pvalue채택이고 휴일의 경우 자체 갯수가 적기 때문에 제외하고 모델을 구축하기로 결정함

#b. 연속형,이산형 변수로 취급되는 balance, duration 

#b1. duration
qqnorm(bank_pre0$duration)
qqline(bank_pre0$duration)
bank_pre0$logduration<-log(bank_pre0$duration+1) #최소가 0이라서 1추가함
qqnorm(bank_pre0$logduration)
qqline(bank_pre0$logduration)
bank_pre0$duration<-bank_pre0$logduration
bank_pre0<-bank_pre0[,-14]

#b2. balance
#은행예금의 이익 시점을 위해서 0을 제거하고 절대값을 씌워서 변환을 실시함
qqnorm(bank_pre0$balance)
qqline(bank_pre0$balance)
bank_pre0<-bank_pre0[bank_pre0$balance!=0,]
bank_pre0$balance0<-log(abs(bank_pre0$balance)) #로그
bank_pre0$balance1<-sqrt(abs(bank_pre0$balance)) #제곱근
bank_pre0$balance2<-1/(abs(bank_pre0$balance)) #역변환
model<- glm(y ~ balance+balance0+balance1+balance2 ,data=bank_pre0,  family=binomial(link="logit") )
summary(model) #pvalue가 가장 낮은 로그모델 선택
qqnorm(bank_pre0$balance0)
qqline(bank_pre0$balance0)
#정규성 어느 정도 만족
bank_pre0$balance<-bank_pre0$balance0
bank_pre0<-bank_pre0[,c(-14,-15,-16)]




#5-1-3 로지스틱
bank_pre0 <- bank_pre0[,-9] #remove day
#bank_pre0(잠재고객) y 1:1 맞추기
bank_pre0_y0 <- bank_pre0[bank_pre0$y=="no",]
bank_pre0_y1 <- bank_pre0[bank_pre0$y=="yes",]
nrow(bank_pre0_y1)
set.seed(1234)
idx <- sample(nrow(bank_pre0_y0), 3151 )
bank_pre0_sample <- bank_pre0_y0[idx,]
summary(bank_pre0_sample)
bank_pre0_sample$wei<-9.5
bank_pre0_y1$wei<-1
bank_pre0 <- rbind(bank_pre0_sample, bank_pre0_y1)
summary(bank_pre0)
set.seed(12366777)
idx <- sample(nrow(bank_pre0), nrow(bank_pre0)*0.7)
bank_pre0_train <- bank_pre0[idx,]
bank_pre0_test <- bank_pre0[-idx,]
bank_pre0_check <- bank_pre0[-idx, "y"]

#나중에 10분위 분석용 자료 추출
#write.table(bank_pre0_train, "C:/Users/bluedice/Desktop/BankData/bank_pre0_train.txt", sep = "," ,quote=F,row.names=F)
#write.table(bank_pre0_test, "C:/Users/bluedice/Desktop/BankData/bank_pre0_test.txt", sep = "," ,quote=F,row.names=F)

bank_pre0_logit <- glm(y ~age+job+marital+education+default+balance+housing+loan+month+duration+campaign, data=bank_pre0_train, family=binomial(link="logit"),weights=wei)
Anova(bank_pre0_logit,type="III",test="Wald")

bank_pre0_logit <- glm(y ~age+job+marital+default+balance+housing+loan+month+duration+campaign, data=bank_pre0_train, family=binomial(link="logit"),weights=wei)
out <- step(bank_pre0_logit, direction = "both")
summary(out)
vif(bank_pre0_logit)

bank_pre0_logit_pred <- predict(out, newdata=bank_pre0_test, type="response")
bank_pre0_logit_pred[bank_pre0_logit_pred > 0.5] <- 1
bank_pre0_logit_pred[bank_pre0_logit_pred <= 0.5] <- 0

tk<-table(bank_pre0_logit_pred, bank_pre0_check)
sum(diag(tk))/sum(tk) #weight쓰면 64.41퍼









#5-2-1 이전 참여고객 로지스틱
bank_pre1 <- bank[bank$poutcome=="failure"|bank$poutcome=="other"|bank$poutcome=="success",]
bank_pre1$poutcome<-as.factor(as.character(bank_pre1$poutcome))
summary(bank_pre1)

#5-2-2 y변수에 대한 사전적인 변수처리
#a. 범주형 변수
#(marital,education,default,housing,loan,day,month)

#job,month,poutcome은 중요해보이기 때문에 검정없이 들고감
#marital
tk<-table(bank_pre1$marital,bank_pre1$y)
prop.test( c(tk[1,2],tk[2,2]),c(tk[1,1]+tk[1,2],tk[2,1]+tk[2,2]),alternative="two.sided",conf.level=1-(0.05/3)) #divorce랑 marry랑 동일하게 나옴
prop.test( c(tk[1,2],tk[3,2]),c(tk[1,1]+tk[1,2],tk[3,1]+tk[3,2]),alternative="two.sided",conf.level=1-(0.05/3))
prop.test( c(tk[2,2],tk[3,2]),c(tk[2,1]+tk[2,2],tk[3,1]+tk[3,2]),alternative="two.sided",conf.level=1-(0.05/3))
bank_pre1$marry<-"marry"
bank_pre1[as.character(bank_pre1$marital)=="single","marry"]<-"single"
tk<-table(bank_pre1$marry,bank_pre1$y)
prop.test( c(tk[1,2],tk[2,2]),c(tk[1,1]+tk[1,2],tk[2,1]+tk[2,2]),alternative="two.sided",conf.level=0.95)
#결혼여부로 차이 존재
#education
tk<-table(bank_pre1$education,bank_pre1$y)
prop.test( c(tk[1,2],tk[2,2]),c(tk[1,1]+tk[1,2],tk[2,1]+tk[2,2]),alternative="two.sided",conf.level=1-(0.05/3)) #primary와 secondary는 귀무가설 채택
prop.test( c(tk[1,2],tk[3,2]),c(tk[1,1]+tk[1,2],tk[3,1]+tk[3,2]),alternative="two.sided",conf.level=1-(0.05/3))
prop.test( c(tk[2,2],tk[3,2]),c(tk[2,1]+tk[2,2],tk[3,1]+tk[3,2]),alternative="two.sided",conf.level=1-(0.05/3))
bank_pre1$university<-"no_univ"
bank_pre1[as.character(bank_pre1$education)=="tertiary","university"]<-"univ"
tk<-table(bank_pre1$university,bank_pre1$y)
prop.test( c(tk[1,2],tk[2,2]),c(tk[1,1]+tk[1,2],tk[2,1]+tk[2,2]),alternative="two.sided",conf.level=0.95) #primary와 secondary는 귀무가설 채택
#대학여부로는 차이 존재
#default
tk<-table(bank_pre1$default,bank_pre1$y)
prop.test( c(tk[1,2],tk[2,2]),c(tk[1,1]+tk[1,2],tk[2,1]+tk[2,2]),alternative="two.sided",conf.level=0.95)
#housing
tk<-table(bank_pre1$housing,bank_pre1$y)
prop.test( c(tk[1,2],tk[2,2]),c(tk[1,1]+tk[1,2],tk[2,1]+tk[2,2]),alternative="two.sided",conf.level=0.95)
#loan
tk<-table(bank_pre1$loan,bank_pre1$y)
prop.test( c(tk[1,2],tk[2,2]),c(tk[1,1]+tk[1,2],tk[2,1]+tk[2,2]),alternative="two.sided",conf.level=0.95)

#day
tk<-table(bank_pre1$day,bank_pre1$y)
prop.test( c(tk[1,2],tk[2,2]),c(tk[1,1]+tk[1,2],tk[2,1]+tk[2,2]),alternative="two.sided",conf.level=0.95)
#day의 경우 pvalue채택이고 휴일의 경우 자체 갯수가 적기 때문에 제외하고 모델을 구축하기로 결정함

#poutcome
tk<-table(bank_pre1$poutcome,bank_pre1$y)
prop.test( c(tk[1,2],tk[2,2]),c(tk[1,1]+tk[1,2],tk[2,1]+tk[2,2]),alternative="two.sided",conf.level=1-(0.05/3)) 
prop.test( c(tk[1,2],tk[3,2]),c(tk[1,1]+tk[1,2],tk[3,1]+tk[3,2]),alternative="two.sided",conf.level=1-(0.05/3))
prop.test( c(tk[2,2],tk[3,2]),c(tk[2,1]+tk[2,2],tk[3,1]+tk[3,2]),alternative="two.sided",conf.level=1-(0.05/3))
#실제로도 각 그룹마다 차이 존재(즉 사용)
str(bank_pre1)
bank_pre1<-bank_pre1[,c(-3,-4,-9)] 
#marital, education, day제거(대체됨)

#5-2-b.
#연속형,이산형 변수로 취급되는 balance, duration 
#duration
qqnorm(bank_pre1$duration)
qqline(bank_pre1$duration)
bank_pre1$logduration<-log(bank_pre1$duration+1) #최소가 0이라서 1추가함

qqnorm(bank_pre1$logduration)
qqline(bank_pre1$logduration) #어느정도 정규성 만족
bank_pre1$duration<-bank_pre1$logduration
bank_pre1<-bank_pre1[,-16]
#balance
#은행예금의 이익 시점을 위해서 0을 제거하고 절대값을 씌워서 변환을 실시함
qqnorm(bank_pre1$balance)
qqline(bank_pre1$balance)
bank_pre1<-bank_pre1[bank_pre1$balance!=0,]
bank_pre1$balance0<-log(abs(bank_pre1$balance)) #로그
bank_pre1$balance1<-sqrt(abs(bank_pre1$balance)) #제곱근
bank_pre1$balance2<-1/(abs(bank_pre1$balance)) #역변환
model<- glm(y ~ balance+balance0+balance1+balance2 ,data=bank_pre1,  family=binomial(link="logit") )
summary(model) #pvalue가 가장 낮은 로그 모델 선택
qqnorm(bank_pre1$balance0)
qqline(bank_pre1$balance0)
#정규성 어느 정도 만족
bank_pre1$balance<-bank_pre1$balance0
bank_pre1<-bank_pre1[,c(-16,-17,-18)]
#5-2-3 로지스틱

#bank_pre1(이전 참여 고객) y 1:1 맞추기
summary(bank_pre1)
bank_pre1_y0 <- bank_pre1[bank_pre1$y=="no",]
bank_pre1_y1 <- bank_pre1[bank_pre1$y=="yes",]


set.seed(1234)
idx <- sample(nrow(bank_pre1_y0), 1787)
bank_pre1_sample <- bank_pre1_y0[idx,]
summary(bank_pre1_sample)
bank_pre1_sample$wei<-3.33333
bank_pre1_y1$wei<-1
bank_pre1 <- rbind(bank_pre1_sample, bank_pre1_y1)
summary(bank_pre1)
#logistic(bank_pre1)
set.seed(1234)
idx <- sample(nrow(bank_pre1), nrow(bank_pre1)*0.7)
bank_pre1_train <- bank_pre1[idx,]
bank_pre1_test <- bank_pre1[-idx,]
bank_pre1_check <- bank_pre1[-idx, "y"]


#나중에 10분위 분석용 자료 추출
#write.table(bank_pre1_train, "C:/Users/bluedice/Desktop/BankData/bank_pre1_train.txt", sep = "," ,quote=F,row.names=F)
#write.table(bank_pre1_test, "C:/Users/bluedice/Desktop/BankData/bank_pre1_test.txt", sep = "," ,quote=F,row.names=F)



bank_pre1_logit <- glm(y ~age+job+default+balance+housing+loan+month+duration+campaign+pdays+previous+poutcome+marry+university, data=bank_pre1_train, family=binomial(link="logit"),weights=wei)
Anova(bank_pre1_logit,type="III",test="Wald")

bank_pre1_logit <- glm(y ~age+job+balance+month+duration+housing+pdays+poutcome+university, data=bank_pre1_train, family=binomial(link="logit"),weights=wei)
out <- step(bank_pre1_logit, direction = "both")
summary(out)
vif(bank_pre1_logit)

bank_pre1_logit_pred <- predict(out, newdata=bank_pre1_test, type="response")
bank_pre1_logit_pred[bank_pre1_logit_pred > 0.5] <- 1
bank_pre1_logit_pred[bank_pre1_logit_pred <= 0.5] <- 0

tk<-table(bank_pre1_logit_pred, bank_pre1_check)
sum(diag(tk))/sum(tk) #weight쓰면 73.066