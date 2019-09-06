setwd("C:/Users/ujjwa/Downloads")
## ----
ld_train=read.csv("loan_data_train.csv",stringsAsFactors = FALSE)
ld_test=read.csv("loan_data_test.csv",stringsAsFactors = FALSE)

library(dplyr)
glimpse(ld_train)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

ld_test$Interest.Rate=NA

ld_train$data='train'
ld_test$data='test'

ld_all=rbind(ld_train,ld_test)
glimpse(ld_all)
names(ld_all)[sapply(ld_all,function(x) is.character(x))]
table(ld_all$Amount.Requested)
 ld_all=ld_all %>% select(-Amount.Funded.By.Investors)
 ld_all=ld_all %>% mutate(Interest.Rate=as.numeric(gsub("%"," ",Interest.Rate)))
 ld_all$Amount.Requested=as.numeric(ld_all$Amount.Requested)
 table(ld_all$Loan.Purpose)
ld_all$Loan.Length=as.numeric(ld_all$Loan.Length=="36 months") 
ld_all=CreateDummies(ld_all,"Loan.Purpose",100)
ld_all=ld_all %>% mutate(Debt.To.Income.Ratio=as.numeric(gsub("%"," ",Debt.To.Income.Ratio)))
ld_all=CreateDummies(ld_all,"State",100)
ld_all=CreateDummies(ld_all,"Home.Ownership",100)
ld_all=CreateDummies(ld_all,"FICO.Range",100)
table(ld_all$Employment.Length)
ld_all=CreateDummies(ld_all,"Open.CREDIT.Lines",100)
glimpse(ld_all)
ld_all$Revolving.CREDIT.Balance=as.numeric(ld_all$Revolving.CREDIT.Balance)
ld_all=CreateDummies(ld_all,"Employment.Length",100)
lapply(ld_all,function(x) sum(is.na(x)))
## NA values

lapply(ld_all,function(x) sum(is.na(x)))

ld_all=ld_all[!(is.na(ld_all$ID)),]

for(col in names(ld_all)){
  
  if(sum(is.na(ld_all[,col]))>0 & !(col %in% c("data","Interest.Rate"))){
    
    ld_all[is.na(ld_all[,col]),col]=mean(ld_all[,col],na.rm=T)
  }
  
}
ld_all=ld_all %>% select(-ID)
# seprate train and test
ld_train=ld_all %>% filter(data=="train") %>% select(-data)
ld_test=ld_all %>% filter(data=="test") %>% select(-data,-Interest.Rate)
# checking the response on training data set
s=sample(1:nrow(ld_train),0.7*nrow(ld_train))
train=ld_train[s,]
test=ld_train[-s,]
fit=lm(Interest.Rate~.,data=train)

library(car)
sort(vif(fit),decreasing = T)
fit=step(fit)
summary(fit)
predict.ir=predict(fit,test)

errors=test$Interest.Rate-predict.ir

errors**2 %>% mean() %>% sqrt()
## on whole data
formula(fit)
# on whole data set
fit.final=lm(Interest.Rate~ Amount.Requested + Loan.Length + Debt.To.Income.Ratio + 
               +                Monthly.Income + Inquiries.in.the.Last.6.Months + Loan.Purpose_home_improvement + 
               +                Loan.Purpose_other + State_TX + State_CA + Home.Ownership_RENT + 
               +                FICO.Range_720_724 + FICO.Range_660_664 + FICO.Range_700_704 + 
               +                FICO.Range_705_709 + FICO.Range_685_689 + FICO.Range_690_694 + 
               +                FICO.Range_665_669 + FICO.Range_695_699 + FICO.Range_680_684 + 
               +                FICO.Range_675_679 + FICO.Range_670_674 + Open.CREDIT.Lines_14 + 
               +                Open.CREDIT.Lines_12 + Open.CREDIT.Lines_13 + Open.CREDIT.Lines_10 + 
               +                Open.CREDIT.Lines_11 + Open.CREDIT.Lines_7 + Open.CREDIT.Lines_6 + 
               +                Open.CREDIT.Lines_9 + Open.CREDIT.Lines_8 + Employment.Length_5years + 
               +                Employment.Length_2years + Employment.Length_LT_1year,data=ld_train)






predict.final=predict(fit,ld_test)
predict.final
write.csv(predict.final,"mysubmission1.csv")
