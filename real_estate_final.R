 setwd("E:\\DATA SCIENCE\\EDVANCER_EDUVENTURE\\EDVANCER_R\\Session 13 - R -Projects\\1- REAL ESTATE PROJECT")
 
train_file="housing_train.csv"
test_file="housing_test.csv"

train=read.csv(train_file,stringsAsFactors = F)
test= read.csv(test_file, stringsAsFactors = F)

test$Price=NA
train$data="train"
test$data="test"

alldata=rbind(train,test)

#########################################################################################
##############################################################################
#1st question

var(x=alldata$Price, na.rm=T)

?sd
sd(alldata$Price, na.rm=T)
summary(alldata$Price)
str(alldata$Price)
?var
###################################################
# 2nd question
str(alldata)
dim(alldata)
?norm
#apply(alldata$Type,type= "O")
?apply
#sum(sapply(alldata$Type, typeof) == "character")
?sapply

#k=sum(alldata$Type=='h')
#s=sum(alldata$Type=='t')

l=sum(alldata$Type=='h')
l
table(alldata$Type)
x1=tapply(sum(alldata$Price),sum(alldata$Type=='h'), mean, na.rm=T)
x1

x2=tapply(sum(alldata$Price),sum(alldata$Type=='t'), mean, na.rm=T)
x2

#############################################################################

sapply(alldata,function(x) sum(is.na(x)))
ncol(alldata)
summary(alldata)

lapply(alldata, function(x) is.numeric(x))

library(dplyr)
glimpse(alldata)
table(sapply(alldata,class))

##Rooms,Distance,Postcode,bedroom2,bathroom,car,lansize,building area, year of built

sum(is.na(alldata$Postcode))
sum(is.na(alldata$Bedroom2))
sum(is.na(alldata$Bathroom))
sum(is.na(alldata$Landsize))
sum(is.na(alldata$BuildingArea))
sum(is.na(test$YearBuilt))

str(alldata$YearBuilt)
glimpse(alldata$YearBuilt)

sapply(alldata,function(x) sum(is.na(x)))

#suburb- unique:142
#frequency cutoff =100, for fun create dummy
x=unique(alldata$Address)
length(x)

unique(alldata$Type)
length(unique(alldata$Type))
?unique
duplicated(alldata$Type,fromLast='T')
count(alldata$Type)

unique(alldata$Postcode)
length(unique(alldata$Postcode))

unique(alldata$Bedroom2)
length(unique(alldata$Bedroom2))

unique(alldata$Bathroom)
length(unique(alldata$Bathroom))

unique(alldata$CouncilArea)
length(unique(alldata$CouncilArea))

unique(alldata$Price)
length(unique(alldata$Price))
summary(alldata$Price)
################################################################################
# 3rd question

alldata$Price
x=tapply(alldata$Price, alldata$CouncilArea, mean,na.rm=T)
x
sort(x)
glimpse(x)

class(alldata$Postcode)

x=tapply(alldata$Bedroom2, alldata$Rooms, mean, na.rm=T)
x
############################################################################
################################################################################

str(alldata)
names(alldata)[sapply(alldata,function(x) is.character(x))]

CreateDummies=function(data,var,freq_cutoff=100)
{
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories)
    {
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

####################################################
for(i in 1: nrow(alldata))
{
  if(is.na(alldata$Bedroom2[i]))
  {
    alldata$Bedroom2[i]=x[alldata$Rooms[i]]
    
  }
}

#bathroo- missing =1978, repalce missing values with simple avg
# car- replace missing values col avg
#######################################################################

sapply(alldata,function(x) sum(is.character(x)))

dim(alldata)
alldata$Address=NULL

alldata$BuildingArea=as.numeric(alldata$BuildingArea)
alldata$YearBuilt=as.numeric(alldata$YearBuilt)

str(alldata)

names(alldata)[sapply(alldata,function(x) is.character(x))]
catcal=c('Suburb',"Type","Method","SellerG","CouncilArea")

for(col in catcal)
{
  alldata=CreateDummies(alldata,col,100)
}

sapply(alldata,function(x) sum(is.na(x)))

missing_valuecol=c("Bedroom2","Car","YearBuilt","BuildingArea","Landsize", "Bathroom")

for(col in missing_valuecol)
{
  alldata[is.na(alldata[,col]),col]=mean(alldata[alldata$data=='train', col], na.rm=T)
}

sapply(alldata,function(x) sum(is.na(x)))
#################################################################################3

missing_value="YearBuilt"

for(col in missing_value)
{
  alldata[is.na(alldata[,col]),col]=max(alldata[alldata$data=='test', col], na.rm=T)
}


sapply(alldata,function(x) sum(is.na(x)))
#############################################################################
###seperate train and test

library(dplyr)
train=alldata %>% filter(data=='train') %>% select(-data)
test=alldata %>% filter(data=='test') %>% select(-data,-Price)

## liner model
set.seed(111)
s=sample(1: nrow(train),nrow(train)*0.8)
t1= train[s,]
t2=train[-s,]

lm.fit= lm(abs(Price)~., data=t1)
#step function takes 5-10 min time to run
lm.fit=step(lm.fit)

library(car)
sort(vif(lm.fit), decreasing = T)[1:3]
formula(lm.fit)

#remove vars with p-values until all of them have p-values under 10%

summary(lm.fit)
#  find rmse value
predictions_t2=predict(lm.fit, newdata=t2)
(t2$Price-predictions_t2)**2  %>% mean() %>%sqrt()

Score=212467/393686.3
Score#(0.539686)

#final model for prediction

lm.fit= lm(abs(Price)~., data=train)

# step function takes 5-10 min to execute
lm.fit=step(lm.fit)
#include dropping vars based on vif and then p-values

test_predictions=predict(lm.fit,newdata=test)
test_predictions1=abs(test_predictions)

write.csv(test_predictions1, "saikrishna_project1_part2.csv", row.names=F)





