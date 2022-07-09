{
library(randomForest)
library(ggplot2)
library(dplyr)
library(tree)
library(party)
library(cvTools)
library(fastDummies)
library(arulesViz)
setwd("D:\\Edvancer\\R\\realstate\\project")

ld_train=read.csv("housing_train.csv",stringsAsFactors = F)
ld_test= read.csv("housing_test.csv",stringsAsFactors = F)
summary(ld_train)


  cusMean = function(condDatas,condValue,meanDatas){
    sum =0
    total = 0
    for (i in 1:length(condDatas)) {
      if(condDatas[i]==condValue){
        if(!is.na(meanDatas[i])){
          sum = sum + meanDatas[i]
        }
        total = total + 1
      }
    }
    return(sum/total)
  }
  
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
      
      data[,name]=as.integer(data[,var]==cat)
    }
    
    data[,var]=NULL
    return(data)
  }
  
  ld_test$Price = NA #adding the price
  ld_train$data='train'
  ld_test$data='test'
  ld_all = rbind(ld_train,ld_test)
  
  
}

{
  
 
  summary(ld_all)
  
   #dropping address and and Council Area
   ld_all$CouncilArea = NULL
   ld_all$Address=NULL
   ld_all$Suburb =NULL
   ld_all$SellerG = NULL
   ld_all$Method = NULL
   
   
   
   ld_all$Type= ifelse(ld_all$Type=="h",1,(ifelse(ld_all$Type=="t",2,3)))
   

   #working with Bedroom2
   ld_all$Bedroom2 = ifelse(ld_all$Bedroom2 == 0,NA,ld_all$Bedroom2)
   for (j in 1:length(ld_all$Bedroom2)) {
     if(is.na(ld_all$Bedroom2[j])){
       ld_all$Bedroom2[j] = as.integer(cusMean(ld_all$Rooms, ld_all$Rooms[j], ld_all$Bedroom2))
     }
   }
   
   #working with Bathroom
   #ld_all$Bathroom = ifelse(ld_all$Bathroom == 0,NA,ld_all$Bathroom)
   for (j in 1:length(ld_all$Bathroom)) {
     if(is.na(ld_all$Bathroom[j])){
       ld_all$Bathroom[j] = as.integer(cusMean(ld_all$Rooms, ld_all$Rooms[j], ld_all$Bathroom))
     }
   }
   
   #working with Car
   #ld_all$Bedroom2 = ifelse(ld_all$Bedroom2 == 0,NA,ld_all$Bedroom2)
   for (j in 1:length(ld_all$Car)) {
     if(is.na(ld_all$Car[j])){
       ld_all$Car[j] = as.integer(cusMean(ld_all$Rooms, ld_all$Rooms[j], ld_all$Car))
     }
   }
      
   #working with Landsize
   ld_all$Landsize = ifelse(ld_all$Landsize == 0,NA,ld_all$Landsize)
   for (j in 1:length(ld_all$Landsize)) {
     if(is.na(ld_all$Landsize[j])){
       ld_all$Landsize[j] = cusMean(ld_all$Rooms, ld_all$Rooms[j], ld_all$Landsize)
     }
   }
   
   #working with BuildingArea
#   ld_all$BuildingArea = ifelse(ld_all$BuildingArea < 50,NA,ld_all$BuildingArea)
#   for (j in 1:length(ld_all$BuildingArea)) {
#     if(is.na(ld_all$BuildingArea[j])){
#       ld_all$BuildingArea[j] = cusMean(ld_all$Rooms, ld_all$Rooms[j], ld_all$BuildingArea)
#     }
#   }
   
  
   ld_all = ld_all %>% mutate(
     #Distance = as.numeric(Distance),
     YearBuilt = as.numeric(ifelse(is.na(YearBuilt),mean(YearBuilt, na.rm = TRUE),YearBuilt)),
     )
   #ld_all$YearBuilt=as.integer(strptime(as.character(ld_all$YearBuilt), "%Y"))
   
   #ld_all$YearBuilt = format(ld_all$YearBuilt, "%Y")
   ld_all$AgeOfBuilding = as.integer(2022-ld_all$YearBuilt)
   ld_all$YearBuilt = NULL
  
   
   #Creating dummy data
   #ld_all=dummy_cols(ld_all, select_columns = c('Type'),
   #                 remove_selected_columns = TRUE)
   
   
 
   
   ld_all=CreateDummies(ld_all,"Postcode",100)
   ld_all=CreateDummies(ld_all ,"BuildingArea",100)
   #ld_all=CreateDummies(ld_all,"Method",100)
   
  }

glimpse(ld_all)



## separate train and test
{
  ld_train=ld_all %>% filter(data=='train') %>% select(-data)
  ld_test=ld_all %>% filter(data=='test') %>% select(-data,-Price)
  
  #### 70:30 Train:Test
  ld_train$data = NULL
  set.seed(2)
  s=sample(1:nrow(ld_train),0.7*nrow(ld_train))
  ld_train1=ld_train[s,]  
  ld_train2=ld_train[-s,]
}
##


#RandomForest
ld.tree=randomForest(Price~.,data=ld_train1)
val.IR=predict(ld.tree, newdata = ld_train2)
rmse_val=((val.IR)-(ld_train2$Price))^2 %>% mean() %>% sqrt()
rmse_val

ld.tree

'glimpse(ld_train1)
glimpse(ld_train2)'


'### Linear Regression
ld.tree=lm(ld_train1$Price~ .,data=ld_train1)
val.IR=predict(ld.tree,newdata = ld_train2)
rmse_val=((val.IR)-(ld_train2$Price))^2 %>% mean() %>% sqrt()
rmse_val
'
'### Building A Descison Tree
ld.tree=tree(ld_train1$Price~ .,data=ld_train1)
val.IR=predict(ld.tree,newdata = ld_train2)
rmse_val=((val.IR)-(ld_train2$Price))^2 %>% mean() %>% sqrt()
#rmse_val=((predicted values)-(actual value))^2 %>% mean() %>% sqrt()
rmse_val

#cTree
ld.tree=ctree(Price~.,data=ld_train1)
val.IR=predict(ld.tree,newdata = ld_train1)
rmse_val=((val.IR)-(ld_train1$Price))^2 %>% mean() %>% sqrt()
rmse_val'



## Making final model on entire training
## and prediction on test/production data

'ld.tree.final=ctree(Price~. ,data=ld_train)
test.pred=predict(ld.tree.final,newdata=ld_test)
write.csv(test.pred,"mysubmission.csv",row.names = F)'

