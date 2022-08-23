{
library(randomForest)
library(ggplot2)
library(dplyr)
library(tree)
library(party)
library(cvTools)
library(fastDummies)
library(arulesViz)
library(randomForestSRC)
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
  var(ld_train$Price)
  sum(is.na(ld_train$YearBuilt))
  
  {
    h_total = 0
    h_price_sum = 0
    t_total = 0
    t_price_sum = 0
    
    for (i in 1:length(ld_train$Price)) {
      if(ld_train$Type[i] == 'h'){
        h_total = h_total+1
        h_price_sum = h_price_sum + ifelse(is.na(ld_train$Price[i]),0,ld_train$Price[i])
      }
      if(ld_train$Type[i] == 't'){
        t_total = t_total+1
        t_price_sum = t_price_sum + ifelse(is.na(ld_train$Price[i]),0,ld_train$Price[i])
      }
    }
    h_price_sum
    h_total
    t_price_sum
    h_total
    
    avg_h = h_price_sum/h_total
    avg_t = t_price_sum/h_total
    
    diff = avg_h- avg_t
    
    avg_h
    avg_t
    diff
  }
  
  length(unique(ld_all$Postcode))
  
  qqnorm(ld_all$Distance)
  
  
  {
    sellers = data.frame(name = unique(ld_train$SellerG))
    sellers$price = 0
    sellers
    for(x in 1:length(sellers$name)){
      for (y in 1:length(ld_train$Price)) {
        if(ld_all$SellerG[y] == sellers$name[x]){
          sellers$price[x] = sellers$price[x] +  ifelse(is.na(ld_train$Price[y]),0,ld_train$Price[y])
        }
      }
    }
    
    h_seller = ''
    max(sellers$price) 
    for (k in 1:length(sellers$name)) {
      if(sellers$price[k]==max(sellers$price) ){
        h_seller = sellers$name[k]
      }
    }
    h_seller
    
  }

  '8. which council area HAS Maximun average price'  
  {
    c_area = data.frame(name = unique(ld_train$CouncilArea))
    c_area = na.omit(c_area)
    c_area$sum = 0
    c_area$count = 0
    c_area$avg = 0
    
    for (i in 1:length(c_area$name)) {
      for (j in 1:ld_train$Price) {
        if(is.na(ld_train$CouncilArea[j])){
          
        }else{
          if(ld_train$CouncilArea[j]==c_area$name[i]){
            c_area$sum[i] = c_area$sum[i] + 
              ifelse(is.na(ld_train$Price[j]),0,ld_train$Price[j])
            c_area$count[i] = c_area$count[i] + 1
          }
        }
      }
      c_area$avg[i] =  c_area$sum[i] /c_area$count[i]
    }
    
  }

  '9. which council area has max varinance in the price'
  {
    varr = data.frame(name = unique(ld_train$CouncilArea))
    varr$var = 0
  
    for (i in 1:length(varr$name)) {
      price_list = list()
      countt = 0
      for (j in 1:length(ld_train$Price)) {
        if(ld_train$CouncilArea[j] == varr$name[i]){
        price_list[length(price_list)+1] = ld_train$Price[j]
        }
      }
      v1=unlist(price_list)
      varr$var[i] = var(v1)
    }
  }
  
}  

{
  
 
  summary(ld_all)
  
   #dropping address and and Council Area
   ld_all$CouncilArea = NULL
   ld_all$Address=NULL
   ld_all$SellerG = NULL
   
   ld_all$Suburb =NULL
   
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
   ld_all$BuildingArea = ifelse(ld_all$BuildingArea < 50,NA,ld_all$BuildingArea)
   for (j in 1:length(ld_all$BuildingArea)) {
     if(is.na(ld_all$BuildingArea[j])){
       ld_all$BuildingArea[j] = cusMean(ld_all$Rooms, ld_all$Rooms[j], ld_all$BuildingArea)
     }
   }
   
  
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
   #ld_all=CreateDummies(ld_all ,"BuildingArea",100)
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
 
Score =212467/rmse_val
Score

#ld.tree

#tuning the model

tune_dl <- tune(Price~., ld_train, doBest = TRUE)

tune_dl$optimal
tune_dl

best_params=data.frame(mtry=35,
                       ntree=500,
                       maxnodes=784,
                       nodesize=1)

ld.rf.final=randomForest(Price~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=ld_train)


val.IR=predict(ld.rf.final,newdata = ld_train2)

rmse_val=((val.IR)-(ld_train2$Price))^2 %>% mean() %>% sqrt()
rmse_val

Score =212467/rmse_val 
Score


## Making final model on entire training
## and prediction on test/production data

ld.tree.final=ctree(Price~. ,data=ld_train)
test.pred=predict(ld.tree.final,newdata=ld_test)
write.csv(test.pred,"rajesh_realstate_p2.csv",row.names = F)
