# Title: FMMO project
# Author: Pierre Merel
# Date: July 2026
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


options(dplyr.summarise.inform= FALSE)
suppressPackageStartupMessages({library(plyr) 
                                library(tidyverse)})

setwd("../../MANUSCRIPT TABLES")

####DESCRIPTIVE ELASTICITIES

#TABLE 1
elasmilksupply<-read.csv("Table 1/milksupply.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate_if(is.numeric, round, digits = 2)
colnames(elasmilksupply) <- c("Region","Milk Supply Elasticity")

elasmilksupplytot<-read.csv("Table 1/milksupplytot.csv") %>%
  mutate_if(is.numeric, round, digits = 2)
elasmilksupplytot$Region<-"Total"
elasmilksupplytot <- elasmilksupplytot %>%
  select(Region, Val) %>%
  rename("Milk Supply Elasticity"=Val)

elasmilksupply<-rbind(elasmilksupply,elasmilksupplytot)

if (!dir.exists("Table 1/txt file")) {
  dir.create("Table 1/txt file")
}

sink(file.path("Table 1/txt file","table1.txt"))
print(elasmilksupply,row.names = F)
sink()

#TABLE D.1
elasdemdom<-read.csv("Table D.1/elasdemdom.csv")
elasdemdom<-reshape(elasdemdom,idvar="DOMI",timevar="N",direction="wide") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  dplyr::rename("Region"="DOMI","Beverages"="Val.Beverage", "Softs"="Val.Softs","Cheese"="Val.Cheese","Butter-Powder"="Val.Butter-Powder") %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(across(where(is.numeric), ~ format(.x,,drop0Trailing = F,trim= T))) 

elasdemfor<-read.csv("Table D.1/elasdemfor.csv")
elasdemfor<-t(elasdemfor[,2]) %>%
  as.data.frame() %>%
  dplyr::mutate(Beverages = "--",Region = "Rest of the World") %>%
  dplyr::rename("Softs"=V1,"Cheese"=V2,"Butter-Powder"=V3) %>%
  dplyr::select(Region,Beverages,Softs,Cheese,"Butter-Powder") %>%
  mutate_if(is.numeric, round, digits = 2)

elasdemdairy<-rbind(elasdemdom,elasdemfor) 

if (!dir.exists("Table D.1/txt file")) {
  dir.create("Table D.1/txt file")
}

sink(file.path("Table D.1/txt file","tabled1.txt"))
print(elasdemdairy,row.names = F)
sink()

#TABLE D.2
elasfeed<-read.csv("Table D.2/elasfeed.csv")
elasfeed<-reshape(elasfeed,idvar="DOMI",timevar="SUBL",direction="wide") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate_if(is.numeric, round, digits = 2) 

colnames(elasfeed)<- c("Region","Grains","Oilseeds","Hay","Silage")

if (!dir.exists("Table D.2/txt file")) {
  dir.create("Table D.2/txt file")
}

sink(file.path("Table D.2/txt file","tabled2.txt"))
print(elasfeed,row.names = F)
sink()

#TABLE D.3
elascrop<-read.csv("Table D.3/elascrop.csv") %>%
  dplyr::rename("Output"="Val")

elascrop<-reshape(elascrop,idvar="DOMI",timevar="L",direction="wide") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate_if(is.numeric, round, digits = 2) 

colnames(elascrop)<- c("Region","Grains","Oilseeds","Hay","Silage","Other Crops")

if (!dir.exists("Table D.3/txt file")) {
  dir.create("Table D.3/txt file")
}

sink(file.path("Table D.3/txt file","tabled3.txt"))
print(elascrop,row.names = F)
sink()

#TABLE D.4
elascompdemand<-read.csv("Table D.4/elascompdemand.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate_if(is.numeric, round, digits = 2)

elascompdemand<-reshape(elascompdemand,idvar ="DOMI", timevar="N", direction = "wide")
colnames(elascompdemand)<- c("Region","Beverages","Softs","Cheese","Butter-Powder")

if (!dir.exists("Table D.4/txt file")) {
  dir.create("Table D.4/txt file")
}

sink(file.path("Table D.4/txt file","tabled4.txt"))
print(elascompdemand,row.names = F)
sink()

####RESULTS

#TABLE 2
wedges<-read.csv("Table 2/delta.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  dplyr::filter(!DOMI=="Unregulated") %>%
  dplyr::filter(!N %in% c("Softs","Butter-Powder")) %>%
  dplyr::filter(K %in% c("Protein","Other-Solids") ) %>%
  dplyr::select(DOMI,K,N,Val) %>%
  mutate(N=case_when(N=="Beverage" ~ "Beverages" , T ~ N)) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(across(where(is.numeric), ~ format(.x,,drop0Trailing = F,trim= T))) 

wedges<-reshape(wedges,idvar = c("DOMI","K"), timevar=c("N"), direction = "wide")
wedges<-reshape(wedges,idvar = c("DOMI"), timevar=c("K"), direction = "wide") 
colnames(wedges)<-c("Region", "Protein/Beverages","Protein/Cheese","Other Solids/Beverages","Other Solids/Cheese")

if (!dir.exists("Table 2/txt file")) {
  dir.create("Table 2/txt file")
}

sink(file.path("Table 2/txt file","table2.txt"))
print(wedges,row.names = F)
sink()

#TABLE 3 (Farm milk effects)
milkeffects1<-read.csv("Table 3/deltamilkval.csv") %>%
  dplyr::rename("Value"="Val") %>%
  mutate(Value=100*Value)

milkeffects1tot<-read.csv("Table 3/deltamilkvaltot.csv") %>%
  dplyr::rename("Value"="Val") %>%
  mutate(Value=100*Value) %>%
  mutate(DOMI="Total") %>%
  dplyr::select(DOMI,Value)

milkeffects1a<-read.csv("Table 3/milkvalshare.csv") %>%
  dplyr::rename("ValShare"="Val") %>%
  mutate(ValShare=100*ValShare) 

milkeffects1atot<-milkeffects1tot %>%
  dplyr::rename("ValShare"="Value") %>%
  mutate(ValShare="100") %>%
  mutate(Empty=NA) %>%
  dplyr::select(DOMI,ValShare)

milkeffects2<-read.csv("Table 3/deltamilkprice.csv") %>%
  dplyr::rename("Price"="Val") %>%
  mutate(Price=100*Price)
milkeffects2tot<-read.csv("Table 3/deltamilkpricetot.csv") %>%
  dplyr::rename("Price"="Val") %>%
  mutate(Price=100*Price) %>%
  mutate(DOMI="Total") %>%
  dplyr::select(DOMI,Price)

milkeffects3<-read.csv("Table 3/deltamilkprod.csv") %>%
  dplyr::rename("Prod"="Val") %>%
  mutate(Prod=100*Prod)
milkeffects3tot<-read.csv("Table 3/deltamilkprodtot.csv") %>%
  dplyr::rename("Prod"="Val") %>%
  mutate(Prod=100*Prod) %>%
  mutate(DOMI="Total") %>%
  dplyr::select(DOMI,Prod)

milkeffects4<-read.csv("Table 3/deltamilkuse.csv") %>%
  dplyr::rename("Use"="Val") %>%
  mutate(Use=100*Use)
milkeffects4tot<-read.csv("Table 3/deltamilkusetot.csv") %>%
  dplyr::rename("Use"="Val") %>%
  mutate(Use=100*Use) %>%
  mutate(DOMI="Total") %>%
  dplyr::select(DOMI,Use)

milkeffects5<-read.csv("Table 3/deltamilkprodshareship.csv") %>%
  dplyr::rename("ShareShippedOut"="Val") %>%
  mutate(ShareShippedOut=100*ShareShippedOut) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(ShareShippedOut=format(ShareShippedOut,drop0Trailing = F,trim= T)) 
milkeffects5tot<-read.csv("Table 3/deltamilkprodshareshiptot.csv") %>%
  dplyr::rename("ShareShippedOut"="Val") %>%
  mutate(ShareShippedOut=100*ShareShippedOut) %>%
  mutate(DOMI="Total") %>%
  dplyr::select(DOMI,ShareShippedOut)

milkeffects6<-read.csv("Table 3/milkprodshareship.csv") %>%
  dplyr::rename("BaseShareShippedOut"="Val") %>%
  mutate(BaseShareShippedOut=100*BaseShareShippedOut) 

milkeffects6tot<-read.csv("Table 3/milkprodshareshiptot.csv") %>%
  dplyr::rename("BaseShareShippedOut"="Val") %>%
  mutate(BaseShareShippedOut=100*BaseShareShippedOut) %>%
  mutate(DOMI="Total") %>%
  dplyr::select(DOMI,BaseShareShippedOut)

milkeffects7<-read.csv("Table 3/deltamilkuseshareship.csv") %>%
  dplyr::rename("ShareShippedIn"="Val") %>%
  mutate(ShareShippedIn=100*ShareShippedIn) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(ShareShippedIn=format(ShareShippedIn,drop0Trailing = F,trim= T))
milkeffects7tot<-read.csv("Table 3/deltamilkprodshareshiptot.csv") %>%
  dplyr::rename("ShareShippedIn"="Val") %>%
  mutate(ShareShippedIn=100*ShareShippedIn) %>%
  mutate(DOMI="Total") %>%
  dplyr::select(DOMI,ShareShippedIn)

milkeffects8<-read.csv("Table 3/milkuseshareship.csv") %>%
  dplyr::rename("BaseShareShippedIn"="Val") %>%
  mutate(BaseShareShippedIn=100*BaseShareShippedIn)
milkeffects8tot<-read.csv("Table 3/milkprodshareshiptot.csv") %>%
  dplyr::rename("BaseShareShippedIn"="Val") %>%
  mutate(BaseShareShippedIn=100*BaseShareShippedIn) %>%
  mutate(DOMI="Total") %>%
  dplyr::select(DOMI,BaseShareShippedIn)

milkeffects<-join(milkeffects1,milkeffects1a,by="DOMI") 
milkeffects<-join(milkeffects,milkeffects2,by="DOMI") 
milkeffects<-join(milkeffects,milkeffects3,by="DOMI") 
milkeffects<-join(milkeffects,milkeffects4,by="DOMI") 
milkeffects<-join(milkeffects,milkeffects5,by="DOMI") 
milkeffects<-join(milkeffects,milkeffects6,by="DOMI")  
milkeffects<-join(milkeffects,milkeffects7,by="DOMI") 
milkeffects<-join(milkeffects,milkeffects8,by="DOMI") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(ValShare=paste0("[",format(ValShare,drop0Trailing = F,trim= T),"]")) %>%
  mutate(BaseShareShippedOut = case_when(BaseShareShippedOut<0 ~ "--", T ~ paste0("[",format(BaseShareShippedOut,drop0Trailing = F,trim= T),"]"))) %>%
  mutate(BaseShareShippedIn = case_when(BaseShareShippedIn<0 ~ "--", T ~ paste0("[",format(BaseShareShippedIn,drop0Trailing = F,trim= T),"]"))) %>%
  mutate(ShareShippedOut = case_when(is.na(ShareShippedOut) ~ "--", T ~ as.character(ShareShippedOut) )) %>%
  mutate(ShareShippedIn = case_when(is.na(ShareShippedIn)  ~ "--",  T ~ as.character(ShareShippedIn) )) 
 
milkeffectstot<-join(milkeffects1tot,milkeffects1atot,by="DOMI") 
milkeffectstot<-join(milkeffectstot,milkeffects2tot,by="DOMI") 
milkeffectstot<-join(milkeffectstot,milkeffects3tot,by="DOMI") 
milkeffectstot<-join(milkeffectstot,milkeffects4tot,by="DOMI") 
milkeffectstot<-join(milkeffectstot,milkeffects5tot,by="DOMI") 
milkeffectstot<-join(milkeffectstot,milkeffects6tot,by="DOMI")  
milkeffectstot<-join(milkeffectstot,milkeffects7tot,by="DOMI") 
milkeffectstot<-join(milkeffectstot,milkeffects8tot,by="DOMI") %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(ValShare=paste0("[",format(ValShare,drop0Trailing = F,trim= T),"]")) %>%
  mutate(BaseShareShippedOut=paste0("[",format(BaseShareShippedOut,drop0Trailing = F,trim= T),"]")) %>%
  mutate(BaseShareShippedIn=paste0("[",format(BaseShareShippedIn,drop0Trailing = F,trim= T),"]")) 

table3<-rbind(milkeffects,milkeffectstot)
colnames(table3)<-c("Region","Milk Production Value","(Baseline Share)","Milk Price","Milk Produced","Milk Used",
                    "Share of Production Value Shipped Out","(Baseline Share)","Share of Use Value Shipped In","(Baseline Share)")

if (!dir.exists("Table 3/txt file")) {
  dir.create("Table 3/txt file")
}

sink(file.path("Table 3/txt file","table3.txt"))
print(table3,row.names = F)
sink()

#TABLE 4 (Effects on component prices)
compprice<-read.csv("Table 4/compprice.csv")
compprice<-reshape(compprice,idvar="DOMI",timevar="K",direction="wide") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val.Fat=100*Val.Fat) %>%
  mutate(Val.Protein=100*Val.Protein) %>%
  dplyr::rename("Val.Other"="Val.Other-Solids") %>%
  mutate(Val.Other=100*Val.Other) %>%
  mutate_if(is.numeric, round, digits = 2) 

compcostindex<-read.csv("Table 4/compcostindex.csv")
compcostindex<-reshape(compcostindex,idvar="DOMI",timevar="N",direction="wide") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val.Beverage=100*Val.Beverage) %>%
  mutate(Val.Softs=100*Val.Softs) %>%
  mutate(Val.Cheese=100*Val.Cheese) %>%
  dplyr::rename("Val.Butter"="Val.Butter-Powder") %>%
  mutate(Val.Butter=100*Val.Butter) %>%
  mutate_if(is.numeric, round, digits = 2)

componentseffects<-join(compprice,compcostindex,by="DOMI")
colnames(componentseffects)<- c("Region","Butterfat","Protein","Other Solids","Beverages","Softs","Cheese","Butter-Powder")

if (!dir.exists("Table 4/txt file")) {
  dir.create("Table 4/txt file")
}

sink(file.path("Table 4/txt file","table4.txt"))
print(componentseffects,row.names = F)
sink()


#TABLE 5 (Effects on dairy products)
dairyquantity<-read.csv("Table 5/dairyquant.csv")
dairyquantity<-reshape(dairyquantity,idvar="DOMI",timevar="N",direction="wide") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val.Beverage=100*Val.Beverage) %>%
  mutate(Val.Softs=100*Val.Softs) %>%
  mutate(Val.Cheese=100*Val.Cheese) %>%
  dplyr::rename("Val.Butter"="Val.Butter-Powder") %>%
  mutate(Val.Butter=100*Val.Butter) %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 2),
                                            nsmall = 2,
                                            trim = TRUE)))

dairyvalue<-read.csv("Table 5/dairyval.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=100*Val) %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 2),
                                            nsmall = 2,
                                            trim = TRUE)))

dairyquantval<-join(dairyquantity,dairyvalue,by="DOMI")

dairyvalueshare<-read.csv("Table 5/dairyvalshare.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=100*Val) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(Val=paste0("[",format(Val,drop0Trailing = F,trim= T),"]")) %>%
  dplyr::rename(Share=Val) 

dairyquantvalwshare<-join(dairyquantval,dairyvalueshare,by="DOMI")

dairyexports<-read.csv("Table 5/dairyexp.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=100*Val) %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 2),
                                            nsmall = 2,
                                            trim = TRUE)))

dairyexportshare<-read.csv("Table 5/dairyexpshare.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=100*Val) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(Val=paste0("[",format(Val,drop0Trailing = F,trim= T),"]")) %>%
  dplyr::rename(Share=Val) 
 
dairyexportswshare<-join(dairyexports,dairyexportshare,by="DOMI")

dairyeffects<-join(dairyquantvalwshare,dairyexportswshare,by="DOMI")
colnames(dairyeffects)<- c("Region","Beverages","Softs","Cheese","Butter-Powder","Production Value","(Baseline Share)","Export Value","(Baseline Share)")

dairyvaluetot<-read.csv("Table 5/dairyvaltot.csv") %>%
  mutate(Val=100*Val) %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 2),
                                            nsmall = 2,
                                            trim = TRUE)))

dairyexportstot<-read.csv("Table 5/dairyexptot.csv") %>%
  mutate(Val=100*Val) %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 2),
                                            nsmall = 2,
                                            trim = TRUE)))

dairytot<-data.frame(matrix(c("Total","--","--","--","--",dairyvaluetot,"[100]",dairyexportstot,"[100]"),ncol=9))
colnames(dairytot)<-colnames(dairyeffects)

table5<-rbind(dairyeffects,dairytot)

if (!dir.exists("Table 5/txt file")) {
  dir.create("Table 5/txt file")
}

sink(file.path("Table 5/txt file","table5.txt"))
print(table5,row.names = F)
sink()


#TABLE 6 (Welfare effects)
cs<-read.csv("Table 6/cs.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=100*Val) %>%
  dplyr::rename(CS="Val") %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 3),
                                            nsmall = 3,
                                            trim = TRUE)))

csval<-read.csv("Table 6/csval.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=Val/1000000) %>%
  dplyr::rename(CSVal="Val") %>%
  mutate_if(is.numeric, round, digits = 0)

ps<-read.csv("Table 6/ps.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=100*Val) %>%
  dplyr::rename(LR="Val") %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 3),
                                            nsmall = 3,
                                            trim = TRUE)))

psval<-read.csv("Table 6/psval.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=Val/1000000) %>%
  dplyr::rename(LRVal="Val") %>%
  mutate_if(is.numeric, round, digits = 0)

welf<-read.csv("Table 6/welf.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=100*Val) %>%
  dplyr::rename(SW="Val") %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 3),
                                            nsmall = 3,
                                            trim = TRUE)))

welfval<-read.csv("Table 6/welfval.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=Val/1000000) %>%
  dplyr::rename(SWVal="Val") %>%
  mutate_if(is.numeric, round, digits = 0)

welfareeffects1<-join(cs, csval, by="DOMI")
welfareeffects2<-join(welfareeffects1,ps, by="DOMI")
welfareeffects3<-join(welfareeffects2,psval, by="DOMI")
welfareeffects4<-join(welfareeffects3,welf, by="DOMI")
welfareeffects<-join(welfareeffects4,welfval, by="DOMI") 

welfareeffects<-welfareeffects %>%
  mutate(CSVal=paste0("[",format(CSVal,drop0Trailing = F,trim= T),"]")) %>%
  mutate(LRVal=paste0("[",format(LRVal,drop0Trailing = F,trim= T),"]")) %>%
  mutate(SWVal=paste0("[",format(SWVal,drop0Trailing = F,trim= T),"]")) 

colnames(welfareeffects) <-c("Region","Consumer Surplus","($ mn)","Land Rents","($ mn)","Social Welfare","($ mn)")

csdom<-read.csv("Table 6/csdom.csv") %>%
  mutate(Val=100*Val) %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 3),
                                            nsmall = 3,
                                            trim = TRUE)))

csdomval<-read.csv("Table 6/csdomval.csv") %>%
  mutate(Val=Val/1E06) %>%
  mutate_if(is.numeric, round, digits = 0) 

psdom<-read.csv("Table 6/psdom.csv") %>%
  mutate(Val=100*Val) %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 3),
                                            nsmall = 3,
                                            trim = TRUE)))

psdomval<-read.csv("Table 6/psdomval.csv") %>%
  mutate(Val=Val/1E06) %>%
  mutate_if(is.numeric, round, digits = 0) 

welfdom<-read.csv("Table 6/welfdom.csv") %>%
  mutate(Val=100*Val) %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 3),
                                            nsmall = 3,
                                            trim = TRUE)))

welfdomval<-read.csv("Table 6/welfdomval.csv") %>%
  mutate(Val=Val/1E06) %>%
  mutate_if(is.numeric, round, digits = 0) 

domwelfeffects<-data.frame(matrix(c("United States",csdom,paste0("[",csdomval,"]"),psdom,paste0("[",psdomval,"]"),welfdom,paste0("[",welfdomval,"]")),ncol=7))
colnames(domwelfeffects)<-colnames(welfareeffects)

csfor<-read.csv("Table 6/csfor.csv") %>%
  mutate(Val=100*Val) %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 3),
                                            nsmall = 3,
                                            trim = TRUE)))

csforval<-read.csv("Table 6/csforval.csv") %>%
  mutate(Val=Val/1E06) %>%
  mutate_if(is.numeric, round, digits = 0) 

welffor<-read.csv("Table 6/welffor.csv") %>%
  mutate(Val=100*Val) %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 3),
                                            nsmall = 3,
                                            trim = TRUE)))

forwelfeffects<-data.frame(matrix(c("Rest of the World",csfor,paste0("[",csforval,"]"),"--","--",welffor,paste0("[",csforval,"]")),ncol=7))
colnames(forwelfeffects)<-colnames(welfareeffects)

cstot<-read.csv("Table 6/cstot.csv") %>%
  mutate(Val=100*Val) %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 3),
                                            nsmall = 3,
                                            trim = TRUE)))

cstotval<-read.csv("Table 6/cstotval.csv") %>%
  mutate(Val=Val/1E06) %>%
  mutate_if(is.numeric, round, digits = 0) 

pstot<-read.csv("Table 6/pstot.csv") %>%
  mutate(Val=100*Val) %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 3),
                                              nsmall = 3,
                                              trim = TRUE)))

welftot<-read.csv("Table 6/welftot.csv") %>%
    mutate(Val=100*Val) %>%
    mutate(across(where(is.numeric), ~ format(round(.x, 3),
                                            nsmall = 3,
                                            trim = TRUE)))
  
welftotval<-read.csv("Table 6/welftotval.csv") %>%
  mutate(Val=Val/1E06) %>%
  mutate_if(is.numeric, round, digits = 0) 

totwelfeffects<-data.frame(matrix(c("Total",cstot,paste0("[",cstotval,"]"),pstot,paste0("[",psdomval,"]"),format(welftot,drop0Trailing = F),paste0("[",welftotval,"]")),ncol=7))
colnames(totwelfeffects)<-colnames(welfareeffects)

table6<-rbind(welfareeffects,domwelfeffects,forwelfeffects,totwelfeffects)

if (!dir.exists("Table 6/txt file")) {
  dir.create("Table 6/txt file")
}

sink(file.path("Table 6/txt file","table6.txt"))
print(table6,row.names = F)
sink()


#TABLE E.1 (SENSITIVITY RESULTS)

sensitivesave<-NULL 
for (s in 1:7) {
  
suffix <- paste0("_s", s)
  
milkvaluechange<-read.csv(paste0("Table E.1/deltamilkvaltot",suffix,".csv")) %>%
  mutate(Val=format(round(Val*100,digits=2),nsmall=2))
         
milkshareshipped<-read.csv(paste0("Table E.1/deltamilkprodshareshiptot",suffix,".csv")) %>%
  mutate(Val=format(round(Val*100,digits=2),nsmall=2))

dairyvaluetot<-read.csv(paste0("Table E.1/dairyvaltot",suffix,".csv")) %>%
   mutate(Val=format(round(Val*100, digits=2),nsmall=2))
          
dairyexportstot<-read.csv(paste0("Table E.1/dairyexptot",suffix,".csv")) %>%
  mutate(Val=format(round(Val*100, digits=2),nsmall=2))

csdom<-read.csv(paste0("Table E.1/csdom",suffix,".csv")) %>%
  mutate(Val=format(round(Val*100, digits=3),nsmall=3))

landrentdom<-read.csv(paste0("Table E.1/psdom",suffix,".csv")) %>%
  mutate(Val=format(round(Val*100, digits=3),nsmall=3))

welfaredom<-read.csv(paste0("Table E.1/welfdom", suffix,".csv")) %>%
  mutate(Val=format(round(Val*100, digits=3),nsmall=3))

csfor<-read.csv(paste0("Table E.1/csfor",suffix,".csv")) %>%
  mutate(Val=format(round(Val*100, digits=3),nsmall=3))

welfarefor<-read.csv(paste0("Table E.1/welffor", suffix,".csv")) %>%
  mutate(Val=format(round(Val*100, digits=3),nsmall=3))

sensitive<-as.table(matrix(c(milkvaluechange[1,1],milkshareshipped[1,1],dairyvaluetot[1,1],dairyexportstot[1,1],
                           csdom[1,1],landrentdom[1,1],welfaredom[1,1],csfor[1,1],welfarefor[1,1]),ncol=9))

sensitivesave<-rbind(sensitivesave,sensitive)
}

sensitivesave<-as.data.frame(sensitivesave)
colnames(sensitivesave)<-c("Milk Prod. Val.", "Share of Milk Val. Shipped","Dairy Prod. Val.","Dairy Export Val.","US CS","US LR","US SW","ROW CS","ROW SW")
rownames(sensitivesave)<-c("Baseline","epsilon_0=2,kappa_0=3","varsigma halved","varsigma doubled","rho=0.5","theta=1.01","theta=1.1")

if (!dir.exists("Table E.1/txt file")) {
  dir.create("Table E.1/txt file")
}

sink(file.path("Table E.1/txt file","tablee1.txt"))
print(sensitivesave,row.names = T)
sink()


#TABLE 7 (COMPARISON OF IDEAL WEDGES AND MILK QUOTA)

milkvaluewedge<-read.csv("Table 7/deltamilkvaltotwedge.csv")
milkvaluewedge<-format(round(milkvaluewedge[1,1]*100,digits=3),nsmall=3)
milkvaluequota<-read.csv("Table 7/deltamilkvaltotquota.csv")
milkvaluequota<-format(round(milkvaluequota[1,1]*100,digits=3),nsmall=3)
milkvalueeffects<-as.table(matrix(c(milkvaluewedge,"",milkvaluequota,""),ncol=4))

dairyvaluewedge<-read.csv("Table 7/dairyvaltotwedge.csv")
dairyvaluewedge<-format(round(dairyvaluewedge[1,1]*100,digits=3),nsmall=3)
dairyvaluequota<-read.csv("Table 7/dairyvaltotquota.csv")
dairyvaluequota<-format(round(dairyvaluequota[1,1]*100,digits=3),nsmall=3)
dairyvalueeffects<-as.table(matrix(c(dairyvaluewedge,"",dairyvaluequota,""),ncol=4))

dairyexportwedge<-read.csv("Table 7/dairyexptotwedge.csv")
dairyexportwedge<-format(round(dairyexportwedge[1,1]*100,digits=3),nsmall=3)
dairyexportquota<-read.csv("Table 7/dairyexptotquota.csv")
dairyexportquota<-format(round(dairyexportquota[1,1]*100,digits=3),nsmall=3)
dairyexporteffects<-as.table(matrix(c(dairyexportwedge,"",dairyexportquota,""),ncol=4))

csdomwedge<-read.csv("Table 7/csdomwedge.csv")
csdomwedge<-format(round(csdomwedge[1,1]*100,digits=3),nsmall=3)
csdomvalwedge<-read.csv("Table 7/csdomvalwedge.csv")
csdomvalwedge<-round(csdomvalwedge[1,1]/1E06,digits=0)

csdomquota<-read.csv("Table 7/csdomquota.csv")
csdomquota<-format(round(csdomquota[1,1]*100,digits=3),nsmall=3)
csdomvalquota<-read.csv("Table 7/csdomvalquota.csv")
csdomvalquota<-round(csdomvalquota[1,1]/1E06,digits=0)

domwelfeffects<-as.table(matrix(c(csdomwedge,paste0("[",csdomvalwedge,"]"),csdomquota,paste0("[",csdomvalquota,"]")),ncol=4))

csforwedge<-read.csv("Table 7/csforwedge.csv")
csforwedge<-format(round(csforwedge[1,1]*100,digits=3),nsmall=3)
csforvalwedge<-read.csv("Table 7/csforvalwedge.csv")
csforvalwedge<-round(csforvalwedge[1,1]/1E06,digits=0)

csforquota<-read.csv("Table 7/csforquota.csv")
csforquota<-format(round(csforquota[1,1]*100,digits=3),nsmall=3)
csforvalquota<-read.csv("Table 7/csforvalquota.csv")
csforvalquota<-round(csforvalquota[1,1]/1E06,digits=0)

forwelfeffects<-as.table(matrix(c(csforwedge,paste0("[",csforvalwedge,"]"),csforquota,paste0("[",csforvalquota,"]")),ncol=4))

cstotwedge<-read.csv("Table 7/cstotwedge.csv")
cstotwedge<-format(round(cstotwedge[1,1]*100,digits=3),nsmall=3)
cstotvalwedge<-read.csv("Table 7/cstotvalwedge.csv")
cstotvalwedge<-round(cstotvalwedge[1,1]/1E06,digits=0)

cstotquota<-read.csv("Table 7/cstotquota.csv")
cstotquota<-format(round(cstotquota[1,1]*100,digits=3),nsmall=3)
cstotvalquota<-read.csv("Table 7/cstotvalquota.csv")
cstotvalquota<-round(cstotvalquota[1,1]/1E06,digits=0)

totwelfeffects<-as.table(matrix(c(cstotwedge,paste0("[",cstotvalwedge,"]"),cstotquota,paste0("[",cstotvalquota,"]")),ncol=4))

table7<-rbind(milkvalueeffects,dairyvalueeffects,dairyexporteffects,domwelfeffects,forwelfeffects,totwelfeffects)
table7<-as.data.frame(table7)
colnames(table7)<- c("Ideal Wedges","($ mn)","Milk Quotas","($ mn)")
rownames(table7)<- c("Milk Processing Val.","Dairy Product Val.","Dairy Export Val.","US CS","ROW CS","Total CS")

if (!dir.exists("Table 7/txt file")) {
  dir.create("Table 7/txt file")
}

sink(file.path("Table 7/txt file","table7.txt"))
print(table7,row.names = T)
sink()
