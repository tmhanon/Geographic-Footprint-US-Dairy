# Title: FMMO project
# Author: Pierre Merel
# Date: July 2026
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

options(dplyr.summarise.inform= FALSE)
suppressPackageStartupMessages({library(plyr)
                                library(tidyverse)})

setwd("../../MANUSCRIPT CLAIMS")

#############################################
##SECTION 1
#############################################

claimintro<-"Claim: The modest share of farm milk in domestic dairy product value (18%)..."

milkshare<-read.csv("Section 1/milkshareofdairydollar.csv") %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%")) 

colnames(milkshare)<-NULL
rownames(milkshare)<-"Share of farm milk in total value of U.S. dairy production:"

if (!dir.exists("Section 1/txt file")) {
  dir.create("Section 1/txt file")
}

sink(file.path("Section 1/txt file","claim.txt"))
cat(claimintro, "\n\n")
cat(rep("-",80), sep="", "\n\n")
print(milkshare,row.names = T)
sink()


#############################################
##SECTION 5.6
#############################################

claimoilseedshare<-"Claim: For example, oilseed crops occupy a very small 
share of cropland in the Pacific Northwest and  Unregulated regions 
(less than 1%)..."

oilseedareashare<-read.csv("Section 5.6/oilseedareashare.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  rename("Region"=DOMI) %>%
  rename("Oilseed cropland share"=Val) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  mutate(across(where(is.numeric), ~ format(.x,,drop0Trailing = F,trim= T))) 

if (!dir.exists("Section 5.6/txt file")) {
  dir.create("Section 5.6/txt file")
}

sink(file.path("Section 5.6/txt file","claim1.txt"))
cat(claimoilseedshare, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(oilseedareashare,row.names = F)
sink()

claimareaelas<-"Claim: As expected, acreage elasticities are much larger
in magnitude than the output supply elasticities..."

elascroparea<-read.csv("Section 5.6/elascroparea.csv") 
elascroparea<-reshape(elascroparea,idvar="DOMI",timevar="L",direction="wide") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate_if(is.numeric, round, digits = 2) 
colnames(elascroparea)<- c("Region","Grains","Oilseeds","Hay","Silage","Other Crops")

elascrop<-read.csv("../MANUSCRIPT TABLES/Table D.3/elascrop.csv") 
elascrop<-reshape(elascrop,idvar="DOMI",timevar="L",direction="wide") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate_if(is.numeric, round, digits = 2) 
colnames(elascrop)<- c("Region","Grains","Oilseeds","Hay","Silage","Other Crops")

sink(file.path("Section 5.6/txt file","claim2.txt"))
cat(claimareaelas, "\n\n")
cat(rep("-",60), sep="", "\n\n")
cat("Crop area elasticities","\n\n")
print(elascroparea,row.names = F)
cat("Crop output elasticities","\n\n")
print(elascrop,row.names = F)
sink()


#############################################
##SECTION 5.7
#############################################

claimmilkelas<-"Claim: For instance,  the Appalachian region, which has
the lowest milk supply elasticity, has the largest feed
crop share of the milk dollar and the largest silage share
of dairy feed expenditure. The Southwest region, which has
the largest milk supply elasticity, has the lowest feed 
crop share of the milk dollar, the lowest silage share of
dairy feed expenditure, and the lowest land share of silage
revenue. "

milksupply<-read.csv("Section 5.7/milksupply.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  rename("Milk supply elasticity"=Val) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(across(where(is.numeric), ~ format(.x,,drop0Trailing = F,trim= T))) 

feedsharemilkrev<-read.csv("Section 5.7/feedsharemilkrev.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  rename("Feed crop share of milk revenue"=Val) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  mutate(across(where(is.numeric), ~ format(.x,,drop0Trailing = F,trim= T))) 

silfeedshare<-read.csv("Section 5.7/silfeedshare.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  rename("Silage share of feed expenditure"=Val) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  mutate(across(where(is.numeric), ~ format(.x,,drop0Trailing = F,trim= T))) 

landsharesilrev<-read.csv("Section 5.7/landsharesilrev.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  rename("Land share of silage revenue"=Val) %>%
  mutate_if(is.numeric, round, digits = 4) %>%
  mutate(across(where(is.numeric), ~ format(.x,,drop0Trailing = F,trim= T))) 

milksupplyclaim1<-merge(feedsharemilkrev,silfeedshare,by="DOMI",sort=F)
milksupplyclaim2<-merge(milksupplyclaim1,landsharesilrev,by="DOMI",sort=F)
milksupplyclaim3<-merge(milksupplyclaim2,milksupply,by="DOMI",sort=F) %>%
  rename(Region=DOMI)

if (!dir.exists("Section 5.7/txt file")) {
  dir.create("Section 5.7/txt file")
}

sink(file.path("Section 5.7/txt file","claim.txt"))
cat(claimmilkelas, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(milksupplyclaim3,row.names = F)
sink()


#############################################
##SECTION 5.8
#############################################

claimcompdembutpow<-"Claim: The one exception is the Appalachian region, 
where the inelastic derived demand for milk components in
butter-powder products can be traced to a particularly low 
cost share of milk components relative to other inputs."

compbutpowcostshare<-read.csv("Section 5.8/compbutpowcostshare.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  rename("Region" = DOMI) %>%
  rename("Milk component cost share in butter-powder products"=Val) %>%
  mutate_if(is.numeric, round, digits = 3) 

if (!dir.exists("Section 5.8/txt file")) {
  dir.create("Section 5.8/txt file")
}

sink(file.path("Section 5.8/txt file","claim.txt"))
cat(claimcompdembutpow, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(compbutpowcostshare,row.names = F)
sink()


#############################################
##SECTION 6 INTRO
#############################################

claim0<-"Claim: Wedges for the butterfat component are 
extremely close to one for all product categories
and all regions."

fatwedges<-read.csv("../MANUSCRIPT TABLES/Table 2/delta.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  dplyr::filter(!DOMI=="Unregulated") %>%
  dplyr::filter(K == "Fat") %>%
  dplyr::select(DOMI,N,Val) %>%
  mutate(N=case_when(N=="Beverage" ~ "Beverages" , T ~ N)) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  mutate(across(where(is.numeric), ~ format(.x,,drop0Trailing = F,trim= T))) 

colnames(fatwedges)<-c("Region","Product","Butterfat Wedge")

if (!dir.exists("Section 6 intro/txt file")) {
  dir.create("Section 6 intro/txt file",recursive=T)
}

sink(file.path("Section 6 intro/txt file","claim.txt"))
cat(claim0, "\n\n")
cat(rep("-",60), sep="", "\n\n")
cat("Regional Wedges for Butterfat Component","\n\n")
print(fatwedges,row.names = F)
sink()


#############################################
##SECTION 6.1
#############################################

claim1a<-"Claim: milk shipments from the Unregulated region towards its 
non-Western destination regions are eliminated,..."

milkship<-read.csv("Section 6.1/milkship.csv") %>%
  mutate(Val=format(round(Val*100, digits=2),nsmall=2))  %>%
  mutate(Val=paste0(Val,"%")) %>%
  filter(DOMI %in% c("Unregulated","California","Pacific-Northwest","Arizona")) %>%
  filter(!DOMJ %in% c("Unregulated","California","Pacific-Northwest","Arizona")) %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(DOMJ=gsub("-"," ",DOMJ)) 

colnames(milkship) <- c("Origin region","Destination region", "Change in milk shipments")

note1<-"This table displays changes in shipments of farm milk from Western regions
(namely California, Pacific Northwest, Arizona, and Unregulated) towards 
non-Western regions. Changes in bilateral trade flows need to be interpreted
with caution as our milk shipment data includes shipments via competing 
routes, which implies that bilateral shipment changes cannot always be 
interpreted in isolation. Baseline shipments from Western regions towards 
non-Western regions include, in addition to shipments from Unregulated, 
relatively modest shipments from both Arizona and Pacific Northwest. These
shipments also go to zero in the counterfactual, so that the milk market 
becomes segmented between Western and non-Western states."

if (!dir.exists("Section 6.1/txt file")) {
  dir.create("Section 6.1/txt file")
}

sink(file.path("Section 6.1/txt file","claim1.txt"))
cat(claim1a, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(milkship,row.names = F)
cat("\n\n")
cat(note1)
sink()

claim1b<-"Claim: Reductions in regional milk output translate
into reductions in regional dairy silage production. These 
reductions, which are not reported here, are modest, typically 
less than 1%, except in the Southwest where silage production 
decreases by 5.9%. The use of other dairy feed crops also 
decreases markedly in the Southwest, by 7.3%."

deltafeedcrop<-read.csv("Section 6.1/deltafeedcropuse.csv") 
deltafeedcrop<-reshape(deltafeedcrop,idvar="DOMI",timevar="SUBL",direction="wide") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate_if(is.numeric, round, digits = 3) 

colnames(deltafeedcrop)<-c("Region","Grains","Oilseeds","Hay","Silage")

note2<-"Note: This table reports changes in regional feed crop use.
Regional use and production of silage are identical since 
this crop is not traded."

sink(file.path("Section 6.1/txt file","claim2.txt"))
cat(claim1b, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(deltafeedcrop,row.names = F)
cat("\n\n")
cat(note2)
sink()

claim2<-"Claim: We find that an increase in transportation 
cost by 62% or more would cause all farm milk shipments to
disappear. (This claim is also stated in Section 1.)"

milkprodshareship<-read.csv("Section 6.1/milkprodshareshiptot.csv") %>%
  mutate(Val=format(round(Val, digits=8),nsmall=8)) 
rownames(milkprodshareship)<-"Initial share of milk production shipped out:"
colnames(milkprodshareship)<-NULL
tau1<-(read.csv("Section 6.1/tau1.csv")[1,1]-1)*100
deltamilkprodshareship1<-read.csv("Section 6.1/deltamilkprodshareshiptot1.csv") %>%
  mutate(Val=format(round(Val, digits=8),nsmall=8)) 
rownames(deltamilkprodshareship1)<-paste("Change in the share of milk production shipped out when milk transportation costs increase by",tau1,"%:")
colnames(deltamilkprodshareship1)<-NULL
tau2<-(read.csv("Section 6.1/tau2.csv")[1,1]-1)*100
deltamilkprodshareship2<-read.csv("Section 6.1/deltamilkprodshareshiptot2.csv") %>%
  mutate(Val=format(round(Val, digits=8),nsmall=8)) 
rownames(deltamilkprodshareship2)<-paste("Change in the share of milk production shipped out when milk transportation costs increase by",tau2,"%:")
colnames(deltamilkprodshareship2)<-NULL

sink(file.path("Section 6.1/txt file","claim3.txt"))
cat(claim2, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(milkprodshareship)
print(deltamilkprodshareship1)
print(deltamilkprodshareship2)
sink()

#############################################
##SECTION 6.2
#############################################

claim2a<-"Claim: the protein price wedge is larger than one for 
beverages, soft products and cheese in all FMMO regions."

wedgesclaim<-read.csv("../MANUSCRIPT TABLES/Table 2/delta.csv") %>%
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  dplyr::filter(!DOMI=="Unregulated") %>%
  dplyr::filter(!N %in% c("Butter-Powder")) %>%
  dplyr::filter(K %in% c("Protein") ) %>%
  dplyr::select(DOMI,N,Val) %>%
  mutate(N=case_when(N=="Beverage" ~ "Beverages" , T ~ N)) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(across(where(is.numeric), ~ format(.x,,drop0Trailing = F,trim= T))) 

wedgesclaim<-reshape(wedgesclaim,idvar="DOMI",timevar="N",direction="wide") 
colnames(wedgesclaim)<-c("Region","Beverages","Softs","Cheese")

if (!dir.exists("Section 6.2/txt file")) {
  dir.create("Section 6.2/txt file",recursive=T)
}

sink(file.path("Section 6.2/txt file","claim.txt"))
cat(claim2a, "\n\n")
cat(rep("-",60), sep="", "\n\n")
cat("Price wedges for Protein (relative to Butter-Powder):","\n\n")
print(wedgesclaim,row.names = F)
sink()


#############################################
##SECTION 6.3
#############################################

claim3a<-"Claim: Thus, the price of beverage products falls in all
FMMO regions while softs, cheese, and butter-powder products 
generally become more expensive."

producerprices<-read.csv("Section 6.4/producerprices.csv") %>% 
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=format(round(Val*100, digits=3),nsmall=3)) %>%
  mutate(Val=paste0(Val,"%"))

producerprices<-reshape(producerprices,idvar="DOMI",timevar="N",direction="wide") 
colnames(producerprices) <-c("Region","Beverages","Softs","Cheese","Butter-Powder")


if (!dir.exists("Section 6.3/txt file")) {
  dir.create("Section 6.3/txt file")
}

sink(file.path("Section 6.3/txt file","claim1.txt"))
cat(claim3a, "\n\n")
cat(rep("-",60), sep="", "\n\n")
cat("Changes in regional producer prices of dairy products","\n\n")
print(producerprices,row.names = F)
sink()


claim3b<-"Claim: For example, the large increase in beverage
production in the Southeast (+5.4%) coincides with more
shipments of beverage products to Florida (+7.6%), the
Pacific Northwest (+7.9%), and the Unregulated region (+9.0%)."

southeastdeltabevship<-read.csv("Section 6.3/deltabevship.csv") %>% 
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=format(round(Val*100, digits=2),nsmall=2)) %>%
  mutate(Val=paste0(Val,"%")) %>%
  filter(DOMI %in% c("Florida","Pacific Northwest","Unregulated")) 
colnames(southeastdeltabevship) <- c("Region","Change in dairy beverage shipment from Southeast")

sink(file.path("Section 6.3/txt file","claim2.txt"))
cat(claim3b, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(southeastdeltabevship,row.names = F)
sink()

#############################################
##SECTION 6.4
#############################################

claim4<-"Claim: The producer price of beverage milk decreases
in all domestic regions except the Unregulated region. The
consumer price index for beverage milk decreases in all 
domestic regions. The domestic and foreign consumer price 
indices for softs, cheese, and butter-powder all increase
(except for cheese in California), even if the producer 
prices for these products do not increase in all domestic regions.
[...] In California, this is due to a combination of a relatively
small decrease in the price of local beverage products (1.2% as
opposed to 2–3% for beverages manufactured in many other 
regions) combined with a particularly high baseline expenditure 
share on butter-powder products (18.9%), the consumer price of 
which rises by 1.8%."

producerprices<-read.csv("Section 6.4/producerprices.csv") %>% 
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=format(round(Val*100, digits=3),nsmall=3)) %>%
  mutate(Val=paste0(Val,"%"))

producerprices<-reshape(producerprices,idvar="DOMI",timevar="N",direction="wide") 
colnames(producerprices) <-c("Region","Beverages","Softs","Cheese","Butter-Powder")

consumerprices<-read.csv("Section 6.4/prindex.csv") %>% 
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=format(round(Val*100, digits=3),nsmall=3)) %>%
  mutate(Val=paste0(Val,"%"))

consumerprices<-reshape(consumerprices,idvar="DOMI",timevar="N",direction="wide") 
colnames(consumerprices) <-c("Region","Beverages","Softs","Cheese","Butter-Powder")

forconsumerprices<-read.csv("Section 6.4/prindexfor.csv") %>%
  mutate(Val=format(round(Val*100, digits=3),nsmall=3)) %>%
  mutate(Val=paste0(Val,"%"))
forconsumerprices<-t(forconsumerprices)
forconsumerprices<-matrix(forconsumerprices[2,],nrow=1)
forconsumerprices<-data.frame(forconsumerprices)
forconsumerprices$X0 <- "--"
forconsumerprices$Region <- "Rest of the World"
forconsumerprices<-forconsumerprices %>% select(Region,X0,X1,X2,X3) 
colnames(forconsumerprices) <- colnames(consumerprices)

consumerprices<-rbind(consumerprices,forconsumerprices)

calbuttpowexpshare<-read.csv("Section 6.4/califbuttpowexpshare.csv") %>%
  mutate(Val=format(round(Val*100, digits=3),nsmall=3)) %>%
  mutate(Val=paste0(Val,"%")) 
rownames(calbuttpowexpshare) <-"Expenditure share on butter-powder products in California:"
colnames(calbuttpowexpshare) <- NULL

if (!dir.exists("Section 6.4/txt file")) {
  dir.create("Section 6.4/txt file")
}

sink(file.path("Section 6.4/txt file","claim1.txt"))
cat(claim4, "\n\n")
cat(rep("-",60), sep="", "\n\n")
cat("Producer prices", "\n\n")
print(producerprices,row.names = F)
cat("\n\n")
cat("Consumer price indices", "\n\n")
print(consumerprices,row.names = F)
cat("\n\n")
print(calbuttpowexpshare,row.names = T)
sink()

claim5<-"Claim: Most beverages consumed in California are 
sourced locally as the expenditure share on local beverages
reaches 85.5%. The beverage price index decreases by only 
1.3% in California."

calbevexpshare<-read.csv("Section 6.4/califbevlocexpshare.csv") %>%
  mutate_if(is.numeric, round, digits = 5) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%")) 
rownames(calbevexpshare) <-"Expenditure share on local beverages in California:"
colnames(calbevexpshare) <- NULL

calbevprindex<-read.csv("Section 6.4/califbevprindex.csv") %>%
  mutate_if(is.numeric, round, digits = 5) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%")) 
rownames(calbevprindex) <-"Change in beverages price index in California:"
colnames(calbevprindex) <- NULL

sink(file.path("Section 6.4/txt file","claim2.txt"))
cat(claim5, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(calbevexpshare,row.names = T)
cat("\n")
print(calbevprindex,row.names = T)
sink()

claim5a<-"Claim: Land rents fall across all domestic regions along with
farm milk and silage prices [...] the Southwest experiences the
largest decrease in the price of dairy silage (7.4%), while the 
effect in the Northeast is due to an unusually large share of 
cropland dedicated to dairy silage (22.4% as opposed to less 
than 10%, and often less than 5%, in other regions)."

silagepr<-read.csv("Section 6.4/silagepr.csv") %>% 
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate_if(is.numeric, round, digits = 5) %>%
  mutate(Val=100*Val) %>% 
  mutate(Val=format(Val,drop0Trailing = F,trim= T)) %>%
  mutate(Val=paste0(Val,"%")) 
colnames(silagepr) <- c("Region","Change in silage price")

silageareashare<-read.csv("Section 6.4/silageareashare.csv") %>% 
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate_if(is.numeric, round, digits = 5) %>%
  mutate(Val=100*Val) %>% 
  mutate(Val=format(Val,drop0Trailing = F,trim= T)) %>%
  mutate(Val=paste0(Val,"%")) 
colnames(silageareashare) <- c("Region","Baseline cropland share")

sink(file.path("Section 6.4/txt file","claim3.txt"))
cat(claim5a, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(silagepr,row.names = F)
cat("\n\n")
print(silageareashare,row.names = F)
sink()


#############################################
##SECTION 6.5
#############################################

claim5b<-"Claim: The decrease in total milk value is most sensitive
to the value of varsigma^cheese as cheese represents the 
most significant share of component value in many FMMO regions."

deltamilkvaltots1<-read.csv("../MANUSCRIPT TABLES/Table E.1/deltamilkvaltot_s1.csv") %>% 
  mutate(Val=format(round(Val*100, digits=3),nsmall=3)) %>%
  mutate(Val=paste0(Val,"%"))
rownames(deltamilkvaltots1)<-c("Baseline:")

deltamilkvaltots8<-read.csv("../MANUSCRIPT TABLES/Table E.1/deltamilkvaltot_s8.csv") %>% 
  mutate(Val=format(round(Val*100, digits=3),nsmall=3)) %>%
  mutate(Val=paste0(Val,"%"))
rownames(deltamilkvaltots8)<-c("Varsigma^bev X 2:")

deltamilkvaltots9<-read.csv("../MANUSCRIPT TABLES/Table E.1/deltamilkvaltot_s9.csv") %>% 
  mutate(Val=format(round(Val*100, digits=3),nsmall=3)) %>%
  mutate(Val=paste0(Val,"%"))
rownames(deltamilkvaltots9)<-c("Varsigma^softs X 2:")

deltamilkvaltots10<-read.csv("../MANUSCRIPT TABLES/Table E.1/deltamilkvaltot_s10.csv") %>% 
  mutate(Val=format(round(Val*100, digits=3),nsmall=3)) %>%
  mutate(Val=paste0(Val,"%"))
rownames(deltamilkvaltots10)<-c("Varsigma^cheese X 2:")

deltamilkvaltots11<-read.csv("../MANUSCRIPT TABLES/Table E.1/deltamilkvaltot_s11.csv") %>% 
  mutate(Val=format(round(Val*100, digits=3),nsmall=3)) %>%
  mutate(Val=paste0(Val,"%"))
rownames(deltamilkvaltots11)<-c("Varsigma^b-p X 2:")

deltamilkvaltotcomp<-rbind(deltamilkvaltots1,deltamilkvaltots8,deltamilkvaltots9,deltamilkvaltots10,deltamilkvaltots11)
colnames(deltamilkvaltotcomp)<-c("Change in milk production value (%)")

dairyprodcompvalshare<-read.csv("Section 6.5/dairyprodcompvalshare.csv") %>% 
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate_if(is.numeric, round, digits = 4) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%"))

dairyprodcompvalshare<-reshape(dairyprodcompvalshare,idvar="DOMI",timevar="N",direction="wide") 
colnames(dairyprodcompvalshare) <-c("Region","Beverages","Softs","Cheese","Butter-Powder")

if (!dir.exists("Section 6.5/txt file")) {
  dir.create("Section 6.5/txt file",recursive=T)
}

sink(file.path("Section 6.5/txt file","claim.txt"))
cat(claim5b, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(deltamilkvaltotcomp,row.names = T)
cat("\n\n")
cat("Share of dairy product in regional component value:","\n\n")
print(dairyprodcompvalshare,row.names = F)
sink()


#############################################
##SECTION 6.6
#############################################

claim6<-"Claim: Relative to baseline, regional consumer prices of beverage
products fall by 2.6–6.2% and those of butter-powder rise
by 3.6–7.3%. As a result, the foreign dairy price index rises
by 2.8%. In contrast, under the milk quotas domestic beverage
prices only fall by 0.6–3.1% and the foreign price index rises
by 1.0%."

priceminwed<-read.csv("Section 6.6/pricechangeminwed.csv") %>%
  mutate(Val=format(round(Val*100, digits=3),nsmall=3)) %>%
  mutate(Val=paste0(Val,"%")) %>%
  filter(N %in% c("Beverage","Butter-Powder"))

pricemaxwed<-read.csv("Section 6.6/pricechangemaxwed.csv") %>%
  mutate(Val=format(round(Val*100, digits=3),nsmall=3)) %>%
  mutate(Val=paste0(Val,"%")) %>%
  filter(N %in% c("Beverage","Butter-Powder"))

pricechangeswed<-merge(priceminwed,pricemaxwed,by="N")
colnames(pricechangeswed) <- c("Product","Minimum change","Maximum change")

forprindexwed<-read.csv("Section 6.6/forprindexchangewed.csv") %>%
  mutate_if(is.numeric, round, digits = 5) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%")) 
rownames(forprindexwed) <-"Change in foreign dairy price index:"
colnames(forprindexwed) <- NULL

priceminqu<-read.csv("Section 6.6/pricechangeminqu.csv") %>%
  mutate_if(is.numeric, round, digits = 5) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%")) %>%
  filter(N %in% c("Beverage"))

pricemaxqu<-read.csv("Section 6.6/pricechangemaxqu.csv") %>%
  mutate_if(is.numeric, round, digits = 5) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%")) %>%
  filter(N %in% c("Beverage"))

pricechangesqu<-merge(priceminqu,pricemaxqu,by="N")
colnames(pricechangesqu) <- c("Product","Minimum change","Maximum change")

forprindexqu<-read.csv("Section 6.6/forprindexchangequ.csv") %>%
  mutate_if(is.numeric, round, digits = 5) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%")) 
rownames(forprindexqu) <-"Change in foreign dairy price index:"
colnames(forprindexqu) <- NULL

if (!dir.exists("Section 6.6/txt file")) {
  dir.create("Section 6.6/txt file")
}

sink(file.path("Section 6.6/txt file","claim1.txt"))
cat(claim6, "\n\n")
cat(rep("-",60), sep="", "\n\n")
cat("Ideal Wedges Counterfactual","\n\n")
print(pricechangeswed,row.names = F)
cat("\n")
print(forprindexwed,row.names = T)
cat("\n\n")
cat("Milk Quota Counterfactual","\n\n")
print(pricechangesqu,row.names = F)
cat("\n")
print(forprindexqu,row.names = T)
sink()


claim7<-"Under the quotas, domestic beverage prices fall
relative to the status quo, yet they are higher 
than in competitive equilibrium."

bevpricediff<-read.csv("Section 6.6/bevpricediff.csv") %>%
  mutate(Val=format(round(Val*100, digits=3),nsmall=3)) %>%
  mutate(Val=paste0(Val,"%")) 
colnames(bevpricediff)<-c("Region","Difference btw bev price under milk quotas and competition")

sink(file.path("Section 6.6/txt file","claim2.txt"))
cat(claim7, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(bevpricediff,row.names = F)


claim8<-"Claim: Indeed, our model implies that quota
rents represent only 0.5% of the total value of milk. 
(This claim is also made in Section 1.)"

quotarentshare<-read.csv("Section 6.6/quotarentshare.csv") %>%
  mutate_if(is.numeric, round, digits = 5) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%")) 
rownames(quotarentshare) <-"Share of quota rent in total milk value:"
colnames(quotarentshare) <- NULL

sink(file.path("Section 6.6/txt file","claim3.txt"))
cat(claim8, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(quotarentshare,row.names = T)
sink()


claim9<-"Claim: In our model, the derived demand for manufacturing
milk can be made more elastic by increasing epsilon_0.
However, even if we increase epsilon_0 to a value of, say, 
100, the resulting derived demand for manufacturing milk
is too small to make component pricing socially more efficient."

cstotwed<-read.csv("Section 6.6/cstot100wed.csv") %>%
  mutate_if(is.numeric, round, digits = 5) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%")) 
rownames(cstotwed) <-"Change in welfare with ideal wedges when epsilon_0=100:"
colnames(cstotwed) <- NULL

cstotqu<-read.csv("Section 6.6/cstot100qu.csv") %>%
  mutate_if(is.numeric, round, digits = 5) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%")) 
rownames(cstotqu) <-"Change in welfare with milk quotas when epsilon_0=100:"
colnames(cstotqu) <- NULL

sink(file.path("Section 6.6/txt file","claim4.txt"))
cat(claim9, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(cstotwed,row.names = T)
cat("\n")
print(cstotqu,row.names = T)
sink()


claim10<-"Claim: Indeed, our model implies that component 
wedges could further be adjusted to increase
the aggregate land rent by only up to 0.35% relative
to its baseline level without reducing domestic consumer
surplus (and without reducing land rents in any region)."

landrentmax<-read.csv("Section 6.6/landrentmax.csv") %>%
  mutate_if(is.numeric, round, digits = 5) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%")) 
rownames(landrentmax) <-"Maximum land rent increase (relative to baseline) that preserves domestic consumer surplus: "
colnames(landrentmax) <- NULL

sink(file.path("Section 6.6/txt file","claim5.txt"))
cat(claim10, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(landrentmax,row.names = T)
sink()

#############################################
##APPENDIX F.3
#############################################

claim11<-"Claim: If we keep the condition, higher milk processing prices
need to be reached in regions where milk shipments would originate,
namely California, Pacific Northwest, Arizona, and Unregulated, 
resulting in land rents in these regions exceeding their baseline
value to the detriment of domestic consumers. However, the
refinement is almost inconsequential in terms of aggregate 
welfare measures."

milkprocprice<-read.csv("Appendix F.3/milkprocprice.csv") %>% 
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=format(round(Val*100, digits=5),nsmall=5)) %>%
  mutate(Val=paste0(Val,"%"))

milkprocpricearb<-read.csv("Appendix F.3/milkprocpricearb.csv") %>% 
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate(Val=format(round(Val*100, digits=5),nsmall=5)) %>%
  mutate(Val=paste0(Val,"%"))

milkprocpricecomp<-merge(milkprocprice,milkprocpricearb,by="DOMI",sort=F)
colnames(milkprocpricecomp) <- c("Region","Without Full Arbitrage (Preferred)","With Full Arbitrage")

shadowvaluerent<-read.csv("Appendix F.3/shadowvalrent.csv") %>% 
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate_if(is.numeric, round, digits = 0) 

shadowvaluerentarb<-read.csv("Appendix F.3/shadowvalrentarb.csv") %>% 
  mutate(DOMI=gsub("-"," ",DOMI)) %>%
  mutate_if(is.numeric, round, digits = 0) 

shadowvaluecomp<-merge(shadowvaluerent,shadowvaluerentarb,by="DOMI",sort=F)
colnames(shadowvaluecomp) <- c("Region","Without Full Arbitrage (Preferred)","With Full Arbitrage")

csdom<-read.csv("Appendix F.3/csdom.csv") %>%
  mutate_if(is.numeric, round, digits = 7) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%"))

csdomarb<-read.csv("Appendix F.3/csdomarb.csv") %>%
  mutate_if(is.numeric, round, digits = 7) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%"))

csdomcomp<-cbind(csdom,csdomarb)
rownames(csdomcomp) <-"Change in domestic consumer surplus:"
colnames(csdomcomp) <-c("Without Full Arbitrage (Preferred)","With Full Arbitrage")

csfor<-read.csv("Appendix F.3/csfor.csv") %>%
  mutate_if(is.numeric, round, digits = 7) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%"))

csforarb<-read.csv("Appendix F.3/csforarb.csv") %>%
  mutate_if(is.numeric, round, digits = 7) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%"))

csforcomp<-cbind(csfor,csforarb)
rownames(csforcomp) <-"Change in foreign consumer surplus:"
colnames(csforcomp) <-c("Without Full Arbitrage (Preferred)","With Full Arbitrage")

cstot<-read.csv("Appendix F.3/cstot.csv") %>%
  mutate_if(is.numeric, round, digits = 7) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%"))

cstotarb<-read.csv("Appendix F.3/cstotarb.csv") %>%
  mutate_if(is.numeric, round, digits = 7) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%"))

cstotcomp<-cbind(cstot,cstotarb)
rownames(cstotcomp) <-"Change in total consumer surplus:"
colnames(cstotcomp) <-c("Without Full Arbitrage (Preferred)","With Full Arbitrage")

pstot<-read.csv("Appendix F.3/prodrenttot.csv") %>%
  mutate_if(is.numeric, round, digits = 7) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%"))

pstotarb<-read.csv("Appendix F.3/prodrenttotarb.csv") %>%
  mutate_if(is.numeric, round, digits = 7) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%"))

pstotcomp<-cbind(pstot,pstotarb)
rownames(pstotcomp) <-"Change in total producer rents:"
colnames(pstotcomp) <-c("Without Full Arbitrage (Preferred)","With Full Arbitrage")

welfarecomp<-rbind(csdomcomp,csforcomp,cstotcomp,pstotcomp)

if (!dir.exists("Appendix F.3/txt file")) {
  dir.create("Appendix F.3/txt file")
}

sink(file.path("Appendix F.3/txt file","claim.txt"))
cat(claim11, "\n\n")
cat(rep("-",80), sep="", "\n\n")
cat("Regional Milk Processing Prices","\n\n")
print(milkprocpricecomp,row.names = F)
cat("\n\n")
cat(rep("-",80), sep="", "\n\n")
cat("Shadow Values of Regional Producer Rent Constraint","\n\n")
print(shadowvaluecomp,row.names = F)
cat("\n\n")
cat(rep("-",80), sep="", "\n\n")
cat("Aggregate Welfare Measures","\n\n")
print(welfarecomp,row.names = T)
cat("\n")
cat("Note: Change in domestic consumer surplus relative to value of domestic dairy consumption.
    Change in foreign consumer surplus relative to value of foreign dairy consumption.
    Change in total consumer surplus relative to the total value of dairy consumption.
    Change in total producer rents relative to the total value of dairy consumption.")
sink()

#############################################
##APPENDIX G
#############################################

claim12<-"Claim: our data imply that the component value
share of beverage products is 19.7%."

compsharebev<-read.csv("Appendix G/sharebev.csv") %>%
  mutate_if(is.numeric, round, digits = 5) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%")) 
rownames(compsharebev) <-"Share of beverages in total milk component value: "
colnames(compsharebev) <- NULL

if (!dir.exists("Appendix G/txt file")) {
  dir.create("Appendix G/txt file")
}

sink(file.path("Appendix G/txt file","claim1.txt"))
cat(claim12, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(compsharebev,row.names = T)
sink()

claim13<-"Claim: The blue dot represents a combination of 
elasticities with epsilon_1 = 0.1 and epsilon_2 = 0.3 as the
derived demand elasticities for milk in beverage 
and other uses, respectively. The values of
epsilon_1 and epsilon_2 are consistent with values 
simulated using the demand-side of our model."

elascompusebev<-read.csv("Appendix G/elascompusebevtot.csv") %>%
  mutate_if(is.numeric, round, digits = 5) 
rownames(elascompusebev) <-"Aggregate milk component demand elasticity for beverages: "
colnames(elascompusebev) <- NULL

elascompuseother<-read.csv("Appendix G/elascompusemantot.csv") %>%
  mutate_if(is.numeric, round, digits = 5) 
rownames(elascompuseother) <-"Aggregate milk component demand elasticity for other dairy products: "
colnames(elascompuseother) <- NULL

sink(file.path("Appendix G/txt file","claim2.txt"))
cat(claim13, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(elascompusebev,row.names = T)
cat("\n")
print(elascompuseother,row.names = T)
sink()

claim14<-"Claim: if we set epsilon_0 = 100, the calculated
derived demand elasticity for manufacturing milk 
reaches only 1.64, which, other things equal, is 
not enough to make component pricing more efficient 
than the quota. The reason why the derived demand
elasticity for manufacturing milk does not rise 
more substantially is that foreign demand only 
represents 3.6% of the consumption value of manufactured 
dairy products (softs, cheese, and butter-powder) 
at baseline. Making component pricing more efficient
than the quota would require, in addition to setting
epsilon_2 = 1.64, that eta be made less than one, for
instance."

elascompuseother100<-read.csv("Appendix G/elascompusemantot100.csv") %>%
  mutate_if(is.numeric, round, digits = 5) 
rownames(elascompuseother100) <-"Component demand elasticity for non-beverage dairy products when epsilon_0=100:"
colnames(elascompuseother100) <- NULL

equationg4<-read.csv("Appendix G/result_eta3.csv", header=F) %>%
  mutate_if(is.numeric, round, digits = 6) 
rownames(equationg4) <-"Value of LHS of Equation (G.4) with epsilon_2=1.64 and eta=3:"
colnames(equationg4) <- NULL

sharemanfor<-read.csv("Appendix G/sharemanfor.csv") %>%
  mutate_if(is.numeric, round, digits = 6) %>%
  mutate(Val=100*Val) %>%
  mutate(Val=paste0(Val,"%")) 
rownames(sharemanfor) <-"Foreign share of total consumption value of non-beverage dairy products:"
colnames(sharemanfor) <- NULL

equationg4eta1<-read.csv("Appendix G/result_eta1.csv", header=F) %>%
  mutate_if(is.numeric, round, digits = 6) 
rownames(equationg4eta1) <-"Value of LHS of Equation (G.4) with epsilon_2=1.64 and eta=1:"
colnames(equationg4eta1) <- NULL

equationg4eta05<-read.csv("Appendix G/result_eta0.5.csv", header=F) %>%
  mutate_if(is.numeric, round, digits = 5) 
rownames(equationg4eta05) <-"Value of LHS of Equation (G.4) with epsilon_2=1.64 and eta=0.5:"
colnames(equationg4eta05) <- NULL

sink(file.path("Appendix G/txt file","claim3.txt"))
cat(claim14, "\n\n")
cat(rep("-",60), sep="", "\n\n")
print(elascompuseother100,row.names = T)
cat("\n")
print(equationg4,row.names = T)
cat("\n")
print(sharemanfor,row.names = T)
cat("\n")
print(equationg4eta1,row.names = T)
cat("\n")
print(equationg4eta05,row.names = T)
sink()
