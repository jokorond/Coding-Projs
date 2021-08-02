library(psych);library(rpart);library(caret);library(partykit);
library(AppliedPredictiveModeling)
library(tidyverse)
library(lubridate)
library(stringr)
library(dplyr)
library(readr)
##Subset the dataset for CESD, STAI, BAI data for each partcpt by test
nethealth <- (Basic_Survey_Merged_W1_8DeId_08_06_19_)
head(nethealth$C)

nethealth %>%
map(~select(nethealth, ~contains('CESD'), ~ends_with('_1')))

CESD_dat <-select(nethealth,egoid,CESD1_1,CESD2_1,CESD3_1,CESD4_1,CESD5_1,CESD6_1,CESD7_1,CESD8_1,CESD9_1,CESD10_1,
       CESD11_1,CESD12_1,CESD13_1,CESD14_1,CESD15_1,CESD16_1,CESD17_1,CESD18_1,CESD19_1,
       CESD20_1)
CESD_dat[rowSums(is.na(CESD_dat)) == 0,]
CESD_dat <- CESD_dat[!apply(is.na(CESD_dat) | CESD_dat == "", 1, all),]
head(CESD_dat)

CESDtotal <-select(nethealth, CESDOverall_1)
summary(CESDtotal)
CESDtotal<- as.numeric(CESDtotal)
  
CESDtotal <- CESDtotal[!apply(is.na(CESDtotal) | CESDtotal == "", 1, all),]
sd(CESDtotal)
(plot(CESDtotal))

STAI_dat <-select(nethealth, egoid, STAITraitTotal_1)
plot((STAI_dat))
STAItotal <- STAI_dat[!apply(is.na(STAI_dat) | STAI_dat=="", 1, all)]

ggplot(CESD_dat)
nethealth$CESDOverall_1
plot(nethealth$BAIsum_1, CESDtotal)
mean(nethealth$CESDOverall_1)
train(nethealth$BAIsum_1 ~ .,CESD_dat,method="rf",tuneLength=1,ntree=100,importance=T)

library(ggplot2)
hist(CESDtotal)

mhGPA <- select(nethealth,CESDOverall_1, STAITraitTotal_1, gpa_fa2015)
plot(CESDOverall_1, STAITraitTotal_1)

mhGPA <- mhGPA[!apply(is.na(mhGPA) | mhGPA == "", 1, all),]
plot(mhGPA$gpa_fa2015, mhGPA$CESDOverall_1)
plot(mhGPA$gpa_fa2015, mhGPA$STAITraitTotal_1)
plot(nethealth$CESDOverall_8 ~ nethealth$gpa_2018)
plot(nethealth$STAITraitGroup_8, nethealth$CESDOvera_8)

uniarcs <-distinct(arcsfall2015[,c('i','j')])
mutate(uniarcs, n())dssx
summarize(arcsfall2015)
tiesct <- uniarcs %>% group_by(i) %>% count(i)
mean(tiesct$n)
plot(tiesct ~ mhGPA$gpa_fa2015)

allvars<-data.frame(nethealth$egoid, nethealth$CESDOverall_1, nethealth$STAITraitTotal_1, nethealth$BAIsum_1, 
                    nethealth$Extraversion_1, nethealth$Agreeableness_1, nethealth$Conscientiousness_1, 
                    nethealth$Neuroticism_1, nethealth$Openness_1, nethealth$SelfEsteem_1, nethealth$Trust_1,
                    stringsAsFactors = TRUE)

tiesct<-data.frame(nethealth$egoid, tiesct$i, tiesct$n)
names(tiesct)[2] <- "tiect"
allvars <-merge(x = allvars, y = tiesct, by.x = "nethealth.egoid", by.y = "i", all.x = TRUE)
allvars<-mutate(allvars, nethealth$gpa_fa2015)
colnames(allvars) <- c("egoid","CESDtot","STAItot",'BAItot','Extrav','Agree','Conscie','Neuro','Open','SelfEst','Trust','GPA','tien')
mldataf<-allvars
psyscores <- c(mldataf$CESDtot,mldataf$STAItot,mldataf$BAItot)

plot(mldataf[,c(3,12)])
as.numeric(lmdf)

library(caret)
lmdf <-mldataf
lmdf<-na.omit(mldataf)
lmdf2 <- select(mldataf, -c('Trust','Open','Extrav','Agree','Conscie','Neuro'))
rf.out <- train(tien ~ ., (lmdf[,-1]),method="rf",tuneLength=1,ntree=100,importance=T)

plot(x=lmdf$CESDtot, y=lmdf$tien)

#Random forest

library(xtable)
vi.rf <- varImp(rf.out)
xtable(vi.rf$importance)

library(partykit)
cf.out <- partykit::cforest(tien ~ ., lmdf[,-1],ntree=100)
vi.cf = varimp(cf.out)
xtable(as.data.frame(vi.cf))



plot(vi.rf$importance ~ lmdf)

#More data cleaning so num values match their type; same for non-num obsvs.

lmdf$BAItot <- as.numeric(as.character(lmdf$BAItot))
lmdf$CESDtot <- as.numeric(as.character(lmdf$CESDtot))
lmdf$STAItot <- as.numeric(as.character(lmdf$STAItot))
lmdf$Open <- as.numeric(as.character(lmdf$Open))
lmdf$SelfEst <- as.numeric(as.character(lmdf$SelfEst))
lmdf$Trust <- as.numeric(as.character(lmdf$Trust))
lmdf$egoid <- as.character(as.factor(lmdf$egoid))

mldataf$BAItot <- as.numeric(as.character(mldataf$BAItot))
mldataf$CESDtot <- as.numeric(as.character(mldataf$CESDtot))
mldataf$STAItot <- as.numeric(as.character(mldataf$STAItot))
mldataf$Open <- as.numeric(as.character(mldataf$Open))
mldataf$SelfEst <- as.numeric(as.character(mldataf$SelfEst))
mldataf$Trust <- as.numeric(as.character(mldataf$Trust))
mldataf$Neuro <- as.numeric(as.character(mldataf$Neuro))
mldataf$Extrav <- as.numeric(as.character(mldataf$Extrav))
mldataf$egoid <- as.character(as.factor(mldataf$egoid))

plot(orange.df$age, orange.df$circumference)

ggplot(data=mldataf) +
  geom_col(mapping=aes(x=mldataf$Extrav, y=mldataf$tien))

library(ggplot2)
ggplot(data=lmdf) +
  geom_point(mapping=aes(x=lmdf$BAItot, y=lmdf$tien, color="student")) +
  geom_smooth(mapping=aes(x=lmdf$BAItot, y=lmdf$tien),method=lm)


ggplot(data=mldataf) +
  geom_point(mapping=aes(x=mldataf$Extrav, y=mldataf$tien, color="participant")) +
  geom_smooth(mapping=aes(x=mldataf$Extrav, y=mldataf$tien),method=lm)

ggplot(data=lmdf) +
  geom_point(mapping=aes(x=lmdf$STAItot, y=lmdf$tien, color="participant")) +
  geom_smooth(mapping=aes(x=lmdf$STAItot, y=lmdf$tien),method=lm)

ggplot(data=mldataf) +
  geom_point(mapping=aes(x=mldataf$STAItot, y=mldataf$tien, color="participant")) +
  geom_smooth(mapping=aes(x=mldataf$STAItot, y=mldataf$tien),method=lm)


library(vip)
library(pdp)
vip(rf.out)
p1 = partial(rf.out,"Extrav",rug=T)
plot(partial(rf.out,"tien"))
pi3 = partial(rf.out,c("STAItot","CESDtot"))
#pi4 = partial(rf.out,c("read","knowledge"))

plotPartial(p1,plot.engine = "ggplot2")
plotPartial(pi3,plot.engine = "ggplot2")
plotPartial(pi4,plot.engine = "ggplot2")
plotPartial(pi3,plot.engine = "ggplot2",levelplot=F)

plot(partial(cf.out,"STAItot",train=lmdf))
plot(partial(cf.out,"math",train=ecls.2))
