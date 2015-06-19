library(Hmisc)
library(plyr)
library(dplyr)
library(mtusRlocal)
library(magrittr)
library(reshape)
library(reshape2)
library(TraMineR)
library(mtusRlocal)

# 
# SN1 ￼￼￼￼Point number Area / quota indicator
# Each person was asked to complete diaries for 2 separate days (weekday & weekend day), so this has the values 1 or 2.
# SN1 + SN2 + SN3 + SN4 is the unique serial number for each record. There are 20981 records on the file.
# 

load(file = '/Users/giacomovagni/Documents/Data/TimeUse/UK/UK_R/dtindividual.RData')
load(file ='/Users/giacomovagni/Documents/Data/TimeUse/UK/UK_R/dtdiaryEpisode.RData')
load(file ='/Users/giacomovagni/Documents/Data/TimeUse/UK/UK_R/dtdiary.RData')
load(file ='/Users/giacomovagni/Documents/Data/TimeUse/UK/UK_R/dthhld.RData')
load(file ='/Users/giacomovagni/Documents/Data/TimeUse/UK/UK_R/dtworksheet.RData')
load(file ='/Users/giacomovagni/Documents/Data/TimeUse/UK/UK_R/dtweight.RData')

dtindividual = as.data.frame( apply(dtindividual, MARGIN = 2, FUN = as.character) )
dtdiary = as.data.frame( apply(dtdiary, MARGIN = 2, FUN = as.character) )

# compute hldid = sn1*1000 + sn2.
# compute persid = sn3.
# compute id = sn4.

# unique id - key 
as.numeric(dtindividual$SN1) * 1000

dtindividual$SN1 = gsub('[[:blank:]]', '', dtindividual$SN1)
dtindividual$SN2 = gsub('[[:blank:]]', '', dtindividual$SN2)
dtindividual$SN3 = gsub('[[:blank:]]', '', dtindividual$SN3)

dtindividual$householdid = paste(dtindividual$SN1, dtindividual$SN2, sep = '')
dtindividual$idno = paste(dtindividual$SN1, dtindividual$SN2, dtindividual$SN3, sep = '')
head(dtindividual)
colnames(dtindividual) = tolower(colnames(dtindividual))
dtindividual[1:4, 'idno']

dtdiary$SN1 = gsub('[[:blank:]]', '', dtdiary$SN1)
dtdiary$SN2 = gsub('[[:blank:]]', '', dtdiary$SN2)
dtdiary$SN3 = gsub('[[:blank:]]', '', dtdiary$SN3)

# unique id - key 
dtdiary$householdid = paste(dtdiary$SN1, dtdiary$SN2, sep = '')
dtdiary$idno = paste(dtdiary$SN1, dtdiary$SN2, dtdiary$SN3, sep = '')
head(dtdiary)
colnames(dtdiary) = tolower(colnames(dtdiary))
dtdiary[1:4, 'idno']

dtindividual[1:10, 'idno']
dtindividual$idno = as.numeric(dtindividual$idno)

dtdiary[1:10, 'idno']
dtdiary$idno = as.numeric(dtdiary$idno)

dtindividual = arrange(dtindividual, idno)
dtdiary = arrange(dtdiary, idno)

#################################
#################################

table(dtindividual$hhtype4)
levels(dtindividual$hhtype5)
levels(dtindividual$ageyngst)
levels(dtindividual$iagegrp)

data = filter(dtindividual, hhtype5 == "Married/cohab couple - with children <= 15 ")
# exclude the > 10 years old 
data = subset(data, ageyngst != "10 - 15 yrs") 
# select only the age group 25-44
data = subset(data, iagegrp == "25 -44 yrs") 

head(data)
table(data$ageyngst)
table(data$iagegrp)

# employement 
table(data$econact3)
data = subset(data, 
              econact3 == "Econ active - full time" | 
              econact3 == "Econ active - part time" |  
              econact3 == "Econ active - unemployed (ILO definition)" | 
              econact3 == "Econ inactive - looking after family/ home"
) 

# problem with number of people living in household 
data_c = filter(data, sn3 == 1 | sn3 == 2)
head(data_c)

data_c$spouse1 

data_c %>% 
  group_by (iethnic, isex) %>% 
  summarise(n(), mean(iage))

data_c %>% 
  group_by (ageyngst) %>% 
  summarise(n(), mean(iage))
  
#################################
#################################

dtdiary_cw = filter(dtdiary, ddayofwk == 'Saturday' | ddayofwk == 'Sunday')
dtdiary_cw = filter(dtdiary_cw, dtype == 'adult diary')

dtdiary_cwc = dtdiary_cw[dtdiary_cw$idno %in% data_c$idno, ] 
n_distinct(dtdiary_cwc$idno)
n_distinct(data_c$idno) 

dataM = merge(data_c, dtdiary_cwc, by = 'idno')
n_distinct(dataM$idno)

select(dataM, idno, contains('sn'), dtype, dmonth, ddayofwk, act1.001)[1:25,]

table(dataM$householdid.x)
table(dataM$householdid.y)

dm = dataM %>% 
  group_by(householdid.x) %>% 
  summarise( nhouse = n() ) %>% 
  merge(., dataM, all = T)

dm[1:10, c(1:10)]
dataC = filter(dm, nhouse == 2)

dataC[1:10, c(1:10)]

dataC %>% 
  group_by (isex) %>% 
  summarise(n(), mean(iage))

gay = dataC %>% 
  group_by (householdid.x, isex) %>% 
  summarise(gay = n())

gay[which(gay$gay == 2), ]
filter(dataC, householdid.x == 22471) [,c('isex')]

dataC = filter(dataC, householdid.x != 22471) 

dataC %>% 
  group_by (isex) %>% 
  summarise(n(),
            mean(iage), 
            sd(iage), 
            mean(ageyngst.x), 
            sd(ageyngst.x)
            )

dataC %>% 
  group_by (isex, econact3.x) %>% 
  summarise( n() 
  )

mary = dataC %>%
  group_by(isex, econact3.x) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

library(ggplot2)
qplot(x = econact3.x, y = freq, fill = isex, data = mary, geom="bar", stat="identity", 
      position="dodge") + theme_minimal()

mary
table(dataC$isex)
dataC[1:10,1:40]

dataC %>%
  group_by(ageyngst.x) %>%
  summarise (n = n()) %>% 
  mutate(freq = n / sum(n))

dataC %>%
  group_by(ageyngst.x, numchild.x) %>%
  summarise (n = n()) %>% 
  mutate(freq = n / sum(n))

dataC %>%
  group_by(isex) %>% 
  summarise (mean(iage), sd(iage))

dataC %>%
  group_by(isex, nssecb_3) %>%
  summarise (n = n()) %>% 
  mutate(freq = n / sum(n))

dataC %>%
  group_by(isex, nssecb_3) %>%
  summarise (n = n()) %>% 
  mutate(freq = n / sum(n))

gorpaf

dataC %>%
  group_by(gorpaf.x) %>%
  summarise (n = n()) %>% 
  mutate(freq = n / sum(n))

totpinc

dataC %>%
  group_by(totpinc.x) %>%
  summarise (n = n()) %>% 
  mutate(freq = n / sum(n))

homo = dataC[, c('idno', 'householdid.x', 'nssecb.3', 'iage', 'iethnic', 'x.q22b2')]
homo$iethnic_rec = ifelse(homo$iethnic == 'WHITE', 'WHITE', '0')
homo$nssecb.3 = relevel(homo$nssecb.3, 'Routine & manual occs')

homo$iage = as.numeric(as.character(homo$iage))

h = homo %>% 
  mutate(nssecb.3) %>% 
  group_by(householdid.x) %>% 
  mutate(occupPartner= rev(nssecb.3), agePartner = rev(iage)) 

cor(h$iage, h$agePartner)
hom = merge(homo, homo, by = 'householdid.x') 
cor(hom$iage.x, hom$iage.y)

levels(hom$nssecb.3.x)

chisq.test(hom$nssecb.3.x, hom$nssecb.3.y)$stdres
chisq.test(hom$nssecb.3.x, hom$nssecb.3.y)
chisq.test(hom$iethnic_rec.x, hom$iethnic_rec.y) 
chisq.test(hom$x.q22b2.x, hom$x.q22b2.y)

table(men = hom$x.q22b2.x, women = hom$x.q22b2.y)
 
table(dataC$x.q22b2, dataC$isex)
chisq.test(hom$iethnic_rec.x, hom$iethnic_rec.y)$stdres

# Marital Status 
pq55
# diploma in hihger edu 
q22b2

library(fmsb)
glmSummary4(glm(nssecb.3.x == "Managerial & professional occs" ~ nssecb.3.y, data = hom), fit = T)
glmSummary4(glm(nssecb.3.x == "Routine & manual occs" ~ nssecb.3.y, data = hom), fit = T)
glmSummary4(glm(nssecb.3.x == "Never worked" ~ nssecb.3.y, data = hom), fit = T)
exp(coef(glm(iethnic_rec.x == 'WHITE' ~ iethnic_rec.y, data = hom)))

prop.table(table(hom$x.q22b2.x, hom$x.q22b2.y),2)

glmSummary4(glm(nssecb.3.x == "Routine & manual occs" ~ nssecb.3.y, data = hom), fit = T)

dataC %>%
  group_by(pq55) %>%
  summarise (n = n()) %>% 
  mutate(freq = n / sum(n))

# save(dataC, file = '/Users/giacomovagni/Documents/Data/dataC.RData')
load(file = '/Users/giacomovagni/Documents/Data/dataC.RData') 

#################################
#################################

dtdiary_cw = filter(dtdiary, ddayofwk == 'Saturday' | ddayofwk == 'Sunday')
dtdiary_cw = filter(dtdiary_cw, dtype == 'adult diary')

dtd = tbl_df(dtdiary_cw)
dtd_c = dtd[dtd$idno %in% dataC$idno, ] 
n_idno = as.character(dtd$idno)
distinct(n_idno) 

# m = merge(dataC, dtd, by = c('idno', 'ddayofwk') )


dtd2 = dtd[ which(table(dtd$idno) == 2), 1:10]

dtd2 %>% 
  group_by (sn2) %>% 
  summarise( length(sn3) )

dtd2 %>% 
  group_by (sn2) %>% 
  summarise( n() )

