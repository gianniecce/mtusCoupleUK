library(Hmisc)
library(plyr)
library(dplyr)
library(mtusRlocal)
library(magrittr)
library(reshape)
library(reshape2)
library(TraMineR)
library(mtusRlocal)
library(Rage)

# 
# SN1 ????????????Point number Area / quota indicator
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

################################################  ###########################################################
################################################  Unique key ################################################
################################################  ###########################################################

# compute hldid = sn1*1000 + sn2.
# compute persid = sn3.
# compute id = sn4.

dtindividual$SN1 = gsub('[[:blank:]]', '', dtindividual$SN1)
dtindividual$SN2 = gsub('[[:blank:]]', '', dtindividual$SN2)
dtindividual$SN3 = gsub('[[:blank:]]', '', dtindividual$SN3)

dtindividual$householdid = paste(dtindividual$SN1, dtindividual$SN2, sep = '')
dtindividual$idno = paste(dtindividual$SN1, dtindividual$SN2, dtindividual$SN3, sep = '')
colnames(dtindividual) = tolower(colnames(dtindividual))

dtdiary$SN1 = gsub('[[:blank:]]', '', dtdiary$SN1)
dtdiary$SN2 = gsub('[[:blank:]]', '', dtdiary$SN2)
dtdiary$SN3 = gsub('[[:blank:]]', '', dtdiary$SN3)

dtdiary$householdid = paste(dtdiary$SN1, dtdiary$SN2, sep = '')
dtdiary$idno = paste(dtdiary$SN1, dtdiary$SN2, dtdiary$SN3, sep = '')
colnames(dtdiary) = tolower(colnames(dtdiary))

dtindividual$idno = as.numeric(dtindividual$idno)
dtdiary$idno = as.numeric(dtdiary$idno)

dtindividual = arrange(dtindividual, idno)
dtdiary = arrange(dtdiary, idno)

################################################  ###########################################################
################################################  Select Sample ############################################# 
################################################  ###########################################################

# select married / coha with children less than 15 
data = filter(dtindividual, hhtype5 == "Married/cohab couple - with children <= 15 ")
# exclude the > 10 years old 
data = subset(data, ageyngst != "10 - 15 yrs") 
# select only the age group 25-44
data = subset(data, iagegrp == "25 -44 yrs") 
# employement 
data = subset(data, 
              econact3 == "Econ active - full time" | 
                econact3 == "Econ active - part time" |  
                econact3 == "Econ active - unemployed (ILO definition)" | 
                econact3 == "Econ inactive - looking after family/ home"
) 

# problem with number of people living in household 
data_c = filter(data, sn3 == 1 | sn3 == 2)

# check 
table(data_c$spouse1) 
table(data_c$spouse2) 
###########

data_c %>% 
  group_by (iethnic, isex) %>% 
  summarise(n(), mean(iage))

data_c %>% 
  group_by (ageyngst) %>% 
  summarise(n(), mean(iage))

data_c %>% 
  filter(isex == 'MALE') %>%
  group_by (hhtype5, econact3) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n),  Total = sum(freq))

data_c %>% 
  filter(isex == 'FEMALE') %>%
  group_by (hhtype5, econact3) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n),  Total = sum(freq))

dt$econact3 = droplevels(dt$econact3)

dt = data_c %>% 
  group_by(sn1, sn2, sn3) %>% 
  mutate(occupPartner= rev(econact3)) 

dt %>% 
  group_by(isex, occupPartner) %>% 
  summarise( n = n() ) %>% 
  mutate(freq = n / sum(n),  Total = sum(freq))


dt$econact0 = ifelse(dt$econact == 'Econ active - in employment', 'Emp', 'Other')

men = filter(dt, isex == 'MALE') 
women = filter(dt, isex == 'FEMALE') 

dtt = merge(men, women, by = c('sn1', 'sn2'))
dtt[1:10, c('sn1', 'sn2', 'isex.x', 'isex.y', 'econact3.x', 'econact3.y', 'econact0.x', 'econact0.y')]

# correct 
prop.table(table(Men = dtt$econact3.x, Women = dtt$econact3.y) )
prop.table(table(Men = dtt$econact0.x, Women = dtt$econact0.y) ) * 100

########

dtdiary_cw = filter(dtdiary, ddayofwk == 'Saturday' | ddayofwk == 'Sunday')
dtdiary_cw = filter(dtdiary_cw, dtype == 'adult diary')

dtdiary_cwc = dtdiary_cw[dtdiary_cw$idno %in% data_c$idno, ] 
dataM = merge(data_c, dtdiary_cwc, by = 'idno')

dataC = filter(dataC, householdid.x != 22471) 

dm = dataM %>% 
  group_by(householdid.x) %>% 
  summarise( nhouse = n() ) %>% 
  merge(., dataM, all = T)

dm[1:10, c(1:10)]
dataC = filter(dm, nhouse == 2)
dataC[1:10, 1:10]

table(dataC$gorpaf.x) 
plot(table(dataC$hrs_job1))
barplot(table(dataC$totpinc.x)) 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
load(file = '/Users/giacomovagni/Documents/Data/dataC.RData') 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 









