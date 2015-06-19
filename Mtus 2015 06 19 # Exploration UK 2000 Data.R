

table(dataC$ddayofwk)

dataC[1:10, c('isex', 'pq45', 'x.pq49a1', 'x.pq49a2', 'x.pq49a5')]

table(dataC$q21ba01)

table(dataC$q26a) # voluntary work during last 4 weeks 
table(dataC$q26b1) # voluntary work during last 4 weeks 

dataC[1:10, c('isex', 'q27a', 'q27b1', 'q27b2', 'q27b3', 'q27da', 'q27db', 'q27dc', 'q27eda', 'q27edb', 'q27eha', 'q27ehb', 'q27fa', 'q27fb')]

table(dataC$q27b1) # person helped 
sum(table(dataC$q27b1)) 
table(dataC$q27fa) # paid 

table(dataC$q28a) # attitude towards ... cooking 
table(dataC$q28c) # attitude towards ... cooking 
table(dataC$q28g, dataC$isex) # attitude towards ... repair 
table(dataC$q28i, dataC$isex) # attitude towards ... tidying house  
table(dataC$q28j, dataC$isex) # attitude towards ... washing 
table(dataC$q28k, dataC$isex) # attitude towards ... ironing  

table(dataC$q29a, dataC$isex) # did during last 4 weeks -> cinema 
table(dataC$q29d, dataC$isex) # did during last 4 weeks -> sport  
table(dataC$q30a, dataC$isex) # did during last 4 weeks -> nbr of times  
table(dataC$q30d, dataC$isex) # did during last 4 weeks -> nbr of times  

table(dataC$q34a, dataC$isex) # self-rated health 
table(dataC$q35d12, dataC$isex) # depression 

table(dataC$x.pq49a1, dataC$isex) # depression 
table(dataC$x.pq49a2, dataC$isex) # depression 
table(dataC$pq53, dataC$isex) # depression 

dataC[1:10, c('isex', 'pq53', 'x.pq49a1', 'x.pq49a2', 'x.pq49a3', 'x.pq49a4')]

table(dataC$pq54) 
table(dataC$pq55) 
table(dataC$livarr.x) 

dataC[1:10, c('isex', 'pq54', 'pq56')]

# working main job 
plot(table(dataC$dml3.111))

seq = select(dataC, householdid.x, idno, contains('act1'))
head(seq)

sek = seqdef(seq[,-c(1:2)], cpal = rainbow(200))
attributes(sek)$labels

seqWork = ifelse(seq == "Working time in main job", 1, 0)
sekW = seqdef(seqWork[,-c(1:2)], cpal = rainbow(2))

mean(rowSums(seqWork) * 10)
mean(as.numeric(as.character(dataC$dml3.111)) )

dataC$workthisday = ifelse( as.numeric(as.character(dataC$dml3.111))  == 0, 0, 1 )
table(dataC$workthisday)

# 300 mintes == 05:00 
dataC_sub = filter(dataC, workthisday == 0)

dc = dataC_sub %>% 
  group_by(householdid.x) %>% 
  summarise( nhouse2 = n() ) %>% 
  merge(., dataC_sub, all = T)

table(dc$nhouse2)

dtc = filter(dc, nhouse2 == 2)
nrow(dtc)

women = which(dtc$isex == 'FEMALE')
men = which(dtc$isex == 'MALE')

dtcseq = select(dtc, contains('act1') )
head(dtcseq)
sequence = seqdef(dtcseq, cpal = rainbow(184))
quartz()
seqdplot(sequence[women, ], border = NA)
seqdplot(sequence[men, ], border = NA)
seqlegend(sequence, fontsize = 0.7)

attributes(sequence)$labels

sequence.2 = seqrecode(sequence,        
                       recodes = 
                         list('Work Related Activities' = c('Unspecified employment', 
                                                            'Working time in main job', 
                                                            'Coffee and other breaks in main job', 
                                                            'Unspecified activities related to employment', 
                                                            'Lunch break', 
                                                            'job seeking', 
                                                            "Other specified organisational work", "Activities related to job seeking", 
                                                            "Other specified activities related to employment", 
                                                            'Unspecified employment', 
                                                            'Working time in main job',                                   
                                                            'Coffee and other breaks in main job', 
                                                            'Lunch break', 
                                                            'Unspecified activities related to employment', 
                                                            'Other specified activity related to employment', 
                                                            'Working time in second job', 
                                                            'Coffee and other breaks in second job'), 
                              
                              'Cooking/washing up' = c('Unspecified food management', 
                                                       'Food preparation', 
                                                       'Baking', 
                                                       'Dish washing', 
                                                       'Preserving', 
                                                       'Other specified food management'), 
                              
                              'Housework' = c('Unspecified household and family care', 
                                              'Unspecified household upkeep', 
                                              'Cleaning dwelling', 
                                              "Cooking/washing up", 
                                              'Cleaning yard',
                                              'Various arrangements',
                                              'Disposal of waste',
                                              'Other specified household upkeep',
                                              'Unspecified making and care for textiles',
                                              'Laundry',
                                              'Ironing', 
                                              "Disposal of Waste", 
                                              'Other specified making and care for textiles',
                                              'Household management not using the internet',
                                              'Household management using the internet',
                                              'bank or pay bills over internet',
                                              'other specified household management over internet'), 
                              
                              'Other domestic work' = c('Heating and water', 
                                                        'Gardening',  
                                                        "Other specified making, repairing and maintaining equipment", 
                                                        'Tending domestic animals', 
                                                        'Caring for pets', 
                                                        'Unspecified construction and repairs', 
                                                        'House construction and renovation', 
                                                        'Repairs of dwelling', 
                                                        'Unspecified make, repair, maintaining equipment', 
                                                        'Other specified make, repair, maintain equipment', 
                                                        'Vehicle maintenance', 
                                                        'Other specified construction and repairs', 
                                                        'Food management as help', 
                                                        'Household upkeep as help',  
                                                        'Gardening and pet care as help', 
                                                        'Construction and repairs as help', 
                                                        'Shopping and services as help',
                                                        'Help in employment and farming',
                                                        'Unspecified childcare as help',
                                                        'Physical child care as help',
                                                        'Teaching a child as help',
                                                        'Read/talk to child as help', 
                                                        'Unspecified gardening and pet care', 
                                                        'Gardening', 
                                                        'Other specified gardening and pet care', 
                                                        'Picking berries, mushrooms and herbs', 
                                                        'Other specified productive exercise'), 
                              
                              'Shopping' = c('Unspecified shopping and services', 
                                             'Unspecified shopping',
                                             'Shopping mainly for food',
                                             'Shopping mainly for clothing',
                                             'Look for accommodation',
                                             'Browse car boot sales',
                                             'Window shopping, leisure shopping',
                                             'Other specified shopping',
                                             'Commercial and administrative services',
                                             'Other specified shopping and services',
                                             'order unspecified goods over internet',
                                             'order food over internet',
                                             'order clothes over internet',
                                             'order other goods over internet',
                                             'order mass media over internet',
                                             'order entertainment over internet', 
                                             "Shopping mainly related to accommodation", 
                                             "Shopping or browsing at car boot sales or antique fairs"), 
                              
                              'Care' = c("Physical care & supervision of an adult household member",  
                                         "Unspecified help to an adult member of another household", 
                                         "Physical care and supervision of an adult as help", 
                                         "Other specified help to an adult household member", 
                                         "Other specified help to an adult member of another household", 
                                         'Unspecified help to adult in household',
                                         'Physical care of adult in household',
                                         'Accompany adult in household',
                                         'Other specified help to an adult',
                                         'Unspecified informal help',  
                                         'Unspecified help to adult of another household',
                                         'Physical care of adult as help',
                                         'Accompany adult as help',
                                         'Other specified adult help' ,
                                         'Other specified informal help'), 
                              
                              'Child care' = c('Unspecified childcare',  
                                               "Unspecified physical care & supervision of a child", 
                                               "Other specified physical care & supervision of a child", 
                                               "Physical care and supervision of a child as help", 
                                               'Unspecified physical care and supervision', 
                                               'Feeding the child', 
                                               'Other specified physical care', 
                                               'Teaching the child', 
                                               'Reading, playing and talking with child', 
                                               'Accompanying child', 
                                               'Other specified childcare', 
                                               'Accompany child as help',
                                               'Other specified childcare as help', 
                                               "Reading, playing & talking to the child as help"), 
                              
                              'Travel' = c('Travel related to household care', # "Travel to/from work or care"   
                                           'Travel related to shopping', 
                                           'Travel related to services', 
                                           "Travel escorting to/ from education", 
                                           'Travel escorting a child (other than education)',
                                           'Travel escorting an adult (other than education)', # "Travel to/from work or care"   
                                           
                                           'Travel in the course of work', # "Travel to/from work or education"   
                                           'Travel to work from home and back only', 
                                           'Travel to work from a place other than home', 
                                           'Travel related to education', 
                                           'Travel escorting to/from education', # "Travel to/from work or education"  
                                           
                                           # Leisure travel
                                           "Travel related to hunting & fishing", 
                                           "Travel related to organisational work", 
                                           "Travel rlt to participatory actv except rel actv" , 
                                           "Travel to visit friends/ relatives in their homes", 
                                           'Travel related to unspecified time use', 
                                           'Travel related to personal business',
                                           'Travel related to organizational work',
                                           'Travel related to informal help to other households',
                                           'Travel related to religious activities',
                                           'Travel related to participatory activities other than religious activities',
                                           'Travel to visit friends/relatives in their homes (not respondent???s household)',
                                           'Travel related to other social activities',
                                           'Travel related to entertainment and culture',
                                           'Travel related to physical exercise',
                                           'Travel related to hunting and fishing',
                                           'Travel related to productive exercise other than hunting and fishing',
                                           'Travel related to gambling',
                                           'Travel related to hobbies other than gambling',
                                           'Travel to holiday base',
                                           'Travel for day trip/just walk',
                                           'No activity but recorded mode of transport',
                                           'Other specified travel'), 
                              
                              'Dressing/toilet' = c('Unspecified personal care',
                                                    'Unspecified other personal care', 
                                                    'Personal services',
                                                    'short gap prior to travel from home or returning home from travel',
                                                    'Wash and dress',
                                                    'Other specified personal care'), 
                              
                              'Sleep' = c('Unspecified sleep',  
                                          'Sleep',
                                          'Imputed sleep',
                                          'Sick in bed'), 
                              
                              'Sport' = c('Unspecified sports and outdoor activities', 
                                          'Unspecified physical exercise',
                                          'Jogging and running',
                                          'Biking, skiing and skating (combined or unclear)',
                                          'Biking',
                                          'skiing or skating',
                                          'Unspecified ball games',
                                          'Indoor pairs or doubles games',
                                          'Indoor team games',
                                          'Outdoor pairs or doubles games',
                                          'Outdoor team games',
                                          'Other specified ball games',
                                          'Gymnastics',
                                          'Fitness',
                                          'Unspecified water sports',
                                          'Swimming',
                                          'Other specified water sports',
                                          'Other specified physical exercise',
                                          'Unspecified productive exercise',
                                          'Hunting and fishing',
                                          'Unspecified sports/prod exercise related activities',
                                          'Activities related to sports',
                                          'Activities related to productive exercise'), 
                              
                              'At church' = c('Religious activities'), 
                              
                              'Civic organizations' = c('Unspecified volunteer work and meetings', 
                                                        "Volunteer work through an organisation", 
                                                        'Unspecified organisational work',
                                                        'Work for an organisation',
                                                        'Volunteer work through an organization',
                                                        'Other specified organizational work',
                                                        'Unspecified participatory activities',
                                                        'Meetings',
                                                        'Other specified participatory activities'), 
                              
                              "Eating" = c('eating', "Eating"), 
                              
                              'TV/Radio' = c(
                                "Listening to sport on the radio",  # Radio 
                                "Listening to recordings ", 
                                "Unspecified radio listening", 
                                'Unspecified listening to radio',
                                'Listen to music on the radio',
                                'Listen to sport on the radio', 
                                'Other specified radio listening', # Radio 
                                "Unspecified video watching", 
                                "Unspecified video watching", 
                                "Unspecified TV watching", 
                                'Unspecified mass media',  
                                'Unspecified watching TV', 
                                'Watch film on TV',
                                'Watch sport on TV',
                                'Other specified TV watching',
                                'Unspecified watching video',
                                'Watch film on video',
                                'Watch sport on video',
                                "Watching a film on TV",                                        
                                "Watching a film on video", 
                                "Watching sport on TV ",                                    
                                "Watching sport on video", 
                                'Other specified video watching'), 
                              
                              'Study/Library' = c("Other specified activities related to school or university ", 
                                                  'Homework', "Unspecified library", 
                                                  'Unspecified computing for information', 
                                                  'Unspecified use of library',
                                                  'Return cds, audio items to library',
                                                  'Return books to library',
                                                  "Other specified library activities", 
                                                  'Use internet at library',
                                                  'Other library computer use',
                                                  'Read newspapers in library',
                                                  "Free Time Study", 
                                                  'Listen to music in library', 
                                                  "Brwing bks rcds audio video,CDs,VDs from library", 
                                                  'Other specified library activity', 
                                                  'Unspecified study', 
                                                  'Unspecified activities related to school or university', 
                                                  'Classes and lectures', 
                                                  'Other specified activities related to school or university'),  
                              
                              'Reading' = c('Unspecified reading', 
                                            'Reading books', 
                                            'Reading periodicals', 
                                            'Other specified reading'),  
                              
                              'Conversation w household members' = c('Socialising with household members'),  
                              
                              'Telephone/Internet/Corres conversation' = c('Telephone conversation', "Unspecified communication by computer" , 
                                                                           'Communication on the internet', 
                                                                           'Correspondence', 
                                                                           "Other specified communication by computing"), 
                              
                              'Visiting-Receiving /Social life' = c( "Visiting and receiving visitors", 
                                                                     'Visiting and receiving visitors and location not 5 (not at other people???s homes)', 
                                                                     'Unspecified social life and entertainment', 
                                                                     'No recorded activity but recorded location away from home', 
                                                                     'Unspecified social life', 
                                                                     'Feasts',
                                                                     'Other specified social life'), 
                              
                              'Computing-programming' = c( 'Computing-programming', "Shping for&ordring food via the internet", 
                                                           'Internet search', "Information searching on the internet", 
                                                           'Other information by computer',
                                                           'Unspecified other computing',
                                                           'Unspecified internet use',
                                                           'Other specified computing', 
                                                           "Computing \226 programming", 
                                                           "Other specified information by computing"), 
                              
                              'Cinema' = c('Cinema'), 
                              
                              'Leisure HighBrow' = c(
                                'Unspecified entertainment and culture', 
                                "Unspecified theatre or concerts", 
                                'Unspecified theatre and concerts',
                                'Plays, musicals or pantomime',
                                'Opera',
                                'Concerts or classical music',
                                'Other live music',
                                'Dance performance',
                                'Other specified theatre or concert', 
                                'Art exhibitions and museums', 
                                'Visiting a historical site',
                                'Visiting a wildlife site',
                                'Visiting a botanical site',
                                'Other specified entertainment and culture',
                                'Other specified entertainment or culture', 
                                'sculpture', 
                                "Making videos, taking photos or related activities", 
                                'pottery',
                                'Unspecified visual arts', 
                                'Painting, drawing, graphic arts',
                                'Make video, take photo',
                                'Other specified visual arts',
                                'Unspecified performing arts',
                                'Singing or musical activity',
                                'Other specified performing arts',
                                'Literary arts', 
                                "Chess and bridge", 
                                "Painting, drawing or other graphic arts", 
                                "Singing or other musical activities", 
                                'Collecting'), 
                              
                              'Leisure' = c(
                                'Resting-time out', "Resting \226 Time out", 
                                'Walking the dog', 
                                'Sports events',  
                                'Walking and hiking', 
                                'Walk or hike at least 2 miles or 1 hour', 'other walk or hike', 
                                "Travel for day trip/ just walk", 
                                "Billiards, pool, snooker or petanque", 
                                "Listening to music", 
                                "Visiting an urban park, playground or designated play area", 
                                "Skiing or skating", "Taking a walk or hike that lasts at least 2 miles or 1 hour", 
                                "Other specified parlour games and play", 
                                "Window shopping or other shopping as leisure", 
                                'Handicraft and producing textiles', 
                                'Visiting a leisure park', # 
                                "Visiting an urban park, playground or designated play area", 
                                "Other walk or hike", 
                                'Free time study', # study 
                                
                                'Gambling', # party ?! 
                                
                                'Unspecified listening to radio and music', 
                                'Listening to recordings', 
                                
                                "Woodcraft, metal craft, sculpture and pottery",  
                                'Unspecified hobbies and games', 
                                'Unspecified arts',
                                'Other specified arts',
                                'Unspecified hobbies',
                                'Unspecified communication by computing',
                                'Other specified communication by computer',
                                'Other specified hobbies',
                                'Unspecified games',
                                'Solo games and play',
                                'Unspecified games and play with others',
                                'Billiards, pool, snooker',
                                'Other specified games',
                                'Computer games',
                                'Other specified games'), 
                              
                              'Unknown activity' = c( 'Filling in the time use diary', 
                                                      'Punctuating activity', 
                                                      'No main activity, no idea what it might be',
                                                      'No main activity, some idea what it might be',
                                                      'Illegible activity',
                                                      'Unspecified time use')
                         )
)

attributes(sequence.2)$labels
sequence_rec  = as.matrix(sequence.2)

tima = c( TimeClock(seq(240, 1430, by = 10)), TimeClock(seq(0, 230, by = 10)) )
length(tima)

colnames(sequence_rec) = tima

library(RColorBrewer)
rc = c( brewer.pal(n = 7, name = 'Set2'), brewer.pal(n = 9, name = 'Set1'), brewer.pal(n = 9, name = 'Set3'))  
length(rc)

seq_n = seqdef(sequence_rec, cpal = rc)
seqdplot(seq_n, border = NA, withlegend = F)
seqlegend(seq_n, ltext = rev(attributes(seq_n)$labels), cpal = rev(rc))
seqlegend(seq_n)

seqmsplot(seq_n, cex.legend = 0.6)
seqmtplot(seq_n, cex.legend = 0.6, withlegend = F)

layout(x)
x = rbind(c(1,1,1,2,2), 
          c(1,1,1,2,2))

seq_where = select(dataC, householdid.x, idno, contains('wher'))
seq_where_r = ifelse(seq_where == " Home ", yes = 'Home', no = ifelse(seq_where == " Other people's home ", "Other people's home", no = 'Other Location'))
seq_where_r[is.na(seq_where_r)] <- 'NA'

seq_where_s = seqdef(seq_where_r[, -c(1,2)])
seqdplot(seq_where_s, border = NA, withlegend = F)
seqlegend(seq_where_s)

attributes(seq_where_s)$labels

# alone 
wit0 
# with children up to 9 
wit1_003
# With children aged 10 to 14 living in your household between
wit2_003
# With other household members 
wit3_003
# With other persons that you know 
wit4_003 

seq_alone = select(dataC, householdid.x, idno, contains('wit0'))
seq_alone_s = seqdef(seq_alone[, -c(1,2)], cpal = c('orange', 'white'))
seqdplot(seq_alone_s, border = NA)

seq_child9 = select(dataC, householdid.x, idno, contains('wit1'))
seq_child9_s = seqdef(seq_child9[, -c(1,2)], cpal = c('orange', 'white'))
seqdplot(seq_child9_s, border = NA)

seq_child14 = select(dataC, householdid.x, idno, contains('wit2'))
seq_child14_s = seqdef(seq_child14[, -c(1,2)], cpal = c('orange', 'white'))
seqdplot(seq_child14_s, border = NA)

seq_houseMemb = select(dataC, householdid.x, idno, contains('wit3'))
seq_houseMemb_s = seqdef(seq_houseMemb[, -c(1,2)], cpal = c('white', 'orange'))
seqdplot(seq_houseMemb_s, border = NA)

seq_acquaintance = select(dataC, householdid.x, idno, contains('wit4'))
seq_acquaintance_s = seqdef(seq_acquaintance[, -c(1,2)], cpal = c('white', 'orange'))
seqdplot(seq_acquaintance_s, border = NA)

n = 988
alone = select(dataC[1:n, ], contains('wit0')) 
alone = ifelse(alone == "Alone or with people you don't know", 1, 0)

child1 = select(dataC[1:n, ], contains('wit1')) 
child1 = ifelse(child1 == "With children up to 9 living in your household", 1, 0)

child2 = select(dataC[1:n, ], contains('wit2')) 
child2 = ifelse(child2 == "With children aged 10 to 14 living in your household", 1, 0)

child = ifelse(child1 == 1 | child2 == 1, 1, 0)

partner = select(dataC[1:n, ], contains('wit3')) 
partner = ifelse(partner == "With other household members", 1, 0)

acquaintance = select(dataC[1:n, ], contains('wit4')) 
acquaintance = ifelse(acquaintance == "With other people that you know", 1, 0)


family <- matrix(0, ncol = ncol(partner), nrow = nrow(partner))
nc = 144
nr = 10

for(j in 1:ncol(partner)){
  for(i in 1:nrow(partner)){
    
    if(acquaintance[i,j] == 1 & child[i,j] != 1 & partner[i,j] != 1) 
    {family[i,j] <- 'acquaintance'}   
    
    if(acquaintance[i,j] == 1 & child[i,j] != 1 & partner[i,j] == 1) 
    {family[i,j] <- 'partner & acquaintance'}   
    
    if(acquaintance[i,j] == 1 & child[i,j] == 1 & partner[i,j] != 1) 
    {family[i,j] <- 'child and acquaintance'}   
    
    if(acquaintance[i,j] == 1 & child[i,j] == 1 | partner[i,j] == 1) 
    {family[i,j] <- 'partner|child and acquaintance'}   
    
    if(partner[i,j] == 1 & child[i,j] == 1 & acquaintance[i,j] != 1)
    {family[i,j] <- 'nuclear'}   
    
    if(alone[i,j] == 1) 
    {family[i,j] <- 'alone'}  
    
    if(partner[i,j] == 1 & child[i,j] != 1 & acquaintance[i,j] != 1) 
    {family[i,j] <- 'partner'}   
    
    if(child[i,j] == 1 & partner[i,j] != 1 & acquaintance[i,j] != 1) 
    {family[i,j] <- 'child'}      
    
  }
}

family

layout(x)

fam_s = seqdef(family)
seqdplot(fam_s, withlegend = F, border = NA)
seqlegend(fam_s)

distseq = seqdist(seqdata = fam_s, method = 'OM', sm = 'CONSTANT')

wardCluster1 <- hclust(as.dist(distseq), method="ward.D")
wardCluster10 = cutree(wardCluster1, k = 8)

attributes(fam_s)$labels
seqdplot(fam_s, withlegend = F, border = NA, group = wardCluster10)




distseq_Act = seqdist(seqdata = seq_n, method = 'OM', sm = 'CONSTANT')

wardCluster1 <- hclust(as.dist(distseq_Act), method="ward.D")
wardCluster10 = cutree(wardCluster1, k = 8)
seqdplot(seq_n, withlegend = F, border = NA, group = wardCluster10)

library('WeightedCluster')
wardRange = as.clustrange(wardCluster1, diss = distseq_Act, ncluster = 10)
wardRange
summary(wardRange)

par(mfrow = c(1,1))
plot(wardRange, stat = c("ASWw", "HG", "PBC", "HC"))


attributes(seq_n)$labels
attributes(fam_s)$labels
attributes(seq_where_s)$labels

expand.grid(
  attributes(seq_n)$labels, 
  attributes(fam_s)$labels, 
  attributes(seq_where_s)$labels
)

25 * 7 * 4 

