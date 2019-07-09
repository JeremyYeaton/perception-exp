library(dplyr)
library(ggplot2)
library(reshape2)

## IMPORT DATA AND STIM LIST ####
stim <- read.table('stimSkeleton.csv',header=T,sep='\t')%>%
  mutate(cond = ordered(cond,levels=c('NC','DN','Not All','All not')))

users <- read.table('meta.csv',header=T,sep=',')%>%
  select('birthplace','currentlocation','growingUp','sex','userCode') %>%
  merge(read.table('users.csv',header=T,sep=','),by='userCode') %>%
  filter(.,!duplicated(userCode))

data <- read.table('xp.csv',header=T,sep=',') %>%
  select('Item','response1','response2','response3','trialnumber','userCode')  %>%
  # merge(.,users,by='userCode') %>%
  mutate(response2 = as.character(response2),response1 = as.character(response1)) %>%
  mutate(response2 = replace(response2, which(response2=='Enregistrement 1'),1)) %>%
  mutate(response2 = replace(response2, which(response2=='Enregistrement 2'),2)) %>%
  mutate(response1 = replace(response1, which(response1=='VRAI'),T)) %>%
  mutate(response1 = replace(response1, which(response1=='FAUX'),F)) %>%
  mutate('rep1' = factor(response1),'rep2' = factor(response2))
  
## MERGE ####
df.all <- merge(data,stim,'Item') %>%
  mutate('interp' = rep1 == r1,
         'recording' = rep2 == r2,
         'confidence' = response3) %>%
  mutate('bothRight' = (interp == T & recording == T),
         'bothWrong' = (interp ==F & recording == F)) %>%
  mutate(interp = replace(interp, which(interp == T),1),
         interp = replace(interp, which(interp == F),0),
         recording = replace(recording, which(recording == T),1),
         recording = replace(recording, which(recording == F),0)) %>%
  na.omit() %>%
  select(Item, userCode, cond, verb, interp, recording, bothRight, bothWrong, confidence)

## SUMMARISE ####

correctInt.summ <- df.all %>%
  filter(verb != 'faire' & verb != 'dire') %>%
  filter(cond == 'NC' | cond == 'DN') %>%
  # filter(interp == 1) %>%
  group_by(cond) %>%
  summarise(sumCorrect = mean(interp),
            sem = sd(interp)/sqrt(length(df.all$interp)))

correctRec.summ <- df.all %>%
  # filter(verb != 'faire' & verb != 'dire') %>%
  filter(cond == 'NC' | cond == 'DN') %>%
  filter(interp == 1) %>%
  group_by(cond) %>%
  summarise(sumCorrect = mean(recording),
            sem = sd(recording)/sqrt(length(df.all$recording)))

