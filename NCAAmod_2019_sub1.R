## Prep data from 2018
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, data.table, magrittr, ggplot2, gridExtra, ggExtra, stringr)
theme_set(theme_bw())
library(tidyr)


#Load Data
setwd('~/Documents/Kaggle/NCAA2019/DataFiles/')
list.files()

teams <- fread('Teams.csv')
seasons <- fread('Seasons.csv')
seeds <- fread('NCAATourneySeeds.csv')
seas_results <- fread('RegularSeasonCompactResults.csv')
tour_results <- fread('NCAATourneyCompactResults.csv')
seas_detail <- fread('RegularSeasonDetailedResults.csv')
tour_detail <- fread('NCAATourneyDetailedResults.csv')
conferences <- fread('Conferences.csv')
team_conferences <- fread('TeamConferences.csv')
coaches <- fread('TeamCoaches.csv')

setkey(teams, TeamID)
setkey(seeds, TeamID)

g1 <-
  teams[seeds][, one_seed := as.numeric(substr(Seed, 2, 3)) == 1][, sum(one_seed), by = TeamName][order(V1, decreasing = T)][1:15,] %>%
  ggplot(aes(x = reorder(TeamName, V1), y = V1)) +
  geom_bar(stat = 'identity', fill = 'darkblue') +
  labs(x = '', y = 'No 1 seeds', title = 'No. 1 Seeds since 1985') +
  coord_flip()
g1

setkey(seas_results, WTeamID)

win_stats <- seas_detail[, .(
  Season,
  TeamID = WTeamID,
  Outcome = rep('W', .N),
  score=WScore,
  opp_score=LScore,
  FGM = WFGM,
  FGA = WFGA,
  FGP = WFGM / WFGA,
  FGP2 = (WFGM - WFGM3) / (WFGA - WFGA3),
  FGM3 = WFGM3,
  FGA3 = WFGA3,
  FGP3 = WFGM3 / WFGA3,
  FTM = WFTM,
  FTA = WFTA,
  FTP = WFTM / WFTA,
  OR = WOR,
  DR = WDR,
  AST = WAst,
  TO = WTO,
  STL = WStl,
  BLK = WBlk,
  PF = WPF,
  ORP = WOR / (WOR + LDR),
  DRP = WDR / (WDR + LOR),
  POS = 0.96 * (WFGA + WTO + 0.44 * WFTA - WOR)
)]

los_stats <- seas_detail[, .(
  Season,
  TeamID = LTeamID,
  Outcome = rep('L', .N),
  score=LScore,
  opp_score=WScore,
  FGM = LFGM,
  FGA = LFGA,
  FGP = LFGM / LFGA,
  FGP2 = (LFGM - LFGM3) / (LFGA - LFGA3),
  FGM3 = LFGM3,
  FGA3 = LFGA3,
  FGP3 = LFGM3 / LFGA3,
  FTM = LFTM,
  FTA = LFTA,
  FTP = LFTM / LFTA,
  OR = LOR,
  DR = LDR,
  AST = LAst,
  TO = LTO,
  STL = LStl,
  BLK = LBlk,
  PF = LPF,
  ORP = (LOR / (LOR + WDR)),
  DRP = LDR / (LDR + WOR),
  POS = 0.96 * (LFGA + LTO + 0.44 * LFTA - LOR)
)]

stats_all <- rbindlist(list(win_stats, los_stats))

stats_season <- stats_all[, .(
  wins=sum(Outcome=='W'),
  loses=sum(Outcome=='L'),
  score=mean(score),
  opp_score=mean(opp_score),
  FGP = sum(FGM) / sum(FGA),
  FGP3 = sum(FGM3) / sum(FGA3),
  FTP = sum(FTM) / sum(FTA),
  ORPG = mean(OR),
  DRPG = mean(DR),
  ASPG = mean(AST),
  TOPG = mean(TO),
  STPG = mean(STL),
  BLPG = mean(BLK),
  PFPG = mean(PF),
  MORP = mean(ORP),
  MPOS = mean(POS),
  TPpcnt = sum(FGM3)/(sum(FGM)+sum(FGM3)))
  , by = c('TeamID', 'Season')]

glimpse(stats_all)
glimpse(stats_season)
glimpse(seeds)

teamyr <- merge(teams,seeds,by="TeamID")
teamyr

teamyr

team_conferences
teamyr <- merge(teamyr,team_conferences,by=c("TeamID","Season"))
teamyr[,seed:=str_extract(Seed,"[:digit:]+")]

coaches[,.N,LastDayNum]
teamyr <- merge(teamyr,coaches[LastDayNum==154,],by=c("TeamID","Season"))
teamyr



kenpom
kenpom[,.N,.(team,TeamName)]

teamyrk <- merge(teamyr,kenpom,by.x=c("TeamName","Season"),by.y=c("TeamName","year"),all.x = T)
teamyrk[Season>=2002 & is.na(adj_em),.(TeamName,Season)]

#Merge Team-year, Kenpom & Season Stats together
s1 <- merge(teamyrk,stats_season,by=c("TeamID","Season"))
s1[,.N,.(seed)][order(seed)]
s1[,.N,.(Season)][order(Season)]

tour_results <- fread('NCAATourneyCompactResults.csv')

#Transform tournament results to attach stats to winning and losing teams
t1 <- tour_results
t1[,id_diff:=WTeamID - LTeamID]

#Team 1 will always be the team with the lower id
t1[,Team1:=ifelse(id_diff < 0,WTeamID,LTeamID)]
t1[,Team2:=ifelse(id_diff > 0,WTeamID,LTeamID)]

#Recodes variables so that result is dummy for team 1 wins
t1[,result:=ifelse(Team1==WTeamID,1,0)]
head(t1)

#Attach team 1 stats by pasting _1 and merging
s1_1 <- s1
names(s1_1)[1:50] <- paste0(names(s1)[1:50],"_1")
head(s1_1)
t2 <- merge(t1,s1_1,by.x=c('Team1','Season'),by.y=c('TeamID_1','Season_1'),all.x=T)

#Attach team 2 stats by pasting _2 and merging
glimpse(s1)
s1_2 <- s1
names(s1_2)[1:50] <- paste0(names(s1)[1:50],"_2")
t3 <- merge(t2,s1_2,by.x=c('Team2','Season'),by.y=c('TeamID_2','Season_2'),all.x=T)

#subset to complete games and 
t_comp <- t3[!is.na(FGP_1),]
t_comp[,.N,Season][order(Season)]

names(s1_1)
glimpse(t_comp)

names(s1_1)
glimpse(t_comp)

#
t_comp[,FGP_diff:=FGP_1-FGP_2]
t_comp[,FGP3_diff:=FGP3_1-FGP3_2]
t_comp[,FTP_diff:=FTP_1-FTP_2]
t_comp[,ORPG_diff:=ORPG_1-ORPG_2]
t_comp[,DRPG_diff:=DRPG_1-DRPG_2]
t_comp[,ASPG_diff:=ASPG_1-ASPG_2]
t_comp[,TOPG_diff:=TOPG_1-TOPG_2]
t_comp[,STPG_diff:=STPG_1-STPG_2]
t_comp[,BLPG_diff:=BLPG_1-BLPG_2]
t_comp[,PFPG_diff:=PFPG_1-PFPG_2]
t_comp[,MORP_diff:=MORP_1-MORP_2]
t_comp[,MPOS_diff:=MPOS_1-MPOS_2]
t_comp[,seed_diff:=as.numeric(seed_1)-as.numeric(seed_2)]

t_comp[,.(.N,mean(result)),,by=seed_diff][order(seed_diff)]

train <- t_comp[Season<2015,]
test <- t_comp[Season>=2015,]

lm1 <- lm(result ~ seed_diff,data=test)
summary(lm1)

pred_lm1 <- predict(lm1,test)

MultiLogLoss <- function(act, pred){
  eps <- 1e-15
  pred <- pmin(pmax(pred, eps), 1 - eps)
  sum(act * log(pred) + (1 - act) * log(1 - pred)) * -1/NROW(act)
}

MultiLogLoss(test$result,pred_lm1)

install.packages("xgboost")
install.packages("caret")
library(xgboost)
library(caret)
head(t_comp)

fwrite(t_comp,"tourney_features.csv")
names(t_comp)[grepl('diff',names(t_comp))]

conf_n <- t_comp[,.N,.(ConfAbbrev_1,ConfAbbrev_2)][order(-N)]

pred <- c("Season","Team1","Team2","result",'seed_1',
          'rank_1',
          'conference_1',
          'adj_em_1',
          'adj_o_1',
          'adj_o_rank_1',
          'adj_d_1',
          'adj_d_rank_1',
          'adj_tempo_1',
          'adj_tempo_rank_1',
          'luck_1',
          'luck_rank_1',
          'sos_adj_em_1',
          'sos_adj_em_rank_1',
          'sos_adj_o_1',
          'sos_adj_o_rank_1',
          'sos_adj_d_1',
          'sos_adj_d_rank_1',
          'nc_sos_adj_em_1',
          'nc_sos_adj_em_rank_1',
          'wins_1',
          'loses_1',
          'score_1',
          'opp_score_1',
          'FGP_1',
          'FGP3_1',
          'FTP_1',
          'ORPG_1',
          'DRPG_1',
          'ASPG_1',
          'TOPG_1',
          'STPG_1',
          'BLPG_1',
          'PFPG_1',
          'MORP_1',
          'MPOS_1',
          'TPpcnt_1',
          'seed_2',
          'rank_2',
          'conference_2',
          'adj_em_2',
          'adj_o_2',
          'adj_o_rank_2',
          'adj_d_2',
          'adj_d_rank_2',
          'adj_tempo_2',
          'adj_tempo_rank_2',
          'luck_2',
          'luck_rank_2',
          'sos_adj_em_2',
          'sos_adj_em_rank_2',
          'sos_adj_o_2',
          'sos_adj_o_rank_2',
          'sos_adj_d_2',
          'sos_adj_d_rank_2',
          'nc_sos_adj_em_2',
          'nc_sos_adj_em_rank_2',
          'wins_2',
          'loses_2',
          'score_2',
          'opp_score_2',
          'FGP_2',
          'FGP3_2',
          'FTP_2',
          'ORPG_2',
          'DRPG_2',
          'ASPG_2',
          'TOPG_2',
          'STPG_2',
          'BLPG_2',
          'PFPG_2',
          'MORP_2',
          'MPOS_2',
          'TPpcnt_2',
          'FGP_diff',
          'FGP3_diff',
          'FTP_diff',
          'ORPG_diff',
          'DRPG_diff',
          'ASPG_diff',
          'TOPG_diff',
          'STPG_diff',
          'BLPG_diff',
          'PFPG_diff',
          'MORP_diff',
          'MPOS_diff',
          'seed_diff')
t_pred <- t_comp[,pred,with=F]
save(t_pred,file='t_pred.Rdata')
write.csv(t_pred,'t_pred.csv')

sapply(t_pred,class)
lapply(t_pred[,sapply(t_pred,is.character)],class)

t_pred[,seed_1:=as.numeric(seed_1)]
t_pred[,seed_2:=as.numeric(seed_2)]
t_pred[,seed_1:=as.numeric(rank_1)]
t_pred[,seed_2:=as.numeric(rank_2)]

t_pred[!complete.cases(t_pred),]

train <- t_pred[t_pred$Season<2015,]
test <- t_pred[t_pred$Season>=2015,]

train_mat <- model.matrix(result~.+0,data=train[,4:91])
test_mat <- model.matrix(result~.+0,data=test[,4:91])
dim(train_mat)
dim(test_mat)

dtrain <- xgb.DMatrix(data=train_mat,label=train$result)
dtest <- xgb.DMatrix(data=test_mat,label=test$result)

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 79, watchlist = list(val=dtest,train=dtrain), print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")

xgpred1 <- predict(xgb1,dtest)
confusionMatrix(xgpred1,test$result)

######### Create Submission ###########

list.files()
sub_samp1 <- fread("SampleSubmissionStage1.csv") %>%
  select(ID) %>% 
  separate(ID, sep = "_", into = c("Season", "Team1", "Team2"), convert = TRUE)

test2 <- merge(sub_samp1,s1_1,by.x=c("Season", "Team1"),by.y=c("Season_1","TeamID_1"),all.x = T)
test2 <- merge(test2,s1_2,by.x=c("Season", "Team2"),by.y=c("Season_2","TeamID_2"),all.x = T)

test2[,FGP_diff:=FGP_1-FGP_2]
test2[,FGP3_diff:=FGP3_1-FGP3_2]
test2[,FTP_diff:=FTP_1-FTP_2]
test2[,ORPG_diff:=ORPG_1-ORPG_2]
test2[,DRPG_diff:=DRPG_1-DRPG_2]
test2[,ASPG_diff:=ASPG_1-ASPG_2]
test2[,TOPG_diff:=TOPG_1-TOPG_2]
test2[,STPG_diff:=STPG_1-STPG_2]
test2[,BLPG_diff:=BLPG_1-BLPG_2]
test2[,PFPG_diff:=PFPG_1-PFPG_2]
test2[,MORP_diff:=MORP_1-MORP_2]
test2[,MPOS_diff:=MPOS_1-MPOS_2]
test2[,seed_diff:=as.numeric(seed_1)-as.numeric(seed_2)]

pred2 <- c("Season","Team1","Team2",'seed_1',
          'rank_1',
          'conference_1',
          'adj_em_1',
          'adj_o_1',
          'adj_o_rank_1',
          'adj_d_1',
          'adj_d_rank_1',
          'adj_tempo_1',
          'adj_tempo_rank_1',
          'luck_1',
          'luck_rank_1',
          'sos_adj_em_1',
          'sos_adj_em_rank_1',
          'sos_adj_o_1',
          'sos_adj_o_rank_1',
          'sos_adj_d_1',
          'sos_adj_d_rank_1',
          'nc_sos_adj_em_1',
          'nc_sos_adj_em_rank_1',
          'wins_1',
          'loses_1',
          'score_1',
          'opp_score_1',
          'FGP_1',
          'FGP3_1',
          'FTP_1',
          'ORPG_1',
          'DRPG_1',
          'ASPG_1',
          'TOPG_1',
          'STPG_1',
          'BLPG_1',
          'PFPG_1',
          'MORP_1',
          'MPOS_1',
          'TPpcnt_1',
          'seed_2',
          'rank_2',
          'conference_2',
          'adj_em_2',
          'adj_o_2',
          'adj_o_rank_2',
          'adj_d_2',
          'adj_d_rank_2',
          'adj_tempo_2',
          'adj_tempo_rank_2',
          'luck_2',
          'luck_rank_2',
          'sos_adj_em_2',
          'sos_adj_em_rank_2',
          'sos_adj_o_2',
          'sos_adj_o_rank_2',
          'sos_adj_d_2',
          'sos_adj_d_rank_2',
          'nc_sos_adj_em_2',
          'nc_sos_adj_em_rank_2',
          'wins_2',
          'loses_2',
          'score_2',
          'opp_score_2',
          'FGP_2',
          'FGP3_2',
          'FTP_2',
          'ORPG_2',
          'DRPG_2',
          'ASPG_2',
          'TOPG_2',
          'STPG_2',
          'BLPG_2',
          'PFPG_2',
          'MORP_2',
          'MPOS_2',
          'TPpcnt_2',
          'FGP_diff',
          'FGP3_diff',
          'FTP_diff',
          'ORPG_diff',
          'DRPG_diff',
          'ASPG_diff',
          'TOPG_diff',
          'STPG_diff',
          'BLPG_diff',
          'PFPG_diff',
          'MORP_diff',
          'MPOS_diff',
          'seed_diff')

t2_pred <- test2[,pred2,with=F]
t2_pred[,result:=1]

t2_pred[,seed_1:=as.numeric(seed_1)]
t2_pred[,seed_2:=as.numeric(seed_2)]
t2_pred[,seed_1:=as.numeric(rank_1)]
t2_pred[,seed_2:=as.numeric(rank_2)]

t2_pred[!complete.cases(t2_pred),]

test2_mat <- model.matrix(result~.+0,data=t2_pred[,4:91])

test2_dmat <- xgb.DMatrix(data=test2_mat)

t2_pred$Pred <- predict(xgb1,test2_dmat)

sub1 <- merge(sub_samp1,t2_pred[,.(Season,Team1,Team2,Pred)],by=c("Season", "Team1", "Team2"))



fwrite(sub1,"NCAA2019_submission1.csv")


sub2 <- t2_pred[,.(ID=paste(Season, Team1, Team2,sep="_"),Pred)]

fwrite(sub2,"NCAA2019_submission2.csv")

