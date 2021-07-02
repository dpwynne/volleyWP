library(tidyverse)
library(datavolley)
library(classInt)
library(cluster)
library(factoextra)
library(gridExtra)

'%!in%' <- function(x,y)!('%in%'(x,y))
start.time <- Sys.time()

# add all .dvw files to working directory first - then my.files will be a list of files to parse
my.files <- list.files(pattern = "*dvw")
all_files <- as.data.frame(matrix(ncol=0,nrow=0))

#run read_dv code for all files in my.files - rbind them into "all_files"
for(q in 1:length(my.files)) {
  x <- read_dv(my.files[q], skill_evaluation_decode = "volleymetrics")
  x$plays$match_date <- x$meta$match$date
  x$plays$setscore_home1 <- x$meta$result$score_home_team[1]
  x$plays$setscore_home2 <- x$meta$result$score_home_team[2]
  x$plays$setscore_home3 <- x$meta$result$score_home_team[3]
  x$plays$setscore_home4 <- x$meta$result$score_home_team[4]
  x$plays$setscore_home5 <- x$meta$result$score_home_team[5]
  x$plays$setscore_away1 <- x$meta$result$score_visiting_team[1]
  x$plays$setscore_away2 <- x$meta$result$score_visiting_team[2]
  x$plays$setscore_away3 <- x$meta$result$score_visiting_team[3]
  x$plays$setscore_away4 <- x$meta$result$score_visiting_team[4]
  x$plays$setscore_away5 <- x$meta$result$score_visiting_team[5]
  all_files <- rbind(all_files, x$plays)
  print(q)
}

#rename total to master and remove non-plays
master <- subset(all_files, substring(code, 2, 2)!="P" & substring(code, 2, 2)!="p"
                 & substring(code, 2, 2)!="z" & substring(code, 2, 2)!="c"
                 & substring(code, 2, 2)!="*" & substring(code, 2, 2)!="$")
master <- master[,c(1,2,4,6,8,10,11,12,13,14,16,19,20,21,23,24,54,55,56,57,58,59,72,73,74,75,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93)]
all_files <- NULL

#create touch_id for all rows - helps w/ ability to reorder properly if I screw something up down the road
master$id_master <- seq.int(nrow(master))
master <- master[order(master$id_master),]

#create column for who the team making the contact is playing against
master$opponent <- NA
master$opponent <- ifelse(master$team == master$home_team, master$visiting_team, master$opponent)
master$opponent <- ifelse(master$team == master$visiting_team, master$home_team, master$opponent)

#create column for each set score differential
master$setscorediff <- NA
master$setscorediff <- ifelse(master$team == master$home_team & master$set_number == 1, master$setscore_home1-master$setscore_away1, master$setscorediff)
master$setscorediff <- ifelse(master$team == master$home_team & master$set_number == 2, master$setscore_home2-master$setscore_away2, master$setscorediff)
master$setscorediff <- ifelse(master$team == master$home_team & master$set_number == 3, master$setscore_home3-master$setscore_away3, master$setscorediff)
master$setscorediff <- ifelse(master$team == master$home_team & master$set_number == 4, master$setscore_home4-master$setscore_away4, master$setscorediff)
master$setscorediff <- ifelse(master$team == master$home_team & master$set_number == 5, master$setscore_home5-master$setscore_away5, master$setscorediff)
master$setscorediff <- ifelse(master$team == master$visiting_team & master$set_number == 1, master$setscore_away1-master$setscore_home1, master$setscorediff)
master$setscorediff <- ifelse(master$team == master$visiting_team & master$set_number == 2, master$setscore_away2-master$setscore_home2, master$setscorediff)
master$setscorediff <- ifelse(master$team == master$visiting_team & master$set_number == 3, master$setscore_away3-master$setscore_home3, master$setscorediff)
master$setscorediff <- ifelse(master$team == master$visiting_team & master$set_number == 4, master$setscore_away4-master$setscore_home4, master$setscorediff)
master$setscorediff <- ifelse(master$team == master$visiting_team & master$set_number == 5, master$setscore_away5-master$setscore_home5, master$setscorediff)

#create column for match score differential
master$matchscorediff <- NA
a <- master %>% filter(team == home_team) %>% rowwise() %>% mutate(matchscorediff = sum(setscore_home1, setscore_home2, setscore_home3, setscore_home4, setscore_home5, na.rm = TRUE) - sum(setscore_away1, setscore_away2, setscore_away3, setscore_away4, setscore_away5, na.rm = TRUE))
b <- master %>% filter(team == visiting_team) %>% rowwise() %>% mutate(matchscorediff = sum(setscore_away1, setscore_away2, setscore_away3, setscore_away4, setscore_away5, na.rm = TRUE) - sum(setscore_home1, setscore_home2, setscore_home3, setscore_home4, setscore_home5, na.rm = TRUE))
master <- rbind(a,b)
master <- master[order(master$id_master),]

#create column as a count (for the denomenator for most equations)
master$count <- 1

#fix E+ grades?
master$evaluation_code <- ifelse(master$skill=="Set" & master$evaluation_code == "+", "#", master$evaluation_code)

#create cover as one of the skills
master$add_cover <- ifelse(master$skill=="Dig" & lag(master$skill, 1)=="Block" & lag(master$team, 2)==master$team, "Cover", master$skill)

#create skill-quality combination
master$skq <- paste(master$add_cover, master$evaluation_code)

#create Forward/Backward looking contacts that help power Input/Output Contacts
master$two_touch_ago <- ifelse(master$point_id == lag(master$point_id, 2), lag(master$skq, 2), NA)
master$one_touch_ago <- ifelse(master$point_id == lag(master$point_id, 1), lag(master$skq, 1), NA)
master$one_touch_future <- ifelse(master$point_id == lead(master$point_id, 1), lead(master$skq, 1), NA)
master$two_touch_future <- ifelse(master$point_id == lead(master$point_id, 2), lead(master$skq, 2), NA)

#create possession data
master$possession <- NA
master$possession <- ifelse(master$skill=="Serve", 1, master$possession)
master$possession <- ifelse(master$skill=="Reception", 2, master$possession)
x <- 0
repeat {
  x = x+1
  master$possession <- ifelse(master$skill!="Serve" & master$skill!="Reception" & lag(master$team,1)==master$team, lag(master$possession), master$possession)
  master$possession <- ifelse(master$skill!="Serve" & master$skill!="Reception" & lag(master$team,1)!=master$team, lag(master$possession) + 1, master$possession)
  if (x == 50){
    break
  }
}

#create rally - possession combination
master$rally_possession <- paste0(master$point_id, "-", master$possession)

#create contact count w/in each possession
master$contact <- NA
master$contact <- ifelse(master$skill=="Serve" | master$skill=="Reception", 1, master$contact)
x <- 0
repeat {
  x = x+1
  master$contact <- ifelse(master$skill!="Serve" & master$skill!="Reception" & lag(master$possession,1)==master$possession, lag(master$contact) + 1, master$contact)
  master$contact <- ifelse(master$skill!="Serve" & master$skill!="Reception" & lag(master$possession,1)!=master$possession, 1, master$contact)
  if (x == 10){
    break
  }
}
master$contact <- ifelse(master$contact == 5, 1, master$contact)
master$contact <- ifelse(master$contact == 6, 2, master$contact)
master$contact <- ifelse(master$contact == 7, 3, master$contact)
master$contact <- ifelse(master$contact == 8, 4, master$contact)



#create column to denote if there is a winning or losing contact & fill in other touches within same possession if poss is won/lost
winners <- c("Serve #", "Attack #", "Block #")
losers <- c("Serve =", "Reception =", "Set =", "Attack =", "Attack /", "Block =", "Dig =", "Freeball =", "Cover =")
master$possession_winner <- ifelse(master$skq %in% winners, 1, 0)
master$possession_winner <- ifelse(lead(master$skq,1) %in% winners & master$rally_possession==lead(master$rally_possession, 1), 1, master$possession_winner)
master$possession_winner <- ifelse(lead(master$skq,2) %in% winners & master$rally_possession==lead(master$rally_possession, 2), 1, master$possession_winner)
master$possession_winner <- ifelse(lead(master$skq,3) %in% winners & master$rally_possession==lead(master$rally_possession, 3), 1, master$possession_winner)

master$possession_loser <- ifelse(master$skq %in% losers, 1, 0)
master$possession_loser <- ifelse(lead(master$skq,1) %in% losers & master$rally_possession==lead(master$rally_possession, 1), 1, master$possession_loser)
master$possession_loser <- ifelse(lead(master$skq,2) %in% losers & master$rally_possession==lead(master$rally_possession, 2), 1, master$possession_loser)
master$possession_loser <- ifelse(lead(master$skq,3) %in% losers & master$rally_possession==lead(master$rally_possession, 3), 1, master$possession_loser)

#create column for winning/losing on NEXT possession
master$poss_win_next <- 0
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,1) &
                                 master$possession==lead(master$possession,1)-1 & master$team!=lead(master$team,1) & lead(master$skq,1) %in% losers, 1, master$poss_win_next)
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,2) &
                                 master$possession==lead(master$possession,2)-1 & master$team!=lead(master$team,2) & lead(master$skq,2) %in% losers, 1, master$poss_win_next)
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,3) &
                                 master$possession==lead(master$possession,3)-1 & master$team!=lead(master$team,3) & lead(master$skq,3) %in% losers, 1, master$poss_win_next)
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,4) &
                                 master$possession==lead(master$possession,4)-1 & master$team!=lead(master$team,4) & lead(master$skq,4) %in% losers, 1, master$poss_win_next)
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,5) &
                                 master$possession==lead(master$possession,5)-1 & master$team!=lead(master$team,5) & lead(master$skq,5) %in% losers, 1, master$poss_win_next)
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,6) &
                                 master$possession==lead(master$possession,6)-1 & master$team!=lead(master$team,6) & lead(master$skq,6) %in% losers, 1, master$poss_win_next)
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,7) &
                                 master$possession==lead(master$possession,7)-1 & master$team!=lead(master$team,7) & lead(master$skq,7) %in% losers, 1, master$poss_win_next)
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,8) &
                                 master$possession==lead(master$possession,8)-1 & master$team!=lead(master$team,8) & lead(master$skq,8) %in% losers, 1, master$poss_win_next)

master$poss_lost_next <- 0
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,1) &
                                  master$possession==lead(master$possession,1)-1 & master$team!=lead(master$team,1) & lead(master$skq,1) %in% winners, 1, master$poss_lost_next)
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,2) &
                                  master$possession==lead(master$possession,2)-1 & master$team!=lead(master$team,2) & lead(master$skq,2) %in% winners, 1, master$poss_lost_next)
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,3) &
                                  master$possession==lead(master$possession,3)-1 & master$team!=lead(master$team,3) & lead(master$skq,3) %in% winners, 1, master$poss_lost_next)
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,4) &
                                  master$possession==lead(master$possession,4)-1 & master$team!=lead(master$team,4) & lead(master$skq,4) %in% winners, 1, master$poss_lost_next)
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,5) &
                                  master$possession==lead(master$possession,5)-1 & master$team!=lead(master$team,5) & lead(master$skq,5) %in% winners, 1, master$poss_lost_next)
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,6) &
                                  master$possession==lead(master$possession,6)-1 & master$team!=lead(master$team,6) & lead(master$skq,6) %in% winners, 1, master$poss_lost_next)
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,7) &
                                  master$possession==lead(master$possession,7)-1 & master$team!=lead(master$team,7) & lead(master$skq,7) %in% winners, 1, master$poss_lost_next)
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,8) &
                                  master$possession==lead(master$possession,8)-1 & master$team!=lead(master$team,8) & lead(master$skq,8) %in% winners, 1, master$poss_lost_next)

#create column for winning/losing on NEXT NEXT possession
master$poss_win_nextnext <- 0
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,1) &
                                     master$possession==lead(master$possession,1)-2 & master$team==lead(master$team,1) & lead(master$skq,1) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,2) &
                                     master$possession==lead(master$possession,2)-2 & master$team==lead(master$team,2) & lead(master$skq,2) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,3) &
                                     master$possession==lead(master$possession,3)-2 & master$team==lead(master$team,3) & lead(master$skq,3) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,4) &
                                     master$possession==lead(master$possession,4)-2 & master$team==lead(master$team,4) & lead(master$skq,4) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,5) &
                                     master$possession==lead(master$possession,5)-2 & master$team==lead(master$team,5) & lead(master$skq,5) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,6) &
                                     master$possession==lead(master$possession,6)-2 & master$team==lead(master$team,6) & lead(master$skq,6) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,7) &
                                     master$possession==lead(master$possession,7)-2 & master$team==lead(master$team,7) & lead(master$skq,7) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,8) &
                                     master$possession==lead(master$possession,8)-2 & master$team==lead(master$team,8) & lead(master$skq,8) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,9) &
                                     master$possession==lead(master$possession,9)-2 & master$team==lead(master$team,9) & lead(master$skq,9) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,10) &
                                     master$possession==lead(master$possession,10)-2 & master$team==lead(master$team,10) & lead(master$skq,10) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,11) &
                                     master$possession==lead(master$possession,11)-2 & master$team==lead(master$team,11) & lead(master$skq,11) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,12) &
                                     master$possession==lead(master$possession,12)-2 & master$team==lead(master$team,12) & lead(master$skq,12) %in% winners, 1, master$poss_win_nextnext)

master$poss_lost_nextnext <- 0
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,1) &
                                      master$possession==lead(master$possession,1)-2 & master$team==lead(master$team,1) & lead(master$skq,1) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,2) &
                                      master$possession==lead(master$possession,2)-2 & master$team==lead(master$team,2) & lead(master$skq,2) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,3) &
                                      master$possession==lead(master$possession,3)-2 & master$team==lead(master$team,3) & lead(master$skq,3) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,4) &
                                      master$possession==lead(master$possession,4)-2 & master$team==lead(master$team,4) & lead(master$skq,4) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,5) &
                                      master$possession==lead(master$possession,5)-2 & master$team==lead(master$team,5) & lead(master$skq,5) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,6) &
                                      master$possession==lead(master$possession,6)-2 & master$team==lead(master$team,6) & lead(master$skq,6) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,7) &
                                      master$possession==lead(master$possession,7)-2 & master$team==lead(master$team,7) & lead(master$skq,7) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,8) &
                                      master$possession==lead(master$possession,8)-2 & master$team==lead(master$team,8) & lead(master$skq,8) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,9) &
                                      master$possession==lead(master$possession,9)-2 & master$team==lead(master$team,9) & lead(master$skq,9) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,10) &
                                      master$possession==lead(master$possession,10)-2 & master$team==lead(master$team,10) & lead(master$skq,10) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,11) &
                                      master$possession==lead(master$possession,11)-2 & master$team==lead(master$team,11) & lead(master$skq,11) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,12) &
                                      master$possession==lead(master$possession,12)-2 & master$team==lead(master$team,12) & lead(master$skq,12) %in% losers, 1, master$poss_lost_nextnext)

#create efficiency w/in possession
master$poss_eff <- master$possession_winner-master$possession_loser
master$poss_eff_next <- master$poss_win_next-master$poss_lost_next
master$poss_eff_nextnext <- master$poss_win_nextnext-master$poss_lost_nextnext

#create rally winner & rally loser
master$rally_winner <- ifelse(master$team==master$point_won_by, 1, 0)
master$rally_loser <- ifelse(master$team!=master$point_won_by, 1, 0)

#create efficiency w/in rally
master$rally_eff <- master$rally_winner-master$rally_loser

#create serving & home team RALLY efficiency
master$serveteam_rallyeff <- ifelse(master$serving_team==master$point_won_by, 1, -1)
master$hometeam_rallyeff <- ifelse(master$home_team==master$point_won_by, 1, -1)

#create serving & home team POSSESSION efficiency
master$st_posswin <- NA
master$st_posslose <- NA
master$ht_posswin <- NA
master$ht_posslose <- NA

master$st_posswin <- ifelse(master$serving_team==master$team & master$possession_winner==1, 1, master$st_posswin)
master$ht_posswin <- ifelse(master$home_team==master$team & master$possession_winner==1, 1, master$ht_posswin)
master$st_posslose <- ifelse(master$serving_team==master$team & master$possession_loser==1, 1, master$st_posslose)
master$ht_posslose <- ifelse(master$home_team==master$team & master$possession_loser==1, 1, master$ht_posslose)

master$serveteam_posseff <- master$st_posswin-master$st_posslose
master$hometeam_posseff <- master$ht_posswin-master$ht_posslose

#is serving team touching the ball?
master$serveteam_touch <- ifelse(master$team==master$serving_team, "yes", "no")

#is home team touching the ball?
master$hometeamtouch <- ifelse(master$home_team==master$team, "yes", "no")

#create endzone+subzone for setting
master$endzone_full <- paste0(master$end_zone, master$end_subzone)

#create location where setter takes ball for R,D,Cover,Freeball codes
master$set_from <- NA
master$set_from <- ifelse(master$add_cover %in% c("Reception", "Dig", "Cover", "Freeball") & lead(master$team, 1)==master$team & substr(master$one_touch_future,1,3)=="Set", lead(master$end_zone,1), master$set_from)

#create what type of attack follows the first touch (attack code starts w/ V or not)
master$attack_type <- NA
master$attack_type <- ifelse(master$add_cover %in% c("Reception", "Dig", "Cover", "Freeball") & lead(master$team, 1)==master$team & lead(master$team,2)==master$team & lead(master$add_cover,1)=="Set",
                             substr(lead(master$attack_code,2),1,1), master$attack_type)

master$in_or_oos <- NA
master$in_or_oos <- ifelse(master$attack_type=="V", "OOS", master$in_or_oos)
master$in_or_oos <- ifelse(master$attack_type %in% c("C", "X", "P"), "In Sys", master$in_or_oos)

master$attack_type <- NULL

#create unique set_id
master$set_id <- paste0(master$match_id, "- Set ", master$set_number)

#create if team won the set overall
master <- master[order(master$id_master),]

master$who_won_set <- NA
master$who_lost_set <- NA
master$who_won_set <- ifelse(master$set_id!=lead(master$set_id,1) & master$team==master$point_won_by, master$team, master$who_won_set)
master$who_won_set <- ifelse(master$set_id!=lead(master$set_id,1) & master$team!=master$point_won_by, master$opponent, master$who_won_set)
master$who_lost_set <- ifelse(master$set_id!=lead(master$set_id,1) & master$team!=master$point_won_by, master$team, master$who_lost_set)
master$who_lost_set <- ifelse(master$set_id!=lead(master$set_id,1) & master$team==master$point_won_by, master$opponent, master$who_lost_set)

master$who_won_set <- ifelse(master$id_master==max(master$id_master) & master$team==master$point_won_by, master$team, master$who_won_set)
master$who_won_set <- ifelse(master$id_master==max(master$id_master) & master$team!=master$point_won_by, master$opponent, master$who_won_set)
master$who_lost_set <- ifelse(master$id_master==max(master$id_master) & master$team!=master$point_won_by, master$team, master$who_lost_set)
master$who_lost_set <- ifelse(master$id_master==max(master$id_master) & master$team==master$point_won_by, master$opponent, master$who_lost_set)

master <- fill(master, who_won_set, who_lost_set, .direction = c("up"))

master$teamwonset <- ifelse(master$team==master$who_won_set, 1, 0)
master$teamlostset <- ifelse(master$team==master$who_lost_set, 1, 0)

master$who_won_set <- NULL
master$who_lost_set <- NULL

master$passer_rating <- NA
master$passer_rating <- ifelse(master$skq=="Reception #", 3, master$passer_rating)
master$passer_rating <- ifelse(master$skq=="Reception +", 3, master$passer_rating)
master$passer_rating <- ifelse(master$skq=="Reception !", 2, master$passer_rating)
master$passer_rating <- ifelse(master$skq=="Reception -", 1, master$passer_rating)
master$passer_rating <- ifelse(master$skq=="Reception /", 1, master$passer_rating)
master$passer_rating <- ifelse(master$skq=="Reception =", 0, master$passer_rating)


write.csv(master, "master1.csv")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#####
#added June 15, 2020 / May 29, 2021
m1 <- read.csv("master1.csv")
m2 <- read.csv("master2.csv")
m3 <- read.csv("master3.csv")
m4 <- read.csv("master4.csv")
m5 <- read.csv("master5.csv")
master <- rbind(m1,m2,m3,m4,m5)
rm(m1,m2,m3,m4,m5)

master$gender <- ifelse(grepl("Men's", master$team), "Men", "Women")

master$kcode <- NA
master$kcode <- ifelse((master$skill %in% c("Reception", "Dig", "Freeball") & !is.na(lead(master$set_code))) |
                         (master$skill == "Set" & !is.na(master$set_code)), "yes", "no")

master$id_master <- seq.int(nrow(master))
master <- master[order(master$id_master),]




