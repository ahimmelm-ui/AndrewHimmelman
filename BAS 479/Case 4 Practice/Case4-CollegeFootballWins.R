
##################################################################################
#Option  (Wins of College Football Teams)
##################################################################################

#See https://www.kaggle.com/datasets/jeffgallini/college-football-team-stats-2019
#I've combined files from 2013-2023, and with the help of ChatGPT reduced it to a
#set of columns which hopefully hold strategic importance when examimining main 
#drivers of wins

FOOTBALL <- read.csv("Case4-CollegeFootballWins.csv")

#For reference, this was the list of columns from the original data chosen by Chat
#as being potentially useful to an analysis
strategic_cols <- c(
  "Team",
  "Year",
  "Win",
  
  # Offensive Efficiency
  "Off.Yards.Play",
  "Pass.Yards.Attempt",
  "Yards.Completion",
  "Pass.Yards.Per.Game",
  "Yards.Rush",
  "Rushing.Yards.per.Game",
  "X3rd.Percent",
  "X4th.Percent",
  
  # Defensive Efficiency
  "Yards.Play.Allowed",
  "Yards.Attempt.Allowed",
  "Yds.Rush.Allowed",
  "Opponent.3rd.Percent",
  "Opponent.4th.Percent",
  
  # Turnovers
  "Turnover.Margin",
  "Avg.Turnover.Margin.per.Game",
  "Interceptions.Thrown.y",
  "Opponents.Intercepted",
  "Fumbles.Lost",
  "Fumbles.Recovered",
  
  # Discipline / Disruption
  "Penalty.Yards.Per.Game",
  "Penalties",
  "Tackle.For.Loss.Per.Game",
  "Sacks",
  "Average.Sacks.per.Game",
  
  # Aggressiveness / Philosophy
  "Pass.Attempts",
  "Rush.Attempts",
  "X3rd.Attempts",
  "X4th.Attempts"
)


#ChatGPT also suggests constructing additional metrics

#Pass–Rush Balance
FOOTBALL$Pass_Rate  <- as.numeric(FOOTBALL$Pass.Attempts) /
  (as.numeric(FOOTBALL$Pass.Attempts) + as.numeric(FOOTBALL$Rush.Attempts))

FOOTBALL$Rush_Rate  <- 1 - FOOTBALL$Pass_Rate

#Yards per Attempt Ratio (Pass vs Run)
FOOTBALL$Pass_to_Rush_Efficiency <-
  as.numeric(FOOTBALL$Pass.Yards.Attempt) /
  as.numeric(FOOTBALL$Yards.Rush)

#Offensive vs Defensive Yards Per Play Differential
FOOTBALL$Net_Yards_Per_Play <-
  as.numeric(FOOTBALL$Off.Yards.Play) -
  as.numeric(FOOTBALL$Yards.Play.Allowed)

#3rd Down Net Efficiency
FOOTBALL$Net_3rd_Down <-
  as.numeric(FOOTBALL$X3rd.Percent) -
  as.numeric(FOOTBALL$Opponent.3rd.Percent)

#4th Down aggressiveness
FOOTBALL$Fourth_Down_Aggressiveness <-
  as.numeric(FOOTBALL$X4th.Attempts) /
  (as.numeric(FOOTBALL$Pass.Attempts) + as.numeric(FOOTBALL$Rush.Attempts))

#Takeaway Ratio
FOOTBALL$Takeaway_Rate <-
  as.numeric(FOOTBALL$Opponents.Intercepted) +
  as.numeric(FOOTBALL$Fumbles.Recovered)

#Giveaway Rate
FOOTBALL$Giveaway_Rate <-
  as.numeric(FOOTBALL$Interceptions.Thrown.y) +
  as.numeric(FOOTBALL$Fumbles.Lost)

#Turnover Discipline Index
FOOTBALL$Turnover_Discipline <-
  FOOTBALL$Takeaway_Rate - FOOTBALL$Giveaway_Rate


#TFL + Sack Disruption Index
FOOTBALL$Disruption_Index <-
  as.numeric(FOOTBALL$Tackle.For.Loss.Per.Game) +
  as.numeric(FOOTBALL$Average.Sacks.per.Game)

#Penalty Severity
FOOTBALL$Penalty_Yards_Per_Penalty <-
  as.numeric(FOOTBALL$Penalty.Yards.Per.Game) /
  as.numeric(FOOTBALL$Penalties)

#Passing Efficiency Under Volume
FOOTBALL$Passing_Load_Adjusted <-
  as.numeric(FOOTBALL$Pass.Yards.Attempt) *
  as.numeric(FOOTBALL$Pass.Attempts)

#Additional thoughts from Chat
# 
# Avoid:
#   
#   Raw points
# 
# Raw touchdowns
# 
# Mechanical scoring outputs
# 
# Focus on:
#   
#   Efficiency
# 
# Situational play
# 
# Discipline
# 
# Aggression
# 
# Balance
# 
# That gives students something to actually recommend.


#Combos
TREE <- rpart(Win~.-Team-Year,data=FOOTBALL,cp=0,minbucket=15)
summarize_tree(TREE)
TREE$cptable
TREE <- rpart(Win~.-Team-Year,data=FOOTBALL,cp=0.0082777440,minbucket=15)
visualize_model(TREE)

#Obviously, net yards per play is going to make a difference.  But "do good on offense" and "stop
#defense" isn't exactly the most actionable advice.  We can remove it from the tree and try again
#by putting a dash in front of the column we want to exclude

TREE <- rpart(Win~.-Team-Year-Net_Yards_Per_Play,data=FOOTBALL,cp=0.015,minbucket=15)
visualize_model(TREE)

#Might there be other columns in the data that aren't the most useful to consider?  Use your 
#football knowledge to tweak which columns coaches/players have the most control over during
#a game or during practice


#Main drivers time
#FOCUS ON WHAT A COACH HAS CONTROL OVER DURING A GAME/PRACTICE and make recommendations/insights accordingly
cat( paste('examine_driver_Ynumeric(Win ~',names(FOOTBALL),', data=FOOTBALL)\n' ))

examine_driver_Ynumeric(Win ~ Off.Yards.Play , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Pass.Yards.Attempt , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Yards.Completion , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Pass.Yards.Per.Game , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Yards.Rush , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Rushing.Yards.per.Game , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ X3rd.Percent , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ X4th.Percent , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Yards.Play.Allowed , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Yards.Attempt.Allowed , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Yds.Rush.Allowed , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Opponent.3rd.Percent , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Opponent.4th.Percent , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Turnover.Margin , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Avg.Turnover.Margin.per.Game , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Interceptions.Thrown.y , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Opponents.Intercepted , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Fumbles.Lost , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Fumbles.Recovered , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Penalty.Yards.Per.Game , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Penalties , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Tackle.For.Loss.Per.Game , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Sacks , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Average.Sacks.per.Game , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Pass.Attempts , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Rush.Attempts , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ X3rd.Attempts , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ X4th.Attempts , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Pass_Rate , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Rush_Rate , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Pass_to_Rush_Efficiency , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Net_Yards_Per_Play , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Net_3rd_Down , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Fourth_Down_Aggressiveness , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Takeaway_Rate , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Giveaway_Rate , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Turnover_Discipline , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Disruption_Index , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Penalty_Yards_Per_Penalty , data=FOOTBALL)
examine_driver_Ynumeric(Win ~ Passing_Load_Adjusted , data=FOOTBALL)


