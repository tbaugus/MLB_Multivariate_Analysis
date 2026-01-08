# Load Libraries 
#install.packages("reshape2")
#install.packages("lightgbm")
library(lightgbm)
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyverse)
library(ResourceSelection)

# set working directory and outpaths 
setwd("G:/My Drive/Data Science/University/Texas Tech/Semester 2/Multivariate/Project/")
filepath = "./batting_2023_data.csv"


# Set/ create folder
outpath = "/1_outfiles"

if (!dir.exists(paste0(getwd(),outpath))){
  dir.create(paste0(getwd(),outpath))
}else{
  print("dir exists")
}

# Read data
df = read.csv(filepath)

############################### Clean and process data #####################################

# Rename columns for better interpretation
df<- df %>% rename ('Games.played'='G',
                    'plate.appearances'='PA',
                    'hits'='H',
                    'home.runs'='HR',
                    'runs.scored'='R',
                    'runs.batted.in'='RBI',
                    'stolen.base'='SB',
                    'batting.average'='AVG',
                    'on.base.perc'='OBP',
                    'running.speed.score'='Spd',
                    'hit.batted.ball'='batted.ball',
                    'bat.exit.velocity'='EV',
                    'Clutch'='Clutch',
                    'Swing.perc'='Swing.',
                    'total.contact.perc'='Contact.',
                    'swing.strike.perc'='SwStr.',
                    'ball.fastball.ratio'='wFB.C',
                    'ball.sliders.ratio'='wSL.C',
                    'ball.cutters.ratio'='wCT.C',
                    'ball.curves.ratio'='wCB.C',
                    'ball.changeups.ratio'='wCH.C',
                    'ball.splitters.ratio'='wSF.C',
                    'Wins.above.replacement'='WAR',
                    'Name'='Name',
                    'Team'='Team',
                    'ball.fastball.perc'='fastball.',
                    'ball.sliders.perc'='SL.',
                    'ball.cutters.perc'='CT.',
                    'ball.curves.perc'='CB.',
                    'ball.changeups.perc'='CH.',
                    'ball.splitters.perc'='SF.',
                    'strike.outs'='SO',
                    'Infieldfly.perc'='IFFB.',
                    'hit.infield.perc'='IFH.',
                    'hit.bunt.perc'='BUH.',
                    'hit.CF.perc'='Cent.',
                    'hit.push.perc'='Oppo.',
                    'hit.softspeed.perc'='Soft.',
                    'hit.medspeed.perc'='Med.',
                    'hit.hardspeed.perc'='Hard.',
                    'out.swing.perc'='O.Swing.',
                    'in.swing.perc'='Z.Swing.',
                    'out.Contact.perc'='O.Contact.',
                    'in.contact.perc'='Z.Contact.',
                    'at.bats'='AB',
                    'singles'='X1B',
                    'doubles'='X2B',
                    'triples'='X3B',
                    'bases.on.balls'='BB',
                    'intentional.bases.on.balls'='IBB',
                    'hit.by.pitch'='HBP',
                    'sacrifice.fly'='SF',
                    'sacrifice.hit'='SH',
                    'ground.into.a.doubleplay'='GDP',
                    'caught.stealing'='CS',
                    'BB.PA'='BB.',
                    'strikouts.plate.perc'='K.',
                    'bases.on.balls.strikeouts'='BB.K',
                    'slugging.perc'='SLG',
                    'OPS'='OPS',
                    'Isolated Power'='ISO',
                    'on.balls.in.play'='BABIP',
                    'ultimate.base.running'='UBR',
                    'wGDP'='wGDP',
                    'stolenbase.stealing.ratio'='wSB',
                    'wRuns.created'='wRC',
                    'wRuns.AboveAvg'='wRAA',
                    'wOBA'='wOBA',
                    'wRuns.created.plus'='wRC.',
                    'GroundBall.fly.ratio'='GB.FB',
                    'LineDrive.perc'='LD.',
                    'GroundBall.perc'='GB.',
                    'Flyball.perc'='Flyball.',
                    'HomeRun.Flyball.ratio'='HR.FB',
                    'hit.infield'='IFH',
                    'hit.bunt'='BUH',
                    'hit.pull.perc'='Pull.',
                    'bat.max.exit.velocity'='maxEV',
                    'launch.angle'='LA',
                    'bat.barrels'='Barrels',
                    'bat.barrel.perc'='Barrel.',
                    'bat.hardHit'='HardHit',
                    'bat.hardHit.perc'='HardHit.',
                    'x.batting.average'='xBA',
                    'x.slugging.perc'='xSLG',
                    'x.wOBA'='xwOBA',
                    'Zone.perc'='Zone.',
                    'F-Strike.perc'='F.Strike.',
                    'called.strike.perc'='CStr.',
                    'called.swing.strikes.perc'='CSW.',
                    'ball.fastball'='wFB',
                    'ball.sliders'='wSL',
                    'ball.cutters'='wCT',
                    'ball.curves'='wCB',
                    'ball.changeups'='wCH',
                    'ball.splitters'='wSF',
                    'ball.fastball.velocity'='FBv',
                    'ball.sliders.velocity'='SLv',
                    'ball.cutters.velocity'='CTv',
                    'ball.curves.velocity'='CBv',
                    'ball.changeups.velocity'='CHv',
                    'ball.splitters.velocity'='SFv',
                    'Batting.wOBA'='Batting',
                    'Base.Running'='Base.Running',
                    'Fielding'='Fielding') 


################################### Outlier identification based on Z-score ############################

num_cols <-names(select_if(df, is.numeric))

# scale 
zscores=scale(df[,num_cols])

#If a player has a variables with Z-score less than -3 or greater than 3 then they are considered an outlier
outliers= apply(zscores, 2, function(x) abs(x) > 3)

# Write outlier matrix
write.csv(outliers, file = "./outliers_matrix.csv", row.names = FALSE)
cleandata = df[!outliers, ]

# Add column identifying outliers
df$outlier<- rowSums(outliers)>0


##################################### Exploratory Data Analysis #########################################
# summarize 
summary(df)

#Structure of Data- See column names, data types, and shape of data frame
str(df)

# null values 
colSums(is.na(df)) 

# dimensions 
dim(df)

# Numerical data only 
num_cols <- sapply(df, is.numeric)

# determinant 
det(cov(df[,num_cols]))



##################################### Distribution #####################################

df_long <- df[,num_cols][1:30] %>% pivot_longer(cols = everything(),
                                               names_to = "variable", values_to = "value")

ggplot(df_long, aes(x = value)) +
  geom_density(fill = "blue") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Density plots for 30 variables",
       x = "Value",
       y = "Density") +
  theme_minimal()+ theme(
    plot.title = element_text(size = 12), 
    axis.title = element_text(size = 2),
    axis.text = element_text(size = 2),
    strip.text = element_text(size = 8)
  )

# KDE plot per team
ggplot(mapping = aes(df$batting.average, col = df$Team)) +
  geom_density(alpha = .5)+
  labs(title = "Density of Batting Average by Team", x = "Batting Average", y = "Density")+
  guides(color = guide_legend(title = "Team")) 

##################################### Batting Average #####################################

# kde for batting average 
mean_value <- mean(df$batting.average, na.rm = TRUE)
ggplot(mapping = aes(x = df$batting.average)) +
  geom_density(fill = "blue") +
  geom_vline(aes(xintercept = mean_value), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Density of Batting Average",
       x = "Batting Average", y = "Density")

# Batting average with key variables
# top players by batting average  
top10_ba<-df[order(df$batting.average, decreasing = TRUE),][1:10,c("Name","batting.average")]
top10_ba

# top teams by mean batting average  
grp_team <- aggregate(df$batting.average, by=list(Category=df$Team), FUN=sum)
grp_team[order(grp_team$x,decreasing = TRUE),][1:10,]


##################################### Correlation #####################################
# Correlation plot
cor_df <- cor(df[,3:length(df)])

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor_df , type="lower", method="color" ,addCoef.col = "black", number.cex= 0.1,  
         tl.col="black", tl.cex =0.1, col=col(200),diag=FALSE, cex.main = 1, main = "Correlation plot")

# Correlation table function

corr_simple <- function(data=df,sig=0.5){
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  corr <- cor(df_cor)
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  corr[corr == 1] <- NA 
  corr <- as.data.frame(as.table(corr))
  corr <- na.omit(corr) 
  corr <- subset(corr, abs(Freq) > sig) 
  corr <- corr[order(-abs(corr$Freq)),] 
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}
cor_df<- corr_simple(df)$corrPos

cor_df[order(cor_df$corr,decreasing=TRUE),]

#write correlation matrix 
write.csv(cor_df[order(cor_df$corr,decreasing=TRUE),], file = paste0(getwd(),outpath,"/corr_matrix.csv"), row.names = FALSE) 


############################ Feature importance (lightgbm) #########################################


# set train and test 
train_indices <- sample(1:nrow(df), size = 0.7*nrow(df))
train_data <- df[train_indices, 3:ncol(df)]
test_data <- df[-train_indices, 3:ncol(df)]

# set lightgbm parameters
lgb_train <- lgb.Dataset(data = as.matrix(train_data[, -which(names(train_data) == "batting.average")]),
                         label = train_data$batting.average)
lgb_test <- lgb.Dataset(data = as.matrix(test_data[, -which(names(test_data) == "batting.average")]),
                        label = test_data$batting.average)


params <- list(
  objective = "regression",
  metric = "l2",
  num_leaves = 31,
  learning_rate = 0.05,
  n_estimators = 100
)

# train model 
model <- lgb.train(params,
                   lgb_train,
                   valids = list(test = lgb_test),
                   num_boost_round = 100,
                   early_stopping_rounds = 10)

# Identify importance
importance <- lgb.importance(model)

#plot importance
ggplot(importance[importance$Gain>0.001,], aes(x = reorder(Feature, -Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Feature Importances in LightGBM Model",
       x = "Feature",
       y = "Importance (Gain)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


####################################### Drop variables ########################################

drop_cols <- c("at.bats")
df<- select(df, -drop_cols)

#Export clean data to csv 
write.csv(df, file = "./cleandata.csv", row.names = FALSE)

