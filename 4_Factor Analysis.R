# Load Libraries 
library(sem)
library(dplyr)
library(corrplot)
#install.packages("lavaan", dependencies = TRUE)
#install.packages("semPlot")
library(semPlot)
library(RColorBrewer)


# set working directory and outpaths 
setwd("G:/My Drive/Data Science/University/Texas Tech/Semester 2/Multivariate/Project/")
filepath = "./cleandata.csv"

# Set/create folder
outpath = "/4_outfiles"

if (!dir.exists(paste0(getwd(),outpath))){
  dir.create(paste0(getwd(),outpath))
}else{
  print("dir exists")
}

df = read.csv(filepath)

##########################  Exploratory Factor Analysis ###########################
num_cols <- sapply(df, is.numeric)

# Scale data 
df_s <- scale(df[,num_cols])

# check if covariance matrix is invertable 
det(cov(df_s))  

# iterative process to determine which variables hinder EFA 
drop_s <- c('home.runs','hit.batted.ball','Base.Running')

# run EFA numerous times with differing factors, analyse the pvalue
fa_iter<- sapply(1:85, function(f) factanal(df_s[,!(colnames(df_s) %in% drop_s)], factors = f,lower = 1)$PVAL)
plot(fa_iter, ylab = "p-value", xlab= "Number of factors", main = "EFA: p-value by number of factors", col = "orange")

# Run EFA and look at the cumulative proportion of the variance
fa_s<-factanal(df_s[,!(colnames(df_s) %in% drop_s)], 
             factors = 4, lower = 0.05)

print(fa_s$loadings, cut= 0.5)

#write factor loadings matrix 
write.csv(round(fa_s$loadings,2), file = paste0(getwd(),outpath,"/factor_loadings.csv"), row.names = TRUE) 


##########################  Confirmatory Factor Analysis ###########################

# Define key columns for CFA (remove Name, Team and at.bats)
key_cols <- c( 'Games.played',
               'plate.appearances',
               'hits',
               'runs.scored',
               'runs.batted.in',
               'stolen.base',
               'batting.average',
               'on.base.perc',
               'running.speed.score',
               'hit.batted.ball',
               'bat.exit.velocity',
               'Clutch',
               'Swing.perc',
               'total.contact.perc',
               'swing.strike.perc',
               'ball.fastball.ratio',
               'ball.sliders.ratio',
               'ball.cutters.ratio',
               'ball.curves.ratio',
               'ball.changeups.ratio',
               'ball.splitters.ratio',
               'Wins.above.replacement')
# check determinant 
det(cov(df[,key_cols]))


# Confirmatory Factor Analysis 
options(fit.indices = c("GFI", "AGFI", "SRMR"))
batting_model <- specifyModel(file = paste0(getwd(),outpath,"/CFA_model.txt") )


cfa <- sem(batting_model,S= cor(df_s[,key_cols]), N = nrow(df) )
cfa

# Path Diagram 
semPaths(cfa, data = df_s[,key_cols])


# Livaan library
library(lavaan)

batting_model <- '
runs =~ runs.scored + runs.batted.in + Clutch + on.base.perc
ball =~ ball.fastball.ratio + ball.sliders.ratio + ball.cutters.ratio + ball.curves.ratio + ball.changeups.ratio + ball.splitters.ratio
hitting =~ hits + runs.batted.in + batting.average + hit.batted.ball + bat.exit.velocity + Swing.perc + total.contact.perc + swing.strike.perc
non.batting =~  + plate.appearances + stolen.base + running.speed.score + Clutch + Wins.above.replacement + Wins.above.replacement + Games.played + plate.appearances
'

model <- cfa(batting_model, data= df[,key_cols])
summary(model)
