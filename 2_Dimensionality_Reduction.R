#load libraries
library(corrplot)
library(MVA)
library(ggplot2)


#read in data
filepath <- ('https://raw.githubusercontent.com/Slyth3/Multivariate-Analysis-Baseball-Hitting-Statistics/main/cleandata.csv')
baseball <- read.csv(filepath)
#set rowmnames to player names
rownames(baseball) <- baseball$Name
#check out data
head(baseball)

#subset interesting columns representative of various talents 
df <- baseball[,c(1,2,5,7,8,9,10,19,29)]

############### Initial Assessment of Data Correlation #################
#correlation matrix
corr.matrix <- round(cor(df[,3:ncol(df)]),3)

#correlation plot
corrplot(corr.matrix, method = "color",
         main = "Correlation Plot")

##################### Outlier Investigation ############################
#scatterplot matrix for initial outlier detection
plot(df[3:ncol(df)], main = "Scatterplot Matrix")
var(df$triples)

#bv boxplot of doubles and triples
plot(df$doubles, df$triples, cex = .3,
     main = "BV Boxpot Triples and Doubles",
     xlab = "Doubles",
     ylab = "Triples")
#add text to the plot (or comment out lines 35-36 for readability)
text(df$doubles, df$triples,
     labels = row.names(df))
#add bvbox
bvbox(cbind(df$doubles, df$triples), add = TRUE)


#bv boxplot of stolen bases and running speed score
plot(df$stolen.base, df$running.speed.score, cex = .3,
     main = "BV Boxplot- Stolen Bases and Running Speed Score",
     xlab = "Stolen Bases",
     ylab = "Running Speed Score")
#add text to the plot (or comment out lines 47-48 for readability)
text(df$stolen.base, df$running.speed.score,
    labels = row.names(df))
#add bvbox
bvbox(cbind(df$stolen.base, df$running.speed.score), add = TRUE)


#bv boxplot of hits and homeruns
plot(df$hits, df$home.runs, cex = .3,
     main = "Bivariate Boxplot- Hits and Homeruns",
     xlab = "Hits",
     ylab = "Homeruns")
#add text to the plot (or comment out lines 60-61 for readability)
text(df$hits, df$home.runs,
    labels = row.names(df))
#add bvbox
bvbox(cbind(df$hits, df$home.runs), add = TRUE)

################################### Conduct PCA #######################################
#scale data
df.s <- scale(df[3:ncol(df)])
#conduct PCA
baseball.pca <- princomp(df.s)
#summary
summary(baseball.pca, loadings = TRUE)

#plot PCA scores by names
ggplot() +
  geom_text(aes(x = baseball.pca$scores[,1],
                y = baseball.pca$scores[,2],
                label = df$Name,
                col = df$Team,
                main = "PCA Scores"),
            nudge_x = .5, nudge_y = .5, check_overlap = TRUE)

#biplot
biplot(baseball.pca, cex = .5, main = "Biplot of PCA Scores")

