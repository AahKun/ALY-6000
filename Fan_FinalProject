#Part I
#================================================================
#load libraries: FSA and plyr
library(FSA)
library(plyr)
#==========================================
#Import the nhlplayoffs.csv and name the table <Bruins>
Bruins <- read.csv("nhlplayoffs.csv", header = TRUE)
#========================================
#Structure of <Bruins>
str(Bruins)
#==========================================
#Summary of <Bruins>
summary(Bruins)
#=========================================
#Take a look at the dataset <Bruins>
headtail(Bruins)
view(Bruins)
#================================================================
#Setting business question 
#"How often did the Boston Bruins progress to the top 16 teams of the playoffs compared to other teams?"
#================================================================
#Start cleaning data-------------------
#Make sure there is no NA in the dataset
CBruins <- na.omit(Bruins)

#Make sure each years has 16 teams recorded
#levels(as.factor(CBruins$year)) #find the recorded years
CBruins <- CBruins[order(CBruins$rank),] #reorder by rank (for following cleaning execution)
row.names(CBruins) <- c(1:nrow(CBruins)) #rename row (for following cleaning execution to get the correct row number)
#view(CBruins)
#For loop, if any year's record does not have completed 16 teams, then that year's record will be deleted.
for (i in levels(as.factor(CBruins$year))) {
  check <- CBruins[CBruins$year == i,] #get the specific year's dataset
  if(setequal(check[["rank"]],c(1:16))){#making sure this specific year has exactly 16 teams
  } else{
    CBruins <- CBruins[-c(as.numeric(row.names(CBruins[CBruins$year == i,]))),] #delete the rows
    row.names(CBruins) <- c(1:nrow(CBruins)) #rename row after delete
  }
}
CBruins <- CBruins[order(-CBruins$year),] #reorder by year (original)
row.names(CBruins) <- c(1:nrow(CBruins)) #rename row
#view(CBruins) 
#Finish cleaning data------------------
#================================================================
#Count the freq of each teams---------------------
ct <- count(CBruins["team"])
#================================================================
#Plot1 Five-Number Summary and Mean of Count of each Team Playoff Appearances (box chart)---------------------------
b <- boxplot(ct$freq, main = "Plot 1: Five-Number Summary and Mean of Count of each Team Playoff Appearances",
             xlab = "Counts", col = "#FCB60C", horizontal = TRUE)
text(b$stats, 1.25, labels = b$stats)
M <- mean(ct$freq)
segments(M,0.8,M,1.2, lty = 3, col = "gray")
text(M, 0.75, paste("mean =", round(M), sep = " "), col = "black")
#================================================================
#order count from high to low
cto <- ct[order(-ct$freq),] 
#================================================================
#Plot2 ordered Freq of each teams (bar chart)--------------------------
varPar <- par(no.readonly = TRUE) #store default graphical parameters setting and set it as modifiable
par(mar = c(7, 4, 4, 2) + 0.1) #gives the number of lines of margin to be specified on the four sides of the plot; c(bottom, left, top, right)

barplot(cto$freq, main = "Plot 2: Count of each Team Playoff Appearances",
        names.arg = cto$team, xlab = "", ylab = "Counts", col= "#FCB60C",
        las=2, cex.names = 0.55)
#set las as perpendicular; set cex.names as 0.55 magnification

par(varPar) #reset parameters
#================================================================
#Plot3 Freq% of each teams (bar chart)-----------------------
varPar <- par(no.readonly = TRUE) #store default graphical parameters setting and set it as modifiable
par(mar = c(7, 4, 4, 2) + 0.1) #gives the number of lines of margin to be specified on the four sides of the plot; c(bottom, left, top, right)

sumfreq <- sum(cto$freq)
b <- barplot((cto$freq/sumfreq)*100, main = "Plot 3: Count% of each Team Playoff Appearances (Bruins)",
        axes = F, ylim = c(0, 1.2 * max((cto$freq/sumfreq)*100, na.rm = T)),
        names.arg = cto$team, xlab = "", ylab = "Counts", col= "#FCB60C",
        las=2, cex.names = 0.55)
#build a bar chart; using freq percentage

axis(side = 2, at = c(0, (cto$freq/sumfreq)*100), las = 1,
     labels = paste(c(0, round((cto$freq/sumfreq)*100)) , "%", sep = ""),
     cex.axis = 0.75)
#add left side Y axis label to plot 3; side = 2 is left Y axis; labels is to replace the labels according to cto$freq
#paste is to convert and concatenate (cto$freq/sumfreq)*100 and "%" to character

text(b[2], (cto$freq[2]/sumfreq)*100, labels = "2nd", adj = c(0.5,-1), col = "red")
#add "2nd" text on the top of boston bruins' bar

axis(side = 1, b[2], las = 2, labels = "Boston Bruins", tick = F, cex.axis = 0.55, col.axis = "red")
#make the Boston Bruins' axis be red (highlight)

par(varPar) #reset parameters
#================================================================
#plot4 Percentage of Boston Bruins Reaching Playoffs (Pie chart)---------------------------------
BCB <- CBruins[CBruins$team == "Boston Bruins",] #filter Boston Bruins
B <- length(BCB$year) #number of years bruins reaching playoffs
t <- length(levels(as.factor(CBruins$year))) #total years

pie(c(t-B,B),
    labels = c(paste(round(100*(t-B)/t, 1), "%", sep = ""), paste(round(100*B/t, 1), "%", sep = "")), 
    main = "Plot 4: Percentage of Boston Bruins Reaching Playoffs", col = c("black","#FCB60C"))
#build a pie chart; Percentage of Boston Bruins Reaching Playoffs

legend("topleft", inset = 0, c("Not Reaching Playoffs","Reaching Playoffs"), cex = 0.8,
       fill = c("black","#FCB60C"))
#add legend; inset is the distance from the edge
#================================================================
#Part II------------------------------
#================================================================
#create a new attribute to record the rank diff each two years-----------
BCBO <- BCB[order(BCB$year),] #reorder by year 
row.names(BCBO) <- c(1:nrow(BCBO)) #rename row 
BCBO$rankDiff[1] <- 0 #new attrubute named <rankDiff> and set the first value as 0
for(i in 2:nrow(BCBO)){ #compute differences of each two years
  BCBO$rankDiff[i] <- BCBO$rank[i-1] - BCBO$rank[i]
  }
#================================================================
#Plot5 visualize the ranking differences of Bruins----------
plot(as.numeric(row.names(BCBO)), BCBO$rankDiff, main = "Plot 5: Bruins Ranking Differences by Year",
     xlab = "Year", ylab = "Differences", axes = F, ylim = c(-15,15),
     pch = 19, col = "#FCB60C")
lines(as.numeric(row.names(BCBO)), BCBO$rankDiff, type = "b", pch = 1) #add line to connect each values

segments(0,0,36,0, lty = 3, col = "#777777") #add 0 diff line

box()#add box

axis(side = 2, at = c(-15,-10,-5,0,5,10,15), las = 1, cex.axis = 1)
#add left side Y axis label to plot 3; side = 2 is left Y axis; at is to set label according to cumCts; las = 1 is horizontal; col.axis and col is to set label color; cex.axis = 0.75 is th set font scale to 75%

axis(side = 1, at = c(1,5,10,15,20,25,30,34), labels = BCBO$year[c(1,5,10,15,20,25,30,34)], las = 1, cex.axis = 1)
#add left side Y axis label to plot 3; side = 1 is left Y axis; at is to set label according to cumCts; las = 1 is horizontal; col.axis and col is to set label color; cex.axis = 0.75 is th set font scale to 75%

#================================================================
#Plot6 Five-Number Summary and Mean----------------------
varPar <- par(no.readonly = TRUE) #store default graphical parameters setting and set it as modifiable
par(mar = c(7, 1, 4, 1) + 0.1) #gives the number of lines of margin to be specified on the four sides of the plot; c(bottom, left, top, right)

b <- boxplot(BCBO$rankDiff, main = "Plot 6: Five-Number Summary and Mean of Bruins Ranking Differences",
             xlab = "Counts", ylim = c(-15,15), col = "#FCB60C", horizontal = TRUE)
text(b$stats, 1.25, labels = b$stats)
M <- mean(ct$freq)
segments(mean(BCBO$rankDiff),0.8,mean(BCBO$rankDiff),1.2, lty = 3, col = "gray")
text(mean(BCBO$rankDiff), 0.75, paste("mean =", round(mean(BCBO$rankDiff)), sep = " "), col = "black")

par(varPar) #reset parameters
#================================================================
#Part III-------------------------------
#================================================================
#Teams Championship Frequency (Bruins)--------------------------
#Create new attribute <cumFreq> that is the cumulative countof each teams---------------
Champ <- Bruins[Bruins$rank == 1,]
champFreq <- count(Champ["team"])
champFreq <- champFreq[order(-champFreq$freq),]
champFreq["cumFreq"] <- cumsum(champFreq$freq)
champFreq["FreqP"] <- 100*champFreq$freq/length(levels(as.factor(Bruins$year)))
champFreq["cumFreqP"] <- cumsum(champFreq$FreqP)

#Plot7 Pareto chart of <Freq> and of each teams-----------------------
varPar <- par(no.readonly = TRUE) #store default graphical parameters setting and set it as modifiable
par(mar = c(6, 4, 3, 3) + 0.1) #gives the number of lines of margin to be specified on the four sides of the plot; c(bottom, left, top, right)

pc<-barplot(champFreq$freq,  
            width = 1, space = 0.1, border = F, axes = F,
            ylim = c(0, 3.9 * max(champFreq$freq, na.rm = T)), 
            ylab = "Cummulative Counts",  col = "#FCB60C",
            names.arg = champFreq$team, cex.names = 0.55,
            main = "Plot 7: Teams Championship Frequency Pareto", las=2)
#border = F is to hide the border fo the bar; axes = F is to hide the axes; set Y axis label from 0 to 3.05*max value of cts; names.arg is to set X axis names

lines(pc, champFreq$cumFreq, type = "b", cex = 0.75,pch = 19, col = "blue")
#Add a cumulative counts line to the <pc> plot

box() #add box

axis(side = 2, at = c(0, champFreq$cumFreq), las = 1, cex.axis = 0.75)
#add left side Y axis label to plot 3; side = 2 is left Y axis; at is to set label according to cumCts; las = 1 is horizontal; col.axis and col is to set label color; cex.axis = 0.75 is th set font scale to 75%

axis(side = 4, at = c(0, champFreq$cumFreq), las = 1,
     labels = paste(c(0, round(champFreq$cumFreqP)) , "%", sep = ""), cex.axis = 0.75)
#add right side Y axis label to plot 3; side = 4 is right Y axis; labels is to replace the labels according to champFreq$cumFreqP

axis(side = 1, c(pc[4],pc[22]), las = 2, labels = c(champFreq$team[4],champFreq$team[22]), tick = F, cex.axis = 0.55, col.axis = "red")
#make the Boston Bruins' axis be red (highlight)

text(c(pc[4],pc[22]), c((champFreq$freq[4]),(champFreq$freq[22])), labels = c(champFreq$freq[4],champFreq$freq[22]), adj = c(0.5,-0.5), col = "black")
#add text on those two highlight teams; labels is count of championship

par(varPar) #reset parameters
#================================================================
#Boston Bruins' winning percentage been over the years----------------------
#Plot8 Historgram of winning percentage distribution
varPar <- par(no.readonly = TRUE) #store default graphical parameters setting and set it as modifiable
par(mar = c(5,4,4,2) + 0.1) #gives the number of lines of margin to be specified on the four sides of the plot; c(bottom, left, top, right)

#View(Bruins[Bruins$team == "Boston Bruins",])

hist(Bruins[Bruins$team == "Boston Bruins",]$win_loss_percentage,
     main = "Plot 8: Histogram of Bruins Winning Percentage",
     xlab = "Winning Percentage", col = "#FCB60C")

par(varPar) #reset parameters
#===================================================================
#goals scored per game in each ranking over the years
#Create a new attribute <goals_scored_per_game>
CBruins["goals_scored_per_game"] <- CBruins$goals_scored/CBruins$games

#Plot9 Goals Scored Per Game by Rank
boxplot(CBruins$goals_scored_per_game~CBruins$rank, 
        main = "Plot 9: NHL Playoff Goals Scored per Game by Rank",
        xlab = "Rank", ylab = "Goals Scored per Game", col = "#FCB60C")
#===================================================================
#goals allowed per game in each ranking over the years
#Create a new attribute <goals_allowed_per_game>
CBruins["goals_allowed_per_game"] <- CBruins$goals_against/CBruins$games

#Plot10 Goals Scored Per Game by Rank
boxplot(CBruins$goals_allowed_per_game~CBruins$rank, 
        main = "Plot 10: NHL Playoff Goals Allowed per Game by Rank",
        xlab = "Rank", ylab = "Goals Allowed per Game", col = "#FCB60C")
#======================================================================

