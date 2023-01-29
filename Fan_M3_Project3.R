#1 - Print your name at the top of the script and load these libraries: FSA, FSAdata, magrittr, dplyr, tidyr plyr and tidyverse
print("Kun-Wei Fan")
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)

#2 - Import the inchBio.csv and name the table <tBio>
tBio <- read.csv("inchBio.csv", header = TRUE) #import the inchBio.csv

#3 - Display the first 3 and last 3 records, summary and structure of <tBio>
headtail(tBio, 3) #display first and last three obs.
summary(tBio)
str(tBio)

#4 - Create an object <cts>, that counts and lists all the species records
cts <- tBio["species"] #Create cts object
#length(cts$species) #counts
cts #lists

#5 - Display just the 8 levels (names) of the species
levels(as.factor(cts$species)) #Method 1: transfer to factor and display level names (order by name)
#unique(cts$species) #Method 2: display level names (order by appearance)

#6 - Create a <temp1> object that displays the different species and the number of records of each species in the dataset. Include this information in your report.
#temp1 <- summary(as.factor(cts$species)) #method 1: use summary to create named int variable of counts
temp1 <- count(cts) #method 2: use count to create two variables, names and freq
temp1

#7 - Create a subset, <temp2>, of just the species variable and display the first 3 records
temp2 <- cts$species[1:3] #first three species variables
temp2

#8 - Create a table, <t>, of the species variable. Display the class of <t>
t <- table(cts$species) #create a table of the species variable
class(t)

#9 - Convert <t> to a data frame named <df> and display the results
df <- as.data.frame(t) #convert to a data frame
df

#10 - Extract and display the frequency values from the <df> data frame as variable freq
freq <- df["Freq"] #extract and import freq from df to freq
freq #display

#11 - Create a table named <tSpec> from the <tBio> species attribute (variable) and confirm that you created a table which displays the number of species in the dataset <tBio>
tSpec <- table(tBio$species) #create a table of tBio's species variable
tSpec

#12 - Create a table named <tSpecPct> that displays the species and percentage of records for each species. Confirm you created a table class.
tSpecPct <- tSpec/nrow(tBio) #count divided by number of obs. is percentage
tSpecPct
class(tSpecPct)

#13 - Convert the table, <tSpecPct>, to a data frame named <dfSP> and confirm that <dfSP> is a data frame
dfSP <- as.data.frame(tSpecPct) #convert to a data frame
dfSP
class(dfSP)

#14 - Create a barplot of <tSpec>
barplot(tSpec,main ="Plot 1: Fish Count", ylab="Counts",
        ylim = c(0,250), col= "red",
        las=2, cex.names = 0.55) 
#Create a bar plot; las is rotate axis label and 2 is perpendicular, which is also vertical for x axis; cex.names is change the magnification of x axis font

#15 - Create a barplot of <tSpecPct>
barplot(tSpecPct,main ="Plot 2: Fish Relative Frequency", ylab="%",
        ylim = c(0,0.35), col= "yellow",
        las=2, cex.names = 0.55) 
#same as #14 but using tSpecPct table

#16 - Rearrange the <dfSP> data frame in descending order of relative frequency. Save the rearranged data frame as the object <data>
data <- dfSP[order(-dfSP$Freq),] #rearrange dfSP in descending order of relative Freq
data

#17 - Rename the <data> columns Var 1 to Species, and Freq to RelFreq
data <- rename(data, c(Var1 = "Species", Freq = "RelFreq")) #Rename the <data> columns Var 1 to Species, and Freq to RelFreq
data

#18 - Add new variables to <data> and call them cumFreq, cts, and cumCts
data["cumFreq"] <- cumsum(data$RelFreq) #add new variable cumFreq, which is cumulative sum of Freq
data["cts"] <- data$RelFreq*nrow(tBio) #add new variable cts, which is count of species
#data[c("cumFreq","cts")] <- c(cumsum(data$RelFreq),data$RelFreq*nrow(tBio)) #if there is no sequential concern, it can be done together
data["cumCts"] <- cumsum(data$cts) #add new variable cumCts, which is cumulative sum of cts
data

#19 - Create a parameter variable <varPar> to store graphical parameters
varPar <- par(no.readonly = TRUE) #store default graphical parameters setting and set it as modifiable
par(mar = c(5, 4, 4, 3) + 0.1) #gives the number of lines of margin to be specified on the four sides of the plot; c(bottom, left, top, right)
#Since I feel that the distance on the right is not enough, I add a little more.

#20 - Create a barplot for data$cts, <pc>
pc<-barplot(data$cts,  
            width = 1, space = 0.1, border = F, axes = F,
            ylim = c(0, 3.05 * max(data$cts, na.rm = T)), 
            ylab = "Cummulative Counts",  
            names.arg = data$Species, cex.names = 0.55,
            main = "Plot 3: Species Pareto", las=2)
#border = F is to hide the border fo the bar; axes = F is to hide the axes; set Y axis label from 0 to 3.05*max value of cts; names.arg is to set X axis names

#21 - Add a cumulative counts line to the <pc> plot
lines(pc, data$cumCts, type = "b", cex = 0.75,pch = 17, col = "blue")
#add line to pc according to cumCts

#22 - Place a grey(grey62) box around the pareto plot
box(col="grey62") #add grey box to plot 3

#23 - Add a left side axis
axis(side = 2, at = c(0, data$cumCts), las = 1,
     col = "grey62", col.axis = "grey62", cex.axis = 0.75)
#add left side Y axis label to plot 3; side = 2 is left Y axis; at is to set label according to cumCts; las = 1 is horizontal; col.axis and col is to set label color; cex.axis = 0.75 is th set font scale to 75%

#24 - Add axis details on right side of box
axis(side = 4, at = c(0, data$cumCts), las = 1,
     labels = paste(c(0, round(data$cumFreq * 100)) , "%", sep = ""),
     col = "cyan4", col.axis = "cyan4",
     cex.axis = 0.75)
#add right side Y axis label to plot 3; side = 4 is right Y axis; labels is to replace the labels according to cumFreq
#paste is to convert and concatenate cumFreq*100 and "%" to character

#25 - Display the finished Species Pareto Plot
mtext("Fan", side = 3, outer = TRUE, 
      line = -4, col = "red", cex = 0.75)
#add my last name on the plot with red color; outer = T is to add text outside of the plot
#line is to adjust the place(line) the text located
#text(0.5, 670, "Fan", col = "red", cex = 0.75) #Another way to add text

par(varPar) #reset graphical parameters as I store in varPar 
