library(tidyverse)
# library(activity)
library(overlap)
#library(lubridate)
#library(plyr)
library(circular)
#library(scales)
#library(CircSpaceTime)


# call the table 

caps<-read.csv(file.choose(),head=T)

# split plot screen into multiple plots

if(!is.null(dev.list())) dev.off()

par(mfcol= c(2,2))
#par(mar=c(2, 2, 1, 1) + 3)

# if(!is.null(dev.list())) dev.off() # clear plots

#### filter for a species ###


######## Cuniculus paca   ################

Species <- filter(caps, Species == "Cuniculus paca" & Buffer == 500)

# Fine mean HF

M=mean(Species$HEH_18_Mean)
# boxplot(Species$HEH_18_Mean)

# Species High HF

SpeciesH <- filter(Species, HEH_18_Mean > M)


# Species Low HF

SpeciesL <- filter(Species, HEH_18_Mean < M)


# Circular Plot High (Grey)
plotdata = circular(SpeciesH$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata, bins=24, col = "gray60", prop=2, cex = 1, uin=1,
          tcl=0.05, main="Paca")


# Circular Plot High (Grey) & Low (White)
plotdata2 = circular(SpeciesL$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata2, bins=24, col = NULL, prop=2, cex = 1, axes=TRUE, uin=1
          , tcl=0.05, main=(unique(Species2$Species)),add=TRUE)


######## Dasyprocta fuliginosa   ################

Species <- filter(caps, Species == "Dasyprocta fuliginosa" & Buffer == 1000)

# Fine mean HF

M=mean(Species$HEH_18_Mean)
# boxplot(Species$HEH_18_Mean)

# Species High HF

SpeciesH <- filter(Species, HEH_18_Mean > M)


# Species Low HF

SpeciesL <- filter(Species, HEH_18_Mean < M)


# Circular Plot High (Grey)
plotdata = circular(SpeciesH$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata, bins=24, col = "gray60", prop=2, cex = 1, uin=1,
          tcl=0.05, main="Agouti")


# Circular Plot High (Grey) & Low (White)
plotdata2 = circular(SpeciesL$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata2, bins=24, col = NULL, prop=2, cex = 1, axes=TRUE, uin=1
          , tcl=0.05, main=(unique(Species2$Species)),add=TRUE)


######## Dasypus novemcinctus   ################

Species <- filter(caps, Species == "Dasypus novemcinctus" & Buffer == 500)

# Fine mean HF

M=mean(Species$HEH_18_Mean)
# boxplot(Species$HEH_18_Mean)

# Species High HF

SpeciesH <- filter(Species, HEH_18_Mean > M)


# Species Low HF

SpeciesL <- filter(Species, HEH_18_Mean < M)


# Circular Plot High (Grey)
plotdata = circular(SpeciesH$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata, bins=24, col = "gray60", prop=2, cex = 1, uin=1,
          tcl=0.05, main="Armadillo")


# Circular Plot High (Grey) & Low (White)
plotdata2 = circular(SpeciesL$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata2, bins=24, col = NULL, prop=2, cex = 1, axes=TRUE, uin=1
          , tcl=0.05, main=(unique(Species2$Species)),add=TRUE)


######## Didelphis marsupialis   ################

Species <- filter(caps, Species == "Didelphis marsupialis" & Buffer == 500)

# Fine mean HF

M=mean(Species$HEH_18_Mean)
# boxplot(Species$HEH_18_Mean)

# Species High HF

SpeciesH <- filter(Species, HEH_18_Mean > M)


# Species Low HF

SpeciesL <- filter(Species, HEH_18_Mean < M)


# Circular Plot High (Grey)
plotdata = circular(SpeciesH$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata, bins=24, col = "gray60", prop=2, cex = 1, uin=1,
          tcl=0.05, main="Common opossum")


# Circular Plot High (Grey) & Low (White)
plotdata2 = circular(SpeciesL$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata2, bins=24, col = NULL, prop=2, cex = 1, axes=TRUE, uin=1
          , tcl=0.05, main=(unique(Species2$Species)),add=TRUE)


######## Eira barbara   ################

Species <- filter(caps, Species == "Eira barbara" & Buffer == 500)

# Fine mean HF

M=mean(Species$HEH_18_Mean)
# boxplot(Species$HEH_18_Mean)

# Species High HF

SpeciesH <- filter(Species, HEH_18_Mean > M)


# Species Low HF

SpeciesL <- filter(Species, HEH_18_Mean < M)


# Circular Plot High (Grey)
plotdata = circular(SpeciesH$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata, bins=24, col = "gray60", prop=2, cex = 1, uin=1,
          tcl=0.05, main="Tayra")


# Circular Plot High (Grey) & Low (White)
plotdata2 = circular(SpeciesL$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata2, bins=24, col = NULL, prop=2, cex = 1, axes=TRUE, uin=1
          , tcl=0.05, main=(unique(Species2$Species)),add=TRUE)


######## Philander opossum   ################

Species <- filter(caps, Species == "Philander opossum" & Buffer == 1000)

# Fine mean HF

M=mean(Species$HEH_18_Mean)
# boxplot(Species$HEH_18_Mean)

# Species High HF

SpeciesH <- filter(Species, HEH_18_Mean > M)


# Species Low HF

SpeciesL <- filter(Species, HEH_18_Mean < M)


# Circular Plot High (Grey)
plotdata = circular(SpeciesH$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata, bins=24, col = "gray60", prop=2, cex = 1, uin=1,
          tcl=0.05, main="Gray four-eyed opossum")


# Circular Plot High (Grey) & Low (White)
plotdata2 = circular(SpeciesL$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata2, bins=24, col = NULL, prop=2, cex = 1, axes=TRUE, uin=1
          , tcl=0.05, main=(unique(Species2$Species)),add=TRUE)


######## Tamandua tetradactyla   ################

Species <- filter(caps, Species == "Tamandua tetradactyla" & Buffer == 250)

# Fine mean HF

M=mean(Species$HEH_18_Mean)
# boxplot(Species$HEH_18_Mean)

# Species High HF

SpeciesH <- filter(Species, HEH_18_Mean > M)


# Species Low HF

SpeciesL <- filter(Species, HEH_18_Mean < M)


# Circular Plot High (Grey)
plotdata = circular(SpeciesH$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata, bins=24, col = "gray60", prop=2, cex = 1, uin=1,
          tcl=0.05, main="Collared anteater")


# Circular Plot High (Grey) & Low (White)
plotdata2 = circular(SpeciesL$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata2, bins=24, col = NULL, prop=2, cex = 1, axes=TRUE, uin=1
          , tcl=0.05, main=(unique(Species2$Species)),add=TRUE)

