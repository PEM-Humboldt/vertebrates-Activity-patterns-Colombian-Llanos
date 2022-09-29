# Load packages

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


######## Catharus ustulatus   ################

Species <- filter(caps, Species == "Catharus ustulatus" & Buffer == 500)

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
          tcl=0.05, main="Swainsons thrush")


# Circular Plot High (Grey) & Low (White)
plotdata2 = circular(SpeciesL$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata2, bins=24, col = NULL, prop=2, cex = 1, axes=TRUE, uin=1
          , tcl=0.05, main=(unique(Species2$Species)),add=TRUE)


######## Crypturellus soui   ################

Species <- filter(caps, Species == "Crypturellus soui" & Buffer == 250)

# Fine mean HF

M=mean(Species$HEH_18_Mean)
# boxplot(Species$HEH_18_Mean)

# Species High HF

SpeciesH <- filter(Species, HEH_18_Mean > M)


# Species Low HF

SpeciesL <- filter(Species, HEH_18_Mean < M)


# Circular Plot High (Grey)
plotdata = circular(SpeciesH$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata, bins=24, col = "gray60", prop=1.8, cex = 1, uin=1,
          tcl=0.05, main="Little tinamou")


# Circular Plot High (Grey) & Low (White)
plotdata2 = circular(SpeciesL$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata2, bins=24, col = NULL, prop=1.8, cex = 1, axes=TRUE, uin=1
          , tcl=0.05, main=(unique(Species2$Species)),add=TRUE)


######## Leptotila verreauxi   ################

Species <- filter(caps, Species == "Leptotila verreauxi" & Buffer == 1000)

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
          tcl=0.05, main="White-tipped dove")


# Circular Plot High (Grey) & Low (White)
plotdata2 = circular(SpeciesL$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata2, bins=24, col = NULL, prop=2, cex = 1, axes=TRUE, uin=1
          , tcl=0.05, main=(unique(Species2$Species)),add=TRUE)


######## Momotus momota   ################

Species <- filter(caps, Species == "Momotus momota" & Buffer == 500)

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
          tcl=0.05, main="Amazonian motmot")


# Circular Plot High (Grey) & Low (White)
plotdata2 = circular(SpeciesL$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata2, bins=24, col = NULL, prop=2, cex = 1, axes=TRUE, uin=1
          , tcl=0.05, main=(unique(Species2$Species)),add=TRUE)


######## Nyctidromus albicollis   ################

Species <- filter(caps, Species == "Nyctidromus albicollis" & Buffer == 250)

# Fine mean HF

M=mean(Species$HEH_18_Mean)
# boxplot(Species$HEH_18_Mean)

# Species High HF

SpeciesH <- filter(Species, HEH_18_Mean > M)


# Species Low HF

SpeciesL <- filter(Species, HEH_18_Mean < M)


# Circular Plot High (Grey)
plotdata = circular(SpeciesH$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata, bins=24, col = "gray60", prop=1.2, cex = 1, uin=1,
          tcl=0.05, main="Pauraque")


# Circular Plot High (Grey) & Low (White)
plotdata2 = circular(SpeciesL$Time.rad, type="angles",units="radians",template="clock24")
rose.diag(plotdata2, bins=24, col = NULL, prop=1.2, cex = 1, axes=TRUE, uin=1
          , tcl=0.05, main=(unique(Species2$Species)),add=TRUE)

