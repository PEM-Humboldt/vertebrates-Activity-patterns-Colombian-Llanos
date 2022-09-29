# Load packages

library(lme4) 
library(scales)
library(DHARMa)
library(gridExtra)
library("grid")
library("ggplotify")

# split into multiple plots

par(mfcol= c(4,2))
par(mar=c(2, 2, 1, 1) + 3)

# if(!is.null(dev.list())) dev.off() # clear plots

### Load the data
Data0 = read.csv(file.choose(),head=T)


#Convert site name to number

Data0$Site=Data0$survey_id


Data0$Site = as.factor(Data0$Site) # turning into factor (just for safety, character also should work)


# Select the Species data needed

Data = Data0[,c("camera_id", "Species", "Site", "Buffer", "HEH_18_Mean", "Day", "Night")] # YOU CAN ALSO USE "HEH_70_Mean" OR "HEH_dif_Mean" OR THE MEDIAN
colnames(Data) = c("Camera", "Species", "Site", "Buffer", "Disturbance", "Day", "Night")

Data = Data[Data$Day != 0 | Data$Night != 0,] #remove sites without any records


######## Cuniculus paca   ################


Data_E <- subset(Data, Data$Species == "Cuniculus_paca") # Decide which Species to test

Data_EB <- subset(Data_E, Data_E$Buffer == 500) # Decide which buffer to test


### fit the model ### In this case for HF_18_Mean

Model = glm(cbind(Day,Night) ~ Disturbance , data = Data_EB ,family = binomial)
summary(Model)


### Create the tables needed to Plot the data - GLM

#for the line
plot_X <- data.frame(Disturbance = seq(min(Data_EB$Disturbance), max(Data_EB$Disturbance), len = 100)) #cpm# data_EB # generates a sequence of values around the interval defined
prd.model <- predict(Model, newdata = plot_X, type = "r", se.fit = T)


#used for plot the outer points (aesthetic only)
OuterPoints = data.frame(matrix(NA, ncol = 2, nrow = 0)) 
colnames(OuterPoints) = c("X", "Y")


#cpm# your dataset have a different structure than mine. This loop will need some adjustments
for(a in Data_EB$Camera){ #create coordinates for the points 
  new = data.frame(X = rep(Data_EB$Disturbance[Data_EB$Camera == a], sum(Data_EB$Day[Data_EB$Camera == a], Data_EB$Night[Data_EB$Camera == a])),
                   Y = c(rep(1.05, Data_EB$Day[Data_EB$Camera == a]), rep(-0.05, Data_EB$Night[Data_EB$Camera == a])))
  OuterPoints = rbind(OuterPoints, new)
}; rm(new, a)


### Scatter PLOT GLM


plot(plot_X$Disturbance, prd.model$fit, type = "l",xlab = expression(italic("Paca")), cex.lab=2, cex.axis=1.2, ylab = "Diurnality",
     ylim = c(-0.1,1.1), yaxt = "n", lwd = 2, font.lab=2) 
lines(plot_X$Disturbance, prd.model$fit - 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
lines(plot_X$Disturbance, prd.model$fit + 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
axis(side = 2, at= seq(0,1,0.25), labels = seq(0,1,0.25), cex.axis=1.2)
#axis(side = 1, at= Xticks, labels = Xlab, cex.axis=0.7) #cpm# this is because my variable was in log
points(jitter(OuterPoints$Y,0.05) ~ jitter(OuterPoints$X,50), cex = 1.2, col = alpha("darkblue", 0.2))

cp = recordPlot()



######## Dasyprocta fuliginosa   ################


Data_E <- subset(Data, Data$Species == "Dasyprocta_fuliginosa") # Decide which Species to test

Data_EB <- subset(Data_E, Data_E$Buffer == 1000) # Decide which buffer to test


### fit the model ### In this case for HF_18_Mean

Model = glm(cbind(Day,Night) ~ Disturbance , data = Data_EB ,family = binomial)
summary(Model)


### Create the tables needed to Plot the data - GLM

#for the line
plot_X <- data.frame(Disturbance = seq(min(Data_EB$Disturbance), max(Data_EB$Disturbance), len = 100)) #cpm# data_EB # generates a sequence of values around the interval defined
prd.model <- predict(Model, newdata = plot_X, type = "r", se.fit = T)


#used for plot the outer points (aesthetic only)
OuterPoints = data.frame(matrix(NA, ncol = 2, nrow = 0)) 
colnames(OuterPoints) = c("X", "Y")


#cpm# your dataset have a different structure than mine. This loop will need some adjustments
for(a in Data_EB$Camera){ #create coordinates for the points 
  new = data.frame(X = rep(Data_EB$Disturbance[Data_EB$Camera == a], sum(Data_EB$Day[Data_EB$Camera == a], Data_EB$Night[Data_EB$Camera == a])),
                   Y = c(rep(1.05, Data_EB$Day[Data_EB$Camera == a]), rep(-0.05, Data_EB$Night[Data_EB$Camera == a])))
  OuterPoints = rbind(OuterPoints, new)
}; rm(new, a)


### Scatter PLOT GLM


plot(plot_X$Disturbance, prd.model$fit, type = "l",xlab = expression(italic("Agouti")), cex.lab=2, cex.axis=1.2, ylab = "Diurnality",
     ylim = c(-0.1,1.1), yaxt = "n", lwd = 2, font.lab=2) 
lines(plot_X$Disturbance, prd.model$fit - 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
lines(plot_X$Disturbance, prd.model$fit + 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
axis(side = 2, at= seq(0,1,0.25), labels = seq(0,1,0.25), cex.axis=1.2)
#axis(side = 1, at= Xticks, labels = Xlab, cex.axis=0.7) #cpm# this is because my variable was in log
points(jitter(OuterPoints$Y,0.05) ~ jitter(OuterPoints$X,50), cex = 1.2, col = alpha("darkblue", 0.2))

df = recordPlot()


######## Dasypus novemcinctus   ################


Data_E <- subset(Data, Data$Species == "Dasypus_novemcinctus") # Decide which Species to test

Data_EB <- subset(Data_E, Data_E$Buffer == 500) # Decide which buffer to test


### fit the model ### In this case for HF_18_Mean

Model = glm(cbind(Day,Night) ~ Disturbance , data = Data_EB ,family = binomial)
summary(Model)


### Create the tables needed to Plot the data - GLM

#for the line
plot_X <- data.frame(Disturbance = seq(min(Data_EB$Disturbance), max(Data_EB$Disturbance), len = 100)) #cpm# data_EB # generates a sequence of values around the interval defined
prd.model <- predict(Model, newdata = plot_X, type = "r", se.fit = T)


#used for plot the outer points (aesthetic only)
OuterPoints = data.frame(matrix(NA, ncol = 2, nrow = 0)) 
colnames(OuterPoints) = c("X", "Y")


#cpm# your dataset have a different structure than mine. This loop will need some adjustments
for(a in Data_EB$Camera){ #create coordinates for the points 
  new = data.frame(X = rep(Data_EB$Disturbance[Data_EB$Camera == a], sum(Data_EB$Day[Data_EB$Camera == a], Data_EB$Night[Data_EB$Camera == a])),
                   Y = c(rep(1.05, Data_EB$Day[Data_EB$Camera == a]), rep(-0.05, Data_EB$Night[Data_EB$Camera == a])))
  OuterPoints = rbind(OuterPoints, new)
}; rm(new, a)


### Scatter PLOT GLM


plot(plot_X$Disturbance, prd.model$fit, type = "l",xlab = expression(italic("Armadillo")), cex.lab=2, cex.axis=1.2, ylab = "Diurnality",
     ylim = c(-0.1,1.1), yaxt = "n", lwd = 2, font.lab=2) 
lines(plot_X$Disturbance, prd.model$fit - 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
lines(plot_X$Disturbance, prd.model$fit + 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
axis(side = 2, at= seq(0,1,0.25), labels = seq(0,1,0.25), cex.axis=1.2)
#axis(side = 1, at= Xticks, labels = Xlab, cex.axis=0.7) #cpm# this is because my variable was in log
points(jitter(OuterPoints$Y,0.05) ~ jitter(OuterPoints$X,50), cex = 1.2, col = alpha("darkblue", 0.2))

dn = recordPlot()


######## Eira barbara   ################


Data_E <- subset(Data, Data$Species == "Eira_barbara") # Decide which Species to test

Data_EB <- subset(Data_E, Data_E$Buffer == 500) # Decide which buffer to test


### fit the model ### In this case for HF_18_Mean

Model = glm(cbind(Day,Night) ~ Disturbance , data = Data_EB ,family = binomial)
summary(Model)


### Create the tables needed to Plot the data - GLM

#for the line
plot_X <- data.frame(Disturbance = seq(min(Data_EB$Disturbance), max(Data_EB$Disturbance), len = 100)) #cpm# data_EB # generates a sequence of values around the interval defined
prd.model <- predict(Model, newdata = plot_X, type = "r", se.fit = T)


#used for plot the outer points (aesthetic only)
OuterPoints = data.frame(matrix(NA, ncol = 2, nrow = 0)) 
colnames(OuterPoints) = c("X", "Y")


#cpm# your dataset have a different structure than mine. This loop will need some adjustments
for(a in Data_EB$Camera){ #create coordinates for the points 
  new = data.frame(X = rep(Data_EB$Disturbance[Data_EB$Camera == a], sum(Data_EB$Day[Data_EB$Camera == a], Data_EB$Night[Data_EB$Camera == a])),
                   Y = c(rep(1.05, Data_EB$Day[Data_EB$Camera == a]), rep(-0.05, Data_EB$Night[Data_EB$Camera == a])))
  OuterPoints = rbind(OuterPoints, new)
}; rm(new, a)


### Scatter PLOT GLM


plot(plot_X$Disturbance, prd.model$fit, type = "l",xlab = expression(italic("Tayra")), cex.lab=2, cex.axis=1.2, ylab = "Diurnality",
     ylim = c(-0.1,1.1), yaxt = "n", lwd = 2, font.lab=2) 
lines(plot_X$Disturbance, prd.model$fit - 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
lines(plot_X$Disturbance, prd.model$fit + 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
axis(side = 2, at= seq(0,1,0.25), labels = seq(0,1,0.25), cex.axis=1.2)
#axis(side = 1, at= Xticks, labels = Xlab, cex.axis=0.7) #cpm# this is because my variable was in log
points(jitter(OuterPoints$Y,0.05) ~ jitter(OuterPoints$X,50), cex = 1.2, col = alpha("darkblue", 0.2))


eb = recordPlot()


######## Didelphis marsupialis   ################


Data_E <- subset(Data, Data$Species == "Didelphis_marsupialis") # Decide which Species to test

Data_EB <- subset(Data_E, Data_E$Buffer == 500) # Decide which buffer to test


### fit the model ### In this case for HF_18_Mean

Model = glm(cbind(Day,Night) ~ Disturbance , data = Data_EB ,family = binomial)
summary(Model)


### Create the tables needed to Plot the data - GLM

#for the line
plot_X <- data.frame(Disturbance = seq(min(Data_EB$Disturbance), max(Data_EB$Disturbance), len = 100)) #cpm# data_EB # generates a sequence of values around the interval defined
prd.model <- predict(Model, newdata = plot_X, type = "r", se.fit = T)


#used for plot the outer points (aesthetic only)
OuterPoints = data.frame(matrix(NA, ncol = 2, nrow = 0)) 
colnames(OuterPoints) = c("X", "Y")


#cpm# your dataset have a different structure than mine. This loop will need some adjustments
for(a in Data_EB$Camera){ #create coordinates for the points 
  new = data.frame(X = rep(Data_EB$Disturbance[Data_EB$Camera == a], sum(Data_EB$Day[Data_EB$Camera == a], Data_EB$Night[Data_EB$Camera == a])),
                   Y = c(rep(1.05, Data_EB$Day[Data_EB$Camera == a]), rep(-0.05, Data_EB$Night[Data_EB$Camera == a])))
  OuterPoints = rbind(OuterPoints, new)
}; rm(new, a)


### Scatter PLOT GLM


plot(plot_X$Disturbance, prd.model$fit, type = "l",xlab = expression(italic("Common opossum")), cex.lab=2, cex.axis=1.2, ylab = " ",
     ylim = c(-0.1,1.1), yaxt = "n", lwd = 2, font.lab=2) 
lines(plot_X$Disturbance, prd.model$fit - 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
lines(plot_X$Disturbance, prd.model$fit + 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
axis(side = 2, at= seq(0,1,0.25), labels = seq(0,1,0.25), cex.axis=1.2)
#axis(side = 1, at= Xticks, labels = Xlab, cex.axis=0.7) #cpm# this is because my variable was in log
points(jitter(OuterPoints$Y,0.05) ~ jitter(OuterPoints$X,50), cex = 1.2, col = alpha("darkblue", 0.2))

dm = recordPlot()





######## Philander opossum   ################


Data_E <- subset(Data, Data$Species == "Philander_opossum") # Decide which Species to test

Data_EB <- subset(Data_E, Data_E$Buffer == 1000) # Decide which buffer to test


### fit the model ### In this case for HF_18_Mean

Model = glm(cbind(Day,Night) ~ Disturbance , data = Data_EB ,family = binomial)
summary(Model)


### Create the tables needed to Plot the data - GLM

#for the line
plot_X <- data.frame(Disturbance = seq(min(Data_EB$Disturbance), max(Data_EB$Disturbance), len = 100)) #cpm# data_EB # generates a sequence of values around the interval defined
prd.model <- predict(Model, newdata = plot_X, type = "r", se.fit = T)


#used for plot the outer points (aesthetic only)
OuterPoints = data.frame(matrix(NA, ncol = 2, nrow = 0)) 
colnames(OuterPoints) = c("X", "Y")


#cpm# your dataset have a different structure than mine. This loop will need some adjustments
for(a in Data_EB$Camera){ #create coordinates for the points 
  new = data.frame(X = rep(Data_EB$Disturbance[Data_EB$Camera == a], sum(Data_EB$Day[Data_EB$Camera == a], Data_EB$Night[Data_EB$Camera == a])),
                   Y = c(rep(1.05, Data_EB$Day[Data_EB$Camera == a]), rep(-0.05, Data_EB$Night[Data_EB$Camera == a])))
  OuterPoints = rbind(OuterPoints, new)
}; rm(new, a)


### Scatter PLOT GLM


plot(plot_X$Disturbance, prd.model$fit, type = "l",xlab = expression(italic("Gray four-eyed opossum")), cex.lab=2, cex.axis=1.2, ylab = " ",
     ylim = c(-0.1,1.1), yaxt = "n", lwd = 2, font.lab=2) 
lines(plot_X$Disturbance, prd.model$fit - 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
lines(plot_X$Disturbance, prd.model$fit + 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
axis(side = 2, at= seq(0,1,0.25), labels = seq(0,1,0.25), cex.axis=1.2)
#axis(side = 1, at= Xticks, labels = Xlab, cex.axis=0.7) #cpm# this is because my variable was in log
points(jitter(OuterPoints$Y,0.05) ~ jitter(OuterPoints$X,50), cex = 1.2, col = alpha("darkblue", 0.2))

po = recordPlot()


######## Tamandua tetradactyla   ################


Data_E <- subset(Data, Data$Species == "Tamandua_tetradactyla") # Decide which Species to test

Data_EB <- subset(Data_E, Data_E$Buffer == 250) # Decide which buffer to test


### fit the model ### In this case for HF_18_Mean

Model = glm(cbind(Day,Night) ~ Disturbance , data = Data_EB ,family = binomial)
summary(Model)


### Create the tables needed to Plot the data - GLM

#for the line
plot_X <- data.frame(Disturbance = seq(min(Data_EB$Disturbance), max(Data_EB$Disturbance), len = 100)) #cpm# data_EB # generates a sequence of values around the interval defined
prd.model <- predict(Model, newdata = plot_X, type = "r", se.fit = T)


#used for plot the outer points (aesthetic only)
OuterPoints = data.frame(matrix(NA, ncol = 2, nrow = 0)) 
colnames(OuterPoints) = c("X", "Y")


#cpm# your dataset have a different structure than mine. This loop will need some adjustments
for(a in Data_EB$Camera){ #create coordinates for the points 
  new = data.frame(X = rep(Data_EB$Disturbance[Data_EB$Camera == a], sum(Data_EB$Day[Data_EB$Camera == a], Data_EB$Night[Data_EB$Camera == a])),
                   Y = c(rep(1.05, Data_EB$Day[Data_EB$Camera == a]), rep(-0.05, Data_EB$Night[Data_EB$Camera == a])))
  OuterPoints = rbind(OuterPoints, new)
}; rm(new, a)


### Scatter PLOT GLM


plot(plot_X$Disturbance, prd.model$fit, type = "l",xlab = expression(italic("Collared anteater")), cex.lab=2, cex.axis=1.2, ylab = " ",
     ylim = c(-0.1,1.1), yaxt = "n", lwd = 2, font.lab=2) 
lines(plot_X$Disturbance, prd.model$fit - 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
lines(plot_X$Disturbance, prd.model$fit + 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
axis(side = 2, at= seq(0,1,0.25), labels = seq(0,1,0.25), cex.axis=1.2)
#axis(side = 1, at= Xticks, labels = Xlab, cex.axis=0.7) #cpm# this is because my variable was in log
points(jitter(OuterPoints$Y,0.05) ~ jitter(OuterPoints$X,50), cex = 1.2, col = alpha("darkblue", 0.2))

tt = recordPlot()

