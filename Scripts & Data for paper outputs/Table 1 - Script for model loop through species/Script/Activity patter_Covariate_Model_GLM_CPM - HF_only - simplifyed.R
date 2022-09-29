
### Clean the environment
rm(list = ls())

### Avoid scientific notation
options(scipen=999)

### Load Packages
library(lme4)
library(scales)
library(tidyverse)
require(MuMIn)





### Load the data

# Data0 = read.csv(file = "Records_Day_Night_Covariates_No_CTPC145_3.csv")

Data0 = read.csv(file.choose(),head=T)



# Remove sites without any records
Data0 = Data0[Data0$Day != 0 | Data0$Night != 0,]

# Remove the 2000m scale since the buffers starts to overlap at this scale
Data0  = Data0[Data0$Buffer != 2000,]




## Save function from stack overflow to detect warnings
# https://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function 
myTryCatch <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(value=value, warning=warn, error=err)
}




### SELECT THE BEST SCALE FOR EACH SPECIES

## create empty dataframe to save the results
results = data.frame(matrix(NA, ncol = 14, nrow = 0))
colnames(results) = c("Species","Class", "Buffer_size", "Intercept",
                      "Intercept_SE","Beta_estimate", "Beta_SE","P_value", "AIC",
                      "AIC_model_set", "Significance", "sample_size","n_of_records", "warnings")


## Save the models for later inspection
Model_list = list()


#MAIN LOOP
for(i in sort(unique(Data0$Species))){
  
  sp = Data0[Data0$Species == i,] #select the species
  
  for(b in sort(unique(sp$Buffer))){ #Select the buffer size
    
    print(paste(i,b)) #report which model is being run
    
    sp_buf = sp[sp$Buffer == b,] #get select the covariable according to the selected buffer size
    
    if( sum(sp_buf$Day) >= 5 & sum(sp_buf$Night) >= 5 ){ #skip species with less than 5 records during day or night
  
      HEH_18_Mean = glm(cbind(Day,Night) ~ HEH_18_Mean, data = sp_buf, family = binomial) #run the model
      
      warn_HEH_18_Mean = myTryCatch(glm(cbind(Day,Night) ~ HEH_18_Mean, data = sp_buf, family = binomial)) #record any warning

      #Save the model for later inspection
      Model_list[[length(Model_list)+1]] = HEH_18_Mean
      names(Model_list)[length(Model_list)] = paste(i,b, sep = "_")
      
      
      #save results - make a temporary data holder
      temp = as.data.frame(matrix(NA,ncol = ncol(results), nrow = 1))
      colnames(temp) = colnames(results)
      
      temp$Species = i
      temp$Class = unique(sp$class)
      temp$Buffer_size = b
      temp$AIC_model_set = paste(i,b)
      temp$AIC = HEH_18_Mean$aic
      temp$sample_size = nrow(sp_buf)
      temp$n_of_records = sum(sp_buf$Day, sp_buf$Night)
      temp$Intercept = summary(HEH_18_Mean)$coefficients["(Intercept)","Estimate"]
      temp$Intercept_SE = summary(HEH_18_Mean)$coefficients["(Intercept)","Std. Error"]
      temp$Beta_estimate = summary(HEH_18_Mean)$coefficients["HEH_18_Mean","Estimate"]
      temp$Beta_SE = summary(HEH_18_Mean)$coefficients["HEH_18_Mean","Std. Error"]
      temp$P_value = summary(HEH_18_Mean)$coefficients["HEH_18_Mean","Pr(>|z|)"]
      temp$Significance = ifelse(temp$P_value <= 0.05, "Significant", "Non-Significant")
      temp$warnings = paste0(warn_HEH_18_Mean$warning, warn_HEH_18_Mean$error, sep = "_")
      
      #save the results
      results = rbind(results, temp)
      
    }
  }
}

rm(temp,i,b,sp,sp_buf,HEH_18_Mean,warn_HEH_18_Mean) #clean the environment




### Change the coefficiets from logit to probability
#see also https://sebastiansauer.github.io/convert_logit2prob/

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

results$Intercept_coef_prob = logit2prob(results$Intercept)
results$Beta_estimate_coef_prob = logit2prob(results$Beta_estimate)




### Select just the best scale of each species

best_scale_results = results


best_scale_results$keep = F

#Mark the best mscale for each species
for(i in unique(best_scale_results$Species)){
  
  best = best_scale_results[best_scale_results$Species == i,] #select the species
  best = best[best$AIC == min(best$AIC),] #find the mest scale
  
  best_scale_results$keep[best_scale_results$Species == i & best_scale_results$Buffer_size == best$Buffer_size] = T #mark it

}

rm(i, best)

best_scale_results  = best_scale_results %>% 
  filter(keep) %>% 
  select(-keep)


#  Safe Data frame "Results" which has all the model outputs in a folder

write.csv(results, file="G:/Final results all models.csv", row.names=FALSE) #Create table


#  Safe Data frame "best_scale_results" which has all the model outputs in a folder

write.csv(best_scale_results, file="G:/Final results best scale only.csv", row.names=FALSE) #Create table


