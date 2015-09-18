## 	CountPVA function


# all variables with <<- assignment will be avaliable outside of the function
# 	Extract variable:
# Each lambda for the duration of the study as "Sitemu"	
# r squared	as "R.sq"
# mu with lower and upper 95% CI as "mu.s" with three columns of "mu.s$fit", "mu.s$lwr", and "mu.s$upr" 
# the set of year intervals of the start year of study and each consecutive addition of years as "years"
#		"years$RangeStart" and "years$RangeEnd"
# Lambda and the lower and upper 95% CIs as "growth.exp" with "growth.exp$fit", "growth.exp$lwr", and "growth.exp$upr"  

#	To use the function, enter "CountPVA(x,y)
#	x The column with the years of the study census
#	y The column with the annual count (years can be missed, the counts are for the matching years in the x year column) 


# To create the function in your R Console, run the following block of code:
#############################################################################################
############################### Count PVA Function ##########################################
##################################### Start #################################################
#############################################################################################

CountPVA <- function(x,y){
  
  LM.table <- c()
  
	for(yrs in 4:length(unique(x))){		# gives 3 transitions before the first Count PVA calculation
		ySpecies <- c(); xvar <- c()
		rsq <- c();
		PVA.lm <- list()
		#Year1<-c(); Year2<-c(); 
    rm(PVA.table)
    
    y.count <- y[1:yrs]
    x.years <- x[1:yrs]
    ySpecies <- log10(y.count[-1]/y.count[-length(y.count)])
    yr <- sqrt(x.years[-1]-x.years[-length(x.years)])
    
		PVA.lm <- lm(ySpecies ~ -1 + yr)
		mu_sp <- predict(PVA.lm, level= 0.95,
		                 interval = "confidence",
		                 se.fit = T)$fit[1,]
    rsq <- c(rsq, R2 = summary(PVA.lm)$adj.r.squared)	# pull R squared values from each year interval 
    LM.table <- rbind(LM.table,data.frame(rsq,RangeStart=min(x),RangeEnd=x[yrs],t(mu_sp)))    
	}
	Year1 <- x.years[-length(x.years)]
	Year2 <- x.years[-1]
	PVA.table <-data.frame(yr,ySpecies,Year1,Year2)
  
  list(PVA = PVA.table,LM = LM.table)
}


save(CountPVA, file = "Q:/Research/Stats & Software/R CODE/Functions/CountPVAFunction.R")

#############################################################################################
############################### Count PVA Function ##########################################
####################################### End #################################################
#############################################################################################









